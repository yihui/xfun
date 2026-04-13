/*
 * A minimal HTTP/1.0 server for xfun's new_app() function.
 *
 * The server runs on 127.0.0.1 only.  httpd_start() opens a non-blocking
 * listen socket.  httpd_poll() is called from R's task-callback mechanism
 * (addTaskCallback); it accepts at most one pending connection per call,
 * parses the HTTP request, dispatches to the matching R handler, and sends
 * the response — all within a single C call so the R handler is invoked
 * synchronously from C via R_tryEval().
 *
 * The R handler convention (same as R's internal httpd) is:
 *   handler(path, query, post, headers)
 * where
 *   path    – character(1), URL path relative to the app root (never empty;
 *             "/" maps to ".")
 *   query   – named character vector of URL-decoded query parameters
 *   post    – raw vector containing the request body (length 0 for GET)
 *   headers – raw vector of request headers formatted as
 *             "Request-Method: METHOD\nField: value\n..."
 *             (LF-terminated lines, no trailing blank line)
 * and the return value is a named list with zero or more of:
 *   payload      – character(1): response body
 *   file         – character(1): path to a file to stream as the body
 *   content-type – character(1): MIME type (default "text/html; charset=UTF-8")
 *   status code  – integer(1): HTTP status (default 200)
 *   header       – named character vector of extra response headers
 */

/* ---- platform portability -------------------------------------------- */
#ifdef _WIN32
#  define WIN32_LEAN_AND_MEAN
#  include <winsock2.h>
#  include <ws2tcpip.h>
typedef SOCKET xfun_socket_t;
#  define XFUN_INVALID_SOCK  INVALID_SOCKET
#  define xfun_close_sock(s) closesocket(s)
#  define xfun_nonblock(s)   do { u_long m_ = 1; ioctlsocket(s, FIONBIO, &m_); } while (0)
#  define strncasecmp        _strnicmp
#  pragma comment(lib, "ws2_32.lib")
#else
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <unistd.h>
#  include <fcntl.h>
typedef int xfun_socket_t;
#  define XFUN_INVALID_SOCK  (-1)
#  define xfun_close_sock(s) close(s)
#  define xfun_nonblock(s)   do { int f_ = fcntl(s, F_GETFL, 0); fcntl(s, F_SETFL, f_ | O_NONBLOCK); } while (0)
#endif

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>  /* R_tryEval */

/* ---- global server state --------------------------------------------- */
static xfun_socket_t server_fd = XFUN_INVALID_SOCK;

/* ---- helpers ---------------------------------------------------------- */

/* URL-decode a NUL-terminated string in-place.  '+' → ' ', %XX → char. */
static void url_decode(char *s) {
    char *dst = s, *src = s;
    while (*src) {
        if (*src == '%' &&
            isxdigit((unsigned char)src[1]) &&
            isxdigit((unsigned char)src[2])) {
            char hex[3] = { src[1], src[2], '\0' };
            *dst++ = (char)strtol(hex, NULL, 16);
            src += 3;
        } else if (*src == '+') {
            *dst++ = ' '; src++;
        } else {
            *dst++ = *src++;
        }
    }
    *dst = '\0';
}

/*
 * Build a named character vector from a query string such as "a=1&b=2".
 * Keys and values are URL-decoded.  Returns an SEXP whose protection is
 * managed by the caller.
 */
static SEXP parse_query(const char *qs) {
    /* count parameters */
    int n = 0;
    if (qs && *qs) {
        n = 1;
        for (const char *p = qs; *p; p++) if (*p == '&') n++;
    }

    SEXP vals = PROTECT(allocVector(STRSXP, n));
    SEXP nms  = PROTECT(allocVector(STRSXP, n));

    if (n > 0) {
        char *buf = strdup(qs);
        char *p = buf;
        for (int i = 0; i < n && p; i++) {
            char *amp = strchr(p, '&');
            if (amp) *amp = '\0';

            char key[512] = "", val[4096] = "";
            char *eq = strchr(p, '=');
            if (eq) {
                int klen = (int)(eq - p);
                if (klen >= (int)sizeof(key)) klen = (int)sizeof(key) - 1;
                memcpy(key, p, klen);  key[klen] = '\0';
                int vlen = (int)strlen(eq + 1);
                if (vlen >= (int)sizeof(val)) vlen = (int)sizeof(val) - 1;
                memcpy(val, eq + 1, vlen);  val[vlen] = '\0';
            } else {
                int klen = (int)strlen(p);
                if (klen >= (int)sizeof(key)) klen = (int)sizeof(key) - 1;
                memcpy(key, p, klen);  key[klen] = '\0';
            }
            url_decode(key);
            url_decode(val);
            SET_STRING_ELT(nms,  i, mkChar(key));
            SET_STRING_ELT(vals, i, mkChar(val));

            p = amp ? amp + 1 : NULL;
        }
        free(buf);
    }

    setAttrib(vals, R_NamesSymbol, nms);
    UNPROTECT(2);
    return vals;
}

/* Send exactly `len` bytes; returns 0 on success, -1 on error. */
static int send_all(xfun_socket_t fd, const char *buf, int len) {
    int sent = 0;
    while (sent < len) {
        int n = (int)send(fd, buf + sent, len - sent, 0);
        if (n <= 0) return -1;
        sent += n;
    }
    return 0;
}

/*
 * Return the value part of the first header line matching `name:` (case-
 * insensitive) in the NUL-terminated buffer `buf`.  Lines are expected to be
 * '\n'-separated (R's httpd format) OR '\r\n'-separated (raw HTTP).  Returns
 * NULL if not found.
 */
static char *find_header(char *buf, const char *name) {
    size_t nlen = strlen(name);
    char *p = buf;
    while (p && *p) {
        if (strncasecmp(p, name, nlen) == 0)
            return p + nlen;
        /* advance to start of next line */
        char *nl = strchr(p, '\n');
        p = nl ? nl + 1 : NULL;
    }
    return NULL;
}

/* Return the named element of an R list, or R_NilValue if absent. */
static SEXP list_get(SEXP lst, const char *name) {
    SEXP nms = getAttrib(lst, R_NamesSymbol);
    if (isNull(nms)) return R_NilValue;
    int n = LENGTH(lst);
    for (int i = 0; i < n; i++)
        if (strcmp(CHAR(STRING_ELT(nms, i)), name) == 0)
            return VECTOR_ELT(lst, i);
    return R_NilValue;
}

/* Format and send an HTTP response from the R handler's return value. */
static void send_response(xfun_socket_t fd, SEXP resp) {
    if (TYPEOF(resp) != VECSXP) {
        send_all(fd, "HTTP/1.0 200 OK\r\nContent-Length: 0\r\n\r\n", 39);
        return;
    }

    SEXP payload   = list_get(resp, "payload");
    SEXP file_sexp = list_get(resp, "file");
    SEXP ctype_s   = list_get(resp, "content-type");
    SEXP status_s  = list_get(resp, "status code");
    SEXP header_s  = list_get(resp, "header");

    int status = isNull(status_s) ? 200 : asInteger(status_s);
    const char *ct = "text/html; charset=UTF-8";
    if (!isNull(ctype_s) && TYPEOF(ctype_s) == STRSXP && LENGTH(ctype_s) > 0)
        ct = CHAR(STRING_ELT(ctype_s, 0));

    /* status line */
    const char *sm;
    switch (status) {
    case 200: sm = "OK"; break;
    case 301: sm = "Moved Permanently"; break;
    case 302: sm = "Found"; break;
    case 304: sm = "Not Modified"; break;
    case 400: sm = "Bad Request"; break;
    case 404: sm = "Not Found"; break;
    case 500: sm = "Internal Server Error"; break;
    default:  sm = "OK"; break;
    }
    char sl[64];
    snprintf(sl, sizeof(sl), "HTTP/1.0 %d %s\r\n", status, sm);
    send_all(fd, sl, (int)strlen(sl));

    /* extra response headers supplied by the handler */
    if (!isNull(header_s) && TYPEOF(header_s) == STRSXP) {
        SEXP hnms = getAttrib(header_s, R_NamesSymbol);
        int  nh   = LENGTH(header_s);
        for (int i = 0; i < nh; i++) {
            const char *hname = (!isNull(hnms) && i < LENGTH(hnms))
                                ? CHAR(STRING_ELT(hnms, i)) : "";
            const char *hval  = CHAR(STRING_ELT(header_s, i));
            char hdr[1024];
            snprintf(hdr, sizeof(hdr), "%s: %s\r\n", hname, hval);
            send_all(fd, hdr, (int)strlen(hdr));
        }
    }

    /* body: either a file or an inline payload */
    if (!isNull(file_sexp) && TYPEOF(file_sexp) == STRSXP && LENGTH(file_sexp) > 0) {
        const char *fpath = CHAR(STRING_ELT(file_sexp, 0));
        FILE *f = fopen(fpath, "rb");
        if (!f) {
            send_all(fd, "Content-Length: 0\r\n\r\n", 21);
            return;
        }
        fseek(f, 0, SEEK_END);
        long fsize = ftell(f);
        fseek(f, 0, SEEK_SET);
        char hdr[512];
        snprintf(hdr, sizeof(hdr),
                 "Content-Type: %s\r\nContent-Length: %ld\r\n\r\n", ct, fsize);
        send_all(fd, hdr, (int)strlen(hdr));
        char chunk[65536];
        size_t nr;
        while ((nr = fread(chunk, 1, sizeof(chunk), f)) > 0)
            send_all(fd, chunk, (int)nr);
        fclose(f);
    } else if (!isNull(payload) && TYPEOF(payload) == STRSXP && LENGTH(payload) > 0) {
        const char *body = CHAR(STRING_ELT(payload, 0));
        int blen = (int)strlen(body);
        char hdr[256];
        snprintf(hdr, sizeof(hdr),
                 "Content-Type: %s\r\nContent-Length: %d\r\n\r\n", ct, blen);
        send_all(fd, hdr, (int)strlen(hdr));
        send_all(fd, body, blen);
    } else {
        send_all(fd, "Content-Length: 0\r\n\r\n", 21);
    }
}

/* ---- exported C functions -------------------------------------------- */

/*
 * httpd_start(ports)
 * ports: integer vector of candidate port numbers
 * Returns: the port actually bound, or -1 on failure.
 */
SEXP httpd_start(SEXP ports) {
#ifdef _WIN32
    WSADATA wsa;
    if (WSAStartup(MAKEWORD(2, 2), &wsa) != 0) return ScalarInteger(-1);
#endif
    int n = LENGTH(ports);
    for (int i = 0; i < n; i++) {
        int port = INTEGER(ports)[i];
        xfun_socket_t fd = socket(AF_INET, SOCK_STREAM, 0);
        if (fd == XFUN_INVALID_SOCK) continue;

        int opt = 1;
        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (const char *)&opt, sizeof(opt));

        struct sockaddr_in addr;
        memset(&addr, 0, sizeof(addr));
        addr.sin_family      = AF_INET;
        addr.sin_port        = htons((unsigned short)port);
        addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

        if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) != 0 ||
            listen(fd, 16) != 0) {
            xfun_close_sock(fd);
            continue;
        }

        xfun_nonblock(fd);
        server_fd = fd;
        return ScalarInteger(port);
    }
    return ScalarInteger(-1);
}

/*
 * httpd_stop()
 * Closes the server socket.
 */
SEXP httpd_stop(void) {
    if (server_fd != XFUN_INVALID_SOCK) {
        xfun_close_sock(server_fd);
        server_fd = XFUN_INVALID_SOCK;
    }
#ifdef _WIN32
    WSACleanup();
#endif
    return R_NilValue;
}

/*
 * httpd_poll(names, handlers)
 * names:    character vector of registered app names
 * handlers: list of R handler functions (parallel to names)
 *
 * Accepts at most one pending connection, parses it, dispatches to the
 * matching handler, and sends the response.  Should be called from a
 * task callback registered with addTaskCallback().
 *
 * Returns TRUE if a connection was processed, FALSE otherwise.
 */
SEXP httpd_poll(SEXP names, SEXP handlers) {
    if (server_fd == XFUN_INVALID_SOCK) return ScalarLogical(FALSE);

    /* non-blocking check for a pending connection */
    fd_set rfds;
    FD_ZERO(&rfds);
    FD_SET(server_fd, &rfds);
    struct timeval tv = { 0, 0 };
    if (select((int)server_fd + 1, &rfds, NULL, NULL, &tv) <= 0)
        return ScalarLogical(FALSE);

    xfun_socket_t cfd = accept(server_fd, NULL, NULL);
    if (cfd == XFUN_INVALID_SOCK) return ScalarLogical(FALSE);

    /* ---- read the HTTP request headers into a dynamic buffer ---------- */
    char  *req     = NULL;
    int    req_len = 0, req_cap = 0;
    int    hdr_end = -1;   /* byte offset right after the "\r\n\r\n" */

    while (hdr_end < 0) {
        fd_set rfd2; FD_ZERO(&rfd2); FD_SET(cfd, &rfd2);
        struct timeval rt = { 5, 0 };
        if (select((int)cfd + 1, &rfd2, NULL, NULL, &rt) <= 0) break;

        if (req_len + 4096 + 1 > req_cap) {
            req_cap = (req_cap == 0) ? 8192 : req_cap * 2;
            char *tmp = (char *)realloc(req, req_cap);
            if (!tmp) { free(req); xfun_close_sock(cfd); return ScalarLogical(FALSE); }
            req = tmp;
        }
        int nr = (int)recv(cfd, req + req_len, req_cap - req_len - 1, 0);
        if (nr <= 0) break;
        req_len += nr;
        req[req_len] = '\0';

        char *he = strstr(req, "\r\n\r\n");
        if (he) { hdr_end = (int)(he - req) + 4; break; }
    }

    if (!req || hdr_end < 0) {
        free(req);
        send_all(cfd, "HTTP/1.0 400 Bad Request\r\nContent-Length: 0\r\n\r\n", 47);
        xfun_close_sock(cfd);
        return ScalarLogical(FALSE);
    }

    /* ---- parse request line: METHOD SP path[?qs] SP HTTP/x.x --------- */
    char *eol = strstr(req, "\r\n");
    if (!eol) { free(req); xfun_close_sock(cfd); return ScalarLogical(FALSE); }
    *eol = '\0';  /* temporarily terminate the request line */

    char method[16] = "GET", raw_path[4096] = "/";
    sscanf(req, "%15s %4095s", method, raw_path);
    *eol = '\r';  /* restore */

    /* split path from query string */
    char *qs_sep = strchr(raw_path, '?');
    const char *qs = "";
    if (qs_sep) { *qs_sep = '\0'; qs = qs_sep + 1; }
    url_decode(raw_path);

    /* ---- parse Content-Length from headers ---------------------------- */
    /* Temporarily NUL-terminate at start of blank line to avoid searching body */
    char *blank_line = req + hdr_end - 4;  /* points to first '\r' of "\r\n\r\n" */
    char  saved_bl   = *blank_line;
    *blank_line = '\0';

    long clen = 0;
    char *cl_p = find_header(eol + 2, "content-length:");
    if (cl_p) {
        while (*cl_p == ' ') cl_p++;
        clen = atol(cl_p);
        if (clen < 0 || clen > 16 * 1024 * 1024) clen = 0;
    }
    *blank_line = saved_bl;  /* restore */

    /* ---- read POST body ----------------------------------------------- */
    char *body = NULL;
    int   body_len = 0;
    if (clen > 0) {
        int have = req_len - hdr_end;
        body = (char *)malloc((int)clen + 1);
        if (body) {
            if (have > 0) {
                int copy = (have < (int)clen) ? have : (int)clen;
                memcpy(body, req + hdr_end, copy);
                body_len = copy;
            }
            while (body_len < (int)clen) {
                fd_set rfd3; FD_ZERO(&rfd3); FD_SET(cfd, &rfd3);
                struct timeval rt2 = { 5, 0 };
                if (select((int)cfd + 1, &rfd3, NULL, NULL, &rt2) <= 0) break;
                int nr = (int)recv(cfd, body + body_len, (int)clen - body_len, 0);
                if (nr <= 0) break;
                body_len += nr;
            }
            body[body_len] = '\0';
        }
    }

    /* ---- build headers buffer in R's httpd format --------------------- */
    /*
     * Format: "Request-Method: METHOD\nField: value\n..." with LF line
     * endings (no trailing blank line).  This matches what R's own httpd
     * passes to handlers, so that existing handler code (e.g. litedown's
     * regex ".*\nlitedown-data: ([^[:space:]]+).*") works unchanged.
     */
    int   hbuf_cap = 4096, hbuf_len = 0;
    char *hbuf = (char *)malloc(hbuf_cap);
    if (!hbuf) { free(req); free(body); xfun_close_sock(cfd); return ScalarLogical(FALSE); }

#define HBUF_WRITE(s, slen) \
    do { \
        int _l = (slen); \
        while (hbuf_len + _l + 1 > hbuf_cap) { \
            hbuf_cap *= 2; \
            char *_t = (char *)realloc(hbuf, hbuf_cap); \
            if (!_t) { free(hbuf); hbuf = NULL; break; } \
            hbuf = _t; \
        } \
        if (hbuf) { memcpy(hbuf + hbuf_len, s, _l); hbuf_len += _l; hbuf[hbuf_len] = '\0'; } \
    } while (0)

    {
        const char prefix[] = "Request-Method: ";
        HBUF_WRITE(prefix, (int)sizeof(prefix) - 1);
        HBUF_WRITE(method, (int)strlen(method));
        HBUF_WRITE("\n", 1);
    }

    /* append each HTTP header line, replacing "\r\n" terminators with "\n" */
    {
        char *p = eol + 2;          /* skip past request line "\r\n" */
        char *headers_end = req + hdr_end - 4; /* stop before blank line */
        while (p < headers_end) {
            char *end = strstr(p, "\r\n");
            if (!end || end > headers_end) end = headers_end;
            if (hbuf) {
                HBUF_WRITE(p, (int)(end - p));
                HBUF_WRITE("\n", 1);
            }
            p = end + 2;  /* skip "\r\n" */
            if (p > headers_end) break;
        }
    }

    if (!hbuf) {
        free(req); free(body);
        xfun_close_sock(cfd);
        return ScalarLogical(FALSE);
    }

    /* ---- route: /appname[/rest] --------------------------------------- */
    const char *rpath = raw_path;
    if (*rpath == '/') rpath++;

    const char *slash = strchr(rpath, '/');
    char app_name[512];
    const char *rel_path;
    if (slash) {
        int nlen = (int)(slash - rpath);
        if (nlen >= (int)sizeof(app_name)) nlen = (int)sizeof(app_name) - 1;
        memcpy(app_name, rpath, nlen);  app_name[nlen] = '\0';
        rel_path = slash + 1;
    } else {
        strncpy(app_name, rpath, sizeof(app_name) - 1);
        app_name[sizeof(app_name) - 1] = '\0';
        rel_path = "";
    }
    if (*rel_path == '\0') rel_path = ".";

    /* find matching handler */
    int hidx = -1;
    int nnames = LENGTH(names);
    for (int i = 0; i < nnames; i++)
        if (strcmp(CHAR(STRING_ELT(names, i)), app_name) == 0) { hidx = i; break; }

    if (hidx < 0) {
        free(req); free(body); free(hbuf);
        send_all(cfd, "HTTP/1.0 404 Not Found\r\nContent-Length: 0\r\n\r\n", 46);
        xfun_close_sock(cfd);
        return ScalarLogical(TRUE);
    }

    /* ---- build R arguments ------------------------------------------- */
    SEXP r_path    = PROTECT(mkString(rel_path));
    SEXP r_query   = PROTECT(parse_query(qs));
    SEXP r_post    = PROTECT(allocVector(RAWSXP, body_len));
    if (body_len > 0) memcpy(RAW(r_post), body, body_len);
    SEXP r_headers = PROTECT(allocVector(RAWSXP, hbuf_len));
    if (hbuf_len > 0) memcpy(RAW(r_headers), hbuf, hbuf_len);

    free(req); free(body); free(hbuf);

    /* ---- call the R handler ------------------------------------------ */
    SEXP handler = VECTOR_ELT(handlers, hidx);
    SEXP call    = PROTECT(lang5(handler, r_path, r_query, r_post, r_headers));
    int  err     = 0;
    SEXP resp    = PROTECT(R_tryEval(call, R_GlobalEnv, &err));

    if (!err && !isNull(resp))
        send_response(cfd, resp);
    else {
        static const char e500[] =
            "HTTP/1.0 500 Internal Server Error\r\n"
            "Content-Type: text/plain\r\nContent-Length: 21\r\n\r\n"
            "Internal Server Error";
        send_all(cfd, e500, (int)sizeof(e500) - 1);
    }

    UNPROTECT(6);
    xfun_close_sock(cfd);
    return ScalarLogical(TRUE);
}
