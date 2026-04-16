/*
 * proxy.c — lightweight reverse-proxy for xfun's new_app() / stop_app().
 *
 * The proxy listens on a user-visible port (LISTEN_PORT) and forwards
 * requests to R's internal httpd (BACKEND_PORT), rewriting the URL so that
 *   LISTEN_PORT  /app-name/rest   →   BACKEND_PORT  /custom/app-name/rest
 *
 * The proxy runs in a background thread (pthreads on Unix/macOS, a Windows
 * thread on Windows).  It never calls any R functions, so it works
 * immediately in interactive sessions on all platforms without requiring the
 * user to press Enter (no R event-loop involvement needed for the proxy
 * itself; R's internal httpd has its own event-loop integration for all
 * platforms).
 *
 * HTTP parsing uses the vendored picohttpparser library.
 */

/* ---- platform socket + thread portability -------------------------------- */
#ifdef _WIN32
#  define WIN32_LEAN_AND_MEAN
#  include <winsock2.h>
#  include <ws2tcpip.h>
#  include <windows.h>
typedef SOCKET xp_sock_t;
#  define XP_INVALID  INVALID_SOCKET
#  define xp_close(s) closesocket(s)
#  define strncasecmp _strnicmp
static HANDLE xp_thread = NULL;
#else
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <unistd.h>
#  include <pthread.h>
typedef int xp_sock_t;
#  define XP_INVALID  (-1)
#  define xp_close(s) close(s)
static pthread_t xp_thread;
static int       xp_thread_valid = 0;
#endif

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <R.h>
#include <Rinternals.h>
#include "picohttpparser.h"

/* ---- proxy state -------------------------------------------------------- */
static xp_sock_t     xp_listen   = XP_INVALID;
static int           xp_backend  = 0;
static volatile int  xp_active   = 0;

/* ---- handle one client connection -------------------------------------- */
#define XP_BUF 65536

static void xp_handle(xp_sock_t cfd)
{
    char ibuf[XP_BUF];
    int  ntot = 0, hend = -1;

    /* receive until complete HTTP headers seen */
    while (ntot < XP_BUF - 1) {
#ifdef _WIN32
        DWORD rcvtv = 5000;
        setsockopt(cfd, SOL_SOCKET, SO_RCVTIMEO, (const char *)&rcvtv, sizeof(rcvtv));
#else
        struct timeval rcvtv = {5, 0};
        setsockopt(cfd, SOL_SOCKET, SO_RCVTIMEO, (const void *)&rcvtv, sizeof(rcvtv));
#endif
        int n = (int)recv(cfd, ibuf + ntot, XP_BUF - 1 - ntot, 0);
        if (n <= 0) goto done;
        ntot += n;
        ibuf[ntot] = '\0';
        const char *he = strstr(ibuf, "\r\n\r\n");
        if (he) { hend = (int)(he - ibuf) + 4; break; }
        /* tolerate bare \n\n */
        he = strstr(ibuf, "\n\n");
        if (he) { hend = (int)(he - ibuf) + 2; break; }
    }
    if (hend < 0) goto done;

    /* parse with picohttpparser */
    const char *method, *path;
    size_t method_len, path_len;
    int minor_ver;
    struct phr_header hdrs[64];
    size_t num_hdrs = 64;
    if (phr_parse_request(ibuf, (size_t)ntot,
                          &method, &method_len,
                          &path,   &path_len,
                          &minor_ver, hdrs, &num_hdrs, 0) < 0)
        goto done;

    /* find Content-Length */
    long clen = 0;
    for (size_t i = 0; i < num_hdrs; i++) {
        if (strncasecmp(hdrs[i].name, "content-length", hdrs[i].name_len) == 0) {
            char tmp[32]; size_t vl = hdrs[i].value_len < 31 ? hdrs[i].value_len : 31;
            memcpy(tmp, hdrs[i].value, vl); tmp[vl] = '\0';
            clen = atol(tmp); break;
        }
    }

    /* rewrite path: /name/rest → /custom/name/rest */
    char npath[4096];
    if (path_len > 0 && path[0] == '/') {
        if (path_len + 8 >= sizeof(npath)) goto done;
        memcpy(npath, "/custom", 7);
        memcpy(npath + 7, path, path_len);
        npath[7 + path_len] = '\0';
    } else {
        if (snprintf(npath, sizeof(npath), "/custom/%.*s",
                     (int)path_len, path) >= (int)sizeof(npath)) goto done;
    }

    /* connect to R's httpd backend */
    xp_sock_t bfd = socket(AF_INET, SOCK_STREAM, 0);
    if (bfd == XP_INVALID) goto done;
    {
        struct sockaddr_in ba;
        memset(&ba, 0, sizeof(ba));
        ba.sin_family      = AF_INET;
        ba.sin_port        = htons((unsigned short)xp_backend);
        ba.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        if (connect(bfd, (struct sockaddr *)&ba, sizeof(ba)) != 0) {
            xp_close(bfd); goto done;
        }
    }

    /* send rewritten request line (downgrade to HTTP/1.0 for simplicity) */
    {
        char rline[4096];
        int rlen = snprintf(rline, sizeof(rline), "%.*s %s HTTP/1.0\r\n",
                            (int)method_len, method, npath);
        send(bfd, rline, rlen, 0);
    }
    /* forward headers, skip hop-by-hop */
    for (size_t i = 0; i < num_hdrs; i++) {
        size_t nl = hdrs[i].name_len;
        if (strncasecmp(hdrs[i].name, "connection",       nl) == 0) continue;
        if (strncasecmp(hdrs[i].name, "keep-alive",       nl) == 0) continue;
        if (strncasecmp(hdrs[i].name, "proxy-connection", nl) == 0) continue;
        char hline[4096];
        int hlen = snprintf(hline, sizeof(hline), "%.*s: %.*s\r\n",
                            (int)nl,              hdrs[i].name,
                            (int)hdrs[i].value_len, hdrs[i].value);
        send(bfd, hline, hlen, 0);
    }
    /* Add X-Xfun-Query header with the raw query string so that the R
     * wrapper can reconstruct a named character vector of URL-decoded
     * parameters (R's httpd provides only URL-decoded values, without
     * the parameter names). */
    {
        const char *qs = NULL; size_t qs_len = 0;
        for (size_t ci = 0; ci < path_len; ci++) {
            if (path[ci] == '?') { qs = path + ci + 1; qs_len = path_len - ci - 1; break; }
        }
        if (qs && qs_len > 0) {
            char qhdr[4096];
            int qlen = snprintf(qhdr, sizeof(qhdr), "X-Xfun-Query: %.*s\r\n",
                                (int)qs_len, qs);
            if (qlen > 0 && qlen < (int)sizeof(qhdr)) send(bfd, qhdr, qlen, 0);
        }
    }
    send(bfd, "\r\n", 2, 0);

    /* forward body already in buffer */
    long body_in_buf = ntot - hend;
    if (body_in_buf > 0) send(bfd, ibuf + hend, (int)body_in_buf, 0);

    /* forward remaining body */
    for (long rem = clen - body_in_buf; rem > 0; ) {
        char tmp[4096];
        int chunk = (rem < (long)sizeof(tmp)) ? (int)rem : (int)sizeof(tmp);
        int r = (int)recv(cfd, tmp, chunk, 0);
        if (r <= 0) break;
        send(bfd, tmp, r, 0);
        rem -= r;
    }

    /* relay response to client */
    {
        char rbuf[XP_BUF];
        int r;
        while ((r = (int)recv(bfd, rbuf, sizeof(rbuf), 0)) > 0)
            send(cfd, rbuf, r, 0);
    }
    xp_close(bfd);

done:
    xp_close(cfd);
}

/* ---- proxy thread ------------------------------------------------------- */
#ifdef _WIN32
static DWORD WINAPI xp_thread_fn(LPVOID arg)
#else
static void *xp_thread_fn(void *arg)
#endif
{
    (void)arg;
    while (xp_active) {
        fd_set rfds; FD_ZERO(&rfds); FD_SET(xp_listen, &rfds);
        struct timeval tv = {0, 100000}; /* 100 ms */
        if (select((int)xp_listen + 1, &rfds, NULL, NULL, &tv) > 0) {
            xp_sock_t cfd = accept(xp_listen, NULL, NULL);
            if (cfd != XP_INVALID) xp_handle(cfd);
        }
    }
#ifdef _WIN32
    return 0;
#else
    return NULL;
#endif
}

/* ---- R entry points ----------------------------------------------------- */

/*
 * proxy_start(ports, backend_port)
 * Try ports in order; bind to the first available one; start background
 * thread.  Returns the bound port on success, -1L on failure.
 */
SEXP proxy_start(SEXP r_ports, SEXP r_backend)
{
#ifdef _WIN32
    WSADATA wsa;
    WSAStartup(MAKEWORD(2, 2), &wsa);
#endif

    /* stop any previously running proxy */
    if (xp_active) {
        xp_active = 0;
#ifdef _WIN32
        if (xp_thread) { WaitForSingleObject(xp_thread, 1000); CloseHandle(xp_thread); xp_thread = NULL; }
#else
        if (xp_thread_valid) { pthread_join(xp_thread, NULL); xp_thread_valid = 0; }
#endif
        if (xp_listen != XP_INVALID) { xp_close(xp_listen); xp_listen = XP_INVALID; }
    }

    xp_backend = INTEGER(r_backend)[0];
    int n = LENGTH(r_ports);

    for (int i = 0; i < n; i++) {
        int port = INTEGER(r_ports)[i];
        xp_sock_t fd = socket(AF_INET, SOCK_STREAM, 0);
        if (fd == XP_INVALID) continue;
        int one = 1;
        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (const char *)&one, sizeof(one));
        struct sockaddr_in addr;
        memset(&addr, 0, sizeof(addr));
        addr.sin_family      = AF_INET;
        addr.sin_port        = htons((unsigned short)port);
        addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) != 0 ||
            listen(fd, 32) != 0) { xp_close(fd); continue; }

        xp_listen = fd;
        xp_active = 1;

#ifdef _WIN32
        xp_thread = CreateThread(NULL, 0, xp_thread_fn, NULL, 0, NULL);
        if (!xp_thread) { xp_active = 0; xp_close(fd); xp_listen = XP_INVALID; return ScalarInteger(-1L); }
#else
        if (pthread_create(&xp_thread, NULL, xp_thread_fn, NULL) != 0) {
            xp_active = 0; xp_close(fd); xp_listen = XP_INVALID; xp_thread_valid = 0;
            return ScalarInteger(-1L);
        }
        xp_thread_valid = 1;
#endif
        return ScalarInteger(port);
    }
    return ScalarInteger(-1L);
}

/*
 * proxy_stop()
 * Signal the thread to stop, wait for it, close the listen socket.
 */
SEXP proxy_stop(void)
{
    xp_active = 0;
#ifdef _WIN32
    if (xp_thread) { WaitForSingleObject(xp_thread, 1000); CloseHandle(xp_thread); xp_thread = NULL; }
    if (xp_listen != XP_INVALID) { xp_close(xp_listen); xp_listen = XP_INVALID; }
    WSACleanup();
#else
    if (xp_thread_valid) { pthread_join(xp_thread, NULL); xp_thread_valid = 0; }
    if (xp_listen != XP_INVALID) { xp_close(xp_listen); xp_listen = XP_INVALID; }
#endif
    return R_NilValue;
}
