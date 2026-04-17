/*
 * proxy.c — shared-port reverse-proxy for xfun's new_app() / stop_app().
 *
 * A single proxy instance listens on one port and can serve multiple
 * xfun apps at the same time.  The app is identified by a "~name" prefix
 * in the request URL path:
 *
 *   PORT/~name/rest  →  BACKEND:/custom/xfun:name:PORT/rest  (named app)
 *   PORT/rest        →  BACKEND:/custom/xfun::PORT/rest       (nameless app)
 *
 * The port number is embedded in the rewritten path so that the R-level
 * httpd handler key (assigned with assign("xfun:name:PORT", ...)) is unique
 * even when two apps share a port under different names.
 *
 * Multiple instances are supported (up to XP_MAX=32), one per distinct
 * listen port.  proxy_start(port, backend) allocates a slot and returns
 * its index; proxy_stop(slot) tears down that instance.
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
#else
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <unistd.h>
#  include <pthread.h>
typedef int xp_sock_t;
#  define XP_INVALID  (-1)
#  define xp_close(s) close(s)
#endif

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <R.h>
#include <Rinternals.h>
#include "picohttpparser.h"

/* ---- per-instance state ------------------------------------------------- */
#define XP_MAX        32
#define XP_BUF        65536

typedef struct {
    xp_sock_t    listen;
    int          port;       /* own listen port (embedded in rewritten path) */
    int          backend;    /* R httpd backend port */
    int          passthrough;/* 1 = forward all paths verbatim; 0 = ~name rewriting */
    volatile int active;
#ifdef _WIN32
    HANDLE       thread;
#else
    pthread_t    thread;
    int          thread_valid;
#endif
} xp_instance_t;

static xp_instance_t xp_inst[XP_MAX];
static int           xp_init_done = 0;

#ifdef _WIN32
static int xp_wsa_refs = 0;
#endif

static void xp_instances_init(void)
{
    if (xp_init_done) return;
    for (int i = 0; i < XP_MAX; i++) {
        xp_inst[i].listen = XP_INVALID;
        xp_inst[i].active = 0;
#ifdef _WIN32
        xp_inst[i].thread = NULL;
#else
        xp_inst[i].thread_valid = 0;
#endif
    }
    xp_init_done = 1;
}

/* ---- handle one client connection --------------------------------------- */

static void xp_handle(xp_sock_t cfd, xp_instance_t *inst)
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

    /* rewrite path.
     *
     * passthrough mode (inst->passthrough = 1):
     *   forward path verbatim — used to proxy the entire R httpd server.
     *
     * ~name rewriting mode (default):
     *   /~name/rest  →  /custom/xfun:name:PORT/rest  (named app)
     *   /~name       →  /custom/xfun:name:PORT/       (named app, root)
     *   /~name?q=1   →  /custom/xfun:name:PORT/?q=1
     *   /rest        →  /custom/xfun::PORT/rest        (nameless app)
     */
    char npath[4096];
    if (inst->passthrough) {
        if (path_len + 1 >= sizeof(npath)) goto done;
        memcpy(npath, path, path_len);
        npath[path_len] = '\0';
    } else if (path_len >= 2 && path[0] == '/' && path[1] == '~') {
        /* named app: extract name up to '/', '?', or end of path */
        size_t ni = 2;
        while (ni < path_len && path[ni] != '/' && path[ni] != '?') ni++;
        size_t nlen = ni - 2;           /* length of name */
        if (nlen > 255) goto done;      /* reject unreasonably long app names */
        const char *rest = path + ni;
        size_t rlen = path_len - ni;
        if (rlen == 0 || rest[0] == '?') {
            /* /~name  or  /~name?q=1 → insert '/' before rest */
            int n = snprintf(npath, sizeof(npath),
                             "/custom/xfun:%.*s:%d/%.*s",
                             (int)nlen, path + 2,
                             inst->port,
                             (int)rlen, rest);
            if (n <= 0 || n >= (int)sizeof(npath)) goto done;
        } else {
            /* /~name/rest */
            int n = snprintf(npath, sizeof(npath),
                             "/custom/xfun:%.*s:%d%.*s",
                             (int)nlen, path + 2,
                             inst->port,
                             (int)rlen, rest);
            if (n <= 0 || n >= (int)sizeof(npath)) goto done;
        }
    } else {
        /* nameless app: /rest → /custom/xfun::PORT/rest */
        int n = snprintf(npath, sizeof(npath),
                         "/custom/xfun::%d%.*s",
                         inst->port, (int)path_len, path);
        if (n <= 0 || n >= (int)sizeof(npath)) goto done;
    }

    /* connect to R's httpd backend */
    xp_sock_t bfd = socket(AF_INET, SOCK_STREAM, 0);
    if (bfd == XP_INVALID) goto done;
    {
        struct sockaddr_in ba;
        memset(&ba, 0, sizeof(ba));
        ba.sin_family      = AF_INET;
        ba.sin_port        = htons((unsigned short)inst->backend);
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
                            (int)nl,                hdrs[i].name,
                            (int)hdrs[i].value_len, hdrs[i].value);
        send(bfd, hline, hlen, 0);
    }
    /* inject X-Xfun-Query header (raw query string for named param reconstruction) */
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

/* ---- proxy thread -------------------------------------------------------- */
#ifdef _WIN32
static DWORD WINAPI xp_thread_fn(LPVOID arg)
#else
static void *xp_thread_fn(void *arg)
#endif
{
    xp_instance_t *inst = (xp_instance_t *)arg;
    while (inst->active) {
        fd_set rfds; FD_ZERO(&rfds); FD_SET(inst->listen, &rfds);
        struct timeval tv = {0, 100000}; /* 100 ms */
        if (select((int)inst->listen + 1, &rfds, NULL, NULL, &tv) > 0) {
            xp_sock_t cfd = accept(inst->listen, NULL, NULL);
            if (cfd != XP_INVALID) xp_handle(cfd, inst);
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
 * proxy_start(port, backend_port, passthrough, host)
 * Allocate a new proxy instance.
 *   port:        port to listen on
 *   backend_port: R httpd backend port to forward to
 *   passthrough: logical — TRUE = forward all paths verbatim (full httpd proxy);
 *                           FALSE = use ~name URL rewriting (for new_app())
 *   host:        bind address ("127.0.0.1" or "0.0.0.0")
 * Returns the slot index (>= 0) on success, -1L on failure.
 */
SEXP proxy_start(SEXP r_port, SEXP r_backend, SEXP r_passthrough, SEXP r_host)
{
    xp_instances_init();

#ifdef _WIN32
    if (xp_wsa_refs == 0) { WSADATA wsa; WSAStartup(MAKEWORD(2, 2), &wsa); }
    xp_wsa_refs++;
#endif

    /* find a free slot */
    int slot = -1;
    for (int i = 0; i < XP_MAX; i++) {
        if (!xp_inst[i].active && xp_inst[i].listen == XP_INVALID) {
            slot = i; break;
        }
    }
    if (slot < 0) goto fail;

    {
        xp_instance_t *inst = &xp_inst[slot];
        inst->backend     = INTEGER(r_backend)[0];
        inst->port        = INTEGER(r_port)[0];
        inst->passthrough = LOGICAL(r_passthrough)[0];

        /* determine bind address */
        const char *host_str = CHAR(STRING_ELT(r_host, 0));
        int use_any = (strcmp(host_str, "0.0.0.0") == 0);

        int port = INTEGER(r_port)[0];
        xp_sock_t fd = socket(AF_INET, SOCK_STREAM, 0);
        if (fd == XP_INVALID) goto fail;

        int one = 1;
        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (const char *)&one, sizeof(one));
        struct sockaddr_in addr;
        memset(&addr, 0, sizeof(addr));
        addr.sin_family      = AF_INET;
        addr.sin_port        = htons((unsigned short)port);
        addr.sin_addr.s_addr = use_any ? htonl(INADDR_ANY) : htonl(INADDR_LOOPBACK);
        if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) != 0 ||
            listen(fd, 32) != 0) { xp_close(fd); goto fail; }

        inst->listen = fd;
        inst->active = 1;

#ifdef _WIN32
        inst->thread = CreateThread(NULL, 0, xp_thread_fn, inst, 0, NULL);
        if (!inst->thread) {
            inst->active = 0; xp_close(fd); inst->listen = XP_INVALID; goto fail;
        }
#else
        if (pthread_create(&inst->thread, NULL, xp_thread_fn, inst) != 0) {
            inst->active = 0; xp_close(fd); inst->listen = XP_INVALID;
            inst->thread_valid = 0; goto fail;
        }
        inst->thread_valid = 1;
#endif
        return ScalarInteger(slot);
    }

fail:
#ifdef _WIN32
    xp_wsa_refs--;
    if (xp_wsa_refs == 0) WSACleanup();
#endif
    return ScalarInteger(-1);
}

/*
 * port_available(port)
 * Try to bind a TCP socket on 127.0.0.1:port; return TRUE if the port is
 * free, FALSE otherwise.  Works on all R versions (no serverSocket() needed).
 */
SEXP xp_port_available(SEXP r_port)
{
#ifdef _WIN32
    WSADATA wsa; WSAStartup(MAKEWORD(2, 2), &wsa);
#endif
    int port = INTEGER(r_port)[0];
    xp_sock_t fd = socket(AF_INET, SOCK_STREAM, 0);
    int ok = 0;
    if (fd != XP_INVALID) {
        int one = 1;
        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (const char *)&one, sizeof(one));
        struct sockaddr_in addr;
        memset(&addr, 0, sizeof(addr));
        addr.sin_family      = AF_INET;
        addr.sin_port        = htons((unsigned short)port);
        addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        ok = (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) == 0);
        xp_close(fd);
    }
#ifdef _WIN32
    WSACleanup();
#endif
    return ScalarLogical(ok);
}

/*
 * proxy_stop(slot)
 * Signal the background thread of the given instance to stop, wait for it,
 * and close the listen socket.
 */
SEXP proxy_stop(SEXP r_slot)
{
    xp_instances_init();
    int slot = INTEGER(r_slot)[0];
    if (slot < 0 || slot >= XP_MAX) return R_NilValue;

    xp_instance_t *inst = &xp_inst[slot];
    if (!inst->active) return R_NilValue;

    inst->active = 0;
#ifdef _WIN32
    if (inst->thread) {
        WaitForSingleObject(inst->thread, 1000);
        CloseHandle(inst->thread);
        inst->thread = NULL;
    }
    if (inst->listen != XP_INVALID) { xp_close(inst->listen); inst->listen = XP_INVALID; }
    xp_wsa_refs--;
    if (xp_wsa_refs == 0) WSACleanup();
#else
    if (inst->thread_valid) { pthread_join(inst->thread, NULL); inst->thread_valid = 0; }
    if (inst->listen != XP_INVALID) { xp_close(inst->listen); inst->listen = XP_INVALID; }
#endif
    return R_NilValue;
}
