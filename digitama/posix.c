/* System Headers */
#if defined(__sun) && defined(__SVR4)
#define _POSIX_C_SOURCE 199506L
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <syslog.h>
#include <stdarg.h>
#include <pwd.h>
#include <grp.h>

/* Quote Headers */

/** User and Group  **/
/* Constants Declaration */
#define BUFSIZE 32
static char lgnbuf[BUFSIZE];
static char grpbuf[BUFSIZE];

int fetch_tamer_ids(char* name, uid_t* uid, gid_t* gid) {
    struct passwd pwd, *passwords;
    char *buf;
    size_t bufsize;
    int status;

    errno = 0; /* make sure returned errno equals saved errno */
    bufsize = sysconf(_SC_GETPW_R_SIZE_MAX);
    if (bufsize == -1) bufsize = 16384;

    buf = malloc(bufsize);
    if (buf == NULL) goto exit_with_errno;

    status = getpwnam_r(name, &pwd, buf, bufsize, &passwords);
    if (passwords == NULL) {
        if (status == 0)
            errno = ENOENT;
        else {
            errno = status;
        }
        goto exit_with_errno;
    }

    (*uid) = pwd.pw_uid;
    (*gid) = pwd.pw_gid;

exit_with_errno:
    if (buf != NULL) free(buf);
    return errno;
}

int fetch_tamer_name(uid_t uid, char** login) {
    struct passwd *pwd;

    errno = 0; /* make sure returned errno equals saved errno */
    pwd = getpwuid(uid);
    if (pwd == NULL) goto exit_with_errno;

    strncpy(lgnbuf, pwd->pw_name, BUFSIZE);
    lgnbuf[BUFSIZE - 1] = '\0';

    (*login) = lgnbuf;

exit_with_errno:
    return errno;
}

int fetch_tamer_group(gid_t gid, char** group) {
    struct group *grp;

    errno = 0; /* make sure returned errno equals saved errno */
    grp = getgrgid(gid);
    if (grp == NULL) goto exit_with_errno;

    strncpy(grpbuf, grp->gr_name, BUFSIZE);
    grpbuf[BUFSIZE - 1] = '\0';

    (*group) = grpbuf;

exit_with_errno:
    return errno;
}

/** syslog **/
/* Facility Constants  */
uintptr_t KERNEL = LOG_KERN;        /* kernel messages */
uintptr_t USER = LOG_USER;         /* random user-level messages */
uintptr_t MAIL = LOG_MAIL;         /* mail system */
uintptr_t DAEMON = LOG_DAEMON;     /* system daemons */
uintptr_t AUTH = LOG_AUTH;         /* security/authorization messages */
uintptr_t SYSLOG = LOG_SYSLOG;     /* messages generated internally by syslogd */
uintptr_t LPR = LOG_LPR;           /* line printer subsystem */
uintptr_t NEWS = LOG_NEWS;         /* netnews subsystem */
uintptr_t UUCP = LOG_UUCP;         /* uucp subsystem */
uintptr_t ALTCRON = LOG_ALTCRON;   /* BSD cron/at subsystem */
uintptr_t AUTHPRIV = LOG_AUTHPRIV; /* BSD security/authorization messages */
uintptr_t FTP = LOG_FTP;           /* file transfer subsystem */
uintptr_t NTP = LOG_NTP;           /* network time subsystem */
uintptr_t AUDIT = LOG_AUDIT;       /* audit subsystem */
uintptr_t CONSOLE = LOG_CONSOLE;   /* BSD console messages */
uintptr_t CRON = LOG_CRON;         /* cron/at subsystem */
uintptr_t LOCAL0 = LOG_LOCAL0;     /* reserved for local use */
uintptr_t LOCAL1 = LOG_LOCAL1;     /* reserved for local use */
uintptr_t LOCAL2 = LOG_LOCAL2;     /* reserved for local use */
uintptr_t LOCAL3 = LOG_LOCAL3;     /* reserved for local use */
uintptr_t LOCAL4 = LOG_LOCAL4;     /* reserved for local use */
uintptr_t LOCAL5 = LOG_LOCAL5;     /* reserved for local use */
uintptr_t LOCAL6 = LOG_LOCAL6;     /* reserved for local use */
uintptr_t LOCAL7 = LOG_LOCAL7;     /* reserved for local use */

/* Priorities Constants */ 
uintptr_t EMERG = LOG_EMERG;       /* system is unusable */
uintptr_t ALERT = LOG_ALERT;       /* action must be taken immediately */
uintptr_t CRIT = LOG_CRIT;         /* critical conditions */
uintptr_t FATAL = LOG_CRIT;        /* critical conditions (racket only) */
uintptr_t ERROR = LOG_ERR;         /* error conditions */
uintptr_t WARNING = LOG_WARNING;   /* warning conditions */
uintptr_t NOTICE = LOG_NOTICE;     /* normal but signification condition */
uintptr_t INFO = LOG_INFO;         /* informational */
uintptr_t DEBUG = LOG_DEBUG;       /* debug-level messages */

/* Log Options */
uintptr_t PID = LOG_PID;            /* log the pid with each message */
uintptr_t CONS = LOG_CONS;          /* log on the console if errors in sending */
uintptr_t ODELAY = LOG_ODELAY;      /* delay open until syslog() is called */
uintptr_t NDELAY = LOG_NDELAY;      /* don't delay open */
uintptr_t NOWAIT = LOG_NOWAIT;      /* if forking to log on console, don't wait() */

inline void setlogmask_one(uintptr_t maskpri) {
    setlogmask(LOG_MASK(maskpri));
}

inline void setlogmask_upto(uintptr_t maskpri) {
    setlogmask(LOG_UPTO(maskpri));
}

/** System Monitor **/
double sysloadavg[3];

/* 
 * Begin ViM Modeline
 * vim:ft=c:ts=4:
 * End ViM
 */

