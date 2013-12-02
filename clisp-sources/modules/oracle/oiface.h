/* oiface.h */

/* This is part of the CLISP Oracle interface from Alma Mater
   Software.  Copyright (C) 2002 Alma Mater Software, Inc.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License version 2 as
  published by the Free Software Foundation; see file GNU-GPL.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

  $Id: oiface.h,v 1.4 2005/05/20 20:03:51 haible Exp $

*/

/* Column info returned to external program.  All pointers here point
   either to corresponding members in (struct column) or to
   constant/static data. */

struct sqlcol {
  char *        name;           /* Points to column.name */
  char *        type;           /* Points to result of decode_data_type(column.dtype)) */
  int           size;           /* Same as column.dsize */
  int           scale;          /* Same as column.scale */
  int           precision;      /* Same as column.precision */
  int           null_ok;        /* Same as column.null_ok */
};

/* Row info returned to external program.  Pointers here point into
   corresponding members in (struct column). */

struct sqlval {
  char *        data;           /* Points to column.data */
  int           is_null;        /* Same as column.indicator -1=NULL 0=exact */
};

/* Bind parameter input to a SQL statement.  We support named
   paramters only (no positional parameters) */
struct sqlparam {
  char *            name;           /* Parameter name */
  struct sqlval     value;          /* Parameter value */
};

/* Column coming back from a SELECT */
struct column {
  /* Column info extracted from OCIAttrGet(stmt) */
  char *        name;       /* OCI_ATTR_NAME */
  ub2           dtype;      /* OCI_ATTR_DATA_TYPE */
  ub2           dsize;      /* OCI_ATTR_DATA_SIZE */
  sb1           scale;      /* OCI_ATTR_SCALE */
  ub1           precision;  /* OCI_ATTR_PRECISION */
  ub1           null_ok;    /* OCI_ATTR_IS_NULL */

  /* Fetch info and results */
  OCIDefine *   def;        /* Returned from OCIDefineByPos() */

  void *        data;       /* Our malloc'd buffer (of dsize bytes) */
  sb2           indicator;  /* "Indicator" variable w/ flags for NULL and truncated:
                                 -2 = data too large, even too large to say size in sb2
                                 -1 = data is null
                                 0 = exact data was fetched
                                 >0 = original size before truncation to "define" variable len */
  ub2           nfetched;   /* No. of bytes actually fetched */
  ub2           rcode;      /* Column-level return code */

  OCILobLocator * lob_locator;  /* Special destination for LobLocator fetch */
};

/* Database connection, incl. (max of one) current statement, as well
   as results and status. */
struct db_conn {

  /* Per connection */
  OCIEnv *          env;            /* Oracle environment handle */
  OCIError *        err;            /* "" */
  OCISvcCtx *       svc;            /* "" */
  int               prefetch_bytes; /* No. of bytes for pre-fetch buffer, or -1 for default */
  int               long_len;       /* No. of bytes to truncate LONG columns */
  int               truncate_ok;    /* Flag: allow truncated fetch? */
  int               auto_commit;    /* Flag: commit after each command? */
  OCIStmt *         stmt;           /* Oracle statement */
  
  char *            user;           /* User ID */
  char *            schema;         /* Schema */
  char *            sid;            /* SID logged on to */

  /* Per executed SQL statement */
  char *            sql;            /* Malloced SQL query or command */
  int               is_command;     /* True if SQL is a command (i.e., not a SELECT) */
  char **           params;         /* Bind parameters for sql command */
  int               nparam;         /* No. of bind parameters */
  struct column *   columns;        /* Result column info and current row data (internal version) */
  struct sqlcol **  sqlcols;        /* Result column info and current row data (external version) */
  struct sqlval **  currow;         /* Current row data */
  int               ncol;           /* No. of columns */

  /* Per fetch */
  int               rows_affected;  /* No. of rows fetched to present, or rows affected by command */
  int               eof;            /* Flag: at EOF? */

  int               success;        /* Set after each operation w/ success */
  char *            errmsg;         /* Malloc'd error buffer */
};

/* Exported routines (w/ void *) */

/* orafns.c */
void *           oracle_connect(char *, char *, char *, char *, int, int, int, int);
int              oracle_disconnect(void *);
int              oracle_exec_sql(void *, char *, struct sqlparam **, int);

int              oracle_ncol(void *);               /* Active SELECT only, may be called any time after exec_sql() */
struct sqlcol ** oracle_column_info(void *);        /* "" */
int              oracle_fetch_row(void *);          /* "" */
int              oracle_eof(void *);                /* Active SELECT only, may be called only after fetch_row() */
struct sqlval ** oracle_row_values(void *);         /* "" */

int              oracle_rows_affected(void *);      /* For SELECT, no. rows fetched so far; others: no. rows insert/update/delete */

int              oracle_success(void *);            /* Check success of last operation.   May be called anytime */

int              oracle_commit(void *);                 /* Commit current transaction (auto_commit off) */
int              oracle_rollback(void *);               /* Rollback current transaction */
int              oracle_set_auto_commit(void *, int);   /* Enable/disable auto-commit */

/* oiface.c */
char *           oracle_last_error(void *);

