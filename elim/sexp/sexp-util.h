/*
Copyright © 2009, 2010 Vivek Dasmohapatra 

email : vivek@etla.org
irc   : fledermaus on freenode, oftc
jabber: fledermaus@jabber.earth.li

This file is part of elim.

elim is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

elim is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with elim.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef _EMACSIM_SEXP_UTIL_H_
#define _EMACSIM_SEXP_UTIL_H_

#include <stdio.h>
#include <stdlib.h>
#include <glib.h>
#include <string.h>
#include "../xnode/xnode.h"

#define ALIST_VAL(s,k)  g_hash_table_lookup( s->x.hash, k )

#define ALIST_VAL_STRING(s,k)        \
    ({ SEXP_VALUE *v;                \
       (v = ALIST_VAL( (s), (k) )) ? \
         ( ( v->type == SEXP_STRING ) ? v->x.string->str : NULL ) : NULL; })
#define ALIST_VAL_STR(s,k) ALIST_VAL_STRING(s,k)

#define ALIST_VAL_GSTRING(s,k)       \
    ({ SEXP_VALUE *v;                \
       (v = ALIST_VAL( (s), (k) )) ? \
         ( ( v->type == SEXP_STRING ) ? v->x.string : NULL ) : NULL; })
#define ALIST_VAL_DATA(s,k) ALIST_VAL_GSTRING(s,k)

#define ALIST_VAL_INT(s,k)           \
    ({ SEXP_VALUE *v;                \
       (v = ALIST_VAL( (s), (k) )) ? \
         ( ( v->type == SEXP_INT ) ? v->x.integer : -1 ) : -1; })

#define ALIST_VAL_PTR(s,k)           \
    ({ SEXP_VALUE *v;                \
       (v = ALIST_VAL( (s), (k) )) ? \
         ( ( v->type == SEXP_INT ) ? (gpointer)v->x.integer : NULL ) : NULL; })

#define ALIST_VAL_BOOL(s,k)          \
    ({ SEXP_VALUE *v;                \
       (v = ALIST_VAL( (s), (k) )) ? \
         ( ( v->type == SEXP_BOOL ) ? v->x.bool : FALSE ) : FALSE; })

#define ALIST_VAL_ALIST(s,k)         \
    ({ SEXP_VALUE *v;                \
       (v = ALIST_VAL( (s), (k) )) ? \
         ( ( v->type == SEXP_ALIST ) ? v->x.hash : NULL ) : NULL; })


typedef enum _sexp_type
{
    SEXP_UNKNOWN ,
    SEXP_STRING  ,
    SEXP_INT     ,
    SEXP_FLOAT   ,
    SEXP_BOOL    ,
    SEXP_B64     ,
    SEXP_ALIST   ,
    SEXP_LIST
} sexp_type;

typedef struct _SEXP_TYPE_MAP SEXP_TYPE_MAP;
struct _SEXP_TYPE_MAP
{
    const char *name;
    sexp_type   type;
};


typedef struct _SEXP_VALUE SEXP_VALUE;
struct _SEXP_VALUE 
{
    sexp_type type;
    union 
    {
        GString    *string ;
        GString    *data   ;
        long        integer; // got to be big enough to hold a pointer
        double      number ;
        gboolean    bool   ;
        GHashTable *hash   ;
        GList      *list   ;
    } x;
};

void         sexp_val_free ( gpointer ptr     );
SEXP_VALUE * sexp_value    ( xmlnode *node    );
char       * sexp_to_str   ( SEXP_VALUE *sexp );

#endif
