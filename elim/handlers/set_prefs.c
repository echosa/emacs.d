/*
Copyright © 2009-2011 Vivek Dasmohapatra 

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
#include "set_prefs.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"
#include "../elim-glibcompat.h"
xmlnode * _h_elim_set_prefs ( const char *name , 
                              const char *id   ,
                              SEXP_VALUE *args , 
                              gpointer    data )
{
    ASSERT_ALISTP( args, id, name );
    elim_ping();
    xmlnode    *rval  = xnode_new( "alist" );
    xmlnode    *set   = xnode_new( "alist" );
    GHashTable *prefs = ALIST_VAL_ALIST( args, "prefs" );
    SEXP_VALUE *PREFS = ALIST_VAL      ( args, "prefs" );

    AL_NODE( rval, "prefs", set );

    fprintf( stderr, "hash: %p; sexp: %p\n", prefs, PREFS );

    if( prefs )
    {
        GList *key  = NULL;
        GList *keys = ELIM_G_HASH_TABLE_GET_KEYS( prefs );

        fprintf( stderr, "keys: %p", keys );

        for( key = keys; key; key = key->next )
        {
            gboolean        done = TRUE;
            const char     *pref = key->data;
            PurplePrefType  type = purple_prefs_get_type( pref );
            switch( type )
            {
              case PURPLE_PREF_BOOLEAN:
                purple_prefs_set_bool  ( pref, ALIST_VAL_BOOL( PREFS, pref ) );
                break;
              case PURPLE_PREF_INT:
                purple_prefs_set_int   ( pref, ALIST_VAL_INT ( PREFS, pref ) );
                break;
              case PURPLE_PREF_STRING:
                purple_prefs_set_string( pref, ALIST_VAL_STR ( PREFS, pref ) );
                break;
              case PURPLE_PREF_PATH:
                purple_prefs_set_path  ( pref, ALIST_VAL_STR ( PREFS, pref ) );
                break;
              default:
                done = FALSE;
            }

            AL_BOOL( set, pref, done );
        }

        g_list_free( keys );
    }

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
