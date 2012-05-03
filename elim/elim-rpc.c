/*
Copyright © 2009,2010 Vivek Dasmohapatra

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
#include "elim-rpc.h"

static guint       id      = 0;
static GHashTable *cb_data = NULL;

xmlnode * response_error( int         code    ,
                          const char *id      ,
                          const char *name    ,
                          const char *message )
{
    GString *status = g_string_new    ( "" );
    xmlnode *error  = xnode_new       ( "function-response" );
    xmlnode *meth   = xnode_new_child ( error, name         );
    xmlnode *value  = xnode_new_child ( error, "alist"      );
    xmlnode *stat   = xnode_new_child ( value, "int"        );
    xmlnode *mesg   = xnode_new_child ( value, "string"     );

    // set the id attribute on the method and the 
    // status and message alist key name attributes:
    xnode_set_attrib      ( meth  , "id"  , id );
    xnode_set_attrib      ( stat  , "name", "status"  );
    xnode_set_attrib      ( mesg  , "name", "message" );

    // insert the data for the status and message:
    g_string_append_printf( status, "%d"       , code );
    xnode_insert_data     ( stat  , status->str, -1   );
    xnode_insert_data     ( mesg  , message    , -1   );
    g_string_free         ( status, TRUE );

    return error;
}


xmlnode * response_value( int         code  ,
                          const char *id    ,
                          const char *name  ,
                          xmlnode    *val   )
{
    GString *status = g_string_new    ( "" );
    xmlnode *mresp  = xnode_new       ( "function-response" );
    xmlnode *meth   = xnode_new_child ( mresp, name         );
    xmlnode *value  = xnode_new_child ( mresp, "alist"      );
    xmlnode *stat   = xnode_new_child ( value, "int"        );

    xnode_set_attrib      ( meth  , "id"  , id );
    xnode_set_attrib      ( stat  , "name", "status" );
    xnode_set_attrib      ( val   , "name", "value"  );

    g_string_append_printf( status, "%d"       , code );
    xnode_insert_data     ( stat  , status->str, -1   );
    g_string_free         ( status, TRUE  );

    xnode_insert_child    ( value , val   );

    return mresp;
}

xmlnode * func_call ( const char *name, const char *id, xmlnode *args )
{
    xmlnode *mcall = xnode_new      ( "function-call" );
    xmlnode *meth  = xnode_new_child( mcall , name    );
    xnode_set_attrib( meth, "id", id );
    if( args )
        xnode_insert_child( mcall, args );
    return mcall;
}

char * new_elim_id ()
{
    GString *ID = g_string_new_len( "", 10 );
    g_string_printf( ID, "%09d", ++id );
    return g_string_free( ID, FALSE );
}

gboolean store_cb_data( char *key, gpointer value )
{
    if( !cb_data )
    {
        cb_data =
          g_hash_table_new_full( g_str_hash, g_str_equal, g_free, NULL );
    }

    fprintf( stderr, "storing callback data: %p\n",  value );
    g_hash_table_insert( cb_data, key, value );

    return TRUE;
}

gpointer fetch_cb_data( const char *key )
{
    if( !cb_data ) { return NULL; }
    gpointer value = g_hash_table_lookup( cb_data, key );
    g_hash_table_remove( cb_data, key );
    return value;
}

gpointer check_cb_data( const char *key )
{
    if( !cb_data ) { return NULL; }
    if( !key     ) { return NULL; }
    return g_hash_table_lookup( cb_data, key );
}
