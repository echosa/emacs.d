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
#include "unregister.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

typedef struct _UNREG_DATA UNREG_DATA;
struct _UNREG_DATA { char *name; char *id; };

void _h_elim_unregister_cb( PurpleAccount *acct , 
                            gboolean       dead , 
                            gpointer       data )
{
    UNREG_DATA *call = data;
    xmlnode    *rval = xnode_new( "alist" );
    xmlnode    *resp = response_value( 0, call->id, call->name, rval );
    fprintf( stderr, "(_elim_unregister_cb:00)\n" );

    AL_BOOL( rval, "unregistered", dead );
    AL_PTR ( rval, "account-uid" , acct );
    AL_STR ( rval, "account-name", purple_account_get_username   ( acct ) );
    AL_STR ( rval, "im-protocol" , purple_account_get_protocol_id( acct ) );

    fprintf( stderr, "(_elim_unregister_cb:02)\n" );
    g_free( call->name );
    fprintf( stderr, "(_elim_unregister_cb:03)\n" );
    g_free( call->id   );
    fprintf( stderr, "(_elim_unregister_cb:04)\n" );
    g_free( call       );
    fprintf( stderr, "(_elim_unregister_cb:05)\n" );
    add_outbound_sexp( resp );
}

xmlnode * _h_elim_unregister ( const char *name , 
                               const char *id   ,
                               SEXP_VALUE *args , 
                               gpointer    data )
{
    fprintf( stderr, "(_elim_unregister)\n" );

    ASSERT_ALISTP( args, id, name );

    elim_ping();
    
    UNREG_DATA *cbh   = NULL;
    const char *aname = ALIST_VAL_STR( args, "account-name" );
    const char *proto = ALIST_VAL_STR( args, "im-protocol"  );
    gpointer    auid  = ALIST_VAL_PTR( args, "account-uid"  );

    fprintf( stderr, "(_elim_unregister:01)\n" );

    PurpleAccount *acct = 
      auid ? find_acct_by_uid( auid ) : purple_accounts_find( aname, proto );

    fprintf( stderr, "(_elim_unregister:02)\n" );

    if( !acct )
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "unknown account" );
    }

    fprintf( stderr, "(_elim_unregister:03)\n" );
    // =================================================================
    cbh       = g_new0  ( UNREG_DATA, 1 );
    cbh->name = g_strdup( name );
    cbh->id   = g_strdup( id   );
    fprintf( stderr, 
             "(_elim_unregister:04 %p %p %p<%s.%s>)\n",  
             acct, _h_elim_unregister_cb, cbh, cbh->name, cbh->id );
    purple_account_unregister( acct, 
                               (PurpleAccountUnregistrationCb)_h_elim_unregister_cb, 
                               cbh );
    // =================================================================

    fprintf( stderr, "(_elim_unregister:05)\n" );
    sexp_val_free( args );
    //return response_value( 0, id, name, rval );
    fprintf( stderr, "(_elim_unregister:06)\n" );
    return NULL;
}
