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
#include "register.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_register ( const char *name , 
                             const char *id   ,
                             SEXP_VALUE *args , 
                             gpointer    data )
{
    ASSERT_ALISTP( args, id, name );

    elim_ping();
    
    const char *aname = ALIST_VAL_STR( args, "account-name" );
    const char *proto = ALIST_VAL_STR( args, "im-protocol"  );
    gpointer    auid  = ALIST_VAL_PTR( args, "account-uid"  );

    PurpleAccount *acct = 
      auid ? find_acct_by_uid( auid ) : purple_accounts_find( aname, proto );

    // =================================================================
    const char *errstr = NULL;
    int         errnum = 0;

    if( !acct )
    {
        errstr = "unknown account";
        errnum = ENXIO; 
    }
    else if( purple_account_is_connected( acct ) )
    {
        errstr = "account already exists";
        errnum = EALREADY;
    }
    else if( purple_account_is_connecting( acct ) )
    {
        errstr = "connection already in progress";
        errnum = EINPROGRESS;
    }

    if( errnum )
    {
        sexp_val_free( args );
        return response_error( errnum, id, name, errstr );
    }
    // =================================================================

    purple_account_register( acct );

    // =================================================================

    xmlnode *rval = xnode_new( "alist" );
    AL_PTR( rval, "account-uid" , acct );
    AL_STR( rval, "account-name", purple_account_get_username   ( acct ) );
    AL_STR( rval, "im-protocol" , purple_account_get_protocol_id( acct ) );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
