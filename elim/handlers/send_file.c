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
#include "send_file.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"


xmlnode * _h_elim_send_file ( const char *name ,
                              const char *id   ,
                              SEXP_VALUE *args ,
                              gpointer data    )
{
    ASSERT_ALISTP( args, id, name );

    elim_ping();
    
    const char *aname = ALIST_VAL_STR( args, "account-name" );
    const char *proto = ALIST_VAL_STR( args, "im-protocol"  );
    gpointer    auid  = ALIST_VAL_PTR( args, "account-uid"  );
    
    PurpleAccount *acct = 
      auid ? find_acct_by_uid( auid ) : purple_accounts_find( aname, proto );

    if( !acct )
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "unknown account" );
    }

    PurpleConnection *conn = purple_account_get_connection( acct );

    if( !conn )
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "account not online" );
    }

    // file can be NULL, but that's Ok, it just triggers a req to the user:
    const char *b_arg = ALIST_VAL_STRING( args, "recipient" );
    const char *file  = ALIST_VAL_STRING( args, "filename"  );

    serv_send_file( conn, b_arg, file );

    // bname is a static buf allocated in purple_normalize: don't free it!
    // also, this means that bname is volatile, so use it immediately after
    // acquiring it, if you make any calls into libpurple they might invalidate
    // its state:
    const char *bname = purple_normalize( acct, b_arg );
    xmlnode    *rval  = xnode_new( "alist" );
    AL_PTR( rval, "account-uid" , acct  );
    AL_STR( rval, "recipient"   , bname );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
