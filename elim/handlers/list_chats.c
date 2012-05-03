/*
Copyright © 2010 Savio Sena
Copyright © 2009 Vivek Dasmohapatra

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

#include "list_chats.h"

#include "../prpl/util.h"
#include "../ui_ops/ops.h"

/* XXX: implement _h_elim_close_chat_list */

xmlnode * _h_elim_list_chats ( const char *name ,
                               const char *id   ,
                               SEXP_VALUE *args ,
                               gpointer   data  )
{
    ASSERT_ALISTP( args, id, name );

    const char *proto = ALIST_VAL_STR( args, "im-protocol"  );
    const char *aname = ALIST_VAL_STR( args, "account-name" );
    gpointer    auid  = ALIST_VAL_PTR( args, "account-uid"  );

    PurpleAccount    *acct     = NULL;
    PurpleConnection *conn     = NULL;
    PurpleRoomlist   *roomlist = NULL;

    FIND_ACCOUNT( args, id, name, acct, auid, aname, proto );

    conn = purple_account_get_connection( acct );
    if( !conn )
        HANDLER_FAIL( args, id, name, ENOENT, "No connection" );

	roomlist = purple_roomlist_get_list( conn );
	if( !roomlist )
        HANDLER_FAIL( args, id, name, ENOENT, "Roomlists not supported" );

    //purple_roomlist_ref( roomlist ); // XXX
    
    xmlnode *rval = xnode_new( "alist" );
    AL_PTR( rval, "account-uid" , acct );
    AL_STR( rval, "account-name", purple_account_get_username   ( acct ) );
    AL_STR( rval, "im-protocol" , purple_account_get_protocol_id( acct ) );
    AL_PTR( rval, "roomlist-uid" , roomlist );

    return response_value( 0, id, name, rval );
}

