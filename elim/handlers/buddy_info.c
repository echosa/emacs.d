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
#include "buddy_info.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_buddy_info ( const char *name ,
                               const char *id   ,
                               SEXP_VALUE *args ,
                               gpointer data    )
{
    ASSERT_ALISTP( args, id, name );
    
    PurpleAccount    *acct  = NULL;
    PurpleConnection *conn  = NULL;
    gpointer          a_uid = NULL;
    gpointer          b_uid = ALIST_VAL_PTR( args, "bnode-uid" );
    PurpleBlistNode  *bnode = find_blist_node_by_uid( b_uid, TRUE );
    PurpleBlistNodeType  bt = PURPLE_BLIST_OTHER_NODE;

    if( !bnode )
        HANDLER_FAIL( args, id, name, ENOENT, "no such buddy" );

    bt = purple_blist_node_get_type( bnode );
    switch( bt )
    {
      case PURPLE_BLIST_BUDDY_NODE:
        a_uid = purple_buddy_get_account( (PurpleBuddy *)bnode );
        break;
      default:
        HANDLER_FAIL( args, id, name, EINVAL, "unsupported buddy type" );
        break;
    }

    FETCH_ACCOUNT( args, id, name, acct, a_uid );
    
    conn = purple_account_get_connection( acct  );

    if( !conn )
        HANDLER_FAIL( args, id, name, ENXIO, "account disconnected" );

    xmlnode *rval = xnode_new( "alist" );
    AL_PTR ( rval, "bnode-uid"   , bnode );
    AL_PTR ( rval, "account-uid" , acct  );
    AL_STR ( rval, "account-name", purple_account_get_username   ( acct ) );
    AL_STR ( rval, "im-protocol" , purple_account_get_protocol_id( acct ) );

    serv_get_info( conn, purple_buddy_get_name( (PurpleBuddy *)bnode ) );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
