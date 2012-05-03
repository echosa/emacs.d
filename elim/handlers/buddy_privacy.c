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
#include "buddy_privacy.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_buddy_privacy ( const char *name ,
                                  const char *id   ,
                                  SEXP_VALUE *args ,
                                  gpointer data    )
{
    ASSERT_ALISTP( args, id, name );
    
    const char       *bname = NULL;
    PurpleAccount    *acct  = NULL;
    PurpleConnection *conn  = NULL;
    gpointer          a_uid = NULL;
    gpointer          b_uid = ALIST_VAL_PTR ( args, "bnode-uid" );
    gboolean          block = ALIST_VAL_BOOL( args, "block" );
    gboolean          allow = ALIST_VAL_BOOL( args, "allow" );

    if( block && allow )
        HANDLER_FAIL( args, id, name, EINVAL, 
                      "user must be blocked or unblocked, not both" );

    if( b_uid )
    {
        PurpleBlistNode  *bnode = find_blist_node_by_uid( b_uid, TRUE );
        PurpleBlistNodeType  bt = PURPLE_BLIST_OTHER_NODE;

        if( !bnode ) 
            HANDLER_FAIL( args, id, name, ENOENT, "no such buddy" );

        bt = purple_blist_node_get_type( bnode );
        switch( bt )
        {
          case PURPLE_BLIST_BUDDY_NODE:
            a_uid = purple_buddy_get_account( (PurpleBuddy *)bnode );
            bname = purple_buddy_get_name   ( (PurpleBuddy *)bnode );
            break;
          default:
            HANDLER_FAIL( args, id, name, EINVAL, "unsupported buddy type" );
            break;
        }
    }
    else
    {
        a_uid = ALIST_VAL_PTR( args, "account-uid" );
        bname = ALIST_VAL_STR( args, "user-name"   );
        if( !bname )
            HANDLER_FAIL( args, id, name, ENOENT, "no user to (un)block" );
    }

    FETCH_ACCOUNT( args, id, name, acct, a_uid );
    if( !( conn = purple_account_get_connection( acct ) ) )
        HANDLER_FAIL( args, id, name, ENXIO, "account disconnected" );


    if     ( block ) purple_privacy_deny ( acct, bname, FALSE, FALSE );
    else if( allow ) purple_privacy_allow( acct, bname, FALSE, FALSE );
    else   // no specific op: toggle
    {
        ( purple_privacy_check( acct, bname ) ?
          purple_privacy_deny ( acct, bname, FALSE, FALSE ) :
          purple_privacy_allow( acct, bname, FALSE, FALSE ) );
    }

    xmlnode *rval = xnode_new( "alist" );
    AL_PTR ( rval, "user-name"   , bname );
    AL_PTR ( rval, "account-uid" , acct  );
    AL_STR ( rval, "account-name", purple_account_get_username   ( acct ) );
    AL_STR ( rval, "im-protocol" , purple_account_get_protocol_id( acct ) );
    AL_BOOL( rval, "blocked"     , !purple_privacy_check( acct, bname ) );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
