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
#include "remove_buddy.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_remove_buddy ( const char *name ,
                                 const char *id   ,
                                 SEXP_VALUE *args ,
                                 gpointer data    )
{
    ASSERT_ALISTP( args, id, name );

    fprintf( stderr, "(elim-remove-buddy)\n" );

    elim_ping();
    
    const char    *aname = ALIST_VAL_STR( args, "account-name" );
    const char    *proto = ALIST_VAL_STR( args, "im-protocol"  );
    gpointer       auid  = ALIST_VAL_PTR( args, "account-uid"  );
    PurpleAccount *acct  = NULL;

    gpointer       b_uid = ALIST_VAL_PTR( args, "bnode-uid" );
    const char    *b_arg = NULL;
    const char    *bname = NULL;
    const char    *gname = NULL; 
    PurpleGroup   *group = NULL; 
    PurpleBuddy   *buddy = NULL;
    gboolean       gone  = FALSE;

    if( b_uid )
    {
        PurpleBlistNodeType type = PURPLE_BLIST_OTHER_NODE;
        PurpleBlistNode    *node = find_blist_node_by_uid( b_uid , TRUE );

        if( !node )
        {
            sexp_val_free( args );
            return response_error( EINVAL, id, name, "rogue buddy pointer" );
        }
        type = purple_blist_node_get_type( node );

        // ===========================================================
        // groups, contacts and chats can safely be removed here:
        // buddies should instead be noted for removal in the next
        // block of code as they require client<->server interaction:
        switch( type )
        {
          case PURPLE_BLIST_GROUP_NODE  :
            purple_blist_remove_group  ( (PurpleGroup   *)node );
            gone = TRUE;
            break;
          case PURPLE_BLIST_CONTACT_NODE:
            purple_blist_remove_contact( (PurpleContact *)node );
            gone = TRUE;
            break;
          case PURPLE_BLIST_CHAT_NODE   :
            FIND_ACCOUNT( args, id, name, acct, auid, aname, proto );
            BNODE_ACCOUNT_CHECK(chat,(PurpleChat *)node, acct, args, id, name);
            purple_blist_remove_chat   ( (PurpleChat    *)node );
            gone = TRUE;
            break;
          case PURPLE_BLIST_BUDDY_NODE  :
            buddy = (PurpleBuddy *)node;
            FIND_ACCOUNT( args, id, name, acct, auid, aname, proto );
            BNODE_ACCOUNT_CHECK( buddy, buddy, acct, args, id, name );
            b_arg = purple_buddy_get_name( buddy );
            bname = purple_normalize( acct, b_arg );
            break;
          default:
            sexp_val_free( args );
            return response_error( EINVAL, id, name, 
                                   "Unknown buddy list node type" );
        }

        if( gone )
        {
            xmlnode *rval = xnode_new( "alist" );
            if( acct )
            {
                AL_STR( rval, "account-name", aname );
                AL_STR( rval, "im-protocol" , proto );
                AL_PTR( rval, "account-uid" , acct  );
            }
            AL_PTR ( rval, "bnode-uid" , buddy );
            AL_ENUM( rval, "bnode-type", type  , ":blist-node-type" );

            sexp_val_free( args );
            return response_value( 0, id, name, rval );
        }
    }
    else
    {
        b_arg = ALIST_VAL_STRING( args, "bnode-name" );
        FIND_ACCOUNT( args, id, name, acct, auid, aname, proto );

        if( b_arg )
        {
            bname = purple_normalize( acct, b_arg   );
            gname = ALIST_VAL_STRING( args, "group" );
            group = ( gname && *gname ) ? purple_find_group( gname ) : NULL;
            buddy = ( group ?
                      purple_find_buddy_in_group( acct, bname, group ) :
                      purple_find_buddy         ( acct, bname        ) );
        }
    }

    if( !b_arg || !*b_arg )
    {
        sexp_val_free( args );
        return response_error( EINVAL, id, name, "buddy not specified" );
    }

    // buddy must be in our local list or libpurple won't remove it from the
    // server list ( determined empirically, confirmed by inspecting code ):
    if( !buddy )
    {
        buddy = purple_buddy_new( acct, bname, bname );
        purple_blist_add_buddy  ( buddy, NULL, NULL, NULL );
    }

    if( buddy )
    {
        // the order of the remove operations is important: it has to be
        // this way round, as noted above: account buddy removal won't 
        // happen if the buddy is not in the blist when we try:
        if( !group ) group = purple_buddy_get_group( buddy );
        // is this correct? what if we have more than one copy of said buddy?
        // potentially confusing. dunno what the right thing to do is here.
        purple_account_remove_buddy( acct, buddy, group );
        purple_blist_remove_buddy( buddy );
    }
    else 
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "no such buddy" );
    }
    
    xmlnode *rval = xnode_new( "alist" );
    AL_STR ( rval, "account-name", purple_account_get_username   ( acct ) );
    AL_STR ( rval, "im-protocol" , purple_account_get_protocol_id( acct ) );
    AL_PTR ( rval, "account-uid" , acct  );
    AL_PTR ( rval, "bnode-uid"   , buddy );
    AL_ENUM( rval, "bnode-type", PURPLE_BLIST_BUDDY_NODE, ":blist-node-type" );
    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
