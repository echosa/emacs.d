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
#include "alias_bnode.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_alias_bnode ( const char *name ,
                                const char *id   ,
                                SEXP_VALUE *args ,
                                gpointer data    )
{
    ASSERT_ALISTP( args, id, name );

    fprintf( stderr, "(elim-alias-bnode)\n" );
    elim_ping();

    const char    *aname = ALIST_VAL_STR( args, "account-name" );
    const char    *proto = ALIST_VAL_STR( args, "im-protocol"  );
    gpointer       auid  = ALIST_VAL_PTR( args, "account-uid"  );
    const char    *alias = ALIST_VAL_STR( args, "alias"        );
    gpointer       b_uid = ALIST_VAL_PTR( args, "bnode-uid"    );
    PurpleAccount *acct  = NULL;
    const char    *b_arg = NULL;
    const char    *bname = NULL;
    const char    *gname = NULL;
    PurpleGroup   *group = NULL;
    PurpleBuddy   *buddy = NULL;
    gboolean       done  = FALSE;

    if( b_uid )
    {
        PurpleBlistNodeType type = PURPLE_BLIST_OTHER_NODE;
        PurpleBlistNode    *node = find_blist_node_by_uid( b_uid, TRUE );

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
          case PURPLE_BLIST_CONTACT_NODE:
            purple_blist_alias_contact ( (PurpleContact *)node, alias );
            done = TRUE;
            break;
          case PURPLE_BLIST_CHAT_NODE   :
            FIND_ACCOUNT( args, id, name, acct, auid, aname, proto );
            BNODE_ACCOUNT_CHECK(chat,(PurpleChat *)node, acct, args, id, name);
            purple_blist_alias_chat( (PurpleChat *)node, alias );
            done = TRUE;
            break;
          case PURPLE_BLIST_BUDDY_NODE  :
            buddy = (PurpleBuddy *)node;
            FIND_ACCOUNT( args, id, name, acct, auid, aname, proto );
            BNODE_ACCOUNT_CHECK( buddy, buddy, acct, args, id, name );
            purple_blist_alias_buddy( (PurpleBuddy *)node, alias );
            done = TRUE;
            break;
          default:
            sexp_val_free( args );
            return response_error( EINVAL, id, name, "Unsupported alias type" );
        }

        if( done )
        {
            xmlnode *rval = xnode_new( "alist" );
            if( acct )
            {
                AL_STR( rval, "account-name", aname );
                AL_STR( rval, "im-protocol" , proto );
                AL_PTR( rval, "account-uid" , acct  );
            }
            AL_PTR ( rval, "bnode-uid"  , buddy );
            AL_STR ( rval, "bnode-alias", alias );
            AL_ENUM( rval, "bnode-type" , type  , ":blist-node-type" );

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

    if( buddy ) { purple_blist_alias_buddy( buddy, alias ); }
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
    AL_STR ( rval, "bnode-alias" , alias );
    AL_ENUM( rval, "bnode-type", PURPLE_BLIST_BUDDY_NODE, ":blist-node-type" );
    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
