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
#include "buddy_menu_action.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

static PurpleMenuAction * 
_check_menu_entry( PurpleMenuAction *entry, gpointer cb_uid )
{
    if( !entry->children || !g_list_length( entry->children ) )
        return ( cb_uid == (gpointer)entry->callback ) ? entry : NULL;
    
    GList            *sub   = NULL; 
    PurpleMenuAction *found = NULL;

    for( sub = entry->children; sub; sub = sub->next )
    {
        PurpleMenuAction *item = (PurpleMenuAction *)sub->data;
        if( ( found = _check_menu_entry( item, cb_uid ) ) )
            return found;
    }

    return NULL;
}

static void _free_buddy_menu_item (PurpleMenuAction *entry)
{
    if( entry->children )
    {
        GList *sub = NULL; 
        for( sub = entry->children; sub; sub = sub->next )
        {
            PurpleMenuAction *item = (PurpleMenuAction *)sub->data;
            _free_buddy_menu_item( item );
        }
        g_list_free( entry->children );
    }

    purple_menu_action_free( entry );
}

xmlnode * _h_elim_buddy_menu_action ( const char *name ,
                                      const char *id   ,
                                      SEXP_VALUE *args ,
                                      gpointer data    )
{
    ASSERT_ALISTP( args, id, name );
    
    const char      *proto = NULL; //ALIST_VAL_STR( args, "im-protocol"  );
    PurpleAccount    *acct  = NULL;
    PurplePlugin     *prpl  = NULL;
    gpointer          a_uid = NULL;  
    gpointer          b_uid = ALIST_VAL_PTR( args, "bnode-uid"   );
    gpointer          c_uid = ALIST_VAL_PTR( args, "menu-action" );
    PurpleMenuAction *found = NULL;
    PurpleBlistNode  *bnode = find_blist_node_by_uid( b_uid, TRUE );
    PurpleBlistNodeType bt = PURPLE_BLIST_OTHER_NODE;
    PurplePluginProtocolInfo *pppi = NULL;

    if( !bnode )
        HANDLER_FAIL( args, id, name, ENOENT, "no such buddy" );

    bt = purple_blist_node_get_type( bnode );
    switch( bt )
    {
      case PURPLE_BLIST_CHAT_NODE:
        a_uid = purple_chat_get_account ( (PurpleChat  *)bnode );
        break;
      case PURPLE_BLIST_BUDDY_NODE:
        a_uid = purple_buddy_get_account( (PurpleBuddy *)bnode );
        break;
      default:
        break;
    }

    FETCH_ACCOUNT( args, id, name, acct, a_uid );
    
    proto = purple_account_get_protocol_id( acct  );
    prpl  = find_plugin_by_protocol       ( proto );
    pppi  = PURPLE_PLUGIN_PROTOCOL_INFO   ( prpl  );
    if( !pppi )
        HANDLER_FAIL( args, id, name, EINVAL, "bad protocol plugin" );

    xmlnode *rval = xnode_new( "alist" );
    AL_PTR ( rval, "bnode-uid"   , bnode );
    AL_PTR ( rval, "account-uid" , acct  );
    AL_STR ( rval, "account-name", purple_account_get_username   ( acct ) );
    AL_STR ( rval, "im-protocol" , purple_account_get_protocol_id( acct ) );

    GList   *entry   = NULL;
    GList   *actions = 
      pppi->blist_node_menu ? pppi->blist_node_menu( bnode ) : NULL;

    for( entry = actions; entry; entry = entry->next )
    {
        PurpleMenuAction *item = (PurpleMenuAction *)entry->data;
        if( !found ) found = _check_menu_entry( item, c_uid );
        if( !found ) _free_buddy_menu_item( item );
    }

    g_list_free( actions );
    actions = purple_blist_node_get_extended_menu( bnode );

    for( entry = actions; entry; entry = entry->next )
    {
        PurpleMenuAction *item = (PurpleMenuAction *)entry->data;
        if( !found ) found = _check_menu_entry( item, c_uid );
        if( !found ) _free_buddy_menu_item( item );
    }
    
    if( found && found->callback )
    {
        void (*callback)(gpointer, gpointer) = (gpointer)found->callback;
        (callback)( bnode, found->data );
        _free_buddy_menu_item( found );
        AL_PTR( rval, "menu-action", callback );
    }
    
    g_list_free( actions );
    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
