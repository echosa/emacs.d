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
#include "buddy_menu.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

static void _add_menu_entry( xmlnode *menu, PurpleMenuAction *entry )
{
    if( !entry->children || !g_list_length( entry->children ) )
    {
        AL_PTR( menu, entry->label, entry->callback );
        return;
    }

    xmlnode *submenu = xnode_new( "alist" );
    AL_NODE( menu, entry->label, submenu  );

    GList *sub = NULL; 

    for( sub = entry->children; sub; sub = sub->next )
    {
        PurpleMenuAction *item = (PurpleMenuAction *)sub->data;
        _add_menu_entry( submenu, item );
    }
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

xmlnode * _h_elim_buddy_menu ( const char *name ,
                               const char *id   ,
                               SEXP_VALUE *args ,
                               gpointer data    )
{
    ASSERT_ALISTP( args, id, name );
    
    const char      *proto = NULL; //ALIST_VAL_STR( args, "im-protocol"  );
    PurpleAccount   *acct  = NULL;
    PurplePlugin    *prpl  = NULL;
    gpointer         a_uid = NULL;  
    gpointer         b_uid = ALIST_VAL_PTR( args, "bnode-uid" );
    PurpleBlistNode *bnode = find_blist_node_by_uid( b_uid, TRUE );
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
    xmlnode *menu = xnode_new( "alist" );
    AL_NODE( rval, "menu"        , menu  );
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
        _add_menu_entry( menu, item );
        _free_buddy_menu_item( item );
    }

    g_list_free( actions );
    actions = purple_blist_node_get_extended_menu( bnode );

    for( entry = actions; entry; entry = entry->next )
    {
        PurpleMenuAction *item = (PurpleMenuAction *)entry->data;
        _add_menu_entry( menu, item );
        _free_buddy_menu_item( item );
    }
    
    g_list_free( actions );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
