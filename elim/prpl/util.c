/*
Copyright © 2009, 2010 Vivek Dasmohapatra 

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
#include <glib.h>
#include <purple.h>
#include <string.h>

PurpleConversation *find_conv_by_acct_uid( PurpleAccount *acct, gpointer id )
{
    GList              *clist = NULL;
    PurpleConversation *conv  = NULL;

    for( clist = purple_get_conversations(); clist; clist = clist->next )
        if( id == clist->data ) { conv = clist->data; break; }

    return 
      ( conv ? (purple_conversation_get_account(conv) == acct) : FALSE ) ? 
        conv : NULL;
}

PurpleConversation *find_conv_by_uid( gpointer uid )
{
    GList              *clist = NULL;
    PurpleConversation *conv  = NULL;

    for( clist = purple_get_conversations(); clist; clist = clist->next )
        if( uid == clist->data ) { conv = clist->data; break; }

    return conv;
}

PurpleAccount *find_acct_by_uid( gpointer uid )
{
    GList         *alist = NULL;
    PurpleAccount *acct  = NULL;

    for( alist = purple_accounts_get_all(); alist; alist = alist->next )
        if( uid == alist->data ) { acct = alist->data; break; }

    return acct;
}

PurpleBlistNode *find_blist_node_by_uid( gpointer uid, gboolean offline )
{
    PurpleBlistNode *node = NULL;
    PurpleBlistNode *find = (PurpleBlistNode *)uid;
    PurpleBlistNode *root = purple_blist_get_root();

    for( node = root; node; node = purple_blist_node_next(node, offline) )
        if( find == node ) return node;

    return NULL;
}

#define PG(type,thing,arg) purple_ ## type ## _get_ ## thing( arg )
PurpleBlistNode *find_blist_node_clone( PurpleBlistNode *bnode )
{
    PurpleBlistNode *clone = NULL;
    PurpleBlistNode * root = purple_blist_get_root();
    PurpleBlistNodeType  t = PURPLE_BLIST_OTHER_NODE;

    if( !bnode ) return NULL;
    
    t = purple_blist_node_get_type( bnode );

    for( clone = root; clone; clone = purple_blist_node_next(clone, TRUE) )
    {
        if( clone == bnode                         ) continue;
        if( purple_blist_node_get_type(clone) != t ) continue;

        switch( t )
        {
          case PURPLE_BLIST_BUDDY_NODE :
            if( PG(buddy,account,(PurpleBuddy*)clone) !=
                PG(buddy,account,(PurpleBuddy*)bnode) )               continue;
            if( strcmp( PG(buddy,name,(PurpleBuddy*)clone) ,
                        PG(buddy,name,(PurpleBuddy*)bnode) ) )        continue;
            break;
          case PURPLE_BLIST_CHAT_NODE  :
            if( PG(chat, account, (PurpleChat*)clone) !=
                PG(chat, account, (PurpleChat*)bnode)  )              continue;
            if( strcmp( PG(chat, name, (PurpleChat*)clone) ,
                        PG(chat, name, (PurpleChat*)bnode) ) )        continue;
            break;
          case PURPLE_BLIST_GROUP_NODE  :
            if( strcmp( PG(group, name, (PurpleGroup*)clone) ,
                        PG(group, name, (PurpleGroup*)bnode) ) )      continue;
            break;
          case PURPLE_BLIST_CONTACT_NODE:
            if( strcmp( PG(contact, alias, (PurpleContact*)clone) ,
                        PG(contact, alias, (PurpleContact*)bnode) ) ) continue;
            if( PG(contact,priority_buddy, (PurpleContact*)clone) !=
                PG(contact,priority_buddy, (PurpleContact*)bnode)  )  continue;
            break;
          default:
            continue;
        }

        return clone;
    }

    return NULL;
}

PurplePlugin *find_plugin_by_protocol( const char *name )
{
    GList        *plist = NULL;
    PurplePlugin *rval  = NULL;

    if( !name || !*name ) return NULL;

    for( plist = purple_plugins_get_protocols(); plist; plist = plist->next )
    {
        PurplePlugin *plugin = plist->data;
        if( !plugin                              ) continue;
        if( !PURPLE_IS_PROTOCOL_PLUGIN( plugin ) ) continue;
        const char *key = purple_plugin_get_id( plugin );
        if( !key || !*key || strcmp( name, key ) ) continue;
        rval = plugin;
        break;
    }

    return rval;
}
