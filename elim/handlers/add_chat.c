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
#include "add_chat.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

static void __ghash_copy_sexp_str( gpointer key, gpointer value, gpointer data )
{
    char       *k   = g_strdup( (char *)key );
    char       *v   = NULL;
    GHashTable *dst = data;
    SEXP_VALUE *val = value;
    GString    *buf = NULL;

    switch( val->type )
    {
      case SEXP_STRING:
        v = g_strdup( val->x.string->str );
        break;
      case SEXP_INT:
      {
          buf = g_string_new( "    " );
          g_string_printf( buf, "%ld", val->x.integer );
          v   = buf->str;
          g_string_free( buf, FALSE );
      }
      break;
      case SEXP_FLOAT:
      {
          buf = g_string_new( "      " );
          g_string_printf( buf, "%f", val->x.number );
          v   = buf->str;
          g_string_free( buf, FALSE );
      }
      break;
      case SEXP_BOOL:
      {
          buf = g_string_new( "      " );
          g_string_printf( buf, "%d", val->x.bool );
          v   = buf->str;
          g_string_free( buf, FALSE );
      }
      break;
      default:
        fprintf( stderr, "unsupported type in chat component in join call\n" );
    }
    
    if( v ) 
    {
        fprintf( stderr, "(elim-debug chat component: (%s . %s) )\n", k, v );
        g_hash_table_insert( dst, k, v );
    }
}


static GHashTable * __ghash_str_sexp__str_str( GHashTable *src )
{
    GHashTable *dst = 
      g_hash_table_new_full( g_str_hash, g_str_equal, g_free, g_free );

    if( src )
        g_hash_table_foreach( src, __ghash_copy_sexp_str, dst );

    return dst;
}

xmlnode * _h_elim_add_chat( const char *name , 
                            const char *id   ,
                            SEXP_VALUE *args , 
                            gpointer    data )
{
    ASSERT_ALISTP( args, id, name );

    elim_ping();

    const char *aname   = ALIST_VAL_STR  ( args, "account-name" );
    const char *proto   = ALIST_VAL_STR  ( args, "im-protocol"  );
    const char *alias   = ALIST_VAL_STR  ( args, "chat-alias"   );
    gpointer    auid    = ALIST_VAL_PTR  ( args, "account-uid"  );
    GHashTable *opts    = ALIST_VAL_ALIST( args, "chat-options" );
    GHashTable *options = __ghash_str_sexp__str_str( opts );
    PurpleAccount *acct = 
      auid ? find_acct_by_uid( auid ) : purple_accounts_find( aname, proto );

    if( !acct )
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "unknown account" );
    }

    // cook up a chat node. if it's already on our buddy list, uncook it
    // and use the old one instead (name should be unique per account
    // so the operation is reasonable - we cannot supply a name as this
    // parameter can be delegated to the plugin to generate automatically):
    // this will trigger a new_node call, and possibly a remove call 
    PurpleChat *chat = purple_chat_new( acct, alias, options );
    const char *chn  = purple_chat_get_name( chat );
    PurpleChat *ch_2 = purple_blist_find_chat( acct, chn );
    if( ch_2 )
    {
        fprintf( stderr, "(elim-debug chat already exists)\n" );
        purple_blist_remove_chat( chat );
        chat = ch_2;
        chn  = purple_chat_get_name( chat );
        purple_blist_alias_chat( chat, alias );
    }

    fprintf( stderr, "(elim-debug adding chat to blist)\n" );
    purple_blist_add_chat( chat, NULL, NULL );

    // if we have a conversation already, prod the client to show it
    fprintf( stderr, "(elim-debug looking for conversation)\n" );
    PurpleConversation *conv =
      purple_find_conversation_with_account( PURPLE_CONV_TYPE_CHAT, chn, acct );
    if( conv )
        purple_conversation_present( conv );

    xmlnode *rval = xnode_new( "alist" );

    AL_STR( rval, "account-name", purple_account_get_username   (acct) );
    AL_STR( rval, "im-protocol" , purple_account_get_protocol_id(acct) );
    AL_PTR( rval, "account-uid" , acct );
    AL_STR( rval, "chat-name"   , chn  );

    if( conv )
    {
        PurpleConversationType pct = purple_conversation_get_type    ( conv );
        PurpleConnectionFlags  pcf = purple_conversation_get_features( conv );

        AL_PTR ( rval, "conv-uid"     , conv );
        AL_STR ( rval, "conv-name"    , purple_conversation_get_name (conv) );
        AL_STR ( rval, "conv-title"   , purple_conversation_get_title(conv) );
        AL_ENUM( rval, "conv-type"    , pct, ":conversation-type" );
        AL_ENUM( rval, "conv-features", pcf, ":connection-flags"  );
    }

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
