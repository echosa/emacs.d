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
#include "join_chat.h"
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
        fprintf( stderr, "(elim chat-component: (%s . %s) )\n", k, v );
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

xmlnode * _h_elim_join_chat( const char *name , 
                             const char *id   ,
                             SEXP_VALUE *args , 
                             gpointer    data )
{
    ASSERT_ALISTP( args, id, name );

    elim_ping();

    PurpleChat *chat  = NULL;
    const char *cname = NULL;
    const char *aname = ALIST_VAL_STR( args, "account-name" );
    const char *proto = ALIST_VAL_STR( args, "im-protocol"  );
    const char *alias = ALIST_VAL_STR( args, "chat-alias"   );
    gpointer    auid  = ALIST_VAL_PTR( args, "account-uid"  );
    gpointer    cuid  = ALIST_VAL_PTR( args, "bnode-uid"    );
    GHashTable *opts  = NULL;
    GHashTable *_opts = NULL;

    PurpleAccount *acct =
      auid ? find_acct_by_uid( auid ) : purple_accounts_find( aname, proto );

    if( !acct )
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "unknown account" );
    }

    // when we have a UID fetch it and sanity check it:
    if( cuid )
    {
        PurpleBlistNode *blnode = find_blist_node_by_uid( cuid, TRUE );
        if( !blnode )
        {
            sexp_val_free( args );
            return response_error( ENXIO, id, name, "rogue chat pointer" );
        }

        if( purple_blist_node_get_type(blnode) != PURPLE_BLIST_CHAT_NODE )
        {
            sexp_val_free( args );
            return response_error( EINVAL, id, name, "buddy is not chat" );
        }

        chat  = (PurpleChat *)blnode;
        cname = purple_chat_get_name      ( chat );
        opts  = purple_chat_get_components( chat );
    }
    else if( (_opts = ALIST_VAL_ALIST( args, "chat-options" )) )
    {
        PurpleChat *ch_2  = NULL;

        // create a purplechat so we can figure out its canonical name:
        // (this is the only way I can see to do this since chat naming
        // is delegated to the prpl protocol plugin chat code):
        opts  = __ghash_str_sexp__str_str( _opts );
        chat  = purple_chat_new( acct, alias, opts );
        cname = purple_chat_get_name  ( chat );
        // now see if we already have a matching chat in this account:
        ch_2  = purple_blist_find_chat( acct , cname );

        // we had a match: destroy the duplicate we just created:
        if( ch_2 )
        {
            purple_blist_remove_chat( chat ); // frees opts by side effect
            purple_blist_alias_chat ( ch_2, alias );
            chat = ch_2;
            opts = purple_chat_get_components( ch_2 );
        }
        else if( chat ) { purple_blist_add_chat( chat, NULL, NULL ); }
        else            { g_hash_table_destroy( opts );              }
    }

    if( !chat || !opts )
    {
        sexp_val_free( args );
        return response_error( EINVAL, id, name, "unable to initialise chat" );
    }

    // if we have a conversation already, prod the client to show it
    fprintf( stderr, "(elim-join-chat looking for conversation)\n" );
    PurpleConversation *conv =
      purple_find_conversation_with_account(PURPLE_CONV_TYPE_CHAT, cname, acct);
    if( conv )
        purple_conversation_present( conv );

    fprintf( stderr, "(elim-join-chat conversation %p)\n", conv );
    // actually join the chat in question:
    // this should result in a conversation being created asynchronously if
    // we weren't in one already:
    serv_join_chat( purple_account_get_connection(acct), opts );
    fprintf( stderr, "(elim-join-chat called serv_join_chat)\n" );

    xmlnode *rval = xnode_new( "alist" );
    AL_PTR( rval, "account-uid" , acct );
    AL_STR( rval, "account-name", purple_account_get_username   (acct) );
    AL_STR( rval, "im-protocol" , purple_account_get_protocol_id(acct) );
    AL_STR( rval, "chat-name"   , cname );

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
