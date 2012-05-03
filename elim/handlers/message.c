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
#include "message.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_message ( const char *name ,
                            const char *id   ,
                            SEXP_VALUE *args ,
                            gpointer data    )
{
    fprintf(stderr, "(elim-debug entered _h_elim_message)");
    ASSERT_ALISTP( args, id, name );
    
    elim_ping();

    const char *aname = ALIST_VAL_STR( args, "account-name" );
    const char *proto = ALIST_VAL_STR( args, "im-protocol"  );
    gpointer    auid  = ALIST_VAL_PTR( args, "account-uid"  );

    PurpleAccount *acct = 
      auid ? find_acct_by_uid( auid ) : purple_accounts_find( aname, proto );

    if( !acct )
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "unknown account" );
    }

    PurpleConversationType pt = PURPLE_CONV_TYPE_UNKNOWN;
    gpointer             cuid = ALIST_VAL_PTR( args, "conv-uid"  );
    const char         *cname = ALIST_VAL_STR( args, "conv-name" );
    PurpleConversation    *pc = find_conv_by_acct_uid( acct, cuid );

    if  ( pc ) pt = purple_conversation_get_type( pc );
    else
    {
        pt = PURPLE_CONV_TYPE_ANY;
        pc = purple_find_conversation_with_account( pt, cname, acct );
        if( !pc )
        {
            pt = PURPLE_CONV_TYPE_IM;
            pc = purple_conversation_new( pt, acct, cname );
        }
        else { pt = purple_conversation_get_type( pc ); }
    }

    if( !pc )
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "new conversation failed" );
    }

    PurpleConvIm   *pci = NULL;
    PurpleConvChat *pcc = NULL;
    const char     *msg = ALIST_VAL_STRING( args, "text" );
    int             len = 0;

    if( msg )
    {
        char *esc = g_markup_escape_text( msg, -1 );

        len = strlen( esc );

        switch( pt )
        {
          case PURPLE_CONV_TYPE_IM:
            pci = purple_conversation_get_im_data( pc );
            purple_conv_im_send( pci, esc );
            break;
          case PURPLE_CONV_TYPE_CHAT:
            pcc = purple_conversation_get_chat_data( pc );
            purple_conv_chat_send( pcc, esc );
            break;
          default:
            g_free       ( esc  );
            sexp_val_free( args );
            return response_error( EINVAL, id, name,
                                   "unknown conversation type" );
        }

        g_free( esc  );
    }

    xmlnode *rval = xnode_new( "alist" );
    AL_INT( rval, "bytes"    , len     );
    AL_PTR( rval, "conv-uid" , pc      );
    AL_STR( rval, "conv-name", purple_conversation_get_name(pc) );

    sexp_val_free( args );
    fprintf(stderr, "(elim-debug leaving _h_elim_message)");
    return response_value( 0, id, name, rval );
}
