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
#include "command.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_command ( const char *name ,
                            const char *id   ,
                            SEXP_VALUE *args ,
                            gpointer data    )
{
    fprintf(stderr, "(elim-debug entered _h_elim_command)");
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
            sexp_val_free( args );
            return response_error( ENOENT, id, name, "conversation not found" );
        }
        else { pt = purple_conversation_get_type( pc ); }
    }

    PurpleCmdStatus c_s = PURPLE_CMD_STATUS_FAILED;
    const char     *cmd = ALIST_VAL_STRING( args, "command" );
    char           *esc = g_markup_escape_text( cmd, -1 );
    char           *err = NULL;
    const char   *error = NULL;
    c_s = purple_cmd_do_command( pc, cmd, esc, &err );

    if( c_s != PURPLE_CMD_STATUS_OK && (!err || !*err) )
        switch( c_s )
        {
          case PURPLE_CMD_STATUS_FAILED    :
            error = "Command failed";
            break;
          case PURPLE_CMD_STATUS_NOT_FOUND :
            error = "Command not found";
            break;
          case PURPLE_CMD_STATUS_WRONG_ARGS:
            error = "Bad command arguments";
            break;
          case PURPLE_CMD_STATUS_WRONG_PRPL:
            error = "Command not valid for this IM protocol";
            break;
          case PURPLE_CMD_STATUS_WRONG_TYPE:
            error = "Command not valid in this conversation";
            break;
          default:
            error = "Unknown command error";
        }

    xmlnode *rval = xnode_new( "alist" );
    AL_PTR ( rval, "conv-uid"      , pc  );
    AL_STR ( rval, "conv-name"     , purple_conversation_get_name(pc) );

    AL_ENUM( rval, "command-status", c_s , ":cmd-status" );
    AL_STR ( rval, "command-error" , err ? err : error   );
    AL_STR ( rval, "command-line"  , cmd );

    g_free       ( err  );
    g_free       ( esc  );
    sexp_val_free( args );

    fprintf(stderr, "(elim-debug leaving _h_elim_command)");
    return response_value( 0, id, name, rval );
}
