/*
Copyright © 2010 Vivek Dasmohapatra

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
#include "typing.h"

#define XSIG( instance, signal, event ) \
     purple_signal_connect( instance                             , \
                            "buddy-" #signal                     , \
                            &handle                              , \
                            PURPLE_CALLBACK(_elim_signal_typing) , \
                            (gpointer) event                     );
static int  handle;

static void _elim_signal_typing    ( PurpleAccount *acct ,
                                     const char    *who  ,
                                     gpointer       data );

void elim_typing_signals_init (void)
{
    gpointer instance = purple_conversations_get_handle();

    XSIG( instance, typing        , PURPLE_TYPING     );
    XSIG( instance, typed         , PURPLE_TYPED      );
    XSIG( instance, typing-stopped, PURPLE_NOT_TYPING );
}

static void _elim_signal_typing ( PurpleAccount *acct  ,
                                  const char    *who   ,
                                  gpointer      data )
{
    PurpleTypingState  state = (PurpleTypingState) data;
    PurpleConversation *conv =
      purple_find_conversation_with_account (PURPLE_CONV_TYPE_IM, who, acct);

    if (conv)
    {
        xmlnode *alist = xnode_new( "alist" );
        char    *ID    = new_elim_id();

        AL_PTR ( alist, "account-uid" , acct );
        AL_PTR ( alist, "conv-uid"    , conv );
        AL_ENUM( alist, "typing-state", state, ":typing-state" );

        xmlnode *mcall = func_call( "elim-typing-update", ID, alist );
        g_free( ID );
        add_outbound_sexp( mcall );
    }
}

