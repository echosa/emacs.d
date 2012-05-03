/*
Copyright © 2009-2010 Vivek Dasmohapatra

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
#include "account_menu_action.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_account_menu_action ( const char *name ,
                                        const char *id   ,
                                        SEXP_VALUE *args ,
                                        gpointer data    )
{
    ASSERT_ALISTP( args, id, name );

    PurplePlugin   *prpl = NULL;
    PurpleAccount  *acct = NULL;
    PurpleConnection *gc = NULL;

    const char *proto = ALIST_VAL_STR( args, "im-protocol"  );
    char       *aname = ALIST_VAL_STR( args, "account-name" );
    gpointer    auid  = ALIST_VAL_PTR( args, "account-uid"  );
    gpointer    action= ALIST_VAL_PTR( args, "menu-action"  );

    if     ( auid           ) acct  = find_acct_by_uid( auid );
    else if( aname && proto ) acct  = purple_accounts_find( aname, proto );

    if( !acct )
        HANDLER_FAIL( args, id, name, ENOENT, "no such account" );

    fprintf( stderr, "(account-menu-action : found account %p)\n", acct );

    proto   = purple_account_get_protocol_id( acct  );
    prpl    = find_plugin_by_protocol       ( proto );
    gc      = purple_account_get_connection ( acct  );

    fprintf( stderr, "(account-menu-action : found connection %p)\n", gc );


    fprintf( stderr, "(account-menu-action : looking for actions... )\n" );
    GList   *entry   = NULL;
    GList   *actions = PURPLE_PLUGIN_ACTIONS( prpl, gc );
    gboolean called  = FALSE;
    fprintf( stderr, "(account-menu-action : actions: %p )\n", actions );

    for( entry = actions; entry; entry = entry->next )
    {
        gboolean matched = FALSE;
        PurplePluginAction *item = (PurplePluginAction *)entry->data;
        if( !item ) continue;
        fprintf( stderr, "(account-menu-action : action: %s\n", item->label );

        if( action && (action == item->callback) )
        {
            matched       = TRUE;
            called        = TRUE;
            item->plugin  = prpl;
            item->context = gc;
            (item->callback)( item );
        }

        purple_plugin_action_free( item );
        fprintf( stderr, "  %s [%d])\n", (matched ? "processed" : "skipped"),
                 matched );
    }

    g_list_free( actions );
    fprintf( stderr, "(account-menu-action : DONE )\n" );

    xmlnode *rval = xnode_new( "alist" );
    AL_BOOL( rval, "action-called", called );
    AL_PTR ( rval, "account-uid"  , acct   );
    AL_STR ( rval, "account-name" , purple_account_get_username   ( acct ) );
    AL_STR ( rval, "im-protocol"  , purple_account_get_protocol_id( acct ) );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
