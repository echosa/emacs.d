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
#include "init.h"
#include "../ui_ops/ops.h"
#include "../signals/sigs.h"

static void _h_elim_warning( const gchar *msg ) { fputs( msg, stderr ); }

xmlnode * _h_elim_init ( const char *name ,
                         const char *id   ,
                         SEXP_VALUE *args ,
                         gpointer data    )
{
    ASSERT_ALISTP( args, id, name );

    char    *dir = ALIST_VAL_STRING( args, "dot-dir" );
    char    *ui  = ALIST_VAL_STRING( args, "ui-id"   );
    gboolean dbg = ALIST_VAL_BOOL  ( args, "debug"   );

    if( !ui ) { ui = "elim"; }

    // libpurple initialisation:
    purple_util_set_user_dir       ( dir  );
    purple_util_init               ();
    purple_core_set_ui_ops         ( &elim_core_ui_ops         );
    purple_eventloop_set_ui_ops    ( &elim_eventloop_ui_ops    );
    purple_blist_set_ui_ops        ( &elim_blist_ui_ops        );
    purple_accounts_set_ui_ops     ( &elim_account_ui_ops      );
    purple_request_set_ui_ops      ( &elim_request_ui_ops      );
    purple_idle_set_ui_ops         ( &elim_idle_ui_ops         );
    purple_connections_set_ui_ops  ( &elim_connections_ui_ops  );
    purple_conversations_set_ui_ops( &elim_conversation_ui_ops );
    purple_notify_set_ui_ops       ( &elim_notify_ui_ops       );
    purple_roomlist_set_ui_ops     ( &elim_roomlist_ui_ops     );

    // load any data for init:
    if( purple_get_core() == NULL )
    {
        // purple debug goes to stdout if we don't divert it here:
        g_set_print_handler( (GPrintFunc)_h_elim_warning );
        // look for plugins in user specified directory tree:
        char *ppath = g_build_filename( purple_user_dir(), "plugins", NULL );
        purple_plugins_add_search_path ( ppath );
        purple_debug_set_enabled( dbg );
        purple_core_init ( ui );
        purple_set_blist ( purple_blist_new() );
        purple_prefs_load();
        purple_blist_load();
        // glib signal initialisation:
        elim_ft_signals_init();
        elim_typing_signals_init();
        // tidy up:
        g_free( ppath );
    }
    else
    {
        const char *cur_ui = purple_core_get_ui();
        if( strcmp( cur_ui, name ) )
        {
            sexp_val_free( args );
            return response_error( EINVAL, id, name, 
                                   "purple has already been initialised" );
        }
    }

    sexp_val_free( args );
    xmlnode *rval = xnode_new( "alist" );
    AL_STR( rval, "ui-id", purple_core_get_ui() );
    return response_value( 0, id, name, rval );
}
