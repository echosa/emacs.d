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
#include "list_plugins.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_list_plugins ( const char *name , 
                                 const char *id   ,
                                 SEXP_VALUE *args , 
                                 gpointer    data )
{
    GList   *plist   = NULL;
    xmlnode *rval    = xnode_new( "alist" );
    xmlnode *plugins = xnode_new( "alist" );

    elim_ping();

    AL_NODE( rval, "plugins", plugins );

    for( plist = purple_plugins_get_all(); plist; plist = g_list_next( plist ) )
    {
        PurplePlugin *p = plist->data;
        const gchar *id = purple_plugin_get_id( p );
        xmlnode     *pn = NULL;

        switch (p->info->type)
        {
          case PURPLE_PLUGIN_LOADER  :
          case PURPLE_PLUGIN_PROTOCOL:
            purple_debug_info( "plugins", "%s elided from plugin list\n", id );
            break;
          case PURPLE_PLUGIN_STANDARD:
            pn = xnode_new( "alist" );
            AL_NODE( plugins, id , pn );
            AL_BOOL( pn, "loaded", purple_plugin_is_loaded  (p) );
            AL_STR ( pn, "desc"  , purple_plugin_get_summary(p) );
            break;
          default:
            purple_debug_warning( "plugins", "%s: unknown plugin type\n", id );
        }
    }

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
