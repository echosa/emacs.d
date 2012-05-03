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
#include "list_protocols.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_list_protocols ( const char *name ,
                                   const char *id   ,
                                   SEXP_VALUE *args ,
                                   gpointer data    )
{
    xmlnode *rval = xnode_new( "alist" );
    GList  *plist = NULL;

    elim_ping();

    for( plist = purple_plugins_get_protocols(); plist; plist = plist->next )
    {
        PurplePlugin *plugin = plist->data;
        if( !plugin                              ) continue;
        if( !PURPLE_IS_PROTOCOL_PLUGIN( plugin ) ) continue;

        const char *key = purple_plugin_get_id  ( plugin );
        const char *val = purple_plugin_get_name( plugin );

        AL_STR( rval, key, val );
    }

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
