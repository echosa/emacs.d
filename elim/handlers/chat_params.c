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
#include "chat_params.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_chat_params( const char *name , 
                               const char *id   ,
                               SEXP_VALUE *args , 
                               gpointer    data )
{
    ASSERT_ALISTP( args, id, name );

    elim_ping();

    const char *proto = ALIST_VAL_STR( args, "im-protocol"  );
    
    if( !proto || !*proto )
    {
        sexp_val_free( args );
        return response_error( EINVAL, id, name, "no protocol specified" );
    }
    
    PurplePlugin *plugin = purple_plugins_find_with_id( proto );

    if( !plugin )
    {
        sexp_val_free( args );
        return response_error( ENOENT, id, name, "no matching plugin found" );
    }

    if( !PURPLE_IS_PROTOCOL_PLUGIN(plugin) )
    {
        sexp_val_free( args );
        return response_error( EINVAL, id, name, "not a protocol plugin" );
    }

    // PURPLE_IS_PROTOCOL_PLUGIN ensures that ->info is set:
    PurplePluginProtocolInfo *pinfo = plugin->info->extra_info;

    if( !pinfo || !pinfo->chat_info )
    {
        sexp_val_free( args );
        return response_error( EINVAL, id, name, "incomplete plugin" );
    }

    xmlnode *rval = xnode_new( "alist" );
    xmlnode *param= xnode_new( "alist" );

    AL_STR ( rval, "im-protocol", proto );
    AL_NODE( rval, "parameters" , param );

    GList   *pcel = (pinfo->chat_info)(NULL);
    GList   *ca;
    for( ca = pcel; ca; ca = ca->next )
    {
        struct proto_chat_entry *pce = ca->data;
        if( !pce ) continue;
        xmlnode *PCE = xnode_new( "alist" );
        AL_STR ( PCE, "label"     , pce->label    );
        AL_BOOL( PCE, "required"  , pce->required );
        AL_BOOL( PCE, "secret"    , pce->secret   );
        if( pce->is_int )
        {
            AL_BOOL( PCE, "is-integer" , pce->is_int );
            AL_INT ( PCE, "minimum"    , pce->min    );
            AL_INT ( PCE, "maximum"    , pce->max    );
        }
        AL_NODE( param, pce->identifier, PCE );
        g_free( pce );
    }

    g_list_free( pcel );
    sexp_val_free( args );
    return response_value( 0, id , name, rval );
}
