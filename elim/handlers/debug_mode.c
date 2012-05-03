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
#include "debug.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_debug_mode ( const char *name ,
                               const char *id   ,
                               SEXP_VALUE *args ,
                               gpointer data    )
{
    gboolean debug;
    
    if( args )
    {
        debug = 
          ( ( args->type == SEXP_ALIST ) ? ALIST_VAL_BOOL( args, "debug" ) :
            ( args->type == SEXP_BOOL  ) ? args->x.bool                    : 
            !purple_debug_is_enabled() );
    }
    else 
        debug = !purple_debug_is_enabled();

    purple_debug_set_enabled( debug );
    debug = purple_debug_is_enabled();

    sexp_val_free( args );
    xmlnode *rval = xnode_new( "bool" );
    xnode_insert_data( rval, debug ? "1" : "0", 1 );
    return response_value( 0, id, name, rval );
}
