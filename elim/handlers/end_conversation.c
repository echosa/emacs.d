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
#include "end_conversation.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_end_conversation ( const char *name ,
                                     const char *id   ,
                                     SEXP_VALUE *args ,
                                     gpointer data    )
{
    fprintf(stderr, "(elim-debug entered _h_elim_end_conversation)");
    ASSERT_ALISTP( args, id, name );
    
    elim_ping();

    gpointer            cuid = ALIST_VAL_PTR( args, "conv-uid"  );
    PurpleConversation *conv = find_conv_by_uid( cuid );

    if( !conv )
    {
        sexp_val_free( args );
        return response_error( ENOENT, id, name, "no such conversation" );
    }

    purple_conversation_destroy( conv );

    sexp_val_free( args );

    xmlnode *rval = xnode_new( "bool" );
    xnode_insert_data( rval,  "1",  1 );
    fprintf(stderr, "(elim-debug leaving _h_elim_end_conversation)");
    return response_value( 0, id, name, rval );
}
