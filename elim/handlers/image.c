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
#include "image.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

#define IGET(i,x) purple_imgstore_get_ ## x ( i )

xmlnode * _h_elim_image ( const char *name ,
                          const char *id   ,
                          SEXP_VALUE *args ,
                          gpointer data    )
{
    ASSERT_ALISTP( args, id, name );
    xmlnode *rval = xnode_new( "alist" );
    int image_id  = (int)ALIST_VAL_INT( args, "image-id" );

    if( image_id <= 0 )
    {
        sexp_val_free( args );
        return response_error( EINVAL, id, name, "bad image ID" );
    }

    fprintf( stderr, "searching for image id %d\n", image_id );
    PurpleStoredImage *image = purple_imgstore_find_by_id( image_id );
    
    if( !image )
    {
        sexp_val_free( args );
        return response_error( ENOENT, id, name, "image ID not found" );
    }

    size_t size = IGET( image , size );

    AL_INT ( rval, "image-id"  , image_id );
    AL_INT ( rval, "image-size", size     );
    AL_STR ( rval, "image-file", IGET( image, filename  ) );
    AL_STR ( rval, "image-type", IGET( image, extension ) );
    AL_DATA( rval, "image-data", IGET( image, data      ) , size );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
