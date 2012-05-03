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
#include "status.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

#define CHECK_STATUS(s,i,n,svar,status)                                 \
     if( ((status) > PURPLE_STATUS_UNSET         ) &&                   \
         ((status) < PURPLE_STATUS_NUM_PRIMITIVES)  )                   \
         svar = status;                                                 \
     else                                                               \
     {                                                                  \
         sexp_val_free(s);                                              \
         return response_error( EINVAL, (i), (n), "Invalid Status Type" ); \
     }

xmlnode * _h_elim_status ( const char *name ,
                           const char *id   ,
                           SEXP_VALUE *args ,
                           gpointer    data )
{
    fprintf(stderr, "(elim-debug entered _h_elim_status)");
    ASSERT_ALISTP( args, id, name );
    
    elim_ping();

    const char *sid   = ALIST_VAL_STR( args, "status-id"      );
    const char *mesg  = ALIST_VAL_STR( args, "status-message" );
    signed int _stype = ALIST_VAL_INT( args, "status-type"    );

    PurpleSavedStatus   *status = purple_savedstatus_find( sid );
    PurpleStatusPrimitive stype = PURPLE_STATUS_UNSET;

    // create a new status:
    if( !status )
    {
        CHECK_STATUS ( args, id, name, stype, _stype );
        status = purple_savedstatus_new( sid, stype );
        if( status )
        {
            if     ( mesg ) purple_savedstatus_set_message( status, mesg );
            else if( sid  ) purple_savedstatus_set_message( status, sid  );
        }
    }
    else
    {
        if( mesg ) purple_savedstatus_set_message( status, mesg );
    }

    if( !status )
    {
        sexp_val_free( args );
        return response_error( EINVAL, id, name, "Could not create status" );
    }

    purple_savedstatus_activate( status );

    xmlnode *rval = xnode_new( "alist" );
    AL_STR ( rval, "status-id"     , purple_savedstatus_get_title  (status) );
    AL_STR ( rval, "status-message", purple_savedstatus_get_message(status) );
    AL_ENUM( rval, "status-type"   , purple_savedstatus_get_type   (status) , 
             ":status-primitive"   );
    
    sexp_val_free( args );
    fprintf(stderr, "(elim-debug leaving _h_elim_status)");
    return response_value( 0, id, name, rval );
}
