/*
Copyright © 2009, 2010 Vivek Dasmohapatra 

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
#include "eventloop_ui_ops.h"

// =========================================================================

typedef struct _purple_glib_ioclosure
{
    PurpleInputFunction function;
    guint               result;
    gpointer            data;
} purple_glib_ioclosure;

#define PURPLE_GLIB_READ_COND  (G_IO_IN  | G_IO_HUP | G_IO_ERR)
#define PURPLE_GLIB_WRITE_COND (G_IO_OUT | G_IO_HUP | G_IO_ERR | G_IO_NVAL)


#ifdef  GLIB_HAS_ADD_SECONDS
#define G_TIMEOUT_ADD_SECONDS(i,f,d) g_timeout_add_seconds( i, f, d );
#else
#define G_TIMEOUT_ADD_SECONDS(i,f,d) g_timeout_add( (i)*1000, f, d );
#endif//GLIB_HAS_ADD_SECONDS

// =========================================================================

static guint    timeout_add         ( guint       interval ,
                                      GSourceFunc function ,
                                      gpointer data        );
static gboolean timeout_remove      ( guint handle );
static guint    input_add           ( int                  fd        ,
                                      PurpleInputCondition cond      ,
                                      PurpleInputFunction  func      ,
                                      gpointer             user_data );
static gboolean input_remove        ( guint handle );
//static int      input_get_error     ( int   fd, int *error );
static guint    timeout_add_seconds ( guint       interval ,
                                      GSourceFunc function ,
                                      gpointer data        );

static void     __elim_io_destroy   ( gpointer data       ) { g_free(data); }
static gboolean __elim_io_invoke    ( GIOChannel  *source , 
                                      GIOCondition cond   , 
                                      gpointer     data   )
{
    purple_glib_ioclosure *closure     = data;
    PurpleInputCondition   purple_cond = 0;
    
    if( cond & PURPLE_GLIB_READ_COND  ) purple_cond |= PURPLE_INPUT_READ;
    if( cond & PURPLE_GLIB_WRITE_COND ) purple_cond |= PURPLE_INPUT_WRITE;
    
    closure->function( closure->data                      , 
                       g_io_channel_unix_get_fd( source ) ,
                       purple_cond                        );
    return TRUE;
}
// =========================================================================

PurpleEventLoopUiOps elim_eventloop_ui_ops =
{
    timeout_add         ,
    timeout_remove      ,
    input_add           ,
    input_remove        ,
//  input_get_error     ,
    NULL                ,
    timeout_add_seconds ,
    NULL                ,
    NULL                ,
    NULL
};

// =========================================================================

static guint timeout_add ( guint       interval ,
                           GSourceFunc function ,
                           gpointer data        )
{
   return g_timeout_add( interval, function, data );
}

static gboolean timeout_remove ( guint handle )
{
    return g_source_remove( handle );
}

static guint input_add ( int                  fd        ,
                         PurpleInputCondition cond      ,
                         PurpleInputFunction  func      ,
                         gpointer             user_data )
{
    purple_glib_ioclosure *closure  = g_new0( purple_glib_ioclosure, 1 );
    GIOCondition           gio_cond = 0;
    GIOChannel            *channel;
    
    if( cond & PURPLE_INPUT_READ  ) gio_cond |= PURPLE_GLIB_READ_COND ;
    if( cond & PURPLE_INPUT_WRITE ) gio_cond |= PURPLE_GLIB_WRITE_COND;

    closure->function = func;
    closure->data     = user_data;
    channel           = g_io_channel_unix_new( fd );
    closure->result   = g_io_add_watch_full( channel            ,
                                             G_PRIORITY_DEFAULT ,
                                             gio_cond           ,
                                             __elim_io_invoke   ,
                                             closure            ,
                                             __elim_io_destroy  );
    g_io_channel_unref( channel );

    return closure->result;
}

static gboolean input_remove ( guint handle ) 
{
    return g_source_remove( handle );
}

// static int input_get_error ( int fd, int *error ) { return 0; }

static guint timeout_add_seconds ( guint       interval ,
                                   GSourceFunc function ,
                                   gpointer    data     )
{
    return G_TIMEOUT_ADD_SECONDS( interval, function, data );
}
