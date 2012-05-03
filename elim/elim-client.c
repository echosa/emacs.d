/*
Copyright © 2009,2010 Vivek Dasmohapatra

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
#include "elim-client.h"

#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>

#define IO_IN  ( G_IO_IN  | G_IO_PRI )
#define IO_ERR ( G_IO_ERR | G_IO_HUP | G_IO_NVAL )
#define IO_OUT   G_IO_OUT
#define CHUNK  4096

// ***************************************************************************
static GQueue         out_queue ;
static SEXP           parser    ;
static GIOChannel    *out       ;

// ***************************************************************************
static sexp_func get_handler ( const char *name );

static gboolean sexp_xmitter (GIOChannel *io, GIOCondition cond, gpointer data);
static gboolean sexp_handler (GIOChannel *io, GIOCondition cond, gpointer data);
static gboolean quit_handler (GIOChannel *in, GIOCondition cond, gpointer data);
// ***************************************************************************
static sexp_func get_handler ( const char *name )
{
    func_handler *handler;
    sexp_func     func   = NULL;

    if( name && *name )
        for( handler = handlers; !func && handler->name; handler++ )
            if( !strcmp( name, handler->name ) ) func = handler->func;

    return func ? func : _h_elim_default;
}

guint add_outbound_sexp ( xmlnode *resp )
{
    guint    qlen = 0;
    GString *sexp = NULL;

    if( !resp ) return g_queue_get_length( &out_queue );

    sexp = xnode_to_sexp_gstring( resp );
    xnode_free( resp );

    if( sexp->len == 0 )
    {
        g_string_free( sexp, TRUE ); 
        return g_queue_get_length( &out_queue );
    }

    g_queue_push_head ( &out_queue, sexp );

    // start up the outbound sexp pump if this is the only thing
    // in the queue (ie it was empty but now it's not):
    if( (qlen = g_queue_get_length( &out_queue )) == 1 )
        g_io_add_watch( out, IO_OUT, (GIOFunc)sexp_xmitter, NULL );
    
    return qlen;
}

static gboolean sexp_xmitter(GIOChannel *io, GIOCondition cond, gpointer data)
{
    GString *sexp = g_queue_peek_tail( &out_queue );
    int      fd   = g_io_channel_unix_get_fd( io );

    if( sexp )
    {
        ssize_t sent = write( fd, sexp->str, sexp->len );

        // no point trying to cope with errors other than EAGAIN & EINTR:
        // and EAGAIN means we have nothing to do here:
        if( sent < 0 )
        {
            switch( errno )
            {
              case EAGAIN:
              case EINTR :
                return TRUE;
              default:
                perror( "during write to output fd" );
                close( fd );
                return FALSE;
            }
        }

        // hooray: we sent the whole sexp. free it and remove from the queue
        if( sent == sexp->len )
        {
            g_string_free   ( sexp, TRUE );
            g_queue_pop_tail( &out_queue );
        }
        else if( sent )
        {
            g_string_erase( sexp, 0, sent );
        }
    }

    // remove ourselves if nothing is left in the queue:
    return g_queue_is_empty( &out_queue ) ? FALSE : TRUE;
}

static gboolean sexp_handler(GIOChannel *io, GIOCondition cond, gpointer data)
{
    int      fd  = g_io_channel_unix_get_fd( io );
    gboolean go  = TRUE;
    char     buf[CHUNK];

    while( go )
    {
        ssize_t len = read( fd, buf, CHUNK );
        if     ( len == 0 ) { go = FALSE; }
        else if( len <  0 )
        {
            switch( errno )
            {
              case EAGAIN:
              case EINTR :
                go = FALSE;
                break;
              default:
                perror( "during read from input fd" );
                close( fd );
                return FALSE;
            }
        }
        else 
        {
            gboolean parse  = TRUE;

            // any unused data gets cached in the parser data structure, 
            // so buf only needs to be passed in once:
            xnode_from_sexp_chunk( buf, &parser, len );

            // now loop until we can't extract another complete
            // s-expression from the buffer:
            while( (parse = (parser.state == SEXP_PARSED)) )
            {
                xmlnode    *root = parser.root;
                xmlnode    *meth = xnode_first_child_tag( root );
                if( meth )
                {
                    sexp_func func =
                      strcmp( "function-call", root->name ) ? 
                        _h_elim_response : get_handler( meth->name );
                    fprintf( stderr     , 
                             "function: %s; handler: %p; (default: %p)\n", 
                             meth->name      , 
                             func            ,
                             _h_elim_default );
                    if( func )
                    {   // handlers are responsible for freeing *args
                        // when they are done with it:
                        const char *id   = xnode_get_attrib( meth, "id" );
                        xmlnode    *argl = xnode_get_next_sibling( meth );
                        SEXP_VALUE *args = argl ? sexp_value( argl ) : NULL;
                        xmlnode    *resp = (func)( meth->name, id, args, NULL );
                        if( resp ) add_outbound_sexp( resp );
                    }
                }
                // free up the parser related state now we're done with it:
                xnode_free( parser.root );
                sexp_rset ( &parser     );
                // see if there's another complete s-expression in the buffer:
                xnode_from_sexp_chunk( "", &parser, 0 );
            }
        }
    }
    
    return TRUE;
}

static gboolean quit_handler(GIOChannel *in, GIOCondition cond, gpointer data)
{
    g_main_loop_quit( (GMainLoop *)data );
    return FALSE;
}

static void reap_child (int sig) { int s; waitpid( -1, &s, WNOHANG ); }

static void set_signal_handlers ()
{
    struct sigaction sig_child;

    sigemptyset( &sig_child.sa_mask );
    sig_child.sa_handler = reap_child;
    sig_child.sa_flags   = SA_NOCLDSTOP | SA_NOCLDWAIT;
    sigaction( SIGCHLD, &sig_child, NULL );
}

int main ( int argc, char **argv )
{
    int         ifd    = fileno( stdin  );
    int         ofd    = fileno( stdout );
    GMainLoop  *gmloop = g_main_loop_new( NULL, FALSE );
    GError     *error  = NULL;
    GIOChannel *in     ;
    GIOFlags    flags  ;
    GIOStatus   status ;

    set_signal_handlers();

#ifdef G_PLATFORM_WIN32
    // no idea if this works or not.
    in  = g_io_channel_win32_new_socket( (HANDLE)win32_get_osfhandle(ifd) );
    out = g_io_channel_win32_new_socket( (HANDLE)win32_get_osfhandle(ofd) );
#else
    in  = g_io_channel_unix_new( ifd );
    out = g_io_channel_unix_new( ofd );
#endif

    // ***********************************************************************
    ELIM_G_QUEUE_INIT( &out_queue );

    // ***********************************************************************
    flags  = g_io_channel_get_flags( in );
    status = g_io_channel_set_flags( in, flags | G_IO_FLAG_NONBLOCK, &error );

    if( status != G_IO_STATUS_NORMAL )
    {
        fprintf( stderr , 
                 "Setting O_NONBLOCK on stdin: [%d] %s\n" ,
                 error->code, error->message );
        g_clear_error( &error );
        exit( error->code );
    }
    // ***********************************************************************

    g_io_add_watch  ( in, IO_IN , (GIOFunc)sexp_handler, NULL   );
    g_io_add_watch  ( in, IO_ERR, (GIOFunc)quit_handler, gmloop );
    sexp_init       ( &parser );
    g_main_loop_run ( gmloop  );
    sexp_exit       ( &parser );
    purple_plugins_unload_all();
    purple_core_quit();
    exit( 0 );
}
