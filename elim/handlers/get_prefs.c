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
#include "get_prefs.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

typedef gboolean (*pref_func)( xmlnode *node, const char *name );
typedef struct _pref_handler { char *name; pref_func func; } pref_handler;

static gboolean _pref_idle_rep     ( xmlnode *node, const char *name )
{
    xmlnode *choices = xnode_new( "alist" );
    AL_NODE( node, "pref-choices", choices );
    AL_STR ( choices, "external (OS/display)", "system" );
    AL_STR ( choices, "internal (libpurple)" , "purple" );
    AL_STR ( choices, "none"                 , "none"   );
    return TRUE;
}

static gboolean _pref_auto_rep ( xmlnode *node, const char *name )
{
    xmlnode *choices = xnode_new( "alist" );
    AL_NODE( node, "pref-choices", choices );
    AL_STR ( choices, "Away + Idle" , "awayidle" );
    AL_STR ( choices, "Never"       , "never"    );
    AL_STR ( choices, "Away"        , "away"     );
    return TRUE; 
}

static gboolean _pref_status ( xmlnode *node, const char *name )
{
    xmlnode *choices = xnode_new( "alist" );
    GList   *status  = purple_savedstatuses_get_all();
    AL_NODE( node, "pref-choices", choices );

    for( ; status; status = status->next )
    {
        PurpleSavedStatus *s = status->data;
        const char    *title = purple_savedstatus_get_title        ( s );
        time_t         value = purple_savedstatus_get_creation_time( s );
        AL_INT( choices, title, value );
    }

    return TRUE;     
}

static gboolean _pref_status_or_nil ( xmlnode *node, const char *name )
{
    xmlnode *choices = xnode_new( "alist" );
    GList   *status  = purple_savedstatuses_get_all();
    AL_NODE( node   , "pref-choices" , choices );
    AL_PTR ( choices, "-unspecified-", NULL    );

    for( ; status; status = status->next )
    {
        PurpleSavedStatus *s = status->data;
        const char    *title = purple_savedstatus_get_title        ( s );
        time_t         value = purple_savedstatus_get_creation_time( s );
        AL_INT( choices, title, value );
    }

    return TRUE;     
}
static gboolean _pref_log_format ( xmlnode *node, const char *name )
{
    GList   *logger  = NULL;
    GList   *loggers = purple_log_logger_get_options();
    xmlnode *choices = xnode_new( "alist" );
    AL_NODE( node, "pref-choices", choices );
    for( logger = loggers; logger; logger = logger->next->next )
    {
        const char *name = logger->data;
        const char *id   = logger->next->data;
        AL_STR( choices, name, id );
    }
    g_list_free( loggers );
    return TRUE;
}
static gboolean _pref_proxy_type ( xmlnode *node, const char *name )
{
    xmlnode *choices = xnode_new( "alist" );
    AL_NODE( node   , "pref-choices"    , choices  );
    AL_STR ( choices, "No Proxy"        , "none"   );
    AL_STR ( choices, "HTTP Proxy"      , "http"   );
    AL_STR ( choices, "SOCKS v4"        , "socks4" );
    AL_STR ( choices, "SOCKS v5"        , "socks5" );
    AL_STR ( choices, "From Environment", "envvar" );
    return TRUE;
}
static gboolean _pref_snd_while ( xmlnode *node, const char *name )
{
    xmlnode *choices = xnode_new( "alist" );
    AL_NODE( node   , "pref-choices"   , choices  );
    AL_INT ( choices, "When Available" , 1        );
    AL_INT ( choices, "When Away"      , 2        );
    AL_INT ( choices, "Always"         , 3        );    
    AL_INT ( choices, "Never"          , 4        );
    return TRUE;    
}
static gboolean _pref_meanwhile_bl ( xmlnode *node, const char *name )
{
    xmlnode *choices = xnode_new( "alist" );
    AL_NODE( node, "pref-choices", choices );
    AL_INT ( choices, "Local Storage Only"        , 1 );
    AL_INT ( choices, "Merge From Server"         , 2 );
    AL_INT ( choices, "Merge From+Save To Server" , 3 );
    AL_INT ( choices, "Sync With Server"          , 4 );
    return TRUE;    
}

static pref_handler handlers[] =
  { { "/purple/away/idle_reporting"          , _pref_idle_rep      } ,
    { "/purple/away/auto_reply"              , _pref_auto_rep      } ,
    { "/purple/savedstatus/default"          , _pref_status        } ,
    { "/purple/savedstatus/idleaway"         , _pref_status        } ,
    { "/purple/savedstatus/startup"          , _pref_status_or_nil } ,
    { "/purple/logging/format"               , _pref_log_format    } ,
    { "/purple/proxy/type"                   , _pref_proxy_type    } ,
    { "/purple/sound/while_status"           , _pref_snd_while     } ,
    { "/plugins/prpl/meanwhile/blist_action" , _pref_meanwhile_bl  } ,
    { NULL                                   , NULL                } };

#define MAYBE_IGNORE_PREF( n )                     \
    { if( !strcmp( (n), "/pidgin"      ) ) return; \
      if( !strcmp( (n), "/plugins/gtk" ) ) return; }

static gboolean _munge_special_pref ( xmlnode *node, const char *name )
{
    pref_handler *h = NULL; 
    if( name )
        for( h = handlers; h && h->name; h++ )
            if( !strcmp( name, h->name ) ) return (h->func)( node, name );
    return FALSE;
}

static void _add_pref_data ( xmlnode *node, const char *name )
{
    xmlnode    *entry = NULL;
    GList      *vlist = NULL;
    PurplePrefType pt = purple_prefs_get_type( name );

    MAYBE_IGNORE_PREF( name );

    entry = xnode_new( "alist" );

    AL_NODE( node , name, entry );
    AL_ENUM( entry, "pref-type", pt, ":pref-type" );
    _munge_special_pref( entry, name );

    switch( pt )
    {
      case PURPLE_PREF_BOOLEAN:
        AL_BOOL( entry, "pref-value", purple_prefs_get_bool  ( name ) );
        break;
      case PURPLE_PREF_INT    :
        AL_INT ( entry, "pref-value", purple_prefs_get_int   ( name ) );
        break;
      case PURPLE_PREF_STRING :
        AL_STR ( entry, "pref-value", purple_prefs_get_string( name ) );
        break;
      case PURPLE_PREF_PATH   :
        AL_STR ( entry, "pref-value", purple_prefs_get_path  ( name ) );
        AL_BOOL( entry, "is-path"   , TRUE );
        break;
      case PURPLE_PREF_STRING_LIST:
        vlist = purple_prefs_get_string_list( name );
      case PURPLE_PREF_PATH_LIST  :
        vlist = purple_prefs_get_path_list  ( name );
        AL_BOOL( entry, "is-path"   , TRUE );
      break;
      case PURPLE_PREF_NONE   :
        break;
      default:
        fprintf( stderr, "pref: %s has type %lu (unsupportd)\n", name, 
                 (unsigned long)pt );
    }

    if( vlist )
    {
        GList   *item = NULL;
        xmlnode *list = xnode_new( "list" );
        AL_NODE( entry, "pref-value", list );
        for( item = vlist; item; item = item->next )
        {
            char    *str    = item->data;
            xmlnode *string = xnode_new( "string" );
            xnode_insert_data ( string, str, -1 );
            xnode_insert_child( list  , string  );
            g_free( str );
        }
        g_list_free( vlist );
    }

    GList *labels = purple_prefs_get_children_names( name );
    GList *label  = NULL;

    if( labels )
    {
        xmlnode *kids = xnode_new( "alist" );
        AL_NODE( entry, "pref-children", kids );
        for( label = labels; label; label = label->next )
        {
            char *n = label->data;
            _add_pref_data( kids, n );
            g_free( n );
        }
        g_list_free( labels );
    }
}

xmlnode * _h_elim_get_prefs ( const char *name , 
                              const char *id   ,
                              SEXP_VALUE *args , 
                              gpointer    data )
{
    elim_ping();
    
    const char *pref_name = 
      ( ( args && (args->type == SEXP_ALIST) ) ? 
        ALIST_VAL_STR( args, "pref-name" ) : "/" );
    
    if( !pref_name || !*pref_name )
        pref_name = "/";

    xmlnode *rval  = xnode_new( "alist" );
    xmlnode *prefs = xnode_new( "alist" );

    AL_NODE( rval, "prefs", prefs );

    _add_pref_data( prefs, "/" );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
