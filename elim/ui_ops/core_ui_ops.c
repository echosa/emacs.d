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
#include "core_ui_ops.h"

static void        _elim_ui_prefs_init ( void );
static void        _elim_debug_ui_init ( void ); /* Unfortunate necessity. */
static void        _elim_ui_init       ( void );
static void        _elim_quit          ( void );
static GHashTable* _elim_get_ui_info   ( void );

PurpleCoreUiOps elim_core_ui_ops = 
{
    _elim_ui_prefs_init ,
    _elim_debug_ui_init ,
    _elim_ui_init       ,
    _elim_quit          ,
    _elim_get_ui_info   ,
    NULL                ,
    NULL                ,
    NULL
};

static GHashTable *info = NULL;

// most of these are unnecessary
static void        _elim_ui_prefs_init ( void ){}
static void        _elim_debug_ui_init ( void ){} 
static void        _elim_ui_init       ( void ){}
static void        _elim_quit          ( void )
{
    if( info ) g_hash_table_destroy( info );
}
static GHashTable* _elim_get_ui_info   ( void )
{
    if( !info )
    {
        const char *ui  = purple_core_get_ui     ();
        const char *ver = purple_core_get_version();
        info = g_hash_table_new( g_str_hash, g_str_equal );
        g_hash_table_insert( info, "name"   , (char *)ui  );
        g_hash_table_insert( info, "version", (char *)ver );
    }

    return info;
}
