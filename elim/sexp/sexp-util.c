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
#include "sexp-util.h"

#define BASE64_ENCODE(x,y) (char *)g_base64_encode(((guchar *)x),(y))
#define BASE64_DECODE(x,y) (char *)g_base64_decode((x),(y))

static char * _sexp_to_str( SEXP_VALUE *sexp, int depth, const char *name );

typedef struct _ITEM ITEM;
struct _ITEM
{
    GString *str;
    guint    depth;
};

SEXP_TYPE_MAP type_map[] = { { "string", SEXP_STRING  } ,
                             { "int"   , SEXP_INT     } ,
                             { "float" , SEXP_FLOAT   } ,
                             { "bool"  , SEXP_BOOL    } ,
                             { "data"  , SEXP_B64     } ,
                             { "alist" , SEXP_ALIST   } ,
                             { "list"  , SEXP_LIST    } , 
                             {  NULL   , SEXP_UNKNOWN } };

static sexp_type _tag_to_type ( const char *name )
{
    sexp_type      type  = SEXP_UNKNOWN;
    SEXP_TYPE_MAP *entry;

    if( name && *name )
        for( entry = type_map; (type == SEXP_UNKNOWN) && entry->name; entry++ )
            if( !strcmp( name, entry->name ) ) type = entry->type;

    return type;
}

void sexp_val_glist_free ( gpointer v, gpointer x ) { sexp_val_free( v ); }

void sexp_val_free ( gpointer ptr )
{
    SEXP_VALUE *value = ptr;
    if( !value ) return;
    switch( value->type )
    {
      case SEXP_INT    :
      case SEXP_FLOAT  :
      case SEXP_BOOL   :
      case SEXP_UNKNOWN:
        break;
      case SEXP_STRING :
        g_string_free( value->x.string, TRUE );
        break;
      case SEXP_B64    :
        g_string_free( value->x.data  , TRUE );
        break;
      case SEXP_ALIST  :
        g_hash_table_unref( value->x.hash );
        break;
      case SEXP_LIST   :
        g_list_foreach( value->x.list, sexp_val_glist_free, NULL );
        g_list_free   ( value->x.list );
        break;
      default:
        // theoretically this can never happen, as we should
        // have short-circuited above at SEXP_UNKNOWN:
        fprintf( stderr, "unsupported sexp type '%d'\n", value->type );
        value = NULL;
        break;
    }
    g_free( value );
}

void _sexp_item_to_str( gpointer key, gpointer value, gpointer udata )
{
    const char *k = key;
    SEXP_VALUE *v = value;
    ITEM       *i = udata;
    GString  *str = i->str;
    guint   depth = i->depth;
    char     *val = NULL;
    char     *pad = NULL;

    pad = depth ? g_strnfill( depth * 2, ' ' ) : "";
    val = _sexp_to_str( v  , (v->type == SEXP_ALIST || 
                              v->type == SEXP_LIST   ) ? depth + 1 : 0, k );
    g_string_append   ( str, pad );
    g_string_append   ( str, val );
    g_free            ( val      );
    if( depth ) g_free( pad );
}

void _sexp_listent_to_str( gpointer value, gpointer udata )
{
    SEXP_VALUE *v = value;
    ITEM       *i = udata;
    GString  *str = i->str;
    guint   depth = i->depth;
    char     *val = NULL;
    char     *pad = NULL;

    pad = depth ? g_strnfill( depth * 2, ' ' ) : "";

    val = _sexp_to_str    ( v  , (v->type == SEXP_ALIST || 
                                  v->type == SEXP_LIST   ) ? depth : 0, NULL );
    g_string_append_printf( str, "\n%s%s", pad, val );
    g_free                ( val      );

    if( depth ) g_free( pad );
}

static char * _sexp_to_str( SEXP_VALUE *sexp, int depth, const char *name )
{
    if( !sexp ) return NULL;

    GString *str = g_string_new( "" );
    char    *pad = NULL;
    char    *k   = NULL;
    GString *key = NULL;
    ITEM     item;

    if( depth )
    {
        pad = g_strnfill     ( depth * 2, ' ' );
        str = g_string_append( str , pad      );
    }
    else { pad = ""; }

    if( name )
    {
        key = g_string_sized_new( 64 );
        g_string_printf( key, "((name . \"%s\"))", name );
        k   = key->str;
    }
    else { k = "nil"; }

    switch( sexp->type )
    {
      case SEXP_INT    :
        g_string_append_printf( str,"(int    %s \"%ld\")", k, sexp->x.integer );
        break;
      case SEXP_FLOAT  :
        g_string_append_printf( str,"(float  %s \"%f\")" , k, sexp->x.number  );
        break;
      case SEXP_BOOL   :
        g_string_append_printf( str,"(bool   %s \"%d\")" , k, sexp->x.bool    );
        break;
      case SEXP_STRING :
        g_string_append_printf( str,"(string %s \"%s\")", k,  
                                sexp->x.string->str         );
        break;
      case SEXP_B64    :
        {
            char *enc = NULL;
            if( (enc = BASE64_ENCODE(sexp->x.data->str, sexp->x.data->len)) )
            {
                g_string_append_printf( str, "(data   %s \"%s\")" , k, enc );
                g_free( enc );
            }
        }
        break;
      case SEXP_ALIST  :
        {
            item.str   = str;
            item.depth = depth + 1;
            g_string_append_printf( str, "%s%s(alist %s ", 
                                    ( depth ? "\n" : "" ) , pad, k );
            g_hash_table_foreach  ( sexp->x.hash, _sexp_item_to_str, &item );
            g_string_append       ( str, ")" );
        }
        break;
      case SEXP_LIST   :
        //
        {
            item.str   = str;
            item.depth = depth + 1;
            g_string_append_printf( str, "\n%s(list %s ", pad, k );
            g_list_foreach        ( sexp->x.list, _sexp_listent_to_str, &item );
            g_string_append       ( str, ")"        );
        }
        break;
      default:
        // theoretically this can never happen, as we should
        // have short-circuited above at SEXP_UNKNOWN:
        fprintf( stderr, "unsupported sexp type '%d'\n", sexp->type );
        break;
    }

    if( depth ) g_free( pad );
    if( key   ) g_string_free( key, TRUE );

    return g_string_free( str, FALSE );
}

char * sexp_to_str ( SEXP_VALUE *sexp )
{
    return _sexp_to_str( sexp, 0, NULL );
}

SEXP_VALUE * sexp_value( xmlnode *node )
{
    sexp_type   type  = SEXP_UNKNOWN;
    SEXP_VALUE *value = NULL;
    char       *data  = NULL;

    g_return_val_if_fail( node, NULL );

    type = _tag_to_type( node->name );
    if( type == SEXP_UNKNOWN ) return NULL;

    data        = xnode_get_data( node );
    value       = g_new0( SEXP_VALUE, 1 );
    value->type = type;

    switch( type )
    {
      case SEXP_STRING:
        value->x.string = g_string_new( data );
        break;
      case SEXP_B64:
        {
            gsize len;
            char *raw = NULL; 
            if( (raw = BASE64_DECODE( data, &len )) )
            {
                value->x.data = g_string_new_len( raw, len );
                g_free( raw );
            }
        };
        break;
      case SEXP_INT:
        value->x.integer = atol( data );
        break;
      case SEXP_FLOAT:
        {
            char  *e = NULL;
            double x = strtod( data, &e );
            value->x.number = x;
        }
        break;
      case SEXP_BOOL:
        value->x.bool = atoi( data ) ? TRUE : FALSE;
        break;
      case SEXP_ALIST:
        {
            xmlnode    *item = xnode_first_child_tag( node );
            GHashTable *hash =
              g_hash_table_new_full( g_str_hash, g_str_equal   , 
                                     g_free    , sexp_val_free );
            while( item )
            {
                const char *name  = xnode_get_attrib( item , "name" );
                SEXP_VALUE *val   = sexp_value( item );
                char       *key   = g_strdup  ( name );
                g_hash_table_replace( hash, key, val );
                item = xnode_get_next_sibling ( item );
            }
            value->x.hash = hash;
        };
        break;
      case SEXP_LIST:
        {
            GList *list = NULL;
            xmlnode * item = xnode_first_child_tag( node );
            while( item )
            {
                list = g_list_append( list, sexp_value( item ) );
                item = xnode_get_next_sibling( item );
            }
            value->x.list = list;
        }
        break;
      default:
        // theoretically this can never happen, as we should
        // have short-circuited above at SEXP_UNKNOWN:
        fprintf( stderr, "unsupported sexp type '%s'\n", node->name );
        g_free ( value );
        value = NULL;
        break;
    }
    g_free( data );

    return value;
}
