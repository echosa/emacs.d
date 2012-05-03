/*
Copyright © 2009, 2011 Vivek Dasmohapatra

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
#include "xnode.h"
#include <libxml/parser.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>

#define BASE64_ENCODE(x,y) (char *)g_base64_encode(((guchar *)x),(y))

#define XML_DECL "<?xml version='1.0' encoding='UTF-8'?>\n"

xmlnode *
xnode_first_child_tag ( xmlnode *node )
{
    if( !node )
        return NULL;

    for( node = node->child; node; node = node->next )
        if( node->type == XMLNODE_TYPE_TAG ) return node;

    return NULL;
}

xmlnode *
xnode_get_next_sibling ( xmlnode *node )
{
    for( node = node->next; node; node = node->next )
        if( node->type == XMLNODE_TYPE_TAG ) return node;
    return NULL;
}

static const char *
xnode_markup_unescape_entity( const char *text, int *length )
{
    const char *pln;
    int len, hash;
    
    if( !text || *text != '&' ) return NULL;

#define ENTITY(s)  (!g_ascii_strncasecmp(text, s, (len = sizeof(s) - 1)))

    if     ( ENTITY("&amp;" ) ) pln = "&";
    else if( ENTITY("&lt;"  ) ) pln = "<";
    else if( ENTITY("&gt;"  ) ) pln = ">";
    else if( ENTITY("&nbsp;") ) pln = " ";
    else if( ENTITY("&copy;") ) pln = "\302\251";//or g_unichar_to_utf8(0xa9)
    else if( ENTITY("&quot;") ) pln = "\"";
    else if( ENTITY("&reg;" ) ) pln = "\302\256";//or g_unichar_to_utf8(0xae)
    else if( ENTITY("&apos;") ) pln = "\'";
    else if( *( text + 1 ) == '#'                    && 
             ( sscanf(text, "&#%u;", &hash )  == 1 ) &&
             hash != 0                               && 
             *( text + 3 + (gint)log10(hash) ) == ';' ) 
    {
        static char buf[7];
        int buflen    = g_unichar_to_utf8( (gunichar)hash, buf );
        buf[ buflen ] = '\0';
        pln           = buf;
        len           = 2;
        while( isdigit( (gint)text[len] ) ) len++;
        if( text[ len ] == ';' ) len++;
    }
    else
        return NULL;

#undef ENTITY

    if( length ) *length = len;
    return pln;
}

char *
xnode_unescape_html( const char *html ) 
{
    if( html )
    {
        const char *c = html;
        GString  *ret = g_string_new( "" );

        while( *c ) 
        {
            int len;
            const char *ent;
            
            if(  (ent = xnode_markup_unescape_entity(c, &len)) != NULL )
            {
                ret = g_string_append( ret, ent );
                c += len;
            } 
            else if ( !strncmp(c, "<br>", 4) ) 
            {
                ret = g_string_append_c(ret, '\n');
                c += 4;
            } 
            else 
            {
                ret = g_string_append_c(ret, *c);
                c++;
            }
        }
        return g_string_free( ret, FALSE );
    }

    return NULL;
}


static xmlnode*
new_node( const char *name, XMLNodeType type )
{
    xmlnode *node = g_new0( xmlnode, 1 );
    
    node->name = g_strdup( name );
    node->type = type;
    
    return node;
}

xmlnode*
xnode_new( const char *name )
{
    g_return_val_if_fail( name != NULL, NULL );

    return new_node( name, XMLNODE_TYPE_TAG );
}

xmlnode *
xnode_new_child( xmlnode *parent, const char *name )
{
    xmlnode *node;

    g_return_val_if_fail( parent != NULL, NULL );
    g_return_val_if_fail( name != NULL, NULL   );

    node = new_node( name, XMLNODE_TYPE_TAG );

    xnode_insert_child( parent, node );

    return node;
}

void
xnode_insert_child(xmlnode *parent, xmlnode *child)
{
    g_return_if_fail( parent != NULL );
    g_return_if_fail( child  != NULL );

    child->parent = parent;

    if( parent->lastchild ) { parent->lastchild->next = child; } 
    else                    { parent->child = child;           }

    parent->lastchild = child;
}

void
xnode_insert_data( xmlnode *node, const char *data, gssize size )
{
    xmlnode *child;
    gsize real_size;

    g_return_if_fail( node != NULL );
    g_return_if_fail( data != NULL );
    g_return_if_fail( size != 0    );

    real_size = size == -1 ? strlen( data ) : size;

    child = new_node( NULL, XMLNODE_TYPE_DATA );

    child->data = g_memdup( data, real_size );
    child->data_sz = real_size;

    xnode_insert_child( node, child );
}

void
xnode_remove_attrib( xmlnode *node, const char *attr )
{
    xmlnode *attr_node, *sibling = NULL;

    g_return_if_fail( node != NULL );
    g_return_if_fail( attr != NULL );

    for( attr_node = node->child; attr_node; attr_node = attr_node->next )
    {
        if( attr_node->type == XMLNODE_TYPE_ATTRIB &&
            !strcmp( attr_node->name, attr ) )
        {
            if( sibling == NULL ) { node->child   = attr_node->next; } 
            else                  { sibling->next = attr_node->next; }

            if( node->lastchild == attr_node ) { node->lastchild = sibling; }
            xnode_free( attr_node );
            return;
        }
        sibling = attr_node;
    }
}

/* Compare two nullable xmlns strings.
 * They are considered equal if they're both NULL or the strings are equal
 */
static gboolean _xnode_compare_xmlns( const char *xmlns1, const char *xmlns2 )
{
    if( xmlns1 == NULL && xmlns2 == NULL  ) return TRUE;
    if( xmlns1 != NULL && xmlns2 != NULL && 
        !strcmp( xmlns1, xmlns2 )         ) return TRUE;

    return FALSE;
}

void
xnode_remove_attrib_with_namespace( xmlnode    *node  , 
                                    const char *attr  , 
                                    const char *xmlns )
{
    xmlnode *attr_node, *sibling = NULL;

    g_return_if_fail( node != NULL );
    g_return_if_fail( attr != NULL );

    for( attr_node = node->child; attr_node; attr_node = attr_node->next )
    {
        if( attr_node->type == XMLNODE_TYPE_ATTRIB         &&
           !strcmp( attr_node->name, attr )                &&
            _xnode_compare_xmlns( xmlns, attr_node->xmlns ) )
        {
            if( sibling == NULL ) { node->child   = attr_node->next; } 
            else                  { sibling->next = attr_node->next; }

            if( node->lastchild == attr_node ) { node->lastchild = sibling; }

            xnode_free( attr_node );
            return;
        }
        sibling = attr_node;
    }
}


void
xnode_set_attrib( xmlnode *node, const char *attr, const char *value )
{
    xmlnode *attrib_node;

    g_return_if_fail( node  != NULL );
    g_return_if_fail( attr  != NULL );
    g_return_if_fail( value != NULL );

    xnode_remove_attrib( node, attr );

    attrib_node       = new_node( attr, XMLNODE_TYPE_ATTRIB );
    attrib_node->data = g_strdup(value);
    xnode_insert_child( node, attrib_node );
}

void
xnode_set_attrib_with_namespace( xmlnode    *node  , 
                                 const char *attr  , 
                                 const char *xmlns , 
                                 const char *value )
{
    xmlnode *attrib_node;

    g_return_if_fail( node  != NULL );
    g_return_if_fail( attr  != NULL );
    g_return_if_fail( value != NULL );

    xnode_remove_attrib_with_namespace( node, attr, xmlns );
    attrib_node = new_node( attr, XMLNODE_TYPE_ATTRIB );

    attrib_node->data  = g_strdup( value );
    attrib_node->xmlns = g_strdup( xmlns );

    xnode_insert_child( node, attrib_node );
}

void
xnode_set_attrib_with_prefix( xmlnode    *node   , 
                              const char *attr   , 
                              const char *prefix , 
                              const char *value  )
{
    xmlnode *attrib_node;

    g_return_if_fail( node  != NULL );
    g_return_if_fail( attr  != NULL );
    g_return_if_fail( value != NULL );

    attrib_node = new_node( attr, XMLNODE_TYPE_ATTRIB );

    attrib_node->data   = g_strdup( value  );
    attrib_node->prefix = g_strdup( prefix );

    xnode_insert_child( node, attrib_node );
}


const char *
xnode_get_attrib( xmlnode *node, const char *attr )
{
    xmlnode *x;
    
    g_return_val_if_fail( node != NULL, NULL );

    for( x = node->child; x; x = x->next ) 
    {
        if( x->type == XMLNODE_TYPE_ATTRIB && !strcmp(attr, x->name) )
            return x->data;
    }

    return NULL;
}

const char *
xnode_get_attrib_with_namespace( xmlnode    *node  , 
                                 const char *attr  , 
                                 const char *xmlns )
{
    xmlnode *x;

    g_return_val_if_fail( node != NULL, NULL );

    for( x = node->child; x; x = x->next ) 
    {
        if( x->type == XMLNODE_TYPE_ATTRIB           &&
            !strcmp(attr, x->name)                   &&
            _xnode_compare_xmlns( xmlns, x->xmlns ) ) { return x->data; }
    }

    return NULL;
}


void xnode_set_namespace( xmlnode *node, const char *xmlns )
{
    g_return_if_fail( node != NULL );

    g_free( node->xmlns );
    node->xmlns = g_strdup( xmlns );
}

const char *xnode_get_namespace( xmlnode *node )
{
    g_return_val_if_fail( node != NULL, NULL );

    return node->xmlns;
}

void xnode_set_prefix( xmlnode *node, const char *prefix )
{
    g_return_if_fail( node != NULL );

    g_free( node->prefix );
    node->prefix = g_strdup( prefix );
}

const char *xnode_get_prefix( xmlnode *node )
{
    g_return_val_if_fail( node != NULL, NULL );
    return node->prefix;
}

void
xnode_free( xmlnode *node )
{
    xmlnode *x, *y;

    if( !node )
        return;

    /* if we're part of a tree, remove ourselves from the tree first */
    if( NULL != node->parent ) 
    {
        if( node->parent->child == node ) 
        {
            node->parent->child = node->next;
            if ( node->parent->lastchild == node )
                node->parent->lastchild = node->next;
        } 
        else 
        {
            xmlnode *prev = node->parent->child;
            while( prev && prev->next != node ) { prev = prev->next; }
            if( prev ) 
            {
                prev->next = node->next;
                if ( node->parent->lastchild == node )
                    node->parent->lastchild = prev;
            }
        }
    }

    /* now free our children */
    x = node->child;
    while( x ) { y = x->next; xnode_free(x); x = y; }

    /* now dispose of ourselves */
    g_free( node->name  );
    g_free( node->data  );
    g_free( node->xmlns );

    if( node->namespace_map )
        g_hash_table_destroy( node->namespace_map );

    g_free( node );
}

xmlnode*
xnode_get_child( const xmlnode *parent, const char *name )
{
    return xnode_get_child_with_namespace( parent, name, NULL );
}

xmlnode *
xnode_get_child_with_namespace( const xmlnode *parent , 
                                const char    *name   , 
                                const char    *ns     )
{
    xmlnode *x, *ret = NULL;
    char **names;
    char *parent_name, *child_name;

    g_return_val_if_fail( parent != NULL, NULL );
    g_return_val_if_fail( name   != NULL, NULL );

    names       = g_strsplit( name, "/", 2 );
    parent_name = names[ 0 ];
    child_name  = names[ 1 ];

    for( x = parent->child; x; x = x->next ) 
    {
        /* XXX: Is it correct to ignore the namespace for the match if
         * none was specified? */
        const char *xmlns = NULL;
        if( ns ) xmlns = xnode_get_namespace( x );

        if( x->type == XMLNODE_TYPE_TAG             && 
            name                                    && 
            !strcmp(parent_name, x->name)           && 
            ( !ns || (xmlns && !strcmp(ns, xmlns)) ) ) 
        {
            ret = x;
            break;
        }
    }

    if( child_name && ret )
        ret = xnode_get_child( ret, child_name );

    g_strfreev( names );
    return ret;
}

char *
xnode_get_data( xmlnode *node )
{
    GString *str = NULL;
    xmlnode *c;

    g_return_val_if_fail( node != NULL, NULL );

    for( c = node->child; c; c = c->next ) 
    {
        if( c->type == XMLNODE_TYPE_DATA ) 
        {
            if( !str ) str = g_string_new_len   ( c->data, c->data_sz );
            else       str = g_string_append_len( str, c->data, c->data_sz );
        }
    }

    if( str == NULL ) return NULL;

    return g_string_free( str, FALSE );
}

char *
xnode_get_data_unescaped( xmlnode *node )
{
    char *escaped   = xnode_get_data( node );
    char *unescaped = escaped ? xnode_unescape_html( escaped ) : NULL;

    g_free( escaped );

    return unescaped;
}

static void
xnode_to_str_foreach_append_ns( const char *key   , 
                                const char *value ,
                                GString    *buf   )
{
    if( *key ) g_string_append_printf( buf, " xmlns:%s='%s'", key   , value );
    else       g_string_append_printf( buf, " xmlns='%s'"   , value );
}

static char *
xnode_to_str_helper( xmlnode *node, int *len, gboolean formatting, int depth )
{
    GString    *text = g_string_new( "" );
    const char *prefix;
    xmlnode    *c;
    char       *node_name, *esc, *esc2, *tab = NULL;
    gboolean    need_end = FALSE, pretty = formatting;

    g_return_val_if_fail( node != NULL, NULL );

    if( pretty && depth ) 
    {
        tab  = g_strnfill(depth * 2, ' ');
        text = g_string_append( text, tab );
    }

    node_name = g_markup_escape_text( node->name, -1 );
    prefix    = xnode_get_prefix( node );

    if( prefix ) g_string_append_printf( text, "<%s:%s", prefix, node_name );
    else         g_string_append_printf( text, "<%s"   , node_name );

    if( node->namespace_map ) 
    {
        g_hash_table_foreach( node->namespace_map                    ,
                              (GHFunc)xnode_to_str_foreach_append_ns , 
                              text                                   );
    } 
    else if( node->xmlns ) 
    {
        if( !node->parent                             || 
            !node->parent->xmlns                      || 
            strcmp( node->xmlns, node->parent->xmlns ) )
        {
            char *xmlns = g_markup_escape_text( node->xmlns, -1 );
            g_string_append_printf( text, " xmlns='%s'", xmlns );
            g_free( xmlns );
        }
    }

    for( c = node->child; c; c = c->next )
    {
        if( c->type == XMLNODE_TYPE_ATTRIB ) 
        {
            const char *aprefix = xnode_get_prefix( c );
            esc  = g_markup_escape_text( c->name, -1 );
            esc2 = g_markup_escape_text( c->data, -1 );

            if( aprefix ) 
                g_string_append_printf(text, " %s:%s='%s'", aprefix, esc, esc2);
            else 
                g_string_append_printf(text, " %s='%s'", esc, esc2);

            g_free( esc  );
            g_free( esc2 );
        } 
        else if( c->type == XMLNODE_TYPE_TAG || c->type == XMLNODE_TYPE_DATA ) 
        {
            if( c->type == XMLNODE_TYPE_DATA ) pretty = FALSE;
            need_end = TRUE;
        }
    }

    if( need_end ) 
    {
        g_string_append_printf( text, ">%s", pretty ? "\n" : "" );

        for( c = node->child; c; c = c->next )
        {
            if( c->type == XMLNODE_TYPE_TAG ) 
            {
                int esc_len;
                esc  = xnode_to_str_helper( c, &esc_len, pretty, depth+1 );
                text = g_string_append_len  ( text, esc, esc_len );
                g_free( esc );
            } 
            else if( c->type == XMLNODE_TYPE_DATA && c->data_sz > 0 ) 
            {
                esc  = g_markup_escape_text( c->data, c->data_sz );
                text = g_string_append     ( text   , esc );
                g_free( esc );
            }
        }

        if( tab && pretty )
            text = g_string_append( text, tab );
        if( prefix ) 
        {
            g_string_append_printf( text                   , 
                                    "</%s:%s>%s"           , 
                                    prefix                 , 
                                    node_name              , 
                                    formatting ? "\n" : "" );
        } 
        else 
        {
            g_string_append_printf( text                   , 
                                    "</%s>%s"              , 
                                    node_name              , 
                                    formatting ? "\n" : "" );
            if( 0 && pretty && !depth )
            {
                fprintf( stderr, "XX> closing %s [%d]\n", node_name, depth );
                fprintf( stderr, "XX> length: %ld; actual: %ld\n", 
                         (long)text->len, (long)strlen( text->str ) );
            }
        }
    } 
    else 
    {
        g_string_append_printf( text, "/>%s", formatting ? "\n" : "" );
    }

    g_free( node_name );

    g_free( tab );

    if( len ) *len = text->len;

    return g_string_free( text, FALSE );
}

char *
xnode_to_str( xmlnode *node, int *len )
{
    return xnode_to_str_helper( node, len, FALSE, 0 );
}

char *
xnode_to_formatted_str( xmlnode *node, int *len )
{
    int   dlen;
    char *xml, *xml_decl;

    g_return_val_if_fail( node != NULL, NULL );

    dlen     = strlen( XML_DECL );
    xml      = xnode_to_str_helper( node, len, TRUE, 0 );
    xml_decl = g_new0( char, dlen + *len + 1 );
    memcpy( xml_decl             , XML_DECL, dlen );
    memcpy( xml_decl + dlen, xml , *len           );
    g_free( xml );

    *len += dlen;
    return xml_decl;
}

struct _xnode_parser_data 
{
    xmlnode *current;
    gboolean error;
};

static void
xnode_parser_element_start_libxml( void           *user_data     ,
                                   const xmlChar  *element_name  ,
                                   const xmlChar  *prefix        ,
                                   const xmlChar  *xmlns         ,
                                   int             nb_namespaces ,
                                   const xmlChar **namespaces    ,
                                   int             nb_attributes ,
                                   int             nb_defaulted  ,
                                   const xmlChar **attributes    )
{
    struct _xnode_parser_data *xpd = user_data;
    xmlnode *node;
    int i, j;

    if( !element_name || xpd->error ) 
    {
        return;
    } 

    if( xpd->current )
        node = xnode_new_child( xpd->current, (const char*) element_name );
    else
        node = xnode_new( (const char *) element_name );

    xnode_set_namespace( node, (const char *) xmlns  );
    xnode_set_prefix   ( node, (const char *) prefix );

    if( nb_namespaces != 0 ) 
    {
        node->namespace_map = 
          g_hash_table_new_full( g_str_hash, g_str_equal, g_free, g_free );

        for( i = 0, j = 0; i < nb_namespaces; i++, j += 2 ) 
        {
            const char *key = (const char *)namespaces[ j     ];
            const char *val = (const char *)namespaces[ j + 1 ];
            g_hash_table_insert( node->namespace_map      ,
                                 g_strdup(key ? key : "") , 
                                 g_strdup(val ? val : "") );
        }
    }

    for( i=0; i < nb_attributes * 5; i+=5 ) 
    {
        char       *txt;
        const char *prefix     = (const char *)attributes[ i + 1 ];
        int         attrib_len = attributes[ i + 4 ] - attributes[ i + 3 ];
        char       *attrib     = g_malloc( attrib_len + 1 );
        memcpy( attrib, attributes[ i + 3 ], attrib_len );
        attrib[ attrib_len ] = '\0';
        txt                  = attrib;
        attrib               = xnode_unescape_html( txt );
        g_free( txt );
        if( prefix && *prefix ) 
        {
            xnode_set_attrib_with_prefix( node                        , 
                                            (const char*) attributes[i] , 
                                            prefix                      , 
                                            attrib                      );
        } 
        else 
        {
            xnode_set_attrib( node, (const char*) attributes[i], attrib );
        }
        g_free( attrib );
    }

    xpd->current = node;
}

static void
xnode_parser_element_end_libxml( void          *user_data    ,
                                   const xmlChar *element_name ,
                                   const xmlChar *prefix       ,
                                   const xmlChar *xmlns        )
{
    struct _xnode_parser_data *xpd = user_data;

    if( !element_name || !xpd->current || xpd->error )
        return;

    if( xpd->current->parent ) 
    {
        if( !xmlStrcmp((xmlChar*) xpd->current->name, element_name) )
            xpd->current = xpd->current->parent;
    }
}

static void
xnode_parser_element_text_libxml( void          *user_data ,
                                    const xmlChar *text      ,
                                    int            text_len  )
{
    struct _xnode_parser_data *xpd = user_data;

    if( !xpd->current || xpd->error ) return;
    if( !text         || !text_len  ) return;

    xnode_insert_data( xpd->current, (const char*) text, text_len );
}

static void
xnode_parser_error_libxml( void *user_data, const char *msg, ... )
{
    struct _xnode_parser_data *xpd = user_data;
    char    errmsg[ 2048 ];
    va_list args;

    xpd->error = TRUE;

    va_start ( args, msg );
    vsnprintf( errmsg, sizeof( errmsg ), msg, args );
    va_end   ( args );

    fprintf( stderr, "xmlnode: Error parsing xml file: %s\n", errmsg );
    exit( 1 );
}

static xmlSAXHandler xnode_parser_libxml = 
{
    NULL                              , /* internalSubset        */
    NULL                              , /* isStandalone          */
    NULL                              , /* hasInternalSubset     */
    NULL                              , /* hasExternalSubset     */
    NULL                              , /* resolveEntity         */
    NULL                              , /* getEntity             */
    NULL                              , /* entityDecl            */
    NULL                              , /* notationDecl          */
    NULL                              , /* attributeDecl         */
    NULL                              , /* elementDecl           */
    NULL                              , /* unparsedEntityDecl    */
    NULL                              , /* setDocumentLocator    */
    NULL                              , /* startDocument         */
    NULL                              , /* endDocument           */
    NULL                              , /* startElement          */
    NULL                              , /* endElement            */
    NULL                              , /* reference             */
    xnode_parser_element_text_libxml  , /* characters            */
    NULL                              , /* ignorableWhitespace   */
    NULL                              , /* processingInstruction */
    NULL                              , /* comment               */
    NULL                              , /* warning               */
    xnode_parser_error_libxml         , /* error                 */
    NULL                              , /* fatalError            */
    NULL                              , /* getParameterEntity    */
    NULL                              , /* cdataBlock            */
    NULL                              , /* externalSubset        */
    XML_SAX2_MAGIC                    , /* initialized           */
    NULL                              , /* _private              */
    xnode_parser_element_start_libxml , /* startElementNs        */
    xnode_parser_element_end_libxml   , /* endElementNs          */
    NULL                              , /* serror                */
};

xmlnode *
xnode_from_str( const char *str, gssize size )
{
    struct _xnode_parser_data *xpd;
    xmlnode *ret;
    gsize    real_size;

    g_return_val_if_fail( str != NULL, NULL );

    real_size = size < 0 ? strlen( str ) : size;
    xpd = g_new0( struct _xnode_parser_data, 1 );

    if( xmlSAXUserParseMemory(&xnode_parser_libxml, xpd, str, real_size) < 0 )
    {
        while( xpd->current && xpd->current->parent )
            xpd->current = xpd->current->parent;

        if( xpd->current ) xnode_free(xpd->current);
        xpd->current = NULL;
    }
    ret = xpd->current;
    if( xpd->error ) 
    {
        ret = NULL;
        if( xpd->current )
            xnode_free(xpd->current);
    }

    g_free( xpd );
    return ret;
}

xmlnode *
xnode_copy( const xmlnode *src )
{
    xmlnode *ret;
    xmlnode *child;
    xmlnode *sibling = NULL;

    g_return_val_if_fail( src != NULL, NULL );

    ret        = new_node( src->name, src->type );
    ret->xmlns = g_strdup( src->xmlns );

    if( src->data ) 
    {
        if( src->data_sz ) 
        {
            ret->data    = g_memdup( src->data, src->data_sz );
            ret->data_sz = src->data_sz;
        } 
        else 
        {
            ret->data    = g_strdup( src->data );
        }
    }

    for( child = src->child; child; child = child->next ) 
    {
        if( sibling ) 
        {
            sibling->next = xnode_copy( child );
            sibling       = sibling->next;
        } 
        else 
        {
            ret->child = xnode_copy( child );
            sibling    = ret->child;
        }
        sibling->parent = ret;
    }

    ret->lastchild = sibling;

    return ret;
}

xmlnode *
xnode_get_next_twin( xmlnode *node )
{
    xmlnode    *sibling;
    const char *ns = xnode_get_namespace( node );

    g_return_val_if_fail( node != NULL, NULL );
    g_return_val_if_fail( node->type == XMLNODE_TYPE_TAG, NULL );

    for(  sibling = node->next; sibling; sibling = sibling->next ) 
    {
        /* XXX: Is it correct to ignore the namespace for the match if
         * none was specified? */
        const char *xmlns = NULL;

        if( ns ) xmlns = xnode_get_namespace( sibling );

        if(  sibling->type == XMLNODE_TYPE_TAG       && 
            !strcmp(node->name, sibling->name)       &&
             ( !ns || (xmlns && !strcmp(ns, xmlns)) ) )
            return sibling;
    }

    return NULL;
}

// ==========================================================================

#define _LIST_ITEM(thing,type) thing = xnode_new( type );

xmlnode * xnode_list_item_string(const char *value)
{
    xmlnode *item;
    _LIST_ITEM( item, "string" );
    xnode_insert_data( item, value ? value : "", -1 );
    return item;
}

// ==========================================================================

#define _ALIST_ITEM(thing,type,name)       \
    thing = xnode_new( type );             \
    xnode_set_attrib ( thing, "name", name );

#define _ALIST_ENUM(thing,type,name,class)  \
    _ALIST_ITEM( thing, type, name );       \
    xnode_set_attrib ( thing, "type", class );

xmlnode * xnode_alist_item_string( const char *name, const char *value )
{
    xmlnode *item;

    _ALIST_ITEM( item, "string", name );
    xnode_insert_data( item, value ? value : "" , -1 );

    return item;
}

xmlnode * xnode_alist_item_integer( const char *name, long value )
{
    xmlnode *item;
    GString *i = g_string_new( "" );

    _ALIST_ITEM( item, "int", name ); 

    g_string_printf  ( i, "%ld", value );
    xnode_insert_data( item, i->str, i->len );
    g_string_free    ( i, TRUE );

    return item;
}

xmlnode * xnode_alist_item_number( const char *name, double value )
{
    xmlnode *item;
    GString *i = g_string_new( "" );

    _ALIST_ITEM( item, "float", name ); 

    g_string_printf  ( i, "%f", value );
    xnode_insert_data( item, i->str, i->len );
    g_string_free    ( i, TRUE );

    return item;
}


xmlnode * xnode_alist_item_enum( const char *name, int value, const char *type )
{
    xmlnode *item;
    GString *i = g_string_new( "" );

    _ALIST_ENUM( item, "int", name, type ); 

    g_string_printf  ( i, "%d", value );
    xnode_insert_data( item, i->str, i->len );
    g_string_free    ( i, TRUE );

    return item;
}

xmlnode * xnode_alist_item_data( const char *name, const char *v, size_t l )
{
    xmlnode *item;
    char    *data = v ? BASE64_ENCODE( v, l ) : "";

    _ALIST_ITEM( item, "data", name );
    xnode_insert_data( item, data ? data : "" , -1 );
    g_free( data );

    return item;
}

xmlnode * xnode_alist_item_boolean( const char *name, gboolean value )
{
    xmlnode *item;

    _ALIST_ITEM( item, "bool", name );
    xnode_insert_data( item, value ? "1" : "0", 1 );

    return item;
}

xmlnode * xnode_alist_item_xnode( const char *name, xmlnode *value )
{
    xnode_set_attrib( value, "name", name );
    return value;
}
