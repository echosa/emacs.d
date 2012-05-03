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
#include "sexp-xml.h"
#include "string.h"
#include <stdio.h>
#include <stdlib.h>

#define STATE                                                   \
    switch(sexp->state)                                         \
    {                                                           \
      case SEXP_TOPLEVEL    : l = "TOPLEVEL"    ; break;        \
      case SEXP_TAG_NAME    : l = "TAG_NAME"    ; break;        \
      case SEXP_TAG_NAMED   : l = "TAG_NAMED"   ; break;        \
      case SEXP_ATTR        : l = "ATTR"        ; break;        \
      case SEXP_ATTR_NAME   : l = "ATTR_NAME"   ; break;        \
      case SEXP_ATTR_NAMED  : l = "ATTR_NAMED"  ; break;        \
      case SEXP_ATTR_VAL    : l = "ATTR_VAL"    ; break;        \
      case SEXP_ATTR_VALUED : l = "ATTR_VALUED" ; break;        \
      case SEXP_NESTED      : l = "NESTED"      ; break;        \
      case SEXP_DATA        : l = "DATA"        ; break;        \
      case SEXP_PARSED      : l = "PARSED"      ; break;        \
      default               : l = "???" ; break;                \
    }

#define DEBUGP_on(x)                                                     \
    { char *l; STATE; fprintf(stderr,"%c%c: %s\n", x, (isprint(c)?c:'.'), l); }
#define DEBUGP_off(x) 

#define DEBUGP     DEBUGP_off('+')
#define DEBUGI     DEBUGP_off('-')

static guint nulls_seen = 0;

void sexp_init( SEXP *sexp )
{   
    sexp->state  = SEXP_TOPLEVEL;
    sexp->start  = FALSE;
    sexp->escape = FALSE;
    sexp->buffer = g_string_sized_new( 4096 );
    sexp->tname  = g_string_sized_new( 32   );
    sexp->aname  = g_string_sized_new( 32   );
    sexp->aval   = g_string_sized_new( 64   );
    sexp->data   = g_string_sized_new( 8192 );
}

void sexp_rset( SEXP *sexp )
{
    sexp->state  = SEXP_TOPLEVEL;
    sexp->start  = FALSE;
    sexp->escape = FALSE;
    sexp->node   = NULL;
    sexp->root   = NULL;
}

void sexp_exit( SEXP *sexp )
{
    g_string_free( sexp->buffer, TRUE );
    g_string_free( sexp->tname , TRUE );
    g_string_free( sexp->aname , TRUE );
    g_string_free( sexp->aval  , TRUE );
    g_string_free( sexp->data  , TRUE );
}

xmlnode * xnode_from_sexp ( const char *sexp )
{
    const char      *s;
    SEXP         s_exp;
    const char    *end  = sexp + strlen( sexp );

    sexp_init( &s_exp );

    for( s = sexp; s < end; s++ )
    {
        xnode_from_sexp_char( *s, &s_exp );
    }
    
    return s_exp.root;
}

int xnode_from_sexp_char ( const char c, SEXP *sexp )
{
    if (!c)
    {
        if (nulls_seen++ >= 50)
        {
            fprintf( stderr, "%u NULs in input, abort.\n", nulls_seen );
            exit( 1 );
        }
        return 0;
    }

    switch( sexp->state )
    {
      case SEXP_TOPLEVEL:
        if     ( c == '(' ) sexp->state = SEXP_TAG_NAME;
        else if( c == ')' ) sexp->state = SEXP_PARSED  ;
        DEBUGP;
        break;
      case SEXP_TAG_NAME:
        if( !sexp->start && !isspace(c) ) { sexp->start = 1; } // depth++
        if(  sexp->start )
        {
            if( sexp->escape ) 
            { 
                sexp->escape = 0; 
                g_string_append_c( sexp->tname, c );
            }
            else if( c == '\\'   ) { sexp->escape = 1;                    }
            else if( !isspace(c) ) { g_string_append_c( sexp->tname, c ); }
            else
            {
                sexp->start = FALSE;
                sexp->state = SEXP_TAG_NAMED;

                if( !sexp->root ) 
                    sexp->node = sexp->root = xnode_new( sexp->tname->str );
                else              
                    sexp->node = xnode_new_child(sexp->node,sexp->tname->str);

                g_string_truncate( sexp->tname, 0 );
            }
        }
        DEBUGP;
        break;
      case SEXP_TAG_NAMED:
        switch( c )
        {
          case 'n':
            sexp->state = SEXP_NESTED;
            break;
          case '(':
            sexp->state = SEXP_ATTR;
            break;
        }
        DEBUGP;
        break;
      case SEXP_ATTR:
        if     ( c == '(' ) sexp->state = SEXP_ATTR_NAME;
        else if( c == ')' ) sexp->state = SEXP_NESTED;
        DEBUGP;
        break;
      case SEXP_ATTR_NAME:
        if( !sexp->start && !isspace(c) ) { sexp->start = 1; }
        if(  sexp->start )
        {
            if( !isspace(c) ) { g_string_append_c( sexp->aname, c ); }
            else
            {
                sexp->start = FALSE;
                sexp->state = SEXP_ATTR_NAMED;
            }
        }
        DEBUGP;
        break;
      case SEXP_ATTR_NAMED:
        if( c == '.' ) sexp->state = SEXP_ATTR_VAL;
        DEBUGP;
        break;
      case SEXP_ATTR_VAL:
        if     ( !sexp->start && c == '"' ) { sexp->start = 1; }
        else if(  sexp->start )
        {
            if( sexp->escape ) 
            { 
                sexp->escape = 0; 
                g_string_append_c( sexp->aval, c ); 
            }
            else
            {
                if     ( c == '\\' ) { sexp->escape = 1;                   }
                else if( c != '"'  ) { g_string_append_c( sexp->aval, c ); }
                else
                {
                    sexp->start = 0;
                    sexp->state = SEXP_ATTR_VALUED;
                    xnode_set_attrib ( sexp->node       , 
                                       sexp->aname->str , 
                                       sexp->aval->str  );
                    g_string_truncate( sexp->aname, 0 );
                    g_string_truncate( sexp->aval , 0 );
                }
            }
        }
        DEBUGP;
        break;
      case SEXP_ATTR_VALUED:
        if( c == ')' ) sexp->state = SEXP_ATTR;
        DEBUGP;
        break;
      case SEXP_NESTED:
        switch( c )
        {
          case '(': sexp->state = SEXP_TAG_NAME; break; // depth++
          case '"': sexp->state = SEXP_DATA    ; break;
          case ')': // depth--
            sexp->state = 
              ( sexp->node = sexp->node->parent) ? SEXP_NESTED : SEXP_PARSED;
          break;
        }
        DEBUGP;
        break;
      case SEXP_DATA:
        if( sexp->escape ) 
        {
            g_string_append_c( sexp->data, (c == 'n') ? '\n' : c ); 
            sexp->escape = 0; 
        }
        else
        {
            if     ( c == '\\' ) { sexp->escape = 1;                   } 
            else if( c != '"'  ) { g_string_append_c( sexp->data, c ); }
            else
            {
                if( sexp->data->len > 0 )
                    xnode_insert_data( sexp->node      , 
                                       sexp->data->str , 
                                       sexp->data->len );
                g_string_truncate( sexp->data, 0 );
                sexp->state = SEXP_NESTED;
            }
        }
        DEBUGP;
        break;
      case SEXP_PARSED:
        fprintf(stderr, "'impossible' SEXP_PARSED state in internal scanner\n");
        break;
    }
    return 1;
}

int xnode_from_sexp_chunk ( const char *src, SEXP *sexp, size_t len )
{
    const char     *s;
    const char   *end;
    const char *start;
    gboolean       go;

    // ***********************************************************************
    // handle any leftover data from the last chunk in the buffer:
    start = sexp->buffer->str; 
    end   = start + sexp->buffer->len;
    for( go = TRUE, s = start; go && (s < end); s++ )
    {
        xnode_from_sexp_char( *s, sexp );
        go = sexp->state != SEXP_PARSED;
    }

    // erase as much of the buffer as we used up:
    g_string_erase( sexp->buffer, 0, s - start );

    if( sexp->state == SEXP_PARSED ) { return 0; }
    // ***********************************************************************

    end = (len >= 0) ? src + len : src + strlen( src );
    for( go = TRUE, s = src; go && (s < end); s++ )
    {
        xnode_from_sexp_char( *s, sexp );
        go = sexp->state != SEXP_PARSED;
    }
    
    // any leftover data goes into the buffer
    g_string_append_len( sexp->buffer, s, end - s );

    return s - src;
}

char * sexp_escape_symbol( const char *symbol, int len );
char * sexp_escape_symbol( const char *symbol, int len )
{
    int            l = ( len >= 0 ) ? len : strlen( symbol );
    const char    *c = NULL;
    const char  *end = NULL;
    GString *escaped = g_string_sized_new( l );
    char       *rval = NULL;

    for( c = symbol, end = symbol + l; c < end; c++ )
    {
        switch( *c )
        {
          case '"'  :
          case '('  :
          case ')'  :
          case '.'  :
          case '\'' :
          case '\\' :
            g_string_append_c( escaped, '\\' );
          default:
            if( isspace(*c) ) 
                g_string_append_c( escaped, '\\' );
            g_string_append_c( escaped, *c   );
            break;
        }
    }

    rval = escaped->str;
    g_string_free( escaped, FALSE );
    return rval;
}

char * sexp_escape_string( const char *symbol, int len, gboolean significant )
{
    int l            = ( len >= 0 ) ? len : strlen( symbol );
    const char    *c = NULL;
    const char  *end = NULL;
    GString *escaped = g_string_sized_new( l );
    char       *rval = NULL;
    gboolean  not_ws = significant ? FALSE : TRUE;

    for( c = symbol, end = symbol + l; c < end; c++ )
    {
        switch( *c )
        {
          case '\\':
          case '"' :
          case '\n':
            g_string_append_c( escaped, '\\' );
          default:
            if( significant && !not_ws ) not_ws = !isspace(*c);
            g_string_append_c( escaped, (*c == '\n') ? 'n' : *c );
            break;
        }
    }

    if( not_ws )
    {
        rval = escaped->str;
        g_string_free( escaped, FALSE );
        return rval;
    }
    
    g_string_free( escaped, TRUE );
    return NULL;
}

static GString *
_xnode_to_sexp( xmlnode *node, int *len, int depth )
{
    GString    *text = g_string_new( "" );
    xmlnode    *c;
    char       *node_name, *esc, *esc2, *tab = NULL;
    gboolean    need_end = FALSE;
    gboolean    has_attr = FALSE;

    g_return_val_if_fail( node != NULL, NULL );

    if( depth )
    {
        tab  = g_strnfill( depth * 2, ' ' );
        text = g_string_append( text, tab );
    }

    node_name = sexp_escape_symbol( node->name, -1 );

    g_string_append_printf( text, "(%-6s ", node_name );

    for( c = node->child; c; c = c->next )
    {
        if( c->type == XMLNODE_TYPE_ATTRIB ) 
        {
            esc  = sexp_escape_symbol( c->name, -1  );
            esc2 = sexp_escape_string( c->data, -1, FALSE );

            // first attributes, open attr alist:
            // otherwise a space for readability
            if( !has_attr ) g_string_append( text, "(" );
            else            g_string_append( text, " " );

            g_string_append_printf(text, "(%s . \"%s\")", esc, esc2 );

            has_attr = TRUE;
            g_free( esc  );
            g_free( esc2 );
        } 
        else if( c->type == XMLNODE_TYPE_TAG || c->type == XMLNODE_TYPE_DATA ) 
        {
            need_end = TRUE;
        }
    }

    if( has_attr ) g_string_append( text, ")"   );
    else           g_string_append( text, "nil" );

    if( need_end ) 
    {
        for( c = node->child; c; c = c->next )
        {
            if( c->type == XMLNODE_TYPE_TAG ) 
            {
                int esc_len;
                GString  *e;
                if( (e = _xnode_to_sexp( c, &esc_len, depth+1 )) )
                {
                    esc  = e->str;
                    text = g_string_append    ( text, "\n"         );
                    text = g_string_append_len( text, esc, esc_len );
                    g_string_free( e, TRUE );
                }
            }
            else if( c->type == XMLNODE_TYPE_DATA && c->data_sz > 0 ) 
            {
                char *data  = g_new0( char, c->data_sz + 1 );
                memcpy( data, c->data, c->data_sz );
                char *unesc = xnode_unescape_html( data );
                if( (esc = sexp_escape_string( unesc, -1, TRUE )) )
                {
                    g_string_append_printf( text, " \"%s\"", esc );
                    g_free( esc );
                }
                g_free( unesc );
                g_free( data  );
            }
        }

    } 

    g_string_append_c( text, ')' );
    g_free           ( node_name );
    g_free           ( tab       );

    if( len ) *len = text->len;

    return text;
}

char * xnode_to_sexp( xmlnode *node, int *len )
{
    GString *sexp = _xnode_to_sexp( node, len, 0 );
    return sexp ? g_string_free( sexp, FALSE ) : NULL;
}


GString * xnode_to_sexp_gstring( xmlnode *node )
{
    int len;
    return _xnode_to_sexp( node, &len, 0 );
}
