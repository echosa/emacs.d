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
#ifndef _EMACSIM_SEXP_XML_H_
#define _EMACSIM_SEXP_XML_H_

#include <ctype.h>
#include "../xnode/xnode.h"

typedef enum _sexp_state 
{
    SEXP_TOPLEVEL    , /* top level of xml   */
    SEXP_TAG_NAME    , /* tag name */
    SEXP_TAG_NAMED   , /* tag name collected */
    SEXP_ATTR        , /* after tag name, looks like an attr alist */
    SEXP_ATTR_NAME   , /* attr alist key */
    SEXP_ATTR_NAMED  , /* obtained an attr alist key */
    SEXP_ATTR_VAL    , /* attr alias value */
    SEXP_ATTR_VALUED , /* obtained an attr alist value */
    SEXP_NESTED      ,
    SEXP_DATA        ,
    SEXP_PARSED
} sexp_state;

typedef struct _SEXP SEXP;
struct _SEXP
{
    xmlnode    *root   ; 
    xmlnode    *node   ;
    sexp_state  state  ;
    gboolean    start  ;
    gboolean    escape ;
    GString    *buffer ;
    GString    *tname  ;
    GString    *aname  ;
    GString    *aval   ;
    GString    *data   ;
};

void      sexp_init             ( SEXP *sexp );
void      sexp_rset             ( SEXP *sexp ); // after a complete sexp
void      sexp_exit             ( SEXP *sexp );
xmlnode * xnode_from_sexp       ( const char *sexp );
int       xnode_from_sexp_char  ( const char c, SEXP *sexp );
int       xnode_from_sexp_chunk ( const char *src, SEXP *sexp, size_t len );
char    * xnode_to_sexp         ( xmlnode *node, int *len );
GString * xnode_to_sexp_gstring ( xmlnode *node );

#endif
