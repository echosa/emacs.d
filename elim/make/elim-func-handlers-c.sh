#!/bin/sh
# Copyright © 2009 Vivek Dasmohapatra 

# email : vivek@etla.org
# irc   : fledermaus on freenode, oftc
# jabber: fledermaus@jabber.earth.li

# This file is part of elim.

# elim is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# elim is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with elim.  If not, see <http://www.gnu.org/licenses/>.

echo   "#include \"elim-func-handlers.h\"";
echo   "func_handler handlers[] = ";
printf "  {";
FILLER="";
for FILE in "$@";
do
    BN=$(basename $FILE .c);
    FN=$(echo $BN | sed -e 's/_/-/g');
    printf "$FILLER { %-15s, _h_elim_%-11s } ,\n" \"$FN\" $BN;
    FILLER="   ";
done;
printf "$FILLER {  %-14s, %-19s } };\n" NULL NULL;
