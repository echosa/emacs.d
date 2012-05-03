# Copyright © 2009,2011 Vivek Dasmohapatra 

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
############################################################################
# make macro functions:
empty       := 
s           := ..
S           := $(empty) $(empty)
INSTALLED = $(if $(shell pkg-config $(subst $s,$S,$(1)) --exists || echo t ),\
                 $(error $(shell echo $(subst $s,$S,$(1))) not installed)  )
############################################################################
# package dependencies:
PACKAGES    := glib-2.0..\>=..2.0.12 libxml-2.0 purple
$(foreach P, $(PACKAGES), $(call INSTALLED, $P))
############################################################################
OLD_GLIB    := $(shell                                                      \
                 if pkg-config glib-2.0 --atleast-version 2.14 2>/dev/null; \
                 then echo '-D'; else echo '-U'; fi;                        )
DEFINES     := -D_GNU_SOURCE                 \
                $(OLD_GLIB)HAS_ADD_SECONDS   \
                $(OLD_GLIB)HAS_QUEUE_INIT    \
                $(OLD_GLIB)HAS_GET_HASH_KEYS
CFLAGS      += -Wall -g -O2 -std=c99 $(DEFINES)
CFLAGS      += $(foreach P, $(PACKAGES), \
                         $(shell pkg-config --cflags $(subst $s,$S,$P))) 
LDFLAGS     += $(foreach P, $(PACKAGES), \
                         $(shell pkg-config --libs   $(subst $s,$S,$P))) 
LDFLAGS     += -lm
TVER        := $(shell etags --version | head -n 1 | grep -i exuberant)
BINARIES    := elim-client
CH_FILES    := $(wildcard *.c         ) \
               $(wildcard prpl/*.c    ) \
               $(wildcard xnode/*.c   ) \
               $(wildcard sexp/*.c    ) \
               $(wildcard ui_ops/*.c  ) \
               $(wildcard handlers/*.c) \
               $(wildcard signals/*.c )
HANDLER_SRC := $(wildcard handlers/*.c)
HANDLER_OBJ := $(patsubst %.c, %.o, $(HANDLER_SRC) )
SIGNAL_SRC  := $(wildcard signals/*.c)
SIGNAL_OBJ  := $(patsubst %.c, %.o, $(SIGNAL_SRC) )
OBJ_FILES   := $(patsubst %.c, %.o, $(CH_FILES) )
UTIL_OBJ    := sexp/sexp-xml.o xnode/xnode.o
CLIENT_OBJ  := $(patsubst %.c, %.o, $(wildcard handlers/*.c))
TAR_FLAGS   := --exclude .git -czvf

############################################################################
.PHONY: clean diag distclean check-libdeps signed-tar tar

all: $(BINARIES) TAGS

############################################################################
# test scripts/utils etc, such as there are:
#test/sexp-test: test/sexp-test.o test/sexp-example.h $(UTIL_OBJ)
#test/sexp-test.o: test/sexp-test.c test/sexp-example.h

############################################################################
# object files and dependencies thereof:
elim-client: $(OBJ_FILES) elim-func-handlers.o

elim-client.o: handler-list.h elim-client-queue.h elim-func-handlers.h

$(OBJ_FILES): %.o: %.c %.h

$(HANDLER_OBJ): ui_ops/ops.h prpl/util.h elim-rpc.h

$(SIGNAL_OBJ): prpl/util.h elim-rpc.h

handlers/init.o: signals/sigs.h

handlers/set_prefs.o: elim-glibcompat.h

############################################################################
# generated source files:
elim-func-handlers.c: make/elim-func-handlers-c.sh $(HANDLER_SRC) 
	$< $(filter-out %.sh, $^) > $@;

ui_ops/ops.h: make/elim-ops-h.sh $(patsubst %.c, %.h, $(wildcard ui_ops/*.c))
	$< $(filter-out %.sh, $^) > $@;

signals/sigs.h: make/sigsigs-h.sh $(patsubst %.c, %.h, $(wildcard signals/*.c))
	$< $(filter-out %.sh, $^) > $@;

handler-list.h: make/handler-list-h.sh $(wildcard handlers/*.h)
	$< $(filter-out %.sh, $^) > $@;

############################################################################
diag:
	@echo "packages : "$(subst $s,$S,$(PACKAGES))
	@echo "CFLAGS   : "$(CFLAGS)
	@echo "LDFLAGS  : "$(LDFLAGS)
	@echo "CH_FILES : "$(CH_FILES)
	@echo "OBJ_FILES: "$(OBJ_FILES)

check-libdeps:
	@echo -n
	$(foreach P, $(PACKAGES),\
	    $(if $(shell pkg-config $(subst $s,$S,$P) --exists && echo t ),\
	         $(info  $(shell echo $(subst $s,$S,$P)) is installed OK)))

clean:
	@( rm -fv $(BINARIES) $(OBJ_FILES) \
	          handler-list.h           \
	          ui_ops/ops.h             \
	          signals/sigs.h 	   \
	          elim-func-handlers.c     );

TAGS: $(CH_FILES)
	@if [ x"$(TVER)" != x ]; then etags --recurse; fi

distclean: clean
	@find . -type f -name *~ -exec rm {} \;
	@rm -fv TAGS


############################################################################

%.tar.gz: distclean
	@tar -C $(abspath $(@D)) $(TAR_FLAGS) $@ $(notdir $(CURDIR))

%.tar.gz.sig: %.tar.gz
	@gpg -b $<

signed-tar: ../$(shell echo elim.$$(date +%Y%m%d-%H%M%S).tar.gz.sig)

tar       : ../$(shell echo elim.$$(date +%Y%m%d-%H%M%S).tar.gz)

############################################################################
dummy:
	@echo -n
