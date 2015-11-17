#################################################################################
#                Testrunner                                                     #
#                                                                               #
#    Copyright (C) 2015 INRIA All rights reserved.                              #
#    Author: Maxence Guesdon, INRIA Saclay                                      #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU Lesser General Public License as             #
#    published by the Free Software Foundation, version 3 of the License.       #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               #
#    GNU Lesser General Public License for more details.                        #
#                                                                               #
#    You should have received a copy of the GNU Lesser General Public           #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#################################################################################

include ../master.Makefile

P=#p -p
PBYTE=#p -p a

OF_FLAGS=-package $(PACKAGES)

COMPFLAGS=-I +ocamldoc -annot -rectypes -g -safe-string #-warn-error A #-dsource #-w +K
OCAMLPP=

LIB_CMXFILES= \
	testrunner.cmx \

LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)

LIB=testrunner.cmxa
LIB_CMXS=$(LIB:.cmxa=.cmxs)
LIB_BYTE=$(LIB:.cmxa=.cma)

MAIN=testrunner
MAIN_BYTE=$(MAIN).byte

MK_TR=mk-$(MAIN)
MK_TR_BYTE=mk-$(MAIN_BYTE)

all: opt byte

opt: $(LIB) $(LIB_CMXS) $(MAIN) $(MK_TR)

byte: $(LIB_BYTE) $(MAIN_BYTE) $(MK_TR_BYTE)

$(MAIN): $(LIB) testrunner_main.cmx
	$(OCAMLFIND) ocamlopt$(P) -package $(PACKAGES) -verbose -linkall -linkpkg -o $@ $(COMPFLAGS) $^

$(MAIN_BYTE): $(LIB_BYTE) testrunner_main.cmo
	$(OCAMLFIND) ocamlc$(PBYTE) -package $(PACKAGES) -linkall -linkpkg -o $@ $(COMPFLAGS) $^

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -a -o $@ $(LIB_CMXFILES)

$(LIB_CMXS): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -shared -o $@ $(LIB_CMXFILES)

$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIB_CMOFILES)

# mk scripts
$(MK_TR): $(LIB)
	@echo -n "Creating $@... "
	@$(RM) $@
	@echo "# Multi-shell script.  Works under Bourne Shell, MPW Shell, zsh." > $@
	@echo "if : == x" >> $@
	@echo "then # Bourne Shell or zsh" >> $@
	@echo "  exec $(OCAMLFIND) ocamlopt -package testrunner -linkpkg -linkall \"\$$@\" testrunner_main.cmx" >> $@
	@echo "else #MPW Shell" >> $@
	@echo "  exec $(OCAMLFIND) ocamlopt -package testrunner -linkpkg -linkall {\"parameters\"} testrunner_main.cmx" >> $@
	@echo "End # uppercase E because \"end\" is a keyword in zsh" >> $@
	@echo "fi" >> $@
	@chmod ugo+rx $@
	@chmod a-w $@
	@echo done

$(MK_TR_BYTE): $(LIB)
	@echo -n "Creating $@... "
	@$(RM) $@
	@echo "# Multi-shell script.  Works under Bourne Shell, MPW Shell, zsh." > $@
	@echo "if : == x" >> $@
	@echo "then # Bourne Shell or zsh" >> $@
	@echo "  exec $(OCAMLFIND) ocamlc -package testrunner -linkpkg -linkall \"\$$@\" testrunner_main.cmo" >> $@
	@echo "else #MPW Shell" >> $@
	@echo "  exec $(OCAMLFIND) ocamlc -package testrunner -linkpkg -linkall {\"parameters\"} testrunner_main.cmo" >> $@
	@echo "End # uppercase E because \"end\" is a keyword in zsh" >> $@
	@echo "fi" >> $@
	@chmod ugo+rx $@
	@chmod a-w $@
	@echo done


##########
.PHONY: doc ocamldoc

SOURCE_FILES=$(shell ls $(LIB_CMXFILES:.cmx=.ml) $(LIB_CMXFILES:.cmx=.mli) 2>/dev/null)

dump.odoc: $(SOURCE_FILES)
	$(OCAMLFIND) ocamldoc -package $(PACKAGES) -rectypes -sort -dump $@ \
	$(SOURCE_FILES)

doc: dump.odoc
	$(MKDIR) ocamldoc
	$(OCAMLFIND) ocamldoc -load dump.odoc -sort -d ocamldoc -html -t "Testrunner"

PKGS := $(shell echo $(PACKAGES) | sed -e "s/,/ /g")
docdep: dump.odoc
	$(MKDIR) ocamldoc
	$(OCAMLDOC) -load $< -d ocamldoc -g odoc_depgraph.cmxs -t "Stog" \
	-width 700 -height 700	\
	-dot-options '-Nfontsize=60. -Granksep=0.1 -Earrowsize=3.0 -Ecolor="#444444" '

docstog: $(ODOC) dump.odoc
	$(MKDIR) ../doc/ref-doc
	rm -fr ../doc/ref-doc/*html
	OCAMLFIND_COMMANDS="ocamldoc=ocamldoc.opt" \
	ocamldoc.opt -load dump.odoc -d ../doc/ref-doc \
	-t "Testrunner library reference documentation" -short-functors \
	-g odoc_depgraph.cmxs -g ./$(ODOC) -width 700 -height 700 -dot-reduce \
	-dot-options '-Nfontsize=40. -Granksep=0.1 -Earrowsize=3.0 -Ecolor="#444444" '


########
install: install-lib install-bin

install-lib:
	@$(OCAMLFIND) install testrunner META \
		$(LIB_CMIFILES) $(LIB_CMXFILES) $(LIB_CMXFILES:.cmx=.o) \
		$(LIB_BYTE) $(LIB) $(LIB:.cmxa=.a) $(LIB_CMXS) \
		testrunner_main.cm* testrunner_main.o

install-share:

install-bin:
	$(CP) $(MAIN) $(MAIN_BYTE) $(MK_TR) $(MK_TR_BYTE) \
		`dirname \`which $(OCAMLC)\``/

uninstall: uninstall-lib uninstall-bin

uninstall-lib:
	@$(OCAMLFIND) remove stog

uninstall-bin:
	for i in $(MAIN) $(MAIN_BYTE)	$(MK_TR) $(MK_TR_BYTE) \
		do $(RM) `dirname \`which $(OCAMLC)\``/$$i; done

#####
clean:
	$(RM) *.cm* *.o *.a *.x *.annot
	$(RM) $(GENERATED_FILES)
	$(RM) $(MAIN) $(MAIN_BYTE)
	$(RM) $(MK_TR) $(ML_TR_BYTE)

#############
GENERATED_FILES=

.PRECIOUS: $(GENERATED_FILES)

.PHONY: clean depend

.depend depend:
	$(OCAMLFIND) ocamldep -package $(PACKAGES) \
	`ls testrunner*.ml testrunner*.mli` > .depend

include .depend

