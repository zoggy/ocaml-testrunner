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

include master.Makefile

# Compilation
#############

all: src

src: dummy
	cd src && $(MAKE) all

re : depend clean all

# Documentation :
#################
doc: dummy
	cd src && $(MAKE) doc

# myself

master.Makefile: master.Makefile.in config.status \
	src/META.in \
	./config.status

config.status: configure
	./config.status --recheck

configure: configure.ac
	autoconf

# backup, clean and depend :
############################

distclean: clean
	$(RM) master.Makefile src/META
	$(RM) -fr config.status autom4te.cache config.log ocaml_config.sh


clean:: dummy
	$(RM) *~ \#*\#
	cd src && $(MAKE) clean

depend: dummy
	cd src && $(MAKE) depend

dummy:

# Web site:
###########
website:
	(cd doc && $(MAKE) DEST_DIR=`pwd`/../../testrunner-pages)

# archive :
###########
archive:
	git archive --prefix=stog-$(VERSION)/ HEAD | gzip > ../testrunner-pages/testrunner-$(VERSION).tar.gz

###########
# Headers
###########
HEADFILES=configure.ac configure \
	master.Makefile.in Makefile src/Makefile web/Makefile \
	checkocaml.ml src/*.ml src/*.mli

headers: dummy
	headache -h header -c .headache_config $(HEADFILES) \
	`ls $(HEADFILES)`

noheaders: dummy
	headache -r -c .headache_config $(HEADFILES) \
	`ls $(HEADFILES)`


############

#webdoc:
#	(cd src && $(MAKE) docstog)
#	cd web && $(MAKE)


#################
# installation
#################

install:
	cd src && $(MAKE) install
install-lib: dummy
	cd src && $(MAKE) install-lib
install-bin: dummy
	cd src && $(MAKE) install-bin

uninstall: dummy
	cd src && $(MAKE) uninstall
uninstall-lib: dummy
	cd src && $(MAKE) uninstall-lib
uninstall-bin: dummy
	cd src && $(MAKE) uninstall-bin

###########################
# additional dependencies
###########################

# DO NOT DELETE
