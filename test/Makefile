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

EXE=../src/testrunner
COMPFLAGS=-I ../src -rectypes

PLUGINS=square.cmxs

$(PLUGINS): ../src/testrunner.cmxa

all: $(PLUGINS)
	$(EXE) `for i in $(PLUGINS); do echo -n "-l $$i"; done` test.json \
	--xml test.xml --count -

clean:
	rm -f *.cm* *.o *.annot test.xml
