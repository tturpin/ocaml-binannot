#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#                                Camlp4                                 #
#                                                                       #
#   Copyright 2004 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################
#
# Makefile for pa_format
# M.Mauny
#

include ../../config/Makefile.cnf

OCAMLTOP=../../..
CAMLP4=../../camlp4/camlp4$(EXE)

OCAMLC=$(OCAMLTOP)/ocamlc$(EXE)
OCAMLOPT=$(OCAMLTOP)/ocamlopt$(EXE)

P4INCLUDES= -nolib -I ../../lib -I ../../meta -I ../../etc
OCAMLINCLUDES= -nostdlib -I $(OCAMLTOP)/stdlib -I ../../lib -I ../../camlp4
OCAMLCFLAGS=-warn-error A $(OCAMLINCLUDES)

SRC=pa_format.ml
OBJS=$(SRC:.ml=.cmo)
OBJSX=$(SRC:.ml=.cmx)

all: $(OBJS)

opt: $(OBJSX)

depend:
	cp .depend .depend.bak
	> .depend
	for file in $(SRC); do \
	  $(CAMLP4) $(P4INCLUDES) `awk 'NR == 1 { ORS=" "; for (i=2; i < NF; i++) print $$i; } ' $$file` pr_depend.cmo $$i | \
	  sed -e 's| \.\./\.\./\.\.| $$(OCAMLTOP)|g' >> .depend; \
	done

clean:
	rm -f *.cm* *.$(O) *.bak .*.bak


.SUFFIXES: .cmx .cmo .cmi .ml .mli

.mli.cmi:
	$(OCAMLC) $(OCAMLCFLAGS) -c $<


.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) -pp "$(CAMLP4) $(P4INCLUDES) `awk 'NR == 1 { ORS=" "; for (i=2; i < NF; i++) print $$i; } ' $<`" -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLCFLAGS) -pp "$(CAMLP4) $(P4INCLUDES) `awk 'NR == 1 { ORS=" "; for (i=2; i < NF; i++) print $$i; } ' $<`" -c $<

include .depend
