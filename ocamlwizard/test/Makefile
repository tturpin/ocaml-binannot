LD_PATH=../../otherlibs/str:../../otherlibs/unix

OCAMLWIZARD=ocamlwizard
RES=match_cases_res.ml expansion_res.ml path_res.ml errors_res.ml

all: $(RES)
	for i in $(RES); do \
	  diff $$i expected/$$i; \
	done

#Generic rules and functions
############################

#numbers <file>
numbers = $(shell grep -n \\$$ $(1).ml | cut -d : -f 1)

#split <file> <numbers>
split =	for i in $(2); do \
	  cat prefix.ml>$(1)_$$i.ml ; \
	  head -$$i $(1).ml | tail -1 >>$(1)_$$i.ml; \
	done

%_no_dollar.ml: %.ml
	sed -e 's/\$$//g' $< | sed -e 's/€//g' >$@


# Pattern matching completion
#############################

M_NUMBERS=$(call numbers,match_cases)
M_CASES=$(foreach n, $(M_NUMBERS), match_cases_$(n).ml)
M_TESTS=$(subst .ml,_mres.ml, $(M_CASES))

$(M_CASES): match_cases.ml
	$(call split,match_cases,$(M_NUMBERS))

match_cases_res.ml: $(M_TESTS)
	cat $^ >$@

%_mres.ml: %.ml %_mcomp.ml
	tail -n 1 $< >$@ ; echo "=>" >>$@ ; \
	head -c `grep -o -b \\\\$$ $< | cut -d : -f 1` $< | tail -n 1 >>$@ ; \
	cat $(subst .ml,_mcomp.ml, $<) >>$@ ; echo EOF\\n >>$@

%_mcomp.ml: %.ml %_no_dollar.ml
	$(OCAMLWIZARD) completion $(subst .ml,_no_dollar.ml, $<) \
	-pos `grep -o -b \\\\$$ $< | cut -d : -f 1` -printer ocaml-pp >$@


# Pattern expansion
###################

PE_NUMBERS=$(call numbers,expansion)
PE_CASES=$(foreach n, $(PE_NUMBERS), expansion_$(n).ml)
PE_TESTS=$(subst .ml,_peres.ml, $(PE_CASES))

$(PE_CASES): expansion.ml
	$(call split,expansion,$(PE_NUMBERS))

expansion_res.ml: $(PE_TESTS)
	cat $^ >$@

%_peres.ml: %.ml %_pecomp.ml
	tail -n 1 $< >$@ ; echo "=>" >>$@ ; \
	head -c `grep -o -b \\\\$$ $< | cut -d : -f 1` $< | tail -n 1 >>$@ ; \
	cat $(subst .ml,_pecomp.ml, $<) >>$@ ; echo EOF\\n >>$@

# We use tail to perform +1 and +2 :-(
%_pecomp.ml: %.ml %_no_dollar.ml
	$(OCAMLWIZARD) completion $(subst .ml,_no_dollar.ml, $<) \
	-pos `tail -c +3 $< | grep -o -b '\->' | tail -n 1 | cut -d : -f 1` \
	-expand `grep -o -b \\\\$$ $< | cut -d : -f 1`-\
	`tail -c +2 $< | grep -o -b € | cut -d : -f 1` \
	-printer ocaml-pp >$@


# Path completion
#################

PC_NUMBERS=$(call numbers,path)
PC_CASES=$(foreach n, $(PC_NUMBERS), path_$(n).ml)
PC_TESTS=$(subst .ml,_pcres.ml, $(PC_CASES))

$(PC_CASES): path.ml
	$(call split,path,$(PC_NUMBERS))

path_res.ml: $(PC_TESTS)
	cat $^ >$@

%_pcres.ml: %.ml %_pccomp.ml
	tail -n 1 $< >$@ ; echo "=>" >>$@ ; \
	head -c `grep -o -b \\\\$$ $< | cut -d : -f 1` $< | tail -n 1 >>$@ ; \
	cat $(subst .ml,_pccomp.ml, $<) >>$@ ; echo EOF\\n >>$@

# We use tail to perform +1 and +2 :-(
%_pccomp.ml: %.ml %_no_dollar.ml
	$(OCAMLWIZARD) completion $(subst .ml,_no_dollar.ml, $<) \
	-pos `grep -o -b \\\\$$ $< | cut -d : -f 1` -printer ocaml-pp >$@


# Completing with errors
########################

errors_res.ml: errors.ml errors_comp.ml
	cat $< >$@ ; echo "=>" >>$@ ; \
	head -c `grep -o -b \\\\$$ $< | cut -d : -f 1` $< | tail -n 1 >>$@ ; \
	cat $(subst .ml,_comp.ml, $<) >>$@ ; echo EOF\\n >>$@

# We use tail to perform +1 and +2 :-(
errors_comp.ml: errors.ml errors_no_dollar.ml
	$(OCAMLWIZARD) completion $(subst .ml,_no_dollar.ml, $<) \
	-pos `grep -o -b \\\\$$ $< | cut -d : -f 1` -printer ocaml-pp >$@


# Old

#test:
#	OCAMLRUNPARAM=b OCAMLLIB=/usr/lib/ocaml ../../boot/ocamlrun ../owz.byte completion test.ml -pos `wc -c test.ml` -printer ocaml-pp -expand 114-115

# LD_LIBRARY_PATH=$(LD_PATH)

#oracle:
#	../../../ocamlwizard-original/owz.opt completion test.ml -pos `wc -c test.ml` -printer ocaml-pp


#tests_cases.ml:
#	for

clean:
	rm -f *~ *.cm* *.annot match_cases_* expansion_* path_* errors_*
