LD_PATH=../../otherlibs/str:../../otherlibs/unix

test:
	OCAMLRUNPARAM=b OCAMLLIB=/usr/lib/ocaml ../../boot/ocamlrun ../owz.byte completion test.ml -pos `wc -c test.ml` -printer ocaml-pp -expand 114-115

# LD_LIBRARY_PATH=$(LD_PATH)

oracle:
	../../../ocamlwizard-original/owz.opt completion test.ml -pos `wc -c test.ml` -printer ocaml-pp

clean:
	rm -f *~ *.cm* *ocamlwizard_tmp_file.* *.annot