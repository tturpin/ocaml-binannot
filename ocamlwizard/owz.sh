#!/bin/bash
OWZ=owz.opt
PROJECT_DIR=`$OWZ -find-project-dir $* 2>/dev/null`
echo ocamlwizard $* >$PROJECT_DIR/.ocamlwizard-stderr
OCAMLLIB=/usr/local/lib/ocaml $OWZ -debug -backtrace $* \
2>>$PROJECT_DIR/.ocamlwizard-stderr | tee $PROJECT_DIR/.ocamlwizard-stdout
exit ${PIPESTATUS[0]}
