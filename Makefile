
all:
	ocaml setup.ml -all

bin:
	ocaml setup.ml -build

tests:
	ocaml setup.ml -test

doc:
	ocaml setup.ml -doc

clean:
	ocaml setup.ml -clean

# install the main binary
install: all
	ocaml setup.ml -install
	#ocamlfind install $(NAME) META $(INSTALL)

reinstall: all
	ocaml setup.ml -reinstall

uninstall:
	ocaml setup.ml -uninstall

push_doc: doc
	scp -r bencode_rpc.docdir/* cedeela.fr:~/simon/root/software/bencode_rpc/

tags:
	find -name '*.mli?' | xargs otags 

.PHONY: install reinstall uninstall tags push_doc clean
