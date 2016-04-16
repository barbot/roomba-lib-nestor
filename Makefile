OCAMLB=ocamlbuild
TARGET=control
OCAMLCP=ocamlcp
LIB=-I,+lablgl
LIBS=lablgl,lablglut
OCAMLBUILDFLAGS=-use-ocamlfind
OCAMLBUILDFLAGS2=-use-ocamlfind -libs lablgl,lablglut -lflags $(LIB) -cflags $(LIB)
LIBOPT=-noassert,-unsafe
DYNLIB=-cclib,/opt/local/lib/libgtk-x11-2.0.0.dylib,-cclib,/usr/lib/libSystem.B.dylib
TYPE=native

.PHONY: control web server ams clean

control: 
	$(OCAMLB) $(OCAMLBUILDFLAGS2) control.$(TYPE)

lib:
	$(OCAMLB) $(OCAMLBUILDFLAGS) roomba_lib.cma

doc:
	$(OCAMLB) $(OCAMLBUILDFLAGS) doc.docdir/index.html


ams: 
	$(OCAMLB) $(OCAMLBUILDFLAGS) roomba_ams.$(TYPE)

web:
	$(OCAMLB) $(OCAMLBUILDFLAGS) serverWeb.$(TYPE)

server: 
	$(OCAMLB) $(OCAMLBUILDFLAGS) server.$(TYPE)


testdisplay:
	$(OCAMLB) $(OCAMLBUILDFLAGS2) testdisplay.native

clean:
	$(OCAMLB) -clean
