OCAMLB=ocamlbuild
TARGET=control
OCAMLCP=ocamlcp
LIB=-I,+threads
#-I,+lablgl,
LIBS=str,unix,threads
#,lablgl,lablglut
OCAMLBUILDFLAGS=-use-ocamlfind -libs $(LIBS) -lflags $(LIB) -cflags $(LIB)
OCAMLBUILDFLAGS2=-libs str,unix,threads,graphics -lflags $(LIB) -cflags $(LIB)
LIBOPT=-noassert,-unsafe
DYNLIB=-cclib,/opt/local/lib/libgtk-x11-2.0.0.dylib,-cclib,/usr/lib/libSystem.B.dylib
TYPE=byte

.PHONY: control web server ams clean

control: 
	$(OCAMLB) $(OCAMLBUILDFLAGS) control.$(TYPE)

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
	$(OCAMLB) $(OCAMLBUILDFLAGS) testdisplay.native

clean:
	$(OCAMLB) -clean
