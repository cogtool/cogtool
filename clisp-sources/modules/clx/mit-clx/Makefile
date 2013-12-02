# Directories used by "make install":
prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib
lisplibdir = $(libdir)/lisp

CLISP = clisp
LISP = $(CLISP)

ALLLISP = attributes.lisp buffer.lisp bufmac.lisp clx.lisp depdefs.lisp \
  dependent.lisp describe.lisp display.lisp fonts.lisp gcontext.lisp \
  graphics.lisp image.lisp input.lisp keysyms.lisp macros.lisp manager.lisp \
  package.lisp requests.lisp resource.lisp text.lisp trace.lisp translate.lisp \
  shape.lisp

ALLFAS =  attributes.fas buffer.fas bufmac.fas clx.fas depdefs.fas \
  dependent.fas describe.fas display.fas fonts.fas gcontext.fas \
  graphics.fas image.fas input.fas keysyms.fas macros.fas manager.fas \
  package.fas requests.fas resource.fas text.fas trace.fas translate.fas \
  shape.fas


# Choose one of the packers:

# Standard Unix packer. Requires GNU tar.
PACK = tar
PACKOPT = cvfhz
PACKEXT = .tar.Z

# The one with the best performance.
#PACK = lha
#PACKOPT = a
#PACKEXT = .lzh

# Popular DOS packer.
#PACK = zip
#PACKOPT = -r
#PACKEXT = .zip

# Popular Atari packer.
#PACK = zoo
#PACKOPT = -add
#PACKEXT = .zoo

SHELL = /bin/sh

DISTRIBFILES = link.sh Makefile README.CLISP $(ALLLISP) $(ALLFAS)
distribdir =

all: defsystem.fas stamp.fas $(ALLFAS) clx.mem

defsystem.fas defsystem.lib : defsystem.lisp
	$(LISP) -q -c defsystem

$(ALLFAS) : stamp.fas

stamp.fas : $(ALLLISP) defsystem.fas
	$(LISP) -m 4MB -q -i defsystem -x '(compile-clx)'
	touch stamp.fas

clx.mem : defsystem.fas $(ALLFAS)
	$(LISP) -m 4MB -q -i defsystem -x '(load-clx "" :macrosp t) (saveinitmem)'
	mv lispinit.mem clx.mem


install : clx.mem
	cp clx.mem $(lisplibdir)/clx.mem
	(echo '#!/bin/sh' ; echo '$(LISP) -m 4MB -M $(lisplibdir)/clx.mem "$$@"') > $(bindir)/clx
	chmod a+x $(bindir)/clx


distrib : src-distrib clisp-distrib

src-distrib : force
	$(PACK) $(PACKOPT) /tmp/clx-src$(PACKEXT) README Makefile *.lisp *.txt

clisp-distrib : force clx.mem
	$(PACK) $(PACKOPT) /tmp/clx$(PACKEXT) README clx.mem


# Make a module
clisp-module : defsystem.fas stamp.fas $(ALLFAS)

# Make a module distribution into $(distribdir)

clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)


clean: force
	-rm -f *.lib *.fas *.mem

force:

