# CLISP Implementation Notes generation
# (c) 2000-2005 Sam Steingold

DTDVER=4.3
IMPNOTES=cl-ent.xml clhs-ent.xml impbody.xml impent.xml mop.xml gray.xml     \
	impext.xml impissue.xml impbyte.xml unix-ent.xml mop-ent.xml faq.xml \
	../modules/berkeley-db/ent-bdb.xml	\
	../modules/berkeley-db/berkeley-db.xml	\
	../modules/dirkey/dirkey.xml		\
	../modules/fastcgi/fastcgi.xml		\
	../modules/i18n/i18n.xml		\
	../modules/matlab/matlab.xml		\
	../modules/netica/netica.xml		\
	../modules/oracle/oracle.xml		\
	../modules/pari/pari.xml		\
	../modules/pcre/pcre.xml		\
	../modules/postgresql/postgresql.xml	\
	../modules/rawsock/rawsock.xml		\
	../modules/readline/readline.xml	\
	../modules/regexp/regexp.xml		\
	../modules/syscalls/syscalls.xml	\
	../modules/wildcard/wildcard.xml	\
	../modules/zlib/zlib.xml
OLINK=tdb-man.xml tdb-pile.xml tdb-chunk.xml olink-pile.xml olink-chunk.xml
RM=/bin/rm -f
# DOC=$(shell pwd)

ifneq (,$(filter Windows%,$(OS)))
JAVAXML=c:/java
JAVA=/cygdrive/c/java/bin/java.exe
SEP=;
DTD=/usr/share/docbook-xml$(DTDVER)/docbookx.dtd
FOP=/cygdrive/c/java/fop-0.20.5/fop.bat
else
JAVAXML=/usr/local/src/xml
JAVA=java
SEP=:
DTD=/usr/share/sgml/docbook/xml-dtd-$(DTDVER)/docbookx.dtd
FOP=fop
endif
DTD=../utils/docbook/docbookx.dtd
NSGMLS=onsgmls

# XMLSOC is found at wildly different locations. It's tedious to write down
# all possible pathnames found so far, but we know no better solution.
XMLSOC := $(firstword $(wildcard /usr/share/sgml/xml.soc \
                                 /usr/share/OpenSP/xml.soc \
                                 /usr/share/sgml/jade_dsl/xml.soc \
                                 /usr/share/sgml/openjade-1.3.1/pubtext/xml.soc \
                                 /usr/share/sgml/declaration/xml.soc \
                                 $(shell locate /xml.soc) \
                                 ))

XALAN=$(JAVAXML)/xalan-j_2_0_0
SAXON=$(JAVAXML)/saxon
CLASSPATH=$(XALAN)/bin/xerces.jar$(SEP)$(XALAN)/bin/xalan.jar$(SEP)$(SAXON)/saxon.jar

# name of XML catalog file to create
# (if not specified, no local catalog created or used)
CATALOG_FILE=catalog.xml

# Local path to DocBook XSL stylesheets on your system.
# Make sure to use a trailing slash.
STYLESHEETS_PATH=/usr/share/docbook-xsl-snapshot/

# Stylesheets URI used in the CLISP project *.xsl files.
STYLESHEETS_URI=http://docbook.sourceforge.net/release/xsl/current/

# The xmlcatalog utility (from libxml2)
XMLCATALOG=xmlcatalog

# chose one:
#TEXI2HTML=texi2html -monolithic -verbose $< -out_file $@
TEXI2HTML=makeinfo --verbose --no-split --no-headers --html $< -o $@

#DIST=cvs2.cons.org:/home/ftp/pub/lisp/clisp/snapshots
DIST=podval:~/public_html/clisp

# When writing docbook documentation, you need the "DocBook Definitive Guide"
# http://docbook.org/tdg/en/html/docbook-x.html

# this is the original DTD
# when processing with a tool which can get the DTD from the net,
# e.g., (open)jade, you can use this definition,
# otherwise you have to stick with the local DTDs
# we allow the use of a local DTD to avoid fetching it from the net each time
ifeq (,$(wildcard $(DTD)))
DTD=http://www.oasis-open.org/docbook/xml/$(DTDVER)/docbookx.dtd
#DTD=http://www.docbook.org/xml/$(DTDVER)/docbookx.dtd
endif

# to generate HTML, you will need:
# http://xml.apache.org/xalan-j
# http://nwalsh.com/docbook/xsl/dbx129.zip
#  or
# http://users.iclway.co.uk/mhkay/saxon/
#  or
# http://openjade.sourceforge.net/
# http://www.jclark.com/jade/

FILLIN=. ../version.sh && sed -e 's,@DTD@,$(DTD),' \
	    -e 's,@DTDVER@,$(DTDVER),' \
	    -e 's,@VERSION@,'$${VERSION_NUMBER}',' \
	    -e 's,@TODAY@,'`date +"%Y-%m-%d"`','

SGML_UNCOMMENT=-e 's/^<!--\#\(.*\)-->$$/\#\1/'

ROFF_UNCOMMENT=-e 's/^\.\\"<!\\-\\-\#\(.*\)\\-\\->$$/\#\1/'

all: check $(CATALOG_FILE) impnotes.html regexp.html man

ifneq (,$(CATALOG_FILE))
# depends on Makefile because of $(STYLESHEETS_PATH)
$(CATALOG_FILE): Makefile
	$(RM) $(CATALOG_FILE)
	$(XMLCATALOG) --create > $(CATALOG_FILE)
	$(XMLCATALOG) --noout --add "rewriteURI" \
		$(STYLESHEETS_URI) $(STYLESHEETS_PATH) $(CATALOG_FILE)
	$(XMLCATALOG) --noout --add "rewriteSystem" \
		$(STYLESHEETS_URI) $(STYLESHEETS_PATH) $(CATALOG_FILE)
XSLTPROC=XML_CATALOG_FILES="$(CATALOG_FILE) ${XML_CATALOG_FILES}" xsltproc
else
XSLTPROC=xsltproc
endif

MAKE_TDB=$(XSLTPROC) --timing --stringparam collect.xref.targets "only" \
	--stringparam targets.filename

tdb-man.xml: clisp.xml pile.xsl common.xsl
	$(MAKE_TDB) "$@" pile.xsl $<

tdb-pile.xml: impnotes.xml $(IMPNOTES) pile.xsl common.xsl
	$(MAKE_TDB) "$@" pile.xsl $<

tdb-chunk.xml: impnotes.xml $(IMPNOTES) chunk.xsl common.xsl
	$(MAKE_TDB) "$@" chunk.xsl $<

clisp.xml: clisp.xml.in ../version.sh Makefile
	$(RM) $@
	$(FILLIN) $< > $@

XMLOUT=$(XSLTPROC) --timing --stringparam target.database.document
DOC_MAN=--stringparam current.docid "man"
DOC_IMP=--stringparam current.docid "impnotes"

clisp.html: clisp.xml common.xsl pile.xsl $(OLINK)
	$(XMLOUT) "olink-pile.xml" $(DOC_MAN) -o $@ pile.xsl $<

_clisp.html: clisp.html
	sed $(SGML_UNCOMMENT) $< > $@

clisp.1: clisp.xml common.xsl man.xsl $(OLINK)
	$(XMLOUT) "olink-pile.xml" $(DOC_MAN) -o $@ man.xsl $<

_clisp.1: clisp.1
	sed $(ROFF_UNCOMMENT) $< > $@

impnotes.xml: impnotes.xml.in $(IMPNOTES) ../version.sh Makefile
	$(RM) $@
	$(FILLIN) $< > $@

check: impnotes.xml $(IMPNOTES) clisp.xml
	$(NSGMLS) -s -e -g -c$(XMLSOC) clisp.xml
	$(NSGMLS) -s -e -g -c$(XMLSOC) impnotes.xml
	xmllint --noout --valid --postvalid --timing --noent clisp.xml
	xmllint --noout --valid --postvalid --timing --noent impnotes.xml

impnotes.texi: impnotes.xml $(IMPNOTES)
	time docbook2texi $<

impnotes.html: impnotes.xml $(IMPNOTES) common.xsl pile.xsl $(OLINK)
	$(XMLOUT) "olink-pile.xml" $(DOC_IMP) -o $@ pile.xsl $<
#	cp impnotes.html impnotes-saved.html
#	tidy -config tidy.conf -f tidy.err impnotes.html || true
	egrep -Hn 'id="id[0-9]*"' $@ || true
	(test -n "$(DIST)" && scp -p impnotes.html impnotes.css $(DIST)/) \
		|| true

man: _clisp.html _clisp.1
	(cd ../build && make clisp.html clisp.1 && \
	 test -n "$(DIST)" && scp -p clisp.1 clisp.html $(DIST)/) || true

impnotes.fo: impnotes.xml $(IMPNOTES) fo.xsl
	$(XMLOUT) "olink-pile.xml" $(DOC_IMP) -o $@ fo.xsl $<

#impnotes.pdf: impnotes.fo
#	$(FOP) -fo $< -pdf $@

impnotes.pdf: impnotes.fo # impnotes.xml $(IMPNOTES) fo.xsl $(OLINK)
#	./xep.bat -xml $< -xsl fo.xsl -out $@
	./xep.bat -fo $< -out $@

html: impnotes.xml $(IMPNOTES) common.xsl chunk.xsl pile.xsl clisp.xml $(OLINK)
	if [ ! -h $@ ]; then $(RM) -r $@; ln -s ../../sf/www/impnotes $@; fi
	cd html && grep -v -e "^#" .symlinks | sed 's/^/ln -vf /' | sh -
	$(XMLOUT) "olink-chunk.xml" $(DOC_IMP) -o $@/ chunk.xsl $<
	$(XSLTPROC) --timing -o $@/id-href.map id-href-map.xsl tdb-chunk.xml
	$(XMLOUT) "olink-chunk.xml" $(DOC_MAN) -o $@/clisp.html pile.xsl clisp.xml
	sed $(SGML_UNCOMMENT) $@/clisp.html > _clisp-1.html
	cd ../build; make clisp-1.html;
	mv -f ../build/clisp-1.html $@/clisp.html
	rm -f _clisp-1.html
#	time xmlto xhtml -o $@/ -m chunk.xsl $<
#	time docbook2html --dsl impnotes.dsl -o $@ $<
	egrep -Hnr 'id="id[0-9]*"' $@ || true
	(test -n "$(DIST)" && scp -p html/*.html html/id-href.map impnotes.css \
	 $(DIST)/impnotes/;) || true

up: impnotes.html html
	scp impnotes.html sf:/home/groups/c/cl/clisp/www/impnotes-new.html
	scp -r html/* sf:/home/groups/c/cl/clisp/www/impnotes-new/

regexp.html: ../modules/regexp/regexp.texinfo
	$(TEXI2HTML)

clean: force
	$(RM) impnotes.xml impnotes.texi impnotes.html impnotes.pdf html \
		clisp.html clisp.1 tdb-*.xml $(CATALOG_FILE) impnotes.fo

count: $(IMPNOTES)
	wc $^

force:
