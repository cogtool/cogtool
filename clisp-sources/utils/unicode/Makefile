
# Destination directory. Define to empty for the current directory.
DESTDIR = ../../src/

# Directory containing conversion tables.
# You have to unpack the conversion-tables.tar.gz package.
TABLESDIR = /home/bruno/chs/conversion-tables

ALL = $(DESTDIR)uni_upcase.c $(DESTDIR)uni_downcase.c $(DESTDIR)uni_attribute.c \
 $(DESTDIR)nls_ascii.c \
 $(DESTDIR)nls_iso8859_1.c \
 $(DESTDIR)nls_iso8859_2.c \
 $(DESTDIR)nls_iso8859_3.c \
 $(DESTDIR)nls_iso8859_4.c \
 $(DESTDIR)nls_iso8859_5.c \
 $(DESTDIR)nls_iso8859_6.c \
 $(DESTDIR)nls_iso8859_7.c \
 $(DESTDIR)nls_iso8859_8.c \
 $(DESTDIR)nls_iso8859_9.c \
 $(DESTDIR)nls_iso8859_10.c \
 $(DESTDIR)nls_iso8859_13.c \
 $(DESTDIR)nls_iso8859_14.c \
 $(DESTDIR)nls_iso8859_15.c \
 $(DESTDIR)nls_iso8859_16.c \
 $(DESTDIR)nls_koi8_r.c \
 $(DESTDIR)nls_koi8_u.c \
 $(DESTDIR)nls_mac_arabic.c \
 $(DESTDIR)nls_mac_centraleurope.c \
 $(DESTDIR)nls_mac_croatian.c \
 $(DESTDIR)nls_mac_cyrillic.c \
 $(DESTDIR)nls_mac_dingbat.c \
 $(DESTDIR)nls_mac_greek.c \
 $(DESTDIR)nls_mac_hebrew.c \
 $(DESTDIR)nls_mac_iceland.c \
 $(DESTDIR)nls_mac_roman.c \
 $(DESTDIR)nls_mac_romania.c \
 $(DESTDIR)nls_mac_symbol.c \
 $(DESTDIR)nls_mac_thai.c \
 $(DESTDIR)nls_mac_turkish.c \
 $(DESTDIR)nls_mac_ukraine.c \
 $(DESTDIR)nls_cp437_ms.c \
 $(DESTDIR)nls_cp437_ibm.c \
 $(DESTDIR)nls_cp737.c \
 $(DESTDIR)nls_cp775.c \
 $(DESTDIR)nls_cp850.c \
 $(DESTDIR)nls_cp852_ms.c \
 $(DESTDIR)nls_cp852_ibm.c \
 $(DESTDIR)nls_cp855.c \
 $(DESTDIR)nls_cp857.c \
 $(DESTDIR)nls_cp860_ms.c \
 $(DESTDIR)nls_cp860_ibm.c \
 $(DESTDIR)nls_cp861_ms.c \
 $(DESTDIR)nls_cp861_ibm.c \
 $(DESTDIR)nls_cp862_ms.c \
 $(DESTDIR)nls_cp862_ibm.c \
 $(DESTDIR)nls_cp863_ms.c \
 $(DESTDIR)nls_cp863_ibm.c \
 $(DESTDIR)nls_cp864_ms.c \
 $(DESTDIR)nls_cp864_ibm.c \
 $(DESTDIR)nls_cp865_ms.c \
 $(DESTDIR)nls_cp865_ibm.c \
 $(DESTDIR)nls_cp866.c \
 $(DESTDIR)nls_cp869_ms.c \
 $(DESTDIR)nls_cp869_ibm.c \
 $(DESTDIR)nls_cp874_ms.c \
 $(DESTDIR)nls_cp874_ibm.c \
 $(DESTDIR)nls_cp1250.c \
 $(DESTDIR)nls_cp1251.c \
 $(DESTDIR)nls_cp1252.c \
 $(DESTDIR)nls_cp1253.c \
 $(DESTDIR)nls_cp1254.c \
 $(DESTDIR)nls_cp1256.c \
 $(DESTDIR)nls_cp1257.c \
 $(DESTDIR)nls_hp_roman8.c \
 $(DESTDIR)nls_nextstep.c \
 $(DESTDIR)nls_jisx0201.c

CC = gcc -O -Wall
ICONV = iconv_prog --loose

all : $(ALL)

$(DESTDIR)nls_ascii.c : $(TABLESDIR)/glibc-2.1-iconv/ASCII.TXT nls_table
	./nls_table ascii nls_ascii.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_1.c : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-1.TXT nls_table
	./nls_table iso8859-1 nls_iso8859_1.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_2.c : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-2.TXT nls_table
	./nls_table iso8859-2 nls_iso8859_2.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_3.c : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-3.TXT nls_table
	./nls_table iso8859-3 nls_iso8859_3.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_4.c : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-4.TXT nls_table
	./nls_table iso8859-4 nls_iso8859_4.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_5.c : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-5.TXT nls_table
	./nls_table iso8859-5 nls_iso8859_5.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_6.c : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-6.TXT nls_table
	./nls_table iso8859-6 nls_iso8859_6.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_7.c : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-7.TXT nls_table
	./nls_table iso8859-7 nls_iso8859_7.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_8.c : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-8.TXT nls_table
	./nls_table iso8859-8 nls_iso8859_8.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_9.c : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-9.TXT nls_table
	./nls_table iso8859-9 nls_iso8859_9.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_10.c : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-10.TXT nls_table
	./nls_table iso8859-10 nls_iso8859_10.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_13.c : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-13.TXT nls_table
	./nls_table iso8859-13 nls_iso8859_13.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_14.c : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-14.TXT nls_table
	./nls_table iso8859-14 nls_iso8859_14.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_15.c : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-15.TXT nls_table
	./nls_table iso8859-15 nls_iso8859_15.c $(DESTDIR) < $<

$(DESTDIR)nls_iso8859_16.c : $(TABLESDIR)/csets-new/ISO-8859-16.TXT nls_table
	./nls_table iso8859-16 nls_iso8859_16.c $(DESTDIR) < $<

$(DESTDIR)nls_koi8_r.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MISC/KOI8-R.TXT nls_table
	./nls_table koi8-r nls_koi8_r.c $(DESTDIR) < $<

$(DESTDIR)nls_koi8_u.c : $(TABLESDIR)/csets-new/KOI8-U.TXT nls_table
	./nls_table koi8-u nls_koi8_u.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_arabic.c : $(TABLESDIR)/jdk-1.1.7b/MAC-ARABIC.TXT nls_table
	./nls_table mac-arabic nls_mac_arabic.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_centraleurope.c : $(TABLESDIR)/jdk-1.1.7b/MAC-CENTRAL-EUROPE.TXT nls_table
	./nls_table mac-centraleurope nls_mac_centraleurope.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_croatian.c : $(TABLESDIR)/jdk-1.1.7b/MAC-CROATIAN.TXT nls_table
	./nls_table mac-croatian nls_mac_croatian.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_cyrillic.c : $(TABLESDIR)/jdk-1.1.7b/MAC-CYRILLIC.TXT nls_table
	./nls_table mac-cyrillic nls_mac_cyrillic.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_dingbat.c : $(TABLESDIR)/jdk-1.1.7b/MAC-DINGBAT.TXT nls_table
	./nls_table mac-dingbat nls_mac_dingbat.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_greek.c : $(TABLESDIR)/jdk-1.1.7b/MAC-GREEK.TXT nls_table
	./nls_table mac-greek nls_mac_greek.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_hebrew.c : $(TABLESDIR)/jdk-1.1.7b/MAC-HEBREW.TXT nls_table
	./nls_table mac-hebrew nls_mac_hebrew.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_iceland.c : $(TABLESDIR)/jdk-1.1.7b/MAC-ICELAND.TXT nls_table
	./nls_table mac-iceland nls_mac_iceland.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_roman.c : $(TABLESDIR)/jdk-1.1.7b/MAC-ROMAN.TXT nls_table
	./nls_table mac-roman nls_mac_roman.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_romania.c : $(TABLESDIR)/jdk-1.1.7b/MAC-ROMANIA.TXT nls_table
	./nls_table mac-romania nls_mac_romania.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_symbol.c : $(TABLESDIR)/jdk-1.1.7b/MAC-SYMBOL.TXT nls_table
	./nls_table mac-symbol nls_mac_symbol.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_thai.c : $(TABLESDIR)/jdk-1.1.7b/MAC-THAI.TXT nls_table
	./nls_table mac-thai nls_mac_thai.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_turkish.c : $(TABLESDIR)/jdk-1.1.7b/MAC-TURKISH.TXT nls_table
	./nls_table mac-turkish nls_mac_turkish.c $(DESTDIR) < $<

$(DESTDIR)nls_mac_ukraine.c : $(TABLESDIR)/jdk-1.1.7b/MAC-UKRAINE.TXT nls_table
	./nls_table mac-ukraine nls_mac_ukraine.c $(DESTDIR) < $<

$(DESTDIR)nls_cp437_ms.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP437.TXT nls_table
	./nls_table cp437-ms nls_cp437_ms.c $(DESTDIR) < $<

$(DESTDIR)nls_cp437_ibm.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/IBM.OLD/PC/CP437.TXT nls_table
	./nls_table cp437-ibm nls_cp437_ibm.c $(DESTDIR) < $<

$(DESTDIR)nls_cp737.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP737.TXT nls_table
	./nls_table cp737 nls_cp737.c $(DESTDIR) < $<

$(DESTDIR)nls_cp775.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP775.TXT nls_table
	./nls_table cp775 nls_cp775.c $(DESTDIR) < $<

$(DESTDIR)nls_cp850.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP850.TXT nls_table
	./nls_table cp850 nls_cp850.c $(DESTDIR) < $<

$(DESTDIR)nls_cp852_ms.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP852.TXT nls_table
	./nls_table cp852-ms nls_cp852_ms.c $(DESTDIR) < $<

$(DESTDIR)nls_cp852_ibm.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/IBM.OLD/PC/CP852.TXT nls_table
	./nls_table cp852-ibm nls_cp852_ibm.c $(DESTDIR) < $<

$(DESTDIR)nls_cp855.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP855.TXT nls_table
	./nls_table cp855 nls_cp855.c $(DESTDIR) < $<

$(DESTDIR)nls_cp857.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP857.TXT nls_table
	./nls_table cp857 nls_cp857.c $(DESTDIR) < $<

$(DESTDIR)nls_cp860_ms.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP860.TXT nls_table
	./nls_table cp860-ms nls_cp860_ms.c $(DESTDIR) < $<

$(DESTDIR)nls_cp860_ibm.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/IBM.OLD/PC/CP860.TXT nls_table
	./nls_table cp860-ibm nls_cp860_ibm.c $(DESTDIR) < $<

$(DESTDIR)nls_cp861_ms.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP861.TXT nls_table
	./nls_table cp861-ms nls_cp861_ms.c $(DESTDIR) < $<

$(DESTDIR)nls_cp861_ibm.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/IBM.OLD/PC/CP861.TXT nls_table
	./nls_table cp861-ibm nls_cp861_ibm.c $(DESTDIR) < $<

$(DESTDIR)nls_cp862_ms.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP862.TXT nls_table
	./nls_table cp862-ms nls_cp862_ms.c $(DESTDIR) < $<

$(DESTDIR)nls_cp862_ibm.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/IBM.OLD/PC/CP862.TXT nls_table
	./nls_table cp862-ibm nls_cp862_ibm.c $(DESTDIR) < $<

$(DESTDIR)nls_cp863_ms.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP863.TXT nls_table
	./nls_table cp863-ms nls_cp863_ms.c $(DESTDIR) < $<

$(DESTDIR)nls_cp863_ibm.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/IBM.OLD/PC/CP863.TXT nls_table
	./nls_table cp863-ibm nls_cp863_ibm.c $(DESTDIR) < $<

$(DESTDIR)nls_cp864_ms.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP864.TXT nls_table
	./nls_table cp864-ms nls_cp864_ms.c $(DESTDIR) < $<

$(DESTDIR)nls_cp864_ibm.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/IBM.OLD/PC/CP864.TXT nls_table
	./nls_table cp864-ibm nls_cp864_ibm.c $(DESTDIR) < $<

$(DESTDIR)nls_cp865_ms.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP865.TXT nls_table
	./nls_table cp865-ms nls_cp865_ms.c $(DESTDIR) < $<

$(DESTDIR)nls_cp865_ibm.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/IBM.OLD/PC/CP865.TXT nls_table
	./nls_table cp865-ibm nls_cp865_ibm.c $(DESTDIR) < $<

$(DESTDIR)nls_cp866.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP866.TXT nls_table
	./nls_table cp866 nls_cp866.c $(DESTDIR) < $<

$(DESTDIR)nls_cp869_ms.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP869.TXT nls_table
	./nls_table cp869-ms nls_cp869_ms.c $(DESTDIR) < $<

$(DESTDIR)nls_cp869_ibm.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/IBM.OLD/PC/CP869.TXT nls_table
	./nls_table cp869-ibm nls_cp869_ibm.c $(DESTDIR) < $<

$(DESTDIR)nls_cp874_ms.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP874.TXT nls_table
	./nls_table cp874-ms nls_cp874_ms.c $(DESTDIR) < $<

$(DESTDIR)nls_cp874_ibm.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/IBM.OLD/PC/CP874.TXT nls_table
	./nls_table cp874-ibm nls_cp874_ibm.c $(DESTDIR) < $<

$(DESTDIR)nls_cp1250.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1250.TXT nls_table
	./nls_table cp1250 nls_cp1250.c $(DESTDIR) < $<

$(DESTDIR)nls_cp1251.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1251.TXT nls_table
	./nls_table cp1251 nls_cp1251.c $(DESTDIR) < $<

$(DESTDIR)nls_cp1252.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1252.TXT nls_table
	./nls_table cp1252 nls_cp1252.c $(DESTDIR) < $<

$(DESTDIR)nls_cp1253.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1253.TXT nls_table
	./nls_table cp1253 nls_cp1253.c $(DESTDIR) < $<

$(DESTDIR)nls_cp1254.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1254.TXT nls_table
	./nls_table cp1254 nls_cp1254.c $(DESTDIR) < $<

$(DESTDIR)nls_cp1256.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1256.TXT nls_table
	./nls_table cp1256 nls_cp1256.c $(DESTDIR) < $<

$(DESTDIR)nls_cp1257.c : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1257.TXT nls_table
	./nls_table cp1257 nls_cp1257.c $(DESTDIR) < $<

$(DESTDIR)nls_hp_roman8.c : $(TABLESDIR)/glibc-2.1-iconv/HP-ROMAN8.TXT nls_table
	./nls_table hp-roman8 nls_hp_roman8.c $(DESTDIR) < $<

$(DESTDIR)nls_nextstep.c : $(TABLESDIR)/clisp/NEXTSTEP.TXT nls_table
	./nls_table nextstep nls_nextstep.c $(DESTDIR) < $<

$(DESTDIR)nls_jisx0201.c : $(TABLESDIR)/clisp/JIS_X0201.TXT nls_table
	./nls_table jisx0201 nls_jisx0201.c $(DESTDIR) < $<

nls_table : nls_table.c
	$(CC) nls_table.c -o nls_table


clean : force
	rm -f *.class nls_table

force :

