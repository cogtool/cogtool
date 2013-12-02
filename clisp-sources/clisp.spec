# Copyright (C) 1998-2005 by Sam Steingold
# Distributed under the terms of the GNU General Public License.
# See <http://www.gnu.org/copyleft/gpl.html>.

# The purpose of this file is creation of source/binary RPMs,
# **NOT** building/installing CLISP.
# I work from a CVS sandbox, so unpacking and patching is irrelevant to me.

# to create the source/binary RPMs, do
#  rpmbuild -bb --sign clisp.spec

%define src /usr/local/src
%define prefix /usr
%define name clisp
%define version %(. version.sh; echo ${VERSION_NUMBER})
%define builddir build-rpm
%define mysrc %{src}/%{name}/current
# do not strip CLISP binaries, otherwise (disassemble #'cons) won't work
%define debug_package %{nil}

# don't you just love that you have to fit the macro into one line?
# this automatically upgrades `release' with each build.
# don't forget to remove the file `.release' when changing `version'.
#%define release %(test -f .release || echo 0 >> .release; echo "1 + " `cat .release` | bc > .,release; mv -fv .,release .release; cat .release)
#%define release %(cat .release)
%define release 1
%define modules rawsock berkeley-db pcre bindings/glibc clx/new-clx zlib

Summary:      Common Lisp (ANSI CL) implementation
Name:         %{name}
Version:      %{version}
Release:      %{release}
# this crap does not accept PNG
#Icon:         clisp.png
License:      GPL
Group:        development/languages
Source:       ftp://ftp.gnu.org/pub/gnu/clisp/latest/
URL:          http://clisp.cons.org/
Packager:     Sam Steingold <sds@gnu.org>
Provides:     clisp, ansi-cl
Distribution: Fedora Core GNU/Linux
BuildRoot:    %{_tmppath}/%{name}-root
%description
%(cat SUMMARY)

This binary distribution was built with the following modules:
 base: i18n regexp syscalls (run "clisp" or "clisp -K base" to use them)
 full: in addition to the above, also
   %{modules}
 (run "clisp -K full" to use them)

%prep
cat <<EOF
This will build RPMs for CLISP: %{name}-%{version}-%{release}.
We assume that you are in the top level source directory already.
No unpacking or patching is done - we go straight to build and
creating the RPMs.  See 'clisp.spec' for more information.
EOF
%setup -T -D -n %{mysrc}
%build
# build CLISP
# this has to be done just once - comment it out if you did this already
#rm -rf %{builddir}
#MODS=''; for m in %{modules}; do MODS=${MODS}' '--with-module=$m; done
#./configure --prefix=%{prefix} --fsstnd=redhat ${MODS} --build %{builddir}
%install
cd %{builddir}
make DESTDIR=$RPM_BUILD_ROOT install

# create the source tar, necessary for source RPMs
# this has to be done just once - comment it out if you did this already
#cd %{mysrc}
#make -f Makefile.devel src-distrib
#ln /tmp/%{name}-%{version}.tar.bz2 /usr/src/redhat/SOURCES/
%clean
rm -rf $RPM_BUILD_ROOT
%files
%{prefix}/bin/clisp
%{prefix}/lib/clisp/
%{prefix}/share/doc/%{name}-%{version}/
%{prefix}/share/man/man1/clisp.1.gz
%{prefix}/share/locale/de/LC_MESSAGES/clisp.mo
%{prefix}/share/locale/de/LC_MESSAGES/clisplow.mo
%{prefix}/share/locale/en/LC_MESSAGES/clisp.mo
%{prefix}/share/locale/en/LC_MESSAGES/clisplow.mo
%{prefix}/share/locale/es/LC_MESSAGES/clisp.mo
%{prefix}/share/locale/es/LC_MESSAGES/clisplow.mo
%{prefix}/share/locale/fr/LC_MESSAGES/clisp.mo
%{prefix}/share/locale/fr/LC_MESSAGES/clisplow.mo
%{prefix}/share/locale/nl/LC_MESSAGES/clisp.mo
%{prefix}/share/locale/nl/LC_MESSAGES/clisplow.mo
%{prefix}/share/locale/ru/LC_MESSAGES/clisp.mo
%{prefix}/share/locale/ru/LC_MESSAGES/clisplow.mo
%{prefix}/share/locale/da/LC_MESSAGES/clisp.mo
%{prefix}/share/locale/da/LC_MESSAGES/clisplow.mo
