Summary: nesC compiler 
Name: nesc
Version: 1.1.1
Release: 2
License: Please see source
Packager: TinyOS/NesC Group, UC Berkeley
Group: Development/Tools
URL: http://sourceforge.net/projects/nescc
Source0: %{name}-%{version}.tar.gz
Requires: avr-binutils >= 2.13.2.1, avr-gcc >= 3.3, avr-libc

%description
nesC is a compiler for a new, C-based language designed to 
support the TinyOS project. nesC provides several advantages
over the existing TinyOS compiler infrastructure: improved
syntax, abundant error reporting, and Java-like interfaces.

%prep
%setup -q

%build
./configure TOSDIR=/opt/tinyos-1.x/tos
make 

%install
make install

%clean
rm -rf $RPM_BUILD_DIR/%{name}-%{version}

%files
/usr/local/lib/ncc/
/usr/local/bin/mig
/usr/local/bin/ncg
/usr/local/bin/ncc
/usr/local/bin/nescc
/usr/local/bin/nesdoc
%defattr(-,root,root,-)

%changelog
* Wed Mar 31 2004 root <kwright@cs.berkeley.edu> 1.1.1
- Resurrect because we cannot have noarch for binaries
* Fri Sep 26 2003 root <kwright@cs.berkeley.edu> 1.1-1
- New source
* Fri Sep 19 2003 root <kwright@cs.berkeley.edu> 1.1pre4-2
- Removed set-mote-id
* Fri Sep 12 2003 root <kwright@cs.berkeley.edu> 1.1pre4-1
- New source
* Fri Aug 15 2003 root <kwright@cs.berkeley.edu> 1.1pre2-1
- Initial build.
