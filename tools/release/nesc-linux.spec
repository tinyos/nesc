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
nesC is a compiler for a new C-based language designed to 
support the TinyOS project. nesC provides several advantages
over the existing TinyOS compiler infrastructure: improved
syntax, abundant error reporting, and Java-like interfaces.

%prep
%setup -q

%build
./configure TOSDIR=/opt/tinyos-1.x/tos/
make 

%install
make install 

%files
/usr/local/lib/ncc/
/usr/local/bin/mig
/usr/local/bin/ncg
/usr/local/bin/ncc
/usr/local/bin/nescc
/usr/local/bin/nesdoc

%defattr(-,root,root,-)
%doc

%changelog
* Wed Mar 31 2004 root <kwright@cs.berkeley.edu>
- Resurrect because we have binaries can't do noarch
* Fri Feb 06 2004 root <kwright@cs.utah.edu> 
- Make noarch version
* Fri Sep 26 2003 root <kwright@cs.utah.edu> 1.1
- New source
* Fri Sep 19 2003 root <kwright@cs.utah.edu> 1.1pre4-2
- Removed set-mote-id
* Fri Aug 26 2003 root <kwright@cs.utah.edu> 1.1pre3-1p
- Initial build.



