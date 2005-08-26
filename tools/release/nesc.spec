# 
# The source must be in a tgz with the 
# name %{target}-%{version}-binutils.tgz.
# When unfolded, the top-level directory 
# must be %{target}-%{version}.
# 

%define version 1.2
%define theprefix /usr

Summary: nesC compiler 
Name: nesc
Version: 1.2
Release: 1
License: GNU GPL Version 2
Packager: TinyOS Group, UC Berkeley
Group: Development/Tools
URL: http://sourceforge.net/projects/nescc
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-root

%description
nesC is a compiler for a new, C-based language designed to 
support the TinyOS project. nesC provides several advantages
over the existing TinyOS compiler infrastructure: improved
syntax, abundant error reporting, and Java-like interfaces.

%prep
%setup -q

%build
./configure --prefix=%{theprefix}
make 

%install
rm -rf %{buildroot}%{theprefix}
make prefix=%{buildroot}%{theprefix} install

%clean
rm -rf $RPM_BUILD_DIR/%{name}-%{version}

%files
%defattr(-,root,root,-)
%{theprefix}
%doc

%changelog
* Mon Mar 14 2005  <kwright@cs.berkeley.edu> 1.1.2b
- Version 1.1.2b; use buildroot
* Tue Jul 27 2004  <dgay@intel-research.net> 1.1.2-1w
- Version 1.1.2
* Fri Sep 26 2003 root <kwright@cs.utah.edu> 1.1-1
- New source
* Fri Sep 19 2003 root <kwright@cs.utah.edu> 1.1pre4-2
- Removed set-mote-id
* Fri Sep 12 2003 root <kwright@cs.utah.edu> 1.1pre4-1
- New source
* Fri Aug 15 2003 root <kwright@cs.utah.edu> 1.1pre2-1
- Initial build.

