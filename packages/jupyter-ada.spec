# SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
#
# SPDX-License-Identifier: MIT

%undefine _hardened_build
%define _gprdir %_GNAT_project_dir

Name:       jupyter-ada
Version:    0.1.0
Release:    git%{?dist}
Summary:    Jupyter Client API in Ada
Group:      Development/Libraries
License:    MIT
URL:        https://github.com/reznikmm/jupyter
### Direct download is not availeble
Source0:    jupyter-ada.tar.gz
BuildRequires:   gcc-gnat
BuildRequires:   fedora-gnat-project-common  >= 3
BuildRequires:   matreshka-devel
BuildRequires:   spawn-devel
BuildRequires:   gprbuild
BuildRequires:   zeromq-ada-devel
BuildRequires:   python3-jupyter-client
BuildRequires:   python3-nbconvert
BuildRequires:   which

# gprbuild only available on these:
ExclusiveArch: %GPRbuild_arches

%description
Jupyter Client API in Ada.

%package devel

Group:      Development/Libraries
License:    MIT
Summary:    Devel package for Jupyter Ada
Requires:       %{name}%{?_isa} = %{version}-%{release}
Requires:   fedora-gnat-project-common  >= 2

%description devel
Devel package for Jupyter Ada

%package kernel
Summary:    Jupyter Ada Kernel
License:    MIT
Group:      System Environment/Libraries
Requires:       %{name}%{?_isa} = %{version}-%{release}

%description kernel
The kernel to run an Ada program piece by piece.

%prep
%setup -q -n jupyter

%build
make  %{?_smp_mflags} GPRBUILD_FLAGS="%Gnatmake_optflags"

%install
rm -rf %{buildroot}
make install DESTDIR=%{buildroot} LIBDIR=%{_libdir} PREFIX=%{_prefix} GPRDIR=%{_gprdir} BINDIR=%{_bindir}

%check
make check LD_LIBRARY_PATH=$PWD/.libs

%post     -p /sbin/ldconfig
%postun   -p /sbin/ldconfig

%files
%doc LICENSES/MIT.txt
%dir %{_libdir}/jupyter-ada
%{_libdir}/jupyter-ada/libjupyterada.so.%{version}
%{_libdir}/libjupyterada.so.%{version}

%files devel
%doc README.md
%{_libdir}/jupyter-ada/libjupyterada.so
%{_libdir}/libjupyterada.so
%{_libdir}/jupyter-ada/*.ali
%{_includedir}/jupyter-ada
%{_gprdir}/jupyter.gpr
%{_gprdir}/manifests/jupyter

%files kernel
%{_bindir}/ada_kernel
%{_bindir}/ada_driver
%dir %{_datadir}/jupyter/kernels/ada
%{_datadir}/jupyter/kernels/ada/kernel.json
%{_gprdir}/manifests/jupyter_ada_{kernel,driver}
%dir %{_prefix}/lib/python3.8/site-packages/notebook/static/components/codemirror/mode/ada/
%{_prefix}/lib/python3.8/site-packages/notebook/static/components/codemirror/mode/ada/ada.js

%changelog
* Tue Jun 16 2020 Maxim Reznik <reznikmm@gmail.com> - 0.1.0-git
- Initial package
