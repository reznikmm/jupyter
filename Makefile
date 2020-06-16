# SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
#
# SPDX-License-Identifier: MIT
#
GPRBUILD_FLAGS = -p -j0
PREFIX                 ?= /usr
GPRDIR                 ?= $(PREFIX)/share/gpr
LIBDIR                 ?= $(PREFIX)/lib
INSTALL_PROJECT_DIR    ?= $(DESTDIR)$(GPRDIR)
INSTALL_INCLUDE_DIR    ?= $(DESTDIR)$(PREFIX)/include/ada-jupyter
INSTALL_LIBRARY_DIR    ?= $(DESTDIR)$(LIBDIR)
INSTALL_ALI_DIR        ?= ${INSTALL_LIBRARY_DIR}/ada-jupyter

GPRINSTALL_FLAGS = --prefix=$(PREFIX) --sources-subdir=$(INSTALL_INCLUDE_DIR)\
 --lib-subdir=$(INSTALL_ALI_DIR) --project-subdir=$(INSTALL_PROJECT_DIR)\
--link-lib-subdir=$(INSTALL_LIBRARY_DIR)

all:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/jupyter.gpr
	gprbuild $(GPRBUILD_FLAGS) -P gnat/jupyter_hello_world.gpr

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/jupyter.gpr
clean:
	gprclean -q -P gnat/jupyter.gpr
	gprclean -q -P gnat/jupyter_hello_world.gpr

