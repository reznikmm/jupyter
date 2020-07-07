# SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
#
# SPDX-License-Identifier: MIT
#
GPRBUILD_FLAGS = -p -j0
PREFIX                 ?= /usr
GPRDIR                 ?= $(PREFIX)/share/gpr
LIBDIR                 ?= $(PREFIX)/lib
BINDIR                 ?= $(PREFIX)/bin
INSTALL_PROJECT_DIR    ?= $(DESTDIR)$(GPRDIR)
INSTALL_INCLUDE_DIR    ?= $(DESTDIR)$(PREFIX)/include/jupyter-ada
INSTALL_EXEC_DIR       ?= $(DESTDIR)$(BINDIR)
INSTALL_LIBRARY_DIR    ?= $(DESTDIR)$(LIBDIR)
INSTALL_ALI_DIR        ?= ${INSTALL_LIBRARY_DIR}/jupyter-ada

GPRINSTALL_FLAGS = --prefix=$(PREFIX) --sources-subdir=$(INSTALL_INCLUDE_DIR)\
 --lib-subdir=$(INSTALL_ALI_DIR) --project-subdir=$(INSTALL_PROJECT_DIR)\
 --link-lib-subdir=$(INSTALL_LIBRARY_DIR) --exec-subdir=$(INSTALL_EXEC_DIR)

all:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/jupyter.gpr
	gprbuild $(GPRBUILD_FLAGS) -P gnat/jupyter_hello_world.gpr
	gprbuild $(GPRBUILD_FLAGS) -P gnat/jupyter_ada_kernel.gpr
	gprbuild $(GPRBUILD_FLAGS) -P gnat/jupyter_ada_driver.gpr

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/jupyter.gpr -XHARDWARE_PLATFORM=x86_64
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/jupyter_ada_kernel.gpr --mode=usage -XHARDWARE_PLATFORM=x86_64
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/jupyter_ada_driver.gpr --mode=usage -XHARDWARE_PLATFORM=x86_64
	mkdir -p $(DESTDIR)$(PREFIX)/share/jupyter/kernels/ada
	sed -e '/.objs/s#.[a-z_/][a-z_/]*#$(BINDIR)/ada_kernel#' kernels/ada/kernel.json >\
	  $(DESTDIR)$(PREFIX)/share/jupyter/kernels/ada/kernel.json
	mkdir -p $(DESTDIR)$(PREFIX)/lib/python3.8/site-packages/notebook/static/components/codemirror/mode/ada/
	cp codemirror/mode/ada/ada.js $(DESTDIR)$(PREFIX)/lib/python3.8/site-packages/notebook/static/components/codemirror/mode/ada/

clean:
	gprclean -q -P gnat/jupyter.gpr
	gprclean -q -P gnat/jupyter_hello_world.gpr
	gprclean -q -P gnat/jupyter_ada_kernel.gpr
	gprclean -q -P gnat/jupyter_ada_driver.gpr

check:
	set -e -x; for J in tests/*.ipynb; do \
	  FILE=`basename $$J .ipynb`; \
	  cp -v $$J .; \
	  PATH=$(PATH):.objs/driver \
	  ADA_PROJECT_PATH=$(PWD)/gnat \
	  JUPYTER_PATH=. jupyter nbconvert --KernelManager.shutdown_wait_time=0.5 \
	    --allow-errors --to markdown --execute $$FILE.ipynb; \
	  diff -u tests/expected/$$FILE.md $$FILE.md; \
	done
