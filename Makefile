# ============================================================
#  GNU Make wrapper for fortran-introsort
#
#  Quick start:
#    make config [FC=gfortran|ifx] [PREFIX=~/.local] [BUILD_TYPE=Release|Debug]
#    make
#    make install
# ============================================================

-include .buildconfig

FC         ?= gfortran
PREFIX     ?= $(HOME)/.local
BUILD_TYPE ?= Release
FYPP       ?= fypp

FC_NAME   := $(notdir $(FC))
BUILD_DIR ?= build/$(FC_NAME)-$(shell echo '$(BUILD_TYPE)' | tr '[:upper:]' '[:lower:]')

FYPP_MAIN := src/intro-sort.fypp
F90_MAIN  := src/intro-sort.F90
FYPP_DEPS := $(wildcard src/*.fypp)

.PHONY: all config build install clean distclean help

all: build

# fypp preprocessing: re-runs when any .fypp file is newer than the .F90
$(F90_MAIN): $(FYPP_DEPS)
	$(FYPP) $(FYPP_MAIN) $@

config: $(F90_MAIN)
	@printf 'FC=%s\nPREFIX=%s\nBUILD_TYPE=%s\nBUILD_DIR=%s\n' \
		'$(FC)' '$(PREFIX)' '$(BUILD_TYPE)' '$(BUILD_DIR)' >| .buildconfig
	cmake -S . -B $(BUILD_DIR) \
		-DCMAKE_Fortran_COMPILER=$(FC) \
		-DCMAKE_BUILD_TYPE=$(BUILD_TYPE) \
		-DCMAKE_INSTALL_PREFIX=$(PREFIX)

build: $(F90_MAIN)
	cmake --build $(BUILD_DIR) --parallel

install: build
	cmake --install $(BUILD_DIR) --prefix $(PREFIX)

clean:
	cmake --build $(BUILD_DIR) --target clean 2>/dev/null || true

distclean:
	rm -rf build .buildconfig
	rm -f $(F90_MAIN)

help:
	@printf '\nUsage:\n'
	@printf '  make config [FC=gfortran|ifx] [PREFIX=~/.local] [BUILD_TYPE=Release|Debug]\n'
	@printf '  make [build]\n'
	@printf '  make install [PREFIX=<dir>]      # PREFIX overrides configured value\n'
	@printf '  make clean\n'
	@printf '  make distclean                   # removes build/ and generated .F90\n\n'
