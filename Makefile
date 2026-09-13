FC = gfortran
FFLAGS ?= -O3 -march=native -flto=auto -fopenmp -frecursive -finline-limit=1000 --param inline-unit-growth=100
BUILD ?= build
EXPR_LAYOUT ?= soa
EXPR_SOURCE = src/expr.f90
ifeq ($(EXPR_LAYOUT),aos)
EXPR_SOURCE = $(BUILD)/expr_aos.f90
endif
COMMON = -std=f2018 -Wall -Wextra -Wimplicit-interface -ffree-line-length-none -I$(abspath include) -J$(abspath $(BUILD)) -I$(abspath $(BUILD))
MODULES = src/base.f90 src/hash_table.f90 src/index_map.f90 src/bignums.f90 src/names.f90 src/sequences.f90 src/levels.f90 src/strings.f90 src/json.f90 $(EXPR_SOURCE) src/read_sets.f90 src/closure.f90 src/spines.f90 src/nbe.f90 src/declarations.f90 src/parser.f90 src/inductive_state.f90 src/kernel.f90
OBJECTS = $(addprefix $(BUILD)/,$(notdir $(MODULES:.f90=.o)))
.PHONY: all test debug clean
all: $(BUILD)/nanoclo-fortran
$(BUILD):
	mkdir -p $@
# One compiler invocation orders module dependencies and avoids racing .mod
# writers under make -j. Debug and release objects live in separate directories.
$(OBJECTS) &: $(MODULES) $(wildcard include/*.inc) | $(BUILD)
	cd $(BUILD) && $(FC) $(COMMON) $(FFLAGS) -c $(abspath $(MODULES))
$(BUILD)/expr_aos.f90: src/expr.f90 scripts/make_expr_aos.py | $(BUILD)
	python3 scripts/make_expr_aos.py $< $@
$(BUILD)/test_core: $(OBJECTS) tests/test_core.f90
	$(FC) $(COMMON) $(FFLAGS) $(OBJECTS) tests/test_core.f90 -o $@
$(BUILD)/bignum_driver: $(OBJECTS) tests/bignum_driver.f90
	$(FC) $(COMMON) $(FFLAGS) $(OBJECTS) tests/bignum_driver.f90 -o $@
$(BUILD)/dump_declarations: $(OBJECTS) tests/dump_declarations.f90
	$(FC) $(COMMON) $(FFLAGS) $(OBJECTS) tests/dump_declarations.f90 -o $@
$(BUILD)/test_inductive: $(OBJECTS) tests/test_inductive.f90
	$(FC) $(COMMON) $(FFLAGS) $(OBJECTS) tests/test_inductive.f90 -o $@
$(BUILD)/test_eval: $(OBJECTS) tests/test_eval.f90
	$(FC) $(COMMON) $(FFLAGS) $(OBJECTS) tests/test_eval.f90 -o $@
$(BUILD)/test_eq_mod: $(OBJECTS) tests/test_eq_mod.f90
	$(FC) $(COMMON) $(FFLAGS) $(OBJECTS) tests/test_eq_mod.f90 -o $@
$(BUILD)/test_nbe: $(OBJECTS) tests/test_nbe.f90
	$(FC) $(COMMON) $(FFLAGS) $(OBJECTS) tests/test_nbe.f90 -o $@
$(BUILD)/test_read_sets: $(OBJECTS) tests/test_read_sets.f90
	$(FC) $(COMMON) $(FFLAGS) $(OBJECTS) tests/test_read_sets.f90 -o $@
$(BUILD)/test_closure: $(OBJECTS) tests/test_closure.f90
	$(FC) $(COMMON) $(FFLAGS) $(OBJECTS) tests/test_closure.f90 -o $@
$(BUILD)/test_expr: $(OBJECTS) tests/test_expr.f90
	$(FC) $(COMMON) $(FFLAGS) $(OBJECTS) tests/test_expr.f90 -o $@
$(BUILD)/test_parser: $(OBJECTS) tests/test_parser.f90
	$(FC) $(COMMON) $(FFLAGS) $(OBJECTS) tests/test_parser.f90 -o $@
$(BUILD)/nanoclo-fortran: $(OBJECTS) src/main.f90
	$(FC) $(COMMON) $(FFLAGS) $(OBJECTS) src/main.f90 -o $@
test: $(BUILD)/bignum_driver $(BUILD)/dump_declarations $(BUILD)/test_inductive $(BUILD)/test_eval $(BUILD)/test_eq_mod $(BUILD)/test_nbe $(BUILD)/test_read_sets $(BUILD)/test_core $(BUILD)/test_closure $(BUILD)/test_expr $(BUILD)/test_parser $(BUILD)/nanoclo-fortran
	$(BUILD)/test_core
	python3 tests/test_bignums.py $(BUILD)/bignum_driver
	$(BUILD)/test_expr
	$(BUILD)/test_closure
	$(BUILD)/test_read_sets
	$(BUILD)/test_nbe
	$(BUILD)/test_eq_mod
	$(BUILD)/test_eval
	$(BUILD)/test_inductive
	$(BUILD)/test_parser
	python3 tests/test_cli.py $(BUILD)/nanoclo-fortran $(EXPORTS)
	python3 tests/test_checker.py $(BUILD)/nanoclo-fortran $(EXPORTS)
	python3 tests/test_decl_parser.py $(BUILD)/dump_declarations $(EXPORTS)
debug:
	$(MAKE) BUILD=build/debug FFLAGS='-O0 -g -fopenmp -frecursive -fcheck=all -fbacktrace -ftrapv' test
clean:
	rm -rf build
