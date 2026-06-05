FC = gfortran
FFLAGS = -std=f2008 -Wall -Wextra -O2 -Jbuild
LDFLAGS = -lncurses
MODDIR = build

CORE_SRC = src/kinds.f90 \
           src/genome.f90 \
           src/phenotype.f90 \
           src/population.f90 \
           src/evolution.f90 \
           src/raster.f90 \
           src/terminal_color.f90 \
           src/renderer_iface.f90 \
           src/ncurses_bindings.f90 \
           src/ncurses_renderer.f90

TEST_SRC = test/matchers.f90 \
           test/test_support.f90 \
           test/fixtures.f90 \
           test/biomorph_tests.f90

CORE_OBJ = $(patsubst %.f90,$(MODDIR)/%.o,$(CORE_SRC))
TEST_OBJ = $(patsubst %.f90,$(MODDIR)/%.o,$(TEST_SRC))

.PHONY: all test run clean

all: biomorph biomorph_tests

$(MODDIR):
	mkdir -p $(MODDIR)/src $(MODDIR)/test

$(MODDIR)/src/%.o: src/%.f90 | $(MODDIR)
	$(FC) $(FFLAGS) -c $< -o $@

$(MODDIR)/test/%.o: test/%.f90 | $(MODDIR)
	$(FC) $(FFLAGS) -c $< -o $@

biomorph: $(CORE_OBJ) $(MODDIR)/src/biomorph.o
	$(FC) $(FFLAGS) -o biomorph $(CORE_OBJ) $(MODDIR)/src/biomorph.o $(LDFLAGS)

$(MODDIR)/src/biomorph.o: src/biomorph.f90 $(CORE_OBJ) | $(MODDIR)
	$(FC) $(FFLAGS) -c $< -o $@

biomorph_tests: $(CORE_OBJ) $(TEST_OBJ)
	$(FC) $(FFLAGS) -o biomorph_tests $(CORE_OBJ) $(TEST_OBJ) $(LDFLAGS)

test: biomorph_tests
	./biomorph_tests

run: biomorph
	./biomorph

clean:
	rm -rf biomorph biomorph_tests $(MODDIR)
