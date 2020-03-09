project_file             = cargame.gpr
sources                  = $(project_file) $(wildcard src/*.ads) $(wildcard src/*.adb)

# NOTE: Don't touch the .dll and .a files in the obj folder. These are
# necessary for linking to GLFW and the like.
objects                  = $(wildcard obj/*.ali) $(wildcard obj/*.o)

gpr_options              = -XAuto_Exceptions=enabled

gprbuild_options         = -gnatW8 -j15 -gnatwa # -gnatm5
gprbuild_debug_options   = -O0 -g # -gnata
gprbuild_release_options = -O2 -s
# gprbuild_lint_options    = -O0 -gnatc

uname=$(uname)

GNATPROVE = /c/GNAT/2018/bin/gnatprove.exe

ifeq ($(OS),Windows_NT)
	gpr_options += -XWindowing_System=windows
else
	gpr_options += -XWindowing_System=x11
endif

all: $(sources)
	mkdir -p bin obj
	gprbuild  -P $(project_file) $(gpr_options) $(gprbuild_options) $(gprbuild_debug_options)

release: $(sources)
	gprbuild  -P $(project_file) $(gpr_options) $(gprbuild_options) $(gprbuild_release_options)

clean: $(objects)
	gprclean  -P $(project_file) $(gpr_options)

prove: $(sources)
	$(GNATPROVE) -P $(project_file) $(gpr_options)

# lint: $(sources)
# 	gprbuild -P $(project_file) $(gpr_options) $(gprbuild_options) $(gprbuild_lint_options)
