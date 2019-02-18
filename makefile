.SUFFIXES:
.PHONY: clean install

CSC := csc
CHICKEN_INSTALL := chicken-install

protocols = xdg-shell-protocol
examples  = tinywl pointer
modules   = slib.getopt \
            wlr.backend \
            wlr.render.wlr-renderer \
            wlr.time \
            wlr.types.wlr-box \
            wlr.types.wlr-compositor \
            wlr.types.wlr-cursor \
            wlr.types.wlr-data-device \
            wlr.types.wlr-input-device \
            wlr.types.wlr-keyboard \
            wlr.types.wlr-matrix \
            wlr.types.wlr-pointer \
            wlr.types.wlr-output \
	    wlr.types.wlr-output-damage \
            wlr.types.wlr-output-layout \
            wlr.types.wlr-seat \
            wlr.types.wlr-surface \
            wlr.types.wlr-tablet-tool \
            wlr.types.wlr-xcursor-manager \
            wlr.types.wlr-xdg-shell \
            wlr.util.edges \
            wlr.util.log

clean = $(addsuffix .o,$(modules) $(protocols) $(addprefix examples.,$(examples))) $(examples) \
        $(addsuffix .import.scm,$(modules)) \
        $(addsuffix .import.so,$(modules)) \
        $(addsuffix .static.o,$(modules)) \
        $(addsuffix .link,$(modules)) \
        $(addsuffix .so,$(modules)) \
        $(addprefix include/,$(addsuffix .h,$(protocols))) \
        $(addprefix src/,$(addsuffix .c,$(protocols))) \
	wlroots.build.sh wlroots.install.sh

all: $(examples)

slib.getopt.o: src/slib/getopt.scm
	$(CSC) -c -unit slib/getopt -J -o $@ $<

examples.tinywl.o: src/examples/tinywl.scm slib.getopt.o
	$(CSC) -c -uses slib/getopt -o $@ $<

examples.pointer.o: src/examples/pointer.scm
	$(CSC) -c -o $@ $<

tinywl: examples.tinywl.o slib.getopt.o
	$(CSC) -o $@ $^

pointer: examples.pointer.o
	$(CSC) -o $@ $^

install:
	$(CHICKEN_INSTALL)

clean:
	rm -f $(clean)
