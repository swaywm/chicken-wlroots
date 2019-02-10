.SUFFIXES:
.PHONY: clean distclean eggs

SHELL = /bin/sh

prefix = /usr/local
exec_prefix = $(prefix)
datarootdir = $(prefix)/share
bindir = $(exec_prefix)/bin
datadir = $(datarootdir)

# {{{ programs and arguments
ALL_CFLAGS := $(CFLAGS) \
              $(shell pkg-config --cflags wlroots) \
              -DWLR_USE_UNSTABLE \
              -fPIC

ALL_LDFLAGS := $(LDFLAGS) \
               $(shell pkg-config --libs wlroots)

ARFLAGS      := rcs

CHKN_SUFFIX  :=
CSC          := csc$(CHKN_SUFFIX)
CHKN_INSTALL := chicken-install$(CHKN_SUFFIX)
CSCFLAGS     :=
ALL_CSCFLAGS := $(CSCFLAGS) \
                $(addprefix -C , $(ALL_CFLAGS)) \
	        -I ./include \
	        -I./include \
		-R bind \
		-X bind

WAYLAND_PROTOCOLS := $(shell pkg-config --variable=pkgdatadir wayland-protocols)
WAYLAND_SCANNER   := $(shell pkg-config --variable=wayland_scanner wayland-scanner)
# }}}

# function to add object extension to a list of files
objfiles = $(foreach f,$(1),$(addprefix ./,$(addsuffix .o,$(subst /,.,$(f)))))
srcfiles = $(foreach f,$(1),$(addprefix src/,$(addsuffix .scm,$(f))))

eggs = foreigners
modules = ffi-helpers \
          slib/getopt \
          wlr/backend \
          wlr/render/wlr-renderer \
          wlr/time \
          wlr/types/wlr-box \
          wlr/types/wlr-compositor \
          wlr/types/wlr-cursor \
          wlr/types/wlr-data-device \
          wlr/types/wlr-input-device \
          wlr/types/wlr-keyboard \
          wlr/types/wlr-matrix \
          wlr/types/wlr-pointer \
          wlr/types/wlr-output \
          wlr/types/wlr-output-layout \
          wlr/types/wlr-seat \
          wlr/types/wlr-surface \
          wlr/types/wlr-xcursor-manager \
          wlr/types/wlr-xdg-shell \
          wlr/util/edges
protocols = xdg-shell-protocol
examples = tinywl

clean = chicken-wlroots.a wlroots.so \
        $(call objfiles,$(modules) $(addprefix examples/,$(examples))) $(examples) \
        $(foreach f,$(modules),$(addprefix ./,$(addsuffix .import.scm,$(subst /,.,$(f))))) \
	$(foreach f,$(modules),$(addprefix ./,$(addsuffix .import.so,$(subst /,.,$(f))))) \
        $(foreach f,$(protocols),$(addprefix ./,$(addsuffix .o,$(f)))) \
        $(foreach f,$(protocols),$(addprefix src/,$(addsuffix .c,$(f)))) \
        $(foreach f,$(protocols),$(addprefix include/,$(addsuffix .h,$(f)))) \
	wlroots.build.sh wlroots.install.sh \
	wlroots.import.scm wlroots.import.so

all: $(examples)

# protocol XML files
xdg-shell-protocol_xml = $(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml

# Example programs depend on all modules
$(foreach f,$(examples),$(eval examples/$(f)_deps = $(modules)))

# All modules depend on ffi-helpers
$(foreach f,$(filter-out ffi-helpers,$(modules)),$(eval $(f)_deps = ffi-helpers))

# Module dependencies.  Every IMPORT of an internal module should
# be represented as a dependency here.
wlr/types/wlr-xdg-shell_deps += wlr/types/wlr-box

# Simple dependencies (e.g. on protocol headers)
wlr/types/wlr-xdg-shell_sdeps = include/xdg-shell-protocol.h

# Every module is a separate compilation unit
$(foreach f,$(modules),$(eval $(call objfiles,$(f)): private ALL_CSCFLAGS += -unit $(f)))

# Template to create a rule building %.o from src/%.scm.  Because we are replacing '/' with '.' in
# output filenames, we can't use a pattern rule here.
define OBJECT_template =
$(call objfiles,$(1)): private ALL_CSCFLAGS += $(foreach d,$($(1)_deps) $($(1)_uses), -uses $(d))
$(call objfiles,$(1)): src/$(1).scm $(call objfiles,$($(1)_deps)) $($(1)_sdeps)
	$$(call cmd,csc)
endef

$(foreach f,$(modules) $(addprefix examples/,$(examples)),$(eval $(call OBJECT_template,$(f))))

# Template to create .c and .h files for Wayland protocols.
define PROTOCOL_template =
include/$(1).h: $($(1)_xml)
	$$(call cmd,wl_scan,server-header)
src/$(1).c: $($(1)_xml) include/$(1).h
	$$(call cmd,wl_scan,private-code)
./$(1).o: src/$(1).c
	$$(call cmd,cc)
endef

$(foreach f,$(protocols),$(eval $(call PROTOCOL_template,$(f))))

# Template to create rules for example programs.
define PROGRAM_template =
$(1): chicken-wlroots.a $(call objfiles,examples/$(1))
	$$(call cmd,cscld)
endef

$(foreach f,$(examples),$(eval $(call PROGRAM_template,$(f))))

ifeq ($(verbose),y)
  quiet =
else
  quiet = quiet_
endif

clean:
	$(if $(clean), rm -f $(clean))

distclean: clean
	$(if $(distclean), rm -f $(distclean))

eggs:
	$(CHKN_INSTALL) -sudo $(eggs)

wlroots.so: $(call objfiles,$(modules) $(protocols)) src/wlroots.scm
	$(call cmd,csc_so,-J $(foreach m,$(modules), -uses $(m)))

chicken-wlroots.a: $(call objfiles,$(modules) $(protocols))
	$(call cmd,ar)

# Generate files with wayland-scanner
quiet_cmd_wl_scan = WL-SCAN $@
      cmd_wl_scan = $(WAYLAND_SCANNER) $(1) $< $@

# CC for program object files (.o)
quiet_cmd_cc      = CC      $@
      cmd_cc      = $(CC) -c $(CPPFLAGS) $(ALL_CFLAGS) -o $@ $<

# scheme compile for object files
quiet_cmd_csc     = CSC     $@
      cmd_csc     = $(CSC) -c $(ALL_CSCFLAGS) -J -o $@ $<

quiet_cmd_ar      = AR      $@
      cmd_ar      = $(AR) $(ARFLAGS) $@ $^

# LD for programs; optional parameter: additional arguments
quiet_cmd_cscld   = LD      $@
      cmd_cscld   = $(CSC) $(addprefix -L , $(ALL_LDFLAGS)) -o $@ $^ $(1)

# LD for shared libraries; optional parameter: additional arguments
quiet_cmd_csc_so  = LD      $@
      cmd_csc_so  = $(CSC) -s $(addprefix -L ,$(ALL_LDFLAGS)) -o $@ $^ $(1)

cmd = @$(if $($(quiet)cmd_$(1)),echo '  $(call $(quiet)cmd_$(1),$(2))' &&) $(call cmd_$(1),$(2))
