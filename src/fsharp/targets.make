SOURCES := $(patsubst $(srcdir)$(tmpdir)%,$(tmpdir)%,$(patsubst %,$(srcdir)%,$(sources)))

.PHONY: install install-bin install-bin-2 install-bin-4 install-lib

clean-2-0: TARGET := $(TARGET_2_0)
clean-2-0:
	@-rm -rf $(tmpdir)
	@-rm -rf $(objdir)
	@-rm -f $(outdir)$(ASSEMBLY)
	@-rm -f $(outdir)$(ASSEMBLY).mdb
	@-rm -f $(outdir)$(NAME).xml
	@-rm -f $(outdir)$(NAME).sigdata
	@-rm -f $(outdir)$(NAME).optdata

clean-2-1: TARGET := $(TARGET_2_1)
clean-2-1:
	@-rm -rf $(tmpdir)
	@-rm -rf $(objdir)
	@-rm -f $(outdir)$(ASSEMBLY)
	@-rm -f $(outdir)$(ASSEMBLY).mdb
	@-rm -f $(outdir)$(NAME).xml
	@-rm -f $(outdir)$(NAME).sigdata
	@-rm -f $(outdir)$(NAME).optdata

clean-4-0: TARGET := $(TARGET_4_0)
clean-4-0:
	@-rm -rf $(tmpdir)
	@-rm -rf $(objdir)
	@-rm -f $(outdir)$(ASSEMBLY)
	@-rm -f $(outdir)$(ASSEMBLY).mdb
	@-rm -f $(outdir)$(NAME).xml
	@-rm -f $(outdir)$(NAME).sigdata
	@-rm -f $(outdir)$(NAME).optdata

do-2-0: DEFINES += $(DEFINES_2_0)
do-2-0: REFERENCES += $(REFERENCES_2_0)
do-2-0: FLAGS += $(FLAGS_2_0)
do-2-0: TARGET := $(TARGET_2_0)
do-2-0: VERSION := $(VERSION_2_0)
do-2-0: monolibdir = $(monolibdir2)
do-2-0: $(objdir) $(objdir)$(TARGET_2_0) $(objdir)$(TARGET_4_0) $(objdir)$(TARGET_2_0)/$(ASSEMBLY)
	@mkdir -p $(outdir)
	@cp $(objdir)$(ASSEMBLY) $(outdir)
	@if test -e $(objdir)$(ASSEMBLY).xml; then \
	    cp $(objdir)$(NAME).xml $(outdir); \
	fi
	@if test -e $(objdir)$(ASSEMBLY).mdb; then \
	    cp $(objdir)$(ASSEMBLY).mdb $(outdir); \
	fi
	@if test -e $(objdir)$(NAME).sigdata; then \
		cp $(objdir)$(NAME).sigdata $(outdir); \
	fi
	@if test -e $(objdir)$(NAME).optdata; then \
		cp $(objdir)$(NAME).optdata $(outdir); \
	fi
	@if test "x$(SIGN)" = "x1"; then \
		sn -q -R $(outdir)$(ASSEMBLY) $(srcdir)../../../mono.snk; \
	fi
	@if test -e Microsoft.FSharp.targets; then \
		cp Microsoft.FSharp.targets $(outdir)Microsoft.FSharp.targets; \
	fi

do-2-1: DEFINES += $(DEFINES_2_1)
do-2-1: REFERENCES += $(REFERENCES_2_1)
do-2-1: FLAGS += $(FLAGS_2_1)
do-2-1: TARGET := $(TARGET_2_1)
do-2-1: VERSION := $(VERSION_2_1)
do-2-1: monolibdir = $(monolibdir2)
do-2-1: $(objdir) $(objdir)$(TARGET_2_1) $(objdir)$(TARGET_4_0) $(objdir)$(TARGET_2_1)/$(ASSEMBLY)
	@mkdir -p $(outdir)
	@cp $(objdir)$(ASSEMBLY) $(outdir)
	@if test -e $(objdir)$(ASSEMBLY).xml; then \
	    cp $(objdir)$(NAME).xml $(outdir); \
	fi
	@if test -e $(objdir)$(ASSEMBLY).mdb; then \
	    cp $(objdir)$(ASSEMBLY).mdb $(outdir); \
	fi
	@if test -e $(objdir)$(NAME).sigdata; then \
		cp $(objdir)$(NAME).sigdata $(outdir); \
	fi
	@if test -e $(objdir)$(NAME).optdata; then \
		cp $(objdir)$(NAME).optdata $(outdir); \
	fi
	@if test "x$(SIGN)" = "x1"; then \
		sn -q -R $(outdir)$(ASSEMBLY) $(srcdir)../../../mono.snk; \
	fi
	@if test -e Microsoft.FSharp.targets; then \
		cp Microsoft.FSharp.targets $(outdir)Microsoft.FSharp.targets; \
	fi

do-4-0: DEFINES += $(DEFINES_4_0)
do-4-0: REFERENCES += $(REFERENCES_4_0)
do-4-0: FLAGS += $(FLAGS_4_0)
do-4-0: TARGET := $(TARGET_4_0)
do-4-0: VERSION := $(VERSION_4_0)
do-4-0: monolibdir = $(monolibdir4)
do-4-0: $(objdir) $(objdir)$(TARGET_2_0) $(objdir)$(TARGET_4_0) $(objdir)$(TARGET_4_0)/$(ASSEMBLY)
	@mkdir -p $(outdir)
	@cp $(objdir)$(ASSEMBLY) $(outdir)
	@if test -e $(objdir)$(ASSEMBLY).xml; then \
	    cp $(objdir)$(NAME).xml $(outdir); \
	fi
	@if test -e $(objdir)$(ASSEMBLY).mdb; then \
	    cp $(objdir)$(ASSEMBLY).mdb $(outdir); \
	fi
	@if test -e $(objdir)$(NAME).sigdata; then \
		cp $(objdir)$(NAME).sigdata $(outdir); \
	fi
	@if test -e $(objdir)$(NAME).optdata; then \
		cp $(objdir)$(NAME).optdata $(outdir); \
	fi
	@if test "x$(SIGN)" = "x1"; then \
		sn -q -R $(outdir)$(ASSEMBLY) $(srcdir)../../../mono.snk; \
	fi
	@if test -e Microsoft.FSharp.targets; then \
		cp Microsoft.FSharp.targets $(outdir)Microsoft.FSharp.targets; \
	fi

install-lib-2: TARGET := $(TARGET_2_0)
install-lib-2: VERSION := $(VERSION_2_0)

install-lib-2-1: TARGET := $(TARGET_2_1)
install-lib-2-1: VERSION := $(VERSION_2_1)

install-lib-4: TARGET := $(TARGET_4_0)
install-lib-4: VERSION := $(VERSION_4_0)

install-bin-2: TARGET := $(TARGET_2_0)
install-bin-2: VERSION := 2

install-bin-2-1: TARGET := $(TARGET_2_1)
install-bin-2-1: VERSION := 2.1

install-bin-4: TARGET := $(TARGET_4_0)


# Install the library binaries in the GAC and the framework directory, 
# Install .optdata/.sigdata if they exist (they go alongside FSharp.Core)
# Install the .targets file. The XBuild targets file gets installed into the place(s) expected for standard F# project
# files. For F# 2.0 project files this is
#     .../Microsoft F#/v4.0/Microsoft.FSharp.targets
# For F# 3.0 project files this is
#     .../Microsoft SDKs/F#/3.0/Framework/v4.0/Microsoft.FSharp.targets
# 
install-lib-2 install-lib-2-1 install-lib-4:
	@echo "Installing $(ASSEMBLY)"
	@mkdir -p $(DESTDIR)/$(libdir)
	@mkdir -p $(DESTDIR)/$(libdir)mono/$(TARGET)
	gacutil -i $(outdir)$(ASSEMBLY) -root $(DESTDIR)/$(libdir) -package $(TARGET)
	@if test -e $(outdir)Microsoft.FSharp.targets; then \
	    mkdir -p $(DESTDIR)/$(libdir)mono/Microsoft\ F#/v$(TARGET)/; \
	    mkdir -p $(DESTDIR)/$(libdir)mono/Microsoft\ SDKs/F#/3.0/Framework/v$(TARGET)/; \
	    ln -fs $(DESTDIR)/$(libdir)mono/$(TARGET)/$(ASSEMBLY) $(DESTDIR)/$(libdir)mono/Microsoft\ F#/v$(TARGET)/$(ASSEMBLY); \
	    ln -fs $(DESTDIR)/$(libdir)mono/$(TARGET)/$(ASSEMBLY) $(DESTDIR)/$(libdir)mono/Microsoft\ SDKs/F#/3.0/Framework/v$(TARGET)/$(ASSEMBLY); \
	    $(INSTALL_LIB) $(outdir)Microsoft.FSharp.targets $(DESTDIR)/$(libdir)mono/$(TARGET)/; \
	    ln -fs $(DESTDIR)/$(libdir)mono/$(TARGET)/Microsoft.FSharp.targets $(DESTDIR)/$(libdir)mono/Microsoft\ F#/v$(TARGET)/Microsoft.FSharp.targets; \
	    ln -fs $(DESTDIR)/$(libdir)mono/$(TARGET)/Microsoft.FSharp.targets $(DESTDIR)/$(libdir)mono/Microsoft\ SDKs/F#/3.0/Framework/v$(TARGET)/Microsoft.FSharp.targets; \
	fi
	@if test -e $(outdir)$(NAME).sigdata; then \
		$(INSTALL_LIB) $(outdir)$(NAME).sigdata $(DESTDIR)/$(libdir)mono/gac/$(NAME)/$(VERSION)__$(TOKEN); \
		ln -fs  ../gac/$(NAME)/$(VERSION)__$(TOKEN)/$(NAME).sigdata $(DESTDIR)/$(libdir)mono/$(TARGET)/$(NAME).sigdata; \
	fi
	@if test -e $(outdir)$(NAME).optdata; then \
		$(INSTALL_LIB) $(outdir)$(NAME).optdata $(DESTDIR)/$(libdir)mono/gac/$(NAME)/$(VERSION)__$(TOKEN); \
		ln -fs ../gac/$(NAME)/$(VERSION)__$(TOKEN)/$(NAME).optdata $(DESTDIR)/$(libdir)mono/$(TARGET)/$(NAME).optdata; \
	fi

install-lib-4-5: install-lib-4
	@if test -e $(DESTDIR)$(libdir)mono/4.5/; then \
		ln -fs $(DESTDIR)$(libdir)mono/4.0/$(ASSEMBLY) $(DESTDIR)$(libdir)mono/4.5/$(ASSEMBLY); \
		if test -e $(DESTDIR)$(libdir)mono/4.0/$(ASSEMBLY).config; then \
		    ln -fs $(DESTDIR)$(libdir)mono/4.0/$(ASSEMBLY).config $(DESTDIR)$(libdir)mono/4.5/$(ASSEMBLY).config; \
		fi; \
		if test -e $(DESTDIR)$(libdir)mono/4.0/$(NAME).sigdata; then \
		    ln -fs $(DESTDIR)$(libdir)mono/4.0/$(NAME).sigdata $(DESTDIR)$(libdir)mono/4.5/$(NAME).sigdata; \
		fi; \
		if test -e $(DESTDIR)$(libdir)mono/4.0/$(NAME).optdata; then \
		    ln -fs $(DESTDIR)$(libdir)mono/4.0/$(NAME).optdata $(DESTDIR)$(libdir)mono/4.5/$(NAME).optdata; \
		fi; \
	fi

# The binaries fsc.exe and fsi.exe only get installed for Mono 4.0 profile
# This also installs 'fsharpc' and 'fsharpi'
install-bin-4:
	sed -e 's,[@]DIR[@],$(libdir)mono/$(TARGET),g' -e 's,[@]TOOL[@],$(ASSEMBLY),g' < $(topdir)launcher > $(outdir)$(subst fs,fsharp,$(NAME))$(VERSION)
	chmod +x $(outdir)$(subst fs,fsharp,$(NAME))$(VERSION)
	@mkdir -p $(DESTDIR)/$(libdir)
	@mkdir -p $(DESTDIR)/$(bindir)
	$(INSTALL_LIB) $(outdir)$(ASSEMBLY) $(DESTDIR)$(libdir)mono/$(TARGET)
	$(INSTALL_BIN) $(outdir)$(subst fs,fsharp,$(NAME))$(VERSION) $(DESTDIR)/$(bindir)


$(objdir) $(objdir)$(TARGET_2_0) $(objdir)$(TARGET_2_1) $(objdir)$(TARGET_4_0):
	mkdir -p $@

$(objdir)$(TARGET_2_0)/$(ASSEMBLY): $(RESOURCES) $(SOURCES)
	mono $(MONO_OPTIONS) $(FSC) -o:$(objdir)$(ASSEMBLY) $(REFERENCES) $(DEFINES) $(FLAGS) $(patsubst %,--resource:%,$(RESOURCES)) $(SOURCES)

$(objdir)$(TARGET_2_1)/$(ASSEMBLY): $(RESOURCES) $(SOURCES)
	mono $(MONO_OPTIONS) $(FSC) -o:$(objdir)$(ASSEMBLY) $(REFERENCES) $(DEFINES) $(FLAGS) $(patsubst %,--resource:%,$(RESOURCES)) $(SOURCES)

$(objdir)$(TARGET_4_0)/$(ASSEMBLY):  $(RESOURCES) $(SOURCES)
	mono $(MONO_OPTIONS) $(FSC) -o:$(objdir)$(ASSEMBLY) $(REFERENCES) $(DEFINES) $(FLAGS) $(patsubst %,--resource:%,$(RESOURCES)) $(SOURCES)
