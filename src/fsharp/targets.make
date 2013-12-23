SOURCES := $(patsubst $(srcdir)$(tmpdir)%,$(tmpdir)%,$(patsubst %,$(srcdir)%,$(sources)))

.PHONY: install install-lib-net20 install-lib-monodroid install-lib-net40

build:
	MONO_ENV_OPTIONS=$(monoopts) xbuild /p:Configuration=$(Configuration) /p:TargetFramework=$(TargetFramework) /p:MonoLibDir40=$(monogacdir40)

clean:
	xbuild /p:Configuration=$(Configuration) /p:TargetFramework=$(TargetFramework) /t:Clean

# Install the library binaries in the GAC and the framework directory, 
# Install .optdata/.sigdata if they exist (they go alongside FSharp.Core)
# Install the .Targets file. The XBuild targets file gets installed into the place(s) expected for standard F# project
# files. For F# 2.0 project files this is
#     /usr/lib/mono/Microsoft F#/v4.0/Microsoft.FSharp.Targets
# For F# 3.0 project files this is
#     /usr/lib/mono/Microsoft SDKs/F#/3.0/Framework/v4.0/Microsoft.FSharp.Targets
# For F# 3.1 project files this is
#     /usr/lib/mono/xbuild/Microsoft/VisualStudio/v12.0/FSharp/Microsoft.FSharp.Targets
# 
# Here 12.0 is 'VisualStudioVersion'. xbuild should set this to 12.0, copying MSBuild.
#
# We put the F# 3.1 targets and link the SDK DLLs for all three locations
#
# We put a forwarding targets file into all three locations. We also put one in 
#     .../lib/mono/xbuild/Microsoft/VisualStudio/v12.0/FSharp/Microsoft.FSharp.Targets
# since this is the correct location, and 'xbuild' may in future start setting VisualStudioVersion to this value.

install-lib:
	@echo "Installing $(ASSEMBLY)"
	@mkdir -p $(DESTDIR)$(gacdir)/$(TARGET)
	@if test "x$(DELAY_SIGN)" = "x1"; then \
		sn -q -R $(outdir)$(ASSEMBLY) $(srcdir)../../../mono.snk; \
	fi
	gacutil -i $(outdir)$(ASSEMBLY) -root $(DESTDIR)$(libdir) -package $(TARGET)
	@if test -e $(outdir)Microsoft.FSharp.Targets; then \
	    $(INSTALL_LIB) $(outdir)Microsoft.FSharp.Targets $(DESTDIR)$(gacdir)/$(TARGET)/; \
	    mkdir -p $(tmpdir); \
	    echo '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">' > $(tmpdir)Microsoft.FSharp.Targets; \
	    echo '    <Import Project="$(gacdir)/$(TARGET)/Microsoft.FSharp.Targets" />' >> $(tmpdir)Microsoft.FSharp.Targets; \
	    echo '</Project>' >> $(tmpdir)Microsoft.FSharp.Targets; \
	    mkdir -p $(DESTDIR)$(gacdir)/Microsoft\ F#/v$(TARGET)/; \
	    mkdir -p $(DESTDIR)$(gacdir)/Microsoft\ SDKs/F#/3.0/Framework/v$(TARGET)/; \
	    mkdir -p $(DESTDIR)$(gacdir)/Microsoft\ SDKs/F#/3.1/Framework/v$(TARGET)/; \
	    mkdir -p $(DESTDIR)$(gacdir)/xbuild/Microsoft/VisualStudio/v/FSharp/; \
	    mkdir -p $(DESTDIR)$(gacdir)/xbuild/Microsoft/VisualStudio/v12.0/FSharp/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(gacdir)/Microsoft\ F#/v$(TARGET)/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(gacdir)/Microsoft\ SDKs/F#/3.0/Framework/v$(TARGET)/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(gacdir)/Microsoft\ SDKs/F#/3.1/Framework/v$(TARGET)/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(gacdir)/xbuild/Microsoft/VisualStudio/v/FSharp/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(gacdir)/xbuild/Microsoft/VisualStudio/v12.0/FSharp/; \
	fi
	@if test -e $(outdir)Microsoft.Portable.FSharp.Targets; then \
	    $(INSTALL_LIB) $(outdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(gacdir)/$(TARGET)/; \
	    echo '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">' > $(tmpdir)Microsoft.Portable.FSharp.Targets; \
	    echo '    <Import Project="$(gacdir)/$(TARGET)/Microsoft.Portable.FSharp.Targets" />' >> $(tmpdir)Microsoft.Portable.FSharp.Targets; \
		echo '</Project>' >> $(tmpdir)Microsoft.Portable.FSharp.Targets; \
	    mkdir -p $(DESTDIR)$(gacdir)/Microsoft\ F#/v$(TARGET)/; \
	    mkdir -p $(DESTDIR)$(gacdir)/Microsoft\ SDKs/F#/3.0/Framework/v$(TARGET)/; \
	    mkdir -p $(DESTDIR)$(gacdir)/Microsoft\ SDKs/F#/3.1/Framework/v$(TARGET)/; \
	    mkdir -p $(DESTDIR)$(gacdir)/xbuild/Microsoft/VisualStudio/v/FSharp/; \
	    mkdir -p $(DESTDIR)$(gacdir)/xbuild/Microsoft/VisualStudio/v12.0/FSharp/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(gacdir)/Microsoft\ F#/v$(TARGET)/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(gacdir)/Microsoft\ SDKs/F#/3.0/Framework/v$(TARGET)/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(gacdir)/Microsoft\ SDKs/F#/3.1/Framework/v$(TARGET)/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(gacdir)/xbuild/Microsoft/VisualStudio/v/FSharp/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(gacdir)/xbuild/Microsoft/VisualStudio/v12.0/FSharp/; \
	fi
	@if test -e $(outdir)$(NAME).xml; then \
		$(INSTALL_LIB) $(outdir)$(NAME).xml $(DESTDIR)$(gacdir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/; \
		ln -fs  ../gac/$(NAME)/$(VERSION)__$(TOKEN)/$(NAME).xml $(DESTDIR)$(gacdir)/$(TARGET)/$(NAME).xml; \
	fi
	@if test -e $(outdir)$(NAME).sigdata; then \
		$(INSTALL_LIB) $(outdir)$(NAME).sigdata $(DESTDIR)$(gacdir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/; \
		ln -fs  ../gac/$(NAME)/$(VERSION)__$(TOKEN)/$(NAME).sigdata $(DESTDIR)$(gacdir)/$(TARGET)/$(NAME).sigdata; \
	fi
	@if test -e $(outdir)$(NAME).optdata; then \
		$(INSTALL_LIB) $(outdir)$(NAME).optdata $(DESTDIR)$(gacdir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/; \
		ln -fs ../gac/$(NAME)/$(VERSION)__$(TOKEN)/$(NAME).optdata $(DESTDIR)$(gacdir)/$(TARGET)/$(NAME).optdata; \
	fi

# Also place some .NET 4.0 libraries into .NET 4.5
install-lib-net45: 
	@if test '$(TargetFramework)' = 'net40'; then \
	  if test -e $(DESTDIR)$(gacdir)/4.5/; then \
		ln -fs ../4.0/$(ASSEMBLY) $(DESTDIR)$(gacdir)/4.5/$(ASSEMBLY); \
		if test -e $(DESTDIR)$(gacdir)/4.0/$(ASSEMBLY).config; then \
		    ln -fs ../4.0/$(ASSEMBLY).config $(DESTDIR)$(gacdir)/4.5/$(ASSEMBLY).config; \
		fi; \
		if test -e $(DESTDIR)$(gacdir)/4.0/$(NAME).sigdata; then \
		    ln -fs ../4.0/$(NAME).sigdata $(DESTDIR)$(gacdir)/4.5/$(NAME).sigdata; \
		fi; \
		if test -e $(DESTDIR)$(gacdir)/4.0/$(NAME).xml; then \
		    ln -fs ../4.0/$(NAME).xml $(DESTDIR)$(gacdir)/4.5/$(NAME).xml; \
		fi; \
		if test -e $(DESTDIR)$(gacdir)/4.0/$(NAME).optdata; then \
		    ln -fs ../4.0/$(NAME).optdata $(DESTDIR)$(gacdir)/4.5/$(NAME).optdata; \
		fi; \
	  fi \
	fi

# The binaries fsc.exe and fsi.exe only get installed for Mono 4.0 profile
# This also installs 'fsharpc' and 'fsharpi'
install-bin:
	chmod +x $(outdir)$(ASSEMBLY)
	sed -e 's,[@]DIR[@],$(gacdir)/$(TARGET),g' -e 's,[@]TOOL[@],$(ASSEMBLY),g' < $(topdir)launcher > $(outdir)$(subst fs,fsharp,$(NAME))
	chmod +x $(outdir)$(subst fs,fsharp,$(NAME))
	@mkdir -p $(DESTDIR)$(gacdir)/$(TARGET)
	@mkdir -p $(DESTDIR)$(bindir)
	$(INSTALL_BIN) $(outdir)$(ASSEMBLY) $(DESTDIR)$(gacdir)/$(TARGET)
	$(INSTALL_BIN) $(outdir)$(subst fs,fsharp,$(NAME)) $(DESTDIR)$(bindir)


