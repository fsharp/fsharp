.PHONY: install-sdk-lib install-gac-lib

build:
	MONO_ENV_OPTIONS=$(monoopts) $(XBUILD) /p:Configuration=$(Configuration) /p:TargetFramework=$(TargetFramework)

clean:
	MONO_ENV_OPTIONS=$(monoopts) $(XBUILD) /p:Configuration=$(Configuration) /p:TargetFramework=$(TargetFramework) /t:Clean

# Install .optdata/.sigdata if they exist (they go alongside FSharp.Core)
# Install the .Targets file. The XBuild targets file gets installed into the place(s) expected for standard F# project
# files. For F# 2.0 project files this is
#     /usr/lib/mono/Microsoft F#/v4.0/Microsoft.FSharp.Targets
# For F# 3.0 project files this is
#     /usr/lib/mono/Microsoft SDKs/F#/3.0/Framework/v4.0/Microsoft.FSharp.Targets
# For F# 3.1 project files this is
#     /usr/lib/mono/xbuild/Microsoft/VisualStudio/v12.0/FSharp/Microsoft.FSharp.Targets
# For F# 4.0 project files this is
#     /usr/lib/mono/xbuild/Microsoft/VisualStudio/v14.0/FSharp/Microsoft.FSharp.Targets
# For F# 4.1 project files this is
#     /usr/lib/mono/xbuild/Microsoft/VisualStudio/v15.0/FSharp/Microsoft.FSharp.Targets
# 
# Here 12.0/14.0/15.0 is 'VisualStudioVersion'. xbuild should set this to 11.0/12.0/14.0/15.0, copying MSBuild.
#
# We put the F# targets and link the SDK DLLs for all these locations
#
# We put a forwarding targets file into all three locations. We also put one in 
#     .../lib/mono/xbuild/Microsoft/VisualStudio/v/FSharp/Microsoft.FSharp.Targets
# since this is the location if 'xbuild' fails to set VisualStudioVersion.
#
install-sdk-lib:
	@echo "Installing $(ASSEMBLY)"
	@mkdir -p $(DESTDIR)$(monodir)/fsharp
	@if test "x$(DELAY_SIGN)" = "x1"; then \
	    echo "Signing $(outdir)$(ASSEMBLY) with Mono key"; \
	    $(monobindir)/sn -q -R $(outdir)$(ASSEMBLY) $(srcdir)../../../mono.snk; \
	fi
	@if test x-$(NAME) = x-FSharp.Compiler; then \
	    echo "Installing extra dependency System.Collections.Immutable.dll to $(DESTDIR)$(monodir)/fsharp/"; \
	    $(INSTALL_LIB) $(outdir)System.Collections.Immutable.dll $(DESTDIR)$(monodir)/fsharp/; \
	    echo "Installing extra dependency System.Reflection.Metadata.dll to $(DESTDIR)$(monodir)/fsharp/"; \
	    $(INSTALL_LIB) $(outdir)System.Reflection.Metadata.dll $(DESTDIR)$(monodir)/fsharp/; \
	fi
	@if test x-$(NAME) = x-FSharp.Build; then \
	    echo "Installing Microsoft.FSharp.Targets and Microsoft.Portable.FSharp.Targets into install locations matching Visual Studio"; \
	    echo " --> $(DESTDIR)$(monodir)/fsharp/"; \
	    echo " --> $(DESTDIR)$(monodir)/Microsoft\ F#/v4.0/"; \
	    echo " --> $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/3.0/Framework/v4.0/"; \
	    echo " --> $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/3.1/Framework/v4.0/"; \
	    echo " --> $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/4.0/Framework/v4.0/"; \
	    echo " --> $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/4.1/Framework/v4.0/"; \
	    echo " --> $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v/FSharp/"; \
	    echo " --> $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v11.0/FSharp/"; \
	    echo " --> $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v12.0/FSharp/"; \
	    echo " --> $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v14.0/FSharp/"; \
	    echo " --> $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v15.0/FSharp/"; \
	    \
	    mkdir -p $(tmpdir); \
	    mkdir -p $(DESTDIR)$(monodir)/Microsoft\ F#/v4.0/; \
	    mkdir -p $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/3.0/Framework/v4.0/; \
	    mkdir -p $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/3.1/Framework/v4.0/; \
	    mkdir -p $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/4.0/Framework/v4.0/; \
	    mkdir -p $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/4.1/Framework/v4.0/; \
	    mkdir -p $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v/FSharp/; \
	    mkdir -p $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v11.0/FSharp/; \
	    mkdir -p $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v12.0/FSharp/; \
	    mkdir -p $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v14.0/FSharp/; \
	    mkdir -p $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v15.0/FSharp/; \
	    \
	    $(INSTALL_LIB) $(outdir)Microsoft.FSharp.Targets $(DESTDIR)$(monodir)/fsharp/; \
	    $(INSTALL_LIB) $(outdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(monodir)/fsharp/; \
	    \
	    echo '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">' > $(tmpdir)Microsoft.FSharp.Targets; \
	    echo '    <Import Project="$(monodir)/fsharp/Microsoft.FSharp.Targets" />' >> $(tmpdir)Microsoft.FSharp.Targets; \
	    echo '</Project>' >> $(tmpdir)Microsoft.FSharp.Targets; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(monodir)/Microsoft\ F#/v4.0/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/3.0/Framework/v4.0/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/3.1/Framework/v4.0/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/4.0/Framework/v4.0/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/4.1/Framework/v4.0/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v/FSharp/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v11.0/FSharp/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v12.0/FSharp/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v14.0/FSharp/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.FSharp.Targets $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v15.0/FSharp/; \
	    \
	    echo '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">' > $(tmpdir)Microsoft.Portable.FSharp.Targets; \
	    echo '    <Import Project="$(monodir)/fsharp/Microsoft.Portable.FSharp.Targets" />' >> $(tmpdir)Microsoft.Portable.FSharp.Targets; \
		echo '</Project>' >> $(tmpdir)Microsoft.Portable.FSharp.Targets; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(monodir)/Microsoft\ F#/v4.0/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/3.0/Framework/v4.0/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/3.1/Framework/v4.0/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/4.0/Framework/v4.0/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#/4.1/Framework/v4.0/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v/FSharp/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v11.0/FSharp/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v12.0/FSharp/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v14.0/FSharp/; \
	    $(INSTALL_LIB) $(tmpdir)Microsoft.Portable.FSharp.Targets $(DESTDIR)$(monodir)/xbuild/Microsoft/VisualStudio/v15.0/FSharp/; \
	fi
	@if test x-$(outsuffix) = x-net40; then \
	    if test -e $(outdir)$(NAME).dll; then \
			echo "Installing $(outdir)$(NAME).dll to $(DESTDIR)$(monodir)/fsharp/"; \
			mkdir -p $(DESTDIR)$(monodir)/fsharp/; \
			$(INSTALL_LIB) $(outdir)$(NAME).dll $(DESTDIR)$(monodir)/fsharp/; \
		fi; \
	    if test -e $(outdir)$(NAME).dll.config; then \
			echo "Installing $(outdir)$(NAME).dll to $(DESTDIR)$(monodir)/fsharp/"; \
			mkdir -p $(DESTDIR)$(monodir)/fsharp/; \
			$(INSTALL_LIB) $(outdir)$(NAME).dll.config $(DESTDIR)$(monodir)/fsharp/; \
		fi; \
	    if test -e $(outdir)$(NAME).xml; then \
			echo "Installing $(outdir)$(NAME).xml into $(DESTDIR)$(monodir)/fsharp/"; \
			mkdir -p $(DESTDIR)$(monodir)/fsharp/; \
			$(INSTALL_LIB) $(outdir)$(NAME).xml $(DESTDIR)$(monodir)/fsharp/; \
	    fi; \
	    if test -e $(outdir)$(NAME).sigdata; then \
			echo "Installing $(outdir)$(NAME).sigdata into $(DESTDIR)$(monodir)/fsharp/"; \
			mkdir -p $(DESTDIR)$(monodir)/fsharp/; \
			$(INSTALL_LIB) $(outdir)$(NAME).sigdata $(DESTDIR)$(monodir)/fsharp/; \
	    fi; \
	    if test -e $(outdir)$(NAME).optdata; then \
			echo "Installing $(outdir)$(NAME).optdata into $(DESTDIR)$(monodir)/fsharp/"; \
			mkdir -p $(DESTDIR)$(monodir)/fsharp/; \
			$(INSTALL_LIB) $(outdir)$(NAME).optdata $(DESTDIR)$(monodir)/fsharp/; \
	    fi; \
	fi
	@if test x-$(NAME) = x-FSharp.Core && test x-$(REFASSEMPATH) != x-; then \
			echo "Installing FSharp.Core $(VERSION) reference assembly into api location"; \
			echo " --> $(DESTDIR)$(monodir)/fsharp/api/$(REFASSEMPATH)/$(VERSION)"; \
			mkdir -p $(DESTDIR)$(monodir)/fsharp/api/$(REFASSEMPATH)/$(VERSION); \
			$(INSTALL_LIB) $(outdir)$(NAME).xml $(DESTDIR)$(monodir)/fsharp/api/$(REFASSEMPATH)/$(VERSION)/; \
			$(INSTALL_LIB) $(outdir)$(NAME).sigdata $(DESTDIR)$(monodir)/fsharp/api/$(REFASSEMPATH)/$(VERSION)/; \
			$(INSTALL_LIB) $(outdir)$(NAME).optdata $(DESTDIR)$(monodir)/fsharp/api/$(REFASSEMPATH)/$(VERSION)/; \
			$(INSTALL_LIB) $(outdir)$(NAME).dll $(DESTDIR)$(monodir)/fsharp/api/$(REFASSEMPATH)/$(VERSION)/; \
	fi
	@if test x-$(NAME) = x-FSharp.Core && test x-$(PCLPATH) != x-; then \
			echo "Installing FSharp.Core $(VERSION) reference assembly into api location"; \
			echo " --> $(DESTDIR)$(monodir)/fsharp/$(PCLPATH)/$(VERSION)"; \
			mkdir -p $(DESTDIR)$(monodir)/fsharp/api/$(PCLPATH)/$(VERSION); \
			$(INSTALL_LIB) $(outdir)$(NAME).xml $(DESTDIR)$(monodir)/fsharp/api/$(PCLPATH)/$(VERSION)/; \
			$(INSTALL_LIB) $(outdir)$(NAME).sigdata $(DESTDIR)$(monodir)/fsharp/api/$(PCLPATH)/$(VERSION)/; \
			$(INSTALL_LIB) $(outdir)$(NAME).optdata $(DESTDIR)$(monodir)/fsharp/api/$(PCLPATH)/$(VERSION)/; \
			$(INSTALL_LIB) $(outdir)$(NAME).dll $(DESTDIR)$(monodir)/fsharp/api/$(PCLPATH)/$(VERSION)/; \
	fi

# Install the library binaries in the GAC directory, 
install-gac-lib:
	@echo "Installing $(ASSEMBLY)"
	@if test "x$(DELAY_SIGN)" = "x1"; then \
	    echo "Signing $(outdir)$(ASSEMBLY) with Mono key"; \
	    $(monobindir)/sn -q -R $(outdir)$(ASSEMBLY) $(srcdir)../../../mono.snk; \
	fi
	@if test -e $(outdir)$(NAME).dll; then \
			echo "Installing $(outdir)$(NAME).dll to $(DESTDIR)$(monodir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/"; \
			mkdir -p $(DESTDIR)$(monodir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/; \
			$(INSTALL_LIB) $(outdir)$(NAME).dll $(DESTDIR)$(monodir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/; \
	fi
	@if test -e $(outdir)$(NAME).xml; then \
			echo "Installing $(outdir)$(NAME).xml into $(DESTDIR)$(monodir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/"; \
			mkdir -p $(DESTDIR)$(monodir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/; \
			$(INSTALL_LIB) $(outdir)$(NAME).xml $(DESTDIR)$(monodir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/; \
	fi
	@if test -e $(outdir)$(NAME).sigdata; then \
			echo "Installing $(outdir)$(NAME).sigdata into $(DESTDIR)$(monodir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/"; \
			mkdir -p $(DESTDIR)$(monodir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/; \
			$(INSTALL_LIB) $(outdir)$(NAME).sigdata $(DESTDIR)$(monodir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/; \
	fi
	@if test -e $(outdir)$(NAME).optdata; then \
			echo "Installing $(outdir)$(NAME).optdata into $(DESTDIR)$(monodir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/"; \
			mkdir -p $(DESTDIR)$(monodir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/; \
			$(INSTALL_LIB) $(outdir)$(NAME).optdata $(DESTDIR)$(monodir)/gac/$(NAME)/$(VERSION)__$(TOKEN)/; \
	fi


# The binaries fsc.exe and fsi.exe only get installed for Mono 4.5 profile
# This also installs 'fsharpc' and 'fsharpi' and 'fsharpiAnyCpu'
install-bin:
	chmod +x $(outdir)$(ASSEMBLY)
	sed -e 's,[@]DIR[@],$(monodir)/fsharp,g' -e 's,[@]TOOL[@],$(ASSEMBLY),g' -e 's,[@]MONOBINDIR[@],$(monobindir),g' < $(topdir)launcher > $(outdir)$(subst fs,fsharp,$(NAME))
	chmod +x $(outdir)$(subst fs,fsharp,$(NAME))
	@mkdir -p $(DESTDIR)$(monodir)/fsharp
	@mkdir -p $(DESTDIR)$(bindir)
	$(INSTALL_BIN) $(outdir)$(ASSEMBLY) $(DESTDIR)$(monodir)/fsharp
	$(INSTALL_BIN) $(outdir)$(ASSEMBLY).config $(DESTDIR)$(monodir)/fsharp
	$(INSTALL_BIN) $(outdir)$(subst fs,fsharp,$(NAME)) $(DESTDIR)$(bindir)


