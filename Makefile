include $(topsrcdir)mono/config.make

.PHONY: restore

restore:
	MONO_ENV_OPTIONS=$(monoopts) mono .nuget/NuGet.exe restore packages.config -PackagesDirectory packages -ConfigFile .nuget/NuGet.Config

# Make the proto using the bootstrap, then make the final compiler using the proto
# We call MAKE sequentially because we don't want build-final to explicitly depend on build-proto,
# as that causes a complete recompilation of both proto and final everytime you touch the
# compiler sources.
all:
	$(MAKE) build-proto
	$(MAKE) build

build-proto: restore
	MONO_ENV_OPTIONS=$(monoopts) $(MSBUILD) /p:Configuration=Proto /p:TargetDotnetProfile=$(TargetDotnetProfile) src/fsharp/FSharp.Build-proto/FSharp.Build-proto.fsproj
	MONO_ENV_OPTIONS=$(monoopts) $(MSBUILD) /p:Configuration=Proto /p:TargetDotnetProfile=$(TargetDotnetProfile) src/fsharp/Fsc-proto/Fsc-proto.fsproj

# The main targets
build:
	MONO_ENV_OPTIONS=$(monoopts) $(MSBUILD) /p:Configuration=$(Configuration) /p:TargetDotnetProfile=net40 src/fsharp/FSharp.Core/FSharp.Core.fsproj
	MONO_ENV_OPTIONS=$(monoopts) $(MSBUILD) /p:Configuration=$(Configuration) /p:TargetDotnetProfile=net40 src/fsharp/FSharp.Build/FSharp.Build.fsproj
	MONO_ENV_OPTIONS=$(monoopts) $(MSBUILD) /p:Configuration=$(Configuration) /p:TargetDotnetProfile=net40 src/fsharp/FSharp.Compiler.Private/FSharp.Compiler.Private.fsproj
	MONO_ENV_OPTIONS=$(monoopts) $(MSBUILD) /p:Configuration=$(Configuration) /p:TargetDotnetProfile=net40 src/fsharp/Fsc/Fsc.fsproj
	MONO_ENV_OPTIONS=$(monoopts) $(MSBUILD) /p:Configuration=$(Configuration) /p:TargetDotnetProfile=net40 src/fsharp/FSharp.Compiler.Interactive.Settings/FSharp.Compiler.Interactive.Settings.fsproj
	MONO_ENV_OPTIONS=$(monoopts) $(MSBUILD) /p:Configuration=$(Configuration) /p:TargetDotnetProfile=net40 src/fsharp/FSharp.Compiler.Server.Shared/FSharp.Compiler.Server.Shared.fsproj
	MONO_ENV_OPTIONS=$(monoopts) $(MSBUILD) /p:Configuration=$(Configuration) /p:TargetDotnetProfile=net40 src/fsharp/fsi/Fsi.fsproj
	MONO_ENV_OPTIONS=$(monoopts) $(MSBUILD) /p:Configuration=$(Configuration) /p:TargetDotnetProfile=net40 src/fsharp/fsiAnyCpu/FsiAnyCPU.fsproj
	MONO_ENV_OPTIONS=$(monoopts) $(MSBUILD) /p:Configuration=$(Configuration) /p:TargetDotnetProfile=net40 src/fsharp/FSharp.Core.Unittests/FSharp.Core.Unittests.fsproj
	$(MAKE) -C mono/policy.2.0.FSharp.Core TargetDotnetProfile=net40 $@
	$(MAKE) -C mono/policy.2.3.FSharp.Core TargetDotnetProfile=net40 $@
	$(MAKE) -C mono/policy.3.3.FSharp.Core TargetDotnetProfile=net40 $@
	$(MAKE) -C mono/policy.3.7.FSharp.Core TargetDotnetProfile=net40 $@
	$(MAKE) -C mono/policy.3.47.FSharp.Core TargetDotnetProfile=net40 $@
	$(MAKE) -C mono/policy.3.78.FSharp.Core TargetDotnetProfile=net40 $@
	$(MAKE) -C mono/policy.3.259.FSharp.Core TargetDotnetProfile=net40 $@
	$(MAKE) -C mono/policy.4.0.FSharp.Core TargetDotnetProfile=net40 $@
	$(MAKE) -C mono/policy.4.3.FSharp.Core TargetDotnetProfile=net40 $@
	$(MAKE) -C mono/policy.4.4.FSharp.Core TargetDotnetProfile=net40 $@
	mkdir -p $(Configuration)/fsharp30/net40/bin
	mkdir -p $(Configuration)/fsharp31/net40/bin
	mkdir -p $(Configuration)/fsharp40/net40/bin
	cp -p packages/FSharp.Core.3.0.2/lib/net40/* $(Configuration)/fsharp30/net40/bin
	cp -p packages/FSharp.Core.3.1.2.5/lib/net40/* $(Configuration)/fsharp31/net40/bin
	cp -p packages/FSharp.Core.4.0.0.1/lib/net40/* $(Configuration)/fsharp40/net40/bin
ifeq ("$(pclenabled47)", "yes")
	mkdir -p $(Configuration)/portable7/bin
	cp -p packages/FSharp.Core.4.1.17/lib/portable-net45+netcore45/* $(Configuration)/portable7/bin
endif
ifeq ("$(pclenabled7)", "yes")
	mkdir -p $(Configuration)/portable47/bin
	cp -p packages/FSharp.Core.4.1.17/lib/portable-net45+sl5+netcore45/* $(Configuration)/portable47/bin
endif
ifeq ("$(pclenabled78)", "yes")
	mkdir -p $(Configuration)/portable78/bin
	cp -p packages/FSharp.Core.4.1.17/lib/portable-net45+netcore45+wp8/* $(Configuration)/portable78/bin
endif
ifeq ("$(pclenabled259)", "yes")
	mkdir -p $(Configuration)/portable259/bin
	cp -p packages/FSharp.Core.4.1.17/lib/portable-net45+netcore45+wpa81+wp8/* $(Configuration)/portable259/bin
endif
ifeq ("$(monodroidenabled)", "yes")
	mkdir -p $(Configuration)/monoandroid10+monotouch10+xamarinios10/bin
	cp -p packages/FSharp.Core.4.1.17/lib/portable-net45+monoandroid10+monotouch10+xamarinios10/* $(Configuration)/monoandroid10+monotouch10+xamarinios10/bin
endif
ifeq ("$(xamarinmacenabled)", "yes")
	mkdir -p $(Configuration)/xamarinmacmobile/bin
	cp -p packages/FSharp.Core.4.1.17/lib/xamarinmac20/* $(Configuration)/xamarinmacmobile/bin
endif



install:
	-rm -fr $(DESTDIR)$(monodir)/fsharp
	-rm -fr $(DESTDIR)$(monodir)/Microsoft\ F#
	-rm -fr $(DESTDIR)$(monodir)/Microsoft\ SDKs/F#
	-rm -fr $(DESTDIR)$(monodir)/gac/FSharp.Core
	-rm -fr $(DESTDIR)$(monodir)/gac/FSharp.Compiler.Private
	-rm -fr $(DESTDIR)$(monodir)/msbuild/Microsoft/VisualStudio/v/FSharp
	-rm -fr $(DESTDIR)$(monodir)/msbuild/Microsoft/VisualStudio/v11.0/FSharp
	-rm -fr $(DESTDIR)$(monodir)/msbuild/Microsoft/VisualStudio/v12.0/FSharp
	-rm -fr $(DESTDIR)$(monodir)/msbuild/Microsoft/VisualStudio/v14.0/FSharp
	-rm -fr $(DESTDIR)$(monodir)/msbuild/Microsoft/VisualStudio/v15.0/FSharp
	$(MAKE) -C mono/FSharp.Core TargetDotnetProfile=net40 install
	$(MAKE) -C mono/FSharp.Build install
	$(MAKE) -C mono/FSharp.Compiler.Private install
	$(MAKE) -C mono/Fsc install
	$(MAKE) -C mono/FSharp.Compiler.Interactive.Settings install
	$(MAKE) -C mono/FSharp.Compiler.Server.Shared install
	$(MAKE) -C mono/fsi install
	$(MAKE) -C mono/fsiAnyCpu install
	$(MAKE) -C mono/FSharp.Core TargetDotnetProfile=net40 FSharpCoreBackVersion=3.0 install
	$(MAKE) -C mono/FSharp.Core TargetDotnetProfile=net40 FSharpCoreBackVersion=3.1 install
	$(MAKE) -C mono/FSharp.Core TargetDotnetProfile=net40 FSharpCoreBackVersion=4.0 install
	$(MAKE) -C mono/policy.2.0.FSharp.Core TargetDotnetProfile=net40 install
	$(MAKE) -C mono/policy.2.3.FSharp.Core TargetDotnetProfile=net40 install
	$(MAKE) -C mono/policy.3.3.FSharp.Core TargetDotnetProfile=net40 install
	$(MAKE) -C mono/policy.3.7.FSharp.Core TargetDotnetProfile=net40 install
	$(MAKE) -C mono/policy.3.47.FSharp.Core TargetDotnetProfile=net40 install
	$(MAKE) -C mono/policy.3.78.FSharp.Core TargetDotnetProfile=net40 install
	$(MAKE) -C mono/policy.3.259.FSharp.Core TargetDotnetProfile=net40 install
	$(MAKE) -C mono/policy.4.0.FSharp.Core TargetDotnetProfile=net40 install
	$(MAKE) -C mono/policy.4.3.FSharp.Core TargetDotnetProfile=net40 install
	$(MAKE) -C mono/policy.4.4.FSharp.Core TargetDotnetProfile=net40 install
ifeq ("$(pclenabled47)", "yes")
	$(MAKE) -C mono/FSharp.Core TargetDotnetProfile=portable47 install
endif
ifeq ("$(pclenabled7)", "yes")
	$(MAKE) -C mono/FSharp.Core TargetDotnetProfile=portable7 install
endif
ifeq ("$(pclenabled78)", "yes")
	$(MAKE) -C mono/FSharp.Core TargetDotnetProfile=portable78 install
endif
ifeq ("$(pclenabled259)", "yes")
	$(MAKE) -C mono/FSharp.Core TargetDotnetProfile=portable259 install
endif
ifeq ("$(monodroidenabled)", "yes")
	$(MAKE) -C mono/FSharp.Core TargetDotnetProfile=monoandroid10+monotouch10+xamarinios10 install
endif
ifeq ("$(xamarinmacenabled)", "yes")
	$(MAKE) -C mono/FSharp.Core TargetDotnetProfile=xamarinmacmobile install
endif
	echo "------------------------------ INSTALLED FILES --------------"
	ls -xlR $(DESTDIR)$(monodir)/fsharp $(DESTDIR)$(monodir)/msbuild $(DESTDIR)$(monodir)/gac/FSharp* $(DESTDIR)$(monodir)/Microsoft*

dist:
	-rm -r fsharp-$(DISTVERSION) fsharp-$(DISTVERSION).tar.bz2
	mkdir -p fsharp-$(DISTVERSION)
	(cd $(topdir) && git archive HEAD |(cd $(builddir)fsharp-$(DISTVERSION) && tar xf -))
	list='$(EXTRA_DIST)'; for s in $$list; do \
		(cp $(topdir)$$s fsharp-$(DISTVERSION)/$$s) \
	done;
	tar cvjf fsharp-$(DISTVERSION).tar.bz2 $(patsubst %,--exclude=%, $(NO_DIST)) fsharp-$(DISTVERSION)
	du -b fsharp-$(DISTVERSION).tar.bz2

