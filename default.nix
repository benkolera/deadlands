{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, hie ? true
}:
let
  hie-nix = (import ./nix/hie-nix.nix { ghc843 = true; inherit hie; });
in with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  withHoogle = true;
  tools = _: hie-nix.hie-tools;
  overrides = self: super: {
    # One of the tests fails in ghcjs
    Glob = pkgs.haskell.lib.dontCheck super.Glob;
  };
})
