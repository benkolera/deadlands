{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, hie ? false
}:
let 
  hie-nix = (import ./nix/hie-nix.nix { ghc843 = true; inherit hie; });
in with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  withHoogle = true;
  tools = _: hie-nix.hie-tools;
