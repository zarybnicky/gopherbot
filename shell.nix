let pkgs = import <nixpkgs> {
  overlays = [ (self: super: {
    haskellPackages = super.haskellPackages.override (oldArgs: {
      overrides = super.lib.composeExtensions (oldArgs.overrides or (_: _: {})) (hself: hsuper: {
        gopherbot = hself.callCabal2nix "gopherbot" ./. {};
      });
    });
  })];
}; in
pkgs.haskellPackages.shellFor {
  packages = p: [p.gopherbot];
  buildInputs = [pkgs.sass];
}
