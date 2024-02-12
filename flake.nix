{
  description = "DIT313-Computability";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  outputs = { self, nixpkgs }:
    let
      lastModifiedDate = self.lastModifiedDate or self.lastModified or "19700101";
      version = builtins.substring 0 8 lastModifiedDate;
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in
    {
      devShells = forAllSystems
        (system:
          let pkgs = nixpkgsFor.${system}; in {
            default = pkgs.mkShell
              {
                buildInputs = with pkgs; [
                  typst
                  typstfmt
                  typst-lsp
                  gyre-fonts
                  zlib
                  haskellPackages.cabal-install
                  haskellPackages.fourmolu
                  haskellPackages.ghcid
                  (haskellPackages.ghcWithPackages
                    (
                      pkgs: [ haskellPackages.QuickCheck ]
                    ))
                  pkgs.haskell-language-server
                ];
                shellHook = ''
                  export TYPST_FONT_PATHS=/nix/store/s45d7530dlxbc0pbwyk684v1kfi505v7-gyre-fonts-2.005/share/fonts/truetype/
                '';
              };
          });
      defaultPackage = forAllSystems (system: self.packages.${system}.dit313);
    };
}
