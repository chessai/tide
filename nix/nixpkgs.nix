let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "dd2e32a4c98de89b98a9fe01032fd3062c31ac21";
    sha256 = "14xzys8g0yq2hmhln6733700fwl31aqny3c045w37h6sdxkmyz9z";
  };

in
  nixpkgs
