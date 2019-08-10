{ lib }:

path:

if !lib.canCleanSource path
then path
else
  lib.cleanSourceWith {
    src = lib.cleanSource path;

    filter = name: type:
      let baseName = baseNameOf (toString name);
          # File extensions to be ignored
          ignoreExts = [
            ".nix"
            ".sh"
            ".md"
            ".txt"
          ];
      in !lib.any (x: x) [
        # Ignore filetypes above
        ((type == "regular") && (lib.any (ext: lib.hasSuffix ext baseName) ignoreExts))
        # Ignore git files
        ((type == "directory") && (baseName == ".git"))
        ((type == "regular") && (baseName == ".gitignore"))
        # Ignore cabal/GHC output
        ((type == "directory") && ((baseName == "dist") || (baseName == "dist-newstyle")))
        ((type == "regular") && (lib.hasPrefix ".ghc" baseName))
      ];

  }

