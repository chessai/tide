{ owner , repo , rev , sha256 }:

builtins.fetchTarball {
  url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  inherit sha256;
}

