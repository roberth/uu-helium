let hostNixPkgs = import <nixpkgs> {};
in import (hostNixPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "fdd84fd86dc3c63db7d2b54b49958bf26c6eb69a";
    sha256 = "1vkpmhad7ax9ih6dfxbqzr8n43058x6r876vjfrrry05l9jmrrbg";
})
