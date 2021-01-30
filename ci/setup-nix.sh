#!/bin/bash

set -euxo pipefail

curl -L https://nixos.org/nix/install | sh

source ~/.nix-profile/etc/profile.d/nix.sh

nix-env -iA cachix -f https://cachix.org/api/v1/install

cachix use iohk
cachix use matobet-rpi
