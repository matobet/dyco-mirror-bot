FROM debian:bullseye
COPY . /build
WORKDIR /build
ENV USER=root
ENV NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1
RUN \
	apt-get update &&\
	apt-get install --no-install-recommends -y 'nix-setup-systemd=2.3.7+dfsg1-1' 'ca-certificates=20210119' &&\
	apt-get clean &&\
	rm -rf /var/lib/apt/lists/*
RUN \
	echo 'filter-syscalls = false' >> /etc/nix/nix.conf &&\
	echo 'sandbox = false' >> /etc/nix/nix.conf &&\
	echo 'substituters = https://cache.nixos.org https://hydra.iohk.io https://matobet-rpi.cachix.org' >> /etc/nix/nix.conf &&\
	echo 'trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= matobet-rpi.cachix.org-1:FBU5CNeBaGQOsDn51HkthVEvHxHAur4t58AvFJLRNSg=' >> /etc/nix/nix.conf &&\
	. /usr/share/doc/nix-bin/examples/nix-profile.sh &&\
	nix-channel --add https://nixos.org/channels/nixpkgs-unstable &&\
	nix-channel --update &&\
	nix-build -A dyco-mirror-bot.components.exes.dyco-mirror-bot-exe
