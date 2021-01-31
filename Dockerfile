FROM magicpak/haskell:8.8 AS build
ENV USER=root
RUN mkdir /build
COPY app /build/app
COPY src /build/src
COPY test /build/test
COPY dyco-mirror-bot.cabal /build/
WORKDIR /build
RUN cabal update && cabal build -j
RUN mv dist-newstyle/build/*/ghc-8.8.4/dyco-mirror-bot-0.1.0.0/x/dyco-mirror-bot-exe/build/dyco-mirror-bot-exe/dyco-mirror-bot-exe ./dyco-mirror-bot-exe &&\
	magicpak -v ./dyco-mirror-bot-exe /bundle

FROM ubuntu:focal
RUN apt-get update &&\
	apt-get install --no-install-recommends ca-certificates -y &&\
	apt-get clean &&\
	rm -rf /var/lib/apt/lists/*

COPY entrypoint.sh /
COPY --from=build /bundle /usr
ENTRYPOINT [ "/entrypoint.sh" ]
CMD [ "/usr/dyco-mirror-bot-exe" ]
