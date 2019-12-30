FROM haskell:8.6@sha256:c56bb7769a1404431629dfe3d9a99e7a57f8550cf4b1e4c5057ecb71453c67c1

ARG STACK_VERSION=2.1.3

# only 8.8 has latest stack :/
RUN stack upgrade --binary-version $STACK_VERSION

WORKDIR /temp

RUN stack update

ADD custom-snapshot.yaml /temp/custom-snapshot.yaml

ADD stack.yaml /temp/stack.yaml

ADD package.yaml /temp/package.yaml

RUN stack build --only-dependencies

RUN stack test --only-dependencies
