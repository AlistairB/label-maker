FROM haskell:8.6@sha256:79e0b63a080b0c1d2acc791b4b65a00076dd6e0e4c49311fbf08c11bde8b0a96

ARG STACK_VERSION=2.1.3

# only 8.8 has latest stack :/
RUN stack upgrade --binary-version $STACK_VERSION
