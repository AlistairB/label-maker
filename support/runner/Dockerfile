FROM debian:stretch@sha256:85c4668abb4f26e913152ba8fd04fca5f1c2345d3e2653855e6bb0acf461ed50
# official haskell images are debian stretch https://github.com/haskell/docker-haskell/issues/8

RUN apt-get update \
 && apt-get install -y \
    libgmp10 \
    ca-certificates \
 && rm -rf /var/lib/apt/lists/*

ADD dist /app

# ci artifacts won't be executable, so need this to ensure it can be run
RUN chmod a+x /app/label-maker-exe

RUN groupadd -r appuser && useradd -r -m -g appuser appuser

RUN chown -R appuser:appuser /app

USER appuser

WORKDIR /cwd

ENTRYPOINT [ "/app/label-maker-exe" ]
