FROM debian:stretch@sha256:99768ac883dc7d4f9c8f55dade9abb39afc1a3d737a8457dc5735ef868288cef
# official haskell images are debian stretch https://github.com/haskell/docker-haskell/issues/8

RUN apt-get update \
 && apt-get install -y \
    libgmp10 \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

ADD dist /app

# ci artifacts won't be executable, so need this to ensure it can be run
RUN chmod a+x /app/label-maker-exe

RUN groupadd -r appuser && useradd -r -m -g appuser appuser

RUN chown -R appuser:appuser /app

USER appuser

CMD echo 'Starting..' && ./label-maker-exe
