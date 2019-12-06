FROM debian:stretch

RUN apt-get update \
 && apt-get install -y \
    libc6 \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

ADD dist /app

# ci artifacts won't be executable, so need this to ensure it can be run
RUN chmod a+x /app/label-maker-exe

RUN groupadd -r appuser && useradd -r -m -g appuser appuser

RUN chown -R appuser:appuser /app

USER appuser

CMD echo 'Starting..' && ./label-maker-exe
