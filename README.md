# Label Maker

Sync github labels between repositories.

This project is WIP.

1. Basic logic working.
2. CURRENT - Get CLI options working.
3. Nicely release with docker for general usage.

Below is how it should eventually work.

## Why?

You have series of related repositories that you want to have the same set of labels. This is useful when tracking issues from many repositories on an org level github project. It is also useful if you have a particular label setup that you like and want to use for many repositories.

## Usage

### Binary

```bash
stack run -- \
  --config-file=config/config.yml \
  --github-api-host=api.github.com \
  --github-token=12345
```

### Docker

```bash
docker run --rm alistairb/label-maker \
  --config-file=config/config.yml \
  --github-api-host=api.github.com \
  --github-token=12345
```

## Config Format

```yaml
organizations:
  cool-org:
    repos: all
  other-org:
    repos:
      - thinger
      - wrangler

labels:
  sync:
    awesome-issue:
      color: '000000' # hex color
    wont-fix:
      color: 'b98fe0' # hex color
  delete:
    - bad-issue-label
    - straight-to-prod
  rename:
    - old-label-name: will-not-fix
      new-label-name: wont-fix
```
## Development

### Running in CI

Due to the time + memory it takes to setup stack/ghc and compile all the dependencies it isn't desirable to do this on CI agents.

Instead this is done separately and a base image is created with everything baked in. It is recommended to push a new version of the base container when there are significant stackage version or library changes.

If the CI agent has cached the docker image you may want to update the version in `auto/release-ci-base` and `docker-compose.yml`.

```bash
auto/release-ci-base
```
