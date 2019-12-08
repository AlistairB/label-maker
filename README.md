# Label Maker

Sync github labels between repositories.

## Why?

You have series of related repositories that you want to have the same set of labels. This is useful when tracking issues from many repositories at an org level github project. It is also useful if you have a particular label setup that you like and want to use for many repositories.

### Cool Things About Label Maker

- It fetches the repo labels first to determine the exact minimum updates required.
- It writes labels in parallel.
- Thus it is very fast overall.
- Syncs labels between multiple orgs.

### Alternatives

- [GitHub Label Sync](https://github.com/Financial-Times/github-label-sync) - More feature rich, but less geared towards a team that wants to keep multiple orgs / repos in sync with a single config / run.

## Usage

```bash
docker run alistairb/label-maker \
  --config-file=config.yml \
  --github-api-host=api.github.com \
  --github-token=12345
```

## Config Format

Note: Label Maker does not touch repo labels not specified in the config in some way. This allows repos to have custom if labels if they need.

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

It is recommended to run on your host with [Haskell Stack](https://haskellstack.org).

### Running Tests

```bash
stack test
```

### Running Locally

```bash
stack run -- \
  --config-file=config.yml \
  --github-api-host=api.github.com \
  --github-token=12345
```

### Working in Docker

```bash
auto/dev-environment bash
stack test # or other commands
```

### Verifying changes

This simplest way is to create a PR and let the build run. Or you can run the commands from:

- `auto/test`
- `auto/hlint`
- `auto/weeder`
- `auto/ghc-check`

These will run in Docker, or you can run the commands on the host directly eg. `stack exec --allow-different-user hlint .`


## Running in CI

Due to the time + memory it takes to setup stack/ghc and compile all the dependencies it isn't desirable to do this on CI agents.

Instead this is done separately and a base image is created with everything baked in.

This is done automatically by [a github action](https://github.com/AlistairB/label-maker/actions?query=workflow%3A%22Publish+CI+Base+Image%22) when dependencies change.
