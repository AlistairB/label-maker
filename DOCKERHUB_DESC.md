# Label Maker

Sync github labels between repositories.

## Why?

You have series of related repositories that you want to have the same set of labels. This is useful when tracking issues from many repositories at an org level github project. It is also useful if you have a particular label setup that you like and want to use for many repositories.

## Usage

### Binary

```bash
stack run -- \
  --config-file=config.yml \
  --github-api-host=api.github.com \
  --github-token=12345
```

### Docker

```bash
docker run alistairb/label-maker \
  --config-file=config.yml \
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
