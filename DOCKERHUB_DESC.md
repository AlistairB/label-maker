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

- https://github.com/Financial-Times/github-label-sync - More feature rich, but less geared towards a team that wants to keep multiple orgs / repos in sync with a single config / run.

## Usage

### Docker

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
  sync: # all labels
    awesome-issue:
      color: '000000' # hex color
    wont-fix:
      color: 'b98fe0'
  delete: # optional
    - bad-issue-label
    - straight-to-prod
  rename: # optional
    - old-label-name: will-not-fix
      new-label-name: wont-fix # this label should exist in sync
```