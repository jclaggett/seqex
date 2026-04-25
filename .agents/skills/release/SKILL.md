---
name: release
description: Cut a release for this project — bump the version in project.clj, commit, push, create a GitHub release, and deploy to Clojars. Use this skill whenever the user asks to release, publish, cut a version, or ship to Clojars.
---

# Release

Use this skill when the user wants to publish a new version of panini.

## Goals

- Keep the version in `project.clj`, the git tag, the GitHub release, and the Clojars artifact in sync.
- Never publish without the user confirming the version number.
- Leave the repo in a clean state after the release.

## Required workflow

1. **Determine the new version.**
   - Read the current version from `project.clj`.
   - Infer the bump type (patch / minor / major) from context, or ask if unclear.
   - Show the user the proposed new version and wait for explicit confirmation before touching any files.

2. **Bump the version.**
   - Edit `project.clj`: update the version string.

3. **Commit and push.**
   - Stage `project.clj` (and any other changed files the user has asked to include).
   - Use the git-commit skill to draft and review the commit message.
   - After the user approves, commit and push to `master`.

4. **Create a GitHub release.**
   - Run `gh release create vX.Y.Z --title "vX.Y.Z" --notes "..."`.
   - Write release notes that summarise what changed since the previous tag. Pull context from recent commits (`git log <prev-tag>..HEAD --oneline`).

5. **Deploy to Clojars.**
   - Run `lein deploy clojars`.
   - Report success or surface any errors.

## Version format

This project uses [Semantic Versioning](https://semver.org/): `MAJOR.MINOR.PATCH`.

- **Patch** — bug fixes, documentation tweaks, no API changes.
- **Minor** — new backwards-compatible functionality.
- **Major** — breaking changes.

## Guardrails

- Never bump the version or create a tag until the user has confirmed the new version number.
- Never run `lein deploy clojars` before the tag and GitHub release are in place.
- If `lein deploy clojars` fails, report the error in full and do not retry silently.
- Do not create a release from a dirty working tree; ensure everything is committed first.
