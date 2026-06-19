# Agent Notes

## Git and Pull Requests

- Use Conventional Commits for commit messages.
- PR descriptions should be a list of items without a heading/title, and should only include modified features and issue operations, for example `Close #123`.
- Do not include validation logs, command output, or other execution details in PR descriptions.

## Typst WASM Source

- The default WASM source must stay on CDN (`jsdelivr`) for normal development and commits.
- To debug against a local `typst.ts` checkout, explicitly set `window.$typst$moduleSource = "local"` in `index.html`.
- Before committing, change any local override back to CDN behavior. In practice, remove or comment out the local override so `Typst.scala` falls back to `jsdelivr`.
- For bundle builds, switch the WASM source to an inline/bundled mode instead of local or CDN.
