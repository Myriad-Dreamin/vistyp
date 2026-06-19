# Agent Notes

## Typst WASM Source

- The default WASM source must stay on CDN (`jsdelivr`) for normal development and commits.
- To debug against a local `typst.ts` checkout, explicitly set `window.$typst$moduleSource = "local"` in `index.html`.
- Before committing, change any local override back to CDN behavior. In practice, remove or comment out the local override so `Typst.scala` falls back to `jsdelivr`.
- For bundle builds, switch the WASM source to an inline/bundled mode instead of local or CDN.
