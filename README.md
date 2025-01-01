## Vistyp

This is a sample project to exhibit how to develop along with/without a local built typst.ts. It was named "Cetz Editor".

### Debug the Vistyp

Run the vite dev server to get hot-reloading functionality:

```bash
# watch scala
$ sbt "~fastLinkJS"
# start a vite dev server
$ yarn run dev
# open the browser
$ open http://localhost:5173
```

### Build the Vistyp

The Vistyp is bundled into a single html file, so you can open it directly in the browser:

```bash
# bundle files
$ yarn run build
# open the browser
$ open out/index.html
```

### Setup the wasm modules

There are three ways to run the sample project (Also see the [`src/index.mts`](./src/index.mts) file).

1. Use the wasm modules from CDN (default):

- ensure that the `projects/cetz-editor` is cloned individually (not in the source tree of `typst.ts`)
  ```bash
  git clone https://github.com/Myriad-Dreamin/cetz-editor.git
  ```
- ensure that you can connect to jsdelivr.net
- change `window.$typst$moduleSource` manually in the [`src/index.mts`](./src/index.mts) file:

  ```ts
  let moduleSource: ModuleSource = "jsdelivr";
  ```

2. Bundle the wasm modules via vite:

```ts
// @ts-ignore
import compiler from "@myriaddreamin/typst-ts-web-compiler/pkg/typst_ts_web_compiler_bg.wasm?url";
// @ts-ignore
import renderer from "@myriaddreamin/typst-ts-renderer/pkg/typst_ts_renderer_bg.wasm?url";
```

3. Serve the wasm modules locally via `typst-ts-dev-server` (See [Setup the typst.ts in local](#setup-the-typstts-in-local)):

- runs `yarn dev` in the `typst.ts` project to start a local server
- change `window.$typst$moduleSource` manually in the [`src/index.mts`](./src/index.mts) file:

  ```ts
  window.$typst$moduleSource = "local";
  ```

### Align the version of the wasm modules with the version of the typst.ts package

Note that you should align the versions:

```json
{
  "peerDependencies": {
    "@myriaddreamin/typst.ts": "0.5.4",
    "@myriaddreamin/typst-ts-renderer": "0.5.4",
    "@myriaddreamin/typst-ts-web-compiler": "0.5.4"
  }
}
```

Otherwise you will get an error like this:

```log
Uncaught (in promise) LinkError: WebAssembly.instantiate(): Import #73 module="wbg" function="__wbindgen_closure_wrapper16065": function import requires a callabl
```

Or this:

```log
panicked at `called Result::unwrap()` on an `Err` value: CheckBytesError(...)
```

### Build and provide the wasm modules in local

Build the typst.ts project and start a local server:

```bash
# Optional: download the font assets if you haven't done so.
$ git submodule update --init --recursive .
# build all of typescript packages
$ yarn install && yarn run build:pkg
# compile typst document for demo
$ cargo run --bin typst-ts-dev-server -- compile --compiler debug corpus --cat skyzh-cv
# start a local server
$ cargo run --bin typst-ts-dev-server -- run http --corpus ./fuzzers/corpora/
```

### Caution

This is a quick and dirty project, so you may face some problems when you try to run it. Please feel free to open an [issue](https://github.com/Myriad-Dreamin/typst.ts/issues) or a [discussion](https://github.com/Myriad-Dreamin/typst.ts/discussions) if you have any questions.

Also feel free to open a [issue](https://github.com/Myriad-Dreamin/cetz-editor/pulls) to help us improve this sample project.
