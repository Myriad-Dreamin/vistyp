import { defineConfig } from 'vite';
import { viteSingleFile } from 'vite-plugin-singlefile';
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";
import { cp, copyFile, mkdir } from "node:fs/promises";
import path from "node:path";

function vistypPackageAssets() {
  return {
    name: "vistyp-package-assets",
    apply: "build",
    async writeBundle(options) {
      const outDir = path.resolve(options.dir ?? "out");
      await mkdir(outDir, { recursive: true });
      await copyFile("index.json", path.join(outDir, "index.json"));
      await cp("packages", path.join(outDir, "packages"), { recursive: true });
    },
  };
}

export default defineConfig({
  plugins: [scalaJSPlugin(), viteSingleFile(), vistypPackageAssets()],
  build: {
    outDir: 'out',
    minify: false,
  },
});
