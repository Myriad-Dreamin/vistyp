import { defineConfig } from 'vite';
import { viteSingleFile } from 'vite-plugin-singlefile';
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";

export default defineConfig({
  plugins: [scalaJSPlugin(), viteSingleFile()],
  build: {
    outDir: 'out',
    minify: false,
  },
});
