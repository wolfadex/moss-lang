import { defineConfig } from "vite";
import elm from "vite-plugin-elm-watch";

export default defineConfig({
  // server: {
  //   proxy: {
  //     "/api": "http://localhost:8000",
  //   },
  // },
  plugins: [
    elm({
      mode: "debug",
    }),
  ],
});
