// Note: This is the Vite development entrypoint
import "./public/style.css";
import Main from "./src/Main.elm";

// Provide this on the `window.Elm` so "public/main.js"
// can be used in production _and_ development.
window.Elm = { Main };
