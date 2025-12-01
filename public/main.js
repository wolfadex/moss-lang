// import "../src/code-mirror";

Object.defineProperty(HTMLElement.prototype, "___getBoundingClientRect", {
  get() {
    return this.getBoundingClientRect();
  },
});

Object.defineProperty(DataTransfer.prototype, "___getDataText", {
  get() {
    return this.getData("text");
  },
});

const app = window.Elm.Main.init();
