import { LRLanguage, LanguageSupport } from "@codemirror/language";
// import { styleTags, tags as t } from "@lezer/highlight";
// import { parser } from "./your-lezer-grammar"; // Generated from .grammar file
import { completeFromList } from "@codemirror/autocomplete";
import { StreamLanguage } from "@codemirror/language";

// // Option 1: Using a Lezer grammar (recommended for complex languages)
// const myLanguage = LRLanguage.define({
//   parser: parser.configure({
//     props: [
//       styleTags({
//         Identifier: t.variableName,
//         String: t.string,
//         Number: t.number,
//         Keyword: t.keyword,
//         Comment: t.comment,
//         "( )": t.paren,
//         "{ }": t.brace,
//       }),
//     ],
//   }),
// });

// Option 2: Simple StreamLanguage for basic highlighting

const myLanguage = StreamLanguage.define({
  token(stream) {
    if (stream.match(/#.*/)) return "comment";
    if (stream.match(/"[^"]*"/)) return "string";
    if (stream.match(/\b(if|elif|else|iff|at|each|size)\b/)) return "keyword";
    if (stream.match(/\b\d+\b/)) return "number";
    if (stream.match(/[a-zA-Z_]\w*/)) return "variableName";
    stream.next();
    return null;
  },
});

// Custom completions
const myCompletions = completeFromList([
  { label: "let", type: "keyword" },
  { label: "fn", type: "keyword" },
  { label: "match", type: "keyword" },
  { label: "myFunction", type: "function", info: "Does something" },
]);

// Create the language support
const myLanguageSupport = new LanguageSupport(myLanguage, [
  myLanguage.data.of({ autocomplete: myCompletions }),
]);

// Apply to editor after Elm mounts it
function initEditor(elementId) {
  const el = document.getElementById(elementId);
  if (el) {
    el.language = myLanguageSupport;
  }
}

// Call after Elm renders
// initEditor("my-editor");

export { myLanguageSupport, initEditor };
