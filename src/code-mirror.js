import { EditorView, basicSetup } from "codemirror";
import { EditorState, Compartment, Prec } from "@codemirror/state";
import { keymap } from "@codemirror/view";
import { autocompletion, completeFromList } from "@codemirror/autocomplete";
import { searchKeymap, search } from "@codemirror/search";
import { closeBrackets, closeBracketsKeymap } from "@codemirror/autocomplete";
import {
  LanguageSupport,
  LRLanguage,
  StreamLanguage,
} from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";
import { LRParser } from "@lezer/lr";
import { oneDark } from "@codemirror/theme-one-dark";

class CodeMirrorEditor extends HTMLElement {
  static get observedAttributes() {
    return ["value", "theme", "readonly"];
  }

  constructor() {
    super();
    this.attachShadow({ mode: "open" });
    this.langCompartment = new Compartment();
    this.themeCompartment = new Compartment();
    this.readonlyCompartment = new Compartment();
    this._updating = false;
    this._language = null;
  }

  connectedCallback() {
    const container = document.createElement("div");
    container.style.cssText = "height:100%;width:100%;";
    this.shadowRoot.appendChild(container);

    const theme = this.getAttribute("theme");
    const readonly = this.hasAttribute("readonly");
    const value = this.getAttribute("value") || "";

    this.view = new EditorView({
      state: EditorState.create({
        doc: value,
        extensions: [
          basicSetup,
          this.langCompartment.of([]),
          this.themeCompartment.of(theme === "dark" ? oneDark : []),
          this.readonlyCompartment.of(EditorState.readOnly.of(readonly)),
          autocompletion(),
          closeBrackets(),
          search(),
          keymap.of([...closeBracketsKeymap, ...searchKeymap]),
          Prec.highest(
            keymap.of([
              {
                key: "Mod-Enter",
                run: () => {
                  this.dispatchEvent(
                    new CustomEvent("submit", {
                      bubbles: true,
                      composed: true,
                    }),
                  );
                  return true;
                },
              },
            ]),
          ),
          EditorView.updateListener.of((update) => {
            if (update.docChanged && !this._updating) {
              this.dispatchEvent(
                new CustomEvent("change", {
                  detail: update.state.doc.toString(),
                  bubbles: true,
                  composed: true,
                }),
              );
            }
          }),
        ],
      }),
      parent: container,
    });

    if (this._language) {
      this._applyLanguage(this._language);
    }
  }

  disconnectedCallback() {
    this.view?.destroy();
  }

  attributeChangedCallback(name, oldVal, newVal) {
    if (!this.view || oldVal === newVal) return;

    switch (name) {
      case "value":
        if (this.view.state.doc.toString() !== newVal) {
          this._updating = true;
          this.view.dispatch({
            changes: {
              from: 0,
              to: this.view.state.doc.length,
              insert: newVal || "",
            },
          });
          this._updating = false;
        }
        break;
      case "theme":
        this.view.dispatch({
          effects: this.themeCompartment.reconfigure(
            newVal === "dark" ? oneDark : [],
          ),
        });
        break;
      case "readonly":
        this.view.dispatch({
          effects: this.readonlyCompartment.reconfigure(
            EditorState.readOnly.of(newVal !== null),
          ),
        });
        break;
    }
  }

  // Set language via property (from JS or Elm JSON)
  set language(langDef) {
    this._language = langDef;
    if (this.view) {
      this._applyLanguage(langDef);
    }
  }

  _applyLanguage(langDef) {
    if (langDef instanceof LanguageSupport) {
      this.view.dispatch({
        effects: this.langCompartment.reconfigure(langDef),
      });
      return;
    }

    // Build from Elm JSON definition
    if (langDef && typeof langDef === "object" && langDef.tokens) {
      const lang = this._buildLanguageFromDef(langDef);
      this.view.dispatch({ effects: this.langCompartment.reconfigure(lang) });
    }
  }

  _buildLanguageFromDef(def) {
    const tokenRules = def.tokens.map((rule) => ({
      regex: new RegExp(rule.pattern),
      token: rule.token,
    }));

    const streamLang = StreamLanguage.define({
      token(stream) {
        for (const rule of tokenRules) {
          if (stream.match(rule.regex)) {
            return rule.token;
          }
        }
        stream.next();
        return null;
      },
    });

    const completions = (def.completions || []).map((c) => ({
      label: c.label,
      type: c.type,
      info: c.info || undefined,
      detail: c.detail || undefined,
    }));

    const extensions = [];
    if (completions.length > 0) {
      extensions.push(
        streamLang.data.of({
          autocomplete: completeFromList(completions),
        }),
      );
    }

    return new LanguageSupport(streamLang, extensions);
  }

  // Set custom completions
  set completions(completionSource) {
    // Reconfigure autocompletion with custom source
    if (this.view && completionSource) {
      // You'd need a compartment for this if you want dynamic updates
    }
  }

  get value() {
    return this.view?.state.doc.toString() || "";
  }
  set value(v) {
    this.setAttribute("value", v);
  }
}

customElements.define("code-mirror", CodeMirrorEditor);

// Export for external use
export { CodeMirrorEditor };
export { LanguageSupport, LRLanguage } from "@codemirror/language";
export { styleTags, tags } from "@lezer/highlight";
export { LRParser } from "@lezer/lr";
