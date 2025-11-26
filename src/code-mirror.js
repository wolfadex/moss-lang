import { EditorView, keymap, Decoration, hoverTooltip } from "@codemirror/view";
import { basicSetup } from "codemirror";
import {
  EditorState,
  Compartment,
  Prec,
  StateField,
  StateEffect,
} from "@codemirror/state";
import { indentWithTab } from "@codemirror/commands";
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

const setDiagnostics = StateEffect.define();

const diagnosticsField = StateField.define({
  create: () => ({ decorations: Decoration.none, diagnostics: [] }),
  update(state, tr) {
    for (const e of tr.effects) {
      if (e.is(setDiagnostics)) {
        const builder = [];
        for (const d of e.value) {
          const from = d.from,
            to = d.to ?? d.from;
          const cls = d.severity === "error" ? "cm-error" : "cm-warning";
          builder.push(
            Decoration.mark({ class: cls }).range(from, Math.max(to, from + 1)),
          );
        }
        return {
          decorations: Decoration.set(builder, true),
          diagnostics: e.value,
        };
      }
    }
    return {
      decorations: state.decorations.map(tr.changes),
      diagnostics: state.diagnostics,
    };
  },
  provide: (f) => EditorView.decorations.from(f, (s) => s.decorations),
});

const diagnosticTooltip = hoverTooltip((view, pos) => {
  const { diagnostics } = view.state.field(diagnosticsField);
  for (const d of diagnostics) {
    const from = d.from,
      to = d.to ?? d.from + 1;
    if (pos >= from && pos <= to && d.message) {
      return {
        pos: from,
        end: to,
        create: () => {
          const el = document.createElement("div");
          el.className = "cm-diagnostic-tooltip";
          el.textContent = d.message;
          return { dom: el };
        },
      };
    }
  }
  return null;
});

const diagnosticsTheme = EditorView.baseTheme({
  ".cm-error": { textDecoration: "underline wavy red" },
  ".cm-warning": { textDecoration: "underline wavy orange" },
  ".cm-diagnostic-tooltip": {
    padding: "4px 8px",
    background: "#333",
    color: "#fff",
    borderRadius: "4px",
    fontSize: "13px",
  },
});

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
          diagnosticsField,
          diagnosticTooltip,
          diagnosticsTheme,
          this.langCompartment.of([]),
          this.themeCompartment.of(theme === "dark" ? oneDark : []),
          this.readonlyCompartment.of(EditorState.readOnly.of(readonly)),
          autocompletion(),
          closeBrackets(),
          search(),
          keymap.of([...closeBracketsKeymap, ...searchKeymap, indentWithTab]),
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

  set diagnostics(diags) {
    if (!this.view || !Array.isArray(diags)) return;
    const mapped = diags
      .map((d) => ({
        from: this._toOffset(d.from),
        to: d.to != null ? this._toOffset(d.to) : null,
        severity: d.severity,
      }))
      .filter((d) => d.from !== null);
    this.view.dispatch({ effects: setDiagnostics.of(mapped) });
  }

  set cursor(pos) {
    if (!this.view) return;
    const offset = this._toOffset(pos);
    if (offset === null) return;
    this.view.dispatch({
      selection: { anchor: offset },
      scrollIntoView: true,
    });
    this.view.focus();
  }

  set selection(sel) {
    if (!this.view || !sel) return;
    const from = this._toOffset(sel.from);
    const to = this._toOffset(sel.to ?? sel.from);
    if (from === null || to === null) return;
    this.view.dispatch({
      selection: { anchor: from, head: to },
      scrollIntoView: true,
    });
    this.view.focus();
  }

  _toOffset(pos) {
    if (typeof pos === "number") {
      return Math.min(Math.max(0, pos), this.view.state.doc.length);
    }
    if (pos && typeof pos.line === "number") {
      const line = Math.min(Math.max(1, pos.line), this.view.state.doc.lines);
      const lineObj = this.view.state.doc.line(line);
      const col = Math.min(Math.max(0, pos.col ?? 0), lineObj.length);
      return lineObj.from + col;
    }
    return null;
  }
}

customElements.define("code-mirror", CodeMirrorEditor);

// Export for external use
export { CodeMirrorEditor };
export { LanguageSupport, LRLanguage } from "@codemirror/language";
export { styleTags, tags } from "@lezer/highlight";
export { LRParser } from "@lezer/lr";
