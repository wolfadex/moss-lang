package main

import "core:fmt"
import "core:strconv"
import "core:os"
import "core:strings"
import "core:path/filepath"

Value_Kind :: enum {
    String,
    Number,
    List,
}

Value :: struct {
    kind: Value_Kind,
    #partial
    union {
        string,
        int,
        []Ast_Node,
    },
}

Stack :: struct {
    values: [dynamic]Value,
}

push :: proc(stack: ^Stack, value: Value) {
    append(&stack.values, value)
}

pop :: proc(stack: ^Stack) -> (Value, bool) {
    if len(stack.values) == 0 {
        return {}, false
    }
    value := stack.values[len(stack.values)-1]
    resize(&stack.values, len(stack.values)-1)
    return value, true
}

Native_Word :: proc(interp: ^Interpreter)

Word :: union {
    Native_Word,
    []Ast_Node,
}

Interpreter :: struct {
    stack: Stack,
    words: map[string]Word,
}

make_interpreter :: proc() -> Interpreter {
    interp: Interpreter
    interp.words = make(map[string]Word)
    interp.words["**builtin__io_print**"] = Native_Word(io_print)
    interp.words["**builtin__string_fromInt**"] = Native_Word(str_from_number)
    interp.words["**builtin__list_foldl**"] = Native_Word(list_foldl)
    interp.words["**builtin__string_concat**"] = Native_Word(string_concat)
    interp.words["+"] = Native_Word(add)
    return interp
}

//...

string_concat :: proc(interp: ^Interpreter) {
    list_val, ok := pop(&interp.stack)
    if !ok {
        fmt.Println("Stack underflow in string.concat")
        return
    }
    if list_val.kind != .List {
        fmt.Println("Type error in string.concat: expected a list")
        return
    }

    // Evaluate the list
    temp_stack: Stack
    temp_interp := Interpreter{stack = temp_stack, words = interp.words}
    eval(&temp_interp, list_val.([]Ast_Node))

    var strings_to_concat []string
    for v in temp_interp.stack.values {
        if v.kind == .String {
            append(&strings_to_concat, v.string)
        } else {
            // Should we handle this error differently?
            fmt.Println("Type error in string.concat: list elements must be strings")
            return
        }
    }

    result := strings.concatenate(strings_to_concat)
    push(&interp.stack, Value{kind = .String, string = result})
}

run :: proc(interp: ^Interpreter, nodes: [dynamic]Ast_Node, script_path: string) {
    main_word: []Ast_Node
    base_path := filepath.dir(script_path)

    for node in nodes {
        if node.kind == .Definition {
            if node.text == "main" {
                main_word = node.children[:]
            } else {
                interp.words[node.text] = node.children[:]
            }
        } else if node.kind == .Import {
            path := node.text
            if strings.has_prefix(path, "file://") {
                path = strings.trim_prefix(path, "file://")
            }

            full_path := filepath.join({base_path, path})

            source_bytes, ok := os.read_entire_file(full_path)
            if !ok {
                fmt.printf("Failed to read import: %s\n", full_path)
                continue
            }
            source := string(source_bytes)

            lexer := make_lexer(source)
            parser := make_parser(&lexer)
            imported_nodes := parse_program(&parser)

            prefix := node.alias
            if prefix == "" {
                prefix = filepath.stem(path)
            }

            for imported_node in imported_nodes {
                if imported_node.kind == .Definition {
                    word_name := prefix + "." + imported_node.text
                    // Check for builtin
                    if len(imported_node.children) == 1 && imported_node.children[0].kind == .Word && strings.has_prefix(imported_node.children[0].text, "**builtin__") {
                        builtin_name := imported_node.children[0].text
                        if builtin, ok := interp.words[builtin_name]; ok {
                            interp.words[word_name] = builtin
                        } else {
                            fmt.printf("Unknown builtin: %s\n", builtin_name)
                        }
                    } else {
                        interp.words[word_name] = imported_node.children[:]
                    }
                }
            }
        }
    }

    if main_word != nil {
        eval(interp, main_word)
    }
}

eval :: proc(interp: ^Interpreter, nodes: []Ast_Node) {
    for node in nodes {
        switch node.kind {
        case .String:
            push(&interp.stack, Value{kind = .String, string = node.text})
        case .Number:
            num, err := strconv.parse_int(node.text)
            if err == nil {
                push(&interp.stack, Value{kind = .Number, int = num})
            } else {
                fmt.printf("Invalid number: %s\n", node.text)
            }
        case .List:
            push(&interp.stack, Value{kind = .List, []Ast_Node = node.children[:]})
        case .Word:
            if word, ok := interp.words[node.text]; ok {
                switch w in word {
                case Native_Word:
                    w(interp)
                case []Ast_Node:
                    eval(interp, w)
                }
            } else {
                fmt.printf("Unknown word: %s\n", node.text)
            }
        case .Definition:
            // Already handled
        }
    }
}

print_value :: proc(value: Value) {

    switch value.kind {

    case .String:

        fmt.printf("\"%s\"", value.string)

    case .Number:

        fmt.printf("%d", value.int)

    case .List:

        fmt.printf("[")

        for node, i in value.([]Ast_Node) {

            switch node.kind {

            case .String:

                print_value(Value{kind = .String, string = node.text})

            case .Number:

                // This is not ideal, as it doesn't handle parse errors

                num, _ := strconv.parse_int(node.text)

                print_value(Value{kind = .Number, int = num})

            case .List:

                print_value(Value{kind = .List, []Ast_Node = node.children[:]})

            case .Word:

                fmt.printf("%s", node.text)

            }



            if i < len(value.([]Ast_Node))-1 {

                fmt.printf(" ")

            }

        }

        fmt.printf("]")

    }

}
io_print :: proc(interp: ^Interpreter) {
    value, ok := pop(&interp.stack)
    if !ok {
        fmt.Println("Stack underflow in io.print")
        return
    }
    print_value(value)
    fmt.println()
}

add :: proc(interp: ^Interpreter) {
    b, ok_b := pop(&interp.stack)
    a, ok_a := pop(&interp.stack)
    if !ok_a || !ok_b {
        fmt.Println("Stack underflow in +")
        return
    }
    if a.kind != .Number || b.kind != .Number {
        fmt.Println("Type error in +: expected two numbers")
        return
    }
    push(&interp.stack, Value{kind = .Number, int = a.int + b.int})
}

str_from_number :: proc(interp: ^Interpreter) {
    value, ok := pop(&interp.stack)
    if !ok {
        fmt.Println("Stack underflow in str.fromNumber")
        return
    }
    if value.kind != .Number {
        fmt.Println("Type error in str.fromNumber: expected a number")
        return
    }
    s := strconv.format_int(value.int)
    push(&interp.stack, Value{kind = .String, string = s})
}

list_foldl :: proc(interp: ^Interpreter) {
    quotation, ok_q := pop(&interp.stack)
    initial, ok_i := pop(&interp.stack)
    list, ok_l := pop(&interp.stack)

    if !ok_q || !ok_i || !ok_l {
        fmt.Println("Stack underflow in foldl")
        return
    }

    if list.kind != .List {
        fmt.Println("Type error in foldl: expected a list")
        return
    }

    if quotation.kind != .List {
        fmt.Println("Type error in foldl: expected a quotation")
        return
    }

    acc := initial
    for v_node in list.([]Ast_Node) {
        // push accumulator
        push(&interp.stack, acc)
        // push element
        eval(interp, []Ast_Node{v_node})
        // eval quotation
        eval(interp, quotation.([]Ast_Node))
        // pop result
        acc, _ = pop(&interp.stack)
    }
    push(&interp.stack, acc)
}
