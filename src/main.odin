package main

import "core:fmt"
import "core:os"

main :: proc() {
	source, ok := os.read_entire_file("examples/hello.qd")
	if !ok {
		fmt.println("Failed to read examples/hello.qd")
		return
	}

	lexer := make_lexer(string(source))

	parser := make_parser(&lexer)

	nodes := parse_program(&parser)


	interpreter := make_interpreter()

	run(&interpreter, nodes, "examples/hello.qd")}
