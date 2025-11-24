package main

Ast_Node_Kind :: enum {
	Word,
	String,
	Number,
	List,
	Lambda,
	Definition,
	Import,
}

Ast_Node :: struct {
	kind:     Ast_Node_Kind,
	text:     string,
	alias:    string,
	children: [dynamic]Ast_Node,
}
