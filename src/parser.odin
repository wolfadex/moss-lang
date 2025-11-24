package main

Parser :: struct {
	lexer:         ^Lexer,
	current_token: Token,
	peek_token:    Token,
}

make_parser :: proc(lexer: ^Lexer) -> Parser {
	p: Parser
	p.lexer = lexer
	p.current_token = next_token(lexer)
	p.peek_token = next_token(lexer)
	return p
}

next_token_parser :: proc(p: ^Parser) {
	p.current_token = p.peek_token
	p.peek_token = next_token(p.lexer)
}

parse_program :: proc(p: ^Parser) -> [dynamic]Ast_Node {
	nodes: [dynamic]Ast_Node
	for p.current_token.kind != .EOF {
		node: Ast_Node
		if p.current_token.kind == .Path {
			node.kind = .Import
			node.text = p.current_token.text
			next_token_parser(p) // Consume path

			if p.current_token.kind == .String {
				node.alias = p.current_token.text
				next_token_parser(p) // Consume alias
			}

			if p.current_token.kind == .Keyword_Use {
				next_token_parser(p) // Consume use
			} else {
				// Handle error: expected 'use' keyword
			}
		} else if p.current_token.kind == .Word &&
		   p.peek_token.kind == .Symbol &&
		   p.peek_token.text == ":" {
			node.kind = .Definition
			node.text = p.current_token.text
			next_token_parser(p) // Consume word
			next_token_parser(p) // Consume colon

			for p.current_token.kind != .EOF {
				child := parse_expression(p)
				append(&node.children, child)
				next_token_parser(p)
			}
		} else {
			node = parse_expression(p)
			next_token_parser(p)
		}
		append(&nodes, node)
	}
	return nodes
}

parse_expression :: proc(p: ^Parser) -> Ast_Node {
	switch p.current_token.kind {
	case .Word:
		return Ast_Node{kind = .Word, text = p.current_token.text}
	case .String:
		return Ast_Node{kind = .String, text = p.current_token.text}
	case .Number:
		return Ast_Node{kind = .Number, text = p.current_token.text}
	case .Symbol:
		if p.current_token.text == "[" {
			node: Ast_Node
			node.kind = .List
			next_token_parser(p) // Consume [
			for p.current_token.kind != .Symbol || p.current_token.text != "]" {
				child := parse_expression(p)
				append(&node.children, child)
				next_token_parser(p)
			}
			return node
		}
	}
	// Should not be reached
	return Ast_Node{}
}
