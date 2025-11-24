package main

import "core:strings"

Lexer :: struct {
	source:  string,
	current: int,
	line:    int,
	column:  int,
}

make_lexer :: proc(source: string) -> Lexer {
	return Lexer{source = source, current = 0, line = 1, column = 1}
}

next_token :: proc(lexer: ^Lexer) -> Token {
	for {
		if lexer.current >= len(lexer.source) {
			return Token{kind = .EOF, text = ""}
		}

		c := lexer.source[lexer.current]
		switch c {
		case ' ', '\t', '\r':
			lexer.current += 1
			lexer.column += 1
		case '\n':
			lexer.current += 1
			lexer.line += 1
			lexer.column = 1
		case '#':
			for lexer.current < len(lexer.source) && lexer.source[lexer.current] != '\n' {
				lexer.current += 1
			}
		default:
			goto parse_token
		}
	}

parse_token:
	start := lexer.current
	c := lexer.source[lexer.current]

	switch {
	case c == '"':
		lexer.current += 1
		start = lexer.current
		for lexer.current < len(lexer.source) && lexer.source[lexer.current] != '"' {
			if lexer.source[lexer.current] == '\n' {
				lexer.line += 1
				lexer.column = 1
			} else {
				lexer.column += 1
			}
			lexer.current += 1
		}
		text := lexer.source[start:lexer.current]
		lexer.current += 1 // Skip closing quote
		return Token{kind = .String, text = text}
	case c >= '0' && c <= '9':
		for lexer.current < len(lexer.source) && lexer.source[lexer.current] >= '0' && lexer.source[lexer.current] <= '9' {
			lexer.current += 1
			lexer.column += 1
		}
		text := lexer.source[start:lexer.current]
		return Token{kind = .Number, text = text}
	case c == ':':
		fallthrough
	case c == '(':
		fallthrough
	case c == ')':
		fallthrough
	case c == '[':
		fallthrough
	case c == ']':
		fallthrough
	case c == '\\':
		fallthrough
	case c == ';':
		lexer.current += 1
		lexer.column += 1
		return Token{kind = .Symbol, text = lexer.source[start:lexer.current]}
	case c == '-':
		if lexer.current+1 < len(lexer.source) && lexer.source[lexer.current+1] == '>' {
			lexer.current += 2
			lexer.column += 2
			return Token{kind = .Symbol, text = "->"}
		}
	}

	for lexer.current < len(lexer.source) {
		c := lexer.source[lexer.current]
		if c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == ':' || c == '(' || c == ')' || c == '[' || c == ']' || c == '\\' || c == ';' {
			break
		}
		lexer.current += 1
		lexer.column += 1
	}

    text := lexer.source[start:lexer.current]
    kind := .Word

    if strings.contains(text, "://") {
        kind = .Path
    } else if text == "use" {
        kind = .Keyword_Use
    } else if text == "give" {
        kind = .Keyword_Give
    }

    return Token{kind = kind, text = text}
}
