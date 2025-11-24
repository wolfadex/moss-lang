package main

Token_Kind :: enum {
	Word,
	String,
	Number,
	Symbol,
	Path,
	Keyword_Use,
	Keyword_Give,
	EOF,
}

Token :: struct {
	kind: Token_Kind,
	text: string,
}
