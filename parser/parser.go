package parser

import (
	"fmt"
	"monkey/ast"
	"monkey/lexer"
	"monkey/token"
)

type Parser struct {
	lex       *lexer.Lexer
	errors    []string
	curToken  token.Token
	peekToken token.Token
}

func New(lex *lexer.Lexer) *Parser {
	parser := &Parser{lex: lex, errors: []string{}}

	// Read two tokens, so curToken and peekToken are both set
	parser.nextToken()
	parser.nextToken()

	return parser
}

func (parser *Parser) nextToken() {
	parser.curToken = parser.peekToken
	parser.peekToken = parser.lex.NextToken()
}

func (parser *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for !parser.curTokenIs(token.EOF) {
		stmt := parser.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		parser.nextToken()
	}
	return program
}

func (parser *Parser) parseStatement() ast.Statement {
	switch parser.curToken.Type {
	case token.LET:
		return parser.parseLetStatement()
	case token.RETURN:
		return parser.parseReturnStatement()
	default:
		return nil
	}
}

func (parser *Parser) parseLetStatement() ast.Statement {
	stmt := &ast.LetStatement{Token: parser.curToken}

	if !parser.expectPeek(token.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{Token: parser.curToken, Value: parser.curToken.Literal}

	if !parser.expectPeek(token.ASSIGN) {
		return nil
	}

	for !parser.curTokenIs(token.SEMICOLON) {
		parser.nextToken()
	}

	return stmt
}

func (parser *Parser) parseReturnStatement() ast.Statement {
	stmt := &ast.ReturnStatement{Token: parser.curToken}

	parser.nextToken()
	for !parser.curTokenIs(token.SEMICOLON) {
		parser.nextToken()
	}

	return stmt
}

func (parser *Parser) curTokenIs(t token.TokenType) bool {
	return parser.curToken.Type == t
}

func (parser *Parser) peekTokenIs(t token.TokenType) bool {
	return parser.peekToken.Type == t
}

func (parser *Parser) expectPeek(t token.TokenType) bool {
	if parser.peekTokenIs(t) {
		parser.nextToken()
		return true
	}
	parser.peekError(t)
	return false
}

func (parser *Parser) Errors() []string {
	return parser.errors
}

func (parser *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead", t, parser.peekToken.Type)
	parser.errors = append(parser.errors, msg)
}
