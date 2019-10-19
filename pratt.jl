const LEFT_PAREN  = Val{:LEFT_PAREN}
const RIGHT_PAREN = Val{:RIGHT_PAREN} 
const PLUS        = Val{:PLUS}
const MINUS       = Val{:MINUS}
const ASTERISK    = Val{:ASTERISK}
const SLASH       = Val{:SLASH}
const CARET       = Val{:CARET}
const LITERAL     = Val{:LITERAL}
const INT_VAL     = Val{:INT_VAL}

const TokenType = Union{
	Type{LEFT_PAREN},
	Type{RIGHT_PAREN},
	Type{PLUS},
	Type{MINUS},
	Type{ASTERISK},
	Type{SLASH},
	Type{CARET},
	Type{LITERAL},
	Type{INT_VAL}
}

@enum Precedences begin
	NONE        = 0
	SUM         = 10
	PRODUCT     = 20
	EXPONENT_R  = 29
	EXPONENT    = 30
	PREFIX      = 40
end

struct Token{TokenType}
	text::String
end

# left binding powers

lbp(token::Union{Token{PLUS}, Token{MINUS}})             = SUM::Precedences
lbp(token::Union{Token{ASTERISK}, Token{SLASH}})         = PRODUCT::Precedences
lbp(token::Token{CARET})                                 = EXPONENT::Precedences
lbp(token::Union{Token{LEFT_PAREN}, Token{RIGHT_PAREN}}) = NONE::Precedences

# null denotations

nud(token::Token,          itr) = error("unexpected token: ", token)
nud(token::Token{PLUS},    itr) = expression(itr, PREFIX::Precedences)
nud(token::Token{MINUS},   itr) = Expr(:call, :-, expression(itr, PREFIX::Precedences))
nud(token::Token{INT_VAL}, itr) = parse(Int, token.text)
nud(token::Token{LITERAL}, itr) = Symbol(token.text)

function nud(token::Token{LEFT_PAREN}, itr)
	expr = expression(itr, NONE::Precedences)
	lookahead = popfirst!(itr)
	
	if typeof(lookahead) != Token{RIGHT_PAREN}
		error(Token(RIGHT_PAREN, ")"), " expected, got: ", lookahead)
	end

	expr
end

# left denotations

led(token::Token,           left, itr) = error("unexpected token: ", token)
led(token::Token{PLUS},     left, itr) = Expr(:call, :+, left, expression(itr, SUM::Precedences))
led(token::Token{MINUS},    left, itr) = Expr(:call, :-, left, expression(itr, SUM::Precedences))
led(token::Token{ASTERISK}, left, itr) = Expr(:call, :*, left, expression(itr, PRODUCT::Precedences))
led(token::Token{SLASH},    left, itr) = Expr(:call, :/, left, expression(itr, PRODUCT::Precedences))
led(token::Token{CARET},    left, itr) = Expr(:call, :^, left, expression(itr, EXPONENT_R::Precedences))

# main parser

function expression(itr, rbp = NONE::Precedences)
	token = popfirst!(itr)
	left = nud(token, itr)

	while Base.peek(itr) != nothing && rbp < lbp(Base.peek(itr))
		token = popfirst!(itr)
		left = led(token, left, itr)
	end

	left
end

# dummy token stream

struct TokenStream
	token_stream::Vector{Token}
end

Base.iterate(stream::TokenStream, count = 1) = count > size(stream.token_stream, 1) ? nothing : (stream.token_stream[count], count + 1)

stream = TokenStream(
		[
			Token{LITERAL}("A"),
			Token{ASTERISK}("*"),
			Token{LEFT_PAREN}("("),
			Token{INT_VAL}("1"),
			Token{PLUS}("+"),
			Token{INT_VAL}("2"),
			Token{CARET}("^"),
			Token{INT_VAL}("3"),
			Token{SLASH}("/"),
			Token{INT_VAL}("3"),
			Token{RIGHT_PAREN}(")")
		]
	)

itr = Iterators.Stateful(stream)

expr = expression(itr)

println("result: ", expr)
dump(expr)
