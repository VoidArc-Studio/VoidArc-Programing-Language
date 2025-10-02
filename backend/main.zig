const std = @import("std");

const TokenType = enum {
    Let, Const, Fn, Return, If, Else, While, For, In, Struct, Enum, Use, Pub,
    Ident, Number, StringLit, BoolLit, LParen, RParen, LBrace, RBrace, Colon, Semi, Arrow, Comma, Dot,
    Plus, Minus, Star, Slash, Eq, EqEq, Lt, Gt, LtEq, GtEq, Bang, BangEq,
    Eof, Error,
};

const Token = struct {
    typ: TokenType,
    lexeme: []const u8,
    line: usize,
    column: usize,
};

const Type = enum {
    I32, U32, F32, String, Bool, Void, Struct, Enum,
    // Więcej...
};

const AstNode = union(enum) {
    VarDecl: struct { name: []const u8, typ: Type, init: ?*AstNode },
    FnDecl: struct { name: []const u8, params: []Param, retType: Type, body: []AstNode },
    ExprStmt: *AstNode,
    ReturnStmt: *AstNode,
    IfStmt: struct { cond: *AstNode, then: []AstNode, els: ?[]AstNode },
    // Więcej...
    BinaryExpr: struct { left: *AstNode, op: TokenType, right: *AstNode },
    Literal: struct { typ: Type, value: []const u8 },
    Ident: []const u8,
};

const Param = struct { name: []const u8, typ: Type };

const Parser = struct {
    tokens: []Token,
    current: usize = 0,
    allocator: std.mem.Allocator,
    hadError: bool = false,
    symbols: std.StringHashMap(Type), // Prosty symbol table

    fn init(allocator: std.mem.Allocator, tokens: []Token) Parser {
        return .{ .tokens = tokens, .allocator = allocator, .symbols = std.StringHashMap(Type).init(allocator) };
    }

    fn deinit(self: *Parser) void {
        self.symbols.deinit();
    }

    fn parse(self: *Parser) ![]AstNode {
        var stmts = std.ArrayList(AstNode).init(self.allocator);
        defer stmts.deinit();

        while (!self.isAtEnd()) {
            try stmts.append(try self.declaration());
        }

        return stmts.toOwnedSlice();
    }

    fn declaration(self: *Parser) !AstNode {
        if (self.match(.Use)) {
            // Parse use, ale pomijamy dla prostoty
            self.consume(.Ident, "Oczekiwano nazwy modułu");
            self.consume(.Colon, "Oczekiwano ':'");
            self.consume(.Colon, "Oczekiwano '::'");
            self.consume(.Ident, "Oczekiwano identyfikatora");
            self.consume(.Semi, "Oczekiwano ';'");
            return AstNode{ .ExprStmt = self.allocator.alloc(AstNode, AstNode{ .Literal = .{ .typ = .Void, .value = "" } }) orelse unreachable }; // Placeholder
        } else if (self.match(.Struct)) {
            // Parse struct
            self.consume(.Ident, "Oczekiwano nazwy struktury");
            self.consume(.LBrace, "Oczekiwano '{'");
            while (!self.check(.RBrace)) {
                self.consume(.Ident, "Oczekiwano pola");
                self.consume(.Colon, "Oczekiwano ':'");
                _ = self.parseType();
                if (!self.check(.RBrace)) self.consume(.Comma, "Oczekiwano ','");
            }
            self.consume(.RBrace, "Oczekiwano '}'");
            return AstNode{ .ExprStmt = self.allocator.alloc(AstNode, AstNode{ .Literal = .{ .typ = .Void, .value = "" } }) orelse unreachable };
        } else if (self.match(.Fn)) {
            return self.fnDeclaration();
        } else {
            return self.statement();
        }
    }

    fn fnDeclaration(self: *Parser) !AstNode {
        const nameTok = self.consume(.Ident, "Oczekiwano nazwy funkcji");
        self.consume(.LParen, "Oczekiwano '('");
        var params = std.ArrayList(Param).init(self.allocator);
        defer params.deinit();
        while (!self.check(.RParen)) {
            const paramName = self.consume(.Ident, "Oczekiwano nazwy parametru");
            self.consume(.Colon, "Oczekiwano ':'");
            const paramType = self.parseType();
            try params.append(.{ .name = paramName.lexeme, .typ = paramType });
            if (!self.check(.RParen)) self.consume(.Comma, "Oczekiwano ','");
        }
        self.consume(.RParen, "Oczekiwano ')'");
        var retType = Type.Void;
        if (self.match(.Arrow)) {
            retType = self.parseType();
        }
        self.consume(.LBrace, "Oczekiwano '{'");
        var body = std.ArrayList(AstNode).init(self.allocator);
        defer body.deinit();
        while (!self.check(.RBrace) and !self.isAtEnd()) {
            try body.append(try self.statement());
        }
        self.consume(.RBrace, "Oczekiwano '}'");
        return AstNode{ .FnDecl = .{ .name = nameTok.lexeme, .params = try params.toOwnedSlice(), .retType = retType, .body = try body.toOwnedSlice() } };
    }

    fn statement(self: *Parser) !AstNode {
        if (self.match(.If)) {
            const cond = try self.allocator.alloc(AstNode, try self.expression());
            self.consume(.LBrace, "Oczekiwano '{' po if");
            var thenBranch = std.ArrayList(AstNode).init(self.allocator);
            defer thenBranch.deinit();
            while (!self.check(.RBrace)) {
                try thenBranch.append(try self.statement());
            }
            self.consume(.RBrace, "Oczekiwano '}'");
            var elseBranch: ?[]AstNode = null;
            if (self.match(.Else)) {
                self.consume(.LBrace, "Oczekiwano '{' po else");
                var elseStmts = std.ArrayList(AstNode).init(self.allocator);
                while (!self.check(.RBrace)) {
                    try elseStmts.append(try self.statement());
                }
                self.consume(.RBrace, "Oczekiwano '}'");
                elseBranch = try elseStmts.toOwnedSlice();
            }
            return AstNode{ .IfStmt = .{ .cond = cond, .then = try thenBranch.toOwnedSlice(), .els = elseBranch } };
        } else if (self.match(.Return)) {
            const expr = try self.expression();
            self.consume(.Semi, "Oczekiwano ';' po return");
            return AstNode{ .ReturnStmt = try self.allocator.alloc(AstNode, expr) };
        } else if (self.match(.Let) or self.match(.Const)) {
            const name = self.consume(.Ident, "Oczekiwano nazwy zmiennej");
            self.consume(.Colon, "Oczekiwano ':' dla typu");
            const typ = self.parseType();
            var init: ?*AstNode = null;
            if (self.match(.Eq)) {
                const expr = try self.expression();
                init = try self.allocator.alloc(AstNode, expr);
            }
            self.consume(.Semi, "Oczekiwano ';'");
            try self.symbols.put(name.lexeme, typ);
            return AstNode{ .VarDecl = .{ .name = name.lexeme, .typ = typ, .init = init } };
        } else {
            const expr = try self.expression();
            self.consume(.Semi, "Oczekiwano ';' po wyrażeniu");
            return AstNode{ .ExprStmt = try self.allocator.alloc(AstNode, expr) };
        }
    }

    fn expression(self: *Parser) !AstNode {
        return self.binary();
    }

    fn binary(self: *Parser) !AstNode {
        var expr = try self.primary();
        while (self.match(.Plus) or self.match(.Minus) or self.match(.Star) or self.match(.Slash) or self.match(.EqEq) or self.match(.BangEq) or self.match(.Lt) or self.match(.Gt) or self.match(.LtEq) or self.match(.GtEq)) {
            const op = self.previous().typ;
            const right = try self.primary();
            const leftPtr = try self.allocator.alloc(AstNode, expr);
            const rightPtr = try self.allocator.alloc(AstNode, right);
            expr = AstNode{ .BinaryExpr = .{ .left = leftPtr, .op = op, .right = rightPtr } };
        }
        return expr;
    }

    fn primary(self: *Parser) !AstNode {
        if (self.match(.Number)) {
            return AstNode{ .Literal = .{ .typ = .I32, .value = self.previous().lexeme } };
        } else if (self.match(.StringLit)) {
            return AstNode{ .Literal = .{ .typ = .String, .value = self.previous().lexeme } };
        } else if (self.match(.Ident)) {
            return AstNode{ .Ident = self.previous().lexeme };
        } else {
            self.errorAtCurrent("Nieoczekiwane wyrażenie");
            return AstNode{ .Literal = .{ .typ = .Void, .value = "" } };
        }
    }

    fn parseType(self: *Parser) Type {
        const tok = self.consume(.Ident, "Oczekiwano typu");
        return switch (tok.lexeme) {
            "i32" => .I32,
            "u32" => .U32,
            "f32" => .F32,
            "string" => .String,
            "bool" => .Bool,
            else => blk: {
                self.errorAt(tok, "Nieznany typ");
                break :blk .Void;
            },
        };
    }

    fn typeCheck(self: *Parser, node: AstNode) void {
        // Prosty type check
        switch (node) {
            .VarDecl => |vd| {
                if (vd.init) |init| {
                    const initType = self.inferType(init.*);
                    if (initType != vd.typ) {
                        self.hadError = true;
                        std.debug.print("Error: Niezgodność typów w deklaracji {s}\n", .{vd.name});
                    }
                }
            },
            .BinaryExpr => |be| {
                const leftType = self.inferType(be.left.*);
                const rightType = self.inferType(be.right.*);
                if (leftType != rightType) {
                    self.hadError = true;
                    std.debug.print("Error: Niezgodność typów w operacji binarnej\n", .{});
                }
            },
            // Dodaj więcej
            else => {},
        }
    }

    fn inferType(self: *Parser, node: AstNode) Type {
        _ = self;
        switch (node) {
            .Literal => |lit| return lit.typ,
            .Ident => |id| return self.symbols.get(id) orelse .Void,
            // Dodaj więcej
            else => return .Void,
        }
    }

    // Pomocnicze funkcje
    fn match(self: *Parser, typ: TokenType) bool {
        if (self.check(typ)) {
            _ = self.advance();
            return true;
        }
        return false;
    }

    fn check(self: *Parser, typ: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.tokens[self.current].typ == typ;
    }

    fn advance(self: *Parser) Token {
        const tok = self.tokens[self.current];
        self.current += 1;
        return tok;
    }

    fn previous(self: *Parser) Token {
        return self.tokens[self.current - 1];
    }

    fn consume(self: *Parser, typ: TokenType, msg: []const u8) Token {
        if (self.check(typ)) {
            return self.advance();
        } else {
            self.errorAtCurrent(msg);
            return .{ .typ = .Error, .lexeme = "", .line = 0, .column = 0 };
        }
    }

    fn isAtEnd(self: *Parser) bool {
        return self.current >= self.tokens.len or self.tokens[self.current].typ == .Eof;
    }

    fn errorAtCurrent(self: *Parser, msg: []const u8) void {
        const tok = self.tokens[self.current];
        std.debug.print("Error: Linia {} Kol {}: {s}. Sugestia: Sprawdź składnię blisko '{s}'\n", .{tok.line, tok.column, msg, tok.lexeme});
        self.hadError = true;
    }

    fn errorAt(self: *Parser, tok: Token, msg: []const u8) void {
        std.debug.print("Error: Linia {} Kol {}: {s}. Sugestia: Sprawdź '{s}'\n", .{tok.line, tok.column, msg, tok.lexeme});
        self.hadError = true;
    }
};

fn lex(source: []const u8, allocator: std.mem.Allocator) ![]Token {
    var tokens = std.ArrayList(Token).init(allocator);
    var line: usize = 1;
    var column: usize = 1;
    var i: usize = 0;

    while (i < source.len) {
        const c = source[i];
        switch (c) {
            ' ', '\t', '\r' => {},
            '\n' => { line += 1; column = 1; },
            '(' => try tokens.append(.{ .typ = .LParen, .lexeme = "(", .line = line, .column = column }),
            ')' => try tokens.append(.{ .typ = .RParen, .lexeme = ")", .line = line, .column = column }),
            '{' => try tokens.append(.{ .typ = .LBrace, .lexeme = "{", .line = line, .column = column }),
            '}' => try tokens.append(.{ .typ = .RBrace, .lexeme = "}", .line = line, .column = column }),
            ':' => try tokens.append(.{ .typ = .Colon, .lexeme = ":", .line = line, .column = column }),
            ';' => try tokens.append(.{ .typ = .Semi, .lexeme = ";", .line = line, .column = column }),
            ',' => try tokens.append(.{ .typ = .Comma, .lexeme = ",", .line = line, .column = column }),
            '.' => try tokens.append(.{ .typ = .Dot, .lexeme = ".", .line = line, .column = column }),
            '+' => try tokens.append(.{ .typ = .Plus, .lexeme = "+", .line = line, .column = column }),
            '-' => {
                if (i + 1 < source.len and source[i+1] == '>') {
                    i += 1;
                    try tokens.append(.{ .typ = .Arrow, .lexeme = "->", .line = line, .column = column });
                } else {
                    try tokens.append(.{ .typ = .Minus, .lexeme = "-", .line = line, .column = column });
                }
            },
            '*' => try tokens.append(.{ .typ = .Star, .lexeme = "*", .line = line, .column = column }),
            '/' => try tokens.append(.{ .typ = .Slash, .lexeme = "/", .line = line, .column = column }),
            '=' => {
                if (i + 1 < source.len and source[i+1] == '=') {
                    i += 1;
                    try tokens.append(.{ .typ = .EqEq, .lexeme = "==", .line = line, .column = column });
                } else {
                    try tokens.append(.{ .typ = .Eq, .lexeme = "=", .line = line, .column = column });
                }
            },
            '<' => {
                if (i + 1 < source.len and source[i+1] == '=') {
                    i += 1;
                    try tokens.append(.{ .typ = .LtEq, .lexeme = "<=", .line = line, .column = column });
                } else {
                    try tokens.append(.{ .typ = .Lt, .lexeme = "<", .line = line, .column = column });
                }
            },
            '>' => {
                if (i + 1 < source.len and source[i+1] == '=') {
                    i += 1;
                    try tokens.append(.{ .typ = .GtEq, .lexeme = ">=", .line = line, .column = column });
                } else {
                    try tokens.append(.{ .typ = .Gt, .lexeme = ">", .line = line, .column = column });
                }
            },
            '!' => {
                if (i + 1 < source.len and source[i+1] == '=') {
                    i += 1;
                    try tokens.append(.{ .typ = .BangEq, .lexeme = "!=", .line = line, .column = column });
                } else {
                    try tokens.append(.{ .typ = .Bang, .lexeme = "!", .line = line, .column = column });
                }
            },
            'a'...'z', 'A'...'Z', '_' => {
                const start = i;
                while (i < source.len and (std.ascii.isAlphanumeric(source[i]) or source[i] == '_')) i += 1;
                const lex = source[start..i];
                const typ = keywordType(lex) orelse .Ident;
                try tokens.append(.{ .typ = typ, .lexeme = lex, .line = line, .column = column });
                i -= 1;
            },
            '0'...'9' => {
                const start = i;
                while (i < source.len and std.ascii.isDigit(source[i])) i += 1;
                try tokens.append(.{ .typ = .Number, .lexeme = source[start..i], .line = line, .column = column });
                i -= 1;
            },
            '"' => {
                i += 1;
                const start = i;
                while (i < source.len and source[i] != '"') i += 1;
                if (i >= source.len) {
                    std.debug.print("Error: Niezamknięty string linia {}\n", .{line});
                    try tokens.append(.{ .typ = .Error, .lexeme = "", .line = line, .column = column });
                } else {
                    try tokens.append(.{ .typ = .StringLit, .lexeme = source[start..i], .line = line, .column = column });
                }
            },
            else => {
                std.debug.print("Error: Nieoczekiwany znak '{c}' linia {} kol {}\n", .{c, line, column});
                try tokens.append(.{ .typ = .Error, .lexeme = &.{c}, .line = line, .column = column });
            },
        }
        i += 1;
        column += 1;
    }
    try tokens.append(.{ .typ = .Eof, .lexeme = "", .line = line, .column = column });
    return tokens.toOwnedSlice();
}

fn keywordType(lex: []const u8) ?TokenType {
    const keywords = .{
        "let" => .Let, "const" => .Const, "fn" => .Fn, "return" => .Return, "if" => .If, "else" => .Else,
        "while" => .While, "for" => .For, "in" => .In, "struct" => .Struct, "enum" => .Enum, "use" => .Use, "pub" => .Pub,
    };
    inline for (std.meta.fields(@TypeOf(keywords))) |field| {
        if (std.mem.eql(u8, lex, field.name)) return @field(keywords, field.name);
    }
    return null;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 3) {
        std.debug.print("Użycie: voidarc-backend check <pliki...>\n", .{});
        std.process.exit(1);
    }

    for (args[2..]) |file| {
        const source = try std.fs.cwd().readFileAlloc(allocator, file, 1_000_000);
        defer allocator.free(source);

        const tokens = try lex(source, allocator);
        defer allocator.free(tokens);

        var parser = Parser.init(allocator, tokens);
        defer parser.deinit();

        const ast = try parser.parse();
        defer {
            // Zwolnij AST - pomijamy dla prostoty
        }

        for (ast) |node| {
            parser.typeCheck(node);
        }

        if (parser.hadError) {
            std.process.exit(1);
        }
    }
    std.debug.print("Sprawdzenie OK\n", .{});
}
