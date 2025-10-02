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
};

const AstNode = union(enum) {
    VarDecl: struct { name: []const u8, typ: Type, init: ?*AstNode },
    FnDecl: struct { name: []const u8, params: []Param, retType: Type, body: []AstNode },
    ExprStmt: *AstNode,
    ReturnStmt: *AstNode,
    IfStmt: struct { cond: *AstNode, then: []AstNode, els: ?[]AstNode },
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
    symbols: std.StringHashMap(Type),

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
            _ = self.consume(.Ident, "Oczekiwano nazwy modułu");
            _ = self.consume(.Colon, "Oczekiwano ':'");
            _ = self.consume(.Colon, "Oczekiwano '::'");
            _ = self.consume(.Ident, "Oczekiwano identyfikatora");
            _ = self.consume(.Semi, "Oczekiwano ';'");
            return AstNode{ .ExprStmt = try self.allocator.create(AstNode) };
        } else if (self.match(.Struct)) {
            _ = self.consume(.Ident, "Oczekiwano nazwy struktury");
            _ = self.consume(.LBrace, "Oczekiwano '{'");
            while (!self.check(.RBrace) and !self.isAtEnd()) {
                _ = self.consume(.Ident, "Oczekiwano pola");
                _ = self.consume(.Colon, "Oczekiwano ':'");
                _ = self.parseType();
                if (!self.check(.RBrace)) _ = self.consume(.Comma, "Oczekiwano ','");
            }
            _ = self.consume(.RBrace, "Oczekiwano '}'");
            return AstNode{ .ExprStmt = try self.allocator.create(AstNode) };
        } else if (self.match(.Fn)) {
            return try self.fnDeclaration();
        } else {
            return try self.statement();
        }
    }

    fn fnDeclaration(self: *Parser) !AstNode {
        const nameTok = self.consume(.Ident, "Oczekiwano nazwy funkcji");
        _ = self.consume(.LParen, "Oczekiwano '('");
        var params = std.ArrayList(Param).init(self.allocator);
        defer params.deinit();
        while (!self.check(.RParen) and !self.isAtEnd()) {
            const paramName = self.consume(.Ident, "Oczekiwano nazwy parametru");
            _ = self.consume(.Colon, "Oczekiwano ':'");
            const paramType = self.parseType();
            try params.append(.{ .name = paramName.lexeme, .typ = paramType });
            if (!self.check(.RParen)) _ = self.consume(.Comma, "Oczekiwano ','");
        }
        _ = self.consume(.RParen, "Oczekiwano ')'");
        var retType = Type.Void;
        if (self.match(.Arrow)) {
            retType = self.parseType();
        }
        _ = self.consume(.LBrace, "Oczekiwano '{'");
        var body = std.ArrayList(AstNode).init(self.allocator);
        defer body.deinit();
        while (!self.check(.RBrace) and !self.isAtEnd()) {
            try body.append(try self.statement());
        }
        _ = self.consume(.RBrace, "Oczekiwano '}'");
        return AstNode{ .FnDecl = .{ .name = nameTok.lexeme, .params = try params.toOwnedSlice(), .retType = retType, .body = try body.toOwnedSlice() } };
    }

    fn statement(self: *Parser) !AstNode {
        if (self.match(.If)) {
            const cond = try self.allocator.create(AstNode);
            cond.* = try self.expression();
            _ = self.consume(.LBrace, "Oczekiwano '{' po if");
            var thenBranch = std.ArrayList(AstNode).init(self.allocator);
            defer thenBranch.deinit();
            while (!self.check(.RBrace) and !self.isAtEnd()) {
                try thenBranch.append(try self.statement());
            }
            _ = self.consume(.RBrace, "Oczekiwano '}'");
            var elseBranch: ?[]AstNode = null;
            if (self.match(.Else)) {
                _ = self.consume(.LBrace, "Oczekiwano '{' po else");
                var elseStmts = std.ArrayList(AstNode).init(self.allocator);
                while (!self.check(.RBrace) and !self.isAtEnd()) {
                    try elseStmts.append(try self.statement());
                }
                _ = self.consume(.RBrace, "Oczekiwano '}'");
                elseBranch = try elseStmts.toOwnedSlice();
            }
            return AstNode{ .IfStmt = .{ .cond = cond, .then = try thenBranch.toOwnedSlice(), .els = elseBranch } };
        } else if (self.match(.Return)) {
            const expr = try self.expression();
            _ = self.consume(.Semi, "Oczekiwano ';' po return");
            const exprPtr = try self.allocator.create(AstNode);
            exprPtr.* = expr;
            return AstNode{ .ReturnStmt = exprPtr };
        } else if (self.match(.Let) or self.match(.Const)) {
            const name = self.consume(.Ident, "Oczekiwano nazwy zmiennej");
            _ = self.consume(.Colon, "Oczekiwano ':' dla typu");
            const typ = self.parseType();
            var initExpr: ?*AstNode = null;
            if (self.match(.Eq)) {
                const expr = try self.expression();
                initExpr = try self.allocator.create(AstNode);
                initExpr.?.* = expr;
            }
            _ = self.consume(.Semi, "Oczekiwano ';'");
            try self.symbols.put(name.lexeme, typ);
            return AstNode{ .VarDecl = .{ .name = name.lexeme, .typ = typ, .init = initExpr } };
        } else {
            const expr = try self.expression();
            _ = self.consume(.Semi, "Oczekiwano ';' po wyrażeniu");
            const exprPtr = try self.allocator.create(AstNode);
            exprPtr.* = expr;
            return AstNode{ .ExprStmt = exprPtr };
        }
    }

    fn expression(self: *Parser) !AstNode {
        return try self.binary();
    }

    fn binary(self: *Parser) !AstNode {
        var expr = try self.primary();
        while (self.match(.Plus) or self.match(.Minus) or self.match(.Star) or self.match(.Slash) or
            self.match(.EqEq) or self.match(.BangEq) or self.match(.Lt) or self.match(.Gt) or
            self.match(.LtEq) or self.match(.GtEq)) {
            const op = self.previous().typ;
        const right = try self.primary();
        const leftPtr = try self.allocator.create(AstNode);
        const rightPtr = try self.allocator.create(AstNode);
        leftPtr.* = expr;
        rightPtr.* = right;
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
        const lexeme = tok.lexeme;
        if (std.mem.eql(u8, lexeme, "i32")) return .I32;
        if (std.mem.eql(u8, lexeme, "u32")) return .U32;
        if (std.mem.eql(u8, lexeme, "f32")) return .F32;
        if (std.mem.eql(u8, lexeme, "string")) return .String;
        if (std.mem.eql(u8, lexeme, "bool")) return .Bool;
        self.errorAt(tok, "Nieznany typ");
        return .Void;
    }

    fn typeCheck(self: *Parser, node: AstNode) void {
        switch (node) {
            .VarDecl => |vd| {
                if (vd.init) |initVal| {
                    const initType = self.inferType(initVal.*);
                    if (initType != vd.typ and initType != .Void and vd.typ != .Void) {
                        self.hadError = true;
                        std.debug.print("Error: Linia {}: Niezgodność typów w deklaracji {s}: oczekiwano {s}, otrzymano {s}\n", .{
                            self.tokens[self.current].line, vd.name, @tagName(vd.typ), @tagName(initType)
                        });
                        std.debug.print("Suggest: Sprawdź typ zmiennej lub inicjalizację\n", .{});
                    }
                }
            },
            .BinaryExpr => |be| {
                const leftType = self.inferType(be.left.*);
                const rightType = self.inferType(be.right.*);
                if (leftType != rightType and leftType != .Void and rightType != .Void) {
                    self.hadError = true;
                    std.debug.print("Error: Linia {}: Niezgodność typów w operacji {s}: lewa {s}, prawa {s}\n", .{
                        self.tokens[self.current].line, @tagName(be.op), @tagName(leftType), @tagName(rightType)
                    });
                    std.debug.print("Suggest: Upewnij się, że typy operandów są zgodne\n", .{});
                }
            },
            .FnDecl => |fd| {
                for (fd.body) |stmt| {
                    self.typeCheck(stmt);
                }
            },
            .IfStmt => |ifs| {
                self.typeCheck(ifs.cond.*);
                for (ifs.then) |stmt| {
                    self.typeCheck(stmt);
                }
                if (ifs.els) |els| {
                    for (els) |stmt| {
                        self.typeCheck(stmt);
                    }
                }
            },
            .ReturnStmt => |rs| {
                self.typeCheck(rs.*);
            },
            .ExprStmt => |es| {
                self.typeCheck(es.*);
            },
            else => {},
        }
    }

    fn inferType(self: *Parser, node: AstNode) Type {
        switch (node) {
            .Literal => |lit| return lit.typ,
            .Ident => |id| return self.symbols.get(id) orelse blk: {
                self.hadError = true;
                std.debug.print("Error: Linia {}: Niezdefiniowana zmienna {s}\n", .{self.tokens[self.current].line, id});
                std.debug.print("Suggest: Zadeklaruj zmienną przed użyciem\n", .{});
                break :blk .Void;
            },
            .BinaryExpr => |be| {
                const leftType = self.inferType(be.left.*);
                const rightType = self.inferType(be.right.*);
                if (leftType == rightType) return leftType;
                return .Void;
            },
            else => return .Void,
        }
    }

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
        }
        self.errorAtCurrent(msg);
        return .{ .typ = .Error, .lexeme = "", .line = 0, .column = 0 };
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
                const lexeme = source[start..i];
                const typ = keywordType(lexeme) orelse .Ident;
                try tokens.append(.{ .typ = typ, .lexeme = lexeme, .line = line, .column = column });
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

fn keywordType(lexeme: []const u8) ?TokenType {
    const keywords = .{
        .@"let" = .Let,
        .@"const" = .Const,
        .@"fn" = .Fn,
        .@"return" = .Return,
        .@"if" = .If,
        .@"else" = .Else,
        .@"while" = .While,
        .@"for" = .For,
        .@"in" = .In,
        .@"struct" = .Struct,
        .@"enum" = .Enum,
        .@"use" = .Use,
        .@"pub" = .Pub,
    };
    inline for (std.meta.fields(@TypeOf(keywords))) |field| {
        if (std.mem.eql(u8, lexeme, field.name)) return @field(keywords, field.name);
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
        defer allocator.free(ast);
        for (ast) |node| {
            parser.typeCheck(node);
        }
        if (parser.hadError) {
            std.process.exit(1);
        }
    }
    std.debug.print("Sprawdzenie OK\n", .{});
}
