import os, strutils, sequtils, osproc, tables

type
  TokenType = enum
    ttLet, ttConst, ttFn, ttReturn, ttIf, ttElse, ttWhile, ttFor, ttIn, ttStruct, ttEnum, ttUse, ttPub,
    ttIdent, ttNumber, ttStringLit, ttBoolLit, ttLParen, ttRParen, ttLBrace, ttRBrace, ttColon, ttSemi, ttArrow, ttComma, ttDot,
    ttPlus, ttMinus, ttStar, ttSlash, ttEq, ttEqEq, ttLt, ttGt, ttLtEq, ttGtEq, ttBang, ttBangEq,
    ttEof, ttError

  Token = object
    typ: TokenType
    lexeme: string
    line, column: int

  TypeKind = enum
    tkI32, tkU32, tkF32, tkString, tkBool, tkVoid

  AstNodeKind = enum
    nkVarDecl, nkFnDecl, nkExprStmt, nkReturnStmt, nkIfStmt, nkBinaryExpr, nkLiteral, nkIdent

  AstNode = ref object
    case kind: AstNodeKind
    of nkVarDecl:
      varName: string
      varType: TypeKind
      varInit: AstNode
    of nkFnDecl:
      fnName: string
      params: seq[tuple[name: string, typ: TypeKind]]
      retType: TypeKind
      body: seq[AstNode]
    of nkExprStmt:
      expr: AstNode
    of nkReturnStmt:
      retExpr: AstNode
    of nkIfStmt:
      cond: AstNode
      thenBranch: seq[AstNode]
      elseBranch: seq[AstNode]
    of nkBinaryExpr:
      left: AstNode
      op: TokenType
      right: AstNode
    of nkLiteral:
      litType: TypeKind
      litValue: string
    of nkIdent:
      idName: string

  Parser = object
    tokens: seq[Token]
    current: int
    hadError: bool
    symbols: Table[string, TypeKind]

proc newParser(tokens: seq[Token]): Parser =
  result = Parser(tokens: tokens, current: 0, hadError: false, symbols: initTable[string, TypeKind]())

proc lex(source: string): seq[Token] =
  result = @[]
  var line = 1
  var column = 1
  var i = 0
  while i < source.len:
    let c = source[i]
    case c
    of ' ', '\t', '\r': discard
    of '\n': inc line; column = 1
    of '(': result.add Token(typ: ttLParen, lexeme: "(", line: line, column: column)
    of ')': result.add Token(typ: ttRParen, lexeme: ")", line: line, column: column)
    of '{': result.add Token(typ: ttLBrace, lexeme: "{", line: line, column: column)
    of '}': result.add Token(typ: ttRBrace, lexeme: "}", line: line, column: column)
    of ':': result.add Token(typ: ttColon, lexeme: ":", line: line, column: column)
    of ';': result.add Token(typ: ttSemi, lexeme: ";", line: line, column: column)
    of ',': result.add Token(typ: ttComma, lexeme: ",", line: line, column: column)
    of '.': result.add Token(typ: ttDot, lexeme: ".", line: line, column: column)
    of '+': result.add Token(typ: ttPlus, lexeme: "+", line: line, column: column)
    of '-':
      if i + 1 < source.len and source[i+1] == '>':
        inc i
        result.add Token(typ: ttArrow, lexeme: "->", line: line, column: column)
      else:
        result.add Token(typ: ttMinus, lexeme: "-", line: line, column: column)
    of '*': result.add Token(typ: ttStar, lexeme: "*", line: line, column: column)
    of '/': result.add Token(typ: ttSlash, lexeme: "/", line: line, column: column)
    of '=':
      if i + 1 < source.len and source[i+1] == '=':
        inc i
        result.add Token(typ: ttEqEq, lexeme: "==", line: line, column: column)
      else:
        result.add Token(typ: ttEq, lexeme: "=", line: line, column: column)
    of '<':
      if i + 1 < source.len and source[i+1] == '=':
        inc i
        result.add Token(typ: ttLtEq, lexeme: "<=", line: line, column: column)
      else:
        result.add Token(typ: ttLt, lexeme: "<", line: line, column: column)
    of '>':
      if i + 1 < source.len and source[i+1] == '=':
        inc i
        result.add Token(typ: ttGtEq, lexeme: ">=", line: line, column: column)
      else:
        result.add Token(typ: ttGt, lexeme: ">", line: line, column: column)
    of '!':
      if i + 1 < source.len and source[i+1] == '=':
        inc i
        result.add Token(typ: ttBangEq, lexeme: "!=", line: line, column: column)
      else:
        result.add Token(typ: ttBang, lexeme: "!", line: line, column: column)
    of Letters, '_':
      let start = i
      while i < source.len and (source[i].isAlphaNumeric or source[i] == '_'): inc i
      let lex = source[start..<i]
      let typ = case lex
      of "let": ttLet
      of "const": ttConst
      of "fn": ttFn
      of "return": ttReturn
      of "if": ttIf
      of "else": ttElse
      of "while": ttWhile
      of "for": ttFor
      of "in": ttIn
      of "struct": ttStruct
      of "enum": ttEnum
      of "use": ttUse
      of "pub": ttPub
      else: ttIdent
      result.add Token(typ: typ, lexeme: lex, line: line, column: column)
      dec i
    of Digits:
      let start = i
      while i < source.len and source[i].isDigit: inc i
      result.add Token(typ: ttNumber, lexeme: source[start..<i], line: line, column: column)
      dec i
    of '"':
      inc i
      let start = i
      while i < source.len and source[i] != '"': inc i
      if i >= source.len:
        echo "Error: Niezamknięty string linia ", line
        result.add Token(typ: ttError, lexeme: "", line: line, column: column)
      else:
        result.add Token(typ: ttStringLit, lexeme: source[start..<i], line: line, column: column)
    else:
      echo "Error: Nieoczekiwany znak '", c, "' linia ", line, " kol ", column
      result.add Token(typ: ttError, lexeme: $c, line: line, column: column)
    inc i
    inc column
  result.add Token(typ: ttEof, lexeme: "", line: line, column: column)

proc match(p: var Parser, typ: TokenType): bool =
  if p.current < p.tokens.len and p.tokens[p.current].typ == typ:
    inc p.current
    return true
  return false

proc consume(p: var Parser, typ: TokenType, msg: string): Token =
  if p.match(typ):
    return p.tokens[p.current - 1]
  else:
    p.hadError = true
    let tok = p.tokens[p.current]
    echo "Error: Linia ", tok.line, " Kol ", tok.column, ": ", msg, ". Sugestia: Sprawdź blisko '", tok.lexeme, "'"
    return Token(typ: ttError, lexeme: "", line: 0, column: 0)

proc parseType(p: var Parser): TypeKind =
  let tok = p.consume(ttIdent, "Oczekiwano typu")
  case tok.lexeme
  of "i32": return tkI32
  of "u32": return tkU32
  of "f32": return tkF32
  of "string": return tkString
  of "bool": return tkBool
  else:
    p.hadError = true
    echo "Error: Nieznany typ '", tok.lexeme, "'"
    return tkVoid

proc primary(p: var Parser): AstNode =
  if p.match(ttNumber):
    return AstNode(kind: nkLiteral, litType: tkI32, litValue: p.tokens[p.current - 1].lexeme)
  elif p.match(ttStringLit):
    return AstNode(kind: nkLiteral, litType: tkString, litValue: p.tokens[p.current - 1].lexeme)
  elif p.match(ttIdent):
    return AstNode(kind: nkIdent, idName: p.tokens[p.current - 1].lexeme)
  else:
    p.hadError = true
    echo "Error: Nieoczekiwane wyrażenie"
    return AstNode(kind: nkLiteral, litType: tkVoid, litValue: "")

proc binary(p: var Parser): AstNode =
  result = p.primary()
  while p.match(ttPlus) or p.match(ttMinus) or p.match(ttStar) or p.match(ttSlash) or p.match(ttEqEq) or p.match(ttBangEq) or p.match(ttLt) or p.match(ttGt) or p.match(ttLtEq) or p.match(ttGtEq):
    let op = p.tokens[p.current - 1].typ
    let right = p.primary()
    result = AstNode(kind: nkBinaryExpr, left: result, op: op, right: right)

proc expression(p: var Parser): AstNode =
  return p.binary()

proc statement(p: var Parser): AstNode =
  if p.match(ttIf):
    let cond = p.expression()
    discard p.consume(ttLBrace, "Oczekiwano '{' po if")
    var thenBranch: seq[AstNode] = @[]
    while not p.match(ttRBrace):
      thenBranch.add p.statement()
    var elseBranch: seq[AstNode] = @[]
    if p.match(ttElse):
      discard p.consume(ttLBrace, "Oczekiwano '{' po else")
      while not p.match(ttRBrace):
        elseBranch.add p.statement()
    return AstNode(kind: nkIfStmt, cond: cond, thenBranch: thenBranch, elseBranch: elseBranch)
  elif p.match(ttReturn):
    let expr = p.expression()
    discard p.consume(ttSemi, "Oczekiwano ';' po return")
    return AstNode(kind: nkReturnStmt, retExpr: expr)
  elif p.match(ttLet) or p.match(ttConst):
    let name = p.consume(ttIdent, "Oczekiwano nazwy zmiennej").lexeme
    discard p.consume(ttColon, "Oczekiwano ':' dla typu")
    let typ = p.parseType()
    var init: AstNode = nil
    if p.match(ttEq):
      init = p.expression()
    discard p.consume(ttSemi, "Oczekiwano ';'")
    p.symbols[name] = typ
    return AstNode(kind: nkVarDecl, varName: name, varType: typ, varInit: init)
  else:
    let expr = p.expression()
    discard p.consume(ttSemi, "Oczekiwano ';' po wyrażeniu")
    return AstNode(kind: nkExprStmt, expr: expr)

proc fnDeclaration(p: var Parser): AstNode =
  let name = p.consume(ttIdent, "Oczekiwano nazwy funkcji").lexeme
  discard p.consume(ttLParen, "Oczekiwano '('")
  var params: seq[tuple[name: string, typ: TypeKind]] = @[]
  while not p.match(ttRParen):
    let paramName = p.consume(ttIdent, "Oczekiwano nazwy parametru").lexeme
    discard p.consume(ttColon, "Oczekiwano ':'")
    let paramType = p.parseType()
    params.add (name: paramName, typ: paramType)
    if not p.tokens[p.current].typ == ttRParen:
      discard p.consume(ttComma, "Oczekiwano ','")
  var retType = tkVoid
  if p.match(ttArrow):
    retType = p.parseType()
  discard p.consume(ttLBrace, "Oczekiwano '{'")
  var body: seq[AstNode] = @[]
  while not p.match(ttRBrace) and p.current < p.tokens.len:
    body.add p.statement()
  return AstNode(kind: nkFnDecl, fnName: name, params: params, retType: retType, body: body)

proc declaration(p: var Parser): AstNode =
  if p.match(ttFn):
    return p.fnDeclaration()
  # Dodaj inne deklaracje
  else:
    return p.statement()

proc parse(p: var Parser): seq[AstNode] =
  result = @[]
  while p.current < p.tokens.len and p.tokens[p.current].typ != ttEof:
    result.add p.declaration()

proc inferType(node: AstNode, symbols: Table[string, TypeKind]): TypeKind =
  case node.kind
  of nkLiteral: return node.litType
  of nkIdent: return symbols.getOrDefault(node.idName, tkVoid)
  of nkBinaryExpr:
    let leftType = inferType(node.left, symbols)
    let rightType = inferType(node.right, symbols)
    if leftType != rightType: return tkVoid # Błąd
    return leftType
  else: return tkVoid

proc typeCheck(node: AstNode, p: var Parser) =
  case node.kind
  of nkVarDecl:
    if node.varInit != nil:
      let initType = inferType(node.varInit, p.symbols)
      if initType != node.varType:
        p.hadError = true
        echo "Error: Niezgodność typów w deklaracji ", node.varName
  of nkBinaryExpr:
    let leftType = inferType(node.left, p.symbols)
    let rightType = inferType(node.right, p.symbols)
    if leftType != rightType:
      p.hadError = true
      echo "Error: Niezgodność typów w operacji binarnej"
  # Dodaj więcej
  else: discard

proc generateC(node: AstNode): string =
  case node.kind
  of nkFnDecl:
    result = "int " & node.fnName & "() {\n"  # Zakładamy main jest int main()
    for stmt in node.body:
      result.add generateC(stmt)
    result.add "}\n"
  of nkVarDecl:
    let cType = case node.varType
      of tkI32: "int"
      of tkString: "char*"
      else: "void"
    result = cType & " " & node.varName
    if node.varInit != nil:
      result.add " = " & generateC(node.varInit)
    result.add ";\n"
  of nkExprStmt:
    result = generateC(node.expr) & ";\n"
  of nkReturnStmt:
    result = "return " & generateC(node.retExpr) & ";\n"
  of nkBinaryExpr:
    result = "(" & generateC(node.left) & " "
    case node.op
    of ttPlus: result.add "+"
    of ttMinus: result.add "-"
    of ttStar: result.add "*"
    of ttSlash: result.add "/"
    else: result.add "?"
    result.add " " & generateC(node.right) & ")"
  of nkLiteral:
    if node.litType == tkString:
      result = "\"" & node.litValue & "\""
    else:
      result = node.litValue
  of nkIdent:
    result = node.idName
  of nkIfStmt:
    result = "if (" & generateC(node.cond) & ") {\n"
    for stmt in node.thenBranch:
      result.add generateC(stmt)
    result.add "}\n"
    if node.elseBranch.len > 0:
      result.add "else {\n"
      for stmt in node.elseBranch:
        result.add generateC(stmt)
      result.add "}\n"
  else:
    result = ""

proc main() =
  let params = commandLineParams()
  if params.len < 1 or params[0] != "build":
    echo "Użycie: voidarc-backend-1 build --target <target> <pliki...>"
    quit(1)

  var target = "linux"
  var files: seq[string] = @[]
  var i = 1
  while i < params.len:
    if params[i] == "--target":
      inc i
      target = params[i]
    else:
      files.add params[i]
    inc i

  var cCode = """
#include <stdio.h>
#include <string.h>

"""
  for file in files:
    let source = readFile(file)
    let tokens = lex(source)
    var parser = newParser(tokens)
    let ast = parser.parse()
    for node in ast:
      parser.typeCheck(node)
    if parser.hadError:
      quit(1)
    for node in ast:
      cCode.add generateC(node)

  writeFile("generated.c", cCode)

  var compiler = if target == "linux": "gcc" else: "x86_64-w64-mingw32-gcc"
  let binName = if target == "windows": "main.exe" else: "main"
  let cmd = compiler & " generated.c -o " & binName
  let (output, exitCode) = execCmdEx(cmd)
  if exitCode != 0:
    echo "Budowa nieudana: ", output
    quit(1)

  echo "Budowa OK"

main()
