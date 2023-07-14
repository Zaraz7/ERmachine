unit ERparser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ERunit;

var
  cu: ControlUnit;
  code: ERcode;

function LineParse(inputString: string): ERoperator;
procedure Parse(inputText: TStrings);
function GetSimpleLine(inputString: string): string;

implementation


type
  TokenType = (Number, Zero, Sum, Copy, Jump, LParen, RParen, Comma, EndOfLine, Unknown);
  Token = record
    TokenType: TokenType;
    Value: String;
  end;
  ERSyntaxError = Class(Exception);
  ERIdentifierError = Class(Exception);

var
  input: string;
  position, line: Integer;
  currentToken: Token;


procedure GetNextToken;
begin
  if position > Length(input) then
  begin
    currentToken.TokenType := EndOfLine;
    currentToken.Value := '';
    Exit;
  end;

  while (input[position] = ' ') and (position < Length(input)) do
        inc(position);

  case input[position] of
    'Z': begin
           currentToken.TokenType := Zero;
         end;
    'S': begin
           currentToken.TokenType := Sum;
         end;
    'T','C': begin
           currentToken.TokenType := Copy;
         end;
    'J': begin
           currentToken.TokenType := Jump;
         end;
    '(': begin
           currentToken.TokenType := LParen;
         end;
    ')': begin
           currentToken.TokenType := RParen;
         end;
    ',': currentToken.TokenType := Comma;

    '/': begin
           currentToken.TokenType := EndOfLine;
           currentToken.Value := '';
           Exit;
         end;
    '0'..'9': begin
      currentToken.TokenType := Number;
      currentToken.Value := '';
      while (position <= Length(input)) and (input[position] in ['0'..'9']) do
      begin
        currentToken.Value := currentToken.Value + input[position];
        Inc(position);
      end;
      Exit;
    end
    else
      currentToken.TokenType := Unknown;
    end;
  currentToken.Value := input[position];
  Inc(position);
end;

function GetSimpleLine(inputString: string): string;
var output: String;
begin
  input := inputString;
  position := 1;
  GetNextToken;
  output:=currentToken.Value;
  while currentToken.TokenType <> EndOfLine do begin
    GetNextToken;
    output:=output+currentToken.Value;
  end;
  Result:=output;
end;

procedure Match(expectedTokenType: TokenType);
var e: String;
begin
  if currentToken.TokenType <> expectedTokenType then begin
    e:='('+intToStr(Line+1)+','+intToStr(position)+') Синтаксическая ошибка: ожидалось ';
    case expectedTokenType of
       Number: e:=e+'число';
       LParen: e:=e+'"("';
       RParen: e:=e+'")"';
       Comma: e:=e+'","';
       EndOfLine: e:=e+'следующая строка';
    end;
    raise ERSyntaxError.Create(e+', а получено "'+currentToken.Value);
  end;
  //GetNextToken;
end;

function LineParse(inputString: string): ERoperator;
var i:byte;
    parameters: array of integer;
begin
  input := inputString;
  position := 1;
  GetNextToken;
  with cu do
    case currentToken.TokenType of
       Zero: begin
         setLength(parameters, 1);
         LineParse.func := @zeroReg;
         end;
       Sum:  begin
         setLength(parameters, 1);
         LineParse.func := @incReg;
         end;
       Copy: begin
         setLength(parameters, 2);
         LineParse.func := @copyReg;
         end;
       Jump: begin
         setLength(parameters, 3);
         LineParse.func := @jumpTo;
         end
    else
      raise ERIdentifierError.Create('('+intToStr(Line+1)+','+intToStr(position)+') Ошибка: неизвестный оператор "'+currentToken.Value+'"');
    end;
  GetNextToken;
  Match(LParen);
  GetNextToken;
  Match(Number);
  parameters[0]:=StrToInt(currentToken.Value);
  i := 1;
  while i < length(parameters) do begin
    GetNextToken;
    Match(Comma);
    GetNextToken;
    Match(Number);
    parameters[i]:=StrToInt(currentToken.Value);
    Inc(i);
  end;
  GetNextToken;
  Match(RParen);
  LineParse.parameters:=parameters;
end;

procedure Parse(inputText: TStrings);
//var i: Integer;
begin
  setLength(code, inputText.Count);
  for line:=0 to inputText.Count-1 do
     code[line]:= LineParse(inputText[line]);
end;

begin
  cu:= ControlUnit.Create;

end.

