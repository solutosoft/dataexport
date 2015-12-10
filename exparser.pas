unit exParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf;

const
  TK_COMMA = ',';
  TK_DOT = '.';
  TK_SPACE = #32;
  TK_RETURN = #13;
  TK_QUOTE = #39;
  TK_DBQUOTE = '"';
  TK_LGROUP = '(';
  TK_RGROUP = ')';
  TK_MINOR = '<';
  TK_MAJOR = '>';
  TK_EQUAL = '=';
  TK_CONCAT = '|';
  TK_PLUS = '+';
  TK_MINUS = '-';
  TK_UNDERSCORE = '_';

  SCRIPT_FUNC_EVAL = 'function exEvaluate: Variant;';

function GetTokens(AExpression: String): TStrings;
function GetScript(AExpression: String): TStrings;

implementation

function GetTokens(AExpression: String): TStrings;
var
  I, J: Integer;
  AChar: Char;
  AToken: String;
begin
  AToken := '';
  Result := TStringList.Create;

  I := 1;
  while( I <= Length(AExpression)) do
  begin
    AChar := AExpression[I];
    if (AChar = TK_QUOTE) or (AChar = TK_DBQUOTE) then
    begin
      AToken := Copy(AExpression, I+1, MaxInt);
      J := Pos(AChar, AToken);
      if (J <= 0) then
        Result.Add(AChar)
      else begin
        AToken := Copy(AToken, 1, J -1);
        Result.Add(AChar + AToken + AChar);
        I := I + J;
      end;
      AToken := '';
    end
    else if (IsCharAlphaNumeric(AChar)) or (AChar = TK_UNDERSCORE)  then
      AToken := AToken + AChar
    else begin
      if (AToken <> '') then
      begin
        Result.Add(AToken);
        AToken := '';
      end;
      if (AChar <> '') then
        Result.Add(AChar);
    end;
    Inc(I);
  end;

  if AToken <> '' then
    Result.Add(AToken);
end;

function GetScript(AExpression: String): TStrings;
var
  ATokens: TStrings;
  AVar: Boolean;
begin
  Result := TStringList.Create;
  ATokens := GetTokens(AExpression);
  try
    AVar := (ATokens.Count > 0) and (SameText(ATokens[0], 'var'));
    Result.Add('program exEvalutator;');
    Result.Add(SCRIPT_FUNC_EVAL);

    if (not AVar) then
      Result.Add('begin');

    Result.Add(AExpression);

    if (not AVar) then
      Result.Add('end;');

    Result.Add('end.');
  finally
    ATokens.Free;
  end;
end;

end.


end.

