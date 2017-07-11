unit exscript;

interface

uses
  SysUtils, Variants, uPSRuntime, uPSCompiler {$IFDEF FPC}, LCLIntf, fpjson {$ELSE}, Windows, JSON {$ENDIF},
  exDefinition, exOptions;

type
  TexJSONData = class(TObject)
  private
    FJSON: {$IFDEF FPC} TJSONData{$ELSE}TJSONValue{$ENDIF};
    FValue: TexValue;
  public
    constructor Create(AData: String);
    destructor Destroy;override;
    function FindValue(APath: String): TexValue;
  end;

procedure RegisterExtraClasses_R(S: TPSRuntimeClassImporter; SE: TPSExec);
procedure RegisterExtraClasses_C(S: TPSPascalCompiler);

implementation

{ Extra functions }

procedure SetDateSeparator(ADateSeparator: Char);
begin
  FormatSettings.DateSeparator := ADateSeparator;
end;

procedure SetTimeSeparator(ATimeSeparator: Char);
begin
  FormatSettings.TimeSeparator := ATimeSeparator;
end;

procedure SetThousandSeparator(AThousandSeparator: Char);
begin
  FormatSettings.ThousandSeparator := AThousandSeparator;
end;

procedure SetDecimalSeparator(ADecimalSeparator: Char);
begin
  FormatSettings.DecimalSeparator := ADecimalSeparator;
end;

procedure SetLongDateFormat(ALongDateFormat: String);
begin
  FormatSettings.LongDateFormat := ALongDateFormat;
end;

procedure SetShortDateFormat(AShortDateFormat: String);
begin
  FormatSettings.ShortDateFormat := AShortDateFormat;
end;

procedure SetLongTimeFormat(ALongTimeFormat: String);
begin
  FormatSettings.LongTimeFormat := ALongTimeFormat;
end;

procedure SetShortTimeFormat(AShortTimeFormat: String);
begin
  FormatSettings.ShortTimeFormat := AShortTimeFormat;
end;

function GetJSONData(AData: String): TexJSONData;
begin
  Result := TexJSONData.Create(AData);
end;

function RemoveMask(AStr: string): String;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AStr) do
  begin
    if (IsCharAlphaNumeric(AStr[I])) then
      Result := Result + AStr[I];
  end;
end;

function StringReplace(ASource, AOldPattern, ANewPattern: String; AIgnoreCase: Boolean): String;
var
  AFlags: TReplaceFlags;
begin
  AFlags := [rfReplaceAll];

  if (AIgnoreCase) then
    AFlags := AFlags + [rfIgnoreCase];

  Result := SysUtils.StringReplace(ASource, AOldPattern, ANewPattern, AFlags);
end;

{ Extra registrations }

procedure RegisterExtraClasses_R(S: TPSRuntimeClassImporter; SE: TPSExec);
begin
  with S.Add(TexValue) do
  begin
    RegisterConstructor(@TexValue.Create, 'Create');
    RegisterMethod(@TexValue.IsNull, 'IsNull');
    RegisterMethod(@TexValue.AsVariant, 'AsVariant');
    RegisterMethod(@TexValue.AsString, 'AsString');
    RegisterMethod(@TexValue.AsInteger, 'AsInteger');
    RegisterMethod(@TexValue.AsFloat, 'AsFloat');
    RegisterMethod(@TexValue.AsBoolean, 'AsBoolean');
    RegisterMethod(@TexValue.AsDateTime, 'AsDateTime');
    RegisterMethod(@TexValue.AsDateBegin, 'AsDateBegin');
    RegisterMethod(@TexValue.AsDateEnd, 'AsDateEnd');
    RegisterMethod(@TexValue.AsArray, 'AsArray');
  end;

  with S.Add(TexOptions) do
  begin
    RegisterConstructor(@TexOptions.Create, 'Create');
    RegisterMethod(@TexOptions.GetAsString, 'GetAsString');
    RegisterMethod(@TexOptions.GetAsInteger, 'GetAsInteger');
    RegisterMethod(@TexOptions.GetAsFloat, 'GetAsFloat');
    RegisterMethod(@TexOptions.GetAsBoolean, 'GetAsBoolean');
  end;
  S.Add(TexFileOptions);
  S.Add(TexFTPOptions);
  S.Add(TexHttpOptions);
  S.Add(TexDatabaseOptions);

  with S.Add(TexJSONData) do
  begin
    RegisterConstructor(@TexJSONData.Create, 'Create');
    RegisterMethod(@TexJSONData.FindValue, 'FindValue');
  end;

  SE.RegisterDelphiFunction(@GetJSONData, 'GETJSONDATA', cdRegister);
  SE.RegisterDelphiFunction(@FormatFloat, 'FORMATFLOAT', cdRegister);
  SE.RegisterDelphiFunction(@FileExists, 'FILEEXISTS', cdRegister);
  SE.RegisterDelphiFunction(@DirectoryExists, 'DIRECTORYEXISTS', cdRegister);
  SE.RegisterDelphiFunction(@DeleteFile, 'DELETEFILE', cdRegister);
  SE.RegisterDelphiFunction(@RenameFile, 'RENAMEFILE', cdRegister);
  SE.RegisterDelphiFunction(@SetDateSeparator, 'SETDATESEPARATOR', cdRegister);
  SE.RegisterDelphiFunction(@SetTimeSeparator, 'SETTIMESEPARATOR', cdRegister);
  SE.RegisterDelphiFunction(@SetThousandSeparator, 'SETTHOUSANDSEPARATOR', cdRegister);
  SE.RegisterDelphiFunction(@SetDecimalSeparator, 'SETDECIMALSEPARATOR', cdRegister);
  SE.RegisterDelphiFunction(@SetLongDateFormat, 'SETLONGDATEFORMAT', cdRegister);
  SE.RegisterDelphiFunction(@SetShortDateFormat, 'SETSHORTDATEFORMAT', cdRegister);
  SE.RegisterDelphiFunction(@SetLongTimeFormat, 'SETLONGTIMEFORMAT', cdRegister);
  SE.RegisterDelphiFunction(@SetShortTimeFormat, 'SETSHORTTIMEFORMAT', cdRegister);
  SE.RegisterDelphiFunction(@QuotedStr, 'QUOTEDSTR', cdRegister);
  SE.RegisterDelphiFunction(@RemoveMask, 'REMOVEMASK', cdRegister);
  SE.RegisterDelphiFunction(@StringReplace, 'STRINGREPLACE', cdRegister);
end;

procedure RegisterExtraClasses_C(S: TPSPascalCompiler);
begin
  S.AddTypeS('TVariantDynArray', 'array of Variant');
  S.AddTypeS('TexDataType', '(datNone, datText, datInteger, datDateTime, datBoolean, datFloat, datCurrency)');
  with S.AddClassN(S.FindClass('TObject'), 'TexValue') do
  begin
    RegisterMethod('Constructor Create(AValue : Variant)');
    RegisterMethod('Function IsNull : Boolean');
    RegisterMethod('Function AsVariant : Variant');
    RegisterMethod('Function AsString : Variant');
    RegisterMethod('Function AsInteger : Variant');
    RegisterMethod('Function AsFloat : Variant');
    RegisterMethod('Function AsBoolean : Variant');
    RegisterMethod('Function AsDateTime : Variant');
    RegisterMethod('Function AsDateBegin : Variant');
    RegisterMethod('Function AsDateEnd : Variant');
    RegisterMethod('Function AsArray : TVariantDynArray');
  end;

  with S.AddClassN(S.FindClass('TStringList'), 'TexOptions') do
  begin
    RegisterMethod('Constructor Create');
    RegisterMethod('Function GetAsString( AName : String; ADefault : String) : String');
    RegisterMethod('Function GetAsInteger( AName : String; ADefault : Integer) : Integer');
    RegisterMethod('Function GetAsFloat( AName : String; ADefault : Double) : Double');
    RegisterMethod('Function GetAsBoolean( AName : String; ADefault : Boolean) : Boolean');
  end;
  S.AddClassN(S.FindClass('TexOptions'),'TexHttpOptions');
  S.AddClassN(S.FindClass('TexOptions'),'TexFileOptions');
  S.AddClassN(S.FindClass('TexOptions'),'TexFTPOptions');
  S.AddClassN(S.FindClass('TexOptions'),'TexDatabaseOptions');

  with S.AddClassN(S.FindClass('TObject'), 'TexJSONData') do
  begin
    RegisterMethod('Constructor Create(AData: String)');
    RegisterMethod('Function FindValue(APath: String): TexValue');
  end;

  S.AddDelphiFunction('function FormatFloat(Const Format : String; Value : Extended) : String;');
  S.AddDelphiFunction('function FileExists (Const FileName : String) : Boolean;');
  S.AddDelphiFunction('function DirectoryExists (Const Directory : String) : Boolean;');
  S.AddDelphiFunction('function DeleteFile (Const FileName : String) : Boolean;');
  S.AddDelphiFunction('procedure SetDateSeparator(ADateSeparator: Char);');
  S.AddDelphiFunction('procedure SetTimeSeparator(ATimeSeparator: Char);');
  S.AddDelphiFunction('procedure SetThousandSeparator(AThousandSeparator: Char);');
  S.AddDelphiFunction('procedure SetDecimalSeparator(ADecimalSeparator: Char);');
  S.AddDelphiFunction('procedure SetLongDateFormat(ALongDateFormat: String);');
  S.AddDelphiFunction('procedure SetShortDateFormat(AShortDateFormat: String);');
  S.AddDelphiFunction('procedure SetLongTimeFormat(ALongTimeFormat: String);');
  S.AddDelphiFunction('procedure SetShortTimeFormat(AShortTimeFormat: String);');
  S.AddDelphiFunction('function QuotedStr(const S: string): string;');
  S.AddDelphiFunction('function RemoveMask(const S: string): string;');
  S.AddDelphiFunction('function StringReplace(ASource, AOldPattern, ANewPattern: String; AIgnoreCase: Boolean): String;');
  S.AddDelphiFunction('function GetJSONData(AData: String): TexJSONData;');
end;

{ TexJSONData }

constructor TexJSONData.Create(AData: String);
begin
  {$IFDEF FPC}
  FJSON := GetJSON(AData);
  {$ELSE}
  FJSON := TJSONObject.ParseJSONValue(AData);
  {$ENDIF}
  FValue := TexValue.Create(Null);
end;

destructor TexJSONData.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

function TexJSONData.FindValue(APath: String): TexValue;
{$IFNDEF FPC}
var
  AData: String;
{$ENDIF}
begin
  try
    {$IFDEF FPC}
    FValue := TexValue.Create(FJSON.FindPath(APath).AsString);
    {$ELSE}
    if (FJSON.TryGetValue(APath, AData)) then
      FValue := TexValue.Create(AData)
    else
      FValue := TexValue.Create(Null);
    {$ENDIF}
  except
    FValue := TexValue.Create(Null);
  end;

  Result := FValue;
end;

end.

