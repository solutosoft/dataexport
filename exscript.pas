unit exscript;

interface

uses
  SysUtils, uPSRuntime, uPSCompiler {$IFDEF FPC}, LCLIntf {$ELSE}, Windows {$ENDIF},
  exDefinition, exOptions;

procedure RegisterSysUtilsLibrary_R(S: TPSExec);
procedure RegisterTexValueClass_R(S: TPSRuntimeClassImporter);
procedure RegisterTexOptionsClass_R(S: TPSRuntimeClassImporter);

procedure RegisterSysUtilsLibrary_C(S: TPSPascalCompiler);
procedure RegisterTexValueClass_C(S: TPSPascalCompiler);
procedure RegisterTexOptionsClass_C(S: TPSPascalCompiler);

implementation

function RemoveMask(AText: string): String;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AText) do
  begin
    if (IsCharAlphaNumeric(AText[I])) then
      Result := Result + AText[I];
  end;
end;

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

procedure RegisterSysUtilsLibrary_R(S: TPSExec);
begin
  S.RegisterDelphiFunction(@FormatFloat, 'FORMATFLOAT', cdRegister);
  S.RegisterDelphiFunction(@FileExists, 'FILEEXISTS', cdRegister);
  S.RegisterDelphiFunction(@DirectoryExists, 'DIRECTORYEXISTS', cdRegister);
  S.RegisterDelphiFunction(@DeleteFile, 'DELETEFILE', cdRegister);
  S.RegisterDelphiFunction(@RenameFile, 'RENAMEFILE', cdRegister);
  S.RegisterDelphiFunction(@RemoveMask, 'REMOVEMASK', cdRegister);
  S.RegisterDelphiFunction(@SetDateSeparator, 'SETDATESEPARATOR', cdRegister);
  S.RegisterDelphiFunction(@SetTimeSeparator, 'SETTIMESEPARATOR', cdRegister);
  S.RegisterDelphiFunction(@SetThousandSeparator, 'SETTHOUSANDSEPARATOR', cdRegister);
  S.RegisterDelphiFunction(@SetDecimalSeparator, 'SETDECIMALSEPARATOR', cdRegister);
  S.RegisterDelphiFunction(@SetLongDateFormat, 'SETLONGDATEFORMAT', cdRegister);
  S.RegisterDelphiFunction(@SetShortDateFormat, 'SETSHORTDATEFORMAT', cdRegister);
  S.RegisterDelphiFunction(@SetLongTimeFormat, 'SETLONGTIMEFORMAT', cdRegister);
  S.RegisterDelphiFunction(@SetShortTimeFormat, 'SETSHORTTIMEFORMAT', cdRegister);
end;

procedure RegisterTexValueClass_R(S: TPSRuntimeClassImporter);
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
end;

procedure RegisterTexOptionsClass_R(S: TPSRuntimeClassImporter);
begin
  with S.Add(TexOptions) do
  begin
    RegisterConstructor(@TexOptions.Create, 'Create');
    RegisterMethod(@TexOptions.GetAsString, 'GetAsString');
    RegisterMethod(@TexOptions.GetAsInteger, 'GetAsInteger');
    RegisterMethod(@TexOptions.GetAsFloat, 'GetAsFloat');
    RegisterMethod(@TexOptions.GetAsBoolean, 'GetAsBoolean');
  end;
  S.Add(TexHttpOptions);
  S.Add(TexFileOptions);
end;

procedure RegisterSysUtilsLibrary_C(S: TPSPascalCompiler);
begin
  S.AddDelphiFunction('function FormatFloat(Const Format : String; Value : Extended) : String;');
  S.AddDelphiFunction('function FileExists (Const FileName : String) : Boolean;');
  S.AddDelphiFunction('function DirectoryExists (Const Directory : String) : Boolean;');
  S.AddDelphiFunction('function DeleteFile (Const FileName : String) : Boolean;');
  S.AddDelphiFunction('function RemoveMask(AText: string): String;');
  S.AddDelphiFunction('procedure SetDateSeparator(ADateSeparator: Char);');
  S.AddDelphiFunction('procedure SetTimeSeparator(ATimeSeparator: Char);');
  S.AddDelphiFunction('procedure SetThousandSeparator(AThousandSeparator: Char);');
  S.AddDelphiFunction('procedure SetDecimalSeparator(ADecimalSeparator: Char);');
  S.AddDelphiFunction('procedure SetLongDateFormat(ALongDateFormat: String);');
  S.AddDelphiFunction('procedure SetShortDateFormat(AShortDateFormat: String);');
  S.AddDelphiFunction('procedure SetLongTimeFormat(ALongTimeFormat: String);');
  S.AddDelphiFunction('procedure SetShortTimeFormat(AShortTimeFormat: String);');
end;

procedure RegisterTexValueClass_C(S: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TObject', 'TexValue') do
  with S.AddClassN(S.FindClass('TObject'), 'TexValue') do
  begin
    RegisterMethod('Constructor Create( AValue : Variant)');
    RegisterMethod('Function IsNull : Boolean');
    RegisterMethod('Function AsVariant : Variant');
    RegisterMethod('Function AsString : String');
    RegisterMethod('Function AsInteger : Integer');
    RegisterMethod('Function AsFloat : Extended');
    RegisterMethod('Function AsBoolean : Boolean');
    RegisterMethod('Function AsDateTime : Double');
    RegisterMethod('Function AsArray : TVariantDynArray');
    RegisterMethod('Function AsDateBegin : Double');
    RegisterMethod('Function AsDateEnd : Double');
  end;
end;

procedure RegisterTexOptionsClass_C(S: TPSPascalCompiler);
begin
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
end;

end.

