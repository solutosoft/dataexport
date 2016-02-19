unit exscript;

interface

uses
  SysUtils, uPSRuntime, uPSCompiler {$IFDEF FPC}, LCLIntf {$ELSE}, Windows {$ENDIF},
  exDefinition;

procedure RegisterSysUtilsLibrary_R(S: TPSExec);
procedure RegisterTexValueClass_R(S: TPSRuntimeClassImporter);

procedure RegisterSysUtilsLibrary_C(S: TPSPascalCompiler);
procedure RegisterTexValueClass_C(S: TPSPascalCompiler);

implementation

function RemoveMask(Text: string): String;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Text) do
  begin
    if (IsCharAlphaNumeric(Text[I])) then
      Result := Result + Text[I];
  end;
end;

procedure RegisterSysUtilsLibrary_R(S: TPSExec);
begin
  S.RegisterDelphiFunction(@FormatFloat, 'FORMATFLOAT', cdRegister);
  S.RegisterDelphiFunction(@FileExists, 'FILEEXISTS', cdRegister);
  S.RegisterDelphiFunction(@DirectoryExists, 'DIRECTORYEXISTS', cdRegister);
  S.RegisterDelphiFunction(@DeleteFile, 'DELETEFILE', cdRegister);
  S.RegisterDelphiFunction(@RenameFile, 'RENAMEFILE', cdRegister);
  S.RegisterDelphiFunction(@RemoveMask, 'REMOVEMASK', cdRegister);
end;

procedure RegisterTexValueClass_R(S: TPSRuntimeClassImporter);
begin
  with S.Add(TexValue) do
  begin
    RegisterConstructor(@TexValue.Create, 'Create');
    RegisterMethod(@TexValue.GetIsNull, 'GetIsNull');
    RegisterMethod(@TexValue.GetAsVariant, 'GetAsVariant');
    RegisterMethod(@TexValue.GetAsString, 'GetAsString');
    RegisterMethod(@TexValue.GetAsInteger, 'GetAsInteger');
    RegisterMethod(@TexValue.GetAsFloat, 'GetAsFloat');
    RegisterMethod(@TexValue.GetAsDateTime, 'GetAsDateTime');
    RegisterMethod(@TexValue.GetDateStart, 'GetDateStart');
    RegisterMethod(@TexValue.GetDateEnd, 'GetDateEnd');
    RegisterMethod(@TexValue.GetAsArray, 'GetAsArray');
  end;
end;

procedure RegisterSysUtilsLibrary_C(S: TPSPascalCompiler);
begin
  S.AddDelphiFunction('function FormatFloat(Const Format : String; Value : Extended) : String;');
  S.AddDelphiFunction('function FileExists (Const FileName : String) : Boolean;');
  S.AddDelphiFunction('function DirectoryExists (Const Directory : String) : Boolean;');
  S.AddDelphiFunction('function DeleteFile (Const FileName : String) : Boolean;');
  S.AddDelphiFunction('function RemoveMask(Text: string): String;');
end;

procedure RegisterTexValueClass_C(S: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TObject', 'TexValue') do
  with S.AddClassN(S.FindClass('TObject'), 'TexValue') do
  begin
    RegisterMethod('Constructor Create( AValue : Variant)');
    RegisterMethod('Function GetIsNull : Boolean');
    RegisterMethod('Function GetAsVariant : Variant');
    RegisterMethod('Function GetAsString : String');
    RegisterMethod('Function GetAsInteger : Integer');
    RegisterMethod('Function GetAsFloat : Extended');
    RegisterMethod('Function GetAsDateTime : TDateTime');
    RegisterMethod('Function GetDateStart : TDateTime');
    RegisterMethod('Function GetDateEnd : TDateTime');
    RegisterMethod('Function GetAsArray : TVariantDynArray');
  end;
end;

end.

