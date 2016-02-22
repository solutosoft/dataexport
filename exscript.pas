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
    RegisterMethod(@TexValue.IsNull, 'IsNull');
    RegisterMethod(@TexValue.AsVariant, 'AsVariant');
    RegisterMethod(@TexValue.AsString, 'AsString');
    RegisterMethod(@TexValue.AsInteger, 'AsInteger');
    RegisterMethod(@TexValue.AsFloat, 'AsFloat');
    RegisterMethod(@TexValue.AsDateTime, 'AsDateTime');
    RegisterMethod(@TexValue.AsDateBegin, 'AsDateBegin');
    RegisterMethod(@TexValue.AsDateEnd, 'AsDateEnd');
    RegisterMethod(@TexValue.AsArray, 'AsArray');
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
    RegisterMethod('Function IsNull : Boolean');
    RegisterMethod('Function AsVariant : Variant');
    RegisterMethod('Function AsString : String');
    RegisterMethod('Function AsInteger : Integer');
    RegisterMethod('Function AsFloat : Extended');
    RegisterMethod('Function AsDateTime : TDateTime');
    RegisterMethod('Function AsArray : TVariantDynArray');
    RegisterMethod('Function AsDateBegin : TDateTime');
    RegisterMethod('Function AsDateEnd : TDateTime');
  end;
end;

end.

