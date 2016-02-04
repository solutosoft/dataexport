unit exscript;

interface

uses
  SysUtils, uPSRuntime, uPSCompiler {$IFDEF FPC}, LCLIntf {$ELSE}, Windows {$ENDIF};

procedure RegisterSysUtilsLibrary_R(S: TPSExec);
procedure RegisterSysUtilsLibrary_C(S: TPSPascalCompiler);

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

procedure RegisterSysUtilsLibrary_C(S: TPSPascalCompiler);
begin
  S.AddDelphiFunction('function FormatFloat(Const Format : String; Value : Extended) : String;');
  S.AddDelphiFunction('function FileExists (Const FileName : String) : Boolean;');
  S.AddDelphiFunction('function DirectoryExists (Const Directory : String) : Boolean;');
  S.AddDelphiFunction('function DeleteFile (Const FileName : String) : Boolean;');
  S.AddDelphiFunction('function RemoveMask(Text: string): String;');
end;

end.

