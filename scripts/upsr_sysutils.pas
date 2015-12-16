unit uPSR_SysUtils;

interface

uses
  SysUtils, uPSRuntime {$IFDEF FPC}, LCLIntf {$ELSE}, Windows {$ENDIF};

procedure RegisterSysUtilsLibrary_R(S: TPSExec);

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

end.

