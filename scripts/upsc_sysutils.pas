unit uPSC_SysUtils;

interface

uses
  SysUtils, uPSCompiler, uPSUtils;

procedure RegisterSysUtilsLibrary_C(S: TPSPascalCompiler);

implementation

procedure RegisterSysUtilsLibrary_C(S: TPSPascalCompiler);
begin
  S.AddDelphiFunction('function FormatFloat(Const Format : String; Value : Extended) : String;');
  S.AddDelphiFunction('function FileExists (Const FileName : String) : Boolean;');
  S.AddDelphiFunction('function DirectoryExists (Const Directory : String) : Boolean;');
  S.AddDelphiFunction('function DeleteFile (Const FileName : String) : Boolean;');
  S.AddDelphiFunction('function RemoveMask(Text: string): String;');
end;

end.

