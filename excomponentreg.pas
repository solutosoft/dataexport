unit exComponentReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, exExporter, exSerializer, Dialogs;

procedure Register;

implementation

{ TexDriverNameProperty }

procedure Register;
begin
  RegisterComponents('Data Export', [TexExporter, TexColumnSerializer, TexJsonSerializer, TexXmlSerializer]);
end;

end.

