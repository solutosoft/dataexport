unit exComponentReg;


interface

uses
  Classes, SysUtils, exExporter, exSerializer;

procedure Register;

implementation

{ TexDriverNameProperty }

procedure Register;
begin
  RegisterComponents('Data Export', [TexExporter, TexColumnSerializer, TexJsonSerializer, TexXmlSerializer]);
end;

end.

