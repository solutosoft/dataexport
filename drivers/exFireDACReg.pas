unit exFireDACReg;

interface

uses
  System.Classes, exFireDACDriver;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('Data Export', [TexFireDACDriver]);
end;

end.
