unit exDataSnapDriverReg;

interface

uses
  System.Classes, exDataSnapDriver;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('Data Export', [TexDataSnapDriver]);
end;

end.
