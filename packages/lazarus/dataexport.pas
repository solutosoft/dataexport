{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dataexport;

interface

uses
  exComponentReg, exDefinition, exExporter, exSerializer, exParser, 
  uPSR_SysUtils, uPSC_SysUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('exComponentReg', @exComponentReg.Register);
end;

initialization
  RegisterPackage('dataexport', @Register);
end.
