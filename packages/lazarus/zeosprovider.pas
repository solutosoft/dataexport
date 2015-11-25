{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit zeosprovider;

interface

uses
  exZeosProvider, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('exZeosProvider', @exZeosProvider.Register);
end;

initialization
  RegisterPackage('zeosprovider', @Register);
end.
