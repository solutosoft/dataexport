unit exOptions;

interface

uses
  Classes, exClasses, exDefinition;

type
  { TexFileOptions }

  TexFileOptions = class(TexOptions)
  public
    constructor Create; override;
  end;

  { TexFileOptions }

  TexHttpOptions = class(TexOptions)
  public
    constructor Create; override;
  end;

const
  sexSFileOptions = 'File';
  sexSHttpOptions = 'Http';

var
  FRegisteredOptions: TexRegisteredClasses;

function GetRegisteredOptions: TexRegisteredClasses;

implementation

function GetRegisteredOptions: TexRegisteredClasses;
begin
  if FRegisteredOptions = nil then
    FRegisteredOptions := TexRegisteredClasses.Create;
  Result := FRegisteredOptions;
end;

{ TexFileOptions }

constructor TexFileOptions.Create;
begin
  inherited Create;
  Editors.Add('FileName', edtFile);
end;

{ TexHttpOptions }

constructor TexHttpOptions.Create;
begin
  inherited Create;
  Editors.Add('BaseUrl', edtText);
  Editors.Add('BasicAuth', edtBoolean, 'False');
  Editors.Add('ProxyServer', edtText);
  Editors.Add('ProxyPort', edtInteger);
  Editors.Add('ProxyUsername', edtText);
  Editors.Add('ProxyPassword', edtPassword);
end;

end.
