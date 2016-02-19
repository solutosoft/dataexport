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
  RegisterOption('FileName', edtFile);
end;

{ TexHttpOptions }

constructor TexHttpOptions.Create;
begin
  inherited Create;
  RegisterOption('BasicAuth', edtBoolean, 'False');
  RegisterOption('ConnectTimeout', edtText);
  RegisterOption('ContentType', edtText);
  RegisterOption('ContentEncoding', edtText);
  RegisterOption('Method', edtText);
  RegisterOption('ProxyServer', edtText);
  RegisterOption('ProxyPort', edtInteger);
  RegisterOption('ProxyUsername', edtText);
  RegisterOption('ProxyPassword', edtPassword);
  RegisterOption('Url', edtText);
end;

end.
