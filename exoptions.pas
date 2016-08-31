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

  { TexFTPOptions }

  TexFTPOptions = class(TexOptions)
  public
    constructor Create; override;
  end;

  { TexFileOptions }

  TexHttpOptions = class(TexOptions)
  public
    constructor Create; override;
  end;

  { TexDatabaseOptions }

  TexDatabaseOptions = class(TexOptions)
  public
    constructor Create; override;
  end;

const
  sexSFileOptions = 'File';
  sexSHttpOptions = 'Http';
  sexSFTPOptions = 'FTP';
  sexSDatabaseOptions = 'Database';

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

{ TexFTPOptions }

constructor TexFTPOptions.Create;
begin
  inherited;
  RegisterOption('FileName', edtFile);
  RegisterOption('Host', edtText);
  RegisterOption('Password', edtText);
  RegisterOption('Port', edtInteger);
  RegisterOption('RemoteDir', edtText);
  RegisterOption('Username', edtText);
end;

{ TexHttpOptions }

constructor TexHttpOptions.Create;
begin
  inherited Create;
  RegisterOption('Accept', edtText);
  RegisterOption('BasicAuthentication', edtBoolean, 'False');
  RegisterOption('ConnectTimeout', edtInteger);
  RegisterOption('ContentType', edtText);
  RegisterOption('ContentEncoding', edtText);
  RegisterOption('Method', edtText);
  RegisterOption('Password', edtText);
  RegisterOption('ProxyPassword', edtPassword);
  RegisterOption('ProxyPort', edtInteger);
  RegisterOption('ProxyServer', edtText);
  RegisterOption('ProxyUsername', edtText);
  RegisterOption('Url', edtText);
  RegisterOption('Username', edtText);
end;

{ TexDatabaseOptions }

constructor TexDatabaseOptions.Create;
begin
  inherited;
  RegisterOption('Database', edtText);
  RegisterOption('Driver', edtText);
  RegisterOption('Server', edtText);
  RegisterOption('Password', edtText);
  RegisterOption('Port', edtInteger);
  RegisterOption('Username', edtText);
end;

end.
