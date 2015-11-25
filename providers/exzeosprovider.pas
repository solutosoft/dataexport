unit exZeosProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, ZConnection, ZDataset, exExporter, exDefinition;

type

  { TexZeosDataProvider }

  TexZeosDataProvider = class(TexDataProvider)
  private
    FConnection: TZConnection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateQuery(AProvider: TexProvider; AMaster: TDataSet): TDataSet; override;
    procedure OpenConnection; override;
    procedure CloseConnection; override;
  end;

procedure Register;

implementation

{ TexZeosDataProvider }

constructor TexZeosDataProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := TZConnection.Create(nil);
  FConnection.LoginPrompt := False;
end;

destructor TexZeosDataProvider.Destroy;
begin
  FConnection.Free;
  inherited Destroy;
end;

function TexZeosDataProvider.CreateQuery(AProvider: TexProvider; AMaster: TDataSet): TDataSet;
var
  I: Integer;
  AQuery: TZQuery;
  AField: TField;
  AParam: TParam;
begin
  AQuery := TZQuery.Create(Self);
  AQuery.SQL.AddStrings(AProvider.SQL);

  if (AMaster <> nil) then
  begin
    for I := 0 to AQuery.Params.Count -1 do
    begin
      AParam := AQuery.Params[I];
      AField := AMaster.FindField(AParam.Name);
      if (AField = nil) then
      begin

      end;
    end;
  end;

  Result := AQuery;
end;

procedure TexZeosDataProvider.OpenConnection;
begin
  with TexExporter(GetOwner).DataProvider do
  begin
    FConnection.HostName := HostName;
    FConnection.Database := Database;
    FConnection.User := UserName;
    FConnection.Password := Password;
    FConnection.Port := Port;
    FConnection.Catalog := Catalog;
  end;
  FConnection.Connected := True;
end;

procedure TexZeosDataProvider.CloseConnection;
begin
  FConnection.Connected := False;
end;

procedure Register;
begin
  RegisterComponents('Data Export', [TexZeosDataProvider]);
end;

end.

