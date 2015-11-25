unit exZeosProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, ZConnection, ZDataset, exExporter, exDefinition;

type

  { TexZeosProvider }

  TexZeosProvider = class(TexProvider)
  private
    FConnection: TZConnection;
  public
    function CreateQuery(APipeline: TexPipeline; AMaster: TDataSet): TDataSet; override;
    procedure OpenConnection; override;
    procedure CloseConnection; override;
  published
    property Connection: TZConnection read FConnection write FConnection;
  end;

procedure Register;

implementation

{ TexZeosProvider }

function TexZeosProvider.CreateQuery(APipeline: TexPipeline; AMaster: TDataSet): TDataSet;
var
  I: Integer;
  AQuery: TZQuery;
  AField: TField;
  AParam: TParam;
begin
  AQuery := TZQuery.Create(Self);
  AQuery.SQL.AddStrings(APipeline.SQL);

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

procedure TexZeosProvider.OpenConnection;
begin
  FConnection.Connected := True;
end;

procedure TexZeosProvider.CloseConnection;
begin
  FConnection.Connected := False;
end;

procedure Register;
begin
  RegisterComponents('Data Export', [TexZeosProvider]);
end;

end.

