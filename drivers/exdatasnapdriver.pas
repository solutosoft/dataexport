unit exdatasnapdriver;

interface

uses
  Classes, SysUtils, DB, Variants, exExporter, DBClient, DSConnect;

type

  { TexDataSnapProvider }

  TexDataSnapDriver = class(TexDriver)
  private
    FConnection: TDSProviderConnection;
    FProviderName: String;
  public
    procedure OpenConnection; override;
    procedure CloseConnection; override;
    procedure ExecSQL(ASQL: String; const AParams: array of Variant); override;
    function ExecSQLScalar(ASQL: String; const AParams: array of Variant): Variant; override;
    function CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet; override;
  published
    property Connection: TDSProviderConnection read FConnection write FConnection;
    property ProviderName: String read FProviderName write FProviderName;
  end;

procedure Register;

implementation

{ TexDataSnapDriver }

procedure TexDataSnapDriver.OpenConnection;
begin
  inherited;
  FConnection.Open;
end;

procedure TexDataSnapDriver.CloseConnection;
begin
  inherited;
  FConnection.Close;
end;

procedure TexDataSnapDriver.ExecSQL(ASQL: String; const AParams: array of Variant);
var
  AQuery: TClientDataSet;
begin
  AQuery := TClientDataSet.Create(nil);
  try
    AQuery.RemoteServer := FConnection;
    AQuery.CommandText := ASQL;
    AssignParamValues(AQuery.Params, AParams);
    AQuery.Execute;
  finally
    AQuery.Free;
  end;
end;

function TexDataSnapDriver.ExecSQLScalar(ASQL: String; const AParams: array of Variant): Variant;
var
  AQuery: TClientDataSet;
begin
  AQuery := TClientDataSet.Create(nil);
  try
    AQuery.RemoteServer := FConnection;
    AQuery.CommandText := ASQL;
    AssignParamValues(AQuery.Params, AParams);
    AQuery.Open;

    if (AQuery.FieldCount > 0) and (not AQuery.IsEmpty) then
      Result := AQuery.Fields[0].Value;
  finally
    AQuery.Free;
  end;
end;

function TexDataSnapDriver.CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet;
var
  I: Integer;
  AParam: TParam;
  AField: TField;
  AValue: Variant;
  AType: TFieldType;
begin
  Result := TClientDataSet.Create(Self);
  with (TClientDataSet(Result)) do
  begin
    CommandText := ASQL;
    RemoteServer := FConnection;
    ProviderName := FProviderName;

    for I := 0 to Params.Count -1 do
    begin
      AParam := Params[I];
      AField := nil;

      if (AMaster <> nil) then
        AField := AMaster.FindField(AParam.Name);

      if (AField <> nil) then
      begin
        AParam.DataType := AField.DataType;
        AParam.Value := AField.Value;
      end
      else begin
        AValue := Exporter.ExtractParamValue(AParam.Name);
        if (not VarIsEmpty(AValue)) then
        begin
          AType := VarTypeToDataType(VarType(AValue));

          if (AType <> ftUnknown) then
            AParam.DataType := AType;

          AParam.Value := AValue;
        end;
      end;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Data Export', [TexDataSnapDriver]);
end;

end.
