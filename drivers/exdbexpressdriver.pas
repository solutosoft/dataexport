unit exdbexpressdriver;

interface

uses
  Classes, SysUtils, DB, Variants, exExporter, SqlExpr;

type
  { TexDBExpressProvider }

  TexDBExpressDriver = class(TexDriver)
  private
    FConnection: TSQLConnection;
  public
    procedure OpenConnection; override;
    procedure CloseConnection; override;
    procedure ExecSQL(ASQL: String; const AParams: array of Variant); override;
    function ExecSQLScalar(ASQL: String; const AParams: array of Variant): Variant; override;
    function CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet; override;
  published
    property Connection: TSQLConnection read FConnection write FConnection;
  end;

procedure Register;

implementation

{ TexDBExpressDriver }

procedure TexDBExpressDriver.OpenConnection;
begin
  inherited;
  FConnection.Open;
end;

procedure TexDBExpressDriver.CloseConnection;
begin
  inherited;
  FConnection.Close;
end;

procedure TexDBExpressDriver.ExecSQL(ASQL: String; const AParams: array of Variant);
var
  AQuery: TSQLQuery;
begin
  AQuery := TSQLQuery.Create(nil);
  try
    AQuery.SQLConnection := FConnection;
    AQuery.SQL.Text := ASQL;
    AssignParamValues(AQuery.Params, AParams);
    AQuery.ExecSQL;
  finally
    AQuery.Free;
  end;
end;

function TexDBExpressDriver.ExecSQLScalar(ASQL: String; const AParams: array of Variant): Variant;
var
  AQuery: TSQLQuery;
begin
  Result := Unassigned;
  AQuery := TSQLQuery.Create(nil);
  try
    AQuery.SQLConnection := FConnection;
    AQuery.SQL.Text := ASQL;
    AssignParamValues(AQuery.Params, AParams);
    AQuery.Open;

    if (AQuery.FieldCount > 0) and (not AQuery.IsEmpty) then
      Result := AQuery.Fields[0].Value;
  finally
    AQuery.Free;
  end;
end;

function TexDBExpressDriver.CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet;
var
  I: Integer;
  AParam: TParam;
  AField: TField;
  AValue: Variant;
  AType: TFieldType;
begin
  Result := TSQLQuery.Create(Self);
  with (TSQLQuery(Result)) do
  begin
    SQL.Text := ASQL;
    SQLConnection := FConnection;

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
  RegisterComponents('Data Export', [TexDBExpressDriver]);
end;

end.
