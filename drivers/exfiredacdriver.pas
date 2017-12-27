unit exfiredacdriver;

interface

uses
  Classes, SysUtils, DB, Variants, exExporter, FireDAC.Comp.Client, FireDAC.Stan.Param;

type

  { TexFireDACProvider }

  TexFireDACDriver = class(TexDriver)
  private
    FConnection: TFDConnection;
  protected
    procedure AssignParamValues(AParams: TFDParams; const AValues: array of Variant);
  public
    procedure OpenConnection; override;
    procedure CloseConnection; override;
    procedure ExecSQL(ASQL: String; const AParams: array of Variant); override;
    function ExecSQLScalar(ASQL: String; const AParams: array of Variant): Variant; override;
    function CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet; override;
  published
    property Connection: TFDConnection read FConnection write FConnection;
  end;

procedure Register;

implementation

{ TexFireDACDriver }

procedure TexFireDACDriver.OpenConnection;
begin
  inherited;
  FConnection.Open;
end;

procedure TexFireDACDriver.AssignParamValues(AParams: TFDParams; const AValues: array of Variant);
var
  I: Integer;
  AType: TFieldType;
begin
  for I := Low(AValues) to High(AValues) do
  begin
    AType := VarTypeToDataType(VarType(AValues[I]));

    if (AType <> ftUnknown) then
      AParams[I].DataType := AType;

    AParams[I].Value := AValues[I];
  end;
end;

procedure TexFireDACDriver.CloseConnection;
begin
  inherited;
  FConnection.Close;
end;

procedure TexFireDACDriver.ExecSQL(ASQL: String; const AParams: array of Variant);
var
  AQuery: TFDQuery;
begin
  AQuery := TFDQuery.Create(nil);
  try
    AQuery.Connection := FConnection;
    AQuery.SQL.Text := ASQL;
    AssignParamValues(AQuery.Params, AParams);
    AQuery.ExecSQL;
  finally
    AQuery.Free;
  end;
end;

function TexFireDACDriver.ExecSQLScalar(ASQL: String; const AParams: array of Variant): Variant;
var
  AQuery: TFDQuery;
begin
  Result := Unassigned;
  AQuery := TFDQuery.Create(nil);
  try
    AQuery.Connection := FConnection;
    AQuery.SQL.Text := ASQL;
    AssignParamValues(AQuery.Params, AParams);
    AQuery.Open;

    if (AQuery.FieldCount > 0) and (not AQuery.IsEmpty) then
      Result := AQuery.Fields[0].Value;
  finally
    AQuery.Free;
  end;
end;

function TexFireDACDriver.CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet;
var
  I: Integer;
  AParam: TFDParam;
  AField: TField;
  AValue: Variant;
  AType: TFieldType;
begin
  Result := TFDQuery.Create(Self);
  with (TFDQuery(Result)) do
  begin
    SQL.Text := ASQL;
    Connection := FConnection;

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
  RegisterComponents('Data Export', [TexFireDACDriver]);
end;

end.
