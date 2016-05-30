unit exZeosProvider;

interface

uses
  Classes, SysUtils, DB, Variants, ZConnection, ZDataset, exClasses, exExporter;

type

  { TexZeosProvider }

  TexZeosProvider = class(TexProvider)
  private
    FConnection: TZConnection;
  public
    procedure OpenConnection; override;
    procedure CloseConnection; override;
    procedure ExecSQL(ASQL: String; const AParams: array of Variant); override;
    function ExecSQLScalar(ASQL: String; const AParams: array of Variant): Variant; override;
    function CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet; override;
  published
    property Connection: TZConnection read FConnection write FConnection;
  end;

procedure Register;

implementation

{ TexZeosProvider }

procedure TexZeosProvider.OpenConnection;
begin
  FConnection.Connected := True;
end;

procedure TexZeosProvider.CloseConnection;
begin
  FConnection.Connected := False;
end;

function TexZeosProvider.CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet;
var
  I: Integer;
  AField: TField;
  AParam: TParam;
  AValue: Variant;
  AType: TFieldType;
begin
  Result := TZQuery.Create(Self);
  with (TZQuery(Result)) do
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

procedure TexZeosProvider.ExecSQL(ASQL: String; const AParams: array of Variant);
var
  AQuery: TZQuery;
begin
  AQuery := TZQuery.Create(nil);
  try
    AQuery.Connection := FConnection;
    AQuery.SQL.Text := ASQL;
    AssignParamValues(AQuery.Params, AParams);
    AQuery.ExecSQL;
  finally
    AQuery.Free;
  end;
end;

function TexZeosProvider.ExecSQLScalar(ASQL: String; const AParams: array of Variant): Variant;
var
  AQuery: TZQuery;
begin
  Result := Unassigned;
  AQuery := TZQuery.Create(nil);
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

procedure Register;
begin
  RegisterComponents('Data Export', [TexZeosProvider]);
end;

end.

