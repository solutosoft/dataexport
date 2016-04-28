unit exDBExpressProvider;

interface

uses
  Classes, SysUtils, DB, Variants, exExporter, SqlExpr;

type
  { TexDBExpressProvider }

  TexDBExpressProvider = class(TexProvider)
  private
    FConnection: TSQLConnection;
  public
    function CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet; override;
    procedure OpenConnection; override;
    procedure CloseConnection; override;
  published
    property Connection: TSQLConnection read FConnection write FConnection;
  end;

procedure Register;

implementation

{ TexDBExpressProvider }

procedure TexDBExpressProvider.OpenConnection;
begin
  inherited;
  FConnection.Open;
end;

procedure TexDBExpressProvider.CloseConnection;
begin
  inherited;
  FConnection.Close;
end;

function TexDBExpressProvider.CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet;
var
  I: Integer;
  AParam: TParam;
  AField: TField;
  AValue: Variant;
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
          case (VarType(AValue)) of
            varsmallint, varinteger, varsingle, varshortint, varword, varlongword, varint64:
              AParam.AsInteger := AValue;
            vardouble:
              AParam.AsFloat := AValue;
            varcurrency:
              AParam.Value := AValue;
            vardate:
              AParam.AsDateTime := AValue;
            varboolean:
              AParam.AsBoolean := AValue;
            varstring, varustring :
              AParam.AsString := AValue;
            else
              AParam.Value := AValue;
          end;
        end;
      end;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Data Export', [TexDBExpressProvider]);
end;

end.
