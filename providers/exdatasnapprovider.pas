unit exDataSnapProvider;

interface

uses
  Classes, SysUtils, DB, Variants, exExporter, DBClient, DSConnect;

type

  { TexDataSnapProvider }

  TexDataSnapProvider = class(TexProvider)
  private
    FConnection: TDSProviderConnection;
    FProviderName: String;
  public
    function CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet; override;
    procedure OpenConnection; override;
    procedure CloseConnection; override;
  published
    property Connection: TDSProviderConnection read FConnection write FConnection;
    property ProviderName: String read FProviderName write FProviderName;
  end;

procedure Register;

implementation

{ TexDataSnapProvider }

procedure TexDataSnapProvider.OpenConnection;
begin
  inherited;
  FConnection.Open;
end;

procedure TexDataSnapProvider.CloseConnection;
begin
  inherited;
  FConnection.Close;
end;

function TexDataSnapProvider.CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet;
var
  I: Integer;
  AParam: TParam;
  AField: TField;
  AValue: Variant;
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
        AParam.Value := AField.Value
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
  RegisterComponents('Data Export', [TexDataSnapProvider]);
end;

end.
