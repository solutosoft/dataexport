unit exFireDACProvider;

interface

uses
  Classes, SysUtils, DB, Variants, exExporter, FireDAC.Comp.Client, FireDAC.Stan.Param;

type

  { TexFireDACProvider }

  TexFireDACProvider = class(TexProvider)
  private
    FConnection: TFDConnection;
  public
    function CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet; override;
    procedure OpenConnection; override;
    procedure CloseConnection; override;
  published
    property Connection: TFDConnection read FConnection write FConnection;
  end;

procedure Register;

implementation

{ TexFireDACProvider }

procedure TexFireDACProvider.OpenConnection;
begin
  inherited;
  FConnection.Open;
end;

procedure TexFireDACProvider.CloseConnection;
begin
  inherited;
  FConnection.Close;
end;

function TexFireDACProvider.CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet;
var
  I: Integer;
  AParam: TFDParam;
  AField: TField;
  AValue: Variant;
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
  RegisterComponents('Data Export', [TexFireDACProvider]);
end;

end.
