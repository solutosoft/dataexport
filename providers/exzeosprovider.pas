unit exZeosProvider;

interface

uses
  Classes, SysUtils, DB, Variants, ZConnection, ZDataset, exExporter;

type

  { TexZeosProvider }

  TexZeosProvider = class(TexProvider)
  private
    FConnection: TZConnection;
  public
    function CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet; override;
    procedure OpenConnection; override;
    procedure CloseConnection; override;

  published
    property Connection: TZConnection read FConnection write FConnection;
  end;

procedure Register;

implementation

{ TexZeosProvider }

function TexZeosProvider.CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet;
var
  I: Integer;
  AField: TField;
  AParam: TParam;
  AValue: Variant;
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
        AParam.Value := AField.Value
      else begin
        AValue := Exporter.ExtractParamValue(AParam.Name);

        if (AValue <> Unassigned) then
        begin
          case (VarType(AValue)) of
            varsmallint, varinteger, varsingle, varshortint, varword, varlongword, varint64{$IFDEF FPC},varqword{$ENDIF}:
              AParam.AsInteger := AValue;
            vardouble{$IFDEF FPC}, vardecimal{$ENDIF}:
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

