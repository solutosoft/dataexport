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
    function CreateQuery(ASQL: String; AParameters: TexParameterList; AMaster: TDataSet): TDataSet; override;
    procedure OpenConnection; override;
    procedure CloseConnection; override;
  published
    property Connection: TZConnection read FConnection write FConnection;
  end;

procedure Register;

implementation

{ TexZeosProvider }

function TexZeosProvider.CreateQuery(ASQL: String; AParameters: TexParameterList; AMaster: TDataSet): TDataSet;
var
  I: Integer;
  AQuery: TZQuery;
  AField: TField;
  AParam: TParam;
  AParameter: TexParameter;
begin
  AQuery := TZQuery.Create(Self);
  AQuery.SQL.Text := ASQL;

  if (AMaster <> nil) then
  begin
    for I := 0 to AQuery.Params.Count -1 do
    begin
      AParam := AQuery.Params[I];
      AField := AMaster.FindField(AParam.Name);
      if (AField <> nil) then
        AParam.Value := AField.Value;
      {else begin
         AParameter := AParameters.FindByName(AParam.Name);
         if (AParameter <> nil) then
            AParam.Value := EvaluateExpression(AParameter.Expression;
      end;}
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

