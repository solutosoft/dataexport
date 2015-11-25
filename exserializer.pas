unit exSerializer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, StrUtils, Variants, exDefinition;

type

  { TexSerializer }

  TexSerializer = class(TComponent)
  protected
    function ExtractValue(AColumn: TexColumn; ADataSet: TDataSet): String;
  public
    function Serialize(ASessions: TexSessionList; AMaster: TDataSet): TStrings; virtual; abstract;
    function FormatData(AData: TStrings): TStrings; virtual; abstract;
  end;

  TexSerializerClass = class of TexSerializer;

  { TexColumnSerializer }

  TexColumnSerializer = class(TexSerializer)
  private
    FDelimiter: String;
  public
    function Serialize(ASessions: TexSessionList; AMaster: TDataSet): TStrings; override;
    function FormatData(AData: TStrings): TStrings; override;
  published
    property Delimiter: String read FDelimiter write FDelimiter;
  end;



implementation

uses
  exExporter;

{ TexColumnSerializer }

function TexColumnSerializer.Serialize(ASessions: TexSessionList; AMaster: TDataSet): TStrings;
var
  I: Integer;
  ASession: TexSession;
  AQuery: TDataSet;
  AExporter: TexExporter;
begin
  Result := TStringList.Create;
  AExporter := GetOwner as TexExporter;

  {AClass := TexDriverAccess.Instance.FindRegisteredClass(AExporter.Properties.Driver);
  ADataAccess := AClass.Create(AExporter) as IexDataAccess;

  try
    for I := 0 to ASessions.Count -1 do
    begin
      if (ASession.Visible) then
      begin
        AProvider := AExporter.Providers.FindByName(ASession.Provider);
        AQuery := ADataAccess.CreateQuery(AExporter, AProvider, AMaster);

      end;
    end;
  finally
    ADataAccess.Free;
  end;     }
end;

function TexColumnSerializer.FormatData(AData: TStrings): TStrings;
begin

end;

{ TexSerializer }

function TexSerializer.ExtractValue(AColumn: TexColumn; ADataSet: TDataSet): String;
var
  ASize: Integer;
  AComplete: Char;
  AExpression: String;
  AAlign: TAlignment;
  AField: TField;
  AValue: Variant;
  ADictionary: TexDictionary;
  AExporter: TexExporter;
begin
  AExporter := GetOwner as TexExporter;
  AField := ADataSet.FindField(AColumn.Name);

  if (AField <> nil) then
    AValue := AField.Value;


  if (AColumn.Dictionary <> '') then
  begin
    ADictionary := AExporter.Dictionaries.FindByName(AColumn.Dictionary);
    if (ADictionary <> nil) then
    begin
      if (AComplete = '') then
        AComplete := ADictionary.Complete;

      if (ASize = 0) then
        ASize := ADictionary.Size;
    end;
  end;

  if (VarIsClear(AValue)) then
    Result := '';

end;

end.

