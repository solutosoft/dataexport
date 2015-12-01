unit exSerializer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, StrUtils, Variants, exDefinition, exExporter;

type

  { TexSerializerBase }

  TexSerializerBase = class(TexSerializer)
  protected
    function ExtractValue(AColumn: TexColumn; ADataSet: TDataSet): String;
  end;

  TexSerializerBaseClass = class of TexSerializerBase;

  { TexColumnSerializer }

  TexColumnSerializer = class(TexSerializerBase)
  private
    FDelimiter: String;
  public
    function Serialize(ASessions: TexSessionList; AMaster: TDataSet): TStrings; override;
    function FormatData(AData: TStrings): TStrings; override;
  published
    property Delimiter: String read FDelimiter write FDelimiter;
  end;



implementation

{ TexColumnSerializer }

function TexColumnSerializer.Serialize(ASessions: TexSessionList; AMaster: TDataSet): TStrings;
var
  I, J,
  ALength: Integer;
  ARow,
  AValue: String;
  AQuery: TDataSet;
  ASession: TexSession;
  APipeline: TexPipeline;
begin
  Result := TStringList.Create;

  for I := 0 to ASessions.Count -1 do
  begin
    ASession := ASessions[I];
    if (ASession.Visible) then
    begin
      APipeline := Exporter.Pipelines.FindByName(ASession.Pipeline);
      AQuery := Exporter.Provider.CreateQuery(APipeline.SQL.Text, Exporter.Parameters, AMaster);
      try
        AQuery.Open;
        while (not AQuery.EOF) do
        begin
          ARow := '';
          for J := 0 to ASession.Columns.Count - 1 do
          begin
            AValue := ExtractValue(ASession.Columns[J], AQuery);
            ARow := ARow + AValue + FDelimiter;
          end;

          ALength := Length(ARow);
          if (FDelimiter <> '') and (ALength > 0) then
             Delete(ARow, ALength, 1);

          Result.Add(ARow);
          Result.AddStrings(Serialize(ASession.Sessions, AQuery));
          AQuery.Next;
        end;
      finally
        AQuery.Free;
      end;
    end;
  end;
end;

function TexColumnSerializer.FormatData(AData: TStrings): TStrings;
begin
  Result := AData;
end;

{ TexSerializerBase }

function TexSerializerBase.ExtractValue(AColumn: TexColumn; ADataSet: TDataSet): String;
var
  ASize: Integer;
  AComplete: Char;
  AExpression: String;
  AAlign: TexAlignment;
  AField: TField;
  ADictionary: TexDictionary;
begin
  AField := ADataSet.FindField(AColumn.Name);

  AComplete := AColumn.Complete;
  ASize := AColumn.Size;
  AAlign := AColumn.Align;
  AExpression := AColumn.Expression;

  if (AField <> nil) then
    Result := AField.AsString;

  if (AColumn.Dictionary <> '') then
  begin
    ADictionary := Exporter.Dictionaries.FindByName(AColumn.Dictionary);
    if (ADictionary <> nil) then
    begin
      if (AComplete = '') then
        AComplete := ADictionary.Complete;

      if (ASize = 0) then
        ASize := ADictionary.Size;

      if (AAlign = altNone) then
         AAlign := ADictionary.Align;
    end;
  end;

  if (ASize <> 0) then
  begin
    if (ASize < Length(Result)) then
       Result := Copy(Result, 1, ASize)
    else begin
      case AAlign of
        altLeft:
          Result := AddCharR(AComplete, Result, ASize);
        altRight:
          Result := AddChar(AComplete, Result, ASize);
      end;
    end;
  end;

end;

end.

