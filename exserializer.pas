unit exSerializer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, StrUtils, Variants, exDefinition, exExporter;

type

  { TexSerializerBase }

  TexSerializerBase = class(TexSerializer)
  protected
    function FindData(ASession: TexSession; AResultMap: TexResutMap): TStrings;
    function ExtractValue(AColumn: TexColumn; ADataSet: TDataSet): String;
  end;

  TexSerializerBaseClass = class of TexSerializerBase;

  { TexColumnSerializer }

  TexColumnSerializer = class(TexSerializerBase)
  private
    FDelimiter: String;
  public
    procedure Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap); override;
  published
    property Delimiter: String read FDelimiter write FDelimiter;
  end;



implementation

{ TexColumnSerializer }

procedure TexColumnSerializer.Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap);
var
  I, J,
  ALength: Integer;
  ARow,
  AValue: String;
  AData: TStrings;
  AQuery: TDataSet;
  ASession: TexSession;
  APipeline: TexPipeline;
begin
  for I := 0 to ASessions.Count -1 do
  begin
    ASession := ASessions[I];
    if (ASession.Visible) then
    begin
      AData := FindData(ASession, AResult);
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

          AData.Add(ARow);
          Serialize(ASession.Sessions, AQuery, AResult);
          AQuery.Next;
        end;
      finally
        AQuery.Free;
      end;
    end;
  end;
end;

{ TexSerializerBase }

function TexSerializerBase.FindData(ASession: TexSession; AResultMap: TexResutMap): TStrings;
var
  APackage: TexPackage;
  AOwner: TexSession;
  ASessionName: String;
begin
  AOwner := TexSessionList(ASession.Collection).GetOwner as TexSession;

  if(AOwner <> nil) then
    ASessionName := AOwner.Name
  else
    ASessionName := ASession.Name;

  APackage := Exporter.Packages.FindBySession(ASessionName);
  if (APackage = nil) and (AOwner = nil) then
     raise Exception.CreateFmt('The session "%s" does not have a file associated', [ASession.Name])
  else begin
    if (AResultMap.IndexOf(APackage.Name) <> -1) then
      Result := AResultMap[APackage.Name]
    else begin
      Result := TStringList.Create;
      AResultMap.Add(APackage.Name, Result);
    end;
  end;
end;

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

