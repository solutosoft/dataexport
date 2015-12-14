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

  { TexColumnSerializer }

  TexColumnSerializer = class(TexSerializerBase)
  private
    FDelimiter: String;
  public
    procedure Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap); override;
  published
    property Delimiter: String read FDelimiter write FDelimiter;
  end;

  { TexHierarchicalSerializer }

  TexHierarchicalSerializer = class(TexSerializerBase)
  protected
    function FormatData(ASession: TexSession; AMaster: TDataSet): String; virtual; abstract;
  public
    procedure Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap); override;
  end;

  { TexJsonSerializer }

  TexJsonSerializer = class(TexHierarchicalSerializer)
  private
    FHideRootKeys: Boolean;
  protected
    function FormatData(ASession: TexSession; AMaster: TDataSet): String; override;
  published
    property HideRootKeys: Boolean read FHideRootKeys write FHideRootKeys default False;
  end;


implementation

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
  AArgs: TexScriptArgs;
  AValue: Variant;
begin
  AValue := Null;
  AField := ADataSet.FindField(AColumn.Name);

  AComplete := AColumn.Complete;
  ASize := AColumn.Size;
  AAlign := AColumn.Align;
  AExpression := AColumn.Expression;

  if (AField <> nil) then
    AValue := AField.AsVariant;

  if (AColumn.Dictionary <> '') then
  begin
    ADictionary := Exporter.Dictionaries.FindByName(AColumn.Dictionary);
    if (ADictionary <> nil) then
    begin
      if (AComplete = VK_CHAR_SPACE) then
        AComplete := ADictionary.Complete;

      if (ASize = 0) then
        ASize := ADictionary.Size;

      if (AAlign = altNone) then
         AAlign := ADictionary.Align;

      if (AExpression = '') then
         AExpression := ADictionary.Expression;
    end;
  end;

  if (AExpression <> '') then
  begin
    AArgs := TexScriptArgs.Create;
    try
      AArgs['value'] := AValue;
      AValue := Exporter.ExecuteExpression(AExpression, AArgs);
    finally
      AArgs.Free;
    end;
  end;

  Result := VarToStrDef(AValue, '');
  if (ASize <> 0) then
  begin
    if (ASize < Length(Result)) then
       Result := Copy(Result, 1, ASize)
    else begin
      case AAlign of
        altLeft, altNone:
          Result := AddCharR(AComplete, Result, ASize);
        altRight:
          Result := AddChar(AComplete, Result, ASize);
      end;
    end;
  end;
end;

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

      AQuery := Exporter.Provider.CreateQuery(APipeline.SQL.Text, AMaster);
      Exporter.CurrentDataSet := AQuery;
      try
        AQuery.Open;
        while (not AQuery.EOF) do
        begin
          ARow := '';
          for J := 0 to ASession.Columns.Count -1 do
          begin
            AValue := ExtractValue(ASession.Columns[J], AQuery);
            ARow := ARow + AValue + FDelimiter;
          end;

          ALength := Length(ARow);
          if (FDelimiter <> '') and (ALength > 0) then
             Delete(ARow, ALength, 1);

          AData.Add(ARow);
          Serialize(ASession.Sessions, AQuery, AResult);

          Exporter.CurrentDataSet := AQuery;
          AQuery.Next;
        end;
      finally
        AQuery.Free;
      end;
    end;
  end;
end;

{ TexHierarchicalSerializer }

procedure TexHierarchicalSerializer.Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap);
var
  I: Integer;
  AData: TStrings;
  ASession: TexSession;
begin
  for I := 0 to ASessions.Count - 1 do
  begin
    ASession := ASessions[I];
    AData := FindData(ASession, AResult);
    AData.Add(FormatData(ASession, AMaster));
  end;
end;

{ TexJsonSerializer }

function TexJsonSerializer.FormatData(ASession: TexSession; AMaster: TDataSet): String;
var
  I, J: Integer;
  ARow,
  AValue: String;
  ASame: Boolean;
  AQuery: TDataSet;
  AOwner: TexSession;
  AColumn: TexColumn;
  APipeline: TexPipeline;
  AElements: TStringList;
begin
  Result := '';
  AElements := TStringList.Create;
  try
    AOwner := ASession.Collection.Owner as TexSession;
    APipeline := Exporter.Pipelines.FindByName(ASession.Pipeline);
    ASame := (AOwner <> nil) and (SameText(ASession.Pipeline, AOwner.Pipeline));

    if (ASame) then
      AQuery := AMaster
    else
      AQuery := Exporter.Provider.CreateQuery(APipeline.SQL.Text, AMaster);

    Exporter.CurrentDataSet := AQuery;
    try
      AQuery.Open;
      while (not AQuery.EOF) do
      begin
        ARow := '';
        for J := 0 to ASession.Columns.Count -1 do
        begin
          AColumn := ASession.Columns[J];
          AValue := ExtractValue(AColumn, AQuery);
          ARow := ARow + Format('"%s":"%s"', [AColumn.Name, AValue]) + ',';
        end;

        for I := 0 to ASession.Sessions.Count - 1 do
          ARow := ARow + FormatData(ASession.Sessions[I], AQuery) + ',';

        Delete(ARow, Length(ARow), 1);
        AElements.Add(Format('{%s}', [ARow]));
        Exporter.CurrentDataSet := AQuery;

        if (ASame) then
          break;

        AQuery.Next;
      end;

      Result := '';
      for J := 0 to AElements.Count -1 do
        Result := Result + AElements[J] + ',';

      Delete(Result, Length(Result), 1);

      if (AElements.Count > 1) then
        Result := Format('[%s]', [Result]);

      if (AOwner <> nil) then
        Result := Format('"%s":%s', [ASession.Name, Result])
      else if (not Self.HideRootKeys) then
        Result := Format('{"%s":%s}', [ASession.Name, Result]);
    finally
      if (not ASame) then
        AQuery.Free;
    end;
  finally
    AElements.Free;
  end;

end;

end.

