unit exSerializer;

interface

uses
  Classes, SysUtils, DB, StrUtils, Variants, {$IFDEF FPC}fgl, {$ELSE} Generics.Collections, {$ENDIF}
  exDefinition, exExporter;

type

  { TexRegisteredClassItem }

  TexRegisteredClassItem = class(TCollectionItem)
  private
    FRegisteredClass: TexSerializerClass;
    FDescription: String;
  public
    property RegisteredClass: TexSerializerClass read FRegisteredClass write FRegisteredClass;
    property Description: String read FDescription write FDescription;
  end;

  { TexRegisteredClasses }

  TexRegisteredClasses = class(TCollection)
  private
    function GetItem(Index: Integer): TexRegisteredClassItem;
    procedure SetItem(Index: Integer; AValue: TexRegisteredClassItem);
  public
    constructor Create;
    function Add: TexRegisteredClassItem;
    property Items[Index: Integer]: TexRegisteredClassItem read GetItem write SetItem; default;
    function FindByDescription(ADescription: String): TexRegisteredClassItem;
    function FindByClassType(AClassType: TexSerializerClass): TexRegisteredClassItem;
    function FindByClassName(AClassName: String): TexRegisteredClassItem;
    procedure RegisterClass(ADescription: String; AClass: TexSerializerClass);
  end;

  { TexBaseSerializer }

  TexBaseSerializer = class(TexSerializer)
  protected
    function FindData(ASession: TexSession; AResultMap: TexResutMap): TStrings;
    function ExtractColumnValue(AColumn: TexColumn; ADataSet: TDataSet): String;
    function BeforeSerialize(AData: String; ASession: TexSession): String;
  end;

  { TexColumnSerializer }

  TexColumnSerializer = class(TexBaseSerializer)
  private
    FDelimiter: String;
  public
    procedure Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap); override;
  published
    property Delimiter: String read FDelimiter write FDelimiter;
  end;

  { TexHierarchicalSerializer }

  TexHierarchicalSerializer = class(TexBaseSerializer)
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

  { TexXmlSerializer }

  TexXmlSerializer = class(TexHierarchicalSerializer)
  private
    FEncoding: String;
    FItemTag: String;
    FVersion: String;
  private
    function EncodeTag(AName, AValue: String): String;
  protected
    function FormatData(ASession: TexSession; AMaster: TDataSet): String; override;
  public
    constructor Create(AExporter: TexExporter); override;
    procedure Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap); override;
  published
    property Version: String read FVersion write FVersion;
    property Encoding: String read FEncoding write FEncoding;
    property ItemTag: String read FItemTag write FItemTag;
  end;

const
  sexSColumnSerializer = 'Column';
  sexSJsonSerializer = 'JSON';
  sexSXMLSerializer = 'XML';

var
  FRegisteredSerializers: TexRegisteredClasses;

function GetRegisteredSerializers: TexRegisteredClasses;

implementation

function GetRegisteredSerializers: TexRegisteredClasses;
begin
  if FRegisteredSerializers = nil then
    FRegisteredSerializers := TexRegisteredClasses.Create;
  Result := FRegisteredSerializers;
end;

{ TexRegisteredClasses }

constructor TexRegisteredClasses.Create;
begin
  inherited Create(TexRegisteredClassItem);
end;

function TexRegisteredClasses.GetItem(Index: Integer): TexRegisteredClassItem;
begin
  Result := TexRegisteredClassItem(inherited GetItem(Index));
end;

procedure TexRegisteredClasses.SetItem(Index: Integer; AValue: TexRegisteredClassItem);
begin
  inherited SetItem(Index, AValue);
end;

function TexRegisteredClasses.Add: TexRegisteredClassItem;
begin
  Result := TexRegisteredClassItem(inherited Add);
end;

function TexRegisteredClasses.FindByClassType(AClassType: TexSerializerClass): TexRegisteredClassItem;
var
  I: Integer;
  AItem: TexRegisteredClassItem;
begin
  Result := nil;
  for I := 0 to Self.Count -1 do
  begin
    AItem := Self.Items[I];
    if (AItem.RegisteredClass = AClassType) then
    begin
      Result := AItem;
      Exit;
    end;
  end;
end;

function TexRegisteredClasses.FindByClassName(AClassName: String): TexRegisteredClassItem;
var
  I: Integer;
  AItem: TexRegisteredClassItem;
begin
  Result := nil;
  for I := 0 to Self.Count -1 do
  begin
    AItem := Self.Items[I];
    if (AItem.RegisteredClass <> nil) and (SameText(AItem.RegisteredClass.ClassName, AClassName)) then
    begin
      Result := AItem;
      Exit;
    end;
  end;
end;

function TexRegisteredClasses.FindByDescription(ADescription: String): TexRegisteredClassItem;
var
  I: Integer;
  AItem: TexRegisteredClassItem;
begin
  Result := nil;
  for I := 0 to Self.Count -1 do
  begin
    AItem := Self.Items[I];
    if (SameText(AItem.Description, ADescription)) then
    begin
      Result := AItem;
      Exit;
    end;
  end;
end;

procedure TexRegisteredClasses.RegisterClass(ADescription: String; AClass: TexSerializerClass);
begin
  with Add do
  begin
    Description := ADescription;
    RegisteredClass := AClass;
  end;
end;

{ TexBaseSerializer }

function TexBaseSerializer.FindData(ASession: TexSession; AResultMap: TexResutMap): TStrings;
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
    {$IFDEF FPC}
    if (AResultMap.IndexOf(APackage.Name) <> -1) then
    {$ELSE}
    if (AResultMap.ContainsKey(APackage.Name)) then
    {$ENDIF}
      Result := AResultMap[APackage.Name]
    else begin
      Result := TStringList.Create;
      AResultMap.Add(APackage.Name, Result);
    end;
  end;
end;

function TexBaseSerializer.ExtractColumnValue(AColumn: TexColumn; ADataSet: TDataSet): String;
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
      {$IFDEF FPC}
      AArgs['value'] := AValue;
      {$ELSE}
      AArgs.AddOrSetValue('value', AValue);
      {$ENDIF}
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
          Result := {$IFDEF FPC}AddCharR(AComplete, Result, ASize){$ELSE} Result.PadRight(ASize, AComplete){$ENDIF};
        altRight:
          Result := {$IFDEF FPC}AddChar(AComplete, Result, ASize){$ELSE} Result.PadLeft(ASize, AComplete){$ENDIF};
      end;
    end;
  end;
end;

function TexBaseSerializer.BeforeSerialize(AData: String; ASession: TexSession): String;
var
  AEvent: TexVariable;
  AParams: TexScriptArgs;
begin
  AParams := TexScriptArgs.Create;
  try
    {$IFDEF FPC}
    AParams['Value'] := AData;
    {$ELSE}
    AParams.AddOrSetValue('Value', AData);
    {$ENDIF}
    AEvent := Exporter.Events.FindByName(EVENT_BEFORE_SERIALIZE);
    if (AEvent <> nil) then
      Result := Exporter.ExecuteExpression(AEvent.Expression, AParams)
    else
      Result := AData;
  finally
    AParams.Free;
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
      try
        AQuery.Open;
        while (not AQuery.EOF) do
        begin
          ARow := '';
          Exporter.CurrentDataSet := AQuery;

          for J := 0 to ASession.Columns.Count -1 do
          begin
            AValue := ExtractColumnValue(ASession.Columns[J], AQuery);
            ARow := ARow + AValue + FDelimiter;
          end;

          ALength := Length(ARow);
          if (FDelimiter <> '') and (ALength > 0) then
             Delete(ARow, ALength, 1);

          ARow := BeforeSerialize(ARow, ASession);
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
  AJson,
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

    try
      AQuery.Open;
      while (not AQuery.EOF) do
      begin
        AJson := '';
        Exporter.CurrentDataSet := AQuery;

        for J := 0 to ASession.Columns.Count -1 do
        begin
          AColumn := ASession.Columns[J];
          AValue := ExtractColumnValue(AColumn, AQuery);
          AJson := AJson + Format('"%s":"%s"', [AColumn.Name, AValue]) + ',';
        end;

        for I := 0 to ASession.Sessions.Count - 1 do
          AJson := AJson + FormatData(ASession.Sessions[I], AQuery) + ',';

        Delete(AJson, Length(AJson), 1);
        AElements.Add(Format('{%s}', [AJson]));

        if (ASame) then
          Break;

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

{ TexXmlSerializer }

constructor TexXmlSerializer.Create(AExporter: TexExporter);
begin
  inherited Create(AExporter);
  FVersion := '1.0';
  FEncoding := 'UTF-8';
  FItemTag := 'item';
end;

procedure TexXmlSerializer.Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap);
var
  {$IFDEF FPC}
  I: Integer;
  {$ENDIF}
  AData: TStrings;
  AKey: String;
begin
  inherited Serialize(ASessions, AMaster, AResult);
  {$IFDEF FPC}
  for I := 0 to AResult.Count -1 do
  begin
    AKey := AResult.Keys[I];
  {$ELSE}
  for AKey in AResult.Keys do
  begin
  {$ENDIF}
    AData := AResult[AKey];
    AData.Insert(0, Format('<?xml version="%s" encoding="%s"?>', [FVersion, FEncoding]));
  end;
end;

function TexXmlSerializer.EncodeTag(AName, AValue: String): String;
begin
  Result := Format('<%s>%s</%s>', [AName, AValue, AName]);
end;

function TexXmlSerializer.FormatData(ASession: TexSession; AMaster: TDataSet): String;
var
  I, J: Integer;
  AXml,
  AValue: String;
  ASame: Boolean;
  AQuery: TDataSet;
  AOwner: TexSession;
  AColumn: TexColumn;
  APipeline: TexPipeline;
begin
  Result := '';
  AOwner := ASession.Collection.Owner as TexSession;
  APipeline := Exporter.Pipelines.FindByName(ASession.Pipeline);
  ASame := (AOwner <> nil) and (SameText(ASession.Pipeline, AOwner.Pipeline));

  if (ASame) then
    AQuery := AMaster
  else
    AQuery := Exporter.Provider.CreateQuery(APipeline.SQL.Text, AMaster);

  try
    AQuery.Open;
    while (not AQuery.EOF) do
    begin
      AXml := '';
      Exporter.CurrentDataSet := AQuery;

      for J := 0 to ASession.Columns.Count -1 do
      begin
        AColumn := ASession.Columns[J];
        AValue := ExtractColumnValue(AColumn, AQuery);
        AXml := AXml + EncodeTag(AColumn.Name, AValue);
      end;

      for I := 0 to ASession.Sessions.Count - 1 do
        AXml := AXml + FormatData(ASession.Sessions[I], AQuery);

      if (not ASame) then
        AXml := EncodeTag(FItemTag, AXml)
      else begin
        Result := Result + AXml;
        Break;
      end;

      Result := Result + AXml;
      AQuery.Next;
    end;
    Result := EncodeTag(ASession.Name, Result);
  finally
    if (not ASame) then
      AQuery.Free;
  end;
end;


initialization
  GetRegisteredSerializers.RegisterClass(sexSColumnSerializer, TexColumnSerializer);
  GetRegisteredSerializers.RegisterClass(sexSJsonSerializer, TexJsonSerializer);
  GetRegisteredSerializers.RegisterClass(sexSXMLSerializer, TexXmlSerializer);

end.

