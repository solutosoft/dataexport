unit exSerializer;

interface

uses
  Classes, SysUtils, DB, StrUtils, Variants,
  {$IFDEF FPC}fgl, {$ELSE} Generics.Collections, RegularExpressions, {$ENDIF}
  exClasses, exDefinition, exExporter;

type
  TexExporterAccess = class(TexExporter);

  { TexBaseSerializer }

  TexBaseSerializer = class(TexSerializer)
  protected
    procedure DoWork;
    procedure DoSerializeData(APackage: TexPackage; AData: WideString);
    function ExtractDataError(ADataSet: TDataSet): String;
    function FindData(ASession: TexSession; AResultMap: TexResutMap): TStrings;
    function ExtractColumnValue(AColumn: TexColumn; ADataSet: TDataSet): String;
    function BeforeSerialize(AData: String; ASession: TexSession): String;
    function ExecuteEvent(AEvent: TexVariable; AArgs: TexScriptArgs = nil): Variant;
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

  TexSQLInsertSerializer = class(TexBaseSerializer)
  public
    procedure Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap); override;
  end;

const
  sexSColumnSerializer = 'Column';
  sexSJsonSerializer = 'JSON';
  sexSXMLSerializer = 'XML';
  sexSSQLInsertSerializer = 'SQLInsert';

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

{ TexBaseSerializer }

procedure TexBaseSerializer.DoSerializeData(APackage: TexPackage; AData: WideString);
begin
  try
    if (Assigned(OnSerializeData)) then
      OnSerializeData(APackage, AData);
  finally
    TexExporterAccess(Exporter).FObjectCounter := 0;
  end;
end;

procedure TexBaseSerializer.DoWork;
begin
  if (Assigned(OnWork)) then
    OnWork(Exporter);
end;

function TexBaseSerializer.FindData(ASession: TexSession; AResultMap: TexResutMap): TStrings;
var
  APackage: TexPackage;
  AOwner: TexSession;
  ASessionName: String;
  AArgs: TexScriptArgs;
begin
  AOwner := TexSessionList(ASession.Collection).GetOwner as TexSession;

  if(AOwner <> nil) then
    ASessionName := AOwner.Name
  else
    ASessionName := ASession.Name;

  APackage := Exporter.Packages.FindBySession(ASessionName);
  if (APackage = nil) and (AOwner = nil) then
     raise Exception.CreateFmt('The session "%s" does not have a package associated', [ASession.Name])
  else begin
    {$IFDEF FPC}
    if (AResultMap.IndexOf(APackage.Name) <> -1) then
    {$ELSE}
    if (AResultMap.ContainsKey(APackage.Name)) then
    {$ENDIF}
      Result := AResultMap[APackage.Name]
    else begin
      AArgs := TexScriptArgs.Create;
      try
        AArgs.Add(TexScriptVar.Create('AParams', APackage.Options));
        ExecuteEvent(APackage.Events.FindByName(PACKAGE_BEFORE_EXEC), AArgs);

        Result := TStringList.Create;
        AResultMap.Add(APackage.Name, Result);
      finally
        AArgs.Free;
      end;
    end;
  end;
end;

function TexBaseSerializer.ExecuteEvent(AEvent: TexVariable; AArgs: TexScriptArgs): Variant;
begin
  Result := Unassigned;
  if (AEvent <> nil) and (Trim(AEvent.Expression) <> '') then
    Result := Exporter.ExecuteExpression(AEvent.Expression, AArgs);
end;

function TexBaseSerializer.ExtractColumnValue(AColumn: TexColumn; ADataSet: TDataSet): String;
var
  ASize: Integer;
  AComplete: Char;
  AAlign: TexAlignment;
  AField: TField;
  ADictionary: TexDictionary;
  AArgs: TexScriptArgs;
  AValue: Variant;
begin
  AValue := Null;
  ADictionary := nil;
  AField := ADataSet.FindField(AColumn.Name);

  AComplete := AColumn.Complete;
  ASize := AColumn.Size;
  AAlign := AColumn.Align;

  if (AField <> nil) then
  begin
    Result := AField.AsString;
    AValue := AField.Value;
  end;

  if (AColumn.Expression <> '') then
  begin
    AArgs := TexScriptArgs.Create;
    try
      AArgs.Add(TexScriptVar.Create('value', AValue));
      AValue := Exporter.ExecuteExpression(AColumn.Expression, AArgs);
    finally
      AArgs.Free;
    end;
  end;

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

      if (ADictionary.Expression <> '') then
      begin
        AArgs := TexScriptArgs.Create;
        try
          AArgs.Add(TexScriptVar.Create('value', AValue));
          AValue := Exporter.ExecuteExpression(ADictionary.Expression, AArgs);
        finally
          AArgs.Free;
        end;
      end;
    end;
  end;

  if (AColumn.Expression <> '') or ((ADictionary <> nil) and (ADictionary.Expression <> '')) then
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

function TexBaseSerializer.ExtractDataError(ADataSet: TDataSet): String;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to ADataSet.FieldCount -1 do
  begin
    Result := Result + Format('{"%s":"%s"},', [
      ADataSet.Fields[I].FieldName,
      ADataSet.Fields[I].AsString
    ]);
  end;

  Delete(Result, Length(Result), 1);
  Result := Format('[%s]', [Result]);
end;

function TexBaseSerializer.BeforeSerialize(AData: String; ASession: TexSession): String;
var
  AArgs: TexScriptArgs;
  AEventResult: Variant;
begin
  AArgs := TexScriptArgs.Create;
  try
    AArgs.Add(TexScriptVar.Create('Value', AData));
    AEventResult := Exporter.ExecuteEvent(EXPORTER_BEFORE_SERIALIZE, AArgs);
    if (AEventResult <> Unassigned) then
      Result := AEventResult
    else
      Result := AData;
  finally
    AArgs.Free;
  end;
end;

{ TexColumnSerializer }

procedure TexColumnSerializer.Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap);
var
  I, J,
  ALength: Integer;
  ARow,
  AValue: String;
  ASame: Boolean;
  AData: TStrings;
  AQuery: TDataSet;
  ASession: TexSession;
  AProvider: TexProvider;
  AOwner: TexSession;
begin
  for I := 0 to ASessions.Count -1 do
  begin
    ASession := ASessions[I];
    if (ASession.Visible) then
    begin
      AData := FindData(ASession, AResult);
      AProvider := Exporter.Providers.FindByName(ASession.Provider);

      AOwner := ASession.Collection.Owner as TexSession;
      ASame := (AOwner <> nil) and (SameText(ASession.Provider, AOwner.Provider));

      if (ASame) then
        AQuery := AMaster
      else
        AQuery := Exporter.Driver.CreateQuery(AProvider.SQL.Text, AMaster);

      try
        AQuery.Open;
        while (not AQuery.EOF) do
        begin
          ARow := '';

          try
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

            if (ASessions.Owner = nil) then
              DoWork;

            if (ASame) then
              Break;
          except
            on E: Exception do
              raise ESerializeException.Create('TexColumnSerializer error', ExtractDataError(AQuery), E);
          end;

          AQuery.Next;
        end;
      finally
        if (not ASame) then
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
    if (ASession.Visible) then
    begin
      AData := FindData(ASession, AResult);
      AData.Add(FormatData(ASession, AMaster));

      if (ASessions.Owner = nil) then
        DoWork;
    end;
  end;
end;

{ TexJsonSerializer }

function TexJsonSerializer.FormatData(ASession: TexSession; AMaster: TDataSet): String;
var
  I, J: Integer;
  AJson,
  AAlias,
  AValue: String;
  ASame: Boolean;
  AQuery: TDataSet;
  AOwner: TexSession;
  AColumn: TexColumn;
  AProvider: TexProvider;
  AElements: TStringList;
  APackage: TexPackage;
begin
  Result := '';
  AElements := TStringList.Create;
  try
    AOwner := ASession.Collection.Owner as TexSession;
    AProvider := Exporter.Providers.FindByName(ASession.Provider);
    ASame := (AOwner <> nil) and (SameText(ASession.Provider, AOwner.Provider));

    if (ASame) then
      AQuery := AMaster
    else
      AQuery := Exporter.Driver.CreateQuery(AProvider.SQL.Text, AMaster);

    try
      AQuery.Open;
      while (not AQuery.EOF) do
      begin
        AJson := '';
        Exporter.CurrentDataSet := AQuery;
        try
          for J := 0 to ASession.Columns.Count -1 do
          begin
            AColumn := ASession.Columns[J];

            AValue := ExtractColumnValue(AColumn, AQuery);
            AValue := TRegEx.Replace(AValue, '\r\n', '\n');

            AAlias := IfThen(Trim(AColumn.Alias) <> '', AColumn.Alias, AColumn.Name);
            AJson := AJson + Format('"%s":"%s"', [AAlias, AValue]) + ',';
          end;

          for I := 0 to ASession.Sessions.Count - 1 do
            AJson := AJson + FormatData(ASession.Sessions[I], AQuery) + ',';

          Delete(AJson, Length(AJson), 1);
          AJson := Format('{%s}', [AJson]);
          AElements.Add(AJson);

          if (ASame) then
            Break;

          if (AOwner = nil) then
          begin
            APackage := Exporter.Packages.FindBySession(ASession.Name);
            DoSerializeData(APackage, AJson);
          end;

          AQuery.Next;
        except
          on E: Exception do
            raise ESerializeException.Create('TexJsonSerializer error', ExtractDataError(AQuery), E);
        end;
      end;

      Result := '';
      for J := 0 to AElements.Count -1 do
        Result := Result + AElements[J] + ',';

      Delete(Result, Length(Result), 1);

      if (not ASame) then
        Result := Format('[%s]', [Result]);

      AAlias := IfThen(Trim(ASession.Alias) <> '', ASession.Alias, ASession.Name);

      if (AOwner <> nil) then
        Result := Format('"%s":%s', [AAlias, Result])
      else if (not Self.HideRootKeys) then
        Result := Format('{"%s":%s}', [AAlias, Result]);
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
  AAlias,
  AValue: String;
  ASame: Boolean;
  AQuery: TDataSet;
  AOwner: TexSession;
  AColumn: TexColumn;
  AProvider: TexProvider;
  APackage: TexPackage;
begin
  Result := '';
  AOwner := ASession.Collection.Owner as TexSession;
  AProvider := Exporter.Providers.FindByName(ASession.Provider);
  ASame := (AOwner <> nil) and (SameText(ASession.Provider, AOwner.Provider));

  if (ASame) then
    AQuery := AMaster
  else
    AQuery := Exporter.Driver.CreateQuery(AProvider.SQL.Text, AMaster);

  try
    AQuery.Open;
    while (not AQuery.EOF) do
    begin
      try
        AXml := '';
        Exporter.CurrentDataSet := AQuery;

        for J := 0 to ASession.Columns.Count -1 do
        begin
          AColumn := ASession.Columns[J];
          AAlias := IfThen(Trim(AColumn.Alias) <> '', AColumn.Alias, AColumn.Name);
          AValue := ExtractColumnValue(AColumn, AQuery);
          AXml := AXml + EncodeTag(AAlias, AValue);
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

        if (AOwner = nil) then
        begin
          APackage := Exporter.Packages.FindBySession(ASession.Name);
          DoSerializeData(APackage, Result);
        end;

        AQuery.Next;
      except
        on E: Exception do
          raise ESerializeException.Create('TexXmlSerializer error', ExtractDataError(AQuery), E);
      end;
    end;
    Result := EncodeTag(ASession.Name, Result);
  finally
    if (not ASame) then
      AQuery.Free;
  end;
end;


{ TexSQLInsertSerializer }

procedure TexSQLInsertSerializer.Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap);
const
  Delimiter = ',';
var
  I, J: Integer;
  AValues,
  AColumns,
  AAlias,
  AValue,
  ACommand: String;
  ASame: Boolean;
  AData: TStrings;
  AColumn: TexColumn;
  AQuery: TDataSet;
  ASession: TexSession;
  AProvider: TexProvider;
  AOwner: TexSession;
begin
  for I := 0 to ASessions.Count -1 do
  begin
    ASession := ASessions[I];
    if (ASession.Visible) then
    begin
      AData := FindData(ASession, AResult);
      AProvider := Exporter.Providers.FindByName(ASession.Provider);
      AOwner := ASession.Collection.Owner as TexSession;
      ASame := (AOwner <> nil) and (SameText(ASession.Provider, AOwner.Provider));

      if (ASame) then
        AQuery := AMaster
      else
        AQuery := Exporter.Driver.CreateQuery(AProvider.SQL.Text, AMaster);

      try
        AQuery.Open;
        while (not AQuery.EOF) do
        begin
          try
            AValues := '';
            AColumns := '';
            Exporter.CurrentDataSet := AQuery;

            for J := 0 to ASession.Columns.Count -1 do
            begin
              AColumn := ASession.Columns[J];
              AAlias := IfThen(Trim(AColumn.Alias) <> '', AColumn.Alias, AColumn.Name);
              AValue := ExtractColumnValue(AColumn, AQuery);
              AValues := AValues + AValue + Delimiter;
              AColumns := AColumns + AAlias + Delimiter;
            end;

            Delete(AValues, Length(AValues), 1);
            Delete(AColumns, Length(AColumns), 1);

            AAlias := IfThen(Trim(ASession.Alias) <> '', ASession.Alias, ASession.Name);
            AValues := BeforeSerialize(AValues, ASession);
            ACommand := Format('insert into %s (%s) values (%s);', [AAlias, AColumns, AValues]);

            AData.Add(ACommand);
            Serialize(ASession.Sessions, AQuery, AResult);

            if (ASessions.Owner = nil) then
              DoWork;

            if (ASame) then
              Break;

            AQuery.Next;
          except
            on E: Exception do
              raise ESerializeException.Create('TexSQLInsertSerializer error', ExtractDataError(AQuery), E);
          end;
        end;
      finally
        if (not ASame) then
          AQuery.Free;
      end;
    end;
  end;
end;

initialization
  GetRegisteredSerializers.RegisterClass(sexSColumnSerializer, TexColumnSerializer);
  GetRegisteredSerializers.RegisterClass(sexSJsonSerializer, TexJsonSerializer);
  GetRegisteredSerializers.RegisterClass(sexSXMLSerializer, TexXmlSerializer);
  GetRegisteredSerializers.RegisterClass(sexSSQLInsertSerializer, TexSQLInsertSerializer);

end.

