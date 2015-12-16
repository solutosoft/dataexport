unit exExporter;

interface

uses
  Classes, SysUtils, Variants, DB,
  {$IFDEF FPC}fgl, RegExpr, {$ELSE} Generics.Collections, RegularExpressions, {$ENDIF}
  uPSComponent, uPSCompiler, uPSRuntime, exDefinition;

const
  SCRIPT_PROGRAM = 'program exEvalutator;';
  SCRIPT_FUNCEVAL_DECL = 'function exEvaluate: Variant;';
  SCRIPT_FUNCEVAL_EXEC = 'exEvaluate';
  SCRIPT_VAR_REGEX = '^\s*var\s+';

  EVENT_BEFORE_SERIALIZE = 'function BeforeSerialize: String';

type
  TexExporter = class;
  TexResutMap = {$IFDEF FPC} specialize TFPGMap {$ELSE} TDictionary {$ENDIF}<String, TStrings>;
  TexScriptArgs = {$IFDEF FPC} specialize TFPGMap {$ELSE} TDictionary {$ENDIF}<String, Variant>;

  { TexComponent }

  TexComponent = class(TComponent)
  public
    procedure LoadFromStream(const AStream: TStream);
    procedure SaveToStream(const AStream: TStream);
  end;

  { TexSerializer }

  TexSerializer = class(TexComponent)
  private
    FExporter: TexExporter;
  public
    procedure Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap); virtual; abstract;
    property Exporter: TexExporter read FExporter write FExporter;
  end;

  TexSerializerClass = class of TexSerializer;

  { TexProvider }

  TexProvider = class(TComponent)
  private
    FExporter: TexExporter;
  public
    procedure OpenConnection; virtual; abstract;
    procedure CloseConnection; virtual; abstract;
    function CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet; virtual; abstract;
    property Exporter: TexExporter read FExporter write FExporter;
  end;

  { TexExporter }

  TexExporter = class(TexComponent)
  private
    FCurrentDataSet: TDataSet;
    FProvider: TexProvider;
    FPackages: TexPackageList;
    FDescription: String;
    FEvents: TexVariableList;
    FParameters: TexParameterList;
    FPipelines: TexPipelineList;
    FSerializer: TexSerializer;
    FSessions: TexSessionList;
    FDictionaries: TexDictionaryList;
    FScript: TPSScript;
    FScriptArgs: TexScriptArgs;
    procedure SetPackages(AValue: TexPackageList);
    procedure SetDictionaries(AValue: TexDictionaryList);
    procedure SetEvents(AValue: TexVariableList);
    procedure SetParameters(AValue: TexParameterList);
    procedure SetPipelines(AValue: TexPipelineList);
    procedure SetProvider(AValue: TexProvider);
    procedure SetSerializer(AValue: TexSerializer);
    procedure SetSessions(AValue: TexSessionList);
    procedure ScriptEngineCompile(Sender: TPSScript);
    procedure ScriptEngineExecute(Sender: TPSScript);
    procedure ScriptEngineExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
    procedure ScriptEngineCompImport(Sender: TObject; x: TPSPascalCompiler);
    function ScriptEngineFindField(AFieldName: String): TField;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExtractParamValue(AName: String): Variant;
    function ExecuteExpression(AScript: String; AArgs: TexScriptArgs = nil): Variant;
    function Execute: TexResutMap;
    property CurrentDataSet: TDataSet read FCurrentDataSet write FCurrentDataSet;
  published
    property Description: String read FDescription write FDescription;
    property Sessions: TexSessionList read FSessions write SetSessions;
    property Provider: TexProvider read FProvider write SetProvider;
    property Dictionaries: TexDictionaryList read FDictionaries write SetDictionaries;
    property Events: TexVariableList read FEvents write SetEvents;
    property Pipelines: TexPipelineList read FPipelines write SetPipelines;
    property Parameters: TexParameterList read FParameters write SetParameters;
    property Serializer: TexSerializer read FSerializer write SetSerializer;
    property Packages: TexPackageList read FPackages write SetPackages;
  end;

procedure LoadExporterFromFile(AExporter: TexExporter; AFileName: String);
procedure SaveExporterToFile(AExporter: TexExporter; AFileName: String);

implementation

uses
  uPSC_dateutils, uPSR_dateutils, uPSC_SysUtils, uPSR_SysUtils, uPSR_DB, uPSC_DB, exSerializer;


procedure LoadExporterFromFile(AExporter: TexExporter; AFileName: String);
var
  AClassName: String;
  AFile: TFileStream;
  AContent: TStringStream;
  {$IFDEF FPC}
  ARegExpr: TRegExpr;
  AParts: TStringList;
  {$ELSE}
  ARegExpr: TRegEx;
  AParts: TArray<string>;
  {$ENDIF}
  ASerializer: TexSerializer;
const
  exprObject = 'object ';
  exprSerializer = 'object\s+(\w+)';
begin
  {$IFDEF FPC}
  AParts := TStringList.Create;
  {$ENDIF}
  AFile := TFileStream.Create(AFileName, fmOpenRead);
  AContent := TStringStream.Create('');
  try
    AContent.CopyFrom(AFile, AFile.Size);
    try
      {$IFDEF FPC}
      SplitRegExpr(exprObject, AContent.DataString, AParts);
      {$ELSE}
      AParts := TRegEx.Split(AContent.DataString, exprObject);
      {$ENDIF}
      AContent := TStringStream.Create(exprObject + AParts[1]);
      AExporter.LoadFromStream(AContent);

      if ({$IFDEF FPC}AParts.Count{$ELSE}Length(AParts){$ENDIF} > 2) then
      begin
        AClassName := '';
        AContent := TStringStream.Create(exprObject + AParts[2]);
        {$IFDEF FPC}
        ARegExpr := TRegExpr.Create;
        try
          ARegExpr.Expression := exprSerializer;
          if (ARegExpr.Exec(AContent.DataString)) then
            AClassName := ARegExpr.Match[1];
        finally
          ARegExpr.Free;
        end;
        {$ELSE}
         ARegExpr := TRegEx.Create(exprSerializer);
         if (ARegExpr.IsMatch(AContent.DataString)) then
           AClassName := ARegExpr.Match(AContent.DataString).Groups[1].Value;
        {$ENDIF}

         ASerializer := TexSerializerFactory.CreateInstance(AClassName, AExporter.Owner);
         ASerializer.LoadFromStream(AContent);
         AExporter.Serializer := ASerializer;
      end;
    except
      raise EParserError.Create('Invalid exporter file format');
    end;
  finally
    {$IFDEF FPC}
    AParts.Free;
    {$ENDIF}
    AFile.Free;
    AContent.Free;
  end;
end;

procedure SaveExporterToFile(AExporter: TexExporter; AFileName: String);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    AExporter.SaveToStream(AStream);
    if (AExporter.Serializer <> nil) then
      AExporter.Serializer.SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

function PrepareScript(AExpression: String): TStrings;
var
  AVar: Boolean;
begin
  Result := TStringList.Create;
  {$IFDEF FPC}
  AVar := ExecRegExpr(SCRIPT_VAR_REGEX, AExpression);
  {$ELSE}
  AVar := TRegEx.IsMatch(AExpression, SCRIPT_VAR_REGEX);
  {$ENDIF}

  Result.Add(SCRIPT_PROGRAM);
  Result.Add(SCRIPT_FUNCEVAL_DECL);

  if (not AVar) then
    Result.Add('begin');

  Result.Add(AExpression);

  if (not AVar) then
    Result.Add('end;');
end;

{ TexComponent }

procedure TexComponent.LoadFromStream(const AStream: TStream);
var
  AMemStream: TMemoryStream;
begin
  AMemStream := TMemoryStream.Create;
  try
    ObjectTextToBinary(AStream, AMemStream);
    AMemStream.Position := 0;
    AMemStream.ReadComponent(Self);
  finally
    AMemStream.Free;
  end;
end;

procedure TexComponent.SaveToStream(const AStream: TStream);
var
  AMemStream: TMemoryStream;
begin
  AMemStream := TMemoryStream.Create;
  try
    AMemStream.WriteComponent(Self);
    AMemStream.Position := 0;
    ObjectBinaryToText(AMemStream, AStream);
  finally
    AMemStream.Free;
  end;
end;

{ TexExporter }

constructor TexExporter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScript := TPSScript.Create(nil);
  FScript.CompilerOptions := [icAllowNoBegin, icAllowNoEnd];
  FScript.OnCompile := {$IFDEF FPC}@{$ENDIF}ScriptEngineCompile;
  FScript.OnExecute := {$IFDEF FPC}@{$ENDIF}ScriptEngineExecute;
  FScript.OnCompImport := {$IFDEF FPC}@{$ENDIF}ScriptEngineCompImport;
  FScript.OnExecImport := {$IFDEF FPC}@{$ENDIF}ScriptEngineExecImport;

  FSessions := TexSessionList.Create(nil);
  FDictionaries := TexDictionaryList.Create;
  FEvents := TexVariableList.Create;
  FPipelines := TexPipelineList.Create;
  FParameters := TexParameterList.Create;
  FPackages := TexPackageList.Create;
end;

destructor TexExporter.Destroy;
begin
  FScript.Free;
  FSessions.Free;
  FDictionaries.Free;
  FEvents.Free;
  FPipelines.Free;
  FParameters.Free;
  FPackages.Free;;
  inherited Destroy;
end;

procedure TexExporter.SetPackages(AValue: TexPackageList);
begin
  FPackages.Assign(AValue);
end;

procedure TexExporter.SetSessions(AValue: TexSessionList);
begin
  FSessions.Assign(AValue);
end;

procedure TexExporter.ScriptEngineCompile(Sender: TPSScript);
var
  I: Integer;
  AKey: String;
begin
  Sender.AddMethod(Self, @TexExporter.ScriptEngineFindField, 'function FindField(AFieldName: String): TField;');
  {$IFDEF FPC}
  for I := 0 to FScriptArgs.Count -1 do
  begin
    AKey := FScriptArgs.Keys[I];
  {$ELSE}
  for AKey in FScriptArgs.Keys do
  begin
  {$ENDIF}
    Sender.AddRegisteredVariable(AKey, 'Variant');
  end;
end;

procedure TexExporter.ScriptEngineExecute(Sender: TPSScript);
var
  {$IFDEF FPC}
  I: Integer;
  {$ENDIF}
  AKey: String;
begin
  {$IFDEF FPC}
  for I := 0 to FScriptArgs.Count -1 do
  begin
    AKey := FScriptArgs.Keys[I];
  {$ELSE}
  for AKey in FScriptArgs.Keys do
  begin
  {$ENDIF}
    PPSVariantVariant(FScript.GetVariable(AKey))^.Data := FScriptArgs[AKey];
  end;
end;

procedure TexExporter.ScriptEngineCompImport(Sender: TObject; x: TPSPascalCompiler);
begin
  RegisterDatetimeLibrary_C(x);
  RegisterSysUtilsLibrary_C(x);
  SIRegisterTFIELD(x);
end;

procedure TexExporter.ScriptEngineExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
begin
  RegisterDateTimeLibrary_R(se);
  RegisterSysUtilsLibrary_R(se);
  RIRegisterTFIELD(x)
end;

function TexExporter.ScriptEngineFindField(AFieldName: String): TField;
begin
  Result := nil;
  if (Assigned(FCurrentDataSet)) then
    Result := FCurrentDataSet.FieldByName(AFieldName);
end;


procedure TexExporter.SetDictionaries(AValue: TexDictionaryList);
begin
  FDictionaries.Assign(AValue);
end;

procedure TexExporter.SetEvents(AValue: TexVariableList);
begin
  FEvents.Assign(AValue);
end;

procedure TexExporter.SetParameters(AValue: TexParameterList);
begin
  FParameters.Assign(AValue);
end;

procedure TexExporter.SetPipelines(AValue: TexPipelineList);
begin
  FPipelines.Assign(AValue);
end;

procedure TexExporter.SetProvider(AValue: TexProvider);
begin
  FProvider := AValue;
  if (FProvider <> nil) then
    FProvider.Exporter := Self;
end;

procedure TexExporter.SetSerializer(AValue: TexSerializer);
begin
  FSerializer := AValue;
  if (FSerializer <> nil) then
    FSerializer.Exporter := Self;
end;

function TexExporter.ExtractParamValue(AName: String): Variant;
var
  AParameter: TexParameter;
begin
  Result := Unassigned;
  AParameter := FParameters.FindByName(AName);
  if (AParameter <> nil) then
  begin
   if (not VarIsClear(AParameter.Value)) then
      Result := AParameter.Value
   else
     Result := ExecuteExpression(AParameter.Expression);

    case AParameter.DataType of
      datText:
        Result := VarAsType(Result, varstring);
      datInteger:
        Result := VarAsType(Result, varinteger);
      datDateTime:
        Result := VarAsType(Result, vardate);
      datBoolean:
        Result := VarAsType(Result, varboolean);
      datFloat:
        Result := VarAsType(Result, vardouble);
      datCurrency:
        Result := VarAsType(Result, varcurrency);
    end;
  end;
end;

function TexExporter.ExecuteExpression(AScript: String; AArgs: TexScriptArgs): Variant;
begin
  FScriptArgs := AArgs;
  FScript.Script.Assign(PrepareScript(AScript));
  if (FScript.Compile) then
    Result := FScript.ExecuteFunction([], SCRIPT_FUNCEVAL_EXEC)
  else
    raise Exception.Create(FScript.CompilerErrorToStr(0));
end;

function TexExporter.Execute: TexResutMap;
begin
  Result := TexResutMap.Create;
  FProvider.OpenConnection;
  try
    Serializer.Serialize(Sessions, nil, Result);
  finally
    FProvider.CloseConnection;
  end;
end;

end.

