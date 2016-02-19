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

  EXPORTER_BEFORE_SERIALIZE = 'function BeforeSerialize: String';
  EXPORTER_BEFORE_EXEC = 'procedure BeforeExecute';
  EXPORTER_AFTER_EXEC = 'procedure AfterExecute';
  EXPORTER_EVENTS: array [1 .. 3] of String = (EXPORTER_BEFORE_EXEC, EXPORTER_AFTER_EXEC, EXPORTER_BEFORE_SERIALIZE);

  PACKAGE_BEFORE_EXEC = 'function BeforeExecute(AParams: TexOptions): Boolean';
  PACKAGE_AFTER_EXEC = 'procedure AfterExecute(AData: String)';
  PACKAGE_EVENTS: array [1 .. 2] of String = (PACKAGE_BEFORE_EXEC, PACKAGE_AFTER_EXEC);

type
  TexExporter = class;
  TexResutMap = {$IFDEF FPC} specialize TFPGMap {$ELSE} TDictionary {$ENDIF}<String, TStrings>;
  TexScriptArgs = {$IFDEF FPC} specialize TFPGMap {$ELSE} TDictionary {$ENDIF}<String, Variant>;
  TexValues = {$IFDEF FPC} specialize TFPGMap {$ELSE} TObjectDictionary {$ENDIF}<String, TexValue>;

  { TexSerializer }

  TexSerializer = class(TPersistent)
  private
    FExporter: TexExporter;
  public
    constructor Create(AExporter: TexExporter); virtual;
    procedure Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap); virtual; abstract;
    property Exporter: TexExporter read FExporter;
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

  TexExporter = class(TComponent)
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
    FSerializerClass: TexSerializerClass;
    FParamValues: TexValues;
    FVariables: TexVariableList;
    function GetSerializerClassName: String;
    procedure SetSerializerClassName(const Value: String);
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
    function ScriptEngineFindParam(AParamName: String): TexValue;
    procedure SetSerializerClass(const Value: TexSerializerClass);
    procedure SetVariables(const Value: TexVariableList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExtractParamValue(AName: String): Variant;
    function ExecuteExpression(AScript: String; AArgs: TexScriptArgs = nil): Variant;
    function Execute: TexResutMap;
    procedure LoadFromStream(const AStream: TStream);
    procedure LoadFromFile(const AFileName: String);
    procedure SaveToStream(const AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    property CurrentDataSet: TDataSet read FCurrentDataSet write FCurrentDataSet;
    property SerializerClass: TexSerializerClass read FSerializerClass write SetSerializerClass;
  published
    property Description: String read FDescription write FDescription;
    property Dictionaries: TexDictionaryList read FDictionaries write SetDictionaries;
    property Events: TexVariableList read FEvents write SetEvents;
    property Packages: TexPackageList read FPackages write SetPackages;
    property Parameters: TexParameterList read FParameters write SetParameters;
    property Pipelines: TexPipelineList read FPipelines write SetPipelines;
    property Provider: TexProvider read FProvider write SetProvider;
    property Sessions: TexSessionList read FSessions write SetSessions;
    property Variables: TexVariableList read FVariables write SetVariables;
    property SerializerClassName: String read GetSerializerClassName write SetSerializerClassName;
    property Serializer: TexSerializer read FSerializer write SetSerializer;
  end;

implementation

uses
  uPSC_dateutils, uPSR_dateutils, uPSR_DB, uPSC_DB, exScript, exSerializer;


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


{ TexSerializer }

constructor TexSerializer.Create(AExporter: TexExporter);
begin
  FExporter := AExporter;
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
  FVariables := TexVariableList.Create;
  FParamValues := TexValues.Create;
end;

destructor TexExporter.Destroy;
begin
  FSessions.Free;
  FDictionaries.Free;
  FEvents.Free;
  FPipelines.Free;
  FParameters.Free;
  FPackages.Free;
  FVariables.Free;
  FParamValues.Free;
  FScript.Free;
  FreeAndNil(FSerializer);
  inherited Destroy;
end;

procedure TexExporter.SetSerializerClass(const Value: TexSerializerClass);
begin
  if FSerializerClass <> Value then
  begin
    FreeAndNil(FSerializer);
    FSerializerClass := Value;

    if (FSerializerClass <> nil) then
      FSerializer := FSerializerClass.Create(Self);
  end;
end;

function TexExporter.GetSerializerClassName: String;
begin
  if FSerializer = nil then
    Result := ''
  else
    Result := FSerializer.ClassName;
end;

procedure TexExporter.SetSerializerClassName(const Value: String);
begin
  SerializerClass := TexSerializerClass(GetRegisteredSerializers.FindByClassName(Value).RegisteredClass);
end;

procedure TexExporter.SetPackages(AValue: TexPackageList);
begin
  FPackages.Assign(AValue);
end;

procedure TexExporter.SetSessions(AValue: TexSessionList);
begin
  FSessions.Assign(AValue);
end;

procedure TexExporter.SetVariables(const Value: TexVariableList);
begin
  FVariables.Assign(Value);
end;

procedure TexExporter.ScriptEngineCompile(Sender: TPSScript);
var
  {$IFDEF FPC}
  I: Integer;
  {$ENDIF}
  AKey: String;
begin
  Sender.AddMethod(Self, @TexExporter.ScriptEngineFindField, 'function FindField(AFieldName: String): TField;');
  Sender.AddMethod(Self, @TexExporter.ScriptEngineFindParam, 'function FindParam(AParamName: String): TexValue;');

  if (Assigned(FScriptArgs)) then
  begin
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
end;

procedure TexExporter.ScriptEngineExecute(Sender: TPSScript);
var
  {$IFDEF FPC}
  I: Integer;
  {$ENDIF}
  AKey: String;
begin
  if (Assigned(FScriptArgs)) then
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
end;

procedure TexExporter.ScriptEngineCompImport(Sender: TObject; x: TPSPascalCompiler);
begin
  RegisterDatetimeLibrary_C(x);
  RegisterSysUtilsLibrary_C(x);
  RegisterTexValueClass_C(x);
  SIRegisterTFIELD(x);
end;

procedure TexExporter.ScriptEngineExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
begin
  RegisterDateTimeLibrary_R(se);
  RegisterSysUtilsLibrary_R(se);
  RegisterTexValueClass_R(x);
  RIRegisterTFIELD(x);
end;

function TexExporter.ScriptEngineFindField(AFieldName: String): TField;
begin
  Result := nil;
  if (Assigned(FCurrentDataSet)) then
    Result := FCurrentDataSet.FieldByName(AFieldName);
end;

function TexExporter.ScriptEngineFindParam(AParamName: String): TexValue;
var
  AParam: TexParameter;
begin
  Result := nil;
  if (FParamValues.ContainsKey(AParamName)) then
    Result := FParamValues[AParamName]
  else begin
    AParam := FParameters.FindByName(AParamName);
    if (AParam <> nil) then
    begin
      Result := TexValue.Create(AParam.Value);
      FParamValues.Add(AParam.Name, Result);
    end;
  end;
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
  if (FSerializer <> nil) and (AValue <> nil) then
    FSerializer.Assign(AValue);
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
  if FSerializer = nil then
    raise Exception.Create('The serializer property must be set');

  Result := TexResutMap.Create;
  FProvider.OpenConnection;
  try
    FParamValues.Clear;
    Serializer.Serialize(Sessions, nil, Result);
  finally
    FProvider.CloseConnection;
  end;
end;

procedure TexExporter.LoadFromStream(const AStream: TStream);
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

procedure TexExporter.LoadFromFile(const AFileName: String);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TexExporter.SaveToFile(const AFileName: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TexExporter.SaveToStream(const AStream: TStream);
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

end.
