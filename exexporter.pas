unit exExporter;

interface

uses
  Classes, SysUtils, Variants, DB,
  {$IFDEF FPC}fgl, RegExpr, md5, {$ELSE} Generics.Collections, RegularExpressions, Hash, {$ENDIF}
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

  TexWorkBeginEvent = procedure(Sender: TObject; SessionCount: Integer) of object;
  TexSerializeDataEvent = procedure(Sender: TexPackage; AData: WideString) of object;

  { TexScriptVar }

  TexScriptVar = class(TPersistent)
  private
    FName: String;
    FValue: Variant;
    FData: TObject;
  public
    constructor Create(AName: String; AValue: Variant); overload;
    constructor Create(AName: String; AData: TObject); overload;
  published
    property Name: String read FName;
    property Data: TObject read FData;
    property Value: Variant read FValue;
  end;

  TexScriptArgs = {$IFDEF FPC} specialize TFPGObjectList {$ELSE} TObjectList {$ENDIF}<TexScriptVar>;
  TexScriptCache = {$IFDEF FPC} specialize TFPGMap {$ELSE} TDictionary {$ENDIF}<String, String>;

  { TexSerializer }

  TexSerializer = class(TPersistent)
  private
    FExporter: TexExporter;
    FOnWork: TNotifyEvent;
    FOnSerializeData: TexSerializeDataEvent;
  public
    constructor Create(AExporter: TexExporter); virtual;
    procedure Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap); virtual; abstract;
    property Exporter: TexExporter read FExporter;
    property OnWork: TNotifyEvent read FOnWork write FOnWork;
    property OnSerializeData: TexSerializeDataEvent read FOnSerializeData write FOnSerializeData;
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
    FScriptCache: TexScriptCache;
    FSerializerClass: TexSerializerClass;
    FVariables: TexVariableList;
    FOnWorkBegin: TexWorkBeginEvent;
    FOnWorkEnd: TNotifyEvent;
    FOnWork: TNotifyEvent;
    FOnSerializeData: TexSerializeDataEvent;
    FOnScriptExecImport: TPSOnExecImportEvent;
    FOnScriptCompImport: TPSOnCompImportEvent;
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
    procedure SetSerializerClass(const Value: TexSerializerClass);
    procedure SetVariables(const Value: TexVariableList);
    procedure ScriptEngineCompile(Sender: TPSScript);
    procedure ScriptEngineExecute(Sender: TPSScript);
    procedure ScriptEngineExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
    procedure ScriptEngineCompImport(Sender: TObject; x: TPSPascalCompiler);
    procedure ScriptEngineSetSessionVisible(ASessionName: String; AVisible: Boolean);
    procedure ScriptEngineSetSessionVisibleAll(AVisible: Boolean);
    function ScriptEngineRecNo: Integer;
    function ScriptEngineFindField(AFieldName: String): TexValue;
    function ScriptEngineFindParam(AParamName: String): TexValue;
    function ScriptEngineGetObjectId: Integer;
  protected
    FObjectCounter: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExtractParamValue(AName: String): Variant;
    function ExecuteExpression(AScript: String; AArgs: TexScriptArgs = nil): Variant;
    function ExecuteEvent(AName: String; AArgs: TexScriptArgs = nil): Variant;
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
    property OnScriptCompImport: TPSOnCompImportEvent read FOnScriptCompImport write FOnScriptCompImport;
    property OnScriptExecImport: TPSOnExecImportEvent read FOnScriptExecImport write FOnScriptExecImport;
    property OnSerializeData: TexSerializeDataEvent read FOnSerializeData write FOnSerializeData;
    property OnWork: TNotifyEvent read FOnWork write FOnWork;
    property OnWorkBegin: TexWorkBeginEvent read FOnWorkBegin write FOnWorkBegin;
    property OnWorkEnd: TNotifyEvent read FOnWorkEnd write FOnWorkEnd;
  end;

implementation

uses
  exSerializer, exScript, uPSC_dateutils, uPSR_dateutils, uPSR_classes, uPSC_classes,
  uPSC_std, uPSR_std;


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

{ TexScriptVar }

constructor TexScriptVar.Create(AName: String; AValue: Variant);
begin
  FName := AName;
  FValue := AValue;
end;

constructor TexScriptVar.Create(AName: String; AData: TObject);
begin
  Create(AName, Null);
  FData := AData;
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

  FScriptCache := TexScriptCache.Create;
  FSessions := TexSessionList.Create(nil);
  FDictionaries := TexDictionaryList.Create;
  FEvents := TexVariableList.Create;
  FPipelines := TexPipelineList.Create;
  FParameters := TexParameterList.Create;
  FPackages := TexPackageList.Create;
  FVariables := TexVariableList.Create;
end;

destructor TexExporter.Destroy;
begin
  FScriptCache.Free;
  FSessions.Free;
  FDictionaries.Free;
  FEvents.Free;
  FPipelines.Free;
  FParameters.Free;
  FPackages.Free;
  FVariables.Free;
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
  AVar: TexScriptVar;
begin
  Sender.AddMethod(Self, @TexExporter.ScriptEngineRecNo, 'function RecNo: Integer;');
  Sender.AddMethod(Self, @TexExporter.ScriptEngineSetSessionVisible, 'procedure SetSessionVisible(ASessionName: String; AVisible: Boolean);');
  Sender.AddMethod(Self, @TexExporter.ScriptEngineSetSessionVisibleAll, 'procedure SetSessionVisibleAll(AVisible: Boolean);');
  Sender.AddMethod(Self, @TexExporter.ScriptEngineFindField, 'function FindField(AFieldName: String): TexValue;');
  Sender.AddMethod(Self, @TexExporter.ScriptEngineFindParam, 'function FindParam(AParamName: String): TexValue;');
  Sender.AddMethod(Self, @TexExporter.ScriptEngineGetObjectId, 'function GetObjectId: Integer;');

  if (Assigned(FScriptArgs)) then
  begin
    for AVar in FScriptArgs do
    begin
      if (Assigned(AVar.Data)) then
        Sender.AddRegisteredVariable(AVar.Name, AVar.Data.ClassName)
      else
        Sender.AddRegisteredVariable(AVar.Name, 'Variant');
    end;
  end;
end;

procedure TexExporter.ScriptEngineCompImport(Sender: TObject; x: TPSPascalCompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, True);

  RegisterTexValueClass_C(x);
  RegisterTexOptionsClass_C(x);

  if (Assigned(FOnScriptCompImport)) then
    FOnScriptCompImport(Sender, x);
end;

procedure TexExporter.ScriptEngineExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);

  RegisterTexValueClass_R(x);
  RegisterTexOptionsClass_R(x);

  if (Assigned(FOnScriptExecImport)) then
    FOnScriptExecImport(Sender, se, x);
end;

procedure TexExporter.ScriptEngineExecute(Sender: TPSScript);
var
  AVar: TexScriptVar;
begin
  if (Assigned(FScriptArgs)) then
  begin
    for AVar in FScriptArgs do
    begin
      if (Assigned(AVar.Data)) then
        FScript.SetVarToInstance(AVar.Name, AVar.Data)
      else
        PPSVariantVariant(FScript.GetVariable(AVar.Name))^.Data := AVar.Value;
    end;
  end;
end;

procedure TexExporter.ScriptEngineSetSessionVisible(ASessionName: String; AVisible: Boolean);
var
  ASession: TexSession;
begin
  ASession := FSessions.FindByName(ASessionName);
  if (ASession <> nil) then
    ASession.Visible := AVisible;
end;

procedure TexExporter.ScriptEngineSetSessionVisibleAll(AVisible: Boolean);
begin
  FSessions.Toggle(AVisible);
end;

function TexExporter.ScriptEngineFindField(AFieldName: String): TexValue;
begin
  if (Assigned(FCurrentDataSet)) then
    Result := TexValue.Create(FCurrentDataSet.FieldByName(AFieldName).Value)
  else
    Result := TexValue.Create(Null);
end;

function TexExporter.ScriptEngineFindParam(AParamName: String): TexValue;
var
  AParam: TexParameter;
begin
  AParam := FParameters.FindByName(AParamName);
  if (AParam <> nil) then
    Result := TexValue.Create(AParam.Value)
  else
    Result := TexValue.Create(Null);
end;

function TexExporter.ScriptEngineGetObjectId: Integer;
begin
  Inc(FObjectCounter);
  Result := FObjectCounter;
end;

function TexExporter.ScriptEngineRecNo: Integer;
begin
  Result := FCurrentDataSet.RecNo;
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
  AVariable: TexVariable;
  AParameter: TexParameter;
begin
  Result := Unassigned;
  AVariable := FVariables.FindByName(AName);

  if (AVariable <> nil) then
    Result := ExecuteExpression(AVariable.Expression)
  else begin
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
end;

function TexExporter.ExecuteExpression(AScript: String; AArgs: TexScriptArgs): Variant;
var
  AHash: String;
  ACompiled: AnsiString;
  AContaisKey: Boolean;
begin
  FScriptArgs := AArgs;
  AHash := {$IFDEF FPC} MD5Print(MD5String(AScript)) {$ELSE} THashMD5.GetHashString(AScript) {$ENDIF};
  AContaisKey := {$IFDEF FPC} FScriptCache.IndexOf(AHash) <> -1 {$ELSE} FScriptCache.ContainsKey(AHash) {$ENDIF};

  if (AContaisKey) then
    FScript.Exec.LoadData(FScriptCache[AHash])
  else begin
    FScript.Script.Assign(PrepareScript(AScript));
    if (FScript.Compile) then
    begin
      if (not FScript.Compile) then
        raise Exception.Create(FScript.CompilerErrorToStr(0))
      else begin
        FScript.GetCompiled(ACompiled);
        FScriptCache.Add(AHash, ACompiled);
      end;
    end;
  end;
  Result := FScript.ExecuteFunction([], SCRIPT_FUNCEVAL_EXEC);
end;

function TexExporter.Execute: TexResutMap;
begin
  if FSerializer = nil then
    raise Exception.Create('The serializer property must be set');

  Result := TexResutMap.Create;
  FScriptCache.Clear;
  FProvider.OpenConnection;
  try
    if (Assigned(FOnWorkBegin)) then
      FOnWorkBegin(Self, Sessions.Count);

    FObjectCounter := 0;
    ExecuteEvent(EXPORTER_BEFORE_EXEC);

    FSerializer.OnWork := FOnWork;
    FSerializer.OnSerializeData := FOnSerializeData;
    FSerializer.Serialize(Sessions, nil, Result);

    ExecuteEvent(EXPORTER_AFTER_EXEC);

    if (Assigned(FOnWorkEnd)) then
      FOnWorkEnd(Self);
  finally
    FProvider.CloseConnection;
  end;
end;

function TexExporter.ExecuteEvent(AName: String; AArgs: TexScriptArgs = nil): Variant;
var
  AEvent: TexVariable;
begin
  Result := Unassigned;
  AEvent := Self.Events.FindByName(AName);

  if (AEvent <> nil) and (Trim(AEvent.Expression) <> '') then
    Result := ExecuteExpression(AEvent.Expression, AArgs);
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
