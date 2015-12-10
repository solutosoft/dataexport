unit exExporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, DB, fgl, RegExpr, uPSComponent, uPSCompiler, uPSRuntime, exDefinition;

const
  SCRIPT_PROGRAM = 'program exEvalutator;';
  SCRIPT_FUNCEVAL_DECL = 'function exEvaluate: Variant;';
  SCRIPT_FUNCEVAL_EXEC = 'exEvaluate';
  SCRIPT_VAR_REGEX = '^\s*var\s+';

type
  TexExporter = class;
  TexResutMap = specialize TFPGMap<String, TStrings>;
  TexScriptArgs = specialize TFPGMap<String, Variant>;

  { TexSerializer }

  TexSerializer = class(TComponent)
  private
    FExporter: TexExporter;
  public
    procedure Serialize(ASessions: TexSessionList; AMaster: TDataSet; AResult: TexResutMap) virtual; abstract;
    property Exporter: TexExporter read FExporter write FExporter;
  end;

  { TexProvider }

  TexProvider = class(TComponent)
  public
    function CreateQuery(ASQL: String; AParameters: TexParameterList; AMaster: TDataSet): TDataSet; virtual; abstract;
    procedure OpenConnection; virtual; abstract;
    procedure CloseConnection; virtual; abstract;
  end;

  { TexExporter }

  TexExporter = class(TComponent)
  private
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
    procedure SetSerializer(AValue: TexSerializer);
    procedure SetSessions(AValue: TexSessionList);
    procedure ScriptCompile(Sender: TPSScript);
    procedure ScriptExecute(Sender: TPSScript);
    procedure ScriptEngineExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
    procedure ScriptEngineCompImport(Sender: TObject; x: TPSPascalCompiler);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(const AStream: TStream);
    procedure LoadFromFile(const AFileName: String);
    procedure SaveToStream(const AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    function ExecuteExpression(AScript: String; AArgs: TexScriptArgs = nil): Variant;
    function Execute: TexResutMap;
  published
    property Description: String read FDescription write FDescription;
    property Sessions: TexSessionList read FSessions write SetSessions;
    property Provider: TexProvider read FProvider write FProvider;
    property Dictionaries: TexDictionaryList read FDictionaries write SetDictionaries;
    property Events: TexVariableList read FEvents write SetEvents;
    property Pipelines: TexPipelineList read FPipelines write SetPipelines;
    property Parameters: TexParameterList read FParameters write SetParameters;
    property Serializer: TexSerializer read FSerializer write SetSerializer;
    property Packages: TexPackageList read FPackages write SetPackages;
  end;


implementation

uses
  uPSC_dateutils,
  uPSR_dateutils,
  uPSC_SysUtils,
  uPSR_SysUtils;


function PrepareScript(AExpression: String): TStrings;
var
  AVar: Boolean;
begin
  Result := TStringList.Create;
  AVar := ExecRegExpr(SCRIPT_VAR_REGEX, AExpression);
  Result.Add(SCRIPT_PROGRAM);
  Result.Add(SCRIPT_FUNCEVAL_DECL);

  if (not AVar) then
    Result.Add('begin');

  Result.Add(AExpression);

  if (not AVar) then
    Result.Add('end;');
end;

{ TexExporter }

constructor TexExporter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScript := TPSScript.Create(nil);
  FScript.CompilerOptions := [icAllowNoBegin, icAllowNoEnd];
  FScript.OnCompile := @ScriptCompile;
  FScript.OnExecute := @ScriptExecute;
  FScript.OnCompImport := @ScriptEngineCompImport;
  FScript.OnExecImport := @ScriptEngineExecImport;

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

procedure TexExporter.ScriptCompile(Sender: TPSScript);
var
  I: Integer;
begin
  for I := 0 to FScriptArgs.Count -1 do
    Sender.AddRegisteredVariable(FScriptArgs.Keys[I], 'Variant');
end;

procedure TexExporter.ScriptExecute(Sender: TPSScript);
var
  I: Integer;
  AKey: String;
begin
  for I := 0 to FScriptArgs.Count -1 do
  begin
    AKey := FScriptArgs.Keys[I];
    PPSVariantVariant(FScript.GetVariable(AKey))^.Data := FScriptArgs[AKey];
  end;
end;

procedure TexExporter.ScriptEngineCompImport(Sender: TObject; x: TPSPascalCompiler);
begin
  RegisterDatetimeLibrary_C(x);
  RegisterSysUtilsLibrary_C(x);
end;

procedure TexExporter.ScriptEngineExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
begin
  RegisterDateTimeLibrary_R(se);
  RegisterSysUtilsLibrary_R(se);
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

procedure TexExporter.SetSerializer(AValue: TexSerializer);
begin
  FSerializer := AValue;
  if (FSerializer <> nil) then
    FSerializer.Exporter := Self;
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

