unit exDefinition;

interface

uses
  Classes, SysUtils, Variants, exClasses;

const
  VK_CHAR_SPACE = #32;

type
  TVariantDynArray = array of Variant;

  TexAlignment = (altNone, altRight, altLeft);
  TexDataType = (datNone, datText, datInteger, datDateTime, datBoolean, datFloat, datCurrency);

  { TexValue }

  TexValue = class(TObject)
  private
    FValue: Variant;
    function GetRange: TVariantDynArray;
    function CheckIsNull(AValue: Variant): Boolean;
    function CastData(AValue: Variant; AVarType: TVarType; ADefault: Variant): Variant;
  public
    constructor Create(AValue: Variant);
    function IsNull: Boolean;
    function AsVariant: Variant;
    function AsString: String;
    function AsInteger: Integer;
    function AsFloat: Extended;
    function AsBoolean: Boolean;
    function AsArray: TVariantDynArray;
    function AsDateTime: Double;
    function AsDateBegin: Double;
    function AsDateEnd: Double;
  end;

  { TexOptions }

  TexOptions = class(TStringList)
  protected
    FEditors: TexEditorList;
    procedure RegisterOption(AName: String; AEditor: TexEditorType; ADefault: String = '');
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    function GetAsString(AName: String; ADefault: String = ''): String;
    function GetAsInteger(AName: String; ADefault: Integer = 0): Integer;
    function GetAsFloat(AName: String; ADefault: Double = 0): Double;
    function GetAsBoolean(AName: String; ADefault: Boolean = False): Boolean;
  end;

  TexOptionsClass = class of TexOptions;

  { TexElement }

  TexElement = class(TCollectionItem)
  private
    FName: String;
  published
    property Name: String read FName write FName;
  end;

  { TexElementList }

  TexElementList = class(TCollection)
  public
    function FindByName(AName: String): TexElement;
  end;

  { TexVariable }

  TexVariable = class(TexElement)
  private
    FExpression: String;
  published
    property Expression: String read FExpression write FExpression;
  end;

  { TexVariableList }

  TexVariableList = class(TexElementList)
  private
    function GetItem(Index: Integer): TexVariable;
    procedure SetItem(Index: Integer; AValue: TexVariable);
  public
    constructor Create;
    function FindByName(AName: String): TexVariable; reintroduce;
    function Add: TexVariable;
    property Items[Index: Integer]: TexVariable read GetItem write SetItem; default;
  end;

  { TexDictionary }

  TexDictionary = class(TexVariable)
  private
    FAlign: TexAlignment;
    FComplete: Char;
    FSize: Integer;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property Align: TexAlignment read FAlign write FAlign default altNone;
    property Complete: Char read FComplete write FComplete default VK_CHAR_SPACE;
    property Size: Integer read FSize write FSize default 0;
  end;

  { TexDictionaryList }

  TexDictionaryList = class(TexElementList)
  private
    function GetItem(Index: Integer): TexDictionary;
    procedure SetItem(Index: Integer; AValue: TexDictionary);
  public
    constructor Create;
    function FindByName(AName: String): TexDictionary; reintroduce;
    function Add: TexDictionary;
    property Items[Index: Integer]: TexDictionary read GetItem write SetItem; default;
  end;

  { TexColumn }

  TexColumn = class(TexDictionary)
  private
    FAlias: String;
    FDictionary: String;
  published
    property Alias: String read FAlias write FAlias;
    property Dictionary: String read FDictionary write FDictionary;
  end;

  { TexColumnList }

  TexColumnList = class(TexElementList)
  private
    function GetItem(Index: Integer): TexColumn;
    procedure SetItem(Index: Integer; AValue: TexColumn);
  public
    constructor Create;
    function FindByName(AName: String): TexColumn; reintroduce;
    function Add: TexColumn;
    property Items[Index: Integer]: TexColumn read GetItem write SetItem; default;
  end;

  { TexComboItem }

  TexComboItem = class(TCollectionItem)
  private
    FDescription: String;
    FValue: Variant;
  published
    property Descrition: String read FDescription write FDescription;
    property Value: Variant read FValue write FValue;
  end;

  { TexComboList }

  TexComboList = class(TCollection)
  private
    function GetItem(Index: Integer): TexComboItem;
    procedure SetItem(Index: Integer; Value: TexComboItem);
  public
    constructor Create;
    function Add: TexComboItem;
    property Items[Index: Integer]: TexComboItem read GetItem write SetItem;default;
  end;

  { TexParameter }

  TexParameter = class(TexVariable)
  private
    FCaption: String;
    FDataType: TexDataType;
    FEditorType: TexEditorType;
    FRequired: Boolean;
    FValue: Variant;
    FItems: TexComboList;
    FWidth: Integer;
    FHeight: Integer;
    procedure SetItems(const Value: TexComboList);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Caption: String read FCaption write FCaption;
    property DataType: TexDataType read FDataType write FDataType default datNone;
    property EditorType: TexEditorType read FEditorType write FEditorType default edtText;
    property Height: Integer read FHeight write FHeight default 0;
    property Items: TexComboList read FItems write SetItems;
    property Required: Boolean read FRequired write FRequired default False;
    property Width: Integer read FWidth write FWidth default 0;
    property Value: Variant read FValue write FValue;
  end;

  { TexParameterList }

  TexParameterList = class(TexElementList)
  private
    function GetItem(Index: Integer): TexParameter;
    procedure SetItem(Index: Integer; AValue: TexParameter);
  public
    constructor Create;
    function FindByName(AName: String): TexParameter; reintroduce;
    function Add: TexParameter;
    property Items[Index: Integer]: TexParameter read GetItem write SetItem; default;
  end;

  { TexPipeline }

  TexPipeline = class(TexElement)
  private
    FSQL: TStrings;
    procedure SetSQL(AValue: TStrings);
  public
    constructor Create(ACollection: TCollection);override;
    destructor Destroy; override;
  published
    property SQL: TStrings read FSQL write SetSQL;
  end;

  { TexPipelineList }

  TexPipelineList = class(TexElementList)
  private
    function GetItem(Index: Integer): TexPipeline;
    procedure SetItem(Index: Integer; AValue: TexPipeline);
  public
    constructor Create;
    function FindByName(AName: String): TexPipeline; reintroduce;
    function Add: TexPipeline;
    property Items[Index: Integer]: TexPipeline read GetItem write SetItem; default;
  end;

  { TexSessionList }

  TexSession = class;
  TexSessionList = class(TexElementList)
  private
    FOwner: TexSession;
    function GetItem(Index: Integer): TexSession;
    procedure SetItem(Index: Integer; AValue: TexSession);
  public
    constructor Create(AOwner: TexSession);
    function GetOwner: TPersistent; override;
    function FindByName(AName: String): TexSession; reintroduce;
    function Add: TexSession;
    property Items[Index: Integer]: TexSession read GetItem write SetItem; default;
  end;

  { TexSession }

  TexSession = class(TexElement)
  private
    FColumns: TexColumnList;
    FPipeline: String;
    FRowCount: Integer;
    FSessions: TexSessionList;
    FVisible: Boolean;
    procedure SeTexColumns(AValue: TexColumnList);
    procedure SeTexSessions(AValue: TexSessionList);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property RowCount: Integer read FRowCount write FRowCount;
  published
    property Columns: TexColumnList read FColumns write SeTexColumns;
    property Pipeline: String read FPipeline write FPipeline;
    property Sessions: TexSessionList read FSessions write SeTexSessions;
    property Visible: Boolean read FVisible write FVisible default True;
  end;

  { TexPackage }

  TexPackage = class(TexElement)
  private
    FSessions: TStrings;
    FOptions: TexOptions;
    FOptionsClass: TexOptionsClass;
    FEvents: TexVariableList;
    procedure SetSessions(AValue: TStrings);
    procedure SetOptions(const Value: TexOptions);
    procedure SetOptionsClass(const Value: TexOptionsClass);
    function GetPackageType: String;
    procedure SetPackageType(const Value: String);
    procedure SetEvents(const Value: TexVariableList);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property OptionsClass: TexOptionsClass read FOptionsClass write SetOptionsClass;
  published
    property Sessions: TStrings read FSessions write SetSessions;
    property PackageType: String read GetPackageType write SetPackageType;
    property Options: TexOptions read FOptions write SetOptions;
    property Events: TexVariableList read FEvents write SetEvents;
  end;

  { TexPackageList }

  TexPackageList = class(TexElementList)
  private
    function GetItem(Index: Integer): TexPackage;
    procedure SetItem(Index: Integer; AValue: TexPackage);
  public
    constructor Create;
    function FindByName(AName: String): TexPackage; reintroduce;
    function FindBySession(AName: String): TexPackage;
    function Add: TexPackage;
    property Items[Index: Integer]: TexPackage read GetItem write SetItem; default;
  end;

implementation

uses
  exOptions;

{ TexValue }

constructor TexValue.Create(AValue: Variant);
begin
  FValue := AValue;
end;

function TexValue.CheckIsNull(AValue: Variant): Boolean;
var
  I: Integer;
begin
  Result := (VarIsClear(AValue)) or (VarIsNull(AValue)) or (VarIsEmpty(AValue));
  if (not Result) then
  begin
    if (VarIsNumeric(AValue)) or (VarType(AValue) = varDate) then
      Result := (AValue = 0)
    else if (VarIsStr(AValue)) then
      Result := Trim(AValue) = ''
    else if (VarIsArray(AValue)) then
    begin
      Result := VarArrayHighBound(AValue, 1) = -1;
      if (not Result) then
      begin
        for I := VarArrayLowBound(AValue, 1) to VarArrayHighBound(AValue, 1) do
        begin
          Result := CheckIsNull(AValue[I]);
          if (Result) then
            Exit;
        end;
      end;
    end;
  end;
end;

function TexValue.GetRange: TVariantDynArray;
var
  I: Integer;
  AValue: Variant;
begin
  SetLength(Result, 2);
  AValue := AsVariant;

  if (not VarIsArray(AValue)) or (VarArrayHighBound(AValue, 1) <> 1) then
    raise EInvalidCast.Create('Invalid range format');

  for I := VarArrayLowBound(AValue, 1) to VarArrayHighBound(AValue, 1) do
    Result[I] := AValue[I];
end;

function TexValue.CastData(AValue: Variant; AVarType: TVarType; ADefault: Variant): Variant;
var
  I: Integer;
begin
  if (VarIsArray(AValue)) then
  begin
    for I := VarArrayLowBound(AValue, 1) to VarArrayHighBound(AValue, 1) do
      AValue[I] := VarAsType(AValue[I], AVarType);

    Result := AValue;
  end
  else begin
    if (VarIsClear(AValue)) then
      Result := ADefault;
    Result := VarAsType(AValue, AVarType);
  end;
end;

function TexValue.IsNull: Boolean;
begin
  Result := CheckIsNull(AsVariant);
end;

function TexValue.AsVariant: Variant;
begin
  Result := FValue;
end;

function TexValue.AsString: String;
begin
  Result := CastData(AsVariant, varString, '');
end;

function TexValue.AsDateTime: Double;
begin
  Result := CastData(AsVariant, varDate, 0);
end;

function TexValue.AsFloat: Extended;
begin
  Result := CastData(AsVariant, varDouble, 0);
end;

function TexValue.AsBoolean: Boolean;
begin
  Result := CastData(AsVariant, varBoolean, False);
end;

function TexValue.AsInteger: Integer;
begin
  Result := CastData(AsVariant, varInteger, 0);
end;

function TexValue.AsDateBegin: Double;
begin
  Result := CastData(GetRange[0], varDate, 0);
end;

function TexValue.AsDateEnd: Double;
begin
  Result := CastData(GetRange[0], varDate, 0);
end;

function TexValue.AsArray: TVariantDynArray;
var
  I,
  AHigh: Integer;
  AValue: Variant;
begin
  AValue := AsVariant;
  SetLength(Result, 0);

  if (VarIsArray(AValue)) then
  begin
    AHigh := VarArrayHighBound(AValue, 1);
    SetLength(Result, AHigh + 1);

    for I := VarArrayLowBound(AValue, 1) to AHigh  do
      Result[I] := AValue[I];
  end;
end;

{ TexOptions }

constructor TexOptions.Create;
begin
  FEditors := TexEditorList.Create;
end;

destructor TexOptions.Destroy;
begin
  FEditors.Free;
  inherited Destroy;
end;

procedure TexOptions.RegisterOption(AName: String; AEditor: TexEditorType; ADefault: String);
begin
  with FEditors.Add do
  begin
    Name := AName;
    EditorType := AEditor;
    DefaultValue := ADefault;
  end;
end;

function TexOptions.GetAsString(AName: String; ADefault: String = ''): String;
begin
  if (IndexOfName(AName) <> -1) then
    Result := Self.Values[AName];

  if (Result = '') then
    Result := ADefault;
end;

function TexOptions.GetAsInteger(AName: String; ADefault: Integer = 0): Integer;
begin
  Result := StrToIntDef(GetAsString(AName), ADefault);
end;

function TexOptions.GetAsFloat(AName: String; ADefault: Double = 0): Double;
begin
  Result := StrToFloatDef(GetAsString(AName), ADefault);
end;

function TexOptions.GetAsBoolean(AName: String; ADefault: Boolean = False): Boolean;
begin
  Result := StrToBoolDef(GetAsString(AName), ADefault);
end;

{ TexElementList }

function TexElementList.FindByName(AName: String): TexElement;
var
  AItem: TCollectionItem;
  AElement: TexElement;
begin
  Result := nil;
  for AItem in Self do
  begin
    AElement := AItem as TexElement;
    if (SameText(AElement.Name, AName)) then
    begin
      Result := AElement;
      Exit;
    end;
  end;
end;

{ TexVariableList }

constructor TexVariableList.Create;
begin
  inherited Create(TexVariable);
end;

function TexVariableList.FindByName(AName: String): TexVariable;
begin
  Result := TexVariable(inherited FindByName(AName));
end;

function TexVariableList.Add: TexVariable;
begin
  Result := TexVariable(inherited Add);
end;

function TexVariableList.GetItem(Index: Integer): TexVariable;
begin
  Result := TexVariable(inherited GetItem(Index));
end;

procedure TexVariableList.SetItem(Index: Integer; AValue: TexVariable);
begin
  inherited SetItem(Index, AValue);
end;

{ TexDictionary }

constructor TexDictionary.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FAlign := altNone;
  FComplete := VK_CHAR_SPACE;
  FSize := 0;
end;

{ TexDictionaryList }

constructor TexDictionaryList.Create;
begin
  inherited Create(TexDictionary);
end;

function TexDictionaryList.FindByName(AName: String): TexDictionary;
begin
  Result := TexDictionary(inherited FindByName(AName));
end;

function TexDictionaryList.GetItem(Index: Integer): TexDictionary;
begin
  Result := TexDictionary(inherited GetItem(Index)) ;
end;

procedure TexDictionaryList.SetItem(Index: Integer; AValue: TexDictionary);
begin
  inherited SetItem(Index, AValue);
end;

function TexDictionaryList.Add: TexDictionary;
begin
  Result := TexDictionary(inherited Add);
end;

{ TexColumnList }

constructor TexColumnList.Create;
begin
  inherited Create(TexColumn);
end;

function TexColumnList.FindByName(AName: String): TexColumn;
begin
  Result := TexColumn(inherited FindByName(AName));
end;

function TexColumnList.Add: TexColumn;
begin
  Result := TexColumn(inherited Add);
end;

function TexColumnList.GetItem(Index: Integer): TexColumn;
begin
  Result := TexColumn(inherited GetItem(Index));
end;

procedure TexColumnList.SetItem(Index: Integer; AValue: TexColumn);
begin
  inherited SetItem(Index, AValue);
end;

{ TexComboList }

constructor TexComboList.Create;
begin
  inherited Create(TexComboItem);
end;

function TexComboList.Add: TexComboItem;
begin
  Result := TexComboItem(inherited Add);
end;

function TexComboList.GetItem(Index: Integer): TexComboItem;
begin
  Result := TexComboItem(inherited GetItem(Index));
end;

procedure TexComboList.SetItem(Index: Integer; Value: TexComboItem);
begin
  inherited SetItem(Index, Value);
end;

{ TexParameter }

constructor TexParameter.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FItems := TexComboList.Create;
  FDataType := datNone;
end;

destructor TexParameter.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TexParameter.SetItems(const Value: TexComboList);
begin
  FItems.Assign(Value);
end;

{ TexParameterList }

constructor TexParameterList.Create;
begin
  inherited Create(TexParameter);
end;

function TexParameterList.FindByName(AName: String): TexParameter;
begin
  Result := TexParameter(inherited FindByName(AName));
end;

function TexParameterList.Add: TexParameter;
begin
  Result := TexParameter(inherited Add);
end;


function TexParameterList.GetItem(Index: Integer): TexParameter;
begin
  Result := TexParameter(inherited GetItem(Index));
end;

procedure TexParameterList.SetItem(Index: Integer; AValue: TexParameter);
begin
  inherited SetItem(Index, AValue);
end;

{ TexPipeline }

constructor TexPipeline.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FSQL := TStringList.Create;
end;

destructor TexPipeline.Destroy;
begin
  FSQL.Free;
  inherited Destroy;
end;

{ TexPipelineList }

constructor TexPipelineList.Create;
begin
  inherited Create(TexPipeline);
end;

function TexPipelineList.FindByName(AName: String): TexPipeline;
begin
  Result := TexPipeline(inherited FindByName(AName));
end;

function TexPipelineList.Add: TexPipeline;
begin
  Result := TexPipeline(inherited Add);
end;

function TexPipelineList.GetItem(Index: Integer): TexPipeline;
begin
  Result := TexPipeline(inherited GetItem(Index));
end;

procedure TexPipelineList.SetItem(Index: Integer; AValue: TexPipeline);
begin
  inherited SetItem(Index, AValue);
end;

{ TexSession }

constructor TexSession.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVisible := True;
  FColumns := TexColumnList.Create;
  FSessions := TexSessionList.Create(Self);
end;

destructor TexSession.Destroy;
begin
  FColumns.Free;
  FSessions.Free;
  inherited Destroy;
end;

{ TexSessionList }

constructor TexSessionList.Create(AOwner: TexSession);
begin
  inherited Create(TexSession);
  FOwner := AOwner;
end;

function TexSessionList.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TexSessionList.FindByName(AName: String): TexSession;
begin
  Result := TexSession(inherited FindByName(AName));
end;

function TexSessionList.Add: TexSession;
begin
  Result := TexSession(inherited Add);
end;

function TexSessionList.GetItem(Index: Integer): TexSession;
begin
  Result := TexSession(inherited GetItem(Index));
end;

procedure TexSessionList.SetItem(Index: Integer; AValue: TexSession);
begin
  inherited SetItem(Index, AValue);
end;

procedure TexSession.SeTexSessions(AValue: TexSessionList);
begin
  FSessions.Assign(AValue);
end;

procedure TexSession.SeTexColumns(AValue: TexColumnList);
begin
  FColumns.Assign(AValue);
end;

procedure TexPipeline.SetSQL(AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;

{ TexPackage }

constructor TexPackage.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FSessions := TStringList.Create;
  FEvents := TexVariableList.Create;
  SetOptionsClass(TexFileOptions);
end;

destructor TexPackage.Destroy;
begin
  FSessions.Free;
  FEvents.Free;
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TexPackage.SetEvents(const Value: TexVariableList);
begin
  FEvents.Assign(Value);
end;

procedure TexPackage.SetOptions(const Value: TexOptions);
begin
  FOptions.Assign(Value);
end;

procedure TexPackage.SetOptionsClass(const Value: TexOptionsClass);
begin
  if FOptionsClass <> Value then
  begin
    FreeAndNil(FOptions);
    FOptionsClass := Value;

    if (FOptionsClass <> nil) then
      FOptions := FOptionsClass.Create;
  end;
end;

function TexPackage.GetPackageType: String;
begin
  if FOptions = nil then
    Result := ''
  else
    Result := FOptions.ClassName;
end;

procedure TexPackage.SetPackageType(const Value: String);
begin
  OptionsClass := TexOptionsClass(GetRegisteredOptions.FindByClassName(Value).RegisteredClass);
end;

procedure TexPackage.SetSessions(AValue: TStrings);
begin
  FSessions.Assign(AValue);
end;

{ TexPackageList }

constructor TexPackageList.Create;
begin
  inherited Create(TexPackage);
end;

function TexPackageList.FindByName(AName: String): TexPackage;
begin
  Result := TexPackage(inherited FindByName(AName));
end;

function TexPackageList.FindBySession(AName: String): TexPackage;
var
  AItem: TCollectionItem;
  AFile: TexPackage;
begin
  Result := nil;
  for AItem in Self do
  begin
    AFile := AItem as TexPackage;
    if (AFile.Sessions.IndexOf(AName) <> -1) then
    begin
      Result := AFile;
      Exit;
    end;
  end;
end;

function TexPackageList.Add: TexPackage;
begin
  Result := TexPackage(inherited Add);
end;

function TexPackageList.GetItem(Index: Integer): TexPackage;
begin
  Result := TexPackage(inherited GetItem(Index));
end;

procedure TexPackageList.SetItem(Index: Integer; AValue: TexPackage);
begin
  inherited SetItem(Index, AValue);
end;

initialization
  GetRegisteredOptions.RegisterClass(sexSFileOptions, TexFileOptions);
  GetRegisteredOptions.RegisterClass(sexSHttpOptions, TexHttpOptions);

end.

