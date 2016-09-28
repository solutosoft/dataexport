unit exComponentReg;


interface

uses
  Classes, SysUtils,{$IFNDEF LCL} DesignIntf, DesignEditors, {$ELSE} PropEdits, {$ENDIF}
  exClasses, exDefinition, exOptions, exExporter, exSerializer, exOptionsDlg, exComboItemsDlg;

type

  { TexDynamicProperty }

  TexDynamicProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean; virtual; abstract;
    function GetRegisteredClasses: TexRegisteredClasses; virtual; abstract;
    function GetClassType: TClass; virtual; abstract;
    procedure SetPropClassValue(AComponent: TPersistent; AClass: TClass); virtual; abstract;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TexSerializerProperty }

  TexSerializerProperty = class(TexDynamicProperty)
  protected
    function HasSubProperties: Boolean; override;
    function GetRegisteredClasses: TexRegisteredClasses; override;
    function GetClassType: TClass; override;
    procedure SetPropClassValue(AComponent: TPersistent; AClass: TClass); override;
  end;

  { TexOptionsClassNameProperty }

  TexPackageTypeProperty = class(TexDynamicProperty)
  protected
    function HasSubProperties: Boolean; override;
    function GetRegisteredClasses: TexRegisteredClasses; override;
    function GetClassType: TClass; override;
    procedure SetPropClassValue(AComponent: TPersistent; AClass: TClass); override;
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;

implementation

{ TexDynamicProperty }

function TexDynamicProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] +
    [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TexDynamicProperty.GetValue: string;
var
  AClassType: TClass;
  AItem: TexRegisteredClassItem;
begin
  Result := '';
  AClassType := GetClassType;

  if (AClassType <> nil) and (HasSubProperties) then
  begin
    AItem := GetRegisteredClasses.FindByClassType(AClassType);
    if (AItem <> nil) then
      Result := AItem.Description;
  end;
end;

procedure TexDynamicProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to GetRegisteredClasses.Count -1 do
    Proc(GetRegisteredClasses.Items[I].Description);
end;

procedure TexDynamicProperty.SetValue(const Value: string);
var
  I: Integer;
  AItem: TexRegisteredClassItem;
begin
  AItem := GetRegisteredClasses.FindByClassName(Value);

  if AItem = nil then
    AItem := GetRegisteredClasses.FindByDescription(Value);

  if (AItem <> nil) then
  begin
    for I := 0 to PropCount - 1 do
      SetPropClassValue(GetComponent(I), AItem.RegisteredClass);

    Modified;
  end;
end;

{ TexSerializerProperty }

function TexSerializerProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := TexExporter(GetComponent(I)).Serializer <> nil;
    if not Result then Exit;
  end;
  Result := True;
end;

function TexSerializerProperty.GetClassType: TClass;
var
  ASerializer: TexSerializer;
begin
  Result := nil;
  ASerializer := TexExporter(GetComponent(0)).Serializer;
  if (ASerializer <> nil) then
    Result := ASerializer.ClassType;
end;

function TexSerializerProperty.GetRegisteredClasses: TexRegisteredClasses;
begin
  Result := GetRegisteredSerializers;
end;

procedure TexSerializerProperty.SetPropClassValue(AComponent: TPersistent; AClass: TClass);
begin
  TexExporter(AComponent).SerializerClass := TexSerializerClass(AClass);
end;

{ TexPackageTypeProperty }

function TexPackageTypeProperty.GetAttributes: TPropertyAttributes;
begin
  Result :=  [paValueList, paSortList];
end;

function TexPackageTypeProperty.GetClassType: TClass;
var
  AOptions: TexOptions;
begin
  Result := nil;
  AOptions := TexPackage(GetComponent(0)).Options;
  if (AOptions <> nil) then
    Result := AOptions.ClassType;
end;

function TexPackageTypeProperty.GetRegisteredClasses: TexRegisteredClasses;
begin
  Result := GetRegisteredOptions;
end;

function TexPackageTypeProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := TexPackage(GetComponent(I)).Options <> nil;
    if not Result then Exit;
  end;
  Result := True;
end;

procedure TexPackageTypeProperty.SetPropClassValue(AComponent: TPersistent; AClass: TClass);
begin
  TexPackage(AComponent).OptionsClass := TexOptionsClass(AClass);
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TexExporter, 'SerializerClassName', nil);
  RegisterPropertyEditor(TypeInfo(TexSerializer), TexExporter, 'Serializer', TexSerializerProperty);

  RegisterPropertyEditor(TypeInfo(string), TexPackage, 'PackageType', TexPackageTypeProperty);
  RegisterPropertyEditor(TypeInfo(TexOptions), TexPackage, 'Options', TexOptionsProperty);

  RegisterPropertyEditor(TypeInfo(TexComboList), TexParameter, 'Items', TexComboItemsProperty);

  RegisterComponents('Data Export', [TexExporter]);
end;

end.

