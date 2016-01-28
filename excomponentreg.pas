unit exComponentReg;


interface

uses
  Classes, SysUtils, DesignIntf, DesignEditors, exExporter, exSerializer;

type
  TexSerializerProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

procedure Register;

implementation

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

function TexSerializerProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] +
    [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TexSerializerProperty.GetValue: string;
var
  AClassType: TexSerializerClass;
  AItem: TexRegisteredClassItem;
  ASerializer: TexSerializer;
begin
  Result := '';
  ASerializer := TexExporter(GetComponent(0)).Serializer;
  if (ASerializer <> nil) then
  begin
    AClassType := TexSerializerClass(TexExporter(GetComponent(0)).Serializer.ClassType);
    if HasSubProperties then
    begin
      AItem := GetRegisteredSerializers.FindByClassType(AClassType);
      if (AItem <> nil) then
        Result := AItem.Description;
    end;
  end;
end;

procedure TexSerializerProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to GetRegisteredSerializers.Count -1 do
    Proc(GetRegisteredSerializers.Items[I].Description);
end;

procedure TexSerializerProperty.SetValue(const Value: string);
var
  I: Integer;
  AItem: TexRegisteredClassItem;
begin
  AItem := GetRegisteredSerializers.FindByClassName(Value);

  if AItem = nil then
    AItem := GetRegisteredSerializers.FindByDescription(Value);

  if (AItem <> nil) then
  begin
    for I := 0 to PropCount - 1 do
      TexExporter(GetComponent(I)).SerializerClass := AItem.RegisteredClass;

    Modified;
  end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TexSerializer), TexExporter, 'Serializer', TexSerializerProperty);
  RegisterPropertyEditor(TypeInfo(string), TexExporter, 'SerializerClassName', nil);

  RegisterComponents('Data Export', [TexExporter]);
end;

end.

