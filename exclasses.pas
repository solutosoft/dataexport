unit exClasses;

interface

uses
  System.SysUtils, System.Classes;

type
  TexEditorType = (
    edtText, edtPassword, edtInteger, edtCurrency, edtBoolean, edtDate, edtDateTime, edtTime,
    edtFile, edtDirectory, edtLookup, edtCombobox, edtCheckbox, edtRangeDate
  );


  { TexRegisteredClassItem }

  TexRegisteredClassItem = class(TCollectionItem)
  private
    FRegisteredClass: TClass;
    FDescription: String;
  public
    property RegisteredClass: TClass read FRegisteredClass write FRegisteredClass;
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
    function FindByClassType(AClassType: TClass): TexRegisteredClassItem;
    function FindByClassName(AClassName: String): TexRegisteredClassItem;
    procedure RegisterClass(ADescription: String; AClass: TClass);
  end;

  { TexEditorItem }

  TexEditorItem = class(TCollectionItem)
  private
    FName: String;
    FEditorType: TexEditorType;
    FDefaultValue: String;
  public
    property Name: String read FName write FName;
    property EditorType: TexEditorType read FEditorType write FEditorType;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
  end;

  { TexEditorList }

  TexEditorList = class(TCollection)
  private
    function GetItem(Index: Integer): TexEditorItem;
    procedure SetItem(Index: Integer; AValue: TexEditorItem);
  public
    constructor Create;
    function Add: TexEditorItem; overload;
    function Add(AName: String; AEditorType: TexEditorType; ADefaultValue: String = ''): TexEditorItem; overload;
    function FindByName(AName: String): TexEditorItem;
    property Items[Index: Integer]: TexEditorItem read GetItem write SetItem; default;
  end;

implementation

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

function TexRegisteredClasses.FindByClassType(AClassType: TClass): TexRegisteredClassItem;
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

procedure TexRegisteredClasses.RegisterClass(ADescription: String; AClass: TClass);
begin
  with Add do
  begin
    Description := ADescription;
    RegisteredClass := AClass;
  end;
end;

{ TexEditorList }

constructor TexEditorList.Create;
begin
  inherited Create(TexEditorItem);
end;

function TexEditorList.Add: TexEditorItem;
begin
  Result := TexEditorItem(inherited Add);
end;

function TexEditorList.Add(AName: String; AEditorType: TexEditorType; ADefaultValue: String = ''): TexEditorItem;
begin
  Result := Add;
  with Result do
  begin
    Name := AName;
    EditorType := AEditorType;
    DefaultValue := ADefaultValue;
  end;
end;

function TexEditorList.FindByName(AName: String): TexEditorItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.Count -1 do
  begin
    if (SameText(Self.Items[I].Name, AName)) then
      Result := Self.Items[I];
  end;
end;

function TexEditorList.GetItem(Index: Integer): TexEditorItem;
begin
  Result := TexEditorItem(inherited GetItem(Index));
end;

procedure TexEditorList.SetItem(Index: Integer; AValue: TexEditorItem);
begin
  inherited SetItem(Index, AValue);
end;

end.
