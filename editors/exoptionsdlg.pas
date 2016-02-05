unit exoptionsdlg;

interface

uses
  SysUtils, {$IFNDEF LCL} DesignIntf, DesignEditors, {$ELSE} PropEdits, {$ENDIF} Variants, Classes, Graphics, Controls, Forms,
  Grids, ExtCtrls, StdCtrls, StrUtils, Math, exClasses, exDefinition;

type
  TOptionsDlg = class(TForm)
    Grid: TStringGrid;
    PanBtn: TPanel;
    BtnClose: TButton;
    BtnOk: TButton;
    CbxValue: TComboBox;
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure CbxValueChange(Sender: TObject);
    procedure CbxValueExit(Sender: TObject);
  private
    FOptions: TexOptions;
  public
    procedure LoadComponent(AOptions: TexOptions);
    procedure SaveComponent;
  end;

  TexOptionsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation

{$R *.dfm}

{ TPackageOptionsDlg }

procedure TOptionsDlg.CbxValueChange(Sender: TObject);
begin
  Grid.Cells[Grid.Col, Grid.Row] := CbxValue.Items[CbxValue.ItemIndex];
  CbxValue.Visible := True;
end;

procedure TOptionsDlg.CbxValueExit(Sender: TObject);
begin
  Grid.Cells[Grid.Col, Grid.Row] := CbxValue.Items[CbxValue.ItemIndex];
  CbxValue.Visible := False;
end;

procedure TOptionsDlg.GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  ARect: TRect;
  AName,
  AValue: String;
  AItem: TexEditorItem;
begin
  if (ACol = 1) then
  begin
    AName := Grid.Cells[0, ARow];
    AValue := Grid.Cells[ACol, ARow];
    AItem := FOptions.Editors.FindByName(AName);

    if (AItem.EditorType in [edtText, edtPassword]) then
      Exit;

    ARect := Grid.CellRect(ACol, ARow);
    ARect.Left := ARect.Left + Grid.Left;
    ARect.Right := ARect.Right + Grid.Left;
    ARect.Top := ARect.Top + Grid.Top;
    ARect.Bottom := ARect.Bottom + Grid.Top;

    case AItem.EditorType of
      {
      edtInteger: ;
      edtCurrency: ;}
      edtBoolean:
      begin
        CbxValue.Clear;
        CbxValue.Items.Add('True');
        CbxValue.Items.Add('False');
        CbxValue.ItemIndex := IfThen(StrToBoolDef(AValue, False), 0, 1);
        CbxValue.Visible := True;
      end;
      {edtDate: ;
      edtDateTime: ;
      edtTime: ;
      edtFile: ;
      edtDirectory: ;
      edtLookup: ;
      edtCombobox: ;
      edtCheckbox: ;
      edtRangeDate: ;}
    end;
    if (CbxValue.Visible) then
    begin
      with CbxValue do
      begin
        Left := ARect.Left + 1;
        Top := ARect.Top + 1;
        Width := (ARect.Right + 1) - ARect.Left;
        Height := (ARect.Bottom + 1) - ARect.Top;
        SetFocus;
      end;
    end;
  end;
  CanSelect := True;
end;

procedure TOptionsDlg.LoadComponent(AOptions: TexOptions);
var
  I: Integer;
  AItem: TexEditorItem;
  AValue: String;
begin
  FOptions := AOptions;
  Grid.RowCount := FOptions.Editors.Count;

  for I := 0 to FOptions.Editors.Count -1 do
  begin
    AItem := FOptions.Editors[I];
    AValue := FOptions.Values[AItem.Name];

    Grid.Cells[0, I] := AItem.Name;
    Grid.Cells[1, I] := IfThen(AValue = '', AItem.DefaultValue, AValue);
  end;
end;

procedure TOptionsDlg.SaveComponent;
var
  I: Integer;
begin
  for I := 0 to Grid.RowCount -1 do
    FOptions.Values[Grid.Cells[0, I]] := Grid.Cells[1, I];
end;

{ TexOptionsProperty }

procedure TexOptionsProperty.Edit;
var
  AComp: TexPackage;
  AFrm: TOptionsDlg;
begin
  AComp := TexPackage(GetComponent(0));
  AFrm := TOptionsDlg.Create(nil);
  try
    AFrm.LoadComponent(AComp.Options);
    if (AFrm.ShowModal = mrOk) then
    begin
      AFrm.SaveComponent;
      Modified;
    end;
  finally
    AFrm.Free;
  end;
end;

function TexOptionsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

end.
