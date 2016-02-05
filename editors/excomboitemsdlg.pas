unit excomboitemsdlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, Grids,
  DesignIntf, DesignEditors, exDefinition;

type
  TCustomGridAccess = class(TCustomGrid);

  TComboItemsDlg = class(TForm)
    Grid: TStringGrid;
    CbxType: TComboBox;
    PanBtn: TPanel;
    BtnOk: TButton;
    BtnClose: TButton;
    BtnAdd: TButton;
    BtnDel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure CbxTypeChange(Sender: TObject);
    procedure CbxTypeExit(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnDelClick(Sender: TObject);
  private
    FComboItems: TexComboList;
  public
    procedure LoadComponent(AComboItems: TexComboList);
    procedure SaveComponent;
  end;

  TexComboItemsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

var
  ComboItemsDlg: TComboItemsDlg;

const
  exVarTypes: array [0..11] of Word = (
    varBoolean, varByte, varLongWord, varCurrency, varDate,  varDouble, varInteger,
    varShortInt, varSingle, varSmallint, varString, varWord
  );

implementation

{$R *.dfm}

procedure TComboItemsDlg.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Grid.ColWidths[0] := 180;
  Grid.Cells[0, 0] := 'Description';

  Grid.ColWidths[1] := 100;
  Grid.Cells[1, 0] := 'Data type';

  Grid.ColWidths[2] := 150;
  Grid.Cells[2, 0] := 'Value';

  for I := Low(exVarTypes) to High(exVarTypes) do
    CbxType.Items.Add(VarTypeAsText(exVarTypes[I]));
end;

procedure TComboItemsDlg.CbxTypeChange(Sender: TObject);
begin
  Grid.Cells[Grid.Col, Grid.Row] := CbxType.Items[CbxType.ItemIndex];
end;

procedure TComboItemsDlg.CbxTypeExit(Sender: TObject);
begin
  Grid.Cells[Grid.Col, Grid.Row] := CbxType.Items[CbxType.ItemIndex];
  CbxType.Visible := False;
end;

procedure TComboItemsDlg.GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  ARect: TRect;
  AIndex: Integer;
begin
  Grid.Enabled := ARow > 0;

  if (ACol = 1) and (Grid.Enabled) then
  begin
    AIndex := CbxType.Items.IndexOf(Grid.Cells[ACol, ARow]);
    CbxType.ItemIndex := AIndex;

    ARect := Grid.CellRect(ACol, ARow);
    ARect.Left := ARect.Left + Grid.Left;
    ARect.Right := ARect.Right + Grid.Left;
    ARect.Top := ARect.Top + Grid.Top;
    ARect.Bottom := ARect.Bottom + Grid.Top;

    with CbxType do
    begin
      Left := ARect.Left + 1;
      Top := ARect.Top + 1;
      Width := (ARect.Right + 1) - ARect.Left;
      Height := (ARect.Bottom + 1) - ARect.Top;
      Visible := True;
      SetFocus;
    end;
  end;
  CanSelect := True;
end;

procedure TComboItemsDlg.BtnAddClick(Sender: TObject);
var
  ARowCount: Integer;
begin
  ARowCount := Grid.RowCount +1;
  Grid.RowCount := ARowCount;
  Grid.Row := ARowCount -1;
  Grid.Enabled := True;
  Grid.SetFocus;
end;

procedure TComboItemsDlg.BtnDelClick(Sender: TObject);
begin
  if (Grid.RowCount > 1) then
    TCustomGridAccess(Grid).DeleteRow(Grid.Row);
end;

procedure TComboItemsDlg.LoadComponent(AComboItems: TexComboList);
var
  I,
  ARow: Integer;
begin
  FComboItems := AComboItems;
  Grid.RowCount := AComboItems.Count + 1;
  for I := 0 to AComboItems.Count -1 do
  begin
    ARow := I+1;
    Grid.Cells[0, ARow] := AComboItems[I].Descrition;
    Grid.Cells[1, ARow] := VarTypeAsText(VarType(AComboItems[I].Value));
    Grid.Cells[2, ARow] := VarToStrDef(AComboItems[I].Value, '');
  end;
end;

procedure TComboItemsDlg.SaveComponent;
var
  I,
  AIndex: Integer;
  AVarType: TVarType;
  AValue: Variant;
begin
  FComboItems.Clear;
  for I := 1 to Grid.RowCount -1 do
  begin
    AIndex := CbxType.Items.IndexOf(Grid.Cells[1, I]);
    AVarType := exVarTypes[AIndex];
    AValue := Grid.Cells[2, I];

    with (FComboItems.Add) do
    begin
      Descrition := Grid.Cells[0, I];
      Value := VarAsType(AValue, AVarType);
    end;
  end;
end;

{ TexComboItemsProperty }

procedure TexComboItemsProperty.Edit;
var
  AComp: TexParameter;
  AFrm: TComboItemsDlg;
begin
  AComp := TexParameter(GetComponent(0));
  AFrm := TComboItemsDlg.Create(nil);
  try
    AFrm.LoadComponent(AComp.Items);
    if (AFrm.ShowModal = mrOk) then
    begin
      AFrm.SaveComponent;
      Designer.Modified;
    end;
  finally
    AFrm.Free;
  end;
end;

function TexComboItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

end.
