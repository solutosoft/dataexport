unit exfiredacdriver;

interface

uses
  Classes, SysUtils, DB, DBClient, DBXCommon, Variants, exExporter, FireDAC.Comp.Client, FireDAC.Stan.Param,
  FireDAC.Phys.Intf, FireDAC.Phys.SQLPreprocessor;

type

  { TexFireDACProvider }

  TexFireDACDataSnapOptions = class(TPersistent)
  private
    FActive: Boolean;
    FServerMethodName: String;
  published
    property Active: Boolean read FActive write FActive;
    property ServerMethodName: String read FServerMethodName write FServerMethodName;
  end;

  TexFireDACDriver = class(TexDriver)
  private
    FConnection: TFDConnection;
    FDataSnapOptions: TexFireDACDataSnapOptions;
    procedure SetDataSnapOptions(const Value: TexFireDACDataSnapOptions);
    procedure ReadStream(AInput: TStream; AOutput: TStream);
  protected
    procedure AssignParamValues(AParams: TFDParams; const AValues: array of Variant);
    function CreateDataSnapQuery(ASQL: String; AMaster: TDataSet; const AParams: array of Variant): TFDMemTable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenConnection; override;
    procedure CloseConnection; override;
    procedure ExecSQL(ASQL: String; const AParams: array of Variant); override;
    function ExecSQLScalar(ASQL: String; const AParams: array of Variant): Variant; override;
    function CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet; override;
  published
    property Connection: TFDConnection read FConnection write FConnection;
    property DataSnapOptions: TexFireDACDataSnapOptions read FDataSnapOptions write SetDataSnapOptions;
  end;

procedure Register;

implementation

{ TexFireDACDriver }

constructor TexFireDACDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataSnapOptions := TexFireDACDataSnapOptions.Create;
end;

function TexFireDACDriver.CreateDataSnapQuery(ASQL: String; AMaster: TDataSet;
  const AParams: array of Variant): TFDMemTable;
var
  I: Integer;
  APreproc: TFDPhysPreprocessor;
  AField: TField;
  AValue: Variant;
  AParam: TFDParam;
  AFDParams: TFDParams;
  ASQLParams: TParams;
  AType: TFieldType;
  AOutput: TMemoryStream;
  AStream: TStream;
  ACommand: TDBXCommand;
begin
  Result := TFDMemTable.Create(Self);
  ACommand := (TObject(FConnection.CliObj) as TDBXConnection).CreateCommand;
  AOutput := TMemoryStream.Create;
  try
    ACommand.CommandType := TDBXCommandTypes.DSServerMethod;
    ACommand.Text := FDataSnapOptions.ServerMethodName;

    AFDParams := TFDParams.Create;
    ASQLParams := TParams.Create;
    APreproc := TFDPhysPreprocessor.Create;
    try
      APreproc.Params := AFDParams;
      APreproc.Source := ASQL;
      APreproc.DesignMode := False;
      APreproc.Instrs := [piCreateParams];
      APreproc.ConnMetadata := FConnection.ConnectionMetaDataIntf;
      APreproc.Execute;

      AssignParamValues(AFDParams, AParams);

      for I := 0 to AFDParams.Count -1 do
      begin
        AParam := AFDParams[I];
        AField := nil;

        if (AMaster <> nil) then
          AField := AMaster.FindField(AParam.Name);

        if (AField <> nil) then
        begin
          AParam.DataType := AField.DataType;
          AParam.Value := AField.Value;
        end
        else begin
          AValue := Exporter.ExtractParamValue(AParam.Name);
          if (not VarIsEmpty(AValue)) then
          begin
            AType := VarTypeToDataType(VarType(AValue));

            if (AType <> ftUnknown) then
              AParam.DataType := AType;

             AParam.Value := AValue;
          end;
        end;

        with ASQLParams.AddParameter do
        begin
          DataType := AParam.DataType;
          Name := AParam.Name;
          Value := AParam.Value;
        end;
      end;

      ACommand.Prepare;
      ACommand.Parameters[0].Value.SetWideString(ASQL);
      ACommand.Parameters[1].Value.AsVariant := PackageParams(ASQLParams);
      ACommand.ExecuteUpdate;

      AStream := ACommand.Parameters[2].Value.GetStream(False);
      try
        ReadStream(AStream, AOutput);
        Result.LoadFromStream(AOutput);
      finally
        AStream.Free;
      end;
    finally
      AOutput.Free;
      AFDParams.Free;
      APreproc.Free;
    end;

  finally
    ACommand.Free;
  end;
end;

destructor TexFireDACDriver.Destroy;
begin
  FDataSnapOptions.Free;
  inherited Destroy;
end;

procedure TexFireDACDriver.OpenConnection;
begin
  inherited;
  FConnection.Open;
end;

procedure TexFireDACDriver.ReadStream(AInput, AOutput: TStream);
const
  BufSize = 4096;
var
  ABuffer: PByte;
  AReaded: Integer;
begin
  if (not Assigned(AInput)) then
    Exit
  else begin
    GetMem(ABuffer, BufSize);
    try
      AInput.Position := 0;
      repeat
        AReaded := AInput.Read(Pointer(ABuffer)^, BufSize);
        if (AReaded > 0) then
          AOutput.WriteBuffer(Pointer(ABuffer)^, AReaded);
      until (AReaded < BufSize);
      AOutput.Position := 0;
    finally
      FreeMem(ABuffer, BufSize);
    end;
  end;
end;

procedure TexFireDACDriver.SetDataSnapOptions(const Value: TexFireDACDataSnapOptions);
begin
  FDataSnapOptions.Assign(Value);
end;

procedure TexFireDACDriver.AssignParamValues(AParams: TFDParams; const AValues: array of Variant);
var
  I: Integer;
  AType: TFieldType;
begin
  for I := Low(AValues) to High(AValues) do
  begin
    AType := VarTypeToDataType(VarType(AValues[I]));

    if (AType <> ftUnknown) then
      AParams[I].DataType := AType;

    AParams[I].Value := AValues[I];
  end;
end;

procedure TexFireDACDriver.CloseConnection;
begin
  inherited;
  FConnection.Close;
end;

procedure TexFireDACDriver.ExecSQL(ASQL: String; const AParams: array of Variant);
var
  AQuery: TFDQuery;
begin
  AQuery := TFDQuery.Create(nil);
  try
    AQuery.Connection := FConnection;
    AQuery.SQL.Text := ASQL;
    AssignParamValues(AQuery.Params, AParams);
    AQuery.ExecSQL;
  finally
    AQuery.Free;
  end;
end;

function TexFireDACDriver.ExecSQLScalar(ASQL: String; const AParams: array of Variant): Variant;
var
  AQuery: TFDQuery;
  ADataSet: TDataSet;
begin
  Result := Unassigned;

  if (FDataSnapOptions.Active) then
    ADataSet := CreateDataSnapQuery(ASQL, nil, [])
  else begin
    AQuery := TFDQuery.Create(nil);
    AQuery.Connection := FConnection;
    AQuery.SQL.Text := ASQL;
    AssignParamValues(AQuery.Params, AParams);
    AQuery.Open;

    ADataSet := AQuery;
  end;

  try
    if (ADataSet.FieldCount > 0) and (not ADataSet.IsEmpty) then
      Result := ADataSet.Fields[0].Value;
  finally
    ADataSet.Free;
  end;
end;

function TexFireDACDriver.CreateQuery(ASQL: String; AMaster: TDataSet): TDataSet;
var
  I: Integer;
  AParam: TFDParam;
  AField: TField;
  AValue: Variant;
  AType: TFieldType;
begin
  if (FDataSnapOptions.Active) then
  begin
    Result := CreateDataSnapQuery(ASQL, AMaster, []);
    Exit;
  end;


  Result := TFDQuery.Create(Self);
  with (TFDQuery(Result)) do
  begin
    SQL.Text := ASQL;
    Connection := FConnection;

    for I := 0 to Params.Count -1 do
    begin
      AParam := Params[I];
      AField := nil;

      if (AMaster <> nil) then
        AField := AMaster.FindField(AParam.Name);

      if (AField <> nil) then
      begin
        AParam.DataType := AField.DataType;
        AParam.Value := AField.Value;
      end
      else begin
        AValue := Exporter.ExtractParamValue(AParam.Name);
        if (not VarIsEmpty(AValue)) then
        begin
          AType := VarTypeToDataType(VarType(AValue));

          if (AType <> ftUnknown) then
            AParam.DataType := AType;

           AParam.Value := AValue;
        end;
      end;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Data Export', [TexFireDACDriver]);
end;

end.
