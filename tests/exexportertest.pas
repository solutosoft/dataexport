unit exExporterTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, RegExpr, fpcunit, testregistry, fpjson, jsonparser, laz2_DOM, laz2_XMLRead, exExporter,
  exZeosProvider, exSerializer, exDefinition, ZConnection, ZSqlProcessor, ZScriptParser;

type

  { TexExporterTest }

  TexExporterTest = class(TTestCase)
  private
    FFixtureDir: String;
    FProvider: TexZeosProvider;
    FConnection: TZConnection;
    procedure PrepareDatabase;
    function CreateExporter(AFileName: String): TexExporter;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    procedure TestStore;
    procedure TestColumnSize;
    procedure TestColumnDelimiter;
    procedure TestJson;
    procedure TestXml;
  end;

implementation

constructor TexExporterTest.Create;
begin
  FFixtureDir := ConcatPaths([ExtractFilePath(Application.ExeName), '../../fixtures']);
  if (not DirectoryExists(FFixtureDir)) then
    CreateDir(FFixtureDir);

  FConnection := TZConnection.Create(nil);
  FConnection.Protocol := 'sqlite-3';
  FConnection.Database := ConcatPaths([ExtractFilePath(Application.ExeName), 'export-test.db']);

  FProvider := TexZeosProvider.Create(nil);
  FProvider.Connection := FConnection;

  PrepareDatabase;
end;

destructor TexExporterTest.Destroy;
begin
  FConnection.Free;
  FProvider.Free;
  inherited Destroy;
end;

procedure TexExporterTest.PrepareDatabase;
var
  ASQLProcessor: TZSQLProcessor;
begin
  if (FileExists(FConnection.Database)) then
     DeleteFile(FConnection.Database);

  FConnection.Connected := True;
  ASQLProcessor := TZSQLProcessor.Create(nil);
  try
    ASQLProcessor.DelimiterType := dtDelimiter;
    ASQLProcessor.Connection := FConnection;
    ASQLProcessor.LoadFromFile(ConcatPaths([FFixtureDir, 'database.sql']));
    ASQLProcessor.Execute;
  finally
    ASQLProcessor.Free;
    FConnection.Connected := False;
  end;
end;

function TexExporterTest.CreateExporter(AFileName: String): TexExporter;
begin
  Result := TexExporter.Create(nil);
  LoadExporterFromFile(Result, ConcatPaths([FFixtureDir,  AFileName]));
  Result.Provider := FProvider;
end;

procedure TexExporterTest.TestStore;
var
  AExporter: TexExporter;
  ASession: TexSession;
  ASerializer: TexSerializer;
  AFileName: String;
begin
  ASerializer := TexXmlSerializer.Create(nil);
  AExporter := TexExporter.Create(nil);
  try
    AExporter.Serializer := ASerializer;

    with (AExporter.Pipelines.Add) do
    begin
      Name := 'persons-pipeline';
      SQL.Text := 'select * from persons';
    end;

    with (AExporter.Dictionaries.Add) do
    begin
      Name := 'money';
      Expression := 'Result := FormatFloat(''#,0.00'', Value);';
    end;

    with (AExporter.Parameters.Add) do
    begin
      Name := 'param1';
      Value := 1;
    end;

    ASession := AExporter.Sessions.Add;
    with(ASession) do
    begin
      Name := 'persons';
      Pipeline := 'persons-pipeline';
    end;

    with (ASession.Columns.Add) do
    begin
      Name := 'firstName';
      Size := 10;
    end;

    with (ASession.Columns.Add) do
    begin
      Name := 'lastName';
      Size := 10;
    end;

    AFileName := ConcatPaths([FFixtureDir, 'storage.def']);

    SaveExporterToFile(AExporter, AFileName);
    AssertTrue(FileExists(AFileName));

    AExporter := TexExporter.Create(nil);
    LoadExporterFromFile(AExporter, AFileName);

    AssertTrue(AExporter.Serializer <> nil);
    AssertEquals(1, AExporter.Pipelines.Count);
    AssertEquals(1, AExporter.Dictionaries.Count);
    AssertEquals(1, AExporter.Parameters.Count);
    AssertEquals(1, AExporter.Sessions.Count);
    AssertEquals(2, AExporter.Sessions[0].Columns.Count);
  finally
    ASerializer.Free;
    AExporter.Free;
  end;
end;

procedure TexExporterTest.TestColumnSize;
var
  AExporter: TexExporter;
  AResult: TexResutMap;
  AData: TStrings;
  ALine: String;
begin
  AExporter := CreateExporter('column-size.def');
  try
     AResult := AExporter.Execute;
     AssertEquals(2, AResult.Count);
     AssertTrue(AResult.IndexOf('persons.txt') <> -1);

     AData := AResult['persons.txt'];
     AssertEquals(3, AData.Count);

     ALine := AData[0];
     AssertEquals(39, Length(ALine));
     AssertEquals('010', Copy(ALine, 1, 3));
     AssertEquals('Administra', Copy(ALine, 4, 10));
     AssertEquals('Root ', Copy(ALine, 14, 5));
     AssertEquals('20/04/1983', Copy(ALine, 19, 10));
     AssertEquals('00153000', Copy(ALine, 29, 8));
     AssertEquals('Yes', Copy(ALine, 37, 3));

     AssertTrue(AResult.IndexOf('products.txt') <> -1);
     AData := AResult['products.txt'];
     AssertEquals(2, AData.Count);

     ALine := AData[0];
     AssertEquals('0000000001', Copy(ALine, 1, 10));
     AssertEquals('Data export extension    ', Copy(ALine, 11, 25));
  finally
    AExporter.Free;
  end;
end;

procedure TexExporterTest.TestColumnDelimiter;
var
  AExporter: TexExporter;
  AResult: TexResutMap;
  AParts,
  AData: TStrings;
begin
  AParts := TStringList.Create;
  AExporter := CreateExporter('column-delimiter.def');
  try
    AResult := AExporter.Execute;

    AssertEquals(1, AResult.Count);
    AssertTrue(AResult.IndexOf('orders.txt') <> -1);

    AData := AResult['orders.txt'];
    AssertEquals(5, AData.Count);

    SplitRegExpr('\|', AData[0], AParts);
    AssertEquals(7, AParts.Count); // the event adds aditional "|"

    AssertEquals('010', AParts[1]);
    AssertEquals('001', AParts[2]);
    AssertEquals('2015-11-10', AParts[3]);
    AssertEquals('Administrator', AParts[4]);
    AssertEquals('The first order - 1530,00', AParts[5]);

    SplitRegExpr('\|', AData[1], AParts);
    AssertEquals(7, AParts.Count);

    AssertEquals('020', AParts[1]);
    AssertEquals('1', AParts[2]);
    AssertEquals('2', AParts[3]);
    AssertEquals('10', AParts[4]);
    AssertEquals('20', AParts[5]);
  finally
    AParts.Free;
    AExporter.Free;
  end;
end;

procedure TexExporterTest.TestJson;
var
  ASerializer: TexJsonSerializer;
  AExporter: TexExporter;
  AResult: TexResutMap;
  AJson,
  ADetails,
  AData: TJSONData;
begin
  ASerializer := TexJsonSerializer.Create(nil);
  AExporter := CreateExporter('hierarchical.def');
  try
    AExporter.Serializer := ASerializer;
    ASerializer.HideRootKeys := True;
    AResult := AExporter.Execute;

    AssertEquals(1, AResult.Count);
    AssertTrue(AResult.IndexOf('invoices') <> -1);

    AJson := GetJSON(AResult['invoices'].Text);
    AssertEquals(2, AJson.Count);

    ASerializer.HideRootKeys := False;
    AResult := AExporter.Execute;
    AJson := GetJSON(AResult['invoices'].Text);

    AData := AJson.FindPath('invoices').Items[0];
    AssertEquals('100', AData.FindPath('type').AsString);
    AssertEquals('001', AData.FindPath('number').AsString);
    AssertEquals('2015-11-10', AData.FindPath('created_at').AsString);
    AssertEquals('The first order', AData.FindPath('description').AsString);

    ADetails := AData.FindPath('details');
    AssertEquals(2, ADetails.Count);

    AData := ADetails.Items[0];
    AssertEquals('200', AData.FindPath('type').AsString);
    AssertEquals('1', AData.FindPath('product_id').AsString);
    AssertEquals('2', AData.FindPath('quantity').AsString);
    AssertEquals('10', AData.FindPath('price').AsString);
    AssertEquals('20', AData.FindPath('total').AsString);

    AData := ADetails.Items[1];
    AssertEquals('200', AData.FindPath('type').AsString);
    AssertEquals('2', AData.FindPath('product_id').AsString);
    AssertEquals('5', AData.FindPath('quantity').AsString);
    AssertEquals('20', AData.FindPath('price').AsString);
    AssertEquals('100', AData.FindPath('total').AsString);
  finally
    AExporter.Free;
    ASerializer.Free;
  end
end;

procedure TexExporterTest.TestXml;
var
  ASerializer: TexXmlSerializer;
  AExporter: TexExporter;
  AResult: TexResutMap;
  AXml: TXMLDocument;
  ADetails,
  AItem: TDOMNode;
  AStream: TStringStream;
begin
  ASerializer := TexXmlSerializer.Create(nil);
  AExporter := CreateExporter('hierarchical.def');

  AXml := TXMLDocument.Create;
  try
    AExporter.Serializer := ASerializer;

    AResult := AExporter.Execute;
    AssertEquals(1, AResult.Count);
    AssertTrue(AResult.IndexOf('invoices') <> -1);

    AStream := TStringStream.Create(AResult['invoices'].Text);
    try
      ReadXMLFile(AXml, AStream);
      AssertEquals('UTF-8', AXml.Encoding);
      AssertEquals('1.0', AXml.XMLVersion);
      AssertTrue(AXml.FirstChild.CompareName('invoices') = 0);

      AssertEquals(2, AXml.FirstChild.ChildNodes.Count);
      AItem := AXml.FirstChild.ChildNodes[0];

      AssertEquals('100', AItem.FindNode('type').TextContent);
      AssertEquals('001', AItem.FindNode('number').TextContent);
      AssertEquals('2015-11-10', AItem.FindNode('created_at').TextContent);
      AssertEquals('The first order', AItem.FindNode('description').TextContent);

      ADetails := AItem.FindNode('details');
      AssertEquals(2, ADetails.ChildNodes.Count);

      AItem := ADetails.ChildNodes[0];
      AssertEquals('200', AItem.FindNode('type').TextContent);
      AssertEquals('1', AItem.FindNode('product_id').TextContent);
      AssertEquals('2', AItem.FindNode('quantity').TextContent);
      AssertEquals('10', AItem.FindNode('price').TextContent);
      AssertEquals('20', AItem.FindNode('total').TextContent);

      AItem := ADetails.ChildNodes[1];
      AssertEquals('200', AItem.FindNode('type').TextContent);
      AssertEquals('2', AItem.FindNode('product_id').TextContent);
      AssertEquals('5', AItem.FindNode('quantity').TextContent);
      AssertEquals('20', AItem.FindNode('price').TextContent);
      AssertEquals('100', AItem.FindNode('total').TextContent);
    finally
      AStream.Free;
    end;
  finally
    AXml.Free;
    AExporter.Free;
    ASerializer.Free;
  end
end;

initialization
  RegisterTest(TexExporterTest);

end.

