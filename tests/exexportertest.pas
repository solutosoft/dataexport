unit exExporterTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, RegExpr, fpcunit, testregistry, exExporter, exZeosProvider, exSerializer,
  ZConnection, ZSqlProcessor, ZScriptParser;

type

  { TexExporterTest }

  TexExporterTest = class(TTestCase)
  private
    FFixtureDir: String;
    FProvider: TexZeosProvider;
    FConnection: TZConnection;
    procedure PrepareDatabase;
    function CreateExporter(AFileName: String; ASerializer: TexSerializer): TexExporter;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    procedure TestColumnSize;
    procedure TestColumnDelimiter;
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
    ASQLProcessor.LoadFromFile(ConcatPaths([FFixtureDir, 'database-win.sql']));
    ASQLProcessor.Execute;
  finally
    ASQLProcessor.Free;
    FConnection.Connected := False;
  end;
end;

function TexExporterTest.CreateExporter(AFileName: String; ASerializer: TexSerializer): TexExporter;
begin
  Result := TexExporter.Create(nil);
  Result.LoadFromFile(ConcatPaths([FFixtureDir, AFileName]));
  Result.Provider := FProvider;
  Result.Serializer := ASerializer;
end;

procedure TexExporterTest.TestColumnSize;
var
  ASerializer: TexColumnSerializer;
  AExporter: TexExporter;
  AResult: TexResutMap;
  AData: TStrings;
  AFirst: String;
begin
  ASerializer := TexColumnSerializer.Create(nil);
  AExporter := CreateExporter('column-size.def', ASerializer);
  try
     AResult := AExporter.Execute;
     AssertEquals(1, AResult.Count);
     AssertTrue(AResult.IndexOf('test.txt') <> -1);

     AData := AResult['test.txt'];
     AssertEquals(3, AData.Count);

     AFirst := AData[0];
     AssertEquals(39, Length(AFirst));
     AssertEquals('010', Copy(AFirst, 1, 3));
     AssertEquals('Administra', Copy(AFirst, 4, 10));
     AssertEquals('Root ', Copy(AFirst, 14, 5));
     AssertEquals('20/04/1983', Copy(AFirst, 19, 10));
     AssertEquals('00153000', Copy(AFirst, 29, 8));
     AssertEquals('Yes', Copy(AFirst, 37, 3));
  finally
    AExporter.Free;
    ASerializer.Free;
  end;
end;

procedure TexExporterTest.TestColumnDelimiter;
var
  ASerializer: TexColumnSerializer;
  AExporter: TexExporter;
  AResult: TexResutMap;
  AParts,
  AData: TStrings;
begin
  AParts := TStringList.Create;
  ASerializer := TexColumnSerializer.Create(nil);
  AExporter := CreateExporter('column-delimiter.def', ASerializer);
  try
    ASerializer.Delimiter := '|';
    AResult := AExporter.Execute;

    AssertEquals(1, AResult.Count);
    AssertTrue(AResult.IndexOf('orders.txt') <> -1);

    AData := AResult['orders.txt'];
    AssertEquals(5, AData.Count);

    SplitRegExpr('\|', AData[0], AParts);
    AssertEquals(5, AParts.Count);

    AssertEquals('010', AParts[0]);
    AssertEquals('001', AParts[1]);
    AssertEquals('2015-11-10', AParts[2]);
    AssertEquals('Administrator', AParts[3]);
    AssertEquals('The first order - 1530,00', AParts[4]);

    SplitRegExpr('\|', AData[1], AParts);
    AssertEquals(5, AParts.Count);

    AssertEquals('020', AParts[0]);
    AssertEquals('1', AParts[1]);
    AssertEquals('2', AParts[2]);
    AssertEquals('10', AParts[3]);
    AssertEquals('20', AParts[4]);
  finally
    AParts.Free;
    AExporter.Free;
    ASerializer.Free;
  end;
end;

initialization
  RegisterTest(TexExporterTest);

end.

