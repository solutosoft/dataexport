unit exExporterTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, fpcunit, testutils, testregistry, exExporter, exZeosProvider, exSerializer,
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
    procedure TestColumnSerializer;
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

procedure TexExporterTest.TestColumnSerializer;
var
  ASerializer: TexColumnSerializer;
  AExporter: TexExporter;
  AResult: TStrings;
begin
  ASerializer := TexColumnSerializer.Create(nil);
  AExporter := CreateExporter('column-size.def', ASerializer);
  try
     AResult := AExporter.Execute;
     AssertEquals(3, AResult.Count);
  finally
    AExporter.Free;
    ASerializer.Free;
  end;
end;

initialization
  RegisterTest(TexExporterTest);

end.

