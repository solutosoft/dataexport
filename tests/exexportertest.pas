unit exExporterTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, fpcunit, testutils, testregistry, exExporter, exDefinition;

type

  { TexExporterTest }

  TexExporterTest = class(TTestCase)
  private
    FFixtureDir: String;
  protected
    procedure SetUp; override;
    function CreateExporter(AFileName: String): TexExporter;
  published
    procedure TestStorage;
    procedure TestColumnSerializer;
  end;

implementation

procedure TexExporterTest.TestStorage;
var
  AFileName: String;
  AExporter: TexExporter;
  ASession: TexSession;
begin
  AExporter := TexExporter.Create(nil);
  try
    with AExporter.Dictionaries.Add do
    begin
      Align := altRight;
      Name := 'money';
      Complete := '0';
      Size := 10;
      Expression := 'Result := RemoveMask(AValue);';
    end;

    with AExporter.Events.Add do
    begin
      Name := 'onSerializeData';
      Expression := 'Result := | + AValue + |';
    end;

    with AExporter.Parameters.Add do
    begin
      Name := 'param1';
    end;

    ASession := AExporter.Sessions.Add;
    with ASession.Columns.Add do
    begin
      Dictionary := 'money';
      Name := 'salary';
    end;

    AFileName := ConcatPaths([FFixtureDir, 'storage.def']);

    AExporter.SaveToFile(AFileName);
    AssertTrue(FileExists(AFileName));

    AExporter := TexExporter.Create(nil);
    AExporter.LoadFromFile(AFileName);

    AssertEquals(1, AExporter.Dictionaries.Count);
    AssertEquals(1, AExporter.Events.Count);
    AssertEquals(1, AExporter.Parameters.Count);
    AssertEquals(1, AExporter.Sessions.Count);
    AssertEquals(1, AExporter.Sessions[0].Columns.Count);
 finally
   AExporter.Free;
 end;
end;

procedure TexExporterTest.TestColumnSerializer;
var
  AExporter: TexExporter;
begin
 AExporter := CreateExporter('column.def');
 try

 finally
   AExporter.Free;
 end;
end;

procedure TexExporterTest.SetUp;
begin
  FFixtureDir := ConcatPaths([ExtractFilePath(Application.ExeName), '../../fixtures']);
  if (not DirectoryExists(FFixtureDir)) then
    CreateDir(FFixtureDir);
end;

function TexExporterTest.CreateExporter(AFileName: String): TexExporter;
begin
  Result := TexExporter.Create(nil);
  Result.LoadFromFile(ConcatPaths([FFixtureDir, AFileName]));
end;

initialization
  RegisterTest(TexExporterTest);

end.

