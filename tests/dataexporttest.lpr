program dataexporttest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, pascalscript, exExporterTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

