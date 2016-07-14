unit exscript;

interface

uses
  SysUtils, uPSRuntime, uPSCompiler {$IFDEF FPC}, LCLIntf {$ELSE}, Windows {$ENDIF},
  exDefinition, exOptions;

procedure RegisterTexValueClass_R(S: TPSRuntimeClassImporter);
procedure RegisterTexOptionsClass_R(S: TPSRuntimeClassImporter);

procedure RegisterTexValueClass_C(S: TPSPascalCompiler);
procedure RegisterTexOptionsClass_C(S: TPSPascalCompiler);

implementation

procedure RegisterTexValueClass_R(S: TPSRuntimeClassImporter);
begin

  with S.Add(TexValue) do
  begin
    RegisterConstructor(@TexValue.Create, 'Create');
    RegisterMethod(@TexValue.IsNull, 'IsNull');
    RegisterMethod(@TexValue.AsVariant, 'AsVariant');
    RegisterMethod(@TexValue.AsString, 'AsString');
    RegisterMethod(@TexValue.AsInteger, 'AsInteger');
    RegisterMethod(@TexValue.AsFloat, 'AsFloat');
    RegisterMethod(@TexValue.AsBoolean, 'AsBoolean');
    RegisterMethod(@TexValue.AsDateTime, 'AsDateTime');
    RegisterMethod(@TexValue.AsDateBegin, 'AsDateBegin');
    RegisterMethod(@TexValue.AsDateEnd, 'AsDateEnd');
    RegisterMethod(@TexValue.AsArray, 'AsArray');
  end;
end;

procedure RegisterTexOptionsClass_R(S: TPSRuntimeClassImporter);
begin
  with S.Add(TexOptions) do
  begin
    RegisterConstructor(@TexOptions.Create, 'Create');
    RegisterMethod(@TexOptions.GetAsString, 'GetAsString');
    RegisterMethod(@TexOptions.GetAsInteger, 'GetAsInteger');
    RegisterMethod(@TexOptions.GetAsFloat, 'GetAsFloat');
    RegisterMethod(@TexOptions.GetAsBoolean, 'GetAsBoolean');
  end;
  S.Add(TexHttpOptions);
  S.Add(TexFileOptions);
end;

procedure RegisterTexValueClass_C(S: TPSPascalCompiler);
begin
  S.AddTypeS('TVariantDynArray', 'array of Variant');
  S.AddTypeS('TexDataType', '(datNone, datText, datInteger, datDateTime, datBoolean, datFloat, datCurrency)');

  with S.AddClassN(S.FindClass('TObject'), 'TexValue') do
  begin
    RegisterMethod('Constructor Create(AValue : Variant)');
    RegisterMethod('Function IsNull : Boolean');
    RegisterMethod('Function AsVariant : Variant');
    RegisterMethod('Function AsString : Variant');
    RegisterMethod('Function AsInteger : Variant');
    RegisterMethod('Function AsFloat : Variant');
    RegisterMethod('Function AsBoolean : Variant');
    RegisterMethod('Function AsDateTime : Variant');
    RegisterMethod('Function AsDateBegin : Variant');
    RegisterMethod('Function AsDateEnd : Variant');
    RegisterMethod('Function AsArray : TVariantDynArray');
  end;
end;

procedure RegisterTexOptionsClass_C(S: TPSPascalCompiler);
begin
  with S.AddClassN(S.FindClass('TStringList'), 'TexOptions') do
  begin
    RegisterMethod('Constructor Create');
    RegisterMethod('Function GetAsString( AName : String; ADefault : String) : String');
    RegisterMethod('Function GetAsInteger( AName : String; ADefault : Integer) : Integer');
    RegisterMethod('Function GetAsFloat( AName : String; ADefault : Double) : Double');
    RegisterMethod('Function GetAsBoolean( AName : String; ADefault : Boolean) : Boolean');
  end;

  S.AddClassN(S.FindClass('TexOptions'),'TexHttpOptions');
  S.AddClassN(S.FindClass('TexOptions'),'TexFileOptions');
end;

end.

