{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mungo.components;

{$warn 5023 off : no warning about unused units}
interface

uses
  mungo.components.colors, mungo.components.types, mungo.components.renderer, 
  mungo.components.geometry, mungo.components.styles, mungo.components.widget, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('mungo.components', @Register);
end.
