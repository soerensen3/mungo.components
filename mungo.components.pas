{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mungo.components;

{$warn 5023 off : no warning about unused units}
interface

uses
  mungo.components.filebrowser, mungo.components.base, mungo.components.colors, mungo.components.themes, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('mungo.components.filebrowser', @mungo.components.filebrowser.Register);
  RegisterUnit('mungo.components.base', @mungo.components.base.Register);
end;

initialization
  RegisterPackage('mungo.components', @Register);
end.
