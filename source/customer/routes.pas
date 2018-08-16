unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses info_controller, main;

initialization
  Route['/'] := TMainModule;  //--> Route['main'] := TMainModule;
  Route['info'] := TInfoModule;

end.

