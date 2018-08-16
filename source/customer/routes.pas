unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses info_controller, main, userprofile_controller;

initialization
  Route['info'] := TInfoModule;
  Route['^/([0-9_]+)/profile/$'] := TProfileModule; // http://localhost/api-test/customer/2/profile/
  Route['/'] := TMainModule;  //--> Route['main'] := TMainModule;

end.

