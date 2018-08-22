unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses info_controller, main, userprofile_controller, customers_controller;

initialization
  Route['info'] := TInfoModule;
  Route['^/([0-9_]+)/profile/$'] := TProfileModule; // http://localhost/api-test/customer/2/profile/
  Route['^/([0-9_]+)'] := TCustomersModule; // handler uri: "/customer/{id}/"
  Route['main'] := TCustomersModule;

  // other variation url
  //Route.Add( '/', TCustomersModule, 'POST');
  //Route.Add( '/', TMainModule, 'GET');
  //Route['/'] := TMainModule;  //--> Route['main'] := TMainModule;

end.

