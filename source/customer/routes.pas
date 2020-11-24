unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses info_controller, main, userprofile_controller, customers_controller;

initialization
  Route['info'] := TInfoModule;

  // regex dgn index match
  //Route['^/([0-9_]+)/profile/$'] := TProfileModule; // http://localhost/api-test/customer/2/profile/
  //Route['^/([0-9_]+)'] := TCustomersModule; // handler uri: "/customer/{id}/"

  // contoh regex dengan nama group
  Route['^/(?P<id>[0-9_]+)/profile/$'] := TProfileModule; // http://localhost/api-test/customer/2/profile/
  Route['^/(?P<id>[0-9_]+)'] := TCustomersModule; // handler uri: "/customer/{id}/"

  Route['/'] := TCustomersModule;

  // other variation url
  //Route.Add( '/', TCustomersModule, 'POST');
  //Route.Add( '/', TMainModule, 'GET');
  //Route['/'] := TMainModule;  //--> Route['main'] := TMainModule;

end.

