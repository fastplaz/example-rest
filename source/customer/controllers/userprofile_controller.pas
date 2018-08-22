unit userprofile_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, fpjson, HTTPDefs, fastplaz_handler, database_lib;

type
  TProfileModule = class(TMyCustomWebModule)
  private
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
  end;

implementation

uses common, json_lib, customer_model;

constructor TProfileModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
end;

destructor TProfileModule.Destroy;
begin
  inherited Destroy;
end;

// GET Method Handler
// example url:
//   http://localhost/api-test/customer/2/profile/
procedure TProfileModule.Get;
var
  json: TJSONUtil;
begin
  json := TJSONUtil.Create;
  json['code'] := Int16(404);

  DataBaseInit();
  with TCustomerModel.Create() do
  begin
    if Find( s2i(_GET['$1'])) then
    begin
      json['code'] := Int16(200);
      json['response/data/id'] := _GET['$1']; // Get First Parameter in url
      json['response/data/name'] := Value['name'];
      json['response/data/description'] := Value['description'];
      json['response/data/profile/biography'] := 'this is example user profile';
      json['response/data/profile/address'] := 'example address';
      json['response/data/profile/city'] := 'example city';
    end;

    Free;
  end;

  Response.ContentType := 'application/json';
  Response.Content := json.AsJSONFormated;
  json.Free;
end;


initialization
  // -> http://yourdomainname/profile
  // The following line should be moved to a file "routes.pas"
  // Route.Add('profile', TProfileModule);

end.

