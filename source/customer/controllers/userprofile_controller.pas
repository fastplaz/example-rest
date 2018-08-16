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

uses common, json_lib;

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
  json['code'] := Int16(200);
  json['data/id'] := _GET['$1']; // Get First Parameter in url
  json['data/message'] := 'this is example user profile';
  Response.ContentType := 'application/json';
  Response.Content := json.AsJSON;
  json.Free;
end;


initialization
  // -> http://yourdomainname/profile
  // The following line should be moved to a file "routes.pas"
  // Route.Add('profile', TProfileModule);

end.

