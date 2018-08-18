unit main;

{$mode objfpc}{$H+}

interface

uses
  customer_model,
  Classes, SysUtils, fpcgi, fpjson, HTTPDefs, fastplaz_handler, database_lib,
  string_helpers, dateutils, datetime_helpers;

type

  { TMainModule }

  TMainModule = class(TMyCustomWebModule)
  private
    Customer: TCustomerModel;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
    function getAllData: string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
    procedure Put; override;
    procedure Patch; override;
    procedure Delete; override;
  end;

implementation

uses
  json_lib, common, common_lib;

const
  MSG_CONTENT_NOTFOUND = 'Content Not Found';
  MSG_DATA_NOTFOUND = 'Data Not Found';

constructor TMainModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TMainModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TMainModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
end;

function TMainModule.getAllData: string;
var
  json: TJSONUtil;
  jArray: TJSONArray;
  iLimit, iOffset: integer;
begin
  json := TJSONUtil.Create;
  json['code'] := Int16(404);
  DataBaseInit();

  Customer := TCustomerModel.Create();
  iLimit := s2i(_GET['limit']);
  iOffset := s2i(_GET['offset']);
  if Customer.Select('cid as id,name,description').Limit(iLimit, iOffset).Open() then
  begin
    jArray := TJSONArray.Create;
    DataToJSON(Customer.Data, jArray, False);

    json['code'] := Int16(200);
    json['count'] := jArray.Count;
    json.ValueArray['data'] := jArray;
    json['timeUsage'] := TimeUsage;
  end;

  Result := json.AsJSONFormated;  // json formated, easy to see json string

  Customer.Free;
  json.Free;
end;

// GET Method Handler
// READ CUSTOMER INFORMATION
// CURL example:
//   Get All Data
//     curl -X GET "http://localhost/api-test/customer/"
//   Get Specific Data
//     curl -X GET "http://localhost/api-test/customer/?id=1"
procedure TMainModule.Get;
var
  json: TJSONUtil;
  s: string;
begin
  json := TJSONUtil.Create;
  json['code'] := Int16(404);

  s := _GET['$1']; // <<-- first parameter in routing: Route['^/([0-9_]+)'] := TMainModule;
  if ((not s.isEmpty) and (s.IsNumeric)) then
    _GET['id'] := s;  // force to parameter 'id'

  //---
  if _GET['id'] = '' then
  begin
    Response.Content := getAllData;
    Exit;
  end;

  DataBaseInit();

  Customer := TCustomerModel.Create();
  if not Customer.Find(_GET['id']) then
  begin
    json['code'] := Int16(404);
    json['message'] := MSG_DATA_NOTFOUND;
    Response.Content := json.AsJSON;
    Response.Code := 404;
    Customer.Free;
    json.Free;
    Exit;
  end;

  json['code'] := Int16(200);
  json['data/id'] := Customer['cid'];
  json['data/name'] := Customer['name'];
  json['data/description'] := Customer['description'];
  json['timeUsage'] := TimeUsage;
  Response.Content := json.AsJSON;

  Customer.Free;
  json.Free;
end;

// POST Method Handler
// ADD CUSTOMER
// CURL example:
//   curl -X POST "http://localhost/api-test/customer/" --data "name=your name&description=this is description"
procedure TMainModule.Post;
var
  json: TJSONUtil;
  authstring: string;
begin
  authstring := Header['Authorization'];
  if authstring <> 'YourAuthKey' then
  begin

  end;
  //TODO: Check Permission

  json := TJSONUtil.Create;
  json['code'] := 204;

  // Check Simple Validation
  if _POST['name'] = '' then
  begin
    json['code'] := Int16(204);
    json['message'] := MSG_CONTENT_NOTFOUND;
    Response.Content := json.AsJSON;
    json.Free;
    Exit;
  end;

  DataBaseInit();

  Customer := TCustomerModel.Create();
  Customer['name'] := UpperCase(_POST['name']);  // make sure validate it first
  Customer['description'] := _POST['description'];
  if Customer.Save() then
  begin
    json['code'] := Int16(200);
    json['data/customer_id'] := Customer.LastInsertID;
  end;
  Customer.Free;

  json['timeUsage'] := TimeUsage;
  CustomHeader['ThisIsCustomHeader'] := 'dataCustomHeader';

  //---
  Response.Content := json.AsJSON;
  json.Free;
end;

// UPDATE CUSTOMER - All Data
// Curl example
//   curl -X PUT "http://localhost/api-test/customer/?id=1" --data "name=your name&description=this is description"
procedure TMainModule.Put;
var
  json: TJSONUtil;
begin
  //TODO: Check Permission

  json := TJSONUtil.Create;
  json['code'] := Int16(204);

  //---
  if _GET['id'] = '' then
  begin
    json['message'] := MSG_CONTENT_NOTFOUND;
    Response.Content := json.AsJSON;
    Exit;
  end;

  DataBaseInit();

  Customer := TCustomerModel.Create();
  if not Customer.Find(_GET['id']) then
  begin
    json['code'] := Int16(404);
    json['id'] := _GET['id'];
    json['message'] := MSG_DATA_NOTFOUND;
    Response.Content := json.AsJSON;
    Response.Code := 404;
    Customer.Free;
    json.Free;
    Exit;
  end;

  Customer['name'] := UpperCase(_POST['name']);  // make sure validate it first
  Customer['description'] := _POST['description'];
  if Customer.Save('cid=' + _GET['id']) then
  begin
    json['code'] := Int16(200);
    json['message'] := 'OK';
  end;

  Response.Content := json.AsJSON;
  Customer.Free;
  json.Free;
end;

// UPDATE CUSTOMER - Partial Data
// Curl example
//   curl -X PATCH "http://localhost/api-test/customer/?id=1" --data "name=my name"
procedure TMainModule.Patch;
var
  json: TJSONUtil;
begin
  //TODO: Check Permission

  json := TJSONUtil.Create;
  json['code'] := Int16(204);

  //---
  if _GET['id'] = '' then
  begin
    json['message'] := MSG_CONTENT_NOTFOUND;
    Response.Content := json.AsJSON;
    Exit;
  end;

  DataBaseInit();

  Customer := TCustomerModel.Create();
  if not Customer.Find(_GET['id']) then
  begin
    json['code'] := Int16(404);
    json['id'] := _GET['id'];
    json['message'] := MSG_DATA_NOTFOUND;
    Response.Content := json.AsJSON;
    Response.Code := 404;
    Customer.Free;
    json.Free;
    Exit;
  end;

  if _POST['name'] <> '' then
    Customer['name'] := UpperCase(_POST['name']);  // make sure validate it first
  if _POST['description'] <> '' then
    Customer['description'] := _POST['description'];
  if Customer.Save('cid=' + _GET['id']) then
  begin
    json['code'] := Int16(200);
    json['message'] := 'OK';
  end;

  Response.Content := json.AsJSON;
  Customer.Free;
  json.Free;
end;

// DELETE CUSTOMER
// Curl example
//   curl -X DELETE "http://localhost/api-test/customer/?id=7"
procedure TMainModule.Delete;
var
  json: TJSONUtil;
begin
  //TODO: Check Permission

  json := TJSONUtil.Create;
  json['code'] := Int16(204);

  //---
  if _GET['id'] = '' then
  begin
    json['message'] := MSG_CONTENT_NOTFOUND;
    Response.Content := json.AsJSON;
    Exit;
  end;

  DataBaseInit();
  json['message'] := MSG_DATA_NOTFOUND;

  Customer := TCustomerModel.Create();
  if Customer.Delete(s2i(_GET['id'])) then
  begin
    if Customer.Data.RowsAffected > 0 then
    begin
      json['code'] := Int16(200);
      json['message'] := 'OK';
    end;
  end;
  Customer.Free;

  Response.Content := json.AsJSON;
  json.Free;
end;



end.
