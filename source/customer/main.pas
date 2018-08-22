unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, fpjson, HTTPDefs, fastplaz_handler, database_lib,
  string_helpers, dateutils, datetime_helpers;

type

  { TMainModule }

  TMainModule = class(TMyCustomWebModule)
  private
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
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

// GET Method Handler
procedure TMainModule.Get;
begin
  Response.Content:= '{"method":"get"}';
end;

// POST Method Handler
procedure TMainModule.Post;
begin
  Response.Content:= '{"method":"post"}';
end;

procedure TMainModule.Put;
begin
  Response.Content:= '{"method":"put"}';
end;

procedure TMainModule.Patch;
begin
  Response.Content:= '{"method":"patch"}';
end;

procedure TMainModule.Delete;
begin
  Response.Content:= '{"method":"delete"}';
end;

end.
