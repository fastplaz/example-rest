unit common_lib;

{$mode objfpc}{$H+}

interface

uses
  database_lib,
  Classes, SysUtils;

function IsPermitted:Boolean;

implementation

function IsPermitted: Boolean;
begin
  //TODO: create code to check permission
  Result := True;
end;

end.

