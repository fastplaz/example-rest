unit customer_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib;

type
  TCustomerModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
  end;

implementation

constructor TCustomerModel.Create(const DefaultTableName: string = '');
begin
  inherited Create( DefaultTableName); // table name = customerss
  //inherited Create('yourtablename'); // if use custom tablename
  primaryKey := 'cid'; // Primary Key Field
end;

end.

