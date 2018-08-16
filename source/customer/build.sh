#!/bin/bash
pwd
mkdir lib
fpc customer.lpr @extra.cfg

echo $PWD
echo Done.... $1
