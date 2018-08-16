# Simple REST API Example

Source ini adalah contoh implementasi REST (API) dengan menggunakan [FastPlaz](http://www.fastplaz.com)

## How To Build

### Requirements

- [FastPlaz_runtime](http://www.fastplaz.com/)


### Source

Dari tulisan ini, saya mencontohkan penggunaan GIT untuk mendapatkan source dari project ini


```bash
$ mkdir -p REST/source/vendors
$ cd REST/source
$ git clone https://github.com/fastplaz/example-rest.git

# install vendors

$ cd vendors
$ git clone -b development https://github.com/fastplaz/fastplaz.git


```


### Compiling


Ada beberapa cara untuk bisa meng-compile code ini, diantaranya:

1. Melalui IDE Lazarus
2. Melalui command-line (fpc)

**Compile dari IDE Lazarus**

Jika menggunakan Lazarus, buka file `customer.lpi` dan compile file tersebut.

Akan terbentuk file binary di `public_html/customer.bin`

**Compile dari Command-Line**

```
course $ ./build.sh 
.../api-simple/source/customer
mkdir: lib: File exists
Free Pascal Compiler version 3.0.4 [2017/11/26] for x86_64
Copyright (c) 1993-2017 by Florian Klaempfl and others
Target OS: Darwin for x86_64
Compiling customer.lpr
Assembling (pipe) lib/customer.s
Linking ../../public_html/customer.bin
27 lines compiled, 0.7 sec
.../api-simple/source/customer
Done....

course $ 
```

## How to Test


### Menambah Data

Proses penambahan data dilakukan melalui method POST.


## Referensi


- [FastPlaz - Pascal Web Framework](http://www.fastplaz.com)
- [Understanding REST](https://github.com/fastplaz/fastplaz/blob/development/REST.md)

