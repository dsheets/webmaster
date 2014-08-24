open Assemblage

let version = "0.1.0"

let netstring = pkg "netstring"
let xmlm = pkg "xmlm"
let uri = pkg "uri"
let cmdliner = pkg "cmdliner"
let unix = pkg "unix"
let bytes = pkg "bytes"

let library = `Path ["lib"]
let cli = `Path ["cli"]

let htmlm  = unit "htmlm"  ~deps:[netstring; xmlm] library
let urlfun = unit "urlfun" ~deps:[netstring; xmlm; uri] library

let webmaster_lib = lib "webmaster" (`Units [htmlm; urlfun])

let webmaster_cli = unit ~deps:[uri; netstring; cmdliner] "webmaster_cli" cli

let webmaster_file = unit ~deps:[bytes] "webmaster_file" cli
let webmaster_resolve    = unit ~deps:[uri; netstring] "webmaster_resolve" cli
let webmaster_relativize = unit "webmaster_relativize" cli

let webmaster_cli_lib =
  lib "cli" ~deps:[webmaster_lib]
    (`Units [
      webmaster_cli;
      webmaster_file;
      webmaster_resolve;
      webmaster_relativize;
     ])

let webmaster = unit "webmaster" cli

let webmaster_bin =
  bin "webmaster" ~deps:[webmaster_cli_lib] (`Units [webmaster])

;;

assemble (project ~version "webmaster" [
  webmaster_lib;
  webmaster_cli_lib;
  webmaster_bin;
])
