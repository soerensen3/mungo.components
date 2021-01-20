unit mungo.components.colors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  p3d.math;

type
  TColorRGBA = TVec4;
  TColorRGB = TVec3;

const
  Black: TColorRGBA =                        ( FCoord: ( $00 / 255, $00 / 255, $00 / 255, 1.0 ));
  White: TColorRGBA =                        ( FCoord: ( $ff / 255, $ff / 255, $ff / 255, 1.0 ));

  //Source: 2016 https://material.google.com/style/color.html#color-color-palette
  Red50: TColorRGBA =                        ( FCoord: ( $ff / 255, $eb / 255, $ee / 255, 1.0 ));
  Red100: TColorRGBA =                       ( FCoord: ( $ff / 255, $cd / 255, $d2 / 255, 1.0 ));
  Red200: TColorRGBA =                       ( FCoord: ( $ef / 255, $9a / 255, $9a / 255, 1.0 ));
  Red300: TColorRGBA =                       ( FCoord: ( $e5 / 255, $73 / 255, $73 / 255, 1.0 ));
  Red400: TColorRGBA =                       ( FCoord: ( $ef / 255, $53 / 255, $50 / 255, 1.0 ));
  Red500: TColorRGBA =                       ( FCoord: ( $f4 / 255, $43 / 255, $36 / 255, 1.0 ));
  Red600: TColorRGBA =                       ( FCoord: ( $e5 / 255, $39 / 255, $35 / 255, 1.0 ));
  Red700: TColorRGBA =                       ( FCoord: ( $d3 / 255, $2f / 255, $2f / 255, 1.0 ));
  Red800: TColorRGBA =                       ( FCoord: ( $c6 / 255, $28 / 255, $28 / 255, 1.0 ));
  Red900: TColorRGBA =                       ( FCoord: ( $b7 / 255, $1c / 255, $1c / 255, 1.0 ));
  RedA100: TColorRGBA =                      ( FCoord: ( $ff / 255, $8a / 255, $80 / 255, 1.0 ));
  RedA200: TColorRGBA =                      ( FCoord: ( $ff / 255, $52 / 255, $52 / 255, 1.0 ));
  RedA400: TColorRGBA =                      ( FCoord: ( $ff / 255, $17 / 255, $44 / 255, 1.0 ));
  RedA700: TColorRGBA =                      ( FCoord: ( $d5 / 255, $00 / 255, $00 / 255, 1.0 ));

  Pink50: TColorRGBA =                       ( FCoord: ( $fc / 255, $e4 / 255, $ec / 255, 1.0 ));
  Pink100: TColorRGBA =                      ( FCoord: ( $f8 / 255, $bb / 255, $d0 / 255, 1.0 ));
  Pink200: TColorRGBA =                      ( FCoord: ( $f4 / 255, $8f / 255, $b1 / 255, 1.0 ));
  Pink300: TColorRGBA =                      ( FCoord: ( $f0 / 255, $62 / 255, $92 / 255, 1.0 ));
  Pink400: TColorRGBA =                      ( FCoord: ( $ec / 255, $40 / 255, $7a / 255, 1.0 ));
  Pink500: TColorRGBA =                      ( FCoord: ( $e9 / 255, $1e / 255, $63 / 255, 1.0 ));
  Pink600: TColorRGBA =                      ( FCoord: ( $d8 / 255, $1b / 255, $60 / 255, 1.0 ));
  Pink700: TColorRGBA =                      ( FCoord: ( $c2 / 255, $18 / 255, $5b / 255, 1.0 ));
  Pink800: TColorRGBA =                      ( FCoord: ( $ad / 255, $14 / 255, $57 / 255, 1.0 ));
  Pink900: TColorRGBA =                      ( FCoord: ( $88 / 255, $0e / 255, $4f / 255, 1.0 ));
  PinkA100: TColorRGBA =                     ( FCoord: ( $ff / 255, $80 / 255, $ab / 255, 1.0 ));
  PinkA200: TColorRGBA =                     ( FCoord: ( $ff / 255, $40 / 255, $81 / 255, 1.0 ));
  PinkA400: TColorRGBA =                     ( FCoord: ( $f5 / 255, $00 / 255, $57 / 255, 1.0 ));
  PinkA700: TColorRGBA =                     ( FCoord: ( $c5 / 255, $11 / 255, $62 / 255, 1.0 ));

  Purple50: TColorRGBA =                     ( FCoord: ( $f3 / 255, $e5 / 255, $f5 / 255, 1.0 ));
  Purple100: TColorRGBA =                    ( FCoord: ( $e1 / 255, $be / 255, $e7 / 255, 1.0 ));
  Purple200: TColorRGBA =                    ( FCoord: ( $ce / 255, $93 / 255, $d8 / 255, 1.0 ));
  Purple300: TColorRGBA =                    ( FCoord: ( $ba / 255, $68 / 255, $c8 / 255, 1.0 ));
  Purple400: TColorRGBA =                    ( FCoord: ( $ab / 255, $47 / 255, $bc / 255, 1.0 ));
  Purple500: TColorRGBA =                    ( FCoord: ( $9c / 255, $27 / 255, $b0 / 255, 1.0 ));
  Purple600: TColorRGBA =                    ( FCoord: ( $8e / 255, $24 / 255, $aa / 255, 1.0 ));
  Purple700: TColorRGBA =                    ( FCoord: ( $7b / 255, $1f / 255, $a2 / 255, 1.0 ));
  Purple800: TColorRGBA =                    ( FCoord: ( $6a / 255, $1b / 255, $9a / 255, 1.0 ));
  Purple900: TColorRGBA =                    ( FCoord: ( $4a / 255, $14 / 255, $8c / 255, 1.0 ));
  PurpleA100: TColorRGBA =                   ( FCoord: ( $ea / 255, $80 / 255, $fc / 255, 1.0 ));
  PurpleA200: TColorRGBA =                   ( FCoord: ( $e0 / 255, $40 / 255, $fb / 255, 1.0 ));
  PurpleA400: TColorRGBA =                   ( FCoord: ( $d5 / 255, $00 / 255, $f9 / 255, 1.0 ));
  PurpleA700: TColorRGBA =                   ( FCoord: ( $aa / 255, $00 / 255, $ff / 255, 1.0 ));

  DeepPurple50: TColorRGBA =                 ( FCoord: ( $ed / 255, $e7 / 255, $f6 / 255, 1.0 ));
  DeepPurple100: TColorRGBA =                ( FCoord: ( $d1 / 255, $c4 / 255, $e9 / 255, 1.0 ));
  DeepPurple200: TColorRGBA =                ( FCoord: ( $b3 / 255, $9d / 255, $db / 255, 1.0 ));
  DeepPurple300: TColorRGBA =                ( FCoord: ( $95 / 255, $75 / 255, $cd / 255, 1.0 ));
  DeepPurple400: TColorRGBA =                ( FCoord: ( $7e / 255, $57 / 255, $c2 / 255, 1.0 ));
  DeepPurple500: TColorRGBA =                ( FCoord: ( $67 / 255, $3a / 255, $b7 / 255, 1.0 ));
  DeepPurple600: TColorRGBA =                ( FCoord: ( $5e / 255, $35 / 255, $b1 / 255, 1.0 ));
  DeepPurple700: TColorRGBA =                ( FCoord: ( $51 / 255, $2d / 255, $a8 / 255, 1.0 ));
  DeepPurple800: TColorRGBA =                ( FCoord: ( $45 / 255, $27 / 255, $a0 / 255, 1.0 ));
  DeepPurple900: TColorRGBA =                ( FCoord: ( $31 / 255, $1b / 255, $92 / 255, 1.0 ));
  DeepPurpleA100: TColorRGBA =               ( FCoord: ( $b3 / 255, $88 / 255, $ff / 255, 1.0 ));
  DeepPurpleA200: TColorRGBA =               ( FCoord: ( $7c / 255, $4d / 255, $ff / 255, 1.0 ));
  DeepPurpleA400: TColorRGBA =               ( FCoord: ( $65 / 255, $1f / 255, $ff / 255, 1.0 ));
  DeepPurpleA700: TColorRGBA =               ( FCoord: ( $62 / 255, $00 / 255, $ea / 255, 1.0 ));

  Indigo50: TColorRGBA =                     ( FCoord: ( $e8 / 255, $ea / 255, $f6 / 255, 1.0 ));
  Indigo100: TColorRGBA =                    ( FCoord: ( $c5 / 255, $ca / 255, $e9 / 255, 1.0 ));
  Indigo200: TColorRGBA =                    ( FCoord: ( $9f / 255, $a8 / 255, $da / 255, 1.0 ));
  Indigo300: TColorRGBA =                    ( FCoord: ( $79 / 255, $86 / 255, $cb / 255, 1.0 ));
  Indigo400: TColorRGBA =                    ( FCoord: ( $5c / 255, $6b / 255, $c0 / 255, 1.0 ));
  Indigo500: TColorRGBA =                    ( FCoord: ( $3f / 255, $51 / 255, $b5 / 255, 1.0 ));
  Indigo600: TColorRGBA =                    ( FCoord: ( $39 / 255, $49 / 255, $ab / 255, 1.0 ));
  Indigo700: TColorRGBA =                    ( FCoord: ( $30 / 255, $3f / 255, $9f / 255, 1.0 ));
  Indigo800: TColorRGBA =                    ( FCoord: ( $28 / 255, $35 / 255, $93 / 255, 1.0 ));
  Indigo900: TColorRGBA =                    ( FCoord: ( $1a / 255, $23 / 255, $7e / 255, 1.0 ));
  IndigoA100: TColorRGBA =                   ( FCoord: ( $8c / 255, $9e / 255, $ff / 255, 1.0 ));
  IndigoA200: TColorRGBA =                   ( FCoord: ( $53 / 255, $6d / 255, $fe / 255, 1.0 ));
  IndigoA400: TColorRGBA =                   ( FCoord: ( $3d / 255, $5a / 255, $fe / 255, 1.0 ));
  IndigoA700: TColorRGBA =                   ( FCoord: ( $30 / 255, $4f / 255, $fe / 255, 1.0 ));

  Blue50: TColorRGBA =                       ( FCoord: ( $e3 / 255, $f2 / 255, $fd / 255, 1.0 ));
  Blue100: TColorRGBA =                      ( FCoord: ( $bb / 255, $de / 255, $fb / 255, 1.0 ));
  Blue200: TColorRGBA =                      ( FCoord: ( $90 / 255, $ca / 255, $f9 / 255, 1.0 ));
  Blue300: TColorRGBA =                      ( FCoord: ( $64 / 255, $b5 / 255, $f6 / 255, 1.0 ));
  Blue400: TColorRGBA =                      ( FCoord: ( $42 / 255, $a5 / 255, $f5 / 255, 1.0 ));
  Blue500: TColorRGBA =                      ( FCoord: ( $21 / 255, $96 / 255, $f3 / 255, 1.0 ));
  Blue600: TColorRGBA =                      ( FCoord: ( $1e / 255, $88 / 255, $e5 / 255, 1.0 ));
  Blue700: TColorRGBA =                      ( FCoord: ( $19 / 255, $76 / 255, $d2 / 255, 1.0 ));
  Blue800: TColorRGBA =                      ( FCoord: ( $15 / 255, $65 / 255, $c0 / 255, 1.0 ));
  Blue900: TColorRGBA =                      ( FCoord: ( $0d / 255, $47 / 255, $a1 / 255, 1.0 ));
  BlueA100: TColorRGBA =                     ( FCoord: ( $82 / 255, $b1 / 255, $ff / 255, 1.0 ));
  BlueA200: TColorRGBA =                     ( FCoord: ( $44 / 255, $8a / 255, $ff / 255, 1.0 ));
  BlueA400: TColorRGBA =                     ( FCoord: ( $29 / 255, $79 / 255, $ff / 255, 1.0 ));
  BlueA700: TColorRGBA =                     ( FCoord: ( $29 / 255, $62 / 255, $ff / 255, 1.0 ));

  LightBlue50: TColorRGBA =                  ( FCoord: ( $e1 / 255, $f5 / 255, $fe / 255, 1.0 ));
  LightBlue100: TColorRGBA =                 ( FCoord: ( $b3 / 255, $e5 / 255, $fc / 255, 1.0 ));
  LightBlue200: TColorRGBA =                 ( FCoord: ( $81 / 255, $d4 / 255, $fa / 255, 1.0 ));
  LightBlue300: TColorRGBA =                 ( FCoord: ( $4f / 255, $c3 / 255, $f7 / 255, 1.0 ));
  LightBlue400: TColorRGBA =                 ( FCoord: ( $29 / 255, $b6 / 255, $f6 / 255, 1.0 ));
  LightBlue500: TColorRGBA =                 ( FCoord: ( $03 / 255, $a9 / 255, $f4 / 255, 1.0 ));
  LightBlue600: TColorRGBA =                 ( FCoord: ( $03 / 255, $9b / 255, $e5 / 255, 1.0 ));
  LightBlue700: TColorRGBA =                 ( FCoord: ( $02 / 255, $88 / 255, $d1 / 255, 1.0 ));
  LightBlue800: TColorRGBA =                 ( FCoord: ( $02 / 255, $77 / 255, $bd / 255, 1.0 ));
  LightBlue900: TColorRGBA =                 ( FCoord: ( $01 / 255, $57 / 255, $9b / 255, 1.0 ));
  LightBlueA100: TColorRGBA =                ( FCoord: ( $80 / 255, $d8 / 255, $ff / 255, 1.0 ));
  LightBlueA200: TColorRGBA =                ( FCoord: ( $40 / 255, $c4 / 255, $ff / 255, 1.0 ));
  LightBlueA400: TColorRGBA =                ( FCoord: ( $00 / 255, $b0 / 255, $ff / 255, 1.0 ));
  LightBlueA700: TColorRGBA =                ( FCoord: ( $00 / 255, $91 / 255, $ea / 255, 1.0 ));

  Cyan50: TColorRGBA =                       ( FCoord: ( $e0 / 255, $f7 / 255, $fa / 255, 1.0 ));
  Cyan100: TColorRGBA =                      ( FCoord: ( $b2 / 255, $eb / 255, $f2 / 255, 1.0 ));
  Cyan200: TColorRGBA =                      ( FCoord: ( $80 / 255, $de / 255, $ea / 255, 1.0 ));
  Cyan300: TColorRGBA =                      ( FCoord: ( $4d / 255, $d0 / 255, $e1 / 255, 1.0 ));
  Cyan400: TColorRGBA =                      ( FCoord: ( $26 / 255, $c6 / 255, $da / 255, 1.0 ));
  Cyan500: TColorRGBA =                      ( FCoord: ( $00 / 255, $bc / 255, $d4 / 255, 1.0 ));
  Cyan600: TColorRGBA =                      ( FCoord: ( $00 / 255, $ac / 255, $c1 / 255, 1.0 ));
  Cyan700: TColorRGBA =                      ( FCoord: ( $00 / 255, $97 / 255, $a7 / 255, 1.0 ));
  Cyan800: TColorRGBA =                      ( FCoord: ( $00 / 255, $83 / 255, $8f / 255, 1.0 ));
  Cyan900: TColorRGBA =                      ( FCoord: ( $00 / 255, $60 / 255, $64 / 255, 1.0 ));
  CyanA100: TColorRGBA =                     ( FCoord: ( $84 / 255, $ff / 255, $ff / 255, 1.0 ));
  CyanA200: TColorRGBA =                     ( FCoord: ( $18 / 255, $ff / 255, $ff / 255, 1.0 ));
  CyanA400: TColorRGBA =                     ( FCoord: ( $00 / 255, $e5 / 255, $ff / 255, 1.0 ));
  CyanA700: TColorRGBA =                     ( FCoord: ( $00 / 255, $b8 / 255, $d4 / 255, 1.0 ));

  Teal50: TColorRGBA =                       ( FCoord: ( $e0 / 255, $f2 / 255, $f1 / 255, 1.0 ));
  Teal100: TColorRGBA =                      ( FCoord: ( $b2 / 255, $df / 255, $db / 255, 1.0 ));
  Teal200: TColorRGBA =                      ( FCoord: ( $80 / 255, $cb / 255, $c4 / 255, 1.0 ));
  Teal300: TColorRGBA =                      ( FCoord: ( $4d / 255, $b6 / 255, $ac / 255, 1.0 ));
  Teal400: TColorRGBA =                      ( FCoord: ( $26 / 255, $a6 / 255, $9a / 255, 1.0 ));
  Teal500: TColorRGBA =                      ( FCoord: ( $00 / 255, $96 / 255, $88 / 255, 1.0 ));
  Teal600: TColorRGBA =                      ( FCoord: ( $00 / 255, $89 / 255, $7b / 255, 1.0 ));
  Teal700: TColorRGBA =                      ( FCoord: ( $00 / 255, $79 / 255, $6b / 255, 1.0 ));
  Teal800: TColorRGBA =                      ( FCoord: ( $00 / 255, $69 / 255, $5c / 255, 1.0 ));
  Teal900: TColorRGBA =                      ( FCoord: ( $00 / 255, $4d / 255, $40 / 255, 1.0 ));
  TealA100: TColorRGBA =                     ( FCoord: ( $a7 / 255, $ff / 255, $eb / 255, 1.0 ));
  TealA200: TColorRGBA =                     ( FCoord: ( $64 / 255, $ff / 255, $da / 255, 1.0 ));
  TealA400: TColorRGBA =                     ( FCoord: ( $1d / 255, $e9 / 255, $b6 / 255, 1.0 ));
  TealA700: TColorRGBA =                     ( FCoord: ( $00 / 255, $bf / 255, $a5 / 255, 1.0 ));

  Green50: TColorRGBA =                      ( FCoord: ( $e8 / 255, $f5 / 255, $e9 / 255, 1.0 ));
  Green100: TColorRGBA =                     ( FCoord: ( $c8 / 255, $e6 / 255, $c9 / 255, 1.0 ));
  Green200: TColorRGBA =                     ( FCoord: ( $a5 / 255, $d6 / 255, $a7 / 255, 1.0 ));
  Green300: TColorRGBA =                     ( FCoord: ( $81 / 255, $c7 / 255, $84 / 255, 1.0 ));
  Green400: TColorRGBA =                     ( FCoord: ( $66 / 255, $bb / 255, $6a / 255, 1.0 ));
  Green500: TColorRGBA =                     ( FCoord: ( $4c / 255, $af / 255, $50 / 255, 1.0 ));
  Green600: TColorRGBA =                     ( FCoord: ( $43 / 255, $a0 / 255, $47 / 255, 1.0 ));
  Green700: TColorRGBA =                     ( FCoord: ( $38 / 255, $8e / 255, $3c / 255, 1.0 ));
  Green800: TColorRGBA =                     ( FCoord: ( $2e / 255, $7d / 255, $32 / 255, 1.0 ));
  Green900: TColorRGBA =                     ( FCoord: ( $1b / 255, $5e / 255, $20 / 255, 1.0 ));
  GreenA100: TColorRGBA =                    ( FCoord: ( $b9 / 255, $f6 / 255, $ca / 255, 1.0 ));
  GreenA200: TColorRGBA =                    ( FCoord: ( $69 / 255, $f0 / 255, $ae / 255, 1.0 ));
  GreenA400: TColorRGBA =                    ( FCoord: ( $00 / 255, $e6 / 255, $76 / 255, 1.0 ));
  GreenA700: TColorRGBA =                    ( FCoord: ( $00 / 255, $c8 / 255, $53 / 255, 1.0 ));

  LightGreen50: TColorRGBA =                 ( FCoord: ( $f1 / 255, $f8 / 255, $e9 / 255, 1.0 ));
  LightGreen100: TColorRGBA =                ( FCoord: ( $dc / 255, $ed / 255, $c8 / 255, 1.0 ));
  LightGreen200: TColorRGBA =                ( FCoord: ( $c5 / 255, $e1 / 255, $a5 / 255, 1.0 ));
  LightGreen300: TColorRGBA =                ( FCoord: ( $ae / 255, $d5 / 255, $81 / 255, 1.0 ));
  LightGreen400: TColorRGBA =                ( FCoord: ( $9c / 255, $cc / 255, $65 / 255, 1.0 ));
  LightGreen500: TColorRGBA =                ( FCoord: ( $8b / 255, $c3 / 255, $4a / 255, 1.0 ));
  LightGreen600: TColorRGBA =                ( FCoord: ( $7c / 255, $b3 / 255, $42 / 255, 1.0 ));
  LightGreen700: TColorRGBA =                ( FCoord: ( $68 / 255, $9f / 255, $38 / 255, 1.0 ));
  LightGreen800: TColorRGBA =                ( FCoord: ( $55 / 255, $8b / 255, $2f / 255, 1.0 ));
  LightGreen900: TColorRGBA =                ( FCoord: ( $33 / 255, $69 / 255, $1e / 255, 1.0 ));
  LightGreenA100: TColorRGBA =               ( FCoord: ( $cc / 255, $ff / 255, $90 / 255, 1.0 ));
  LightGreenA200: TColorRGBA =               ( FCoord: ( $b2 / 255, $ff / 255, $59 / 255, 1.0 ));
  LightGreenA400: TColorRGBA =               ( FCoord: ( $76 / 255, $ff / 255, $03 / 255, 1.0 ));
  LightGreenA700: TColorRGBA =               ( FCoord: ( $64 / 255, $dd / 255, $17 / 255, 1.0 ));

  Lime50: TColorRGBA =                       ( FCoord: ( $f9 / 255, $fb / 255, $e7 / 255, 1.0 ));
  Lime100: TColorRGBA =                      ( FCoord: ( $f0 / 255, $f4 / 255, $c3 / 255, 1.0 ));
  Lime200: TColorRGBA =                      ( FCoord: ( $e6 / 255, $ee / 255, $9c / 255, 1.0 ));
  Lime300: TColorRGBA =                      ( FCoord: ( $dc / 255, $e7 / 255, $75 / 255, 1.0 ));
  Lime400: TColorRGBA =                      ( FCoord: ( $d4 / 255, $e1 / 255, $57 / 255, 1.0 ));
  Lime500: TColorRGBA =                      ( FCoord: ( $cd / 255, $dc / 255, $39 / 255, 1.0 ));
  Lime600: TColorRGBA =                      ( FCoord: ( $c0 / 255, $ca / 255, $33 / 255, 1.0 ));
  Lime700: TColorRGBA =                      ( FCoord: ( $af / 255, $b4 / 255, $2b / 255, 1.0 ));
  Lime800: TColorRGBA =                      ( FCoord: ( $9e / 255, $9d / 255, $24 / 255, 1.0 ));
  Lime900: TColorRGBA =                      ( FCoord: ( $82 / 255, $77 / 255, $17 / 255, 1.0 ));
  LimeA100: TColorRGBA =                     ( FCoord: ( $f4 / 255, $ff / 255, $81 / 255, 1.0 ));
  LimeA200: TColorRGBA =                     ( FCoord: ( $ee / 255, $ff / 255, $41 / 255, 1.0 ));
  LimeA400: TColorRGBA =                     ( FCoord: ( $c6 / 255, $ff / 255, $00 / 255, 1.0 ));
  LimeA700: TColorRGBA =                     ( FCoord: ( $ae / 255, $ea / 255, $00 / 255, 1.0 ));

  Yellow50: TColorRGBA =                     ( FCoord: ( $ff / 255, $fd / 255, $e7 / 255, 1.0 ));
  Yellow100: TColorRGBA =                    ( FCoord: ( $ff / 255, $f9 / 255, $c4 / 255, 1.0 ));
  Yellow200: TColorRGBA =                    ( FCoord: ( $ff / 255, $f5 / 255, $9d / 255, 1.0 ));
  Yellow300: TColorRGBA =                    ( FCoord: ( $ff / 255, $f1 / 255, $76 / 255, 1.0 ));
  Yellow400: TColorRGBA =                    ( FCoord: ( $ff / 255, $ee / 255, $58 / 255, 1.0 ));
  Yellow500: TColorRGBA =                    ( FCoord: ( $ff / 255, $eb / 255, $3b / 255, 1.0 ));
  Yellow600: TColorRGBA =                    ( FCoord: ( $fd / 255, $d8 / 255, $35 / 255, 1.0 ));
  Yellow700: TColorRGBA =                    ( FCoord: ( $fb / 255, $c0 / 255, $2d / 255, 1.0 ));
  Yellow800: TColorRGBA =                    ( FCoord: ( $f9 / 255, $a8 / 255, $25 / 255, 1.0 ));
  Yellow900: TColorRGBA =                    ( FCoord: ( $f5 / 255, $7f / 255, $17 / 255, 1.0 ));
  YellowA100: TColorRGBA =                   ( FCoord: ( $ff / 255, $ff / 255, $8d / 255, 1.0 ));
  YellowA200: TColorRGBA =                   ( FCoord: ( $ff / 255, $ff / 255, $00 / 255, 1.0 ));
  YellowA400: TColorRGBA =                   ( FCoord: ( $ff / 255, $ea / 255, $00 / 255, 1.0 ));
  YellowA700: TColorRGBA =                   ( FCoord: ( $ff / 255, $d6 / 255, $00 / 255, 1.0 ));

  Amber50: TColorRGBA =                      ( FCoord: ( $ff / 255, $f8 / 255, $e1 / 255, 1.0 ));
  Amber100: TColorRGBA =                     ( FCoord: ( $ff / 255, $ec / 255, $b3 / 255, 1.0 ));
  Amber200: TColorRGBA =                     ( FCoord: ( $ff / 255, $e0 / 255, $82 / 255, 1.0 ));
  Amber300: TColorRGBA =                     ( FCoord: ( $ff / 255, $d5 / 255, $4f / 255, 1.0 ));
  Amber400: TColorRGBA =                     ( FCoord: ( $ff / 255, $ca / 255, $28 / 255, 1.0 ));
  Amber500: TColorRGBA =                     ( FCoord: ( $ff / 255, $c1 / 255, $07 / 255, 1.0 ));
  Amber600: TColorRGBA =                     ( FCoord: ( $ff / 255, $b3 / 255, $00 / 255, 1.0 ));
  Amber700: TColorRGBA =                     ( FCoord: ( $ff / 255, $a0 / 255, $00 / 255, 1.0 ));
  Amber800: TColorRGBA =                     ( FCoord: ( $ff / 255, $8f / 255, $00 / 255, 1.0 ));
  Amber900: TColorRGBA =                     ( FCoord: ( $ff / 255, $6f / 255, $00 / 255, 1.0 ));
  AmberA100: TColorRGBA =                    ( FCoord: ( $ff / 255, $e5 / 255, $7f / 255, 1.0 ));
  AmberA200: TColorRGBA =                    ( FCoord: ( $ff / 255, $d7 / 255, $40 / 255, 1.0 ));
  AmberA400: TColorRGBA =                    ( FCoord: ( $ff / 255, $c4 / 255, $00 / 255, 1.0 ));
  AmberA700: TColorRGBA =                    ( FCoord: ( $ff / 255, $ab / 255, $00 / 255, 1.0 ));

  Orange50: TColorRGBA =                     ( FCoord: ( $ff / 255, $f3 / 255, $e0 / 255, 1.0 ));
  Orange100: TColorRGBA =                    ( FCoord: ( $ff / 255, $e0 / 255, $b2 / 255, 1.0 ));
  Orange200: TColorRGBA =                    ( FCoord: ( $ff / 255, $cc / 255, $80 / 255, 1.0 ));
  Orange300: TColorRGBA =                    ( FCoord: ( $ff / 255, $b7 / 255, $4d / 255, 1.0 ));
  Orange400: TColorRGBA =                    ( FCoord: ( $ff / 255, $a7 / 255, $26 / 255, 1.0 ));
  Orange500: TColorRGBA =                    ( FCoord: ( $ff / 255, $98 / 255, $00 / 255, 1.0 ));
  Orange600: TColorRGBA =                    ( FCoord: ( $fb / 255, $8c / 255, $00 / 255, 1.0 ));
  Orange700: TColorRGBA =                    ( FCoord: ( $f5 / 255, $7c / 255, $00 / 255, 1.0 ));
  Orange800: TColorRGBA =                    ( FCoord: ( $ef / 255, $6c / 255, $00 / 255, 1.0 ));
  Orange900: TColorRGBA =                    ( FCoord: ( $e6 / 255, $51 / 255, $00 / 255, 1.0 ));
  OrangeA100: TColorRGBA =                   ( FCoord: ( $ff / 255, $d1 / 255, $80 / 255, 1.0 ));
  OrangeA200: TColorRGBA =                   ( FCoord: ( $ff / 255, $ab / 255, $40 / 255, 1.0 ));
  OrangeA400: TColorRGBA =                   ( FCoord: ( $ff / 255, $91 / 255, $00 / 255, 1.0 ));
  OrangeA700: TColorRGBA =                   ( FCoord: ( $ff / 255, $6d / 255, $00 / 255, 1.0 ));

  DeepOrange50: TColorRGBA =                 ( FCoord: ( $fb / 255, $e9 / 255, $e7 / 255, 1.0 ));
  DeepOrange100: TColorRGBA =                ( FCoord: ( $ff / 255, $cc / 255, $bc / 255, 1.0 ));
  DeepOrange200: TColorRGBA =                ( FCoord: ( $ff / 255, $ab / 255, $91 / 255, 1.0 ));
  DeepOrange300: TColorRGBA =                ( FCoord: ( $ff / 255, $8a / 255, $65 / 255, 1.0 ));
  DeepOrange400: TColorRGBA =                ( FCoord: ( $ff / 255, $70 / 255, $43 / 255, 1.0 ));
  DeepOrange500: TColorRGBA =                ( FCoord: ( $ff / 255, $57 / 255, $22 / 255, 1.0 ));
  DeepOrange600: TColorRGBA =                ( FCoord: ( $f4 / 255, $51 / 255, $1e / 255, 1.0 ));
  DeepOrange700: TColorRGBA =                ( FCoord: ( $e6 / 255, $4a / 255, $19 / 255, 1.0 ));
  DeepOrange800: TColorRGBA =                ( FCoord: ( $d8 / 255, $43 / 255, $15 / 255, 1.0 ));
  DeepOrange900: TColorRGBA =                ( FCoord: ( $bf / 255, $36 / 255, $0c / 255, 1.0 ));
  DeepOrangeA100: TColorRGBA =               ( FCoord: ( $ff / 255, $9e / 255, $80 / 255, 1.0 ));
  DeepOrangeA200: TColorRGBA =               ( FCoord: ( $ff / 255, $6e / 255, $40 / 255, 1.0 ));
  DeepOrangeA400: TColorRGBA =               ( FCoord: ( $ff / 255, $3d / 255, $00 / 255, 1.0 ));
  DeepOrangeA700: TColorRGBA =               ( FCoord: ( $dd / 255, $2c / 255, $00 / 255, 1.0 ));

  Brown50: TColorRGBA =                      ( FCoord: ( $ef / 255, $eb / 255, $e9 / 255, 1.0 ));
  Brown100: TColorRGBA =                     ( FCoord: ( $d7 / 255, $cc / 255, $c8 / 255, 1.0 ));
  Brown200: TColorRGBA =                     ( FCoord: ( $bc / 255, $aa / 255, $a4 / 255, 1.0 ));
  Brown300: TColorRGBA =                     ( FCoord: ( $a1 / 255, $88 / 255, $7f / 255, 1.0 ));
  Brown400: TColorRGBA =                     ( FCoord: ( $8d / 255, $6e / 255, $63 / 255, 1.0 ));
  Brown500: TColorRGBA =                     ( FCoord: ( $79 / 255, $55 / 255, $48 / 255, 1.0 ));
  Brown600: TColorRGBA =                     ( FCoord: ( $6d / 255, $4c / 255, $41 / 255, 1.0 ));
  Brown700: TColorRGBA =                     ( FCoord: ( $5d / 255, $40 / 255, $37 / 255, 1.0 ));
  Brown800: TColorRGBA =                     ( FCoord: ( $4e / 255, $34 / 255, $2e / 255, 1.0 ));
  Brown900: TColorRGBA =                     ( FCoord: ( $3e / 255, $27 / 255, $23 / 255, 1.0 ));

  Grey50: TColorRGBA =                       ( FCoord: ( $fa / 255, $fa / 255, $fa / 255, 1.0 ));
  Grey100: TColorRGBA =                      ( FCoord: ( $f5 / 255, $f5 / 255, $f5 / 255, 1.0 ));
  Grey200: TColorRGBA =                      ( FCoord: ( $ee / 255, $ee / 255, $ee / 255, 1.0 ));
  Grey300: TColorRGBA =                      ( FCoord: ( $e0 / 255, $e0 / 255, $e0 / 255, 1.0 ));
  Grey400: TColorRGBA =                      ( FCoord: ( $bd / 255, $bd / 255, $bd / 255, 1.0 ));
  Grey500: TColorRGBA =                      ( FCoord: ( $9e / 255, $9e / 255, $9e / 255, 1.0 ));
  Grey600: TColorRGBA =                      ( FCoord: ( $75 / 255, $75 / 255, $75 / 255, 1.0 ));
  Grey700: TColorRGBA =                      ( FCoord: ( $61 / 255, $61 / 255, $61 / 255, 1.0 ));
  Grey800: TColorRGBA =                      ( FCoord: ( $42 / 255, $42 / 255, $42 / 255, 1.0 ));
  Grey900: TColorRGBA =                      ( FCoord: ( $21 / 255, $21 / 255, $21 / 255, 1.0 ));

  BlueGrey50: TColorRGBA =                   ( FCoord: ( $ec / 255, $ef / 255, $f1 / 255, 1.0 ));
  BlueGrey100: TColorRGBA =                  ( FCoord: ( $cf / 255, $d8 / 255, $dc / 255, 1.0 ));
  BlueGrey200: TColorRGBA =                  ( FCoord: ( $b0 / 255, $be / 255, $c5 / 255, 1.0 ));
  BlueGrey300: TColorRGBA =                  ( FCoord: ( $90 / 255, $a4 / 255, $ae / 255, 1.0 ));
  BlueGrey400: TColorRGBA =                  ( FCoord: ( $78 / 255, $90 / 255, $9c / 255, 1.0 ));
  BlueGrey500: TColorRGBA =                  ( FCoord: ( $60 / 255, $7d / 255, $8b / 255, 1.0 ));
  BlueGrey600: TColorRGBA =                  ( FCoord: ( $54 / 255, $6e / 255, $7a / 255, 1.0 ));
  BlueGrey700: TColorRGBA =                  ( FCoord: ( $45 / 255, $5a / 255, $64 / 255, 1.0 ));
  BlueGrey800: TColorRGBA =                  ( FCoord: ( $37 / 255, $47 / 255, $4f / 255, 1.0 ));
  BlueGrey900: TColorRGBA =                  ( FCoord: ( $26 / 255, $32 / 255, $38 / 255, 1.0 ));


implementation

end.

