{$ifdef FPC}
{$mode tp}
{$J+}
{$H-}
{$endif}
{ $define FandRunV}
{$define FandNetV}
{$ifndef FPC}{$define FandGraph}{$endif}
{$define FandDML}
{ $define FandSQL}              { with FandNetV }
{ $define FandDemo}
{ $define FandTest}
{ $define FandAng}
{ $define Trial}                 { trial version }
{$define FandProlog}
{$ifdef FandProlog}{$define FandLProc}{$endif}

{$ifdef FandGraph}{$define FandGraphParse}{$endif}
{$ifdef FPC}{$define FandGraphParse}{$endif}
{$ifndef FPC}
{$N-}                       {real}
{$endif}
{ $N+}                       {emul}     {coproc}
{ $E+}                       {emul}
{ $E-}                                  {coproc}
{ $define Coproc}            {emul}     {coproc}