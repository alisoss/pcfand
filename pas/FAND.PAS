{$M 24000,16384,655360 }
{$I switches}
{$A-,B-,F-,G-,I+,O-,R-,S+,V+,X+}

uses dos,
{$ifdef FandGraph}
     graph,
{$endif}
     drivers,base,obase,obaseww,
     access,oaccess,runfrml,olongstr,
{$ifdef FandSQL} channel, {$endif}
{$ifdef FandDML}     dml, {$endif}
     rdrun, wwmenu,wwmix,
     compile, rdfildcl,rdmerg,rdrprt,rdproc, rdedit,
     sort,runmerg,runrprt,printtxt,
     genrprt,
     editor,expimp,runproc,
     runedi,runproj,runfand
{$ifdef FandGraph}
     ,rungraph,GrGlob,GrInit,GrMenu,GrMenu1,Gr2D,Gr3DQ,Gr3DD,GrPoly,GrTransF
     ,rdprolg,runprolg
{$endif}
     ;


{$O obase}
{$O obaseww}
{$O oaccess}
{$O olongstr}
{$O wwmenu}
{$O wwmix}
{$O compile}
{$O rdfildcl}
{$O rdmerg}
{$O rdrprt}
{$O rdproc}
{$O rdedit}
{$O sort}
{$O runmerg}
{$O runrprt}
{$O genrprt}
{$O printtxt}
{$O genrprt}
{$O editor}
{$O expimp}
{$O runproc}
{$O runedi}
{$O runproj}
{$O runfand}
{$ifdef FandGraph}
 {$O rdprolg}
 {$O runprolg}
{$endif}
{$ifdef FandGraph}
{$O rungraph}
{$O GrGlob}
{$O GrInit}
{$O GrMenu}
{$O GrMenu1}
{$O Gr2D}
{$O Gr3DQ}
{$O Gr3DD}
{$O GrPoly}
{$O GrTransF}
{$endif}

begin
  asm mov BPBound,bp end;
  InitRunFand;
end.