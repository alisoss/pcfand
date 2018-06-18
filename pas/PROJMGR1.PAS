function IsCurrChpt:boolean;
  begin IsCurrChpt:=CRdb^.FD=CFile end;

function ExtToTyp(Ext:ExtStr):char;
  begin if (Ext='') or SEquUpcase(Ext,'.HLP')  {$ifdef FandSQL}
           or SEquUpcase(Ext,'.SQL')           {$endif}
                                then ExtToTyp:='6' else
        if SEquUpcase(Ext,'.X') then ExtToTyp:='X' else
        if SEquUpcase(Ext,'.DTA') then ExtToTyp:='8' else
        if SEquUpcase(Ext,'.DBF') then ExtToTyp:='D' else
        if SEquUpcase(Ext,'.RDB') then ExtToTyp:='0' else ExtToTyp:='?' end;

procedure ReleaseFDLDAfterChpt;
var FD:FileDPtr; R:RdbDPtr;
begin
  if Chpt^.Chain<>nil then CloseFAfter(Chpt^.Chain);
  Chpt^.Chain:=nil; LinkDRoot:=CRdb^.OldLDRoot; FuncDRoot:=CRdb^.OldFCRoot;
  CFile:=Chpt;CRecPtr:=E^.NewRecPtr; R:=CRdb^.ChainBack;
  if R<>nil then CRdb^.HelpFD:=R^.HelpFD else CRdb^.HelpFD:=nil;
  CompileFD:=true;
end;


{$ifndef FandRunV}

type RdbRecVars = record
  Typ:char; Name:string12; Ext:ExtStr; Txt,OldTxt:longint;
  FTyp:char; CatIRec:word; isSQL:boolean end;

function NetFileTest(const X:RdbRecVars):boolean;
begin with X do begin
  NetFileTest:=false; if (typ<>'F') or (CatIRec=0) or isSQL then exit;
  RdCatPathVol(CatIRec); if IsNetCVol then NetFileTest:=true;
end end;

procedure GetSplitChptName(var Name:string12;var Ext:ExtStr);
var i:word;
begin
  ext:=''; name:=TrailChar(' ',_shorts(ChptName)); i:=pos('.',name);
  if i=0 then exit; ext:=copy(name,i,255); name:=copy(name,1,i-1);
end;

procedure GetRdbRecVars(RecPtr:pointer;var X:RdbRecVars);
var s1:string[1]; p,p2,cr:pointer; ld:LinkDPtr;
begin with X do begin
  cr:=CRecPtr; CRecPtr:=RecPtr; s1:=_shorts(ChptTyp); typ:=s1[1];
  GetSplitChptName(name,ext); txt:=_t(ChptTxt); oldtxt:=_t(ChptOldTxt);
  if typ='F' then begin
    ftyp:=ExtToTyp(ext); catirec:=GetCatIRec(name,false); isSQL:=false;
    if oldtxt<>0 then begin
      ld:=LinkDRoot; MarkBoth(p,p2); if RdFDSegment(0,oldtxt) then begin
        ftyp:=CFile^.Typ; if CFile^.IsSQLFile then ext:='.SQL' else
        case ftyp of
          '0':ext:='.RDB';'D':ext:='.DBF'; '8':ext:='.DTA' else ext:='.000'

        end end;
      LinkDRoot:=ld; CFile:=Chpt; ReleaseBoth(p,p2) end;
    {$ifdef FAndSQL} if ext='.SQL' then isSQL:=true {$endif}  end;
  CRecPtr:=cr;
end end;
function ChptDelFor(const X:RdbRecVars):boolean;
begin with X do begin
  ChptDelFor:=true; SetUpdHandle(ChptTF^.Handle); ReleaseFDLDAfterChpt;
  case typ of
   ' ':exit;
   'D','P':SetCompileAll;
   'F':begin if oldtxt=0 then exit;  {don't delete if the record is new}
       SetCompileAll;
       if isSQL then exit; SetMsgPar(name);
       if not PromptYN(814) or NetFileTest(X) and not PromptYN(836) then begin
          ChptDelFor:=false; exit end;
       if catirec<>0 then begin
         WrCatField(catirec,CatFileName,''); if not PromptYN(815) then exit;
         RdCatPathVol(catirec); TestMountVol(CPath[1]) end
       else begin CDir:=''; CName:=X.name; CExt:=ext end;
       DeleteFile(CDir+CName+CExt); CExtToT; DeleteFile(CPath);
       if ftyp='X' then begin CExtToX; DeleteFile(CPath) end end;
   else ChptTF^.CompileProc:=true end;
end end;

function ChptDel:boolean;
var New:RdbRecVars;
begin
  if not IsCurrChpt then begin ChptDel:=true; exit end;
  GetRdbRecVars(E^.NewRecPtr,New); ChptDel:=ChptDelFor(New);
end;

function ChptWriteCRec:word; { 0-O.K., 1-fail, 2-fail and undo}
  var New,Old:RdbRecVars;
procedure RenameWithOldExt;
begin
  CExt:=Old.ext; RenameFile56(Old.name+CExt,New.name+CExt,false);
  CExtToT; RenameFile56(Old.name+CExt,New.name+CExt,false); CExtToX;
  if Old.ftyp='X' then RenameFile56(Old.name+CExt,New.name+CExt,false);
end;
function IsDuplFileName(name:string12):boolean;
var I:word; n:string12; e:ExtStr; cr:pointer;
label 1;
begin
  IsDuplFileName:=true; if SEquUpcase(name,Chpt^.Name) then exit;
  cr:=CRecPtr; CRecPtr:=GetRecSpace;
  for I:=1 to Chpt^.NRecs do if I<>CRec then begin
    ReadRec(I); if _shorts(ChptTyp)='F' then begin
      GetSplitChptName(n,e); if SEquUpcase(name,n) then goto 1 end end;
  IsDuplFileName:=false;
1:ReleaseStore(CRecPtr);CRecPtr:=cr;
end;
var FD1,FD2:FileDPtr; p,p2:pointer; s:LongStrPtr; pos:longint; b:boolean;
    eq:integer;
label 0,1,11,2;
begin
  ChptWriteCRec:=0; if not IsCurrChpt then exit;
  if not TestIsNewRec then begin
    eq:=CompArea(Pchar(CRecPtr)+2,Pchar(E^.OldRecPtr)+2,CFile^.RecLen-2);
    if eq=ord(_equ) then exit end;
  GetRdbRecVars(E^.NewRecPtr,New);
  if not TestIsNewRec then GetRdbRecVars(E^.OldRecPtr,Old);
  ChptWriteCRec:=1; {$ifndef FandGraph}
  if New.typ='L' then begin WrLLF10Msg(659);exit end;{$endif}
  if New.typ in ['D','U'] then begin
    if New.name<>'' then begin WrLLF10Msg(623);exit end end
  else if New.typ<>' ' then
    if not IsIdentifStr(New.name) or (New.typ<>'F')and(New.ext<>'') then begin
      WrLLF10Msg(138);exit end;
  if New.typ='F' then begin
       if length(New.name)>8 then begin WrLLF10Msg(1002);exit end;
       if New.ftyp='?' then begin WrLLF10Msg(1067);exit end;
       if IsDuplFileName(New.name) then begin WrLLf10Msg(1068);exit end;
       if (New.ftyp='0') and (New.txt<>0) then begin WrLLF10Msg(1083);exit end;
       if NetFileTest(New) and not TestIsNewRec and
          (Old.typ='F') and (eq<>ord(_equ)) and not PromptYN(824) then begin
            ChptWriteCRec:=2; exit end;
       end;
  if (New.typ in ['D','I','U']) or not TestIsNewRec and (Old.typ in ['D','I','U']) then begin
    ReleaseFDLDAfterChpt; SetCompileAll end;
  if TestIsNewRec then begin ReleaseFDLDAfterChpt; goto 2 end;
  if New.typ<>Old.typ then begin
1:  if not ChptDelFor(Old) then exit; t_(ChptOldTxt,0);
    if New.typ='F' then ReleaseFDLDAfterChpt; goto 2 end;
  if New.typ in [' ','I'] then goto 2;
  if New.typ<>'F' then begin
    if New.name<>Old.name then
      if New.typ in ['E','P'] then begin ReleaseFDLDAfterChpt; SetCompileAll end
      else ChptTF^.CompileProc:=true;
    if (New.typ='R') and (New.txt=0) then ReleaseFDLDAfterChpt;
    goto 2 end;
  ReleaseFDLDAfterChpt; SetCompileAll;
  if (New.oldtxt<>0) and (New.name<>Old.name) then
    if Old.catirec<>0 then WrCatField(Old.catirec,CatFileName,New.name)
    else if not Old.isSQL then RenameWithOldExt;
2:b_(ChptVerif,true); ChptWriteCRec:=0;
  SetUpdHandle(ChptTF^.Handle);
end;

{$else}
function ChptDel:boolean;
    begin ChptDel:=true end;
function ChptWriteCRec:word;
    begin ChptWriteCRec:=0 end;

{$endif}

{==========================================================================}

var CFileF:FileDPtr;
var sz:longint; nTb:word; Tb:pointer;

procedure WrFDSegment(RecNr:longint);
function O(p:pointer):pointer; assembler;
asm xor dx,dx; mov ax,p[2].word; or ax,ax; jz @1;
    mov dx,CFile[2].word; sub ax,dx; mov cl,4; shl ax,cl; add ax,p.word;
@1: end;
function OCF(p:pointer):pointer; assembler;
asm xor dx,dx; mov ax,p[2].word; or ax,ax; jz @1;
    mov dx,CFileF[2].word; sub ax,dx; mov cl,4; shl ax,cl; add ax,p.word;
@1: end;
function OTb(Nm:string):pointer;
var s:StringPtr; sofs:word absolute s; i:word;
label 1;
begin
  s:=Tb; for i:=1 to nTb do begin
    if SEquUpcase(s^,Nm) then goto 1; inc(sofs,length(s^)+1) end;
  inc(nTb); inc(sz,length(Nm)+1); if sz>MaxLStrLen then RunError(664);
  s:=StoreStr(Nm);
1:OTb:=O(s);
end;
function OLinkD(Ld:LinkDPtr):pointer;
  begin OLinkD:=ptr(word(OTb(Ld^.FromFD^.Name)),word(OTb(Ld^.RoleName))) end;
procedure OFrml(var Z:FrmlPtr);
var cf,fd1:FileDPtr; fl:FrmlList; ld:LinkDPtr;
begin
  if Z<>nil then begin
    Z:=O(Z); with Z^ do case Op of
   _field:Field:=OCF(Field);
   _access:begin LD:=OCF(LD); cf:=CFileF; CFileF:=File2; File2:=OTb(CFileF^.Name);
                 OFrml(P1); CFileF:=cf end;
   _userfunc:begin FC:=OTb(FC^.Name); fl:=FrmlList(@FrmlL); while fl^.Chain<>nil do begin
          fl^.Chain:=O(fl^.Chain); fl:=fl^.Chain; OFrml(fl^.Frml) end end;
   _owned:begin ld:=ownLD; fd1:=ld^.FromFD; ownLD:=OLinkD(ld); cf:=CFileF; CFileF:=fd1;
          OFrml(ownBool); OFrml(ownSum); CFileF:=cf end;
   #$60..#$af:OFrml(P1);
   #$b0..#$ef:begin OFrml(P1); OFrml(P2) end;
   #$f0..#$ff:begin OFrml(P1); OFrml(P2); OFrml(P3) end;
end end end;
procedure OKF(kf:KeyFldDPtr);
begin
  while kf^.Chain<>nil do begin
    kf^.Chain:=O(kf^.Chain); kf:=kf^.Chain; kf^.FldD:=O(kf^.FldD) end;
end;
var cf:FileDPtr; s:StringList; f:FieldDPtr; k:KeyDPtr; ad:AddDPtr;
    c:ChkDPtr; ld:LinkDPtr; n,oldsz:word; fdsaved,p2:pointer; ss:LongStrPtr;
    id:ImplDPtr; li:LiRootsPtr;
label 1;
begin
  sz:=AbsAdr(HeapPtr)-AbsAdr(CFile); if sz>MaxLStrLen then RunError(664);
  oldsz:=sz; nTb:=0; Tb:=O(HeapPtr);
  MarkStore2(p2); fdsaved:=GetStore2(sz); Move(CFile^,fdsaved^,sz);
  CFileF:=CFile; with CFile^ do begin
    TF:=O(TF); XF:=O(XF); if OrigFD<>nil then OrigFD:=OTb(OrigFD^.Name);
    s:=StringList(@ViewNames); while s^.Chain<>nil do begin
      s^.Chain:=O(s^.Chain); s:=s^.Chain end;
    f:=FieldDPtr(@FldD); while f^.Chain<>nil do begin
      f^.Chain:=O(f^.Chain); f:=f^.Chain; if f^.Flg and f_Stored=0 then OFrml(f^.Frml) end;
    k:=KeyDPtr(@Keys); while k^.Chain<>nil do begin
      k^.Chain:=O(k^.Chain); k:=k^.Chain; k^.Alias:=O(k^.Alias);
      OKF(KeyFldDPtr(@k^.KFlds)) end;
    ad:=AddDPtr(@Add); while ad^.Chain<>nil do begin
      ad^.Chain:=O(ad^.Chain); ad:=ad^.Chain; ad^.LD:=O(ad^.LD); OFrml(ad^.Frml);
      if ad^.Assign then OFrml(ad^.Bool) else begin c:=ad^.Chk;
        if c<>nil then begin ad^.Chk:=O(c); c^.HelpName:=O(c^.HelpName) end end;
      cf:=CFileF; CFileF:=ad^.File2;
      ad^.File2:=OTb(CFileF^.Name); ad^.Field:=OCF(ad^.Field);
      if not ad^.Assign and (c<>nil) then begin OFrml(c^.Bool); OFrml(c^.TxtZ) end;
      CFileF:=cf end;
    ld:=O(LinkDRoot); n:=nLDs; while n>0 do begin
      OKF(KeyFldDPtr(@ld^.Args)); cf:=CFileF; CFileF:=ld^.ToFD;
      ld^.ToFD:=OTb(CFileF^.Name); ld^.ToKey:=OCF(ld^.ToKey); CFileF:=cf;
      dec(n); if n>0 then begin ld^.Chain:=O(ld^.Chain); ld:=ld^.Chain end end;
    Chain:=O(LinkDRoot); IRec:=FDVersion; Handle:=ofs(Tb^);
    end;
  if CFile^.LiOfs>0 then begin
    li:=Normalize(AbsAdr(CFile)+CFile^.LiOfs);
    id:=ImplDPtr(@li^.Impls); while id^.Chain<>nil do begin
      id^.Chain:=O(id^.Chain); id:=id^.Chain; id^.FldD:=O(id^.FldD);
      OFrml(id^.Frml) end;
    c:=ChkDPtr(@li^.Chks); while c^.Chain<>nil do begin
      c^.Chain:=O(c^.Chain); c:=c^.Chain;
      c^.HelpName:=O(c^.HelpName); OFrml(c^.TxtZ); OFrml(c^.Bool) end end;
  ss:=Ptr(PtrRec(CFile).Seg-1,14); ss^.LL:=sz; cf:=CFile; CFile:=Chpt;
  StoreChptTxt(ChptOldTxt,ss,false); WriteRec(RecNr);
  CFile:=cf; Move(fdsaved^,CFile^,oldsz);
  ReleaseStore2(p2);
end;


function RdFDSegment(FromI:word;Pos:longint):boolean;
type Pr=PtrRec;
var Sg,SgF:word;{CFile-Seg}
function GetFD(p:pointer;WithSelf:boolean):FileDPtr;
begin
  if p<>nil then begin
    LexWord:=StringPtr(Ptr(Sg,Pr(p).Ofs))^;
    if WithSelf and EquUpcase(CFile^.Name) then p:=CFile
    else p:=FindFileD end;
  GetFD:=p;
end;
function GetFC(p:pointer):FuncDPtr;
var fc:FuncDPtr;
label 1;
begin
  LexWord:=StringPtr(Ptr(Sg,Pr(p).Ofs))^;
  fc:=FuncDRoot; while fc<>nil do begin
    if EquUpcase(fc^.Name) then goto 1; fc:=fc^.Chain end;
1:GetFC:=fc;
end;
function GetLinkD(P:pointer):LinkDPtr;
var ld:LinkDPtr; fd:FileDPtr;
begin
  LexWord:=StringPtr(Ptr(Sg,Pr(P).Seg))^; fd:=FindFileD; ld:=LinkDRoot;
  while ld<>nil do begin
    if (ld^.FromFD=fd) and (ld^.RoleName=StringPtr(Ptr(Sg,word(P)))^) then
    begin GetLinkD:=ld; exit end; ld:=ld^.Chain end;
  GetLinkD:=nil;
end;
procedure SgFrml(var Z:FrmlPtr);
var fl:FrmlList; SgFold:word;
begin
  if Z<>nil then begin Pr(Z).Seg:=Sg; with Z^ do case Op of
   _field:Pr(Field).Seg:=SgF;
   _access:begin if LD<>nil then Pr(LD).Seg:=SgF; File2:=GetFD(File2,LD<>nil);
           SgFold:=SgF; SgF:=Pr(File2).Seg; SgFrml(P1); SgF:=SgFold end;
   _userfunc:begin FC:=GetFC(FC); fl:=FrmlList(@FrmlL); while fl^.Chain<>nil do begin
          Pr(fl^.Chain).Seg:=Sg; fl:=fl^.Chain; SgFrml(fl^.Frml) end end;
   _owned:begin ownLD:=GetLinkD(ownLD); SgFold:=SgF; SgF:=Pr(ownLD^.FromFD).Seg;
          SgFrml(ownBool); SgFrml(ownSum); SgF:=SgFold end;
   #$60..#$af:SgFrml(P1);
   #$b0..#$ef:begin SgFrml(P1); SgFrml(P2) end;
   #$f0..#$ff:begin SgFrml(P1); SgFrml(P2); SgFrml(P3) end;
end end end;
procedure SgKF(kf:KeyFldDPtr);
begin
  while kf^.Chain<>nil do begin
    Pr(kf^.Chain).Seg:=Sg; kf:=kf^.Chain; Pr(kf^.FldD).Seg:=Sg end;
end;
var s:StringList; f:FieldDPtr; k:KeyDPtr; ad:AddDPtr; c:ChkDPtr;
    ld,ld1:LinkDPtr; n:integer; ss:LongStrPtr; lw:string; p:pointer;
begin     { CFile= Rdb; CRecPtr= 'F' chapter record }
  RdFDSegment:=false; lw:=LexWord;
  AlignLongStr; ss:=CFile^.TF^.Read(1,Pos);
  if (ss^.LL<=sizeof(FileD)) then exit;
  if CRdb^.Encrypted then CodingLongStr(ss);
  Sg:=Pr(ss).Seg+1; SgF:=Sg; CFile:=Ptr(Sg,0);
  with CFile^ do begin
    if IRec<>FDVersion then exit;  RdFDSegment:=true;
    Tb:=Ptr(Sg,Handle); Handle:=$ff;
    if TF<>nil then Pr(TF).Seg:=Sg; if XF<>nil then Pr(XF).Seg:=Sg;
    f:=FieldDPtr(@FldD); while f^.Chain<>nil do begin
      Pr(f^.Chain).Seg:=Sg; f:=f^.Chain;
      if f^.Flg and f_Stored=0 then SgFrml(f^.Frml) end;
    OrigFD:=GetFD(OrigFD,false);
    s:=StringList(@ViewNames); while s^.Chain<>nil do begin
      Pr(s^.Chain).Seg:=Sg; s:=s^.Chain end;
    k:=KeyDPtr(@Keys); while k^.Chain<>nil do begin
      Pr(k^.Chain).Seg:=Sg; k:=k^.Chain; Pr(k^.Alias).Seg:=Sg;
      SgKF(KeyFldDPtr(@k^.KFlds)) end;
    ad:=AddDPtr(@Add); while ad^.Chain<>nil do begin
      Pr(ad^.Chain).Seg:=Sg; ad:=ad^.Chain;
      if ad^.LD<>nil then Pr(ad^.LD).Seg:=Sg; SgFrml(ad^.Frml);
      if ad^.Assign then SgFrml(ad^.Bool) else if ad^.Chk<>nil then begin
          Pr(ad^.Chk).Seg:=Sg; with ad^.Chk^ do
          if HelpName<>nil then Pr(HelpName).Seg:=Sg end;
      ad^.File2:=GetFD(ad^.File2,true); SgF:=Pr(ad^.File2).Seg; Pr(ad^.Field).Seg:=SgF;
      if not ad^.Assign then begin
        c:=ad^.Chk; if c<>nil then begin SgFrml(c^.Bool); SgFrml(c^.TxtZ) end end;
      SgF:=Sg end;
    ld1:=LinkDRoot; ld:=LinkDPtr(CFile^.Chain); Pr(ld).Seg:=Sg;
    n:=nLDs; if n>0 then LinkDRoot:=ld; while n>0 do begin
      if n=1 then ld^.Chain:=ld1 else Pr(ld^.Chain).Seg:=Sg;
      SgKF(KeyFldDPtr(@ld^.Args));
      ld^.FromFD:=CFile; ld^.ToFD:=GetFD(ld^.ToFD,true);
      Pr(ld^.ToKey).Seg:=Pr(ld^.ToFD).Seg; ld:=ld^.Chain; dec(n) end;
    CatIRec:=GetCatIRec(Name,Typ='0'{multilevel});
    ChptPos.R:=CRdb; ChptPos.IRec:=FromI;
    {$ifdef FandSQL}SetIsSQLFile;{$endif} CompileRecLen;
    end;
  p:=Tb; if CFile^.LiOfs>0 then begin
    p:=Normalize(AbsAdr(CFile)+CFile^.LiOfs); CFile^.LiOfs:=0 end;
  ReleaseStore(p); LexWord:=lw;
end;

function FindHelpRecNr(FD:FileDPtr; txt:string):word;
var cf:FileDPtr; cr:pointer; md:LockMode; NmF,TxtF:FieldDPtr; i:word;
    nm:string[80];
label 1,2;
begin
  ConvToNoDiakr(txt[1],length(txt),Fonts.VFont); cf:=CFile; cr:=CRecPtr;
  CFile:=FD; CRecPtr:=GetRecSpace;
  md:=NewLMode(RdMode); if CFile^.Handle=$ff then goto 1;
  NmF:=CFile^.FldD; TxtF:=NmF^.Chain;
  for i:=1 to CFile^.NRecs do begin
    ReadRec(i); nm:=TrailChar(' ',_shorts(NmF));
    ConvToNoDiakr(nm[1],length(Nm),Fonts.VFont);
    if EqualsMask(@txt[1],length(txt),nm) then begin
      while (i<CFile^.NRecs) and (_t(TxtF)=0) do begin inc(i); ReadRec(i) end;
      FindHelpRecNr:=i; goto 2 end end;
1:FindHelpRecNr:=0;
2:OldLMode(md); ReleaseStore(CRecPtr); CFile:=cf; CRecPtr:=cr;
end;

function PromptHelpName(var N:word):boolean;
var txt:string;
begin
  PromptHelpName:=false; txt:='';
  PromptLL(153,txt,1,true); if (length(txt)=0) or (KbdChar=_ESC_) then exit;
  N:=FindHelpRecNr(CFile,txt); if N<>0 then PromptHelpName:=true;
end;

procedure EditHelpOrCat(cc,kind:word;txt:string);
var FD:FileDPtr; EO:EditOptPtr; i,n:word;
const nCat:word=1; iCat:word=1; nHelp:word=1; iHelp:word=1;
      nFrml:record Op:char; R:float end=(Op:_const;R:0.0);
      iFrml:record Op:char; R:float end=(Op:_const;R:0.0);
begin
  if cc=_AltF2_ then begin
    FD:=CRdb^.HelpFD; if kind=1 then FD:=CFile^.ChptPos.R^.HelpFD;
    if FD=nil then exit;
    if kind=0 then begin i:=iHelp; n:=nHelp end else begin
      i:=3; n:=FindHelpRecNr(FD,txt);
      if n=0 then KbdBuffer:=#0#60+txt end end
  else begin FD:=CatFD; i:=iCat; n:=nCat end;
  if kind<>2 then WrEStatus;
  EO:=GetEditOpt; EO^.Flds:=AllFldsList(FD,false);
  EO^.WFlags:=EO^.WFlags or WPushPixel;
  if (kind=0) or (n<>0) then begin
    iFrml.R:=i; nFrml.R:=n;
    EO^.StartRecNoZ:=FrmlPtr(@nFrml); EO^.StartIRecZ:=FrmlPtr(@iFrml) end;
  EditDataFile(FD,EO); ReleaseStore(EO);
  if cc=_AltF2_ then begin nHelp:=EdRecNo; iHelp:=EdIRec end
  else begin ResetCatalog; nCat:=EdRecNo; iCat:=EdIRec end;
  if kind<>2 then RdEStatus;
end;

procedure StoreChptTxt(F:FieldDPtr;S:LongStrPtr;Del:boolean);
var s2:LongStrPtr; p:pointer; LicNr:word; oldpos,pos:longint;
begin
  licNr:=ChptTF^.LicenseNr; oldpos:=_t(F); MarkStore(p);
  if CRdb^.Encrypted then if LicNr<>0 then begin
      s2:=GetStore($8100{possibly longer}); XEncode(s,s2); s:=s2 end
  else CodingLongStr(S);
  if Del then if licNr=0 then ChptTF^.Delete(OldPos)
    else if OldPos<>0 then ChptTF^.Delete(OldPos-licNr);
  pos:=ChptTF^.Store(S);
  if licNr=0 then t_(F,pos) else t_(F,pos+licNr);
  ReleaseStore(p);
end;