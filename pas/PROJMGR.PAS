const UserW:longint=0;

procedure SetChptFldDPtr;
  begin ChptTF:=Chpt^.TF;
        ChptTxtPos:=Chpt^.FldD;ChptVerif:=ChptTxtPos^.Chain;
        ChptOldTxt:=ChptVerif^.Chain;ChptTyp:=ChptOldTxt^.Chain;
        ChptName:=ChptTyp^.Chain;ChptTxt:=ChptName^.Chain end;

procedure CreateOpenChpt(const Nm:string;create:boolean);
function SetRdbDir(Typ:char;const Nm:string):string;
var r,rb:RdbDPtr; d:DirStr;
begin
  r:=CRdb; rb:=r^.ChainBack; if rb=nil then TopRdb:=r; CVol:='';
  if Typ='\' then begin
    rb:=TopRdb; CRdb:=rb; CFile^.CatIRec:=GetCatIRec(Nm,false);
    CRdb:=r end;
  if CFile^.CatIRec<>0 then begin
    CPath:=RdCatField(CFile^.CatIRec,CatPathName);
    if CPath[2]<>':' then begin
      d:=rb^.RdbDir; if CPath[1]='\' then CPath:=copy(d,1,2)+CPath else begin
        AddBackSlash(d); CPath:=d+CPath end end;
    FSplit(CPath,CDir,CName,CExt); DelBackSlash(CDir) end
  else if rb=nil then CDir:=TopRdbDir else begin
    CDir:=rb^.RdbDir; AddBackSlash(CDir); CDir:=CDir+CFile^.Name end;
  with r^ do begin
    RdbDir:=CDir; if TopDataDir='' then DataDir:=CDir else
    if rb=nil then DataDir:=TopDataDir else begin
      d:=rb^.DataDir; AddBackSlash(d); DataDir:=d+CFile^.Name end end;
  CDir:=CDir+'\';
end;
procedure ResetRdOnly;
begin
  if Chpt^.UMode=RdOnly then begin
    CloseFile; IsInstallRun:=true; OpenF(Exclusive); IsInstallRun:=false end;
end;
var R:RdbDPtr; top:boolean; p:PathStr; s:string; oldChptTF:TFilePtr;
    i,n:integer; nr:string[10]; Nm1:string[8]; um:FileUseMode;
label 1,2,3;
begin
  top:=CRdb=nil; FileDRoot:=nil; R:=GetZStore(sizeof(R^));
  oldChptTF:=ChptTF;
  R^.ChainBack:=CRdb; R^.OldLDRoot:=LinkDRoot; R^.OldFCRoot:=FuncDRoot;
  MarkStore2(R^.Mark2);
  RdMsg(51); s:=MsgLine; RdMsg(48); val(MsgLine,n,i);
  str(TxtCols-n,nr); s:=s+nr; SetInpStr(s);
  if (Nm[1]='\') then Nm1:=copy(Nm,2,8) else Nm1:=Nm;
  RdFileD(Nm1,'0','');{old CRdb for GetCatIRec}
  R^.FD:=CFile; CRdb:=R; CFile^.RecPtr:=GetRecSpace;
  SetRdbDir(Nm[1],Nm1); p:=CDir+Nm1+'.RDB';
  CFile^.Drive:=TestMountVol(CPath[1]);
  SetChptFldDPtr; if not spec.Rdbcomment then ChptTxt^.L:=1;
  SetMsgPar(p);
  if top then begin UserName:=''; UserCode:=0; AccRight:=#0; goto 2 end;
  CRdb^.HelpFD:=CRdb^.ChainBack^.HelpFD;
1:{$I-} ChDir(R^.RdbDir);{$I+}
  if IOResult<>0 then
    if create and (IsTestRun or not top) then begin
      {$I-} MkDir(R^.RdbDir);{$I+} if IOResult<>0 then RunError(620);
      goto 1 end
    else RunError(631);
2:if IsTestRun or not create then um:=Exclusive else um:=RdOnly;
  if OpenF(um) then begin
    if ChptTF^.CompileAll then ResetRdOnly
    else if not top and (ChptTF^.TimeStmp<oldChptTF^.TimeStmp) then begin
      ResetRdOnly; SetCompileAll end;
    goto 3 end;
  if not create or (top and not IsTestRun) then RunError(631);
  OpenCreateF(Exclusive); SetCompileAll;
3:if HasPassword(Chpt,1,'') then CRdb^.Encrypted:=false
  else CRdb^.Encrypted:=true;
end;
procedure CloseChpt;
var p,p2:pointer; del:boolean; d:DirStr; i:word;
begin
  if CRdb=nil then exit; ClearHelpStkForCRdb;
  SaveFiles; del:=Chpt^.NRecs=0; d:=CRdb^.RdbDir; CloseFAfter(FileDRoot);
  LinkDRoot:=CRdb^.OldLDRoot; FuncDRoot:=CRdb^.OldFCRoot;
  p:=CRdb; p2:=CRdb^.Mark2; CRdb:=CRdb^.ChainBack; ReleaseBoth(p,p2);
  if CRdb<>nil then begin
    FileDRoot:=CRdb^.FD; SetChptFldDptr; ChDir(CRdb^.RdbDir);
    if del then begin
      {$I-} RmDir(d);{$I+} if IOResult<>0 then begin
        SetMsgPar(d);WrLLF10Msg(621) end end end
  else begin
    ChDir(OldDir); for i:=1 to FloppyDrives do ReleaseDrive(i) end;
end;




procedure GoCompileErr(IRec,N:word);
  begin IsCompileErr:=true;InpRdbPos.R:=CRdb;InpRdbPos.IRec:=IRec;
        CurrPos:=0;RdMsg(N);GoExit end;

{$ifndef FandRunV}
procedure ClearXFUpdLock; far;
  begin with CFile^ do if XF<>nil then XF^.UpdLockCnt:=0 end;

function CompRunChptRec(CC:word):boolean;
procedure Diagnostics(MaxHp:pointer;Free:longint;FD:FileDPtr);
var p:pointer; s1,s2,s3,s4:string8; r:RdbDPtr;
begin
  r:=CRdb; while r^.ChainBack<>nil do r:=r^.ChainBack;
  str(AbsAdr(CRdb)-AbsAdr(r),s1);  { bytes on top of this RDB }
  str(AbsAdr(HeapPtr)-AbsAdr(E^.AfterE),s2); { bytes for FileD's }
  if FD<>nil then begin
    p:=FD^.Chain; if p=nil then p:=HeapPtr; str(AbsAdr(p)-AbsAdr(FD),s3) end
  else str(AbsAdr(MaxHp)-AbsAdr(HeapPtr),s3);  { bytes of this chapter }
  str(Free,s4); Set4MsgPar(s1,s2,s3,s4);WrLLF10Msg(136);
end;
function FindFD:FileDPtr;
var FD:FileDPtr; FName:string12; d:DirStr; name:NameStr; ext:ExtStr;
label 1;
begin
  FName:=trailchar(' ',_shorts(ChptName)); FSplit(FName,d,name,ext);
  FD:=FileDRoot;while FD<>nil do begin
    if SEquUpcase(FD^.Name,name) then goto 1; FD:=FD^.Chain end;
1:FindFD:=FD;
end;
var STyp:string[1]; p,p2,MaxHp:pointer; er:ExitRecord; OldE:EditDPtr;
    RP:RdbPos; Free:longint; WasError,WasGraph,uw,mv:boolean;
    FD,lstFD:FileDPtr; oldLd,ld:LinkDPtr; EO:EditOptPtr; nStrm:word;
label 1,2;
begin
  OldE:=E; MarkBoth(p,p2); WrEStatus; NewExit(Ovr,er); goto 2;
  IsCompileErr:=false; uw:=false; mv:=MausVisible;
  lstFD:=LastInChain(FileDRoot); oldLd:=LinkDRoot;
  WasError:=true; WasGraph:=IsGraphMode;
  FD:=nil; STyp:=_shorts(ChptTyp); RP.R:=CRdb; RP.IRec:=CRec; {$ifdef FandSQL}
  nStrm:=nStreams;                                            {$endif}
  if CC=_AltF9_ then
    if FindChpt('P','MAIN',true,RP) then goto 1 else WrLLF10Msg(58)
  else case STyp[1] of
   'F':begin FD:=FindFD; if (FD<>nil) and (CC=_CtrlF9_) then begin
         EO:=GetEditOpt;CFile:=FD;EO^.Flds:=AllFldsList(CFile,false);
         if SelFldsForEO(EO,nil) then EditDataFile(FD,EO) end end;
   'E':if CC=_CtrlF9_ then begin
         EO:=GetEditOpt; EO^.FormPos:=RP; EditDataFile(nil,EO) end
       else begin PushEdit; RdFormOrDesign(nil,nil,RP) end;
   'M':begin SetInpTT(RP,true); ReadMerge; if CC=_CtrlF9_ then RunMerge end;
   'R':begin SetInpTT(RP,true); ReadReport(nil); if CC=_CtrlF9_ then begin
         RunReport(nil);SaveFiles;ViewPrinterTxt end end;
   'P':if CC=_CtrlF9_ then begin
1:       if UserW<>0 then begin PopW(UserW); uw:=true end;
         RunMainProc(RP,CRdb^.ChainBack=nil) end
       else begin lstFD:=LastInChain(FileDRoot); ld:=LinkDRoot;
          SetInpTT(RP,true);ReadProcHead; ReadProcBody;
          lstFD^.Chain:=nil; LinkDRoot:=ld end;
 {$ifdef FandProlog}
   'L':if CC=_CtrlF9_ then begin
         TextAttr:=ProcAttr; ClrScr; RunProlog(RP,nil) end;
 {$endif}
  end;
  WasError:=false;
2:MaxHp:=HeapPtr; ReleaseStore2(p2); Free:=StoreAvail; RestoreExit(er);
  RunMsgClear; if WasError then begin     {$ifdef FandSQL}
    ShutDownStreams(nStrm); {$endif}
    TextAttr:=colors.uNorm;
    if IsGraphMode and not WasGraph then ScrTextMode(false,false)
    else ClrScr end;
  if uw then begin UserW:=0;{mem overflow}UserW:=PushW(1,1,TxtCols,TxtRows) end;
  SaveFiles; if mv then ShowMouse;
  if WasError then ForAllFDs(ClearXFUpdLock);
  CFile:=lstFD^.Chain; while CFile<>nil do begin
    CloseFile; CFile:=CFile^.Chain end;
  lstFD^.Chain:=nil; LinkDRoot:=oldLd;
  ReleaseBoth(p,p2); E:=OldE; RdEStatus; CRdb:=RP.R; PrevCompInp:=nil;
  ReadRec(CRec);
  if IsCompileErr then CompRunChptRec:=false
  else begin
    CompRunChptRec:=true; if WasError then exit; b_(ChptVerif,false);
    WriteRec(CRec); if CC=_CtrlF8_ then Diagnostics(MaxHp,Free,FD) end;
end;
{$endif}

procedure RdUserId(Chk:boolean);
var Z:FrmlPtr; pw,pw2,name:string20; code:word; acc:string;
label 1;
begin
  @RdFldNameFrml:=nil;
  RdLex; if Lexem=^z then exit; if chk then pw:=PassWord(false);
1:TestLex(_quotedstr); name:=LexWord; RdLex; Accept(',');
  code:=RdInteger; Accept(',');
  Z:=RdStrFrml; pw2:=RunShortStr(Z); ReleaseStore(Z);
  if Lexem=',' then begin RdLex; RdByteList(acc) end
  else begin acc[0]:=#1; acc[1]:=char(code) end;
  if chk then begin
    if SEquUpcase(pw,pw2) then begin
      UserName:=name; UserCode:=code; UserPassword:=pw2; AccRight:=acc; exit end end
  else if code=0 then begin
      UserName:=name; UserCode:=code; UserPassWord:=pw2 end;
  if Lexem<>^z then begin Accept(';'); if Lexem<>^z then goto 1 end;
  if chk then RunError(629);
end;

function CompileRdb(Displ,Run,FromCtrlF10:boolean):boolean;

function RdF(var FileName:string12):pointer;
var d:DirStr; name:NameStr; ext:ExtStr; FDTyp:char; s:string;
    IdF,TxtF:FieldDPtr;  i,n:integer; nr:string[10];
begin
  FSplit(FileName,d,name,ext); FDTyp:=ExtToTyp(ext);
  if FDTyp='0' then begin
    RdMsg(51); s:=MsgLine; RdMsg(49); val(MsgLine,n,i);
    str(TxtCols-n,nr); s:=s+nr; SetInpStr(s) end
  else SetInpTTPos(_t(ChptTxt),CRdb^.Encrypted);
  RdF:=RdFileD(Name,FDTyp,ext);
end;
function MakeDbfDcl(Nm:string):longint;
var Hd:DBaseHd; Fd:DBaseFld; i,n,h:word; t:LongStrPtr; c:char;
    s:string[80]; s1:string[10]; p:pointer;
begin
  CPath:=FExpand(Nm+'.DBF'); CVol:='';
  i:=GetCatIRec(Nm,true); if i<>0 then RdCatPathVol(i);
  h:=OpenH(_isoldfile,RdOnly); TestCPathError;
  ReadH(h,32,Hd); n:=(Hd.HdLen-1) div 32 -1; t:=GetStore(2); t^.LL:=0;
  for i:=1 to n do begin
    ReadH(h,32,Fd); s:=StrPas(Fd.Name);
    case Fd.Typ of 'C':c:='A'; 'D':c:='D'; 'L':c:='B'; 'M':c:='T'; 'N','F':c:='F' end;
    s:=s+':'+c;
    case c of
      'A': begin str(Fd.Len,s1); s:=s+','+s1 end;
      'F': begin dec(Fd.Len,Fd.Dec); if Fd.Dec<>0 then dec(Fd.Len);
           str(Fd.Len,s1); s:=s+','+s1; str(Fd.Dec,s1); s:=s+'.'+s1 end end;
    s:=s+';'^m^j;
    p:=GetStore(length(s)); Move(s[1],p^,length(s)); inc(t^.LL,length(s)) end;
  longs_(ChptTxt,t); CloseH(h);
end;

function MergeOldNew(Verif:boolean;Pos:longint):boolean;
function EquStoredF(F1,F2:FieldDPtr):boolean;
label 1;
begin
  EquStoredF:=false;
1:while (F1<>nil) and (F1^.Flg and f_Stored=0) do F1:=F1^.Chain;
  while (F2<>nil) and (F2^.Flg and f_Stored=0) do F2:=F2^.Chain;
  if F1=nil then begin if F2<>nil then exit; EquStoredF:=true; exit end;
  if (F2=nil) or not FldTypIdentity(F1,F2) or
  (F1^.Flg and not f_Mask<>F2^.Flg and not f_Mask) then exit;
  F1:=F1^.Chain; F2:=F2^.Chain; goto 1;
end;
function EquKeys(K1,K2:KeyDPtr):boolean;
var KF1,KF2:KeyFldDPtr;
begin
  EquKeys:=false; while K1<>nil do begin
    if (K2=nil) or (K1^.Duplic<>K2^.Duplic) then exit;
    KF1:=K1^.KFlds; KF2:=K2^.KFlds; while KF1<>nil do begin
      if (KF2=nil) or (KF1^.CompLex<>KF2^.CompLex) or (KF1^.Descend<>KF2^.Descend)
        or (KF1^.FldD^.Name<>KF2^.FldD^.Name) then exit;
      KF1:=KF1^.Chain; KF2:=KF2^.Chain end;
    if KF2<>nil then exit;
    K1:=K1^.Chain; K2:=K2^.Chain end;
  if K2<>nil then exit; EquKeys:=true;
end;
procedure DeleteF;
begin CloseFile; SetCPathVol; DeleteFile(CPath);
  CExtToX; if CFile^.XF<>nil then DeleteFile(CPath);
  CExtToT; if CFile^.TF<>nil then DeleteFile(CPath) end;
function MergAndReplace(FDOld,FDNew:FileDPtr):boolean;
var s:string; er:ExitRecord; p:PathStr;
label 1,2;
begin
  NewExit(Ovr,er);goto 1; s:='#I1_'+FDOld^.Name+' #O1_@'; SetInpStr(s);
  SpecFDNameAllowed:=true; ReadMerge; SpecFDNameAllowed:=false;
  RunMerge; SaveFiles; RestoreExit(er);
  CFile:=FDOld; DeleteF; CFile:=FDNew; CloseFile; FDOld^.Typ:=FDNew^.Typ;
  SetCPathVol;p:=CPath;CFile:=FDOld;SetCPathVol;RenameFile56(p,CPath,false);
  CFile:=FDNew;{TF^.Format used}CExtToT;p:=CPath;
  SetCPathVol; CExtToT; RenameFile56(CPath,p,false);
  MergAndReplace:=true; exit;
1:RestoreExit(er); CFile:=FDOld; CloseFile; CFile:=FDNew; DeleteF;
  SpecFDNameAllowed:=false; MergAndReplace:=false;
end;
var FDNew,FDOld:FileDPtr; Name:string[20]; ld:LinkDPtr;
label 1;
begin
  ld:=LinkDRoot; MergeOldNew:=false;
  FDNew:=CFile; SetCPathVol; Name:=FDNew^.Name; FDNew^.Name:='@';
  CFile:=Chpt; if not RdFDSegment(0,Pos) then goto 1;
  ChainLast(FileDRoot,CFile); FDOld:=CFile; FDOld^.Name:=Name;
  if ((FDNew^.Typ<>FDOld^.Typ) or not EquStoredF(FDNew^.FldD,FDOld^.FldD))
{$ifdef FandSQL} and not FDNew^.IsSQLFile and not FDOld^.IsSQLFile {$endif}
    then begin MergAndReplace(FDOld,FDNew); MergeOldNew:=true end
  else if (FDOld^.Typ='X') and not EquKeys(FDOld^.Keys,FDNew^.Keys) then begin
    SetCPathVol; CExtToX; DeleteFile(CPath) end;
1:FDNew^.Chain:=nil; LinkDRoot:=ld; Move(Name,FDNew^.Name,length(Name)+1);
  CFile:=FDNew;CRecPtr:=Chpt^.RecPtr;
end;
var Buf:array[1..40]of word;  w:longint;
function CompileMsgOn:word;
var s:string[12];
begin
  RdMsg(15);
  if IsTestRun then begin
    w:=PushWFramed(0,0,30,4,colors.SNorm,MsgLine,'',WHasFrame+WDoubleFrame+WShadow);
    RdMsg(117); s:=GetDLine(MsgLine[1],length(MsgLine),'/',1);
    gotoxy(3,2); write(s); CompileMsgOn:=length(s);
    gotoxy(3,3); write(GetDLine(MsgLine[1],length(MsgLine),'/',2)) end
  else begin
    ScrRdBuf(0,TxtRows-1,Buf,40); w:=0; CompileMsgOn:=0;
    ScrClr(0,TxtRows-1,length(MsgLine)+2,1,' ',colors.zNorm);
    ScrWrStr(1,TxtRows-1,MsgLine,colors.zNorm) end;
end;
procedure CompileMsgOff;
  begin if w<>0 then PopW(w) else ScrWrBuf(0,TxtRows-1,Buf,40) end;

                                         { CompileRdb - body }
var I,J,OldTxt,Txt,OldCRec:longint; STyp:string[1]; Typ:char;
    Name:string12; dir:DirStr; nm:NameStr; ext:ExtStr;
    Verif,FDCompiled,Encryp:boolean; Mode:char;  RP:RdbPos;
    p,p1,p2:pointer; er:ExitRecord; OldE:EditDPtr; lmsg:word; ld:LinkDPtr;
    RprtTxt:LongStrPtr; top:boolean; lstFD:FileDPtr;
label 1,2;
begin
  OldE:=E; MarkBoth(p,p2); p1:=p; NewExit(Ovr,er);goto 1;
  IsCompileErr:=false; FDCompiled:=false; OldCRec:=CRec;RP.R:=CRdb;
  top:=CRdb^.ChainBack=nil;
  if top then begin
    UserName[0]:=#0;UserCode:=0;UserPassword[0]:=#0;AccRight[0]:=#0;
    if ChptTF^.CompileAll or CompileFD then Switches[0]:=#0 end;
  lmsg:=CompileMsgOn;
  CRecPtr:=Chpt^.RecPtr; Encryp:=CRdb^.Encrypted;
  for I:=1 to Chpt^.NRecs do begin
    ReadRec(I); RP.IRec:=I;
    Verif:=_b(ChptVerif); STyp:=_shorts(ChptTyp); Typ:=STyp[1];
    Name:=trailchar(' ',_shorts(ChptName)); Txt:=_t(ChptTxt);
    if Verif and ((ChptTF^.LicenseNr<>0) or Encryp or (Chpt^.UMode=RdOnly))
      then GoCompileErr(I,647);
    if Verif or ChptTF^.CompileAll or FromCtrlF10 or (Typ='U') or
       (Typ in ['F','D']) and CompileFD or
       (Typ='P') and ChptTF^.CompileProc then begin
      OldTxt:=_t(ChptOldTxt); InpRdbPos:=RP;
     if IsTestRun then begin
        clrscr; gotoxy(3+lmsg,2); write(I:4);
        gotoxy(3+lmsg,3); write(STyp:4,_shorts(ChptName):14);
        if not (Typ in [' ','D','U']) then begin { dupclicate name checking }
          for J:=1 to I-1 do begin ReadRec(J);
           if (STyp=_shorts(ChptTyp)) and SEquUpcase(Name,trailchar(' ',_shorts(ChptName)))
             then GoCompileErr(I,649) end;
          ReadRec(I) end end;
      case Typ of
        'F':begin FDCompiled:=true; ld:=LinkDRoot; MarkStore(p1);
            FSplit(Name,dir,nm,ext); if (Txt=0) and IsTestRun then begin
              SetMsgPar(Name);
              if SEquUpcase(ext,'.DBF') and PromptYN(39) then begin
                t_(ChptOldTxt,0); OldTxt:=0; MakeDBFDcl(nm);
                Txt:=_t(ChptTxt); WriteRec(I) end end;
            {$ifndef FandSQL} if SEquUpcase(ext,'.SQL') then GoCompileErr(I,654); {$endif}
            if Verif or ChptTF^.CompileAll or (OldTxt=0) then begin
2:            p1:=RdF(Name); WrFDSegment(I);
              if CFile^.IsHlpFile then CRdb^.HelpFD:=CFile;
              if OldTxt>0 then
                MergeOldNew(Verif,OldTxt); ReleaseStore(p1);
              CFile:=Chpt;
              if ChptTF^.LicenseNr=0 then ChptTF^.Delete(OldTxt)
              else if OldTxt<>0 then ChptTF^.Delete(OldTxt-ChptTF^.LicenseNr);
              end
            else if not RdFDSegment(I,OldTxt) then begin
              LinkDRoot:=ld; ReleaseStore(p1); CFile:=Chpt; goto 2 end
            else begin
              ChainLast(FileDRoot,CFile); MarkStore(p1);
              if CFile^.IsHlpFile then CRdb^.HelpFD:=CFile end end;
        'M':begin SetInpTTPos(Txt,Encryp);ReadMerge end;
        'R':if (Txt=0) and IsTestRun then begin
              RprtTxt:=SelGenRprt(Name); CFile:=Chpt; if RprtTxt=nil then GoCompileErr(I,1145);
              longs_(ChptTxt,RprtTxt); WriteRec(I) end
            else begin SetInpTTPos(Txt,Encryp);ReadReport(nil) end;
        'P':begin lstFD:=LastInChain(FileDRoot); ld:=LinkDRoot;
             SetInpTTPos(Txt,Encryp);ReadProcHead;ReadProcBody;
             lstFD^.Chain:=nil;LinkDRoot:=ld end;
        'E':begin PushEdit;RdFormOrDesign(nil,nil,RP);E:=OldE end;
        'U':begin if not top or (I>1) then GoCompileErr(I,623);
            if Txt<>0 then begin ResetCompilePars; SetInpTTPos(Txt,Encryp);
              RdUserId(not IsTestRun or (ChptTF^.LicenseNr<>0)); MarkStore(p1) end end;
        'D':begin ResetCompilePars; SetInpTTPos(Txt,Encryp); ReadDeclChpt;
              MarkStore(p1) end;
      {$ifdef FandProlog}
        'L':begin SetInpTTPos(Txt,Encryp); ReadProlog(I) end;
      {$endif}
    end end;
    ReleaseBoth(p1,p2);CFile:=Chpt;CRecPtr:=Chpt^.RecPtr;
    if Verif then begin ReadRec(I);b_(ChptVerif,false);WriteRec(I) end;
  end;
  with ChptTF^ do if CompileAll or CompileProc then begin
    CompileAll:=false;CompileProc:=false;SetUpdHandle(Handle) end;
  CompileFD:=false; CompileRdb:=true; RestoreExit(er);
  if not Run then begin CRecPtr:=E^.NewRecPtr; ReadRec(CRec) end;
  CompileMsgOff; {$ifdef FandSQL}
  if top and (Strm1<>nil) then Strm1^.Login(UserName,UserPassword); {$endif}
  exit;
1:RestoreExit(er); CompileRdb:=false; CompileMsgOff;
  ReleaseFDLDAfterChpt; PrevCompInp:=nil;
  ReleaseBoth(p,p2); E:=OldE; CFile:=Chpt;
  if not Run then CRecPtr:=E^.NewRecPtr;
  if not IsCompileErr then begin InpRdbPos.IRec:=I end;
end;

{$ifndef FandRunV}
procedure GotoErrPos(var Brk:word);
var s:ScreenStr;
begin
  IsCompileErr:=false; s:=MsgLine; if InpRdbPos.R<>CRdb then begin
    DisplEditWw; SetMsgPar(s); WrLLF10Msg(110);
    if InpRdbPos.IRec=0 then SetMsgPar('') else SetMsgPar(InpRdbPos.R^.FD^.Name);
    WrLLF10Msg(622); Brk:=0; exit end;
  if CurrPos=0 then begin
    DisplEditWw; GotoRecFld(InpRdbPos.IRec,E^.FirstFld^.Chain);
    SetMsgPar(s); WrLLF10Msg(110); Brk:=0;exit end;
  CFld:=E^.LastFld; SetNewCRec(InpRdbPos.IRec,true);
  r_(ChptTxtPos,integer(CurrPos)); WriteRec(CRec);
  EditFreeTxt(ChptTxt,s,true,Brk);
end;
{$endif}


function EditExecRdb(const Nm,ProcNm:string;ProcCall:InstrPtr):boolean;
var EscCode:boolean;
procedure WrErrMsg630;
begin
  IsCompileErr:=false; SetMsgPar(MsgLine); WrLLF10Msg(110);
  SetMsgPar(Nm); WrLLF10Msg(630);
end;

var Brk,cc:word; p:pointer; passw:string[20]; b,top,wasGraph:boolean;
    er,er2:ExitRecord; RP:RdbPos; EO:EditOptPtr; w:longint;
label 0,1,2,3,4,41,5,6,8,9;
begin
  EditExecRdb:=false; top:=CRdb=nil; EscCode:=false; w:=UserW; UserW:=0;
  wasGraph:=IsGraphMode;
  {$ifdef FandSQL} if top then SQLConnect;  {$endif}
  NewExit(Ovr,er);goto 9;
  CreateOpenChpt(Nm,true); CompileFD:=true;
{$ifndef FandRunV}
  if not IsTestRun or (ChptTF^.LicenseNr<>0) or
     not top and CRdb^.Encrypted then begin
{$endif}
     MarkStore(p);EditRdbMode:=false;
     if CompileRdb(false,true,false) then
       if FindChpt('P',ProcNm,true,RP) then
         begin NewExit(Ovr,er2);goto 0;  IsCompileErr:=false;
           if ProcCall<>nil then begin ProcCall^.Pos:=RP; CallProcedure(ProcCall) end
           else RunMainProc(RP,top);
           EditExecRdb:=true; goto 9;
         0:if IsCompileErr then WrErrMsg630; goto 9 end
       else begin Set2MsgPar(Nm,ProcNm); WrLLF10Msg(632) end
     else if IsCompileErr then WrErrMsg630;
{$ifndef FandRunV}
      if (ChptTF^.LicenseNr<>0) or CRdb^.Encrypted
         or (Chpt^.UMode=RdOnly) then goto 9;
      ReleaseFDLDAfterChpt; ReleaseStore(p) end
   else if not top then UserW:=PushW(1,1,TxtCols,TxtRows);
   EditRdbMode:=true; if CRdb^.Encrypted then passw:=Password(false);
   IsTestRun:=true; EO:=GetEditOpt; EO^.Flds:=AllFldsList(Chpt,true);
   EO^.Flds:=EO^.Flds^.Chain^.Chain^.Chain;
   NewEditD(Chpt,EO);
   E^.MustCheck:=true; {ChptTyp}
   if CRdb^.Encrypted then
   if HasPassword(Chpt,1,passw) then begin
     CRdb^.Encrypted:=false; SetPassWord(Chpt,1,''); CodingCRdb(false) end
   else begin WrLLF10Msg(629); goto 9 end;
   if not OpenEditWw then goto 8; EditExecRdb:=true; Chpt^.WasRdOnly:=false;
   if not top and (Chpt^.NRecs>0) then
     if CompileRdb(true,false,false) then begin
       if FindChpt('P',ProcNm,true,RP) then GotoRecFld(RP.IRec,CFld) end
     else goto 4
   else if ChptTF^.IRec<=Chpt^.NRecs then GotoRecFld(ChptTF^.IRec,CFld);
1: RunEdit(nil,Brk);
2: cc:=KbdChar; SaveFiles;
   if (cc=_CtrlF10_) or ChptTF^.CompileAll or CompileFD then begin
     ReleaseFDLDAfterChpt;SetSelectFalse;E^.Bool:=nil;
     ReleaseStore(E^.AfterE) end;
   if cc=_CtrlF10_ then begin
     SetUpdHandle(ChptTF^.Handle);
     if not CompileRdb(true,false,true) then goto 3;
     if not PromptCodeRdb then goto 6; Chpt^.WasRdOnly:=true; goto 8 end;
   if Brk<>0 then begin
     if not CompileRdb(Brk=2,false,false) then begin
3:     if IsCompileErr then goto 4; if Brk=1 then DisplEditWw;
       GotoRecFld(InpRdbPos.IRec,E^.FirstFld^.Chain); goto 1 end;
     if cc=_AltF2_ then begin
       EditHelpOrCat(cc,0,''); goto 41 end;
     if not CompRunChptRec(cc) then begin
4:     GotoErrPos(Brk); goto 5 end;
41:  if Brk=1 then begin
       EditFreeTxt(ChptTxt,'',true,Brk);
5:     if Brk<>0 then goto 2 else goto 1 end
     else begin
6:     DisplEditWw; goto 1 end end;
   ChptTF^.IRec:=CRec; SetUpdHandle(ChptTF^.Handle);
8: PopEdit;
{$endif}
9: RestoreExit(er);
   if not wasGraph and IsGraphMode then ScrTextMode(false,false);
   if UserW<>0 then PopW(UserW); UserW:=w; RunMsgClear;
   CloseChpt;
{$ifdef FandSQL}  if top then SQLDisconnect;  {$endif}
end;

procedure InstallRdb(n:NameStr);
procedure UpdateUTxt;
var w:longint; TxtPos,LicNr:word; S,s2:LongStrPtr; Srch,Upd,b:boolean;
    OldPos,Pos:longint; er:ExitRecord; p,p1:pointer;
label 1,2,3,4;
begin
  CFile:=Chpt; CRecPtr:=Chpt^.RecPtr; LicNr:=ChptTF^.LicenseNr; MarkStore(p1);
  if CFile^.NRecs=0 then goto 1; ReadRec(1);
  if _shorts(ChptTyp)<>'U' then begin
1:  WrLLF10Msg(9); {exit} end;
  w:=PushW(1,1,TxtCols,TxtRows-1); TxtPos:=1; TextAttr:=colors.tNorm;
  OldPos:=_t(ChptTxt); S:=_longs(ChptTxt); b:=false;
  if CRdb^.Encrypted then CodingLongStr(S); NewExit(Ovr,er); goto 4;
  SetInpLongStr(S,false); MarkStore(p); RdUserId(false); ReleaseStore(p); b:=true;
2:SimpleEditText('T','','',@S^.A,$7FFF,S^.LL,TxtPos,Upd);
  SetInpLongStr(S,false); MarkStore(p); RdUserId(false); ReleaseStore(p); b:=false;
  if Upd then begin StoreChptTxt(ChptTxt,s,true); WriteRec(1) end;
3:PopW(w); ReleaseStore(p1); exit;
4:if b then begin
    WrLLF10MsgLine; ReleaseStore(p); if PromptYN(59) then goto 2; goto 3 end;
  WrLLF10Msg(9); goto 3;
end;
procedure UpdateCat;
  var EO:EditOptPtr;
  begin CFile:=CatFD; if CatFD^.Handle=$FF then OpenCreateF(Exclusive);
        EO:=GetEditOpt; EO^.Flds:=AllFldsList(CatFD,true);
        EditDataFile(CatFD,EO); ChDir(OldDir); ReleaseStore(EO) end;
var er:ExitRecord; passw:string[20]; w:PMenuBoxS; i:word;
label 0,1;
begin
  NewExit(Ovr,er);goto 1; CreateOpenChpt(n,false);
  if not HasPassword(Chpt,1,'') and not HasPassword(Chpt,2,'') then begin
    passw:=Password(false); if not HasPassword(Chpt,2,passw) then begin
      WrLLF10Msg(629); goto 1 end end;
  if Chpt^.UMode=RdOnly then begin UpdateCat; goto 1 end;
  RdMsg(8); New(w,Init(43,6,StringPtr(@MsgLine))); i:=1;
0:i:=w^.Exec(i); case i of
   0:begin w^.Done; ReleaseStore(w); goto 1 end;
   1:begin UpdateCat;goto 0 end;
   2:UpdateUTxt;
   3:SetPassWord(Chpt,2,PassWord(true)) end;
  SetUpdHandle(ChptTF^.Handle); goto 0;
1:RestoreExit(er); CloseChpt;
end;