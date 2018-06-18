{$I Switches}
{$A-,B-,D+,F+,G-,I+,O-,R-,S+,V+,X+,L-}
unit RdRun;

interface

uses dos,drivers,base,access
{$ifdef FandSQL}    ,channel {$endif}
     ;

type                         { Merge }
  MInstrCode = (_zero,_move,_output,_locvar,_parfile,_ifthenelseM);
  AssignDPtr = ^AssignD;
  AssignD = record
    Chain:AssignDPtr;
    case Kind:MInstrCode of
     _zero      :(FldD:FieldDPtr);
     _move      :(ToPtr,FromPtr:BytePtr;L:word);
     _output    :(Add:boolean;Frml:FrmlPtr;OFldD:FieldDPtr);
     _locvar    :(Add1:boolean;Frml1:FrmlPtr;LV:LocVarPtr);
     _parfile   :(Add2:boolean;Frml2:FrmlPtr;
                  FD:FileDPtr;PFldD:FieldDPtr);
    _ifthenelseM:(Bool:FrmlPtr;Instr,ElseInstr:AssignDPtr)
  end;
  InpDPtr = ^InpD;
  OutpFDPtr = ^OutpFD;
  OutpFD = record
    Chain: OutpFDPtr;
    FD: FileDPtr;
    Md:LockMode;
    RecPtr: pointer;
    InplFD: FileDPtr;
    Append: boolean;
{$ifdef FandSQL}
    Strm: SQLStreamPtr;
{$endif}
  end;
  OutpRDPtr = ^OutpRD;
  OutpRD=record
    Chain:OutpRDPtr;
    OD:OutpFDPtr; {nil=dummy}
    Bool:FrmlPtr;
    Ass:AssignDPtr;
  end;
  ConstList = ^ConstListEl;
  LvDescrPtr = ^LvDescr;
  InpD = record                       {used in Merge+Report}
    Scan:XScanPtr;
    AutoSort:boolean;
    SK:KeyFldDPtr;
    Md:LockMode;
    IRec:longint;
    ForwRecPtr:pointer;
    Bool:FrmlPtr;
    SQLFilter:boolean;
    MFld:KeyFldDPtr;
    Sum:SumElPtr;
    Exist:boolean;
    Op:char;
    Count:float;
    Chk:ChkDPtr;
    OpErr:Char;
    Error:boolean;
    OpWarn:char;
    Warning:boolean;
    ErrTxtFrml:FrmlPtr; {<>nil then record error test used}
    case byte of
     1:( SFld:KeyFldDPtr;                { only Report }
         OldSFlds:ConstList;
         FrstLvS,LstLvS:LvDescrPtr);{ FrstLvS^.Ft=DE }
     2:( IsInplace:boolean;              { only Merge }
         RD:OutpRDPtr);
  end;
  ConstListEl = record
    Chain:ConstList;
    case char of
      'S':(S:string);
      'R':(R:float);
      'B':(B:boolean);
  end;
                              { Report }

  AutoRprtMode = (_ALstg,_ARprt,_ATotal,_AErrRecs);
  RprtFDList=^RprtFDListEl;
  RprtFDListEl = record
    Chain:RprtFDList; FD:FileDPtr; ViewKey:KeyDPtr;
    Cond:FrmlPtr; KeyIn:KeyInDPtr; SQLFilter:boolean;
    LVRecPtr:pointer
  end;
  RprtOptPtr = ^RprtOpt;
  RprtOpt = record
    FDL:RprtFDListEl;
    Path:StringPtr;
    CatIRec:word;
    UserSelFlds,UserCondQuest,FromStr,SyntxChk:boolean;
    Times:FrmlPtr;
    Mode:AutoRprtMode;
    RprtPos:RdbPos;
    Flds,     {<>nil => autoreport}
    Ctrl,Sum:FieldList;
    SK:KeyFldDPtr;
    WidthFrml,Head:FrmlPtr;
    Width:word;
    CondTxt:StringPtr;
    HeadTxt:LongStrPtr;
    Style:char;
    Edit,PrintCtrl:boolean;
  end;

  RFldDPtr = ^RFldD;
  BlkDPtr = ^BlkD;
  RFldD = record
    Chain:RFldDPtr;
    FrmlTyp,Typ:char;    {R,F,D,T}
    BlankOrWrap:boolean; {long date 'DD.MM.YYYY'}
    Frml:FrmlPtr;
    Name:string[1]; {curr. length}
  end;
  BlkD = record
    Chain:BlkDPtr;
    Bool:FrmlPtr;
    Sum:SumElPtr;
    Txt:Pchar;          {sequence of strings}
    AbsLine,SetPage,NotAtEnd,FF1,FF2:boolean;
    LineBound,LineNo,PageNo:FrmlPtr;
    NTxtLines,NBlksFrst,DHLevel:word;
    RFD:RFldDPtr;
    BeforeProc,AfterProc:AssignDPtr;
  end;
  LvDescr = record
    Chain,ChainBack:LvDescrPtr;
    ZeroLst:FloatPtrList;
    Hd,Ft:BlkDPtr;
    Fld:FieldDPtr;
  end;
                                { Edit }

  InstrPtr = ^Instr;
  EdExKeyDPtr = ^EdExKeyD;
  EdExKeyD = record
    Chain:EdExKeyDPtr;
    Break:byte;
    KeyCode:word end;
  EdExitDPtr = ^EdExitD;
  EdExitD = record
    Chain:EdExitDPtr;
    Keys:EdExKeyDPtr; AtWrRec,AtNewRec,NegFlds:boolean; Flds:FieldList;   {in edittxt not used}
    case Typ:char of
      'R':(RO:pointer);
      'P':(Proc:InstrPtr);                     {in edittxt only 'P','Q'}
      {'Q' quit   #0 dummy}
  end;
  EditOptPtr = ^EditOpt;
  EditOpt = record
    FormPos:RdbPos;
    UserSelFlds,SetOnlyView,NegDupl,NegTab,NegNoEd,SyntxChk:boolean;
    Flds,Dupl,Tab,NoEd:FieldList;
    Cond:FrmlPtr;
    Head,Last,CtrlLast,AltLast,ShiftLast,Mode:FrmlPtr;
    StartRecNoZ,StartRecKeyZ,StartIRecZ,StartFieldZ,
    SaveAfterZ,WatchDelayZ,RefreshDelayZ:FrmlPtr;
    W:WRectFrml;
    ZAttr,ZdNorm,ZdHiLi,ZdSubset,ZdDel,ZdTab,ZdSelect:FrmlPtr;
    Top:FrmlPtr;
    WFlags:byte;
    ExD:EdExitDPtr;
    Journal:FileDPtr;
    ViewName:StringPtr;
    OwnerTyp:char;
    DownLD:LinkDPtr;
    DownLV:LocVarPtr;
    DownRecPtr,LVRecPtr:pointer;
    KIRoot:KeyInDPtr;
    SQLFilter:boolean;
    SelKey,ViewKey:KeyDPtr;
  end;

 EFldDPtr = ^EFldD;
  EFldD = object
    Chain,ChainBack:EFldDPtr;
    FldD:FieldDPtr;
    Chk:ChkDPtr;
    Impl:FrmlPtr;
    Dep:DepDPtr;
    KL:KeyList;
    Page,Col,Ln,L:byte;
    ScanNr:word;
    Tab,Dupl,Used,EdU,EdN:boolean;
    function Ed(IsNewRec:boolean):boolean;
  end;
  ERecTxtDPtr = ^ERecTxtD;
  ERecTxtD = record
    Chain:ERecTxtDPtr;
    N:word;
    SL:StringList;
  end;
  EditDPtr = ^EditD;
  EditD = record
    PrevE:EditDPtr;
    FD:FileDPtr;
    OldMd:LockMode;
    IsUserForm:boolean;
    Flds:FieldList;
    OldRecPtr,NewRecPtr :pointer;
    FrstCol,FrstRow,LastCol,LastRow,Rows:byte;
    V:WRect; ShdwX,ShdwY:byte;
    Attr,dNorm,dHiLi,dSubSet,dDel,dTab,dSelect:byte;
    Top:StringPtr;
    WFlags:byte;                                 { copied from EO }
    ExD:EdExitDPtr;                              {      "         }
    Journal:FileDPtr;                            {      "         }
    ViewName:StringPtr;                          {      "         }
    OwnerTyp:char; { #0=CtrlF7 }                 {      "         }
    DownLD:LinkDPtr;                             {      "         }
    DownLV:LocVarPtr;                            {      "         }
    DownRecPtr,LVRecPtr:pointer;                 {      "         }
    KIRoot:KeyInDPtr;                            {      "         }
    SQLFilter:boolean;                           {      "         }
    SelKey:WKeyDPtr;                             {      "         }
    HdTxt:StringList; NHdTxt:byte;
    SaveAfter,WatchDelay,RefreshDelay:word;
    RecNrPos,RecNrLen:byte;
    NPages:byte;
    RecTxt:ERecTxtDPtr;
    NRecs:byte;     {display}
    FirstFld,LastFld,StartFld:EFldDPtr;
    CFld,FirstEmptyFld:EFldDPtr;                         {copied}
    VK:KeyDPtr; WK:WKeyDPtr;                             {  "   }
    BaseRec:longint; IRec:byte;                          {  "   }
    IsNewRec,Append,Select,WasUpdated,EdRecVar,          {  "   }
    AddSwitch,ChkSwitch,WarnSwitch,SubSet,               {  "   }
    NoDelTFlds,WasWK:boolean;                            {  "   }
    NoDelete,VerifyDelete,NoCreate,F1Mode,               {  "   }
    OnlyAppend,OnlySearch,Only1Record,OnlyTabs,          {  "   }
    NoESCPrompt,MustESCPrompt,Prompt158,NoSrchMsg,       {  "   }
    WithBoolDispl,Mode24,NoCondCheck,F3LeadIn,           {  "   }
    LUpRDown,MouseEnter,TTExit,                          {  "   }
    MakeWorkX,NoShiftF7Msg,MustAdd,                      {  "   }
    MustCheck,SelMode: boolean;                          {  "   }
    DownSet,IsLocked,WwPart:boolean;
    DownKey:KeyDPtr;
    LockedRec:longint;
    Cond,Bool:FrmlPtr;
    BoolTxt,Head,Last,CtrlLast,AltLast,ShiftLast:StringPtr;
    NFlds,NTabsSet,NDuplSet,NEdSet:word;
    EdUpdated:boolean;
    Impl:ImplDPtr;
    StartRecNo:longint; StartRecKey:StringPtr; StartIRec:integer;
    OwnerRecNo:longint;
    ShiftF7LD: LinkDPtr;
    AfterE:pointer;
  end;

  PInstrCode = (_menubox,_menubar,_ifthenelseP,_whiledo,
             _repeatuntil,_break,_exit,_cancel,_save,_closefds,
             _window,_clrscr,_clrww,_clreol,_gotoxy,_display,
             _writeln,_comment,_setkeybuf,_clearkeybuf,_headline,
             _call,_exec,_copyfile,_proc,_lproc,_merge,_sort,_edit,_report,
             _edittxt,_printtxt,_puttxt,_sql,
             _asgnloc,_asgnpar,_asgnfield,_asgnedok,_asgnrand,_asgnusertoday,
             _randomize,
             _asgnusercode,_asgnusername,
             _asgnaccright,_asgnxnrecs,
             _asgnnrecs,_asgncatfield,_asgnrecfld,_asgnrecvar,_asgnclipbd,
             _turncat,_appendrec,_deleterec,_recallrec,_readrec,_writerec,
             _linkrec,
             _releasedrive,_mount,_indexfile,_getindex,_forall,
             _withshared,_withlocked,_withgraphics,
             _memdiag,_wait,_delay,_beep,_sound,_nosound,_help,_setprinter,
             _graph,_putpixel,_line,_rectangle,_ellipse,_floodfill,_outtextxy,
             _backup,_backupm,_resetcat,
             _setedittxt,_setmouse,_checkfile,_login,_sqlrdwrtxt,
             _portout
             );
  CpOption = (cpNo,cpFix,cpVar,cpTxt);
  CopyDPtr = ^CopyD;
  CopyD = record
    Path1:StringPtr; {FrmlPtr if cpList}
    CatIRec1:word;
    FD1:FileDPtr;
    ViewKey:KeyDPtr;
    WithX1:boolean;
    Opt1:CpOption;
    Path2:StringPtr; {  "  }
    CatIRec2:word;
    FD2:FileDPtr;
    WithX2:boolean;
    Opt2:CpOption;
    HdFD:FileDPtr;
    HdF:FieldDPtr;
    Append,NoCancel:boolean;
    Mode:byte;
  end;
  ChoiceDPtr = ^ChoiceD;
  ChoiceD = record
    Chain:ChoiceDPtr;
    HelpName:StringPtr;
    Displ,DisplEver,Enabled,TxtConst:boolean;
    Bool:FrmlPtr;
    Instr:InstrPtr;
    TxtFrml:FrmlPtr;
    Txt:StringPtr;
  end;
  WrLnDPtr = ^WrLnD;
  WrLnD = record
    Chain:WrLnDPtr;
    Frml:FrmlPtr;
    Typ:char; {S,B,F,D}
    case char of
      'F':(N,M:byte);
      'D':(Mask:StringPtr);
  end;
  LockDPtr = ^LockD;
  LockD = record
    Chain:LockDPtr;
    FD:FileDPtr;
    Frml:FrmlPtr;
    Md,OldMd:LockMode;
    N:longint;
  end;
  GraphVDPtr = ^GraphVD;
  GraphVD = record
    Chain:GraphVDPtr;
    XZ,YZ,Velikost:FrmlPtr; {real}
    BarPis,Text:FrmlPtr; {string}
  end;
  GraphWDPtr = ^GraphWD;
  GraphWD = record
    Chain:GraphWDPtr;
    XZ,YZ,XK,YK:FrmlPtr; {real}
    BarPoz,BarPis,Text:FrmlPtr; {string}
  end;
  GraphRGBDPtr = ^GraphRGBD;
  GraphRGBD = record
    Chain:GraphRGBDPtr;
    Barva:FrmlPtr; {string}
    R,G,B:FrmlPtr; {real}
  end;
  WinGPtr = ^ WinG;
  WinG = record
    W:WRectFrml;
    WR:WRect;
    ColFrame,ColBack,ColFor:FrmlPtr;  {string}
    Top:FrmlPtr;
    WFlags:byte;
  end;
  GraphDPtr = ^GraphD;
  GraphD = record
    FD:FileDPtr;
    GF:FrmlPtr;
    X,Y,Z:FieldDPtr;
    ZA:array[0..9]of FieldDPtr;
    HZA:array[0..9]of FrmlPtr;
    T,H,HX,HY,HZ,C,D,R,P,CO,Assign,Cond:FrmlPtr; {string}
    S,RS,RN,Max,Min,SP:FrmlPtr; {real}
    Interact:boolean;
    V:GraphVDPtr;
    W:GraphWDPtr;
    RGB:GraphRGBDPtr;
    KeyIn:KeyInDPtr;
    SQLFilter:boolean;
    ViewKey:KeyDPtr;
    WW:WinGPtr;
  end;
  TypAndFrml = record
    case FTyp:char of
    'S','R','B':(Frml:FrmlPtr; FromProlog,IsRetPar:boolean);
    'r','i','f':(FD:FileDPtr; RecPtr:pointer);
    'f'        :(TxtFrml:FrmlPtr; Name:StringPtr{if RecPtr<>nil});
  end;
  Instr = record
    Chain:InstrPtr;
    case Kind:PInstrCode of
      _menubox,_menubar:
            (HdLine:FrmlPtr; HelpRdb:RdbDPtr; WasESCBranch:boolean;
             ESCInstr:InstrPtr; Choices:ChoiceDPtr;
             Loop,PullDown,Shdw:boolean;
             X,Y,XSz:FrmlPtr; mAttr:array[0..3] of FrmlPtr);
      _ifthenelseP,_whiledo,_repeatuntil:
             (Bool:FrmlPtr; Instr,ElseInstr:InstrPtr);
      _merge,_display:(Pos:RdbPos);
      _proc:(PPos:RdbPos; N:byte; ExPar:boolean; TArg:array[1..2]of TypAndFrml);
      _lproc:(lpPos:RdbPos; lpName:StringPtr);
      _call:(RdbNm,ProcNm:StringPtr;ProcCall:InstrPtr);
      _exec:(ProgPath:StringPtr;ProgCatIRec:word;
             NoCancel,FreeMm,LdFont,TextMd:boolean;Param:FrmlPtr);
      _copyfile:(CD:CopyDPtr);
      _writeln:(LF:byte{0-write,1-writeln,2-message,3-message+help};WD:WrLnD;
                mHlpRdb:RdbDPtr;mHlpFrml:FrmlPtr);
      _gotoxy:(GoX,GoY:FrmlPtr);
      _asgnloc,_asgnusercode,_asgnusername,_asgnaccright,_asgnedok,
      _asgnrand,_asgnusertoday:
             (Frml:FrmlPtr;Add:boolean;AssLV:LocVarPtr);
      {_setprinter,_sound,_delay:(Frml:FrmlPtr);  real}
      {_setkeybuf,_headline,_sql:(Frml:FrmlPtr); string}
      _help:(Frml0:FrmlPtr;HelpRdb0:RdbDPtr);
      _asgnpar,_asgnnrecs,_asgnfield:
             (Frml1:FrmlPtr;Add1:boolean;FD:FileDPtr;FldD:FieldDPtr;
             RecFrml:FrmlPtr;Indexarg:boolean);
      _asgnrecfld:(Frml2:FrmlPtr;Add2:boolean;AssLV2:LocVarPtr;RecFldD:FieldDPtr);
      _asgncatfield:(Frml3:FrmlPtr;FD3:FileDPtr;CatIRec:word;CatFld:FieldDPtr);
      _asgnrecvar,_linkrec:(RecLV1,RecLV2:LocVarPtr;
             case byte of 0:(Ass:AssignDPtr); 1:(LinkLD:LinkDPtr));
      _asgnxnrecs:(xnrIdx:WKeyDPtr);
      _readrec,_writerec,
      _recallrec,_deleterec:(RecNr:FrmlPtr;AdUpd:boolean;
             case byte of 0:(LV:LocVarPtr;ByKey:boolean;Key:KeyDPtr;
                             CompOp:char);
                          1:(RecFD:FileDPtr{..dto..}));
      {_appendrec:(..RecFD);}
      _turncat:(NextGenFD:FileDPtr;
             FrstCatIRec,NCatIRecs:word; TCFrml:FrmlPtr);
      _sort:(SortFD:FileDPtr;SK:KeyFldDPtr);
      _edit:(EditFD:FileDPtr;EO:EditOptPtr);
      _report:(RO:RprtOptPtr);
      _edittxt,_printtxt:
            (TxtPath:StringPtr;TxtCatIRec:word; TxtLV:LocVarPtr;
            EdTxtMode:char;ExD:EdExitDPtr;
            WFlags:byte; TxtPos,TxtXY,ErrMsg:FrmlPtr;
            Ww:WRectFrml; Atr:FrmlPtr; Hd:FrmlPtr;
            Head,Last,CtrlLast,AltLast,ShiftLast:FrmlPtr);
      _puttxt:(TxtPath1:StringPtr;TxtCatIRec1:word;
            Txt:FrmlPtr;App:boolean);
      _releasedrive:(Drive:FrmlPtr);
      _mount:(MountCatIRec:word;MountNoCancel:boolean);
      _indexfile:(IndexFD:FileDPtr;Compress:boolean);
      _getindex:(giLV:LocVarPtr;giMode:char; {+,-,blank}
              giCond:FrmlPtr; { or RecNr-Frml }
              giKD:KeyDPtr;giKFlds:KeyFldDPtr;
              giKIRoot:KeyInDPtr;giSQLFilter:boolean;
              giOwnerTyp:char;giLD:LinkDPtr;giLV2:LocVarPtr);
      _window:(W:WRectFrml; Attr:FrmlPtr; WwInstr:InstrPtr; Top:FrmlPtr;
            WithWFlags:byte);
      _clrww:(W2:WRectFrml; Attr2,FillC:FrmlPtr);
      _forall:(CFD:FileDPtr; CKey:KeyDPtr; CVar,CRecVar:LocVarPtr;
            CKIRoot:KeyInDPtr; CBool{or SQLTxt}:FrmlPtr; CInstr:InstrPtr;
            CLD:LinkDPtr; CWIdx,inSQL,CSQLFilter,CProcent:boolean;
            COwnerTyp:char;CLV:LocVarPtr);
      _withshared,_withlocked,_withgraphics:
           (WDoInstr,WElseInstr:InstrPtr;WasElse:boolean;WLD:LockD);
      _graph:(GD:GraphDPtr);
      _putpixel,_line,_rectangle,_ellipse,_floodfill,_outtextxy:
           (Par1,Par2,Par3,Par4,Par5,Par6,Par7,Par8,Par9,Par10,Par11:FrmlPtr);
      _backup: (BrCatIRec:word; IsBackup,NoCompress,BrNoCancel:boolean);
      _backupm: (bmX:array[1..5] of byte;
                bmDir,bmMasks{backup only}:FrmlPtr;bmSubDir,bmOverwr:boolean);
      _closefds:(clFD:FileDPtr);
      _setedittxt:(Insert,Indent,Wrap,Just,ColBlk,Left,Right:FrmlPtr);
      _setmouse:(MouseX,MouseY,Show:FrmlPtr);
      _checkfile:(cfFD:FileDPtr;cfPath:StringPtr;cfCatIRec:word);
      _login:(liName,liPassWord:FrmlPtr);
      _sqlrdwrtxt:(TxtPath2:StringPtr;TxtCatIRec2:word;IsRead:boolean;
            sqlFD:FileDPtr;sqlKey:KeyDPtr;sqlFldD:FieldDPtr;sqlXStr:FrmlPtr);
      _portout:(IsWord,Port,PortWhat:FrmlPtr);
  end;

var
  OldMFlds,NewMFlds:ConstList;   { Merge + Report}
  IDA:array[1..9] of InpDPtr;
  MaxIi:integer;
  OldMXStr:XString;                  { Merge }
  OutpFDRoot:OutpFDPtr;
  OutpRDs:OutpRDPtr;
  Join:boolean;

  PrintView:boolean;                  { Report }
  Rprt:text;

  RprtHd,PageHd,PageFt:BlkDPtr;
  PFZeroLst:FloatPtrList;
  FrstLvM,LstLvM:LvDescrPtr; { LstLvM^.Ft=RF }
  SelQuest:boolean;
  PgeSizeZ,PgeLimitZ:FrmlPtr;
                                     { Edit }
  EditDRoot:EditDPtr;
  CompileFD,EditRdbMode:boolean;

  LVBD:LocVarBlkD;


const
  CalcTxt:string='';                     { calc }
  MergOpGroup:record Op:char;Group:float end=(Op:_const;Group:0.0);


procedure ResetLVBD;
procedure SetMyBP(Bp:ProcStkPtr);
procedure PushProcStk;
procedure PopProcStk;
function LockForAdd(FD:FileDPtr;Kind:word;Ta:boolean;var md:LockMode):boolean;
function RunAddUpdte(Kind:char;CRold:pointer;notLD:LinkDPtr):boolean;
function RunAddUpdte1(Kind:char;CRold:pointer;
               Back:boolean; StopAD:AddDPtr; notLD:LinkDPtr):boolean;

function TestExitKey(KeyCode:word;X:EdExitDPtr):boolean;
procedure SetCompileAll;


implementation

uses obaseww,oaccess,runfrml;

function EFldD.Ed(IsNewRec:boolean):boolean;
  begin Ed:=(FldD^.Flg and f_Stored<>0) and (EdU or IsNewRec and EdN) end;



procedure ResetLVBD;
begin
  FillChar(LVBD,sizeof(LVBD),0); LVBD.Size:=2*4;
end;
procedure SetMyBP(Bp:ProcStkPtr);
begin
  MyBP:=Bp; if MyBP<>nil then LVBD.Root:=MyBP^.LVRoot else LVBD.Root:=nil;
end;
procedure PushProcStk;
var ps:ProcStkPtr; lv:LocVarPtr;
begin
  ps:=GetZStore(LVBD.Size); ps^.ChainBack:=MyBP; MyBP:=ps;
  lv:=LVBD.Root; ps^.LVRoot:=lv;
  while lv<>nil do begin
    with lv^ do if (FTyp in ['R','S','B']) and (Init<>nil) then LVAssignFrml(lv,MyBP,false,Init);
    lv:=lv^.Chain end;
end;
procedure PopProcStk;
var lv:LocVarPtr;
begin
  lv:=MyBP^.LVRoot; while lv<>nil do begin
    if lv^.FTyp='S' then TWork.Delete(LongintPtr(LocVarAd(LV))^);
    lv:=lv^.Chain end;
  SetMyBP(MyBP^.ChainBack);
end;

function RunAddUpdte1(Kind:char{+,-,d}; CRold:pointer;
             Back{tracking}:boolean; StopAD:AddDPtr; notLD:LinkDPtr):boolean;
procedure CrIndRec;
  begin CreateRec(CFile^.NRecs+1); RecallRec(CFile^.NRecs) end;
function Link(AD:AddDPtr;var N:longint; var Kind2:char):boolean;
var CR:pointer; LD:LinkDPtr;
begin
  Link:=true; LD:=AD^.LD; Kind2:='d';
  if LD<>nil then
    if LinkUpw(LD,N,false) then exit else SetMsgPar(LD^.RoleName)
  else begin
    if not LinkLastRec(AD^.File2,N,false)
     {$ifdef FandSQL} and not CFile^.IsSQLFile {$endif} then
      begin IncNRecs(1); WriteRec(1) end;
    exit end;
  Kind2:='+';
  if (AD^.Create=2) or (AD^.Create=1) and PromptYN(132) then begin
{$ifdef FandSQL} if CFile^.IsSQLFile then Strm1^.InsertRec(false,true) else {$endif}
    begin  ClearDeletedFlag;
      if (LD<>nil) and (CFile^.Typ='X') then begin CrIndRec; N:=CFile^.NRecs end
      else CreateRec(N) end;
    exit end;
  WrLLF10Msg(119); Link:=false;
end;
function TransAdd(AD:AddDPtr;FD:FileDPtr;RP:pointer;CRnew:pointer;N:longint;Kind2:char;Back:boolean):boolean;
var CRold:pointer; x:XString; ld:LinkDPtr;
begin
  if CFile^.Add=nil then begin TransAdd:=true;exit end;
  if Kind2='+' then begin
    CRecPtr:=CRnew; TransAdd:=RunAddUpdte1('+',nil,Back,nil,nil); exit end;
  CRold:=GetRecSpace; CRecPtr:=CRold;
{$ifdef FandSQL} if CFile^.IsSQLFile then begin
   ld:=AD^.LD; if ld=nil then Strm1^.SelectXRec(nil,nil,_equ,false)
   else begin
     CFile:=FD; CRecPtr:=RP; x.PackKF(ld^.Args);
     CFile:=ld^.ToFD; CRecPtr:=CRold; Strm1^.SelectXRec(ld^.ToKey,@x,_equ,false) end end
  else {$endif}  ReadRec(N);
  CRecPtr:=CRnew; TransAdd:=RunAddUpdte1('d',CRold,Back,nil,nil);
  ReleaseStore(CRold);
end;
function Add(AD:AddDPtr;RP:pointer;R:float):boolean;
  begin Add:=true; CRecPtr:=RP; if Back then R:=-R;
        r_(AD^.Field,_r(AD^.Field)+R); if AD^.Chk=nil then exit;
        if not Back and not RunBool(AD^.Chk^.Bool) then
           begin SetMsgPar(RunShortStr(AD^.Chk^.TxtZ));
           WrLLF10Msg(110);Add:=false end  end;
procedure WrUpdRec(AD:AddDPtr;FD:FileDPtr;RP,CRnew:pointer;N:longint);
var x:XString; ld:LinkDPtr;
begin
   CRecPtr:=CRnew;
{$ifdef FandSQL} if CFile^.IsSQLFile then begin
   ld:=AD^.LD; if ld=nil then Strm1^.UpdateXFld(nil,nil,AD^.Field)
   else begin
     CFile:=FD; CRecPtr:=RP; x.PackKF(ld^.Args);
     CFile:=ld^.ToFD; CRecPtr:=CRnew; Strm1^.UpdateXFld(ld^.ToKey,@x,AD^.Field) end end
  else {$endif}  WriteRec(N);
end;
function Assign(AD:AddDPtr):boolean;
  var R:float; S:LongStrPtr; ss:string; B:boolean;
      F:FieldDPtr; Z:FrmlPtr; Pos,N2:longint; Kind2:char;
  begin Assign:=true;if not RunBool(AD^.Bool) then exit;
      F:=AD^.Field; Z:=AD^.Frml;
      case F^.FrmlTyp of
       'R':R:=RunReal(Z);
       'S':if F^.Typ='T' then S:=RunLongStr(Z) else ss:=RunShortStr(Z);
       else B:=RunBool(Z) end;
      if not Link(AD,N2,Kind2) then begin Assign:=false;exit end;
      case F^.FrmlTyp of
       'R':r_(F,R);
       'S':if F^.Typ='T' then longs_(F,S) else s_(F,ss);
        else b_(F,B) end;
      WriteRec(N2) end;
  var AD,ADback:AddDPtr; N2,N2old:longint;   Kind2,Kind2old:char;
      CF,CF2,CR,CR2,CR2old,p:pointer; R,Rold:float; b:boolean;
  label 1,fail;
  begin RunAddUpdte1:=true;
      AD:=CFile^.Add; CF:=CFile; CR:=CRecPtr; MarkStore(p); ADback:=nil;
      while AD<>nil do
      begin if AD=StopAD then begin ReleaseStore(p);exit end;
         if (notLD<>nil) and (AD^.LD=notLD) then goto 1;
         if AD^.Assign then if Assign(AD) then goto 1 else goto fail;
         R:=RunReal(AD^.Frml); if Kind='-' then R:=-R;  Rold:=0;
         if Kind='d' then
            begin CRecPtr:=CRold; Rold:=RunReal(AD^.Frml) end;
         ADback:=AD; CF2:=AD^.File2; N2:=0; N2old:=0;
         if R<>0 then
            begin CRecPtr:=CR;
                  if not Link(AD,N2,Kind2) then goto fail;  CR2:=CRecPtr end;
         if Rold<>0 then
            begin CFile:=CF; CRecPtr:=CRold;
                  if not Link(AD,N2old,Kind2old) then goto fail;  CR2old:=CRecPtr;
                  if N2old=N2 then
                     begin R:=R-Rold; if R=0 then goto 1; N2old:=0 end end;
         if (N2=0) and (N2old=0) then goto 1;
         CFile:=CF2;
         if N2old<>0 then
            begin if not Add(AD,CR2old,-Rold) then goto fail end;
         if N2<>0 then
            begin if not Add(AD,CR2,R) then goto fail end;
         if (N2old<>0)
            and not TransAdd(AD,CF,CR,CR2old,N2old,Kind2Old,false) then goto fail;
         if (N2<>0)
            and not TransAdd(AD,CF,CR,CR2,N2,Kind2,false) then
            begin if N2old<>0 then b:=TransAdd(AD,CF,CR,CR2old,N2Old,Kind2Old,true);
                  goto fail end;
         if N2old<>0 then WrUpdRec(AD,CF,CR,CR2old,N2old);
         if N2<>0 then WrUpdRec(AD,CF,CR,CR2,N2);
       1:ReleaseStore(p); CFile:=CF; CRecPtr:=CR; AD:=AD^.Chain;
      end;
      exit;
 fail:ReleaseStore(p); CFile:=CF; CRecPtr:=CR; RunAddUpdte1:=false;
      if ADback<>nil then b:=RunAddUpdte1(Kind,CRold,true,ADback,notLD);  { backtracking }
  end;
function LockForAdd(FD:FileDPtr;Kind:word;Ta:boolean;var md:LockMode):boolean;
var AD:AddDPtr; md1:LockMode; {0-ExLMode,1-lock,2-unlock}
begin
  LockForAdd:=false; CFile:=FD; AD:=FD^.Add; while AD<>nil do begin
    with AD^ do if CFile<>File2 then begin
      CFile:=File2; case Kind of
        0: if Ta then CFile^.TaLMode:=CFile^.LMode else CFile^.ExLMode:=CFile^.LMode;
        1: begin  md:=WrMode; if Create>0 then md:=CrMode;
           if not TryLMode(md,md1,2) then exit end;
        2: if Ta then OldLMode(CFile^.TaLMode) else OldLMode(CFile^.ExLMode) end;
      if not LockForAdd(CFile,Kind,Ta,md) then exit end;
      AD:=AD^.Chain end;
  LockForAdd:=true;
end;
function RunAddUpdte(Kind:char;CRold:pointer;notLD:LinkDPtr):boolean;
var CF:FileDPtr; b:boolean; w:longint; md:LockMode;
begin
  CF:=CFile; LockForAdd(CF,0,false,md);
  while not LockForAdd(CF,1,false,md) do begin
    SetCPathVol; Set2MsgPar(CPath,LockModeTxt[md]);
    LockForAdd(CF,2,false,md); w:=PushWrLLMsg(825,false);
    KbdTimer(spec.NetDelay,0); if w<>0 then PopW(w) end;
  CFile:=CF; b:=RunAddUpdte1(Kind,CRold,false,nil,notLD); RunAddUpdte:=b;
  LockForAdd(CF,2,false,md); CFile:=CF;
end;

function TestExitKey(KeyCode:word;X:EdExitDPtr):boolean;
var E:EdExKeyDPtr;
begin
  E:=X^.Keys; while E<>nil do begin
    if KeyCode=E^.KeyCode then begin
      TestExitKey:=true; EdBreak:=E^.Break; exit end;
    E:=E^.Chain end;
  TestExitKey:=false;
end;

procedure SetCompileAll;
begin with ChptTF^ do begin
  CompileAll:=true; TimeStmp:=today+currtime; SetUpdHandle(Handle);
end end;

end.

