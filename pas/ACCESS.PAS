{$I Switches}
{$A-,B-,F+,G-,I+,O-,R-,S+,V+,X+,L+}
unit Access;

interface

uses dos,
{$ifdef FandGraph} graph, {$endif}
     drivers,base;


                { COMMON DECLARATION }

const
  LeftJust=1; {RightJust=0  coded in M for Typ='N','A'}
  Ascend=0;Descend=6;  {used in SortKey}
  XPageSize=1024; XPageOverHead=7; MaxIndexLen=123 { min.4 items };
  oLeaf=3; oNotLeaf=7;
  f_Stored=1; f_Encryp=2; f_Mask=4; f_Comma=8;          {FieldD flags}
type
  LockMode=(NullMode,NoExclMode,NoDelMode,NoCrMode,RdMode,
            WrMode,CrMode,DelMode,ExclMode);
const LockModeTxt:array[LockMode]of string[6]=
  ('NULL','NOEXCL','NODEL','NOCR','RD','WR','CR','DEL','EXCL');
const
    MPageSize=512; XPageShft=10; MPageShft=9;
type
  FrmlPtr=^FrmlElem;
  FileDPtr=^FileD;
  FieldDPtr=^FieldDescr;
  FieldList=^FieldListEl;
  FieldListEl=record Chain:FieldList; FldD:FieldDPtr end;
  FrmlList=^FrmlListEl;
  FrmlListEl=record Chain:FrmlList;
                    Frml:FrmlPtr;
             end;
  StringList=^StringListEl;
  StringListEl=record Chain:StringList;
                      S:string end;
  FloatPtrList=^FloatPtrListEl;
  FloatPtrListEl=record Chain:FloatPtrList;
                         RPtr:FloatPtr;
                  end;
  LinkDPtr=^LinkD;
  FuncDPtr=^FuncD;
  KeyDPtr=^XKey;
  WKeyDPtr=^XWKey;
  KeyList=^KeyListEl;
  KeyListEl = record Chain:KeyList; Key:KeyDPtr end;
  LocVarPtr=^LocVar;
  FrmlElem=record Op:char;
                  case byte of
                   0:(P1,P2,P3,P4,P5,P6:FrmlPtr;Delim:char);
                   1:(N01,N02,N03,N04,
                      N11,N12,N13,N14,
                      N21,N22,N23,N24,
                      N31:byte);
                   1:(W01,W02,W11,W12,W21,W22:word);
                   2:(R:float);
                   4:(S:string);
                   5:(B:boolean);
                   6:(PP1:FrmlPtr;Mask:ScreenStr);
                   7:(Field:FieldDPtr);                         {_field}
                   7:(P011:FrmlPtr;File2:FileDPtr;LD:LinkDPtr);
                       {LD=nil for param}   {_access}
                       {LD=RecPtr}          {_recvarfld}
                   8:(Frml:FrmlPtr;NewFile:FileDPtr;NewRP:pointer); {_newfile}
                   9:(FD:FileDPtr);             {_lastupdate,_generation}
                  10:(CatIRec:word;CatFld:FieldDPtr); {_catfield}
                  11:(PPP1,PP2:FrmlPtr;FldD:FieldDPtr);       {_prompt}
                  12:(PPPP1,PPP2,PP3:FrmlPtr;Options:ScreenStr);{_pos,_replace}
                  13:(FFD:FileDPtr;Key:KeyDPtr;Arg:array[1..2]of FrmlPtr);
                                 {_recno/typ='R' or'S'/,_recnoabs,_recnolog}
                  14:(PPPPP1:FrmlPtr;RecFD:FileDPtr;RecFldD:FieldDPtr);
                                                 {_accrecno,_isdeleted}
                  15:(LinkLD:LinkDPtr; case LinkFromRec:boolean of
                       true:(LinkLV:LocVarPtr);
                       false:(LinkRecFrml:FrmlPtr));      {_link}
                  16:(PPPPPP1,PPPP2:FrmlPtr;TxtPath:StringPtr;TxtCatIRec:word);
                                                        {_gettxt,_filesize}
                  18:(BPOfs:word);                      { _getlocvar }
                  19:(FC:FuncDPtr;FrmlL:FrmlList);      { _userfunc }
                  20:(LV:LocVarPtr;PackKey:KeyDPtr);    { _keyof,_lvdeleted }
                  21:(EvalP1:FrmlPtr; EvalTyp:char; EvalFD:FileDPtr); {_eval}
                  22:(WKey:WKeyDPtr);  {_indexnrecs}
                  23:(ownBool,ownSum:FrmlPtr;ownLD:LinkDPtr); { _owned }
           end;
  KeyInDPtr=^KeyInD;
  KeyInD=record Chain:KeyInDPtr;
                FL1,FL2:FrmlList; {FL2=nil only 1 value, else intervall}
                XNrBeg,N:longint;   { set by xscan }
                X1,X2:StringPtr;
         end;
  SumElPtr=^SumElem;
  SumElem=record Chain:SumElPtr;
                 Op:char;
                 R:float;
                 Frml:FrmlPtr;
          end;
  FieldDescr=record Chain:FieldDPtr;
                    Typ,FrmlTyp:char;
                    L,M,NBytes,Flg:byte;
                    case boolean {Stored} of
                     True :(Displ:integer);
                     False:(Frml:FrmlPtr;
                            Name:string[1] {curr.length});
              end;
  KeyFldDPtr=^KeyFldD;
  KeyFldD=record Chain:KeyFldDPtr;
                 FldD:FieldDPtr;
                 CompLex,Descend:boolean end;
  RdbDPtr=^RdbD;
  RdbPos=record R:RdbDPtr;
                IRec:word end;
  ChkDPtr=^ChkD;
  ChkD=record Chain:ChkDPtr;
              Bool:FrmlPtr;
              HelpName:StringPtr;
              TxtZ:FrmlPtr;
              Warning:boolean;
       end;
  DepDPtr = ^DepD;
  DepD = record
    Chain:DepDPtr; Bool,Frml:FrmlPtr;
  end;
  ImplDPtr = ^ImplD;
  ImplD = record
    Chain:ImplDPtr; FldD:FieldDPtr; Frml:FrmlPtr;
  end;
  LiRootsPtr = ^LiRoots;
  LiRoots = record
    Chks:ChkDPtr; Impls:ImplDPtr;
  end;
  AddDPtr=^AddD;
  AddD=record Chain:AddDPtr;
              Field:FieldDPtr;
              File2:FileDPtr;
              LD:LinkDPtr;
              Create:byte; {0- no,1-!,2-!!}
              Frml:FrmlPtr;
              case Assign:boolean of
               true :(Bool:FrmlPtr);
               false:(Chk:ChkDPtr);
       end;
  PwCodeArr=array[1..20] of char;
  TFilePtr = ^TFile;
  TFile = object
    Handle:word;
    FreePart:longint;
    Reserved,CompileProc,CompileAll:boolean;
    IRec:word;
    FreeRoot,MaxPage:longint;
    TimeStmp:real;
    LicenseNr:integer;
    MLen:longint;
    PwCode,Pw2Code:PwCodeArr;
    Format:(T00Format,DbtFormat,FptFormat);
    BlockSize:word; {FptFormat}
    IsWork:boolean;
    procedure Err(n:word;ex:boolean);
    procedure TestErr;
    function UsedFileSize:longint;
    function NotCached:boolean;
    procedure RdPrefix(Chk:boolean);
    procedure WrPrefix;
    procedure SetEmpty;
    procedure Create;
    function NewPage(NegL:boolean):longint;
    procedure ReleasePage(PosPg:longint);
    procedure Delete(Pos:longint);
    function Read(StackNr:word;Pos:longint):LongStrPtr;
    function Store(S:LongStrPtr):longint;
  private
    procedure RdWr(ReadOp:boolean;Pos:longint;N:word;var X);
  end;
  XFilePtr = ^XFile;
  FileD = object
    Chain:FileDPtr;
    RecLen:word;
    RecPtr:pointer;
    NRecs:longint;
    WasWrRec,WasRdOnly,EOF:boolean;
    Typ:char;        { 8=Fand 8;6=Fand 16;X= .X; 0=RDB; C=CAT }
    Handle:word;
    IRec:longint;
    FrstDispl:word;
    TF:TFilePtr;
    ChptPos:RdbPos;     {zero for Rdb and FD translated from string }
    TxtPosUDLI:word;    {=0 if not present}
    OrigFD:FileDPtr;    { like orig. or nil}
    Drive:byte;         {1=A:,2=B:,else 0}
    CatIRec:word;
    FldD:FieldDPtr;
    IsParFile,IsJournal,IsHlpFile,typSQLFile,IsSQLFile,IsDynFile:boolean;
    UMode:FileUseMode;
    LMode,ExLMode,TaLMode:LockMode;
    ViewNames:StringList;  {after each string: byte string with user codes }
    XF:XFilePtr;
    Keys:KeyDPtr;
    Add:AddDPtr;
    nLDs,LiOfs:word;
    Name:NameStr;
    function UsedFileSize:longint;
    function IsShared:boolean;
    function NotCached:boolean;
    function GetNrKeys:word;
  end;
  DBaseFld = record
    Name:array[0..10]of char;
    Typ:char; Displ:longint;
    Len,Dec:byte; x2:array[1..14]of byte;
  end;
  DBaseHd = record
    Ver:byte;
    Date:array[1..3]of byte;
    NRecs:longint;
    HdLen,RecLen:word; x:array[1..20]of byte;
    Flds:array[1..1] of DBaseFld;
  end;
  LinkD=record Chain:LinkDPtr;
               IndexRoot:word;
               MemberRef:byte; { 0-no, 1-!, 2-!!(no delete)}
               Args:KeyFldDPtr;
               FromFD,ToFD:FileDPtr;
               ToKey:KeyDPtr;
               RoleName:string[1];
        end;
  LocVarBlkD = record
    Root:LocVarPtr;
    NParam,Size:word;
  end;
  FuncD=record Chain:FuncDPtr;
               FTyp:char;
               LVB:LocVarBlkD; {1.LV is result}
               Instr:pointer; {InstrPtr}
               Name:string[1];
        end;
  LocVar=record Chain:LocVarPtr; IsPar:boolean;
                case FTyp:char of
                 'r','f','i':(FD:FileDPtr;RecPtr:pointer{KeyDPtr};Name:string[1]);
                 'S','R','B':(Op:char;BPOfs:word;IsRetPar:boolean;Init:FrmlPtr {..Name} );
         end;
  RdbD=record ChainBack:RdbDPtr;
              FD,HelpFD:FileDPtr;  { FD=FileDRoot and =Chpt for this RDB }
              OldLDRoot:LinkDPtr;
              OldFCRoot:FuncDPtr;
              Mark2:pointer;   { markstore2 at beginning }
              Encrypted:boolean;
              RdbDir,DataDir:DirStr;
       end;
  WRectFrml= record C1,R1,C2,R2:FrmlPtr end;
  XStringPtr = ^XString;
  XString = object
    S:string255;
    procedure Clear;
    procedure StoreReal(R:real; KF:KeyFldDPtr);
    procedure StoreStr(V:string; KF:KeyFldDPtr);
    procedure StoreBool(B:boolean; KF:KeyFldDPtr);
    procedure StoreKF(KF:KeyFldDPtr);
    procedure PackKF(KF:KeyFldDPtr);
    function PackFrml(FL:FrmlList; KF:KeyFldDPtr):boolean;
{$ifdef FandSQL}
    procedure GetF(Off,Len:word; Descend:boolean; var Buf);
    procedure GetD(Off:word; Descend:boolean; var R);
    procedure GetN(Off,Len:word; Descend:boolean; var Buf);
    function GetA(Off,Len:word; CompLex,Descend:boolean; var Buf):word;
{$endif}
   private
    procedure StoreD(var R; Descend:boolean);
    procedure StoreN(var N;Len:word; Descend:boolean);
    procedure StoreF(var F;Len:word; Descend:boolean);
    procedure StoreA(var A;Len:word; CompLex,Descend:boolean);
  end;
  XItemPtr=^XItem;
  XItem = object
    Nr:array[1..3] of byte; {NN: RecNr /on leaf/ or NumberofRecordsBelow}
    DownPage:longint; {not on leaf}
    {M:byte  number of equal bytes /not stored bytes/ }
    {Index:string  /L=length, A:area ptr/}
    function GetN:longint;
    procedure PutN(N:longint);
    function GetM(O:word):word;
    procedure PutM(O:word;M:word);
    function GetL(O:word):word;
    procedure PutL(O:word;L:word);
    function Next(O:word):XItemPtr;
    function UpdStr(O:word;S:StringPtr):word;
  end;
  XPagePtr=^XPage;
  XPage = object
    IsLeaf:boolean;
    GreaterPage:longint;  { or free pages chaining }
    NItems:word;
    A:array[1..XPageSize-4]of byte;            { item array }
    function Off:word;
    function XI(I:word):XItemPtr;
    function EndOff:word;
    function Underflow:boolean;
    function Overflow:boolean;
    function StrI(I:word):string;
    function SumN:longint;
    procedure Insert(I:word;var SS; var XX:XItemPtr);
    procedure InsDownIndex(I:word;Page:longint;P:XPagePtr);
    procedure Delete(I:word);
    procedure AddPage(P:XPagePtr);
    procedure SplitPage(P:XPagePtr;ThisPage:longint);
  end;
  XWFilePtr = ^XWFile;
  XKey = object
    Chain:KeyDPtr;
    KFlds:KeyFldDPtr;
    Intervaltest,Duplic,InWork:boolean;
    IndexRoot:word; IndexLen:byte;
    NR:longint;    {used only by XWKey}
    Alias:StringPtr;
    function XF:XWFilePtr;
    function NRecs:longint;
    function Search(var XX:XString;AfterEqu:boolean;var RecNr:longint):boolean;
    function SearchIntvl(var XX:XString;AfterEqu:boolean;var RecNr:longint):boolean;
    function PathToNr:longint;
    procedure NrToPath(I:longint);
    function PathToRecNr:longint;
    function RecNrToPath(var XX:XString;RecNr:longint):boolean;
    function NrToRecNr(I:longint):longint;
    function NrToStr(I:longint):string;
    function RecNrToNr(RecNr:longint):longint;
    function FindNr(var X:XString;var IndexNr:longint):boolean;
    procedure InsertOnPath(var XX:XString;RecNr:longint);
    function Insert(RecNr:longint;Try:boolean):boolean;
    procedure DeleteOnPath;
    function Delete(RecNr:longint):boolean;
  end;
  XWKey = object(XKey)
    procedure Open(KF:KeyFldDPtr;Dupl,Intvl:boolean);
    procedure Close;
    procedure Release;
    procedure ReleaseTree(Page:longint;IsClose:boolean);
    procedure OneRecIdx(KF:KeyFldDPtr;N:longint);
    procedure InsertAtNr(I,RecNr:longint);
    function InsertGetNr(RecNr:longint):longint;
    procedure DeleteAtNr(I:longint);
    procedure AddToRecNr(RecNr:longint;Dif:integer);
  end;
  XWFile = object
    UpdLockCnt,Handle:word;
    FreeRoot,MaxPage:longint;
    procedure Err(N:word);
    procedure TestErr;
    function UsedFileSize:longint;
    function NotCached:boolean;
    procedure RdPage(P:XPagePtr;N:longint);
    procedure WrPage(P:XPagePtr;N:longint);
    function NewPage(P:XPagePtr):longint;
    procedure ReleasePage(P:XPagePtr;N:longint);
  end;
  XFile = object(XWFile)
    NRecs,NRecsAbs:longint;  {FreeRoot..NrKeys read/written by 1 instr.}
    NotValid:boolean;
    NrKeys:byte;
    NoCreate,FirstDupl:boolean;
    procedure SetEmpty;
    procedure RdPrefix;
    procedure WrPrefix;
    procedure SetNotValid;
  end;
  XScanPtr = ^XScan;
  XScan = object(TObject)
    FD:FileDPtr;
    Key:KeyDPtr;
    Bool:FrmlPtr;
    Kind:byte;
    NRecs,IRec,RecNr:longint;
    hasSQLFilter,EOF:boolean;
    constructor Init(aFD:FileDPtr;aKey:KeyDPtr;aKIRoot:KeyInDPtr;aWithT:boolean);
    procedure Reset(ABool:FrmlPtr;SQLFilter:boolean);
    procedure ResetSort(aSK:KeyFldDPtr;var BoolZ:FrmlPtr;OldMd:LockMode;SQLFilter:boolean);
    procedure SubstWIndex(WK:WKeyDPtr);
    procedure ResetOwner(XX:XStringPtr;aBool:FrmlPtr);
    procedure ResetOwnerIndex(LD:LinkDPtr;LV:LocVarPtr;aBool:FrmlPtr);
    {$ifdef FandSQL}
    procedure ResetSQLTxt(Z:FrmlPtr);                    {$endif}
    procedure ResetLV(aRP:pointer);
    procedure Close;
    procedure SeekRec(I:longint);
    procedure GetRec;
  private
    KIRoot:KeyInDPtr;
    OwnerLV:LocVarPtr;
    SK:KeyFldDPtr;
    X:XItemPtr;
    P:XPagePtr;
    NOnPg:word;
    KI:KeyInDPtr;
    NOfKI,iOKey:longint;
    TempWX,NotFrst,withT:boolean;
    Strm:pointer; {SQLStreamPtr or LVRecPtr}
    procedure SeekOnKI(I:longint);
    procedure SeekOnPage(Page:longint;I:word);
    procedure NextIntvl;
  end;
  CompInpDPtr = ^CompInpD;
  CompInpD = record
    ChainBack: CompInpDPtr;
    InpArrPtr:CharArrPtr;
    InpRdbPos:RdbPos;
    InpArrLen,CurrPos,OldErrPos:word
  end;
 {#$0...#$5f = 0-ary, #$60...#$af = 1-ary, #$b0...#$ef = 2-ary, #$f0..=3-ary}
const _equ=#1;_lt=#2;_le=#3;_gt=#4;_ge=#5;_ne=#6;   {lexema}
      _subrange=#$7;_number=#$8;_assign=#$9;_identifier=#$A;
      _addass=#$B;_quotedstr=#$C;
      _const=#$10{float/string/boolean};             {0-ary instructions}
      _field=#$11{fieldD}; _getlocvar=#$12{BPOfs};
      _access=#$13{fieldD or nil for exist,newfileD,linkD or nil};
      _recvarfld=#$14;{fieldD,fileD,recptr}
      _today=#$18;_currtime=#$19;_pi=#$1A;_random=#$1B;
      _exitcode=#$1d;_edrecno=#$1e;_getwordvar=#$1f;{n:0..}
      _memavail=#$22;_maxcol=#$23;_maxrow=#$24;
      _getmaxx=#$25;_getmaxy=#$26;
      _lastupdate=#$27;_nrecs=#$28;_nrecsabs=#$29; {FD}
      _generation=#$2a; {FD}
      _recno=#$2b;_recnoabs=#$2c;_recnolog=#$2d;{FD,K,Z1,Z2,...}
      _filesize=#$2e;{txtpath,txtcatirec}
      _txtpos=#$2f;_cprinter=#$30;_mousex=#$31;_mousey=#$32;
      _txtxy=#$33;_indexnrecs=#$34;_owned=#$35;{bool,sum,ld}        {R}
      _catfield=#$36;{CatIRec,CatFld}
      _password=#$37;_version=#$38;_username=#$39;_edfield=#$3a;
      _accright=#$3b;_readkey=#$3c;_edreckey=#$3d;_edbool=#$3e;
      _edfile=#$3f;_edkey=#$40;_clipbd=#$41;_keybuf=#$42;
      _keyof=#$43;{LV,KeyD}                                          {S}
      _edupdated=#$44;_keypressed=#$45;_escprompt=#$46;
      _trust=#$47;{bytestring}_lvdeleted=#$48;{LV}                   {B}
      _userfunc=#$49;{fc,frmllist}
      _isnewrec=#$4a;_mouseevent=#$4b;_ismouse=#$4c;{what,button-masks}
      _testmode=#$4d;
      _newfile=#$60{newfile,newRP};                   {1-ary instructions}
      _lneg=#$61;
      _inreal=#$62{precision,constlst}; _instr=#$63{tilda,constlst};
      _isdeleted=#$64;{RecFD}_setmybp=#$65;
      _modulo=#$66{length,modulo,weight1,...};                       {B}
      _getpath=#$68;_upcase=#$69;_lowcase=#$6A;
      _leadchar=#$6B;_getenv=#$6C;
      _trailchar=#$6D;{char}_strdate=#$6E{maskstring};
      _nodiakr=#$6F;                                                 {S}
      _char=#$70;_sqlfun=#$71;                                      {SR}
      _unminus=#$73;_abs=#$74;_int=#$75;_frac=#$76;_sqr=#$77;
      _sqrt=#$78;_sin=#$79;_cos=#$7A;_arctan=#$7b;_ln=#$7c;
      _exp=#$7d;_typeday=#$7e;_color=#$7f;
      _link=#$90{LD};                                                {R}
      _val=#$91;_valdate=#$92{maskstring};_length=#$93;_linecnt=#$94;
      _diskfree=#$95;_ord=#$96;_eval=#$97{Typ};                      {RS}
      _accrecno=#$98{FD,FldD};                                    {R,S,B}
      _promptyn=#$99;                                                {BS}
      _conv=#$a0;   { used in Prolog}
      _and=#$b1;_or=#$b2;_limpl=#$b3;_lequ=#$b4;     {2-ary instructions}
      _compreal=#$b5{compop,precision};_compstr=#$b6{compop,tilda};  {B}
      _concat=#$c0;                                                  {S}
      _repeatstr=#$c2;                                               {SSR}
      _gettxt=#$c3; {txtpath,txtcatirec}                             {SRR}
      _plus=#$c4;_minus=#$c5;_times=#$c6;_divide=#$c7;
      _div=#$c8;_mod=#$c9;_round=#$ca;
      _addwdays=#$cb;_difwdays=#$cc;{typday}
      _addmonth=#$cd;_difmonth=#$ce;_inttsr=#$cf{ptr};
      _min=#$d0; _max=#$d1; {used in Prolog}                         {R}
      _equmask=#$d2;                                               {BSS}
      _prompt=#$d3{fieldD};                                      {R,S,B}
      _portin=#$d4;                                                {RBR}
      _cond=#$f0{bool or nil,frml,continue or nil}; {3-ary instructions}
      _copy=#$f1;_str=#$f2;                                          {S}
      _selectstr=#$f3;_copyline=#$f4;                                {SSRR}
      _pos=#$f5{options};_replace=#$f6{options};                     {RSSR}
      _mousein=#$f7{P4};

var FileDRoot:FileDPtr;                         { only current RDB }
    LinkDRoot:LinkDPtr; FuncDRoot:FuncDPtr;     { for all RDBs     }
    CFile:FileDPtr;
    CRecPtr:pointer;
    CViewKey:KeyDPtr;
    TopRdbDir,TopDataDir:DirStr; CatFDName:NameStr;
    CRdb,TopRdb:RdbDPtr;
    CatFD,HelpFD:FileDPtr;

var XPath:array[1..10]of record Page:longint;I:word end; XPathN:word;
    XWork:XWFile;
    TWork:TFile;
const ClpBdPos:longint=0;
    IsTestRun:boolean=false;IsInstallRun:boolean=false;

var Chpt:FileDPtr absolute FileDRoot; ChptTF:TFilePtr;
    ChptTxtPos,
    ChptVerif,            { updated record }
    ChptOldTxt:FieldDPtr; { ChptTyp='F' : -1 = new unchecked record,
                                else = old declaration }
    ChptTyp,ChptName,ChptTxt:FieldDPtr;

const
  FloppyDrives=3;
  EscPrompt:boolean=false;
  UserName:string[20]='';
  UserPassword:string[20]='';
  AccRight:string[255]=#0;
  EdUpdated:boolean=false;
  EdRecNo:longint=0;
  EdRecKey:string='';
  EdKey:string[32]='';
  EdOk:boolean=false; EdField:string[32]='';
  LastTxtPos:longint=0;
  TxtXY:longint=0;
            { consecutive word-sized /for formula access/}
  RprtLine:word=0; RprtPage:word=0; PgeLimit:word=0;  {report}
  EdBreak:word=0; EdIRec:word=1;             {common - alphabetical order}
  MenuX:word=1; MenuY:word=1;
  UserCode:word=0;
var
  WordVarArr:array[0..8] of word absolute RprtLine;
  CatRdbName,CatFileName,CatArchiv,CatPathName,CatVolume:FieldDptr;
  MountedVol:array[1..FloppyDrives] of VolStr; {1=A,2=B,3=CPMDrive}
const
  SQLDateMask:string[30]='DD.MM.YYYY hh:mm:ss';

function FieldDMask(F:FieldDPtr):StringPtr;

                      { COMPARE FUNCTIONS }

const Power10:array[0..20] of float=(1E0,1E1,1E2,1E3,1E4,1E5,1E6,1E7,1E8,1E9,
         1E10,1E11,1E12,1E13,1E14,1E15,1E16,1E17,1E18,1E19,1E20);

procedure RunErrorM(Md:LockMode;N:word);
 function CompLongStr(S1,S2:LongStrPtr):integer;
  inline($8C/$DA/           {MOV    DX,DS}
         $5F/$07/$5E/$1F/   {POP    DI/ES/SI/DS}
         $8B/$0C/           {MOV    CX,[SI]}
         $26/$3B/$0D/       {CMP    CX,ES:[DI]}
         $B8/$01/$00/       {MOV    AX,0001    = }
         $74/$0B/           {JZ     l1}
         $B8/$02/$00/       {MOV    AX,0002    < }
         $72/$06/           {JC     l1}
         $B8/$04/$00/       {MOV    AX,0004    > }
         $26/$8B/$0D/       {MOV    CX,ES:[DI]}
         $83/$F9/$00/    {l1 CMP    CX,0}
         $74/$11/           {JZ     l2}
         $46/$46/$47/$47/   {INC    SI/SI/DI/DI}
         $FC/               {CLD}
         $F3/$A6/           {REP    CMPSB}
         $74/$08/           {JZ     l2}
         $B8/$04/$00/       {MOV    AX,0004     > }
         $73/$03/           {JNC    l2}
         $B8/$02/$00/       {MOV    AX,0002     < }
         $8E/$DA         {l2 MOV    DS,DX});
 function CompStr(S1,S2:string):integer;
 function CompLongShortStr(S1:LongStrPtr;S2:StringPtr):integer;
  inline($8C/$DA/           {MOV    DX,DS}
         $5F/$07/$5E/$1F/   {POP    DI/ES/SI/DS}
         $8B/$0C/           {MOV    CX,[SI]}
         $81/$F9/$FF/$00/   {CMP    CX,00FF}
         $76/$03/           {JNA    l1}
         $B9/$FF/$00/       {MOV    CX,00FF}
         $26/$3A/$0D/    {l1 CMP    CL,ES:[DI]}
         $B8/$01/$00/       {MOV    AX,0001    = }
         $74/$0B/           {JZ     l2}
         $B8/$02/$00/       {MOV    AX,0002    < }
         $72/$06/           {JC     l2}
         $B8/$04/$00/       {MOV    AX,0004    > }
         $26/$8A/$0D/       {MOV    CL,ES:[DI]}
         $83/$F9/$00/    {l2 CMP    CX,0}
         $74/$10/           {JZ     l3}
         $46/$46/$47/       {INC    SI/SI/DI}
         $FC/               {CLD}
         $F3/$A6/           {REP    CMPSB}
         $74/$08/           {JZ     l3}
         $B8/$04/$00/       {MOV    AX,0004     > }
         $73/$03/           {JNC    l3}
         $B8/$02/$00/       {MOV    AX,0002     < }
         $8E/$DA         {l3 MOV    DS,DX});   
 function CompArea(A,B:pointer;L:integer):integer;
     inline($8C/$DA/               {MOV DX,DS}
            $59/$5F/$07/$5E/$1F/   {POP CX/DI/ES/SI/DS}
            $FC/                   {CLD}
            $F3/$A6/               {REPE CMPSB}
            $B8/>1/                {MOV AX,1}
            $74/$08/               {JE  lab1}
            $B8/>2/                {MOV AX,2}
            $72/$03/               {JC  lab1}
            $B8/>4/                {MOV AX,4}
            $8E/$DA           {lab1 MOV DS,DX});
 function CompLexLongStr(S1,S2:LongStrPtr):word;
 function CompLexLongShortStr(S1:LongStrPtr;S2:StringPtr):word;
 function CompLexStr(const S1,S2:string):word;
 function EquKFlds(KF1,KF2:KeyFldDPtr):boolean;
procedure Code(var A; L:word);
procedure CodingLongStr(S:LongStrPtr);
procedure UnPack(var PackArr;var NumArr;NoDigits:word);
procedure Pack(var NumArr;var PackArr;NoDigits:word);
function RealFromFix(var FixNo;FLen:word):float;
procedure FixFromReal(r:float;var FixNo;flen:word);

function LocVarAd(LV:LocVarPtr):pointer;


                       { FILE MANAGEMENT }

function GetRecSpace:pointer;
function GetRecSpace2:pointer;
function CFileRecSize:word;
procedure SetTWorkFlag;
procedure ClearTWorkFlag;
 function HasTWorkFlag:boolean;
procedure SetUpdFlag;
procedure ClearUpdFlag;
 function HasUpdFlag:boolean;
 function DeletedFlag:boolean;
procedure ClearDeletedFlag;
procedure SetDeletedFlag;
procedure ClearCacheCFile;
procedure CloseClearHCFile;
 function ChangeLMode(Mode:LockMode;Kind:word;RdPref:boolean):boolean;
 function TryLMode(Mode:LockMode;var OldMode:LockMode;Kind:word):boolean;
 function NewLMode(Mode:LockMode):LockMode;
procedure OldLMode(Mode:LockMode);
 function TryLockN(N:longint;Kind:word):boolean;
procedure UnLockN(N:longint);
procedure CloseGoExit;
procedure TestCFileError;
procedure TestCPathError;
procedure CExtToT;
procedure CExtToX;
 function RdPrefix:word;
procedure RdPrefixes;
procedure WrPrefix;
procedure WrPrefixes;
procedure XFNotValid;


procedure TestXFExist;
 function XNRecs(K:KeyDPtr):longint;
procedure TryInsertAllIndexes(RecNr:longint);
procedure RecallRec(RecNr:longint);
procedure DeleteAllIndexes(RecNr:longint);
procedure DeleteXRec(RecNr:longint;DelT:boolean);
procedure OverwrXRec(RecNr:longint;P2,P:pointer);


procedure IncNRecs(N:longint);
procedure DecNRecs(N:longint);
procedure SeekRec(N:longint);
procedure PutRec;
procedure ReadRec(N:longint);
procedure WriteRec(N:longint);
procedure CreateRec(N:longint);
procedure DeleteRec(N:longint);
function LinkLastRec(FD:FileDPtr;var N:longint;WithT:boolean):boolean;
procedure AsgnParFldFrml(FD:FileDPtr;F:FieldDPtr;Z:FrmlPtr;Ad:boolean);
 function SearchKey(var XX:XString;Key:KeyDPtr;var NN:longint):boolean;
 function LinkUpw(LD:LinkDPtr;var N:longint;WithT:boolean):boolean;
procedure AssignNRecs(Add:boolean;N:longint);

                    { FIELD ACCESS }

 function _ShortS(F:FieldDPtr):string;
 function _LongS(F:FieldDPtr):LongStrPtr;
 function _R(F:FieldDPtr):float;
 function _B(F:FieldDPtr):boolean;
 function _T(F:FieldDPtr):longint;
procedure S_(F:FieldDPtr;S:string);
procedure LongS_(F:FieldDPtr;S:LongStrPtr);
procedure R_(F:FieldDPtr;R:float);
procedure B_(F:FieldDPtr;B:boolean);
procedure T_(F:FieldDPtr;Pos:longint);
procedure ZeroAllFlds;
procedure DelTFld(F:FieldDPtr);
procedure DelDifTFld(Rec,CompRec:pointer;F:FieldDPtr);
procedure ClearRecSpace(p:pointer);
procedure DelAllDifTFlds(Rec,CompRec:pointer);
procedure DelTFlds;
procedure CopyRecWithT(p1,p2:pointer);



type proc=procedure;
function ReadDelInTWork(Pos:longint):LongStrPtr;
function StoreInTWork(S:LongStrPtr):longint;
procedure ForAllFDs(P:proc);
function IsActiveRdb(FD:FileDPtr):boolean;


procedure ResetCompilePars;
var
  CurrChar:char;                       { Compile }
  ForwChar,ExpChar,Lexem:char;
  LexWord:string;
  SpecFDNameAllowed,IdxLocVarAllowed,FDLocVarAllowed,IsCompileErr:boolean;
  PrevCompInp:CompInpDPtr;                  { saved at "include" }
  InpArrPtr:CharArrPtr; InpRdbPos:RdbPos;   {    "       "       }
  InpArrLen,CurrPos,OldErrPos:word;         {    "       "       }
  FrmlSumEl:SumElPtr;               {set while reading sum/count argument}
  FrstSumVar,FileVarsAllowed:boolean;
  RdFldNameFrml:function(var FTyp:char):FrmlPtr;
  RdFunction:function(var FTyp:char):FrmlPtr;
  ChainSumEl:procedure;                          {set by user}
  LstCompileVar:byte;                         { boundary }

const Switches:string[20]='';
      SwitchLevel:word=0;

implementation   {***********************************************************}

uses obaseww,oaccess,runfrml,olongstr,
     rdrun,   {PushProcStk,PopProcStk called from RunFrml}
{$ifdef FandSQL} channel, {$endif}
     wwmenu,wwmix,
              {menu called from dml;
              password, selectdiskfile called from runfrml}
     rdproc,  {getevalfrml}
     runproc, {RunProcedure}
     editor,  {findtext called from frml}
     runedi,  {prompts/r/b called from frml,dml; testisnewrec from runfrml}
     compile,  {by CompileCatDcl}
     sort,     {createindexfile}
     expimp;   {OldToNewCat}


procedure NoOvr; far; assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push dx; push ax;
end;



procedure RunErrorM(Md:LockMode;N:word);
begin OldLMode(Md); RunError(N) end;



function FieldDMask(F:FieldDPtr):StringPtr; assembler;
asm  les di,F; mov dx,es; lea ax,es:[di+1].FieldDescr.Name;
     mov bl,es:[di].FieldDescr.Name.byte; xor bh,bh; add ax,bx end;
     
{$S-}
function GetRecSpace:pointer;
  { at reclen+1 : texts in TWork(=recvar)?   reclen+2 : data updated? }
  begin GetRecSpace:=GetZStore(CFile^.RecLen+2) end;
function GetRecSpace2:pointer;
  begin GetRecSpace2:=GetZStore2(CFile^.RecLen+2) end;
function CFileRecSize:word;
  begin CFileRecSize:=CFile^.RecLen+2 end;
procedure SetTWorkFlag; assembler;
asm  les di,CFile; mov bx,es:[di].FileD.RecLen; les di,CRecPtr;
     mov es:[di+bx].byte,1 end;
procedure ClearTWorkFlag; assembler;
asm  les di,CFile; mov bx,es:[di].FileD.RecLen; les di,CRecPtr;
     mov es:[di+bx].byte,0 end;
function HasTWorkFlag:boolean; assembler;
asm  les di,CFile; mov bx,es:[di].FileD.RecLen; les di,CRecPtr;
     mov al,es:[di+bx].byte; xor ah,ah end;
procedure SetUpdFlag; assembler;
asm  les di,CFile; mov bx,es:[di].FileD.RecLen; les di,CRecPtr;
     mov es:[di+bx+1].byte,1 end;
procedure ClearUpdFlag; assembler;
asm  les di,CFile; mov bx,es:[di].FileD.RecLen; les di,CRecPtr;
     mov es:[di+bx+1].byte,0 end;
function HasUpdFlag:boolean; assembler;
asm  les di,CFile; mov bx,es:[di].FileD.RecLen; les di,CRecPtr;
     mov al,es:[di+bx+1].byte; xor ah,ah end;


function LocVarAd(LV:LocVarPtr):pointer; assembler;
asm  les di,LV; mov dx,MyBP[2].word;
     mov ax,MyBP.word; add ax,es:[di].LocVar.BPOfs end;
{$S+}

function DeletedFlag:boolean; assembler;
asm  push ds; les di,CRecPtr; lds si,CFile; xor ax,ax;
     cmp [si].FileD.Typ,'X'; jne @1; cmp es:[di].byte,0; je @3; jmp @2;
@1:  cmp [si].FileD.Typ,'D'; jne @3; cmp es:[di].byte,'*'; jne @3;
                          {20H,08H valid records}
@2:  mov ax,1;
@3:  pop ds end;

procedure ClearDeletedFlag; assembler;
asm  push ds; les di,CRecPtr; lds si,CFile;
     cmp [si].FileD.Typ,'X'; jne @1; mov es:[di].byte,0; jmp @2;
@1:  cmp [si].FileD.Typ,'D'; jne @2; mov es:[di].byte,' ';
@2:  pop ds end;

procedure SetDeletedFlag; assembler;
asm  push ds; les di,CRecPtr; lds si,CFile;
     cmp [si].FileD.Typ,'X'; jne @1; mov es:[di].byte,1; jmp @2;
@1:  cmp [si].FileD.Typ,'D'; jne @2; mov es:[di].byte,'*';
@2:  pop ds end;


function CompStr(S1,S2:string):integer; assembler;
asm  push ds; lds si,S1; les di,S2; cld; lodsb; mov ah,es:[di]; inc di;
     mov cl,ah; cmp ah,al; jna @1; mov cl,al;
@1:  xor ch,ch; jcxz @2;
     repe cmpsb; jb @3; ja @4;
@2:  cmp al,ah; jb @3; ja @4;
     mov ax,_equ; jmp @5;
@3:  mov ax,_lt; jmp @5;
@4:  mov ax,_gt;
@5:  pop ds
end;

procedure TranslateOrd; near; assembler;
asm  mov   BYTE PTR ss:[di], 0
     push  di
     jcxz  @4
     xor   bx, bx
 @1: inc   si  { ligature: remains 1 char / otherwise XString.GetA doesn't
                 work and Sort also assumes the maximal key length constant/,
                 in CharOrdTab after first char}
     mov   bl, es:[si]
     mov   al, BYTE PTR CharOrdTab [bx]
{$ifndef FandAng}
     cmp   al, 49H        { h }
     jnz   @2
     cmp   BYTE PTR ss:[di], 43H   { c }
     jnz   @2
     mov   BYTE PTR ss:[di], 4aH   { ch }
     jmp   @3
{$endif}
 @2: inc   di
     mov   ss:[di], al
 @3: loop  @1
 @4: cmp   dx,1
     jne   @5
     mov   BYTE PTR ss:[di+1], 0
 @5: mov   ax, di
     pop   di
     sub   ax, di
     mov   ss:[di], al
end;

function TranslateOrdBack:word; assembler; { al=char => <es:di> }
asm  {$ifndef FandAng}
     cmp al,4aH; jne @1; { ch ? }
     mov ax,4843H; stosw; jmp @2 { 'CH' }
     {$endif}
@1:  push es; push di; push cx;
     mov di,SEG @Data; mov es,di; mov di,OFFSET CharOrdTab; mov cx,256;
     repne scasb; mov ax,255; sub ax,cx;
     pop cx; pop di; pop es; stosb;
@2:  end;


procedure CmpLxStr; assembler;
asm  push ds; xchg ax,cx; mov ax,ss; mov ds,ax; mov es,ax;
     inc di; lodsb; mov bl,al;
     cmp bl,cl; mov ax,1; je @3; mov ax,2; jb @3; mov ax,4; xchg bl,cl;
@3:  xor ch,ch; jcxz @4;
     repe cmpsb; jz @4; mov ax,2; jb @4; mov ax,4;
@4:  pop ds;
end;
function CompLexLongStr(S1,S2:LongStrPtr):word; assembler;
var Str11,Str22: array[0..256] of byte;     { max. 255 }
asm  cld; les si,S1; mov cx,es:[si]; inc si; cmp cx,255; jb @1; mov cx,255;
@1:  lea di,Str11; mov dx,1; call TranslateOrd;
     les si,S2; mov cx,es:[si]; inc si; cmp cx,255; jb @2; mov cx,255;
@2:  lea di,Str22; mov dx,1; call TranslateOrd; lea si,Str11;
     call CmpLxStr;
end;

function CompLexLongShortStr(S1:LongStrPtr;S2:StringPtr):word; assembler;
var Str11,Str22: array[0..256] of byte;
asm  cld; les si,S1; mov cx,es:[si]; inc si; cmp cx,255; jb @1; mov cx,255;
@1:  lea di,Str11; mov dx,1; call TranslateOrd;
     les si,S2; mov cl,es:[si]; xor ch,ch;
     lea di,Str22; mov dx,1; call TranslateOrd; lea si,Str11;
     call CmpLxStr;
end;
function CompLexStr(const S1,S2:string):word; assembler;
var Str11,Str22: array[0..256] of byte;
asm  cld; les si,S1; mov cl,es:[si]; xor ch,ch;
     lea di,Str11; mov dx,1; call TranslateOrd;
     les si,S2; mov cl,es:[si]; xor ch,ch;
     lea di,Str22; mov dx,1; call TranslateOrd; lea si,Str11;
     call CmpLxStr;
end;


function EquKFlds(KF1,KF2:KeyFldDPtr):boolean;
begin
  EquKFlds:=false; while KF1<>nil do begin
    if (KF2=nil) or (KF1^.CompLex<>KF2^.CompLex) or (KF1^.Descend<>KF2^.Descend)
      or (KF1^.FldD^.Name<>KF2^.FldD^.Name) then exit;
    KF1:=KF1^.Chain; KF2:=KF2^.Chain end;
  if KF2<>nil then exit; EquKFlds:=true;
end;


{$I type}
{$I fileacc}
{$I index}
{$I recacc}

{$S-}
procedure Code(var A; L:word); assembler;
asm   cld; mov  cx,L;jcxz @2; les di,A; mov al,0aaH;
@1:   xor  es:[di],al; inc di; loop @1;
@2:
end;
{$S+}
procedure XDecode(S:LongStrPtr); assembler;
var RMask:byte; h,oldL,newL:word;
asm  push ds; cld; lds si,S; mov es,S[2].word; mov bx,si;
     lodsw; cmp ax,0; je @5; mov di,si; add bx,ax; {bx=bound}
     mov ax,[bx]; xor ax,0ccccH; add si,ax; {si+Displ}
     dec bx; mov cl,[bx]; and cl,3; mov al,9cH; rol al,cl; mov RMask,al;
     xor ch,ch;
@1:  lodsb; mov dh,0ffH; mov dl,al;
@2:  cmp si,bx; jae @4;{input boundary}
     test dh,1; jz @1; {count 8 bits}  test dl,1; jnz @3;
     lodsb; rol RMask,1; xor al,RMask; stosb; shr dx,1; jmp @2; {copy 1 byte}
@3:  lodsb; mov cl,al; lodsw; push si; mov si,S.word;
     add si,ax;
     rep movsb; pop si; shr dx,1; jmp @2;  {repeat prev. sequence}
@4:  mov ax,di; mov di,S.word; sub ax,di; sub ax,2; stosw;
@5:  pop ds;
end;
procedure CodingLongStr(S:LongStrPtr);
begin
  if CFile^.TF^.LicenseNr=0 then Code(S^.A,S^.LL) else XDecode(S);
end;

procedure DirMinusBackslash(var D:DirStr);
   begin if (length(D)>3) and (D[length(D)]='\') then dec(D[0]) end;


function StoreInTWork(S:LongStrPtr):longint;
begin StoreInTWork:=TWork.Store(S) end;

function ReadDelInTWork(Pos:longint):LongStrPtr;
begin ReadDelInTWork:=TWork.Read(1,Pos); TWork.Delete(Pos) end;

procedure ForAllFDs(P:proc);
var R:RdbDPtr; cf:FileDPtr;
begin
  cf:=CFile; R:=CRdb; while R<>nil do begin
    CFile:=R^.FD; while CFile<>nil do begin P; CFile:=CFile^.Chain end;
    R:=R^.ChainBack end;
  CFile:=cf;
end;

function IsActiveRdb(FD:FileDPtr):boolean;
var R:RdbDPtr;
begin
  IsActiveRdb:=true; R:=CRdb; while R<>nil do begin
    if FD=R^.FD then exit; R:=R^.ChainBack end;
  IsActiveRdb:=false;
end;


procedure ResetCompilePars;
begin
  RdFldNameFrml:=RdFldNameFrmlF; @RdFunction:=nil; @ChainSumEl:=nil;
  FileVarsAllowed:=true; FDLocVarAllowed:=false; IdxLocVarAllowed:=false;
  PrevCompInp:=nil;
end;


end.