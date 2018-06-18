{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L-}

unit wwmenu;

interface

uses dos,
     drivers,base,access,rdrun;

const
  sfCursorVis = $0002;
  sfCursorBig = $0004;
  sfShadow    = $0008;
  sfFramed    = $0010;
  sfFrDouble  = $0020;
  sfFocused   = $0040;
  sfModal     = $2000;
type
  TRect = object
    A,Size:TPoint;
    function Contains(var P:TPoint):boolean;
  end;
  TWindow = object(TObject)
    Orig,Size,Shadow:TPoint;
    SavedW,SavedLLW:longint;
    State:word;
    WasCrsEnabled:boolean;
    constructor Init(C1,R1,C2,R2:byte; Attr:word;const top,bottom:string;
                     SaveLL:boolean);
    destructor Done; virtual;
    procedure Assign(C1,R1,C2,R2:byte);
    function Col1:word;
    function Row1:word;
    function Col2:word;
    function Row2:word;
    function Contains(var T:TPoint):boolean;
    function GetState(Flag:word):boolean;
    procedure SetState(Flag:word;On:boolean);
  end;

  PMenuBox = ^TMenuBox;
  PMenu = ^TMenu;
  TMenu = object(TWindow)
    Parent:PMenu;
    iTxt,nTxt,
    mx,my:word;
    HlpRdb:RdbDPtr;
    Palette:array[0..3]of byte; {norm,curr,char,disabled}
    IsBoxS:boolean;
    constructor Init;
    destructor Done; virtual;
    procedure ClearHlp;
    function Enabled(I:word):boolean; virtual;
    function ExecItem(var I:word):boolean; virtual;
    function FindChar:boolean;
    function GetHlpName:string; virtual;
    procedure GetItemRect(I:word;var R:TRect); virtual;
    function GetText(I:integer):string; virtual;
    procedure HandleEvent;
    function IsMenuBar:boolean;
    procedure LeadIn(var T:TPoint);
    procedure Next;
    function ParentsContain(var T:TPoint):boolean;
    procedure Prev;
    function UnderMenuBar:boolean;
    procedure WrText(I:word);
    procedure SetPalette(aPD:InstrPtr);
  end;
  TMenuBox = object(TMenu)
    constructor Init(C1,R1:word);
    function Exec(IStart:word):word;
    procedure GetItemRect(I:word;var R:TRect); virtual;
  end;
  PMenuBoxS = ^TMenuBoxS;
  TMenuBoxS = object(TMenuBox)
    MsgTxt:StringPtr;
    constructor Init(C1,R1:word;Msg:StringPtr);
    function GetHlpName:string; virtual;
    function GetText(I:integer):string; virtual;
  end;
  PMenuBoxP = ^TMenuBoxP;
  TMenuBoxP = object(TMenuBox)
    PD:InstrPtr;
    CRoot:ChoiceDPtr;
    HdTxt:StringPtr;
    constructor Init(C1,R1:word;aParent:PMenu;aPD:InstrPtr);
    function Enabled(I:word):boolean; virtual;
    function ExecItem(var I:word):boolean; virtual;
    function GetHlpName:string; virtual;
    function GetText(I:integer):string; virtual;
  end;
  PMenuBar = ^TMenuBar;
  TMenuBar = object(TMenu)
    nBlks:word;
    DownI:array[1..30]of byte;
    constructor Init(C1,R1,Cols:word);
    function Exec:word;
    function GetDownMenu(var W:PMenuBox):boolean; virtual;
    procedure GetItemRect(I:word;var R:TRect); virtual;
  end;
  PMenuBarS = ^TMenuBarS;
  TMenuBarS = object(TMenuBar)
    MsgTxt:StringPtr;
    constructor Init(MsgNr:word);
    function GetDownMenu(var W:PMenuBox):boolean; virtual;
    function GetHlpName:string; virtual;
    function GetText(I:integer):string; virtual;
  end;
  PMenuBarP = ^TMenuBarP;
  TMenuBarP = object(TMenuBar)
    PD:InstrPtr;
    CRoot:ChoiceDPtr;
    constructor Init(aPD:InstrPtr);
    function Enabled(I:word):boolean; virtual;
    function ExecItem(var I:word):boolean; virtual;
    function GetDownMenu(var W:PMenuBox):boolean; virtual;
    function GetHlpName:string; virtual;
    function GetText(I:integer):string; virtual;
  end;


function Menu(MsgNr,IStart:word):word;
function PrinterMenu(Msg:word):boolean;
procedure MenuBoxProc(PD:InstrPtr);
procedure MenuBarProc(PD:InstrPtr);

function GetHlpText(R:RdbDPtr;S:string;ByName:boolean;var IRec:word):LongStrPtr;
procedure DisplLLHelp(R:RdbDPtr;const Name:string;R24:boolean);

implementation

uses obase,obaseww,runfrml,runproj, {EditHelpOrCat}
     runproc, {RunInstr,RunProc}
     editor;  {Help}


function TRect.Contains(var P:TPoint):boolean;
begin
  Contains:= (P.X>=A.X) and (P.X<A.X+Size.X) and (P.Y>=A.Y) and (P.Y<A.Y+Size.Y);
end;


{ TWindow }
                                                     
constructor TWindow.Init(C1,R1,C2,R2:byte; Attr:word; const top,bottom:string;
                          SaveLL:boolean);
var i,n,m:word; s:string[80]; l:byte absolute s;
begin
  Assign(C1,R1,C2,R2);
  if GetState(sfShadow) then begin
    Shadow.X:=minw(2,TxtCols-Col2); Shadow.Y:=minw(1,TxtRows-Row2) end;
  WasCrsEnabled:=Crs.Enabled; CrsHide;
  SavedW:=PushW1(Orig.X+1,Orig.Y+1,Orig.X+Size.X+Shadow.X,Orig.Y+Size.Y+Shadow.Y,true,false);
  if SaveLL then SavedLLW:=PushW1(1,TxtRows,TxtCols,TxtRows,true,false) else SavedLLW:=0;
  if Shadow.Y=1 then ScrColor(Orig.X+2,Row2,Size.X+Shadow.X-2,colors.ShadowAttr);
  if Shadow.X>0 then for i:=Row1 to Row2 do ScrColor(Col2,i,Shadow.X,colors.ShadowAttr);
  if GetState(sfFramed) then begin
    n:=0; if GetState(sfFrDouble) then n:=9;
    ScrWrFrameLn(Orig.X,Orig.Y,N,Size.X,Attr);
    for i:=1 to Size.Y-2 do ScrWrFrameLn(Orig.X,Orig.Y+i,N+6,Size.X,Attr);
    ScrWrFrameLn(Orig.X,Orig.Y+Size.Y-1,N+3,Size.X,Attr);
    m:=Size.X-2;
    if length(top)<>0 then begin
      s:=' '+top+' '; l:=minw(l,m);
      ScrWrStr(Col1+(m-l)div 2,Orig.Y,s,Attr) end;
    if length(bottom)<>0 then begin
      s:=' '+bottom+' '; l:=minw(l,m);
      ScrWrStr(Col1+(m-l)div 2,Row2-1,s,Attr) end end
  else ScrClr(Orig.X,Orig.Y,Size.X,Size.Y,' ',Attr);
end;
destructor TWindow.Done;
begin
  if SavedLLW<>0 then PopW(SavedLLW); PopW(SavedW);
  if WasCrsEnabled then CrsShow;
  inherited Done;
end;
procedure TWindow.Assign(C1,R1,C2,R2:byte);
var cols,rows,m:integer;
begin
  m:=0; if GetState(sfFramed) then M:=2;
  cols:=C2+m; if C1<>0 then cols:=C2-C1+1; cols:=maxi(m+1,mini(cols,TxtCols));
  if C1=0 then Orig.X:=(TxtCols-cols) div 2 else Orig.X:=mini(C1-1,TxtCols-cols);
  rows:=R2+M; if R1<>0 then rows:=R2-R1+1; rows:=maxi(m+1,mini(rows,TxtRows));
  if R1=0 then Orig.Y:=(TxtRows-Rows) div 2 else Orig.Y:=mini(R1-1,TxtRows-rows);
  Size.Assign(Cols,Rows);
end;                            
function TWindow.Col1:word;
  begin Col1:=Orig.X+1 end;
function TWindow.Row1:word;
  begin Row1:=Orig.Y+1 end;
function TWindow.Col2:word;
  begin Col2:=Orig.X+Size.X end;
function TWindow.Row2:word;
  begin Row2:=Orig.Y+Size.Y end;
function TWindow.Contains(var T:TPoint):boolean;
  var R:TRect absolute Orig;
  begin Contains:=R.Contains(T) end;
function TWindow.GetState(Flag:word):boolean;
begin
  GetState:=State and Flag=Flag;
end;                          
procedure TWindow.SetState(Flag:word;On:boolean);
begin
  if On then State:=State or Flag else State:=State and not Flag;
end;



                     {  T M E N U  }


constructor TMenu.Init;
begin
  mx:=MenuX; my:=MenuY;
end;
destructor TMenu.Done;
begin
  MenuX:=mx; MenuY:=my; TWindow.Done;
end;
procedure TMenu.ClearHlp;
begin
  if HlpRdb<>nil then ClearLL(colors.uNorm);
end;
function TMenu.Enabled(I:word):boolean;
begin
  Enabled:=true;
end;
function TMenu.ExecItem(var I:word):boolean;
begin
  ExecItem:=false;
end;
function TMenu.FindChar:boolean;
var s:string[80]; i,j,k:word; c2:char;
begin
  FindChar:=false; i:=iTxt; for j:=1 to nTxt do if Enabled(j) then begin
    s:=GetText(j); if length(s)>0 then begin
      k:=Pos(^w,s); if k<>0 then c2:=s[k+1] else c2:=s[1];
      if upcase(NoDiakr(c2))=upcase(NoDiakr(char(KbdChar))) then begin
        iTxt:=j; WrText(i); FindChar:=true; exit end end end;
end;
function TMenu.GetHlpName:string;
begin
end;
procedure TMenu.GetItemRect(I:word;var R:TRect);
begin
end;
function TMenu.GetText(I:integer):string;
begin
end;
procedure TMenu.HandleEvent;
var i:word; hlp:string[40]; frst:boolean;
label 1,2;
begin
  WrText(iTxt); i:=iTxt; frst:=true; hlp:=GetHlpName; TestEvent;
1:KbdChar:=0; with Event do case What of
    evMouseDown:
      if Contains(Where) then begin KbdChar:=_M_; LeadIn(Where) end
      else if MouseInRect(0,TxtRows-1,TxtCols,1) then goto 2
      else if ParentsContain(Where) then begin KbdChar:=_ESC_; exit end;
    evKeyDown:
      case KeyCode of
       _Home_,_PgUp_ :begin iTxt:=0; Next; WrText(i) end;
       _End_,_PgDn_  :begin iTxt:=nTxt+1; Prev; WrText(i) end;
       _F1_    :begin
2:              ClrEvent; if HlpRdb<>nil then Help(HlpRdb,hlp,false); KbdChar:=0 end;
       _AltF10_:begin ClrEvent; Help(nil,'',false); KbdChar:=0 end;
       _AltF2_ :if IsTestRun and not IsBoxS then begin
                ClrEvent; EditHelpOrCat(_AltF2_,2,hlp); KbdChar:=0 end;
       else     KbdChar:=KeyCode end;
     else if frst then begin DisplLLHelp(HlpRdb,hlp,false); frst:=false end;
       ClrEvent; WaitEvent(0); goto 1 end;
  ClrEvent;
end;
function TMenu.IsMenuBar:boolean;
begin
  IsMenuBar:=Size.Y=1;
end;
procedure TMenu.LeadIn(var T:TPoint);
var i,j:word; r:TRect;
begin
  i:=iTxt; for j:=1 to nTxt do begin
    GetItemRect(j,r); if r.Contains(T) and Enabled(j) and (GetText(j)<>'') then begin
      iTxt:=j; WrText(i); WrText(j); exit end end;
  KbdChar:=0;
end;
procedure TMenu.Next;
begin
  repeat if iTxt<nTxt then inc(iTxt) else iTxt:=1
  until Enabled(iTxt) and (GetText(iTxt)<>'');
end;
function TMenu.ParentsContain(var T:TPoint):boolean;
var P:PMenu;
begin
  P:=Parent; ParentsContain:=true; while P<>nil do begin
    if P^.Contains(T) then exit; P:=P^.Parent end;
  ParentsContain:=false;
end;
procedure TMenu.Prev;
begin
  repeat if iTxt>1 then dec(iTxt) else iTxt:=nTxt
  until Enabled(iTxt) and (GetText(iTxt)<>'');
end;
function TMenu.UnderMenuBar:boolean;
begin
  UnderMenuBar:=(Parent<>nil) and (Parent^.IsMenuBar);
end;
procedure TMenu.WrText(I:word);
var s:string[80]; j,posw,x,x2,y:word; attr:byte; r:TRect; red,ena:boolean;
begin
  s:=GetText(I);
  if length(s)=0 then begin  { menubox only }
    ScrWrFrameLn(Orig.X,Orig.Y+I,18,Size.X,Palette[0]); exit end;
  GetItemRect(I,r); x:=r.A.X; y:=r.A.Y; x2:=x+r.Size.X;
  ena:=Enabled(I);
  if I=iTxt then attr:=Palette[1] else
  if ena then attr:=Palette[0] else attr:=Palette[3];
  posw:=pos(^w,s); ScrWrChar(x,y,' ',attr); inc(x); red:=false;
  for j:=1 to length(s) do begin
    if (s[j]=^w) or (posw=0) and ((j=1) or (j=2)) then
      if ena and (I<>iTxt) then
        if red then begin attr:=Palette[0]; red:=false end
        else begin attr:=Palette[2]; red:=true end;
    if s[j]<>^w then begin ScrWrChar(x,y,s[j],attr); inc(x) end end;
  while x<x2 do begin ScrWrChar(x,y,' ',attr); inc(x) end;
end;
procedure TMenu.SetPalette(aPD:InstrPtr);
var i:word;
begin with aPD^ do begin
  Palette[0]:=RunWordImpl(mAttr[0],colors.mNorm);
  Palette[1]:=RunWordImpl(mAttr[1],colors.mHiLi);
  Palette[2]:=RunWordImpl(mAttr[2],colors.mFirst);
  Palette[3]:=RunWordImpl(mAttr[3],colors.mDisabled);
end end;

               {  T M E N U B O X  }

constructor TMenuBox.Init(C1,R1:word);
var cols,c2,r2,i,l:word; hd:string[80];
begin
  TMenu.Init;
  hd:=GetText(0); cols:=length(hd);
  for i:=1 to nTxt do begin
    l:=LenStyleStr(GetText(i)); if l>cols then cols:=l end;
  inc(cols,4); if (cols+2>TxtCols) or (nTxt+2>TxtRows) then RunError(636);
  c2:=Cols; r2:=nTxt; if C1<>0 then inc(c2,C1+1); if R1<>0 then inc(r2,R1+1);
  SetState(sfFramed,true);
  TWindow.Init(C1,R1,c2,r2,Palette[0],hd,'',HlpRdb<>nil);
  for i:=1 to nTxt do WrText(i);
end;
function TMenuBox.Exec(IStart:word):word;
var i,j:word; w:PMenuBox;
label 1,2,3,4;
begin
  if nTxt=0 then begin Exec:=0; exit end;
  j:=0; iTxt:=IStart; if iTxt=0 then iTxt:=1; Prev; Next;  {get valid iTxt}
1:TMenu.HandleEvent; i:=iTxt;
  case KbdChar of
   _M_     :goto 2;
   _ESC_   :begin i:=0; goto 3 end;
   _Up_    :begin Prev; WrText(i) end;
   _Down_  :begin Next; WrText(i) end;
   _left_  :if UnderMenuBar then begin j:=1; goto 4 end;
   _right_ :if UnderMenuBar then begin j:=2; goto 4 end;
   else if not FindChar then goto 1; WrText(iTxt);
2:    i:=iTxt; MenuX:=Orig.X+4; MenuY:=Orig.Y+i+2;
3:    ClearHlp; if not ExecItem(i) then begin
4:      Exec:=(j shl 8)+i; exit end end;
  goto 1;
end;
procedure TMenuBox.GetItemRect(I:word;var r:TRect);
begin
  r.A.X:=Orig.X+2; r.A.Y:=Orig.Y+I; r.Size.X:=Size.X-4; r.Size.Y:=1;
end;


               {  T M E N U B O X S  }

constructor TMenuBoxS.Init(C1,R1:word;Msg:StringPtr);
begin
  TObject.Init; MsgTxt:=StoreStr(Msg^);
  HlpRdb:=RdbDPtr(HelpFD); IsBoxS:=true;
  nTxt:=CountDLines(MsgTxt^[1],length(MsgTxt^),'/')-2;
  Move(colors.mNorm,Palette,3);
  SetState(sfShadow,true);
  TMenuBox.Init(C1,R1);
end;
function TMenuBoxS.GetHlpName:string;
var s:string;
begin
  str(iTxt,s); GetHlpName:=GetText(-1)+'_'+s;
end;
function TMenuBoxS.GetText(I:integer):string;
begin          {helpname/head/text1/text2/...}
  GetText:=GetDLine(MsgTxt^[1],length(MsgTxt^),'/',I+2);
end;

function Menu(MsgNr,IStart:word):word;
var w:PMenuBoxS; p:pointer;
begin
  MarkStore(p); RdMsg(MsgNr); New(w,Init(0,0,StringPtr(@MsgLine)));
  Menu:=w^.Exec(IStart); w^.Done; ReleaseStore(p);
end;

function PrinterMenu(Msg:word):boolean;
var w:PMenuBoxS; i,j:word; p:pointer; nr:string[3]; nm,lpt:string;
begin
  MarkStore(p); RdMsg(Msg); j:=prCurr;
  for prCurr:=0 to prMax-1 do begin
    i:=printer[prCurr].Lpti; str(i,nr); nm:=PrTab(prName); ReplaceChar(nm,'/','-');
    lpt:='(LPT'+nr+')'; if printer[prCurr].toMgr then lpt:='';
    MsgLine:=MsgLine+'/'+nm+copy('      ',1,maxi(0,9-length(nm)))+lpt end;
  prCurr:=j;
  New(w,Init(0,0,StringPtr(@MsgLine))); i:=w^.Exec(prCurr+1);
  if i>0 then SetCurrPrinter(i-1); w^.Done; ReleaseStore(p);
  PrinterMenu:=i>0;
end;



              {  T M E N U B O X P  }

function CI(C:ChoiceDPtr;I:word):ChoiceDPtr;
label 1;
begin
1:if C^.Displ then dec(i); if i=0 then begin CI:=C; exit end;
  C:=C^.Chain; goto 1;
end;
function CountNTxt(C:ChoiceDPtr;IsMenuBar:boolean):word;
var n,nValid:word; s:string; b:boolean;
begin
  n:=0; nValid:=0; while C<>nil do begin
    b:=RunBool(C^.Bool); C^.Displ:=false;
    if b or C^.DisplEver then begin
      C^.Displ:=true; inc(n); s:=RunShortStr(C^.TxtFrml);
      if length(s)<>0 then begin
        s[0]:=char(mini(length(s),TxtCols-6));
        if pos(^w,s)=0 then s:=^w+s[1]+^w+copy(s,2,255) end
      else if IsMenuBar then s:=' ';
      C^.Txt:=StoreStr(s); if s='' then b:=false; if b then inc(nValid) end;
    C^.Enabled:=b; C:=C^.Chain end;
  if nValid=0 then n:=0; CountNTxt:=n;
end;

constructor TMenuBoxP.Init(C1,R1:word;aParent:PMenu;aPD:InstrPtr);
var s:string;
begin
  TObject.Init; PD:=aPD; Parent:=aParent;
  s:=RunShortStr(aPD^.HdLine); s[0]:=char(mini(length(s),TxtCols-6));
  HdTxt:=StoreStr(s); HlpRdb:=aPD^.HelpRdb; CRoot:=aPD^.Choices;
  nTxt:=CountNTxt(CRoot,false); SetPalette(aPD);
  if aPD^.X<>nil then begin C1:=RunInt(aPD^.X); R1:=RunInt(aPD^.Y) end
  else if aPD^.PullDown and (aParent=nil) then begin C1:=MenuX; R1:=MenuY end;
  if aPD^.Shdw then SetState(sfShadow,true);
  TMenuBox.Init(C1,R1);
end;
function TMenuBoxP.Enabled(I:word):boolean;
begin
  Enabled:=CI(CRoot,I)^.Enabled;
end;
function TMenuBoxP.ExecItem(var I:word):boolean;
begin
  ExecItem:=false; if not PD^.PullDown then exit;
  if I=0 then begin
    if (Event.What=evMouseDown) or not PD^.WasESCBranch then exit;
    RunInstr(PD^.ESCInstr) end
  else RunInstr(CI(CRoot,I)^.Instr);
  if ExitP then begin I:=255; exit end; I:=0;
  if PD^.Loop then begin
    if BreakP then begin BreakP:=false; exit end;
    ExecItem:=true end
  else
    if BreakP then begin BreakP:=false; I:=255 end;
end;
function TMenuBoxP.GetHlpName:string;
var S:StringPtr;
begin
  S:=CI(CRoot,iTxt)^.HelpName;
  if S<>nil then GetHlpName:=S^ else GetHlpName:='';
end;
function TMenuBoxP.GetText(I:integer):string;
begin
  if I=0 then GetText:=HdTxt^ else GetText:=CI(CRoot,I)^.Txt^;
end;

procedure MenuBoxProc(PD:InstrPtr);
var w:PMenuBoxP; i:word; mx,my:byte; p:pointer;
label 1;
begin
  i:=1; MarkStore(p);
1:New(w,Init(0,0,nil,PD)); i:=w^.Exec(i); w^.Done; ReleaseStore(p);
  if not PD^.PullDown then begin
    if i=0 then begin
      if not PD^.WasESCBranch then exit; RunInstr(PD^.ESCInstr) end
    else RunInstr(CI(PD^.Choices,i)^.Instr);
    if BreakP or ExitP then begin if PD^.Loop then BreakP:=false end
    else if PD^.Loop then goto 1 end;
end;

               {  T M E N U B A R  }

constructor TMenuBar.Init(C1,R1,Cols:word);
var c2,r2,i,l:word;
begin
  TMenu.Init;
  l:=0; for i:=1 to nTxt do l:=l+LenStyleStr(GetText(i))+2;
  if l>TxtCols then RunError(636); Cols:=maxw(l,Cols);
  if nTxt=0 then nBlks:=0 else nBlks:=(Cols-l)div nTxt;
  while (Cols-l-nBlks*nTxt)<nBlks do dec(nBlks);
  c2:=Cols; if C1<>0 then inc(c2,C1-1); r2:=1; if R1<>0 then r2:=R1;
  TWindow.Init(C1,R1,c2,r2,Palette[0],'','',HlpRdb<>nil);
end;
function TMenuBar.GetDownMenu(var W:PMenuBox):boolean;
begin
  GetDownMenu:=false;
end;
function TMenuBar.Exec:word;
var i:word; w:PMenuBox; enter,down:boolean; r:TRect;
label 1,2,3,4;
begin
  if nTxt=0 then begin Exec:=0; exit end;
  down:=false; iTxt:=1; Prev; Next;  {get valid iTxt}
  for i:=1 to nTxt do WrText(i);
1:TMenu.HandleEvent; i:=iTxt; enter:=false;
  case KbdChar of
   _M_     :begin enter:=true; goto 2 end;
   _ESC_   :begin i:=0; goto 4 end;
   _Down_  :goto 3;
   _left_  :begin Prev; WrText(i); if down then goto 2 end;
   _right_ :begin Next; WrText(i); if down then goto 2 end;
   else if not FindChar then goto 1; enter:=true;
2:    WrText(iTxt);
3:    GetItemRect(iTxt,r); MenuX:=r.A.X+1; MenuY:=r.A.Y+2;
      if GetDownMenu(w) then begin
        i:=w^.Exec(DownI[iTxt]); w^.Done; ReleaseStore(w);
        DownI[iTxt]:=lo(i); enter:=false; down:=true;
        if hi(i)=1 then begin i:=iTxt; Prev; WrText(i); goto 2 end;
        if hi(i)=2 then begin i:=iTxt; Next; WrText(i); goto 2 end;
        down:=false; if i=0 then goto 1;
        Exec:=(iTxt shl 8)+i; exit end;
      if enter then begin
        i:=iTxt;
4:      ClearHlp;
        if not ExecItem(i) then begin Exec:=i shl 8; exit end end end;
  goto 1;
end;
procedure TMenuBar.GetItemRect(I:word;var R:TRect);
var j,x:word;
begin
  x:=Orig.X+nBlks;
  for j:=1 to I-1 do inc(x,LenStyleStr(GetText(j))+2+nBlks);
  R.A.X:=x; R.A.Y:=Orig.Y; R.Size.X:=LenStyleStr(GetText(I))+2; R.Size.Y:=1;
end;

constructor TMenuBarS.Init(MsgNr:word);
begin
  TObject.Init;
  RdMsg(MsgNr); MsgTxt:=StoreStr(MsgLine); HlpRdb:=RdbDPtr(HelpFD);
  nTxt:=(CountDLines(MsgTxt^[1],length(MsgTxt^),'/')-1) div 2;
  Move(colors.mNorm,Palette,3);
  TMenuBar.Init(1,1,TxtCols);
end;
function TMenuBarS.GetDownMenu(var W:PMenuBox):boolean;
var TNr:string[10]; n,err:word; p:PMenuBoxS;
begin
  GetDownMenu:=false; TNr:=GetText(nTxt+iTxt);
  val(TNr,n,err); if (length(TNr)=0) or (err<>0) then exit;
  RdMsg(n); New(p,Init(MenuX,MenuY,StringPtr(@MsgLine))); p^.Parent:=@Self; w:=p;
  GetDownMenu:=true;
end;
function TMenuBarS.GetHlpName:string;
var s:string;
begin
  str(iTxt,s); GetHlpName:=GetText(0)+'_'+s;
end;
function TMenuBarS.GetText(I:integer):string;
begin          {helpname/text1/text2/...subnr1/subnr2/...}
  GetText:=GetDLine(MsgTxt^[1],length(MsgTxt^),'/',I+1);
end;

            {  T M E N U B A R P  }

constructor TMenuBarP.Init(aPD:InstrPtr);
var x1,y1,l1:word;
begin
  TObject.Init; PD:=aPD; HlpRdb:=PD^.HelpRdb; CRoot:=PD^.Choices;
  nTxt:=CountNTxt(CRoot,true); SetPalette(PD);
  y1:=1; if PD^.Y<>nil then y1:=RunInt(PD^.Y);
  x1:=1; l1:=TxtCols; if PD^.X<>nil then begin
    x1:=RunInt(PD^.X); l1:=RunInt(PD^.XSz) end;
  TMenuBar.Init(x1,y1,l1);
end;
function TMenuBarP.Enabled(I:word):boolean;
begin
  Enabled:=CI(CRoot,I)^.Enabled;
end;
function TMenuBarP.ExecItem(var I:word):boolean;
var r:TRect;
begin
  ExecItem:=false;
  if I=0 then begin if not PD^.WasESCBranch then exit; RunInstr(PD^.ESCInstr) end
  else RunInstr(CI(CRoot,I)^.Instr);
  I:=0; if BreakP or ExitP then begin BreakP:=false; exit end;
  ExecItem:=true;
end;
function TMenuBarP.GetDownMenu(var W:PMenuBox):boolean;
var PD1:InstrPtr; p:PMenuBoxP;
begin
  GetDownMenu:=false; PD1:=CI(CRoot,iTxt)^.Instr;
  if (PD1=nil) or (PD1^.Chain<>nil) or (PD1^.Kind<>_menubox)
     or not PD1^.PullDown then exit;
  New(p,Init(MenuX,MenuY,@Self,PD1)); W:=p; GetDownMenu:=true;
end;
function TMenuBarP.GetHlpName:string;
var S:StringPtr;
begin
  S:=CI(CRoot,iTxt)^.HelpName;
  if S<>nil then GetHlpName:=S^ else GetHlpName:='';
end;
function TMenuBarP.GetText(I:integer):string;
begin
  GetText:=CI(CRoot,I)^.Txt^;
end;

procedure MenuBarProc(PD:InstrPtr);
var w:PMenuBarP; p:pointer;
begin
  MarkStore(p); New(w,Init(PD)); w^.Exec; w^.Done; ReleaseStore(p);
end;

function GetHlpText(R:RdbDPtr;S:string;ByName:boolean;var IRec:word):LongStrPtr;
var NmF,TxtF:FieldDPtr; i:word; T:LongStrPtr; Nm:string;
    fo:TVideoFont; cf:FileDPtr; cr,p:pointer; md:LockMode;
label 1,2,3,4,5;
begin
  cr:=CRecPtr; MarkStore2(p); T:=nil;
  if ByName then begin
    if R=nil then goto 5; CFile:=FileDPtr(R);
    if CFile=HelpFD then begin if CFile^.Handle=$ff then goto 5 end
    else begin CFile:=R^.HelpFD; if CFile=nil then goto 5 end;
    ConvToNoDiakr(S[1],length(S),Fonts.VFont) end;
1:md:=NewLMode(RdMode); if CFile^.Handle=$ff then goto 5;
  CRecPtr:=GetRecSpace2; NmF:=CFile^.FldD; TxtF:=NmF^.Chain;
  if not ByName then begin
    i:=maxw(1,minw(IRec,CFile^.NRecs)); ReadRec(i); goto 2 end;
  for i:=1 to CFile^.NRecs do begin
    ReadRec(i);
    Nm:=TrailChar(' ',_shorts(NmF));
    if CFile=HelpFD then fo:=foKamen else fo:=Fonts.VFont;
    ConvToNoDiakr(Nm[1],length(Nm),fo);
    if EqualsMask(@S[1],length(S),Nm) then begin
2:    T:=_longs(TxtF);
      if not ByName or (T^.LL>0) or (i=CFile^.NRecs) then begin
        if CFile=HelpFD then ConvKamenToCurr(T^.A,T^.LL); IRec:=i; goto 3 end;
      ReleaseStore(T); inc(i); ReadRec(i); goto 2 end end;
3:OldLMode(md); ReleaseStore2(p);
  if (T=nil) and (CFile<>HelpFD) then begin
4:  r:=r^.ChainBack; if r<>nil then
      if (r^.HelpFD<>nil) and (r^.HelpFD<>CFile) then begin
        CFile:=r^.HelpFD; goto 1 end
      else goto 4 end;
5:CRecPtr:=cr; GetHlpText:=T;
end;


procedure DisplLLHelp(R:RdbDPtr;const Name:string;R24:boolean);
var s:LongStrPtr; p:pointer; i,y:word; iRec:word; cf:FileDPtr;
label 1;
begin
  if (R=nil) or (R<>RdbDPtr(HelpFD)) and (R^.HelpFD=nil) then exit;
  MarkStore(p); cf:=CFile;
  if Name<>'' then begin
    iRec:=0; s:=GetHlpText(R,Name,true,iRec);
    if s<>nil then begin
      s:=CopyLine(s,1,1); MsgLine[0]:=char(minw(s^.LL,sizeof(MsgLine)-1));
      Move(s^.A,MsgLine[1],length(MsgLine));
      if MsgLine[1]='{' then begin
        MsgLine:=copy(MsgLine,2,255); i:=pos('}',MsgLine);
        if i>0 then delete(MsgLine,i,255) end;
      MsgLine[0]:=char(minw(TxtCols,length(MsgLine)));
      goto 1 end end;
  MsgLine:='';
1:y:=TxtRows-1; if R24 then dec(y);
  ScrWrStr(0,y,MsgLine,colors.nNorm);
  ScrClr(length(MsgLine),y,TxtCols-length(MsgLine),1,' ',colors.nNorm);
  CFile:=cf; ReleaseStore(p);
end;


end.
