{$A-}
{$DEFINE SAVESCREEN}

unit WINDOWS;

interface

uses crt,dos,mydos;

          { *** screen *** }

var TxtCols,TxtRows:word; {current text mode}
{$IFDEF SAVESCREEN}
    OrgScr:pointer; {saved original screen}
{$ENDIF}

 function Xcol :word; {current window - upper left corner}
 function Xrow :word;
 function Ycol :word; {lower right corner}
 function Yrow :word;
 function ActCols :word; {actual size}
 function ActRows :word;
 function AbsWhereX :word;
 function AbsWhereY :word;

const     _rel=true;_abs=false;

procedure AppendLine; {to current window}
procedure TruncLine; {from current window}
procedure TruncFrame (width:word);
 function ScreenAddr (c,r:word; rel:boolean):pointer; {video RAM base address}
 function CurrScreenAddr :pointer;
procedure ChangeAttr (attr,c,r,length:word; rel:boolean);
 function ReadFromScreen (c,r,length:word; rel:boolean):ScreenStr;
procedure MyWindow (c,r,cols,rows:word; rel:boolean);

          { *** keyboard *** }

const _bool=1;_escape=2;
      _digit=11;_real=12;_filename=13;_hex=14;
      _uppercase=21;_char=22;_spec=23; _any=255; {filters for MyReadKey}
      _NoKey=0;_CtrlKey=1;_CharKey=2;_AltKey=3;_FunKey=4;_MoveKey=5;_ExtKey=6; {set in KeyCode}

type  ProcType=procedure;

var   KeyCode:word; LastCh:char; {last pressed character}
      HotKeys {escape sequence instead alt-key} :boolean;

 function MyKeyPressed :boolean; {non-empty KeyBuff or keypressed}
 function MyReadKey (filter:word):char; {reads from KeyBuff or from keyboard}
procedure SetKeyBuff (s:string);
procedure AppKeyBuff (s:string);
procedure TopKeyBuff (s:string);
procedure ClrKeyBuff;

procedure SetBreak (ch:char; proc:ProcType; help:ScreenStr); {breaks MyReadKey procedure}
procedure ClrBreak (ch:char);

procedure ClrUpdate; {update checking}
procedure SetUpdate;
 function Updated:boolean;

procedure Wait; {waits for any key}
procedure WaitESC; {...escape key}
procedure WaitCR; {...enter}

          {*** windowing ***}

 function WwResult :word;

type      WwAttrs=record wmin,wmax,attr,cm,cx,cy:word end;

procedure SaveWwAttr (var attrs:WwAttrs);
procedure LoadWwAttr (attrs:WwAttrs);
 function Framed :boolean;

procedure PushWw (xc,xr,cols,rows:word); {xc,xr...0(auto-place),$FF(centre)}   {windows stack}
procedure PopWw;
 function SaveWw :pointer; {windows heap}
procedure UpdateWw (p:pointer);
procedure LoadWw (p:pointer);
procedure FreeWw (p:pointer);
procedure Frame (width:word;top,bottom:ScreenStr); {frames and truncates current window}
procedure ShadowFrame (color:word);

procedure LastLine (s:ScreenStr); {report to last line, (...) parts will be highlighted}
procedure DelLL;

 function MarkWw :pointer; {switching among disjunct windows}
 function OpenedWw (p:pointer):boolean;
procedure SwitchWw (p:pointer);
procedure SwitchTopWw;

 function Verify (c,r:word;s:ScreenStr):boolean; {Y/N prompt}
procedure Message (c,r:word;s:ScreenStr); {confirm by CR-key}
procedure EditLine (var s:string; pos,maxlen,maxcol,filter:word);

          {*** select ***}

const _single=0;_double=1;_wide=2;_singleL=3;_doubleL=4;_wideL=5; {frame width}
type  SelPars=record {parameters for select, sets after every PushSelect}
                     Caps:boolean; {(false) all strings will be upcased}
                     Abcd:boolean; {(false) alphabetical order in window}
                     LeftJust:boolean; {(true) left/right justify}
                     Order:boolean; {(false) move possible/impossible}
                     Static:boolean;  {(true) static/dynamic window size}
                     Subset:boolean;  {(false) select subset/one string}
                     PopUp:boolean; {(false) pop-up/pull-down window}
                     Cols,Rows,Tab,Maxlen:word; {(4,5,2,8) window dimensions}
                     x,y,FrameWidth:word; {(0,0,_double) window place}
                     StrUp,StrDn:ScreenStr; {('','') to frame}
                     Chars:string4; {('Ø') for select subset with attributes}
              end;
var   ss:SelPars; CharSelect:char; {return attribute}

procedure PutSelect (s:ScreenStr);
procedure PushSelString;
procedure PushSelSubset;
procedure PopSelect;
 function SelString :ScreenStr;
procedure SelSubset;
 function GetSelect :ScreenStr;

          {*** menus ***}

type  MenPars=record {parameters for menu, sets after every PushMenu}
                     PopUp:boolean; {(false) pop-up/pull-down window}
                     x,y,FrameWidth:word; {(0,0,_single)}
                     StrUp,StrDn:ScreenStr; {('','') to frame}
                     Breaks:string4; {breaks characters}
              end;
var   mm:MenPars;

procedure PushMenu (var adr);
procedure PopMenu;
 function Menu: word;
procedure MenuOff;

          {*** monitor ***}

type  ColorType=record sFrame,sItem,sSel,sMove:byte; {select}
                       mFrame,mItem,mSel:byte; {menu}
                       lNorm,lHigh:byte; {last line}
                       vFrame,vText:byte; {verify}
                       gFrame,gText:byte; {message}
                       pFrame,pText:byte; {prompt}
                       eText:byte; {edit}
                end;
const MonoColors:ColorType=(sFrame:MonoInvr;sItem:MonoInvr;sSel:MonoNorm;sMove:MonoNormBl;
                            mFrame:MonoHiLi;mItem:MonoNorm;mSel:MonoInvr;
                            lNorm:MonoNorm;lHigh:MonoHiLi;
                            vFrame:MonoNorm;vText:MonoHiLi;
                            gFrame:MonoNorm;gText:MonoHiLi;
                            pFrame:MonoNorm;pText:MonoHiLi;
                            eText:MonoInvr);
      BWColors:ColorType=(sFrame:$70;sItem:$70;sSel:$0A;sMove:$8A;
                          mFrame:$0E;mItem:$08;mSel:$0E;
                          lNorm:$08;lHigh:$0E;
                          vFrame:$08;vText:$0E;
                          gFrame:$08;gText:$0E;
                          pFrame:$08;pText:$0E;
                          eText:$70);
      COColors:ColorType=(sFrame:$70;sItem:$70;sSel:$4E;sMove:$CB;
                          mFrame:$30;mItem:$30;mSel:$1E;
                          lNorm:$30;lHigh:$1E;
                          vFrame:$20;vText:$20;
                          gFrame:$4e;gText:$4e;
                          pFrame:$60;pText:$60;
                          eText:$70);
var   Colors:ColorType;

          {*** applications ***}

 function SelectDiskFile (init:PathStr; HdStr:ScreenStr):PathStr;

implementation

var WwMin:record col,row:byte end absolute WindMin;
    WwMax:record col,row:byte end absolute WindMax;

function Xcol:word;
  begin Xcol:=succ(WwMin.col) end;
function Xrow:word;
  begin Xrow:=succ(WwMin.row) end;
function Ycol:word;
  begin Ycol:=succ(WwMax.col) end;
function Yrow:word;
  begin Yrow:=succ(WwMax.row) end;
function ActCols:word;
  begin ActCols:=succ(ycol-xcol) end;
function ActRows:word;
  begin ActRows:=succ(yrow-xrow) end;
function AbsWhereX:word;
  begin AbsWhereX:=Xcol+whereX-1 end;
function AbsWhereY:word;
  begin AbsWhereY:=Xrow+whereY-1 end;
procedure AdjustVideoMode;
  begin TxtCols:=VideoCols; TxtRows:=VideoPageSize div (2*VideoCols);
        window(1,1,TxtCols,TxtRows);
  end;

procedure AppendLine;
  begin inc(WwMax.row) end;
procedure TruncLine;
  begin dec(WwMax.row) end;
procedure TruncFrame(width:word);
  begin case width of _single.._wide:window(xcol+1,xrow+1,ycol-1,yrow-1);
                    _singleL.._wideL:window(xcol,xrow+1,ycol,yrow-1);
  end   end;
function ScreenAddr(c,r:word; rel:boolean):pointer;
  begin if rel then ScreenAddr:=ptr(VideoAddr,2*TxtCols*(pred(r)+pred(xrow))+2*(pred(c)+pred(xcol)))
               else ScreenAddr:=ptr(VideoAddr,2*TxtCols*pred(r)+2*pred(c))
  end;
function CurrScreenAddr:pointer;
  begin CurrScreenAddr:=ScreenAddr(whereX,whereY,_rel) end;
procedure ChangeAttr(attr,c,r,length:word; rel:boolean);
  var p:^byte; ofs:word absolute p;
      i:word; a:array[1..MaxTxtCols,1..2]of byte;
  begin p:=ScreenAddr(c,r,rel);
        MoveFromScreen(p^,a[1,1],2*length);
        for i:=1 to length do a[i,2]:=attr; MoveToScreen(a[1,1],p^,2*length);
  end;
function ReadFromScreen(c,r,length:word; rel:boolean):ScreenStr;
  var a:array[1..MaxTxtCols,1..2]of char; s:string; p:pointer; i:word;
  begin p:=ScreenAddr(c,r,rel);
        MoveFromScreen(p^,a[1,1],2*length);
        s[0]:=char(length);for i:=1 to length do s[i]:=a[i,1];ReadFromScreen:=s;
  end;
procedure MyWindow (c,r,cols,rows:word; rel:boolean);
  begin if rel then window(Xcol+c-1,Xrow+r-1,cols+Xcol+c-2,rows+Xrow+r-2)
               else window(c,r,cols+c-1,rows+r-1);
  end;
function Colored(ch:char):word;
  begin colored:=byte(ch)+(word(TextAttr) shl 8) end;

{ *** keyboard *** }

const MaxKeyBuf=16;
var   KeyBuf:string[MaxKeyBuf];

procedure SetKeyBuff (s:string);
  begin KeyBuf:=s end;
procedure AppKeyBuff (s:string);
  begin KeyBuf:=KeyBuf+s end;
procedure TopKeyBuff (s:string);
  begin KeyBuf:=s+KeyBuf end;
procedure ClrKeyBuff;
  var r:registers;
  begin KeyBuf:='';
        with r do begin AH:=$0C;
                        AL:=$00;
                        MsDos(r);
  end             end;

type PtrBreak=^BreakItem;
     BreakItem=record next:PtrBreak;
                      key:char;
                      action:ProcType;
                      hlp:string1;
               end;
var  TopBreak:PtrBreak; BreakInt:boolean;

procedure SetBreak (ch:char; proc:ProcType; help:ScreenStr);
  var p,q,x:PtrBreak;
  begin GetMem(x,sizeof(BreakItem)+length(help)-1);
        with x^ do begin key:=ch; action:=proc; move(help,hlp,length(help)+1) end;
        p:=@TopBreak; q:=TopBreak;
        while (q^.key<ch) and (q<>nil) do begin p:=q; q:=q^.next end;
        p^.next:=x; x^.next:=q;
  end;
procedure ClrBreak (ch:char);
  var p,q:PtrBreak;
  begin p:=@TopBreak; q:=TopBreak;
        while (q^.key<>ch) and (q<>nil) do begin p:=q; q:=q^.next end;
        if q=nil then exit;
        p^.next:=q^.next; FreeMem(q,sizeof(BreakItem)+length(q^.hlp)-1);
  end;
function BreakHelp: ScreenStr;
  var p:PtrBreak; s:ScreenStr;
  begin s:=' '; p:=TopBreak;
        while p<>nil do begin if p^.hlp<>'' then s:=s+p^.hlp+'  '; p:=p^.next end;
        BreakHelp:=s;
  end;
function Break(ch:char):boolean;
  var p:PtrBreak;
  begin break:=false; p:=TopBreak;
        while (p^.key<>ch) and (p<>nil) do p:=p^.next;
        if p<>nil then begin break:=true; BreakInt:=true;
                             p^.action;
                             BreakInt:=false;
  end                  end;

var Updt:boolean;

function ReadKey:char;
  begin if KeyBuf='' then ReadKey:=crt.readkey
                     else begin ReadKey:=KeyBuf[1]; dec(KeyBuf[0]);
                                move(KeyBuf[2],KeyBuf[1],byte(KeyBuf[0]));
  end                     end;
function MyKeyPressed :boolean;
  begin MyKeyPressed:=KeyPressed or (KeyBuf<>'') end;
function MyReadKey(filter:word):char;
  var c,d,e:char; RdFromKbd:boolean;
  function digit:boolean;
    begin digit:=(c>='0') and (c<='9') end;
  function letter:boolean;
    begin letter:=(upcase(c)>='A') and (upcase(c)<='Z') end;
  label 1;
  begin RdFromKbd:=(KeyBuf='');
      1:c:=ReadKey; KeyCode:=_NoKey;
        case c of
         #00:begin c:=ReadKey;
                   if not(BreakInt) then if Break(c) then goto 1;
                   case c of
                   _AltQ.._AltM:KeyCode:=_AltKey;
                   _F1.._F10,_ShiftF1.._ShiftF10,_CtrlF1.._CtrlF10,_AltF1.._AltF10:KeyCode:=_FunKey;
                   _Home.._Del,_CtrlLeft.._CtrlHome,_CtrlPgUp:KeyCode:=_MoveKey;
                   else KeyCode:=_ExtKey;
                   end;
                   if HotKeys then if KeyCode=_AltKey then
                   begin SetKeyBuff(_ESC+_ESC+_ESC+_ESC+_ESC+_ESC+_ESC+_ESC+_ESC+AltCh(c));goto 1 end;
             end;
    #01..#31:begin KeyCode:=_CtrlKey end;
   #32..#255:begin case filter of
                _escape:if c<>_ESC then goto 1;
                 { _bool:begin c:=upcase(c); if (c<>'Y') and (c<>'N') then goto 1 end; Angl }
                 _bool:begin c:=upcase(c); if (c<>'Y') and (c<>'N') then goto 1 end;
                 _digit:if not(digit) then goto 1;
                  _real:if not(digit) and (c<>'.') and (c<>'+') and (c<>'-') and (c<>'e') and (c<>'E') then goto 1;
              _filename:begin c:=upcase(c);
                        if not(letter) and not(digit) and (c<>'.') and (c<>'*') and (c<>'?') and (c<>'\') and (c<>':')
                        then goto 1;
                        end;
                   _hex:begin c:=upcase(c);
                        if not(digit) and ((c<'A') or (c>'F')) then goto 1;
                        end;
             _uppercase:c:=upcase(c);
                  _char:if (c<#32) or (c>#127) then goto 1;
                  _spec:goto 1;
                   _any:;
                    end;
                   KeyCode:=_CharKey;
        end  end;
        LastCh:=c; MyReadKey:=c;
        if (KeyCode=_CharKey) then Updt:=true;
  end;

procedure ClrUpdate;
  begin Updt:=false end;
procedure SetUpdate;
  begin Updt:=true end;
function Updated:boolean;
  begin Updated:=Updt end;

procedure Wait;
  var ch:char;
  begin ClrKeyBuff; ch:=MyReadKey(_any) end;
procedure WaitEsc;
  var ch:char;
  begin while MyReadKey(_escape)<>_ESC do end;
procedure WaitCR;
  var ch:char;
  begin while MyReadKey(_any)<>_CR do end;

{ *** windowing *** }

var WwError:word;

function WwResult:word;
  begin WwResult:=WwError;WwError:=0 end;
procedure SetWwError(code:word);
  begin WwError:=code end;
procedure WwHalt;
  begin if WwError<>0 then Halt(101) end;

procedure SaveWwAttr(var attrs:WwAttrs);
  begin with attrs do begin wmin:=WindMin;wmax:=WindMax;attr:=TextAttr;
                           cm:=GetCursorMode;cx:=whereX;cy:=whereY;
  end                end;
procedure LoadWwAttr(attrs:WwAttrs);
  begin with attrs do begin WindMin:=wmin;WindMax:=wmax;TextAttr:=attr;
                           SetCursorMode(cm);gotoxy(cx,cy);
  end                end;
function Framed:boolean;
  var s:string1;
  begin if (xcol=1) or (xrow=1) then begin framed:=false; exit end;
        s:=ReadFromScreen(0,0,1,_rel);
        framed:=(s[1]='⁄') or (s[1]='…') or (s[1]='‹');
  end;

type PtrWw=^Ww;
     Ww=record next:PtrWw;
               typ:char;
               attrs:WwAttrs;
               xc,xr,yc,yr:byte;
               scr:byte;
        end;
var  TopWw,ActWw:PtrWw;
     GlobAttrs:WwAttrs;

function WwX:word; forward;
function WwY:word; forward;

procedure PushWw(xc,xr,cols,rows:word);
  var yc,yr,i:word;
      p:PtrWw;q:pointer;ofsP:word absolute p;ofsQ:word absolute q;
  begin if cols>TxtCols then cols:=TxtCols;
        if rows>TxtRows then rows:=TxtRows;
        if xc=$FF then xc:=(TxtCols-cols) div 2 else if xc=0 then xc:=WwX;
        if xr=$FF then xr:=(TxtRows-rows) div 2 else if xr=0 then xr:=WwY;
        if (xc=0) or (xc+cols>TxtCols+1) then xc:=1;
        if (xr=0) or (xr+rows>TxtRows+1) then xr:=1;
        yc:=xc+cols-1; yr:=xr+rows-1; cols:=2*cols; {char+attr}
        GetMem(pointer(p),rows*cols+pred(sizeof(Ww)));
        p^.xc:=xc;p^.xr:=xr;p^.yc:=yc;p^.yr:=yr;
        if TopWw=nil then begin SaveWwAttr(GlobAttrs) end
                     else begin SaveWwAttr(TopWw^.attrs) end;
        p^.typ:='W';p^.next:=TopWw;TopWw:=p;
        q:=ScreenAddr(xc,xr,_abs);p:=addr(p^.scr);
        for i:=1 to rows do begin MoveFromScreen(q^,p^,cols);
                                  inc(ofsQ,2*TxtCols); inc(ofsP,cols);
                            end;
        window(xc,xr,yc,yr);ActWw:=TopWw;
  end;
procedure PopWw;
  var rows,cols,i:word;
      p:PtrWw;q:pointer;ofsP:word absolute p;ofsQ:word absolute q;
  begin p:=TopWw;
        with p^ do begin cols:=2*succ(yc-xc);rows:=succ(yr-xr) end;
        FreeMem(pointer(p),rows*cols+pred(sizeof(Ww)));
        TopWw:=p^.next;ActWw:=TopWw;
        if TopWw=nil then LoadWwAttr(GlobAttrs) else LoadWwAttr(TopWw^.attrs);
        q:=ScreenAddr(p^.xc,p^.xr,_abs);p:=addr(p^.scr);
        for i:=1 to rows do begin MoveToScreen(p^,q^,cols);
                                  inc(ofsQ,2*TxtCols); inc(ofsP,cols);
  end                       end;
function SaveOrUpdate(r:pointer):pointer;
  var i,xc,xr,yc,yr,ac,ar,cols:word;
      p:PtrWw;q:pointer;ofsP:word absolute p;ofsQ:word absolute q;
  begin xc:=xcol;xr:=xrow;yc:=ycol;yr:=yrow;
        if (r=nil) and framed then begin dec(xc);dec(xr);inc(yc);inc(yr) end;
        ac:=succ(yc-xc);ar:=succ(yr-xr);cols:=2*ac;
        if r=nil then GetMem(pointer(p),ar*cols+pred(sizeof(Ww)))
                 else p:=r;
        p^.xc:=xc;p^.xr:=xr;p^.yc:=yc;p^.yr:=yr; p^.typ:='C';
        SaveWwAttr(p^.attrs); SaveOrUpdate:=p;
        q:=ScreenAddr(xc,xr,_abs);p:=addr(p^.scr);
        for i:=1 to ar do begin MoveFromScreen(q^,p^,cols);
                                inc(ofsQ,2*TxtCols); inc(ofsP,cols);
  end                     end;
function SaveWw:pointer;
  begin SaveWw:=SaveOrUpdate(nil) end;
procedure UpdateWw (p:pointer);
  var q:pointer;
  begin q:=SaveOrUpdate(p) end;
procedure LoadWw(p:pointer);
  var i,xc,xr,yc,yr,ac,ar:word;
      pp:PtrWw absolute p;q:pointer;ofsPP:word absolute p;ofsQ:word absolute q;
  begin xc:=pp^.xc;xr:=pp^.xr;yc:=pp^.yc;yr:=pp^.yr;
        ac:=succ(yc-xc);ar:=succ(yr-xr);
        PushWw(xc,xr,ac,ar); LoadWwAttr(pp^.attrs);
        q:=ScreenAddr(xc,xr,_abs);p:=addr(pp^.scr);
        for i:=1 to ar do begin MoveToScreen(p^,q^,2*ac);
                                inc(ofsQ,2*TxtCols); inc(ofsPP,2*ac);
  end                     end;
procedure FreeWw (p:pointer);
  var ac,ar:word; pp:PtrWw absolute p;
  begin ac:=succ(pp^.yc-pp^.xc);ar:=succ(pp^.yr-pp^.xr);
        FreeMem(p,ar*2*ac+pred(sizeof(Ww)));
  end;
procedure Frame(width:word;top,bottom:ScreenStr);
  type  FrameDir=(ul,ur,u,dr,dl,d,l,r);
        FrameChars=array[FrameDir]of char;
  const FrameCharsSingle:FrameChars =('⁄','ø','ƒ','Ÿ','¿','ƒ','≥','≥');
        FrameCharsDouble:FrameChars=('…','ª','Õ','º','»','Õ','∫','∫');
        FrameCharsWide:FrameChars  =('‹','‹','‹','ﬂ','ﬂ','ﬂ','›','ﬁ');
        { ⁄ƒƒƒƒƒƒƒƒƒƒø     …ÕÕÕÕÕÕÕÕÕÕª     ‹‹‹‹‹‹‹‹‹‹‹‹ }
        { ≥..........≥     ∫..........∫     ›..........ﬁ }
        { ¿ƒƒƒƒƒƒƒƒƒƒŸ     »ÕÕÕÕÕÕÕÕÕÕº     ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ }
  var   i:word; f:FrameChars; ch:char;
  begin if width<=_wide then begin
          if (ActCols<3) or (ActRows<3) then exit;
          case width of _single:f:=FrameCharsSingle;
                        _double:f:=FrameCharsDouble;
                         else   f:=FrameCharsWide;
          end;
          gotoxy(1,1);write(f[ul]);for i:=2 to (ActCols-1) do write(f[u]);write(f[ur]);
          for i:=2 to ActRows-1 do begin gotoxy(1,i);write(f[l],f[r]:ActCols-1) end;
          gotoxy(1,ActRows);write(f[dl]);for i:=2 to (ActCols-1) do write(f[d]);
          word(ScreenAddr(ycol,yrow,_abs)^):=colored(f[dr]);
                             end
                        else begin clrscr;
                                   case width of _singleL:ch:='ƒ';
                                                 _doubleL:ch:='Õ';
                                                  else    ch:='ﬂ';
                                   end;
                                   gotoxy(1,1);for i:=1 to (ActCols) do write(ch);
                                   gotoxy(1,ActRows);for i:=1 to (ActCols-1) do write(ch);
                                   word(ScreenAddr(ycol,yrow,_abs)^):=colored(ch);
                             end;
        if (length(top)>0)and(length(top)<ActCols-2)
        then begin gotoxy((ActCols-length(top)) div 2,1);write(' ',top,' ') end;
        if (length(bottom)>0)and(length(bottom)<ActCols-2)
        then begin gotoxy((ActCols-length(bottom)) div 2,ActRows);write(' ',bottom,' ') end;
        TruncFrame(width);
  end;
procedure ShadowFrame (color:word);
  var a:word;
  begin inc(WwMin.row); inc(WwMin.col);
        a:=TextAttr; TextAttr:=color; clrscr;
        dec(WwMin.row); dec(WwMin.col); dec(WwMax.row); dec(WwMax.col);
        TextAttr:=a; clrscr;
  end;

procedure LastLine (s:ScreenStr);
  var attrs:WwAttrs; i:word;
  label 1;
  begin SaveWwAttr(attrs);
        window(1,TxtRows,TxtCols,TxtRows);TextAttr:=Colors.lNorm;clreol;
        write(' ');
        for i:=1 to length(s) do if whereX=TxtCols then goto 1
                            else if s[i]='(' then TextAttr:=Colors.lHigh
                            else if s[i]=')' then TextAttr:=Colors.lNorm
                            else write(s[i]);
     1: LoadWwAttr(attrs);
  end;
procedure DelLL;
  begin LastLine(BreakHelp) end;

function MarkWw :pointer;
  begin MarkWw:=TopWw end;
function OpenedWw (p:pointer):boolean;
  function Conj(p,q:PtrWw):boolean;
    var pxc,pxr,pyc,pyr,qxc,qxr,qyc,qyr:word;
    begin pxc:=p^.xc;pxr:=p^.xr;pyc:=p^.yc;pyr:=p^.yr; {framed->dec missing}
          qxc:=q^.xc;qxr:=q^.xr;qyc:=q^.yc;qyr:=q^.yr;
          conj:=(((pxc>=qxc)and(pxc<=qyc)) or ((pyc>=qxc)and(pyc<=qyc)) or
                 ((qxc>=pxc)and(qxc<=pyc)) or ((qyc>=pxc)and(qyc<=pyc))) and
                (((pxr>=qxr)and(pxr<=qyr)) or ((pyr>=qxr)and(pyr<=qyr)) or
                 ((qxr>=pxr)and(qxr<=pyr)) or ((qyr>=pxr)and(qyr<=pyr)))
    end;
  var q:PtrWw; i:word;
  begin OpenedWw:=false;q:=TopWw;
        while q<>p do begin if conj(PtrWw(p),q) then exit;q:=q^.next end;
        OpenedWw:=true;
  end;
procedure SwitchWw (p:pointer);
  begin if OpenedWw(p) then begin SaveWwAttr(ActWw^.attrs);
                                  ActWw:=p;
                                  LoadWwAttr(ActWw^.attrs);
  end                       end;
procedure SwitchTopWw;
  begin SwitchWw(TopWw) end;

function Verify (c,r:word;s:ScreenStr):boolean;
  var cols:word; ch:char;
  begin cols:=length(s)+11; if cols<20 then cols:=20; PushWw(c,r,cols,3);
        TextAttr:=Colors.vFrame; Frame(_double,'Otazka','');
        TextAttr:=Colors.vText;write(' ',s,' (A/N)?');
        repeat ch:=myreadkey(_bool) until KeyCode=_CharKey;
        verify:=ch='A'; PopWw;
  end;

procedure Message (c,r:word;s:ScreenStr);
  var cols:word; ch:char;
  begin cols:=length(s)+4; if cols<20 then cols:=20; PushWw(c,r,cols,3);
        TextAttr:=Colors.gFrame; Frame(_double,'stiskni '+enterCh,'');
        TextAttr:=Colors.gText;write(' ',s);
        repeat ch:=myreadkey(_any) until (KeyCode=_CtrlKey) and (ch=_CR); PopWw;
  end;

procedure EditLine(var s:string; pos,maxlen,maxcol,filter:word);
  var sLen:byte absolute s; savedS:string;
      addr:pointer; cx,cy,base:word; insert:boolean;
  procedure WriteStr;
    var BuffLine:array [1..MaxTxtCols] of word;
        i:word; attr:byte; chr:char; item:word absolute chr;
    begin if pos<=base then base:=pred(pos)
                       else if pos>base+maxcol+1 then base:=pos-maxcol-1;
          if (pos=base+1) and (base>0) then dec(base);
          attr:=TextAttr;
          for i:=base+1 to base+maxcol do
              begin if i<=sLen then chr:=s[i] else chr:=' ';BuffLine[i-base]:=item end;
          MoveToScreen(BuffLine,addr^,2*maxcol); gotoxy(cx+pos-base-1,cy);
    end;
  var ch:char; attr:WwAttrs;
  label 1;
  begin SaveWwAttr(attr); cx:=whereX; cy:=whereY; addr:=CurrScreenAddr;
        base:=0; insert:=false; savedS:=s;
        if maxcol=0 then maxcol:=maxlen;
        if maxcol>ActCols-cx then maxcol:=ActCols-cx;
        if pos>sLen then pos:=sLen+1;
        while (sLen>0) and (s[sLen]=' ') do dec(sLen);
        CursorOn; WriteStr; ch:=MyReadKey(Filter);
        if KeyCode=_CharKey then begin fillchar(s[1],MaxLen,' ');sLen:=0;pos:=1 end;
        while (ch<>_CR) and (ch<>_ESC) do
          begin if KeyCode=_MoveKey then case ch of
                  _left :if (pos>1) then dec(pos);
                  _right:if pos<=sLen then inc(pos);
                  _ins  :begin insert:=not(insert); if insert then SetCursorMode(Cursors.Half) else CursorOn end;
                  _del  :if pos<=sLen then begin 1:move(s[pos+1],s[pos],sLen-pos);
                                                  s[sLen]:=' ';dec(sLen);
                                           end;
                  _home :pos:=1;
                  _end  :pos:=succ(sLen);
                                        end
                                   else
                if KeyCode=_CtrlKey then begin
                  if ch=_BS then if pos>1 then begin dec(pos);goto 1 end;
                                         end
                                   else
                if KeyCode=_CharKey then
                begin if (pos<=MaxLen) and ((sLen<MaxLen) or (not(insert))) then
                        begin if insert then begin move(s[pos],s[pos+1],sLen-pos+1);inc(sLen) end
                                        else if pos>sLen then inc(sLen);
                              s[pos]:=ch;inc(pos);
                end     end
                                    else beep;
                WriteStr; ch:=MyReadKey(Filter);
          end;
       if ch=_ESC then begin s:=savedS; WriteStr end; LoadWwAttr(attr);
  end;

{ *** select *** }

type PtrStr=^StrItem;
     StrItem=record next:PtrStr;
                    tag:char;
                    str:string[1]
             end;
var  FirstStr,LastStr:PtrStr;

type SelVars=record cols,rows,tab,wwSize,
                    c,r,actcmax,cmax,base,bmax,scrmaxlen,maxlen,sum:word;
                    SelKeys:string4; NoSelKeys,ActSelKey:word;
                    oneWw:boolean;
                    LeftJust,Order,Subset,PopUp:boolean;
                    x,y:word;
                    FirstS,LastS:PtrStr;
             end;
     PtrSel=^SelItem;
     SelItem=record level:byte;
                    w:PtrWw;
                    next:PtrSel;
                    sv:SelVars;
              end;
var  TopSel:PtrSel;

(*procedure SelDump(sv:selvars);
  begin PushWw($FF,$FF,60,7); Frame(_wide,'select dump','press any key');
        with sv do begin writeln(' cols:',cols,' rows:',rows,' tab:',tab,' wwSize:',wwSize,
                    ' c:',c,' r:',r,' actmax:',actcmax,' cmax:',cmax,' base:',base,
                    ' bmax:',bmax,' scrmaxlen:',scrmaxlen,' maxlen:',maxlen,' sum:',sum,
                    ' SeKey:',SelKeys,' NoSelKeys:',NoSelKeys,' oneWw:',oneWw,
                    ' LeftJust:',LeftJust,' Order:',Order,' Subset:',Subset,
                    ' x:',x,' y:',y,hexa(FirstS):10,hexa(LastS):10);
                    wait; PopWw;
  end              end;*)

procedure PutSelect(s:ScreenStr);
  var p:PtrStr;
  begin if s<>'' then begin GetMem(pointer(p),length(s)+pred(sizeof(StrItem)));
                            move(s[0],p^.str[0],succ(length(s)));
                            p^.tag:=' '; p^.next:=nil;
                            if FirstStr=nil then FirstStr:=p else LastStr^.next:=p;
                            LastStr:=p;
                      end
                 else begin
  end                 end;

procedure SetSPars;
  begin with ss do begin
        Caps:=false;Abcd:=false;LeftJust:=true;PopUp:=false;Order:=false;Static:=true;Subset:=false;
        x:=0;y:=0;Cols:=4;Rows:=5;Tab:=2;Maxlen:=8; FrameWidth:=_double;
        StrUp:='';StrDn:=''; Chars:='Ø';
  end             end;

procedure DisplSel(var sv:SelVars;b:integer);
  var p:PtrStr; i,j:word;
  begin with sv do begin if sum=0 then exit;
        if b>bmax then b:=bmax
                  else if b<0 then b:=0 else b:=b-(b mod cols);
        base:=b;
        if base=bmax then actcmax:=cmax else actcmax:=cols;
        p:=FirstS; for i:=1 to b do p:=p^.next; TextAttr:=Colors.sItem;
        for j:=1 to rows do
          for i:=1 to cols do
            if b<sum then begin
              gotoxy(2+pred(i)*tab,j);
              if subset then write(p^.tag);
              if LeftJust then write(p^.str,'':maxlen-length(p^.str))
                          else write(p^.str:maxlen);
              p:=p^.next;inc(b);
                          end;
        clreol; {ItemAttr(Colors.sSel);}
        gotoxy(1,1);if base>0 then write(upCh) else write(' ');
        if base<bmax then begin i:=colored(downCh);
                                MoveToScreen(i,ScreenAddr(ActCols,ActRows,_rel)^,2);
  end              end    end;

procedure PushSelect;
  var sv:SelVars; p:PtrSel;
  procedure InitVars;
    const fr=2;
    var   w:word; p:PtrStr;
    begin with sv do begin x:=ss.x; y:=ss.y;
          if FirstStr=nil then begin PushWw(x,y,30,3);
                                     Frame(ss.framewidth,ss.strup,ss.strdn); exit;
                               end;
          FirstS:=FirstStr; LastS:=nil; FirstStr:=nil; LastStr:=nil;
          leftjust:=ss.leftjust; order:=ss.order; subset:=ss.subset; popup:=ss.PopUp;
          SelKeys:=ss.chars;
          sum:=0; maxlen:=0;
          p:=FirstS; while p<>nil do begin if length(p^.str)>maxlen then maxlen:=length(p^.str);
                                           inc(sum); p:=p^.next;
                                     end;
          if ss.Maxlen>maxlen then maxlen:=ss.Maxlen; if maxlen>TxtCols then maxlen:=TxtCols-10;
          if subset then scrmaxlen:=succ(maxlen) else scrmaxlen:=maxlen; tab:=scrmaxlen+ss.Tab;
         {if x+ss.Cols*(tab+ss.Tab)-ss.Tab>TxtCols then x:=1;
          if y+ss.Rows>TxtRows then y:=1;}
          cols:=ss.Cols; if cols>sum then cols:=sum;
          ss.Cols:=ss.Cols*tab+2+fr-ss.Tab; ss.Rows:=ss.Rows+2;
          rows:=(sum div cols);
          if(sum mod cols)<>0 then inc(rows);
          if rows>ss.Rows-fr then begin rows:=ss.Rows-fr;oneWw:=false end else oneWw:=true;
          wwSize:=cols*rows;w:=cols*tab-ss.Tab+2+fr;
          if ss.Static then PushWw(x,y,ss.Cols,ss.Rows)
                       else PushWw(x,y,w,rows+fr);
          TextAttr:=Colors.sFrame; Frame(ss.framewidth,ss.strup,ss.strdn);
          cmax:=sum mod cols;if cmax=0 then cmax:=cols;
          bmax:=(sum-wwSize);bmax:=bmax+cols-cmax;
          c:=1;r:=1; base:=0;
          NoSelKeys:=length(SelKeys); ActSelKey:=1;
    end              end;
  procedure Capitals;
    var p:PtrStr; i,j:word;
    begin p:=sv.FirstS;
          for i:=1 to sv.sum do begin for j:=1 to length(p^.str) do
                                          p^.str[j]:=upcase(p^.str[j]);
                                      p:=p^.next;
    end                         end;
  procedure AbcdSort;
    var p,q,r:PtrStr; i:word; sorted:boolean;
    begin repeat r:=addr(sv.FirstS);p:=sv.FirstS;q:=sv.FirstS^.next; sorted:=true;
                 for i:=1 to pred(sv.sum) do
                     begin if p^.str>q^.str then
                              begin {if i=1 then sv.FirstS:=q else} r^.next:=q;
                                    p^.next:=q^.next;q^.next:=p;
                                    r:=q;q:=p^.next; sorted:=false;
                              end           else
                              begin r:=p;p:=q;q:=q^.next;
                     end      end;
          until  sorted;
    end;
    begin {of PushSelect}
          GetMem(p,sizeof(SelItem)); fillchar(sv,sizeof(sv),0);
          InitVars;
          if sv.sum>0 then begin if ss.Caps then Capitals;
                                 if ss.Abcd then AbcdSort;
                           end;
          DisplSel(sv,0);
          p^.sv:=sv; p^.next:=TopSel; if TopSel=nil then p^.level:=1 else p^.level:=succ(TopSel^.level);
          TopSel:=p; TopWw^.typ:='S'; TopSel^.w:=TopWw;
          SetSPars;
    end {of PushSelect};

procedure PushSelString;
  begin ss.subset:=false; PushSelect end;
procedure PushSelSubset;
  begin ss.subset:=true; PushSelect end;

procedure PopSel;
  var p:PtrStr;
  begin if TopSel=nil then exit;
        p:=TopSel^.sv.FirstS;
        while p<>nil do begin {message($FF,$FF,'Str...FreeMem('+hexa(p)+','+hexw(0)+')');}
                              FreeMem(pointer(p),length(p^.str)+pred(sizeof(StrItem)));
                              p:=p^.next;
                        end;
        {message($FF,$FF,'TopSel...FreeMem('+hexa(topsel)+','+hexw(sizeof(selitem))+')');}
        FreeMem(TopSel,sizeof(SelItem));
        TopSel:=TopSel^.next;
  end;
procedure PopSelect;
  begin if TopSel=nil then exit; PopSel; PopWw end;

function Select:ScreenStr;
  var sv:SelVars; ch:char; i:word; attrs:WwAttrs;
  function CurrPos:word;
    begin with sv do CurrPos:=base+pred(r)*cols+pred(c) end;
  function CurrItem:PtrStr;
    var p:PtrStr; i:word;
    begin with sv do begin
          p:=FirstS;for i:=1 to CurrPos do p:=p^.next;CurrItem:=p
    end              end;
  function CurrStr:ScreenStr;
    begin CurrStr:=CurrItem^.str end;
  procedure CurrXY;
    begin with sv do gotoxy(2+pred(c)*tab,r) end;
  procedure ItemAttr(attr:word);
    begin with sv do ChangeAttr(attr,pred(c)*tab+2,r,scrmaxlen,_rel) end;
  const high=true;low=false;
  procedure Display(b:word);
    begin DisplSel(sv,b); ItemAttr(Colors.sSel) end;
  procedure InitStep;
    var i:word; ch:char; toggle:boolean;
    begin with sv do begin
          SaveWwAttr(attrs); with TopSel^.w^ do window(xc,xr,yc,yr); TruncFrame(_double);
          ItemAttr(Colors.sSel); toggle:=NoSelKeys>1;
          {if subset then if order then if toggle then ToggleLLMsg(203) else LLMsg(204)
                                  else if toggle then ToggleLLMsg(205) else LLMsg(206);}
          {F1-Select F2-Unselect F9-Move F10-Toggle}
    end              end;
  procedure FirstLetter;
    var p:PtrStr; i:word;
    begin with sv do begin
          p:=FirstS; i:=0;
          while(p^.str[1]<>ch) and (p^.str[1]<>upcase(ch)) and (i<sum)
          do begin p:=p^.next;inc(i) end;
          if (p^.str[1]=ch) or (p^.str[1]=upcase(ch)) then
             begin if(i<base)or(i>=base+wwSize) then display(i);
                   ItemAttr(Colors.sItem);
                   c:=succ((i-base)mod cols);
                   r:=succ((i-base)div cols);
                   ItemAttr(Colors.sSel);
    end      end     end;
  procedure Up;
    begin with sv do begin
          if r>1 then begin ItemAttr(Colors.sItem);dec(r);ItemAttr(Colors.sSel) end
                 else if base>0 then display(base-cols);
    end              end;
  procedure Down;
    begin with sv do begin
          if r<rows then begin ItemAttr(Colors.sItem);inc(r);
                               if (r=rows) and (c>actcmax) then
                                  if rows=1 then c:=cmax else dec(r);
                               ItemAttr(Colors.sSel);
                         end
                    else if base<bmax then
                            begin display(base+cols);
                                  if c>actcmax then
                                     begin ItemAttr(Colors.sItem);
                                           if rows=1 then c:=cmax else dec(r);
                                           ItemAttr(Colors.sSel);
    end              end    end      end;
  procedure Left;
    begin with sv do begin
          if (r<>1) or (c<>1) then
             begin itemattr(Colors.sItem);dec(c);if c=0 then begin c:=cols;dec(r) end;
                   itemattr(Colors.sSel);
             end              else
             begin if base=0 then exit;
                   Up;itemattr(Colors.sItem);c:=cols;itemattr(Colors.sSel);
    end      end     end;
  procedure Right;
    begin with sv do begin
          if (r<rows) or (c<actcmax) then
             begin itemattr(Colors.sItem);inc(c);if c=succ(cols) then begin c:=1;inc(r) end;
                   itemattr(Colors.sSel);
             end                     else
             begin if base=bmax then exit;
                   Down;itemattr(Colors.sItem);c:=1;r:=rows;itemattr(Colors.sSel);
    end      end     end;
  procedure PgUp;
    begin with sv do begin if base=0 then exit;display(base-wwSize);
    end              end;
  procedure PgDn;
    begin with sv do begin
          if base=bmax then exit;display(base+wwSize);
          if (base=bmax)and(r=rows)and(c>cmax) then
             begin itemattr(Colors.sItem);if rows=1 then c:=cmax else dec(r);
                   itemattr(Colors.sSel);
    end      end     end;
  procedure Home;
    begin with sv do begin
          itemattr(Colors.sItem);r:=1;c:=1;
          if base=0 then itemattr(Colors.sSel) else display(0)
    end              end;
  procedure Eend;
    begin with sv do begin
          itemattr(Colors.sItem);r:=rows;c:=cmax;
          if base=bmax then itemattr(Colors.sSel) else display(bmax)
    end              end;
  procedure InsDel(ch:char);
    var p:PtrStr;
    begin with sv do begin
          if not(subset) then exit;
          p:=CurrItem; if p^.tag<>ch then begin p^.tag:=ch; display(base) end;
          right;
    end              end;
  procedure InsDelAll(ch:char);
    var p:PtrStr;
    begin with sv do begin
          if not(subset) then exit;
          p:=FirstS; while p<>nil do begin p^.tag:=ch; p:=p^.next end;
          display(base);
    end              end;
  procedure MoveItem;
    function Step(pos:integer):boolean;
      var po,qo,pn,qn,h:PtrStr; oldP,newP,i,j:integer;
      begin with sv do begin
            step:=false;oldP:=CurrPos;newP:=oldP+pos;
            if (newP<0) or (newP>pred(sum)) then exit;if pos>0 then inc(newP);
            po:=addr(FirstS);for i:=1 to oldP do po:=po^.next;qo:=po^.next;
            pn:=addr(FirstS);for i:=1 to newP do pn:=pn^.next;qn:=pn^.next;
            h:=qo^.next;pn^.next:=qo;qo^.next:=qn;po^.next:=h;
            if (newP>=base) and (newP<=base+wwSize) then display(base);
            step:=true;
      end              end;
    var ch:char; a:byte;
    begin with sv do begin
          a:=Colors.sSel; Colors.sSel:=Colors.sMove; itemattr(Colors.sSel);
          while true do
            begin ch:=MyReadKey(_any); if KeyCode=_MoveKey then
                  case ch of _Left:if step(-1) then left;
                            _Right:if step(1) then right;
                               _Up:if step(-cols) then up;
                             _Down:if step(cols) then down;
                  end                                      else
                  if (KeyCode=_FunKey) and (ch=_F9) then
                     begin Colors.sSel:=a; itemattr(Colors.sSel); exit;
    end     end      end end;
  procedure Toggle;
    begin with sv do begin
          inc(ActSelKey); if ActSelKey>NoSelKeys then ActSelKey:=1;
    end              end;
  procedure Return;
    begin with sv do begin
          if LastCh=_ESC then begin Select:=''; PopSelect end
                         else begin Select:=CurrStr; LastS:=FirstS; TopSel^.sv:=sv;
                                    if PopUp then PopWw else LoadWwAttr(attrs);
                              end;
           DelLL;
    end              end;

begin {of Select}
      sv:=TopSel^.sv;
      if sv.sum=0 then begin write(' Zadne nejsou'); waitESC; return; exit end;
      InitStep;
      with sv do
      while true do begin ch:=MyReadKey(_any); case KeyCode of
            _CharKey:FirstLetter;
            _CtrlKey:if (ch=_CR) or (ch=_ESC) then begin return;exit end;
            _MoveKey:case ch of
                    _left:left;
                   _right:right;
                     _up:up;
                   _down:down;
                   _PgUp:if not(oneWw) then PgUp;
                   _PgDn:if not(oneWw) then PgDn;
                   _Home:home;
                    _End:eend;
                     end;
             _FunKey:case ch of
                      _F1:insdel(SelKeys[ActSelKey]);
                      _F2:insdel(' ');
                  _CtrlF1:insdelall(SelKeys[ActSelKey]);
                  _CtrlF2:insdelall(' ');
                      _F9:if Order then MoveItem;
                     _F10:if NoSelKeys>1 then toggle;
end   end           end                        end; {of Select}

function SelString:ScreenStr;
  begin SelString:=Select; if TopSel^.sv.PopUp then PopSel end;
procedure SelSubset;
  var s:ScreenStr;
  begin s:=Select end;
function GetSelect :ScreenStr;
  var p:PtrStr;
  begin GetSelect:=''; CharSelect:=' '; p:=TopSel^.sv.LastS;
        if p=nil then begin if TopSel^.sv.PopUp then PopSel; exit end;
        while p^.tag=' '
        do begin p:=p^.next; if p=nil then begin if TopSel^.sv.PopUp then PopSel; exit;
           end                             end;
        GetSelect:=p^.str; CharSelect:=p^.tag;
        TopSel^.sv.LastS:=p^.next;
  end;

{ *** menu *** }

type PtrMenu=^MenuItem;
     MenuItem=record next:PtrMenu;
                     w:PtrWw;
                     pos,sum:word;
                     PopUp:boolean;
                     breaks:string4;
                     name:array[1..1{sum},1..5]of byte; {col,row,len,pos,cap}
              end;
var  TopMenu:PtrMenu;

procedure MenuAttr(high:boolean);
  var i,attr:word;
  begin if high then attr:=Colors.mSel else attr:=Colors.mItem;
        with Topmenu^ do begin ChangeAttr(attr,name[pos,1],name[pos,2],name[pos,3],_abs);
                               if not(high) then ChangeAttr(Colors.mSel,name[pos,1]+name[pos,4]-1,name[pos,2],1,_abs);
  end                    end;
const high=true;low=false;

procedure SetMPars;
  begin with mm do begin
        PopUp:=false;x:=0;y:=0;FrameWidth:=_single;
        StrUp:='';StrDn:='';Breaks:='';
  end              end;

procedure PushMenu(var adr);
  type PtrStr=^Str;
       Str=record next:PtrStr;
                  ss:string[1];
           end;
  var s:ScreenStr; attrs:WwAttrs;
      p:^char; ofsP:word absolute p; first,last:PtrStr; help:pointer;
      c,r,i,j,maxlen,sum:word; hor:boolean;
  begin SaveWwAttr(attrs);
        c:=mm.x; if c=0 then c:=WwX;
        r:=mm.y; if r=0 then r:=WwY;
        p:=addr(adr);if p^='/' then begin hor:=true;inc(OfsP) end
                               else hor:=false;
        maxlen:=0;sum:=0;
        repeat s:='';
               repeat s:=s+p^;inc(ofsP);
               until p^='/';
               inc(ofsP);inc(sum);
               GetMem(help,sizeof(str)+pred(length(s))); PtrStr(help)^.next:=nil;
               if sum=1 then First:=help else Last^.next:=help; Last:=help;
               move(s[0],last^.ss[0],succ(length(s)));
               if hor then maxlen:=maxlen+length(s)
                      else if length(s)>maxlen then maxlen:=length(s);
        until  p^='/';
        help:=TopMenu;
        GetMem(pointer(TopMenu),sizeof(MenuItem)+pred(sum)*5); {sizeof(MenuItem.name[i])};
        TopMenu^.next:=PtrMenu(help);TopMenu^.sum:=sum;TopMenu^.PopUp:=mm.PopUp;
        TopMenu^.breaks:=mm.breaks;
        if hor then PushWw(c,r,maxlen+(sum+2)*3,1) {???}
               else begin PushWw(c,r,maxlen+4,sum+2); TextAttr:=Colors.mFrame;
                          with mm do Frame(FrameWidth,StrUp,StrDn)
                    end;
        TopWw^.typ:='M'; TopMenu^.w:=TopWw;
        TextAttr:=Colors.mItem;
        for i:=1 to sum do begin if hor then write('   ') else gotoxy(2,i);
                                 with TopMenu^ do with first^ do
                                 begin name[i,1]:=AbsWhereX;name[i,2]:=AbsWhereY;
                                       name[i,3]:=length(ss);
                                       j:=1; while (j<=length(ss)) and ((ss[j]<'A') or (ss[j]>'Z')) do inc(j);
                                       if j>length(ss) then j:=1; name[i,4]:=j; name[i,5]:=byte(ss[j]); write(ss); pos:=i;
                                       if i=1 then MenuAttr(high) else MenuAttr(low);
                                       first:=next;
                           end   end;
        if hor then write('   ');
        help:=first; while help<>nil do begin FreeMem(help,sizeof(str)+pred(length(s)));
                                              help:=PtrStr(help)^.next;
                                        end;
        TopMenu^.pos:=1;
        SetMPars; LoadWwAttr(attrs);
  end;
procedure PopMenu;
  begin if TopMenu=nil then exit;
        FreeMem(pointer(TopMenu),sizeof(MenuItem)+pred(TopMenu^.sum)*5); {sizeof(MenuItem.name[i])}
        PopWw; TopMenu:=TopMenu^.next;
  end;
function Menu:word;
  var ch:char; r:word; attrs:WwAttrs;
  procedure Return(m:word);
    begin menu:=m; if KeyCode=_FunKey then exit;
          if ((TopMenu^.PopUp) or (ch=_ESC)) and (TopMenu^.next<>nil)
          then PopMenu else LoadWwAttr(attrs);
    end;
  begin with TopMenu^ do begin
          SaveWwAttr(attrs); with w^ do window(xc,xr,yc,yr); CursorOff;
          if name[1,2]<name[sum,2] then TruncFrame(_single);
          MenuAttr(high);
          repeat ch:=MyReadKey(_uppercase);
                 case KeyCode of
                 _FunKey :if system.pos(ch,breaks)<>0 then begin return($FF); exit end;
                 _CharKey:begin r:=0;
                                repeat inc(r);
                                until  (name[r,5]=byte(ch)) or (r>sum);
                                if r<=sum then begin MenuAttr(low);pos:=r;MenuAttr(high);
                                                     return(pos);exit;
                          end                  end;
                 _CtrlKey:case ch of
                           _CR:begin return(pos);exit end;
                          _ESC:begin return(0);exit end;
                          end;
                 _MoveKey:case ch of
                     _left,_up:begin MenuAttr(low);dec(pos);if pos<1 then pos:=sum;MenuAttr(high) end;
                  _right,_down:begin MenuAttr(low);inc(pos);if pos>sum then pos:=1;MenuAttr(high) end;
                 end      end;
          until false;
  end                    end;
procedure MenuOff;
  begin MenuAttr(low) end;

function WwX :word;
  var p:PtrWw;
  begin p:=TopWw; while (p<>nil) and (p^.typ='W') do p:=p^.next;
        if p=nil then WwX:=Xcol+whereX-1 else case p^.typ of
           'M':with TopMenu^ do WwX:=name[pos,1]+1;
           'S':with TopSel^ do WwX:=w^.xc+2;
          else WwX:=AbsWhereX+1;
  end                                       end;
function WwY :word;
  var p:PtrWw;
  begin p:=TopWw; while (p<>nil) and (p^.typ='W') do p:=p^.next;
        if p=nil then WwY:=Xrow+whereY-1 else case p^.typ of
           'M':with TopMenu^ do WwY:=name[pos,2]+1;
           'S':with TopSel^ do WwY:=w^.xr+1+sv.r;
          else WwY:=AbsWhereY+1;
  end                                       end;

{ *** applications *** }

function SelectDiskFile (init:PathStr; HdStr:ScreenStr):PathStr;
  var act,mask,name:string; SR:SearchRec; ext:string3; ch:string1; d,p,t,n:PathStr;
      xx,yy:word;
  procedure Step;
    begin FindFirst(act+ch+mask+#00,0,SR);
          while DosError=0 do begin PutSelect(SR.name);
                                    FindNext(SR);
                              end;
          FindFirst(act+ch+'*.*'+#00,Directory,SR);
          while DosError=0 do begin if (SR.Attr=Directory) and (SR.name<>'.')
                                    then PutSelect('\'+SR.name);
                                    FindNext(SR);
                              end;
          ss.x:=xx; ss.y:=yy; ss.StrUp:=HdStr; ss.StrDn:=act+ch+mask;
          ss.PopUp:=true; ss.Abcd:=true; ss.Maxlen:=12; ss.Tab:=4;
          PushSelString; name:=SelString;
    end;
  label 1;
  begin SelectDiskFile:=''; xx:=ss.x; yy:=ss.y; PushWw(xx,yy,24,3);
        if init='' then init:='*.*';
        TextAttr:=Colors.pFrame; Frame(_single,HdStr,'');
        gotoxy(2,1); TextAttr:=Colors.pText; mask:=init;
        DivFileName(mask,d,p,n,t); if t='' then ext:='*' else ext:=t;
        EditLine(mask,1,pred(sizeof(mask)),20,_filename); PopWw;
        if LastCh=_ESC then exit; DivFileName(mask,d,p,n,t);
        if n='' then mask:='*' else mask:=n; mask:=mask+'.';
        if t='' then mask:=mask+ext else mask:=mask+t;
        if (d+p)='' then GetDir(0,act) else act:=d+p;
        if act[length(act)]<>'\' then ch:='\' else ch:='';
        if (pos('*',mask)=0) and (pos('?',mask)=0) and (n<>'')
        then begin SelectDiskFile:=act+ch+mask; exit end;
      1:step;
        if name[1]='\' then begin dec(name[0]);move(name[2],name[1],byte(name[0]));
                                  if name='..' then begin repeat dec(act[0]) until act[length(act)]='\';ch:='' end
                                               else begin act:=act+ch+name;ch:='\' end;
                                  goto 1;
                            end;
        if name<>'' then SelectDiskFile:=act+ch+name;
  end;

(*procedure OSshell;
  begin
{$IFDEF SAVESCREEN}
        LoadWw(OrgScr);
{$ELSE}
        clrscr;
{$ENDIF}
        writeln('type EXIT to return');
        CallPgm(EnvrParStr('COMSPEC'),'');
{$IFDEF SAVESCREEN}
        UpdateWw(OrgScr); PopWw;
{$ENDIF}
  end;*)

var ExitSave:pointer;

{$F+} procedure MyExit; {$F-}
  begin
{$IFDEF SAVESCREEN}
        LoadWw(OrgScr);
{$ENDIF}
        ExitProc:=ExitSave;
  end;

begin TxtCols:=ActCols; TxtRows:=ActRows;
      KeyCode:=_NoKey; LastCh:=#00; HotKeys:=false; KeyBuf:=''; ClrUpdate;
      TopBreak:=nil; BreakInt:=false;
      TopWw:=nil; ActWw:=nil; WwError:=0;
      TopSel:=nil; FirstStr:=nil; LastStr:=nil; SetSPars;
      TopMenu:=nil; SetMPars;
      if LastMode=Mono then Colors:=MonoColors
                       else if EGApresent then Colors:=COColors
                                          else Colors:=BWColors;
      ExitSave:=ExitProc; ExitProc:=@MyExit;
{$IFDEF SAVESCREEN}
      OrgScr:=SaveWw; clrscr; CursorOff;
{$ENDIF}
end.