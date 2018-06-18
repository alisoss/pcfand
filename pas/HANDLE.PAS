var OldNumH:word;
const OldHTPtr:pointer=nil;
{$ifdef FandDemo}
  const files = 30;
{$else}
  const files = 250; {files in CONFIG.SYS -3}
{$endif}
var CardHandles:word;
    Handles,UpdHandles,FlshHandles:set of 0..254;
{$S-}
procedure SetRes(FLAGS,AX:word);
  begin if(FLAGS and FCarry)=0 then HandleError:=0 else HandleError:=AX end;
function IsHandle(H:word):boolean;
  begin IsHandle:=(H<>$FF) and (H in Handles) end;
function IsUpdHandle(H:word):boolean;
  begin IsUpdHandle:=(H<>$FF) and (H in UpdHandles) end;
function IsFlshHandle(H:word):boolean;
  begin IsFlshhandle:=(H<>$FF) and (H in FlshHandles) end;
procedure SetHandle(H:word);
  begin if H=$FF then exit; Handles:=Handles+[H]; inc(CardHandles) end;
procedure SetUpdHandle(H:word);
  begin if H=$FF then exit; UpdHandles:=UpdHandles+[H] end;
procedure SetFlshHandle(H:word);
  begin if H=$FF then exit; FlshHandles:=FlshHandles+[H] end;          
procedure ResetHandle(H:word);
  begin if H=$FF then exit; Handles:=Handles-[H]; dec(CardHandles) end;
procedure ResetUpdHandle(H:word);
  begin if IsUpdHandle(H) then begin
    UpdHandles:=UpdHandles-[H]; SetFlshHandle(H) end end;
procedure ResetFlshHandle(H:word);
  begin if H=$FF then exit; FlshHandles:=FlshHandles-[H] end;
procedure ClearHandles;
  begin Handles:=[]; CardHandles:=0 end;
procedure ClearUpdHandles;
  begin UpdHandles:=[] end;
procedure ClearFlshHandles;
  begin FlshHandles:=[] end;
function IsNetCVol:boolean;
begin
{$ifdef FandNetV}
  IsNetCVol:=(CVol='#') or (CVol='##') or SEquUpcase(CVol,'#R');
{$else}
  IsNetCVol:=false;
{$endif}
end;

procedure ExtendHandles;
begin
  FillChar(NewHT,sizeof(NewHT),$FF); with CurPSP^ do begin
    OldHTPtr:=HanPtr; OldNumH:=NumHan; Move(OldHTPtr^,NewHT,OldNumH);
    HanPtr:=@NewHT; NumHan:=sizeof(NewHT)-1 end;
  ClearHandles; ClearUpdHandles; ClearFlshHandles;
end;

procedure UnExtendHandles;
var h:word;
begin
  for h:=0 to files do if IsHandle(h) then CloseH(h);
  if OldHTPtr=nil then exit;
  for h:=OldNumH to 255 do if NewHT[h]<>$FF then CloseH(h);
  Move(NewHT,OldHTPtr^,OldNumH);
  with CurPSP^ do begin NumHan:=OldNumH; HanPtr:=OldHTPtr end;
  OldHTPtr:=nil;
end;

function MoveH(dist:longint;method,handle:word):longint;
var r:registers;
begin with r do begin
  AH:=$42; AL:=method; BX:=handle;
  CX:=memW[seg(dist):ofs(dist)+2]; DX:=memW[seg(dist):ofs(dist)];
  MsDos(r); SetRes(FLAGS,AX); MoveH:=(longint(DX) shl 16)+AX;
end end;

function PosH(handle:word):longint;
  begin PosH:=MoveH(0,1,handle) end;

procedure SeekH(handle:word;pos:longint);
begin
  if handle=$ff then RunError(705); MoveH(pos,0,handle);
end;


function FileSizeH(handle:word):longint;
var pos:longint;
begin
  pos:=PosH(handle);FileSizeH:=MoveH(0,2,handle);SeekH(handle,pos);
end;

{ define nolock}

function TryLockH(Handle:word;Pos:longint;Len:word):boolean;
{$ifndef nolock} assembler;
asm  mov ah,5cH; mov al,0; mov bx,Handle; les dx,Pos; mov cx,es;
     mov di,Len; xor si,si; int 21H; mov ax,0; jc @1; mov ax,1; @1:
{$else} begin TryLockH:=true; {$endif}
end;

procedure UnLockH(Handle:word;Pos:longint;Len:word);
{$ifndef nolock} assembler;
asm  mov ah,5cH; mov al,1; mov bx,Handle; les dx,Pos; mov cx,es;
     mov di,Len; xor si,si; int 21H;
{$else} begin {$endif}
end;

function OpenH(Mode:FileOpenMode;UM:FileUseMode):word;
var r:registers; w:longint; path:PathStr;
    s:string;
const Txt:array[FileUseMode]of string[4]=('Clos','OpRd','OpRs','OpSh','OpEx');
label 1;
begin
  path:=CPath+#0; if CardHandles=files then RunError(884);
  w:=0; with r do begin
1:case Mode of
   _isoldfile,_isoldnewfile: AH:=$3D; _isoverwritefile: AH:=$3C;
   _isnewfile: AH:=$5B end;
  DS:=seg(path);DX:=ofs(path)+1;CX:=0;
  case UM of
    RdOnly:AL:=$20; RdShared:AL:=$40; Shared:AL:=$42; Exclusive:AL:=$12 end;
  MSDos(r); SetRes(r.FLAGS,r.AX);
  if IsNetCVol{Retry} and (HandleError in [$05,$21]) then begin
    if w=0 then begin Set2MsgPar(path,Txt[UM]); w:=PushWrLLMsg(825,false) end;
    LockBeep; KbdTimer(spec.NetDelay,0); goto 1 end;
  OpenH:=$FF;
  case HandleError of
   0: begin OpenH:=AX; SetHandle(AX); if Mode<>_isoldfile then SetUpdHandle(AX) end;
   2 {frueher 5,80,150}:if Mode=_isoldnewfile then begin Mode:=_isnewfile; goto 1 end end;
  if w<>0 then PopW(w);
  {Mark*** problemy pokrac disket pri zalohovani}
  {
  if HandleError <> 0 then begin
     str(HandleError,s);
     setMsgPar('open='+s);
     wrllF10msg(862);
  end
  }
end end;

function ReadH(handle,bytes:word;var buffer):word;
var r:registers;
begin with r do begin
  if handle=$ff then RunError(706);
  AH:=$3F; DS:=seg(buffer); DX:=ofs(buffer); CX:=bytes; BX:=handle;
  MsDos(r); SetRes(FLAGS,AX); ReadH:=AX;
end end;

procedure ReadLongH(handle:word;bytes:longint;buf:pointer);
begin
  while bytes>=$7ff0 do begin
    ReadH(handle,$7ff0,buf^); dec(bytes,$7ff0); inc(PtrRec(buf).Seg,$7ff) end;
  ReadH(handle,bytes,buf^);
end;

procedure WriteH(handle,bytes:word;var buffer);
var r:registers; s:string;
begin with r do begin
  if handle=$ff then RunError(706);
  AH:=$40; DS:=seg(buffer); DX:=ofs(buffer); CX:=bytes; BX:=handle;
  MsDos(r); SetRes(FLAGS,AX); SetUpdHandle(handle);
  if (HandleError=0)and(AX<>bytes) then HandleError:=1;
  {mark*** problemy pokrac disket pri zalohovani}
  {
  if HandleError <> 0 then begin
     str(HandleError,s);
     setMsgPar('write='+s);
     wrllF10msg(862);
  end
  }

end end;

procedure WriteLongH(handle:word;bytes:longint;buf:pointer);
begin
  while bytes>=$7ff0 do begin
    WriteH(handle,$7ff0,buf^); dec(bytes,$7ff0); inc(PtrRec(buf).Seg,$7ff) end;
  WriteH(handle,bytes,buf^);
end;

procedure CloseH(handle:word);
var r:registers;
begin with r do begin
  if handle=$FF then exit; AH:=$3E; BX:=handle; MsDos(r); SetRes(FLAGS,AX);
  if HandleError=0 then begin
    ResetHandle(handle); ResetUpdHandle(handle); ResetFlshHandle(handle) end;
end end;

procedure FlushH(handle:word);
var r:registers;
begin with r do begin
  if handle=$ff then exit; AH:=$45; BX:=handle; MsDos(r); SetRes(FLAGS,AX);
  if HandleError=0 then begin SetHandle(AX); SetUpdHandle(AX); CloseH(AX) end;
end end;

procedure FlushHandles;
var h:word;
begin
  if CardHandles=files then exit; for h:=0 to files do
    if IsUpdHandle(h) or IsFlshHandle(h) then FlushH(h);
  ClearUpdHandles; ClearFlshHandles;
end;


procedure TruncH(handle:word;N:longint);
  begin if handle=$FF then exit; if FileSizeH(handle)>N then begin
        SeekH(handle,N); WriteH(handle,0,ptr(0,0)^) end end;

procedure CloseClearH(var h:word);
  begin if h=$FF then exit; CloseH(h); ClearCacheH(h); h:=$FF end;

procedure SetFileAttr(Attr:word); assembler;
asm  lea si,CPath; lodsb; mov dx,si; xor ah,ah; add si,ax; mov [si],ah;
     push ds; mov cx,Attr; mov ax,4301H; int 21H;
     jc @1; xor ax,ax; @1:pop ds; mov HandleError,ax;
end;

function GetFileAttr:word; assembler;
asm  lea si,CPath; lodsb; mov dx,si; xor ah,ah; add si,ax; mov [si],ah;
     push ds; mov ax,4300H; int 21H;
     jc @1; xor ax,ax; jmp @2; @1:xor cx,cx; @2:pop ds;
     mov HandleError,ax; mov ax,cx;
end;


procedure RdWrCache(ReadOp:boolean;Handle:word;NotCached:boolean;Pos:longint;
         N:word;var Buf);
var
  PgeIdx,PgeRest:integer; err:word; PgeNo:longint; Z:CachePagePtr;
begin
  if Handle=$FF then RunError(706);
  if NotCached then begin
    SeekH(Handle,Pos);
    if ReadOp then ReadH(Handle,N,Buf) else WriteH(Handle,N,Buf);
    if HandleError=0 then exit;
    err:=HandleError; SetCPathForH(Handle); SetMsgPar(CPath);
    RunError(700+err) end;
  asm mov ax,Pos.word; mov dx,CachePageSize; mov cx,dx; dec cx;
      and ax,cx; mov PgeIdx,ax; sub dx,ax; mov PgeRest,dx end;
  PgeNo:= Pos shr CachePageShft;
  Z:=Cache(Handle,PgeNo);
  while N>PgeRest do begin
    if ReadOp then move(Z^.Arr[PgeIdx],Buf,PgeRest)
    else begin move(Buf,Z^.Arr[PgeIdx],PgeRest); Z^.Upd:=true end;
    asm mov ax,Buf.word; add ax,PgeRest; mov Buf.word,ax end;
    dec(N,PgeRest); inc(PgeNo);
    Z:=Cache(Handle,PgeNo); PgeRest:=CachePageSize; PgeIdx:=0;
    end;
  if ReadOp then move(Z^.Arr[PgeIdx],Buf,N)
  else begin move(Buf,Z^.Arr[PgeIdx],N); Z^.Upd:=true; SetUpdHandle(Handle) end;
end;


{_________________________________________________________}


function GetDateTimeH(handle:word):longint;
  var r:registers;
  begin with r do
          begin AH:=$57;AL:=0;BX:=handle;
            MsDos(r);SetRes(FLAGS,AX);
            GetDateTimeH:=(longint(DX) shl 16)+CX;
           end
  end;

procedure DeleteFile(path:PathStr);
var r:registers;
begin
  path:=path+#$00; with r do begin
    AH:=$41; DS:=seg(path); DX:=ofs(path)+1; MsDos(r); SetRes(FLAGS,AX);
end end;

procedure RenameFile56(OldPath,NewPath:PathStr;Msg:boolean);
var r:registers;
begin
  OldPath:=OldPath+#00; NewPath:=NewPath+#00; with r do begin
    AH:=$56; DS:=seg(OldPath); DX:=ofs(OldPath)+1;
    ES:=seg(NewPath); DI:=ofs(NewPath)+1; MsDos(r); SetRes(FLAGS,AX);
    if Msg and (HandleError<>0) then begin
      Set2MsgPar(OldPath,NewPath); RunError(829) end end;
end;
                                                      
function MyFExpand(Nm:String12;EnvName:String8):PathStr;
var p:PathStr; d,f:DirStr;
begin
  GetDir(0,d);
  f:=FandDir; DelBackSlash(f);
  ChDir(f);
  p:=GetEnv(EnvName); AddBackSlash(p);
  if p<>'' then p:=p+Nm else begin
    p:=FSearch(Nm,GetEnv('PATH')); if p=''then p:=Nm end;
  MyFExpand:=FExpand(p); ChDir(d);
end;
{$S+}