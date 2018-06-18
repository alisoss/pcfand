function RdNextPart:boolean;
  var ppa:CharArrPtr; L11,L1,iL,MI:word;
      BL,FSize,Rest,Max,Pos:longint;
      Pass:word;
  label 1;
  procedure LastLine(from,num:word;var Ind,Count:word);
    var C:^char; COfs:word absolute C;
        i:word;
    begin Count:=0;Ind:=from;C:=@T^[from];
      for i:=1 to num do
        begin inc(COfs);if C^=_CR then begin inc(Count);Ind:=from+i end end;
      if (Count>0)and(T^[Ind+1]=_LF) then inc(Ind);
    end;
  begin      {*****NextPart****}
    Max:=MinL(MaxLenT,StoreAvail+LenT);Pass:=Max-(Max shr 3);
{    Pass:=MaxLenT-(MaxLenT shr 3);}
    Part.MovL:=0;Part.MovI:=0;MI:=0;RdNextPart:=false;
    if AllRd then exit;
    if Part.LenP=0 then begin LenT:=0;T:=GetStore(0) end;
    with Part do
      begin Pos:=PosP;BL:=LineP;
        if LenT<=(Pass shr 1) then goto 1;
        LastLine(0,LenT-(Pass shr 1),MI,MovL);     { 28kB+1 radek}
        SetColorOrd(ColorP,1,MI+1);
        inc(Pos,MI);dec(LenT,MI);
        move(T^[MI+1],T^[1],LenT);ReleaseStore(@T^[LenT+1]);
      end;
    with Part do begin LineP:=BL+MovL;PosP:=Pos;MovI:=MI end;
  1:inc(Pos,LenT);
    FSize:=FileSizeH(TxtFh); AllRd:=false;iL:=LenT;
    repeat Rest:=FSize-Pos;
      if Rest>$1000 then L11:=$1000 else L11:=Rest;
      Max:=StoreAvail;if Max>$400 then dec(Max,$400);
      if L11>Max then L11:=Max;
      ppa:=GetStore(L11); L1:=L11;
      if L1>0 then
        begin SeekH(TxtFh,Pos);ReadH(TxtFh,L1,ppa^);
{          if ppa^[L1]=^z then
            begin ReleaseStore(@ppa^[L1]);dec(L1) end} end;
      AllRd:=Pos+L11>=FSize;
      inc(LenT,L1);inc(Pos,L1);
    until (LenT>Pass) or AllRd or (L11=Max);
    LastLine(iL,LenT-iL-1,iL,L1);
    if AllRd then iL:=LenT;
    if (iL<LenT) then begin LenT:=iL;AllRd:=false end;
    with Part do begin LenP:=LenT;UpdP:=false end;
    if (T^[LenT]=^z)and AllRd then dec(LenT);
    if (LenT<=1) then exit;  {????????}
    ReleaseStore(@T^[succ(LenT)]);
    RdNextPart:=true;
  end;


function RdPredPart:boolean;
  var ppa:CharArrPtr; L1,L11,MI:word;
      BL,FSize,Rest,Max,Pos:longint;
      Pass:word;
  label 1;
  procedure FirstLine(from,num:word;var Ind,Count:word);
    var C:^char; COfs:word absolute C;
        i:word;
    begin Count:=0;Ind:=from-1;C:=@T^[from];
      for i:=0 to num-1 do
        begin dec(COfs);if C^=_CR then begin inc(Count);Ind:=from-i end end;
      if (Count>0)and(T^[Ind+1]=_LF) then inc(Ind);
    end;
  begin      {*****PredPart****}
    Max:=MinL(MaxLenT,StoreAvail+LenT);Pass:=Max-(Max shr 3);
{    Pass:=MaxLenT-(MaxLenT shr 3);}
    Part.MovL:=0;MI:=0;RdPredPart:=false;
    if Part.PosP=0 then exit;
    with Part do
      begin Pos:=PosP;BL:=LineP;
        if LenT<=(Pass shr 1) then goto 1;
        FirstLine(LenT+1,LenT-(Pass shr 1),L1,L11);
        if L1<LenT then
          begin AllRd:=false;LenT:=L1;ReleaseStore(@T^[LenT+1]) end;
      end;
  1:L11:=LenT;
    repeat
      if Pos>$1000 then L1:=$1000 else L1:=Pos;
      Max:=StoreAvail;if Max>$400 then dec(Max,$400);
      if L1>Max then L1:=Max;ppa:=GetStore(L1); move(T^,T^[L1+1],LenT);
      if L1>0 then
        begin SeekH(TxtFh,Pos-L1);ReadH(TxtFh,L1,T^) end;
      inc(LenT,L1);dec(Pos,L1);
    until (LenT>Pass)or(Pos=0)or(L1=Max);
    L11:=LenT-L11;FirstLine(L11+1,L11,MI,Part.MovL);
    if Pos=0 then MI:=L11
    else if Part.MovL>0 then begin dec(Part.MovL);MI:=L11-MI end;
    L1:=L11-MI;dec(LenT,L1);inc(Pos,L1);
    if L1>0 then
      begin move(T^[L1+1],T^,LenT);
        ReleaseStore(@T^[LenT+1]) end;
    with Part do
      begin PosP:=Pos;LineP:=BL-MovL;LenP:=LenT;MovI:=MI;UpdP:=false;
        SetColorOrd(ColorP,1,MI+1) end;
    if (LenT=0) then exit;  {????????}
    RdPredPart:=true;
  end;

procedure UpdateFile;
  var n,HErr:word; d,pos1,pos:longint;  p:pointer;
  begin
    n:=$FFF0; if n>StoreAvail then n:=StoreAvail; p:=GetStore(n);
    d:=longint(LenT)-Part.LenP; HErr:=0;
    pos1:=Part.PosP+Part.LenP;
    if d>0 then
      begin pos:=AbsLenT;
        while pos>pos1 do begin if pos-pos1<n then n:=pos-pos1;dec(pos,n);
                          SeekH(TxtFh,pos);ReadH(TxtFh,n,p^);
                          SeekH(TxtFh,pos+d);WriteH(TxtFh,n,p^);
                          if HandleError<>0 then HErr:=HandleError end;
      end
    else if d<0 then
      begin pos:=pos1;
        while pos<AbsLenT do
          begin if pos+n>AbsLenT then n:=AbsLenT-pos;
            SeekH(TxtFh,pos);ReadH(TxtFh,n,p^);
            SeekH(TxtFh,pos+d);WriteH(TxtFh,n,p^);inc(pos,n) end;
        TruncH(TxtFh,AbsLenT+d)
      end;
    ReleaseStore(p);
    SeekH(TxtFh,Part.PosP);if LenT>0 then WriteH(TxtFh,LenT,T^);
    if HandleError<>0 then HErr:=HandleError;
    FlushH(TxtFh); AbsLenT:=FileSizeH(TxtFh);
    if HErr<>0 then begin SetMsgPar(TxtPath);WrLLF10Msg(700+HErr) end;
    with Part do begin UpdP:=false;LenP:=LenT;
                   if (PosP<400)and(LenP>$400) then UpdPHead:=false end;
  end;

procedure RdPart;
  begin LenT:=Part.LenP;T:=GetStore(LenT);if LenT=0 then exit;
    SeekH(TxtFh,Part.PosP);ReadH(TxtFh,LenT,T^) end;
procedure NullChangePart;
  begin ChangePart:=false;Part.MovI:=0;Part.MovL:=0 end;
procedure RdFirstPart;
  begin NullChangePart;
    with Part do begin PosP:=0;LineP:=0;LenP:=0;ColorP:='' end;
    AllRd:=false;ChangePart:=RdNextPart;
  end;

procedure OpenTxtFh(Mode:char);
  var UM:FileUseMode;
  begin
    CPath:=TxtPath; CVol:=TxtVol;TestMountVol(CPath[1]);
    if Mode=ViewM then UM:=RdOnly else UM:=Exclusive;
    TxtFH:=OpenH(_isoldnewfile,UM);
    if HandleError<>0 then begin
      SetMsgPar(CPath);RunError(700+HandleError) end;
    AbsLenT:=FileSizeH(TxtFH);
  end;
