
              { TYPE ACCESS }

procedure UnPack(var PackArr;var NumArr;NoDigits:word); assembler;
asm  push ds; cld; lds si,PackArr; les di,NumArr; mov dx,NoDigits; mov cl,4;
@1:  lodsb; mov ah,al; shr al,cl; add al,30H; stosb; dec dx; jz @2;
     mov al,ah; and al,0fH; add al,30H; stosb; dec dx; jnz @1;
@2:  pop ds;
end;

type ByteMask=array[1..maxint]of byte;

procedure Pack(var NumArr;var PackArr;NoDigits:word);
  var source:ByteMask absolute NumArr;
      target:ByteMask absolute PackArr;
      i:word;
  begin for i:=1 to (NoDigits shr 1) do
          target[i]:=((source[(i shl 1)-1] and $0F) shl 4)
                     or (source[i shl 1] and $0F);
        if odd(NoDigits) then
          target[(NoDigits shr 1)+1]:=(source[NoDigits] and $0F) shl 4;
  end;

const {$ifdef Coproc} DblS=8 {$else} DblS=5 {$endif};
      FixS=8;
type Fix=array[1..FixS] of byte;
     RealMask=array[0..DblS] of byte;
     Dbl=array[1..DblS]of byte;

function RealFromFix(var FixNo;FLen:word):float;
{$ifdef Coproc}
  var r:float;rr:Dbl absolute r;
      ff:Fix;
      exp,first,lef,rig,i:integer;
      neg:boolean;
      b:byte;
  label 1;
  begin fillchar(rr,DblS,0);move(FixNo,ff,flen);
    neg:=(ff[1] and $80)<>0;
    if neg then
      begin
        if ff[1]=$80 then begin
          for i:=2 to flen do if ff[i]<>$00 then goto 1;   {NULL value}
          RealFromFix:=0.; exit end;
1:      for i:=1 to flen do ff[i]:=not(ff[i]);inc(ff[flen]);
        i:=flen;while ff[i]=0 do begin dec(i);if i>0 then inc(ff[i]) end end;
    first:=1;while ff[first]=0 do inc(first);
    if first>flen then begin RealFromFix:=0;exit end;
    lef:=0;b:=ff[first];while (b and $80)=0 do begin b:=b shl 1;inc(lef) end;
    ff[first]:=ff[first] and ($7F shr lef);exp:=(flen-first)shl 3-lef+1030;
    if lef=7 then inc(first);lef:=(lef+5)and $07;rig:=8-lef;i:=DblS-1;
    if (rig<=4)and(First<=flen) then begin rr[i]:=ff[first] shr rig;dec(i) end;
    while (i>0)and(first<flen) do
      begin
       rr[i]:=ff[first]shl lef+ff[first+1]shr rig;dec(i);inc(first) end;
    if (first=flen)and(i>0) then rr[i]:=ff[first]shl lef;
    rr[DblS-1]:=rr[DblS-1]and $0F+(exp and $0F)shl 4;rr[DblS]:=exp shr 4;
    if neg then rr[DblS]:=rr[DblS] or $80;
    RealFromFix:=r;
  end;
{$else}
  type RealFix=array[0..FixS]of byte;
  var RealNo:RealMask;r:float absolute RealNo;
      F:Fix;
      neg:boolean;first,last,left,right,i,j:word;b:byte;
  label 1;
  begin fillchar(RealNo,DblS+1,0);move(FixNo,F,FLen);neg:=(F[1] and $80)<>0;
               if neg then begin
                if F[1]=$80 then begin
                  for i:=2 to flen do if F[i]<>$00 then goto 1;   {NULL value}
                  RealFromFix:=0.; exit end;
        1:      for i:=1 to flen do F[i]:=not(F[i]); inc(F[FLen]); i:=fLen;
                while F[i]=0 do begin dec(i); if i>0 then inc(F[i]) end end;
               first:=1;while F[first]=0 do inc(first);
               if first>FLen then {zero} else
                 begin left:=0;b:=F[first];
                   while (b and $80)=0 do begin b:=b shl 1;inc(left) end;
                   right:=8-left;i:=first;j:=DblS;
                   while (j>0)and(i<FLen) do
                     begin RealNo[j]:=(F[i] shl left)or(F[i+1] shr right);
                       inc(i);dec(j) end;
                   if (i=Flen)and(j>0) then RealNo[j]:=F[i] shl left;
                   RealNo[0]:=(FLen-first)*8+right+$80;
                   RealNo[5]:=RealNo[5] and $7F;
                   if neg then RealNo[5]:=RealNo[5] or $80;
                 end;
        RealFromFix:=r;
  end;
{$endif}
procedure FixFromReal(r:float;var FixNo;flen:word);
{$ifdef Coproc}
  var rr:Dbl absolute r;
      ff:Fix absolute FixNo;
      exp,first,rig,lef,i:integer;
      neg:boolean;
  begin fillchar(ff,flen,0);if r>0 then r:=r+0.5 else r:=r-0.5;
    neg:=boolean(rr[DblS]and $80);
    exp:=rr[DblS-1] shr 4+word(rr[DblS] and $7F) shl 4;
    if exp<2047 then
      begin
        rr[DblS]:=0;rr[DblS-1]:=rr[DblS-1] and $0F;
        if exp>0 then rr[DblS-1]:=rr[DblS-1] or $10 else inc(exp);dec(exp,1023);
        if exp>(flen shl 3)-1 then {overflow} exit;
        lef:=(exp+4) and $0007;rig:=8-lef;
        if (exp and $0007)>3 then inc(exp,4);first:=7-exp shr 3;i:=flen;
        while (first<DblS)and(i>0) do
          begin ff[i]:=rr[first] shr rig+rr[first+1] shl lef;
            dec(i);inc(first) end;
        if i>0 then ff[i]:=rr[first] shr rig;
        if neg then
          begin for i:=1 to flen do ff[i]:=not ff[i];inc(ff[flen]);
            i:=flen;while ff[i]=0 do begin dec(i);
            if i>0 then inc(ff[i]) end end;
      end;
  end;
{$else}
  var RealNo:RealMask absolute r;
      exp:byte absolute r;
      f:Fix absolute FixNo;
      rexp,fexp,rlow,fhigh,left,right,i,j:integer;
      neg:boolean;
  begin FillChar(f,flen,0);
      if r>0 then r:=r+0.5 else r:=r-0.5;
      rexp:=exp-$80;fexp:=(flen shl 3)-1;
      if rexp<1 then {zero} else if rexp>fexp then {overflow} else
        begin neg:=(RealNo[DblS] and $80)=$80;
            RealNo[DblS]:=RealNo[DblS] or $80;
            rlow:=DblS-(rexp shr 3);fhigh:=((fexp-rexp)shr 3)+1;
            left:=rexp and $0007;right:=8-left;
            f[fhigh]:=RealNo[DblS] shr right;
            j:=fhigh+1; i:=4;
            while (i>0)and(i>=rlow) do
              begin f[j]:=(RealNo[i] shr right)or(RealNo[i+1] shl left);
                inc(j);dec(i) end;
            if (rlow<=0)and(j<=flen) then f[j]:=RealNo[1] shl left;
            if neg then begin
                          for i:=1 to flen do f[i]:=not(f[i]);
                          inc(f[flen]);i:=flen;
                          while f[i]=0 do begin dec(i);inc(f[i]) end;
                        end;
        end;
  end;
{$endif}
