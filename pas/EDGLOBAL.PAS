function RdFldNameFrmlT(var FTyp:char):FrmlPtr; far;
begin Error(8) end;

procedure MyWrLLMsg(s:string);
  begin if HandleError=4 then s:='';SetMsgPar(s);WrLLF10Msg(700+HandleError);
  end;
procedure MyRunError(s:string;n:word);
  begin SetMsgPar(s);RunError(n) end;
procedure HMsgExit(s:pathstr);
  begin case HandleError of
            0  :exit;
            1  :begin s:=s[1];SetMsgPar(s);RunError(700+HandleError) end;
            2,3:begin SetMsgPar(s);RunError(700+HandleError) end;
            4  :RunError(704) end;
  end;

function FindChar(var Num:word;C:char;Pos:word;Len:word):word;
var I,J,K:word;cc:char;
begin I:=Len;J:=Num;K:=Pos-1;cc:=C;
      inline($C4/$3E/T/           {       les DI,addr(T)     }
             $03/$BE/K/           {       add DI,K           }
             $8B/$8E/I/           {       mov CX,[BP+I]      }
             $2B/$8E/K/           {       sub CX,[BP+K]      }
             $8B/$9E/J/           {       mov BX,[BP+J]      }
             $31/$D2/             {       xor DX,DX          }
             $8A/$86/cc/          {       mov AL,[BP+cc]     }
             $FC/                 {       cld                }
             $42/                 { 1:    inc DX             }
             $F2/$AE/             {       repnz scasb        }
             $75/$04/             {       jnz 2              }
             $39/$DA/             {       cmp DX,BX          }
             $75/$F7/             {       jnz 1              }
             $89/$8E/I/           { 2:    mov [BP+I],CX      }
             $89/$96/J    );      {       mov [BP+J],BX      }
      FindChar:=Len-I;Num:=J;
end;

(*function FindCharBack(var Num:word;C:char;Pos:word;Len:word):word;
var I,J,K:word;cc:char;
begin I:=Len;J:=Num;K:=Pos-1;cc:=C;
      inline($C4/$3E/T/           {       les DI,addr(T)     }
             $03/$BE/K/           {       add DI,K           }
             $8B/$8E/I/           {       mov CX,[BP+I]      }
             $2B/$8E/K/           {       sub CX,[BP+K]      }
             $8B/$9E/J/           {       mov BX,[BP+J]      }
             $31/$D2/             {       xor DX,DX          }
             $8A/$86/cc/          {       mov AL,[BP+cc]     }
             $FD/                 {       std                }
             $42/                 { 1:    inc DX             }
             $F2/$AE/             {       repnz scasb        }
             $75/$04/             {       jnz 2              }
             $39/$DA/             {       cmp DX,BX          }
             $75/$F7/             {       jnz 1              }
             $89/$8E/I/           { 2:    mov [BP+I],CX      }
             $89/$96/J    );      {       mov [BP+J],BX      }
      FindCharBack:=Len-I;Num:=J;
end;   *)

function TestOptStr(c:char):boolean;
  begin TestOptStr:=(pos(c,OptionStr)<>0)or(pos(upcase(c),OptionStr)<>0) end;

function FindString(var I:word;Len:word):boolean;
  function FindUpcChar(C:char;Pos:word;Len:word):word;
  var I,K:word;cc:char;
  begin I:=Len;K:=Pos-1;cc:=C;
        inline($C4/$3E/T/           {       les DI,addr(T)          }
               $03/$BE/K/           {       add DI,K                }
               $8B/$8E/I/           {       mov CX,[BP+I]           }
               $2B/$8E/K/           {       sub CX,[BP+K]           }
               $31/$DB/             {       xor BX,BX               }
               $8A/$9E/cc/          {       mov BL,[BP+cc]          }
               $81/$C3/UpcCharTab/  {       add BX,ofs(UpcCharTab)  }
               $8A/$07/             {       mov AL,[BX]             }
               $31/$DB/             {  1:   xor BX,BX               }
               $26/$8A/$1D/         {       mov BL,ES:[DI]          }
               $81/$C3/UpcCharTab/  {       add BX,ofs(UpcCharTab)  }
               $47/                 {       inc DI                  }
               $3A/$07/             {       cmp AL,[BX]             }
               $E0/$F2/             {       loopz 1                 }
               $89/$8E/I    );      {       mov [BP+I],CX           }
        FindUpcChar:=Len-I;
  end;
     {  !!!!!!!! spojit do jednoho , rozdil jen  ofs UpcCharTab,CharOrdTab }
  function FindOrdChar(C:char;Pos:word;Len:word):word;
  var I,K:word;cc:char;
  begin I:=Len;K:=Pos-1;cc:=C;
        inline($C4/$3E/T/           {       les DI,addr(T)          }
               $03/$BE/K/           {       add DI,K                }
               $8B/$8E/I/           {       mov CX,[BP+I]           }
               $2B/$8E/K/           {       sub CX,[BP+K]           }
               $31/$DB/             {       xor BX,BX               }
               $8A/$9E/cc/          {       mov BL,[BP+cc]          }
               $81/$C3/CharOrdTab/  {       add BX,ofs(UpcCharTab)  }
               $8A/$07/             {       mov AL,[BX]             }
               $31/$DB/             {  1:   xor BX,BX               }
               $26/$8A/$1D/         {       mov BL,ES:[DI]          }
               $81/$C3/CharOrdTab/  {       add BX,ofs(UpcCharTab)  }
               $47/                 {       inc DI                  }
               $3A/$07/             {       cmp AL,[BX]             }
               $E0/$F2/             {       loopz 1                 }
               $89/$8E/I    );      {       mov [BP+I],CX           }
        FindOrdChar:=Len-I;
  end;
function SEquOrder(S1,S2:string):boolean;
  var i:integer;
 begin SEquOrder:=false; if length(S1)<>length(S2) then exit;
      for i:=1 to length(S1) do
      if CharOrdTab[S1[i]]<>CharOrdTab[S2[i]] then exit;
      SEquOrder:=true end;
  var j,i1:word;
      s1,s2:string;
      c:char;
  label 1;
  begin FindString:=false;c:=FindStr[1];
    if FindStr<>'' then
      begin 1: if TestOptStr('~') then i1:=FindOrdChar(c,I,Len)
               else if TestOptStr('u') then i1:=FindUpcChar(c,I,Len)
                    else begin j:=1;i1:=FindChar(j,c,I,Len) end;
        I:=i1;
        if I+length(FindStr)>Len then exit; s2:=FindStr;
        move(T^[I],s1[1],length(FindStr));s1[0]:=char(length(FindStr));
        if TestOptStr('~') then
               begin if not SEquOrder(s1,s2) then
                       begin inc(I);goto 1 end end
        else if TestOptStr('u') then
          begin if not SEquUpcase(s1,s2) then
                  begin inc(I);goto 1 end end
        else if s1<>s2 then begin inc(I);goto 1 end;
        if TestOptStr('w') then
          if ((I>1)and not (T^[I-1] in Oddel))or
             (not(T^[I+length(FindStr)] in Oddel)) then begin inc(I);goto 1 end;
        FindString:=true;inc(I,length(FindStr));
      end;
  end;

procedure SetColorOrd(var CO:ColorOrd;First,Last:word);
  function FindCtrl(F,L:word):word;
    var I,K:word;
    begin I:=L;K:=F-1;
          inline($C4/$3E/T/        {      les DI,addr(T)   }
                 $03/$BE/K/        {      add DI,K         }
                 $8B/$8E/I/        {      mov CX,[BP+I]    }
                 $2B/$8E/K/        {      sub CX,[BP+K]    }
                 $26/$8A/$05/      {  1:  mov AL,ES:[DI]   }
                 $3C/$01/          {      cmp AL,^a        }
                 $74/$1A/          {      jz 2             }
                 $3C/$02/          {      cmp AL,^b        }
                 $74/$18/          {      jz 2             }
                 $3C/$04/          {      cmp AL,^d        }
                 $74/$14/          {      jz 2             }
                 $3C/$05/          {      cmp AL,^e        }
                 $74/$10/          {      jz 2             }
                 $3C/$11/          {      cmp AL,^q        }
                 $74/$0C/          {      jz 2             }
                 $3C/$13/          {      cmp AL,^s        }
                 $74/$08/          {      jz 2             }
                 $3C/$17/          {      cmp AL,^w        }
                 $74/$04/          {      jz 2             }
                 $47/              {      inc DI           } { $4E dec DI}
                 $49/              {      dec CX           }
                 $75/$DD/          {      jnz 1            }
                 $89/$8E/I);       {  2:  mov [BP+I],CX    }
          FindCtrl:=L-I+1;
    end;
  var I,pp:word;
      len:byte absolute CO;
  begin I:=FindCtrl(First,Last);
    while I<Last do
      begin pp:=pos(T^[I],CO);
        if pp>0 then CO:=copy(CO,1,pp-1)+copy(CO,pp+1,len-pp)
        else  CO:=CO+T^[I];
        I:=FindCtrl(I+1,Last);
      end;
  end;

procedure SimplePrintHead;
  var ln:string;
  procedure GetNum(var NN:word);
    var i,n:word;
    begin val(leadchar(' ',trailchar(' ',copy(ln,4,255))),n,i);
          if i=0 then NN:=n end;
(*  var s:String3;
      hefo,cp,pl:word;
      i,i1,i2:word;
      auto:boolean;
  label 1;
  begin
{    PHNum:=0;cp:=3;pl:=72;hefo:=0;s:=' ';i:=1;}
    cp:=spec.CpLines;pl:=spec.AutoRprtLimit+cp;PHNum:=0;hefo:=0;
    auto:=false;s:=' ';i:=1;
    while s<>'' do
      begin i2:=1;i1:=FindChar(i2,_CR,i,LenT);if i1=LenT then goto 1;
        dec(i1,i);i1:=MinW($FF,i1);ln[0]:=chr(i1);move(T^[i],ln[1],i1);
        s:=copy(ln,1,3);
        if SEquUpcase(s,'.cp') then begin inc(PHNum);GetNum(cp);auto:=true end else
        if SEquUpcase(s,'.pl') then begin inc(PHNum);GetNum(pl) end else
        if SEquUpcase(s,'.po')or SEquUpcase(s,'.ti')or SEquUpcase(s,'.ff')or SEquUpcase(s,'.nm')
           then inc(PHNum) else
        if SEquUpcase(s,'.he')or SEquUpcase(s,'.fo') then
          begin inc(PHNum);inc(hefo,2);auto:=true end else goto 1;
        inc(i,i1+1);if T^[i]=_LF then inc(i);
        if i>LenT then goto 1;
      end;
    1:if PHNum<>0 then PPageS:=pl-cp-hefo else PPageS:=$7FFF;
  end;    *)
  begin PHNum:=0;PPageS:=$7FFF end;

