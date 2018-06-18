uses crt,dos,FANDDML;
                 {  DEBUGGING  }

type string2=string[2];string4=string[4];string9=string[9];
const HexStr:array[0..15]of char='0123456789ABCDEF';
procedure wait;     var c:char; begin c:=readkey end;
function HexB(b:byte):string2;
  begin HexB:=HexStr[b shr 4] + HexStr[b and $0F] end;
function HexW(i:word):string4;
  begin HexW:=HexB(hi(i)) + HexB(lo(i)) end;
function HexD(i:LongInt):string8;
  var a:array[1..4]of byte absolute i;
  begin HexD:=HexB(a[4])+HexB(a[3])+HexB(a[2])+HexB(a[1]) end;
function HexPtr(p:pointer):string9;
  var a:array[1..2]of word absolute p;
  begin HexPtr:=HexW(a[2])+':'+HexW(a[1]) end;
procedure DispH(var ad;NoBytes:integer);
  var i:integer; p:^byte;
  begin p:=addr(ad);
        for i:=1 to NoBytes do
        begin write(HexB(p^),' ');p:=Ptr(seg(p^),ofs(p^)+1) end;
  end;

var f,g:FilePtr;
    N2,F2,BB,A2:ItemPtr; r:real; s:string; b:boolean; l:word;
    z:pointer; ftyp:char; err:boolean; pos:word;
begin
                          { nur mit Standard-TPL uebersetzen (heap) !!!! }
   gotoxy(1,20);
   writeln('AAAAAAAAAAAAAAAAa');wait;
    _opendml(0);
   writeln('BBBBBBBBBBBBBBBBb');wait;
end.
