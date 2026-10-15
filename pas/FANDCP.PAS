{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L-}
unit fandcp;
{ CP852 to and from Unicode, UTF-8, CP1250 and Latin-2 }
interface
const Cp852To1250:array[0..127] of byte=(
    $C7,$FC,$E9,$E2,$E4,$F9,$E6,$E7,$B3,$EB,$D5,$F5,
    $EE,$8F,$C4,$C6,$C9,$C5,$E5,$F4,$F6,$BC,$BE,$8C,
    $9C,$D6,$DC,$8D,$9D,$A3,$D7,$E8,$E1,$ED,$F3,$FA,
    $A5,$B9,$8E,$9E,$CA,$EA,$AC,$9F,$C8,$BA,$AB,$BB,
    $3F,$3F,$3F,$3F,$3F,$C1,$C2,$CC,$AA,$3F,$3F,$3F,
    $3F,$AF,$BF,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$C3,$E3,
    $3F,$3F,$3F,$3F,$3F,$3F,$3F,$A4,$F0,$D0,$CF,$CB,
    $EF,$D2,$CD,$CE,$EC,$3F,$3F,$3F,$3F,$DE,$D9,$3F,
    $D3,$DF,$D4,$D1,$F1,$F2,$8A,$9A,$C0,$DA,$E0,$DB,
    $FD,$DD,$FE,$B4,$AD,$BD,$B2,$A1,$A2,$A7,$F7,$B8,
    $B0,$A8,$FF,$FB,$D8,$F8,$3F,$A0
  );
const From1250:array[0..127] of byte=(
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$E6,$00,
    $97,$9B,$A6,$8D,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$E7,$00,$98,$9C,$A7,$AB,$FF,$F3,$F4,$9D,
    $CF,$A4,$00,$F5,$F9,$00,$B8,$AE,$AA,$F0,$00,$BD,
    $F8,$00,$F2,$88,$EF,$00,$00,$00,$F7,$A5,$AD,$AF,
    $95,$F1,$96,$BE,$E8,$B5,$B6,$C6,$8E,$91,$8F,$80,
    $AC,$90,$A8,$D3,$B7,$D6,$D7,$D2,$D1,$E3,$D5,$E0,
    $E2,$8A,$99,$9E,$FC,$DE,$E9,$EB,$9A,$ED,$DD,$E1,
    $EA,$A0,$83,$C7,$84,$92,$86,$87,$9F,$82,$A9,$89,
    $D8,$A1,$8C,$D4,$D0,$E4,$E5,$A2,$93,$8B,$94,$F6,
    $FD,$85,$A3,$FB,$81,$EC,$EE,$FA
  );
const Cp852ToLat2:array[0..127] of byte=(
    $C7,$FC,$E9,$E2,$E4,$F9,$E6,$E7,$B3,$EB,$D5,$F5,
    $EE,$AC,$C4,$C6,$C9,$C5,$E5,$F4,$F6,$A5,$B5,$A6,
    $B6,$D6,$DC,$AB,$BB,$A3,$D7,$E8,$E1,$ED,$F3,$FA,
    $A1,$B1,$AE,$BE,$CA,$EA,$3F,$BC,$C8,$BA,$3F,$3F,
    $3F,$3F,$3F,$3F,$3F,$C1,$C2,$CC,$AA,$3F,$3F,$3F,
    $3F,$AF,$BF,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$C3,$E3,
    $3F,$3F,$3F,$3F,$3F,$3F,$3F,$A4,$F0,$D0,$CF,$CB,
    $EF,$D2,$CD,$CE,$EC,$3F,$3F,$3F,$3F,$DE,$D9,$3F,
    $D3,$DF,$D4,$D1,$F1,$F2,$A9,$B9,$C0,$DA,$E0,$DB,
    $FD,$DD,$FE,$B4,$AD,$BD,$B2,$B7,$A2,$A7,$F7,$B8,
    $B0,$A8,$FF,$FB,$D8,$F8,$3F,$A0
  );
const FromLat2:array[0..127] of byte=(
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$FF,$A4,$F4,$9D,
    $CF,$95,$97,$F5,$F9,$E6,$B8,$9B,$8D,$F0,$A6,$BD,
    $F8,$A5,$F2,$88,$EF,$96,$98,$F3,$F7,$E7,$AD,$9C,
    $AB,$F1,$A7,$BE,$E8,$B5,$B6,$C6,$8E,$91,$8F,$80,
    $AC,$90,$A8,$D3,$B7,$D6,$D7,$D2,$D1,$E3,$D5,$E0,
    $E2,$8A,$99,$9E,$FC,$DE,$E9,$EB,$9A,$ED,$DD,$E1,
    $EA,$A0,$83,$C7,$84,$92,$86,$87,$9F,$82,$A9,$89,
    $D8,$A1,$8C,$D4,$D0,$E4,$E5,$A2,$93,$8B,$94,$F6,
    $FD,$85,$A3,$FB,$81,$EC,$EE,$FA
  );
const Cp852Uni:array[0..127] of word=(
    $00C7,$00FC,$00E9,$00E2,$00E4,$016F,$0107,$00E7,
    $0142,$00EB,$0150,$0151,$00EE,$0179,$00C4,$0106,
    $00C9,$0139,$013A,$00F4,$00F6,$013D,$013E,$015A,
    $015B,$00D6,$00DC,$0164,$0165,$0141,$00D7,$010D,
    $00E1,$00ED,$00F3,$00FA,$0104,$0105,$017D,$017E,
    $0118,$0119,$00AC,$017A,$010C,$015F,$00AB,$00BB,
    $2591,$2592,$2593,$2502,$2524,$00C1,$00C2,$011A,
    $015E,$2563,$2551,$2557,$255D,$017B,$017C,$2510,
    $2514,$2534,$252C,$251C,$2500,$253C,$0102,$0103,
    $255A,$2554,$2569,$2566,$2560,$2550,$256C,$00A4,
    $0111,$0110,$010E,$00CB,$010F,$0147,$00CD,$00CE,
    $011B,$2518,$250C,$2588,$2584,$0162,$016E,$2580,
    $00D3,$00DF,$00D4,$0143,$0144,$0148,$0160,$0161,
    $0154,$00DA,$0155,$0170,$00FD,$00DD,$0163,$00B4,
    $00AD,$02DD,$02DB,$02C7,$02D8,$00A7,$00F7,$00B8,
    $00B0,$00A8,$02D9,$0171,$0158,$0159,$25A0,$00A0
  );
const Cp1250Uni:array[0..127] of word=(
    $20AC,$0000,$201A,$0000,$201E,$2026,$2020,$2021,
    $0000,$2030,$0160,$2039,$015A,$0164,$017D,$0179,
    $0000,$2018,$2019,$201C,$201D,$2022,$2013,$2014,
    $0000,$2122,$0161,$203A,$015B,$0165,$017E,$017A,
    $00A0,$02C7,$02D8,$0141,$00A4,$0104,$00A6,$00A7,
    $00A8,$00A9,$015E,$00AB,$00AC,$00AD,$00AE,$017B,
    $00B0,$00B1,$02DB,$0142,$00B4,$00B5,$00B6,$00B7,
    $00B8,$0105,$015F,$00BB,$013D,$02DD,$013E,$017C,
    $0154,$00C1,$00C2,$0102,$00C4,$0139,$0106,$00C7,
    $010C,$00C9,$0118,$00CB,$011A,$00CD,$00CE,$010E,
    $0110,$0143,$0147,$00D3,$00D4,$0150,$00D6,$00D7,
    $0158,$016E,$00DA,$0170,$00DC,$00DD,$0162,$00DF,
    $0155,$00E1,$00E2,$0103,$00E4,$013A,$0107,$00E7,
    $010D,$00E9,$0119,$00EB,$011B,$00ED,$00EE,$010F,
    $0111,$0144,$0148,$00F3,$00F4,$0151,$00F6,$00F7,
    $0159,$016F,$00FA,$0171,$00FC,$00FD,$0163,$02D9
  );
function S852ToUtf8(const S:ansistring):ansistring;
function SUtf8To852(const S:ansistring):ansistring;
function S1250ToUtf8(const S:ansistring):ansistring;
function SUtf8To1250(const S:ansistring):ansistring;
function S852To1250(const S:ansistring):ansistring;
function S1250To852(const S:ansistring):ansistring;
function UniToUtf8(U:longint):ansistring;
function NextUtf8(const S:ansistring; var I:longint):longint;
implementation
function UniToUtf8(U:longint):ansistring;
begin
  if U<$80 then UniToUtf8:=chr(U)
  else if U<$800 then
    UniToUtf8:=chr($C0 or (U shr 6))+chr($80 or (U and $3F))
  else if U<$10000 then
    UniToUtf8:=chr($E0 or (U shr 12))+chr($80 or ((U shr 6) and $3F))+
               chr($80 or (U and $3F))
  else
    UniToUtf8:=chr($F0 or (U shr 18))+chr($80 or ((U shr 12) and $3F))+
               chr($80 or ((U shr 6) and $3F))+chr($80 or (U and $3F));
end;
function AsciiFallback(U:longint):ansistring;
begin
  case U of
    $00A0,$2002,$2003,$2009,$202F: AsciiFallback:=' ';
    $2010,$2011,$2012,$2013,$2014,$2015,$2212: AsciiFallback:='-';
    $2018,$2019,$201A,$201B,$2032: AsciiFallback:=#39;
    $201C,$201D,$201E,$201F,$2033,$00AB,$00BB: AsciiFallback:='"';
    $2026: AsciiFallback:='...';
    $2022,$00B7: AsciiFallback:='*';
    $20AC: AsciiFallback:='EUR';
    $2122: AsciiFallback:='(TM)';
    $00A9: AsciiFallback:='(C)';
    $00AE: AsciiFallback:='(R)';
  else AsciiFallback:='';
  end;
end;
function NextUtf8(const S:ansistring; var I:longint):longint;
var c,n,k:integer; u:longint;
begin
  c:=ord(S[I]); inc(I);
  if c<$80 then begin NextUtf8:=c; exit end;
  if (c and $E0)=$C0 then begin u:=c and $1F; n:=1 end
  else if (c and $F0)=$E0 then begin u:=c and $0F; n:=2 end
  else if (c and $F8)=$F0 then begin u:=c and $07; n:=3 end
  else begin NextUtf8:=c; exit end;
  for k:=1 to n do begin
    if (I>length(S)) or ((ord(S[I]) and $C0)<>$80) then begin
      NextUtf8:=$FFFD; exit end;
    u:=(u shl 6) or (ord(S[I]) and $3F); inc(I);
  end;
  NextUtf8:=u;
end;
function S852ToUtf8(const S:ansistring):ansistring;
var i:longint; r:ansistring;
begin
  r:='';
  for i:=1 to length(S) do
    if ord(S[i])<$80 then r:=r+S[i]
    else r:=r+UniToUtf8(Cp852Uni[ord(S[i])-$80]);
  S852ToUtf8:=r;
end;
function S1250ToUtf8(const S:ansistring):ansistring;
var i:longint; r:ansistring; u:word;
begin
  r:='';
  for i:=1 to length(S) do
    if ord(S[i])<$80 then r:=r+S[i]
    else begin
      u:=Cp1250Uni[ord(S[i])-$80];
      if u=0 then r:=r+'?' else r:=r+UniToUtf8(u) end;
  S1250ToUtf8:=r;
end;
function FromUni(U:longint; const Tab:array of word; var Hit:boolean):char;
var k:integer;
begin
  Hit:=true;
  if U<$80 then begin FromUni:=chr(U); exit end;
  for k:=0 to 127 do
    if Tab[k]=U then begin FromUni:=chr(k+$80); exit end;
  Hit:=false; FromUni:='?';
end;
function Utf8ToTab(const S:ansistring; const Tab:array of word):ansistring;
var i:longint; u:longint; r,f:ansistring; c:char; hit:boolean;
begin
  r:=''; i:=1;
  while i<=length(S) do begin
    u:=NextUtf8(S,i);
    c:=FromUni(u,Tab,hit);
    if hit then r:=r+c
    else begin f:=AsciiFallback(u); if f='' then r:=r+'?' else r:=r+f end;
  end;
  Utf8ToTab:=r;
end;
function SUtf8To852(const S:ansistring):ansistring;
begin SUtf8To852:=Utf8ToTab(S,Cp852Uni) end;
function SUtf8To1250(const S:ansistring):ansistring;
begin SUtf8To1250:=Utf8ToTab(S,Cp1250Uni) end;
function S852To1250(const S:ansistring):ansistring;
var i:longint; r:ansistring;
begin
  r:=S;
  for i:=1 to length(r) do
    if ord(r[i])>=$80 then r[i]:=chr(Cp852To1250[ord(r[i])-$80]);
  S852To1250:=r;
end;
function S1250To852(const S:ansistring):ansistring;
begin S1250To852:=SUtf8To852(S1250ToUtf8(S)) end;
end.
