{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L-}
unit fandpdf;
{ FAND print text (CP852, printer control codes) to PDF }
interface
function FandTxtToPdf(const InPath,OutPath:string):boolean;
procedure FandDeliverPdf(const Path:ansistring; WantOpen:boolean);
implementation
uses sysutils {$ifdef UNIX},unix{$endif};
const
  MarginPt   = 36.0;
  CourierW   = 0.6;
  MaxSizePt  = 10.0;
  MinSizePt  = 4.5;
  LeadFactor = 1.18;
  WideCols   = 96;
  A4w        = 595.28;
  A4h        = 841.89;
  cFF  = $0C;
  cBold= $02;
  cBin = $10;
type
  TGlyph = record G:string[20]; F:byte end;
const
  Glyphs:array[$80..$FF] of TGlyph = (
  (G:'Ccedilla'; F:0),
  (G:'udieresis'; F:0),
  (G:'eacute'; F:0),
  (G:'acircumflex'; F:0),
  (G:'adieresis'; F:0),
  (G:'uring'; F:0),
  (G:'cacute'; F:0),
  (G:'ccedilla'; F:0),
  (G:'lslash'; F:0),
  (G:'edieresis'; F:0),
  (G:'Ohungarumlaut'; F:0),
  (G:'ohungarumlaut'; F:0),
  (G:'icircumflex'; F:0),
  (G:'Zacute'; F:0),
  (G:'Adieresis'; F:0),
  (G:'Cacute'; F:0),
  (G:'Eacute'; F:0),
  (G:'Lacute'; F:0),
  (G:'lacute'; F:0),
  (G:'ocircumflex'; F:0),
  (G:'odieresis'; F:0),
  (G:'Lcaron'; F:0),
  (G:'lcaron'; F:0),
  (G:'Sacute'; F:0),
  (G:'sacute'; F:0),
  (G:'Odieresis'; F:0),
  (G:'Udieresis'; F:0),
  (G:'Tcaron'; F:0),
  (G:'tcaron'; F:0),
  (G:'Lslash'; F:0),
  (G:'multiply'; F:0),
  (G:'ccaron'; F:0),
  (G:'aacute'; F:0),
  (G:'iacute'; F:0),
  (G:'oacute'; F:0),
  (G:'uacute'; F:0),
  (G:'Aogonek'; F:0),
  (G:'aogonek'; F:0),
  (G:'Zcaron'; F:0),
  (G:'zcaron'; F:0),
  (G:'Eogonek'; F:0),
  (G:'eogonek'; F:0),
  (G:'logicalnot'; F:0),
  (G:'zacute'; F:0),
  (G:'Ccaron'; F:0),
  (G:'scedilla'; F:0),
  (G:'guillemotleft'; F:0),
  (G:'guillemotright'; F:0),
  (G:''; F:46),
  (G:''; F:58),
  (G:''; F:35),
  (G:''; F:124),
  (G:''; F:43),
  (G:'Aacute'; F:0),
  (G:'Acircumflex'; F:0),
  (G:'Ecaron'; F:0),
  (G:'Scedilla'; F:0),
  (G:''; F:43),
  (G:''; F:124),
  (G:''; F:43),
  (G:''; F:43),
  (G:'Zdotaccent'; F:0),
  (G:'zdotaccent'; F:0),
  (G:''; F:43),
  (G:''; F:43),
  (G:''; F:43),
  (G:''; F:43),
  (G:''; F:43),
  (G:''; F:45),
  (G:''; F:43),
  (G:'Abreve'; F:0),
  (G:'abreve'; F:0),
  (G:''; F:43),
  (G:''; F:43),
  (G:''; F:43),
  (G:''; F:43),
  (G:''; F:43),
  (G:''; F:61),
  (G:''; F:43),
  (G:'currency'; F:0),
  (G:'dcroat'; F:0),
  (G:'Dcroat'; F:0),
  (G:'Dcaron'; F:0),
  (G:'Edieresis'; F:0),
  (G:'dcaron'; F:0),
  (G:'Ncaron'; F:0),
  (G:'Iacute'; F:0),
  (G:'Icircumflex'; F:0),
  (G:'ecaron'; F:0),
  (G:''; F:43),
  (G:''; F:43),
  (G:''; F:35),
  (G:''; F:95),
  (G:'Tcommaaccent'; F:0),
  (G:'Uring'; F:0),
  (G:''; F:126),
  (G:'Oacute'; F:0),
  (G:'germandbls'; F:0),
  (G:'Ocircumflex'; F:0),
  (G:'Nacute'; F:0),
  (G:'nacute'; F:0),
  (G:'ncaron'; F:0),
  (G:'Scaron'; F:0),
  (G:'scaron'; F:0),
  (G:'Racute'; F:0),
  (G:'Uacute'; F:0),
  (G:'racute'; F:0),
  (G:'Uhungarumlaut'; F:0),
  (G:'yacute'; F:0),
  (G:'Yacute'; F:0),
  (G:'tcommaaccent'; F:0),
  (G:''; F:63),
  (G:'hyphen'; F:0),
  (G:''; F:63),
  (G:''; F:63),
  (G:''; F:63),
  (G:''; F:63),
  (G:'section'; F:0),
  (G:'divide'; F:0),
  (G:''; F:63),
  (G:'degree'; F:0),
  (G:''; F:63),
  (G:''; F:63),
  (G:'uhungarumlaut'; F:0),
  (G:'Rcaron'; F:0),
  (G:'rcaron'; F:0),
  (G:''; F:35),
  (G:''; F:63)
  );
type
  TBuf = record P:PByte; Len,Cap:longint end;
procedure BufInit(var b:TBuf);
begin b.P:=nil; b.Len:=0; b.Cap:=0 end;
procedure BufFree(var b:TBuf);
begin if b.P<>nil then FreeMem(b.P,b.Cap); BufInit(b) end;
procedure BufNeed(var b:TBuf;n:longint);
var q:PByte; c:longint;
begin
  if b.Len+n<=b.Cap then exit;
  c:=b.Cap; if c=0 then c:=8192;
  while c<b.Len+n do c:=c*2;
  GetMem(q,c); if b.P<>nil then begin Move(b.P^,q^,b.Len); FreeMem(b.P,b.Cap) end;
  b.P:=q; b.Cap:=c;
end;
procedure BufByte(var b:TBuf;v:byte);
begin BufNeed(b,1); PByte(PtrUInt(b.P)+b.Len)^:=v; inc(b.Len) end;
procedure BufRaw(var b:TBuf;const src;n:longint);
begin if n<=0 then exit; BufNeed(b,n); Move(src,PByte(PtrUInt(b.P)+b.Len)^,n); inc(b.Len,n) end;
procedure BufS(var b:TBuf;const s:string);
begin if length(s)>0 then BufRaw(b,s[1],length(s)) end;
procedure BufI(var b:TBuf;v:longint);
var s:string;
begin str(v,s); BufS(b,s) end;
procedure BufN(var b:TBuf;v:double);
var s:string; i:integer;
begin
  str(v:0:2,s);
  i:=length(s);
  while (i>1) and (s[i]='0') do dec(i);
  if (i>1) and (s[i]='.') then dec(i);
  BufS(b,copy(s,1,i));
end;
procedure BufPdfStr(var b:TBuf;const src;n:longint);
var i:longint; p:PByte; v:byte;
begin
  p:=@src;
  for i:=0 to n-1 do begin
    v:=PByte(PtrUInt(p)+i)^;
    if (v=40) or (v=41) or (v=92) then BufByte(b,92);
    BufByte(b,v);
  end;
end;
function FandTxtToPdf(const InPath,OutPath:string):boolean;
var
  src:PByte; srcLen:longint;
  used:array[0..255] of boolean;
  maxcols,i:longint;
  size,lead,pw,ph,avail,x,y:double;
  perpage,sheets:longint;
  objOfs:^longint; nObj,maxObj,pagesObj,encObj,f1,f2,infoObj:longint;
  pdf,pg:TBuf;
  firstPage:longint;
  fh:file;
  ok:boolean;
  procedure StartObj(n:longint);
  begin
    PLongint(PtrUInt(objOfs)+(n-1)*sizeof(longint))^:=pdf.Len;
    BufI(pdf,n); BufS(pdf,' 0 obj'#10);
  end;
  procedure EndObj;
  begin BufS(pdf,#10'endobj'#10) end;
  procedure Walk(Emit:boolean);
  var k:longint; c,v:byte; col,startcol:longint; bold,inrun:boolean;
      runStart:longint; lineNo:longint; cnt:longint;
    procedure FlushRun(e:longint);
    begin
      if not inrun then exit;
      if Emit then begin
        BufS(pg,'BT /F'); if bold then BufS(pg,'2') else BufS(pg,'1');
        BufS(pg,' '); BufN(pg,size); BufS(pg,' Tf ');
        BufN(pg,MarginPt+startcol*CourierW*size); BufS(pg,' '); BufN(pg,y);
        BufS(pg,' Td (');
        BufPdfStr(pg,PByte(PtrUInt(src)+runStart)^,e-runStart);
        BufS(pg,') Tj ET'#10);
      end;
      inrun:=false;
    end;
    procedure NewSheet;
    begin
      if not Emit then begin inc(sheets); exit end;
      StartObj(nObj);
      BufS(pdf,'<< /Length '); BufI(pdf,pg.Len); BufS(pdf,' >>'#10'stream'#10);
      BufRaw(pdf,pg.P^,pg.Len);
      BufS(pdf,#10'endstream'); EndObj; inc(nObj);
      StartObj(nObj);
      BufS(pdf,'<< /Type /Page /Parent '); BufI(pdf,pagesObj);
      BufS(pdf,' 0 R /MediaBox [0 0 '); BufN(pdf,pw); BufS(pdf,' '); BufN(pdf,ph);
      BufS(pdf,'] /Resources << /Font << /F1 '); BufI(pdf,f1);
      BufS(pdf,' 0 R /F2 '); BufI(pdf,f2); BufS(pdf,' 0 R >> >> /Contents ');
      BufI(pdf,nObj-1); BufS(pdf,' 0 R >>'); EndObj; inc(nObj);
      pg.Len:=0; y:=ph-MarginPt-size; lineNo:=0;
    end;
    procedure EndLine(e:longint);
    begin
      FlushRun(e);
      inc(lineNo); col:=0; startcol:=0;
      if Emit then begin
        y:=y-lead;
        if lineNo>=perpage then NewSheet;
      end else begin
        if lineNo>=perpage then begin inc(sheets); lineNo:=0 end;
      end;
    end;
  begin
    col:=0; startcol:=0; bold:=false; inrun:=false; lineNo:=0; runStart:=0;
    if Emit then begin pg.Len:=0; y:=ph-MarginPt-size end else sheets:=0;
    k:=0;
    while k<srcLen do begin
      c:=PByte(PtrUInt(src)+k)^; inc(k);
      case c of
        13: if (k<srcLen) and (PByte(PtrUInt(src)+k)^=10) then FlushRun(k-1)
            else EndLine(k-1);
        10: EndLine(k-1);
        cFF: begin
               FlushRun(k-1);
               if Emit then begin
                 if (lineNo>0) or (pg.Len>0) then NewSheet
               end else
                 if lineNo>0 then begin inc(sheets); lineNo:=0 end;
               col:=0; startcol:=0;
             end;
        cBin: begin
                FlushRun(k-1);
                if k+1<srcLen then begin
                  cnt:=PByte(PtrUInt(src)+k)^ or (PByte(PtrUInt(src)+k+1)^ shl 8);
                  inc(k,2+cnt);
                end else k:=srcLen;
              end;
        cBold: begin FlushRun(k-1); bold:=not bold; startcol:=col end;
      else
        if c<32 then begin FlushRun(k-1); startcol:=col end
        else begin
          if not inrun then begin inrun:=true; runStart:=k-1; startcol:=col end;
          if not Emit then begin
            used[c]:=true;
            inc(col); if col>maxcols then maxcols:=col;
          end else inc(col);
        end;
      end;
    end;
    FlushRun(k);
    if Emit then begin
      if (lineNo>0) or (pg.Len>0) then NewSheet
    end else
      if lineNo>0 then inc(sheets);
    if (not Emit) and (sheets=0) then sheets:=1;
  end;
  procedure WriteEncoding;
  var b,prev:longint; any:boolean;
  begin
    any:=false;
    for b:=$80 to $FF do if used[b] and (Glyphs[b].G<>'') then any:=true;
    if not any then begin encObj:=0; exit end;
    StartObj(encObj);
    BufS(pdf,'<< /Type /Encoding /BaseEncoding /WinAnsiEncoding /Differences [');
    prev:=-2;
    for b:=$80 to $FF do
      if used[b] and (Glyphs[b].G<>'') then begin
        if b<>prev+1 then begin BufS(pdf,' '); BufI(pdf,b) end;
        BufS(pdf,' /'); BufS(pdf,Glyphs[b].G);
        prev:=b;
      end;
    BufS(pdf,' ]>>'); EndObj;
  end;
  procedure WriteFont(n:longint;const nm:string);
  begin
    StartObj(n);
    BufS(pdf,'<< /Type /Font /Subtype /Type1 /BaseFont /'); BufS(pdf,nm);
    if encObj<>0 then begin BufS(pdf,' /Encoding '); BufI(pdf,encObj); BufS(pdf,' 0 R') end
    else BufS(pdf,' /Encoding /WinAnsiEncoding');
    BufS(pdf,' >>'); EndObj;
  end;
var n,xref:longint;
begin
  FandTxtToPdf:=false;
  src:=nil; objOfs:=nil; BufInit(pdf); BufInit(pg);
  {$I-}
  assign(fh,InPath); reset(fh,1);
  if IOResult<>0 then exit;
  srcLen:=FileSize(fh);
  if srcLen<0 then srcLen:=0;
  GetMem(src,srcLen+1);
  if srcLen>0 then BlockRead(fh,src^,srcLen);
  close(fh);
  if IOResult<>0 then ;
  {$I+}
  for i:=0 to 255 do used[i]:=false;
  maxcols:=0;
  for i:=0 to srcLen-1 do begin
    n:=PByte(PtrUInt(src)+i)^;
    if (n>=$80) and (Glyphs[n].F<>0) then PByte(PtrUInt(src)+i)^:=Glyphs[n].F;
  end;
  Walk(false);
  if maxcols>WideCols then begin pw:=A4h; ph:=A4w end
  else begin pw:=A4w; ph:=A4h end;
  avail:=pw-2*MarginPt;
  if maxcols=0 then size:=MaxSizePt
  else begin
    size:=avail/(CourierW*maxcols);
    if size>MaxSizePt then size:=MaxSizePt;
    if size<MinSizePt then size:=MinSizePt;
  end;
  lead:=size*LeadFactor;
  perpage:=trunc((ph-2*MarginPt)/lead);
  if perpage<1 then perpage:=1;
  Walk(false);
  maxObj:=6+2*sheets+8;
  GetMem(objOfs,maxObj*sizeof(longint));
  for i:=0 to maxObj-1 do PLongint(PtrUInt(objOfs)+i*sizeof(longint))^:=0;
  pagesObj:=2; encObj:=3; f1:=4; f2:=5; infoObj:=6;
  firstPage:=7; nObj:=firstPage;
  BufS(pdf,'%PDF-1.4'#10'%'#226#227#207#211#10);
  StartObj(1); BufS(pdf,'<< /Type /Catalog /Pages '); BufI(pdf,pagesObj);
  BufS(pdf,' 0 R >>'); EndObj;
  WriteEncoding;
  WriteFont(f1,'Courier'); WriteFont(f2,'Courier-Bold');
  StartObj(infoObj); BufS(pdf,'<< /Producer (PC FAND) /Title (');
  BufPdfStr(pdf,OutPath[1],length(OutPath)); BufS(pdf,') >>'); EndObj;
  Walk(true);
  StartObj(pagesObj);
  BufS(pdf,'<< /Type /Pages /Count '); BufI(pdf,(nObj-firstPage) div 2);
  BufS(pdf,' /Kids [');
  n:=firstPage+1;
  while n<nObj do begin BufS(pdf,' '); BufI(pdf,n); BufS(pdf,' 0 R'); inc(n,2) end;
  BufS(pdf,' ]>>'); EndObj;
  xref:=pdf.Len;
  BufS(pdf,'xref'#10'0 '); BufI(pdf,nObj); BufS(pdf,#10);
  BufS(pdf,'0000000000 65535 f '#10);
  for i:=1 to nObj-1 do begin
    n:=PLongint(PtrUInt(objOfs)+(i-1)*sizeof(longint))^;
    BufS(pdf,copy('0000000000',1,10-length(IntToStr(n)))+IntToStr(n));
    BufS(pdf,' 00000 n '#10);
  end;
  BufS(pdf,'trailer'#10'<< /Size '); BufI(pdf,nObj);
  BufS(pdf,' /Root 1 0 R /Info '); BufI(pdf,infoObj); BufS(pdf,' 0 R >>'#10);
  BufS(pdf,'startxref'#10); BufI(pdf,xref); BufS(pdf,#10'%%EOF'#10);
  ok:=false;
  {$I-}
  assign(fh,OutPath); rewrite(fh,1);
  if IOResult=0 then begin
    BlockWrite(fh,pdf.P^,pdf.Len);
    ok:=IOResult=0;
    close(fh);
    if IOResult<>0 then ;
  end;
  {$I+}
  if src<>nil then FreeMem(src,srcLen+1);
  if objOfs<>nil then FreeMem(objOfs,maxObj*sizeof(longint));
  BufFree(pdf); BufFree(pg);
  FandTxtToPdf:=ok;
end;
{$ifdef UNIX}
function ShQ(const s:ansistring):ansistring;
var k:integer; r:ansistring;
begin
  r := #39;
  for k := 1 to length(s) do
    if s[k]=#39 then r := r + #39'\'#39#39 else r := r + s[k];
  ShQ := r + #39;
end;
{$endif}
procedure FandDeliverPdf(const Path:ansistring; WantOpen:boolean);
{$ifdef UNIX}
var act,cmd:ansistring;
begin
  act := GetEnvironmentVariable('FAND_PRINT');
  if act='' then
    if WantOpen then act := 'open' else act := 'none';
  if act='none' then exit;
  if act='lpr' then begin
    cmd := 'lpr';
    if GetEnvironmentVariable('FAND_PRINTER')<>'' then cmd := cmd+' -P '+ShQ(GetEnvironmentVariable('FAND_PRINTER'));
    cmd := cmd+' '+ShQ(Path);
  end else
    cmd := {$ifdef DARWIN} 'open '+ShQ(Path)
           {$else} 'xdg-open '+ShQ(Path)+' >/dev/null 2>&1 &'
           {$endif};
  fpSystem(cmd);
end;
{$else}
begin
end;
{$endif}
end.
