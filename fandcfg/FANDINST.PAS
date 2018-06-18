{$A-}

uses dos,crt,printer,
     mydos,windows;

const ver='4.20';
var   version:array[1..4]of char;
      CfgPath,CfgPathFull : string;
      AttrCfg:word;
      kl:char;

type  types=(H1,H2,H5,H11,D1,D2,D3,D5,B,U1,H127);

procedure SetFileAttr( Attr:word) ;
     var len : byte absolute CfgPathFull ;
          rg : Registers ;
     begin inc(len); CfgPath[len]:=chr($00);
           rg.AX:=$4301; rg.CX:=Attr; rg.DX:=Ofs(CfgPathFull)+1; rg.DS:=Seg(CfgPathFull);
           MsDos(rg); dec(len); end ;

function GetFileAttr : word ;
     var len : byte absolute CfgPathFull ;
          rg : Registers ;
     begin inc(len); CfgPathFull[len]:=chr($00);
           rg.AH:=$43; rg.AL:=$00; rg.DX:=Ofs(CfgPathFull)+1; rg.DS:=Seg(CfgPathFull);
           MsDos(rg); dec(len); GetFileAttr:=rg.CX; end ;


procedure WorkPlace ;
   var CfgVyp:string ;
   begin MyWindow(1,2,80,TxtRows-2,_abs);  ClrScr ;
         MyWindow(44,TxtRows-4,36,4,_abs); Frame(_double,'','');
         writeln(' PC FAND '+ver+'- instalacni program');
         CfgVyp:=CfgPathFull ;
         if length(CfgVyp)>32 then CfgVyp:=copy(CfgVyp,length(CfgVyp)-31,32);
         write(' ',CfgVyp);
         {write  (' (C) 1992 Tichy & Jezek, Novy Bor');}
         MyWindow(1,2,42,TxtRows-2,_abs); Frame(_single,'','');
   end ;


procedure EditLine (var s:string; pos,maxlen,maxcol,filter:word);
  var SavedAttr:word;
  begin SavedAttr:=TextAttr; TextAttr:=Colors.eText;
        windows.EditLine(s,pos,maxlen,maxcol,filter);
        TextAttr:=SavedAttr;
  end;

procedure Edit (t:types; var result);
  var pH1:byte absolute result; pH2:word absolute result;
      pH5:string5 absolute result; pH11:string11 absolute result;
      pD3:byte absolute result; pD5:word absolute result;
      pD2:byte absolute result;
      pB:boolean absolute result; pU1:char absolute result;
      pD1:byte absolute result;
      pH127:string127 absolute result;
  var s:string; i:word;
  begin case t of H1:begin s:=hexB(pH1); EditLine(s,1,2,2,_hex);
                           pH1:=HexToB(s);
                     end;
                  H2:begin s:=hexW(pH2); EditLine(s,1,4,4,_hex);
                           pH2:=HexToW(s);
                     end;
                  H5:begin s:=''; for i:=1 to length(pH5) do s:=s+HexB(byte(pH5[i]));
                           EditLine(s,1,10,10,_hex);
                           if length(s)<2 then s:='';
                           for i:=1 to length(s) div 2 do pH5[i]:=char(HexToB(copy(s,2*i-1,2)));
                           pH5[0]:=char(length(s) div 2);
                     end;
                 H127:begin s:=''; for i:=1 to length(pH127) do s:=s+HexB(byte(pH127[i]));
                           EditLine(s,1,254,254,_hex);
                           if length(s)<2 then s:='';
                           for i:=1 to length(s) div 2 do pH127[i]:=char(HexToB(copy(s,2*i-1,2)));
                           pH127[0]:=char(length(s) div 2);
                     end;
                 H11:begin s:=''; for i:=1 to length(pH11) do s:=s+HexB(byte(pH11[i]));
                           EditLine(s,1,22,22,_hex);
                           if length(s)<2 then s:='';
                           for i:=1 to length(s) div 2 do pH11[i]:=char(HexToB(copy(s,2*i-1,2)));
                           pH11[0]:=char(length(s) div 2);
                     end;
                  D3:begin str(pD3:3,s); i:=1; while s[i]=' ' do delete(s,1,1);
                           EditLine(s,1,3,3,_digit); val(s,pD3,i);
                     end;
                  D2:begin str(pD2:2,s); i:=1; while s[i]=' ' do delete(s,1,1);
                           EditLine(s,1,2,2,_digit); val(s,pD2,i);
                     end;
                  D5:begin str(pD5:5,s); i:=1; while s[i]=' ' do delete(s,1,1);
                           EditLine(s,1,5,5,_digit); val(s,pD5,i);
                     end;
                   B:begin if pB=true then s:='A' else s:='N'; EditLine(s,1,1,1,_bool);
                           pB:=s='A';
                     end;
                  U1:begin s:=pU1; EditLine(s,1,1,1,_uppercase); pU1:=s[1];
                     end;
                  D1:begin str(pD1:1,s); EditLine(s,1,1,1,_digit); val(s,pD1,i);
                     end;
        end;
  end;

procedure Disp (t:types; var value);
  var pH1:byte absolute value; pH2:word absolute value;
      pH5:string5 absolute value; pH11:string11 absolute value;
      pD3:byte absolute value; pD5:word absolute value;
      pB:boolean absolute value; pU1:char absolute value;
      pD1:byte absolute value; pD2:byte absolute value;
      pH127:string127 absolute value;
  var s:string; i:word;
  begin case t of H1:begin s:=hexB(pH1) end;
                  H2:begin s:=hexW(pH2) end;
                  H5:begin s:=''; for i:=1 to length(pH5) do s:=s+HexB(byte(pH5[i]))+' ';
                     end;
                 H11:begin s:=''; for i:=1 to length(pH11) do s:=s+HexB(byte(pH11[i]));
                     end;
                 H127:begin s:=''; for i:=1 to length(pH127) do s:=s+HexB(byte(pH127[i]));
                     end;
                  D3:begin str(pD3:3,s); i:=1; while s[i]=' ' do delete(s,1,1) end;
                  D2:begin str(pD2:2,s); i:=1; while s[i]=' ' do delete(s,1,1) end;
                  D5:begin str(pD5:5,s); i:=1; while s[i]=' ' do delete(s,1,1) end;
                  D1:begin str(pD1:1,s) end;
                   B:begin if pB=true then s:='A' else s:='N' end;
                  U1:begin s:=pU1 end;
        end;
        clreol; write(s);
  end;

const sItems=31;
const sNames:array[1..sItems] of string[38]=
                      ('        Pocet vet k ulozeni v editoru',
                       'Sirka',
                       'Logicka delka',
                       'Ukonceni strany',
                       'Vystup rovnou na tiskarnu ?',
                       'Vyber tiskarny pred tiskem ?',
                       'Graficky oddelit stranky ?',
                       'Znak pro oddeleni',
                       '              Potvrdit ESC z editoru ?',
                       '     Potvrdit ESC z nedokoncene vety ?',
                       'Potvrzeni hlasky  ENTER misto F10',
                       'Komentar k RDB pri ladeni ?',
                       'CP/M sachta (napr. D,E,F,...)',
                       'Obnoveni obrazovky(sec)',
                       'Opak. blokovaneho pristupu(CPU)',
                       'Prvni pristup - prodleva (CPU) ',
                       '              - pocet opakovani',
                       'Pipani pri opakovani blokovani',
                       'Pipani pri chybach ?',
                       'Max.velikost XMS pro FAND (kB)',
                       'Potlacit Ctrl-Break ?',
                       'Cislo implicitni klavesnice',
                       'Vypnuta',
                       'Vymena klaves',
                       'Interval pro dvojity stisk(CPU)',
                       'Interval pro opakovani(CPU)',
                       'Prodleva Ctrl,Alt,Shift(CPU)',
                       'Automat. prepsat navesti diskety',
                       'Interval zhasnuti obrazovky(sec)',
                       'Posun implicitniho stoleti',
                       'Kontrola volneho mista na disku');

const sTypes:array[1..sItems] of types=
            (D3,D3,D3,D3,B,B,B,U1,B,B,B,B,U1,D5,D5,D3,D3,B,B,D5,B,D1,B,B,D1,D1,D1,B,D5,D1,B);
var   sCodes:record UpdCount:byte;
                    AutoRprtWidth,AutoRprtLimit,CpLines:byte;
                    AutoRprtPrint,ChoosePrMsg,TxtInsPg:boolean;
                    TxtCharPg:char;
                    ESCverify:boolean;
                    NoPrompt158:boolean;
                    F10Enter:boolean;
                    RDBcomment:boolean;
                    CPMdrive:char;
                    RefreshDelay,NetDelay:word;
                    LockDelay,LockRetries:byte;
                    Beep, LockBeepAllowed:boolean;
                    XMSMaxKb:word;
                    NoCheckBreak:boolean;
                    KbdTyp:byte;
                    NoMouseSupport,MouseReverse:boolean;
                    DoubleDelay,RepeatDelay,CtrlDelay:byte;
                    OverwrLabeledDisk:boolean;
                    ScreenDelay:word;
                    OffDefaultYear:byte;
                    WithDiskFree:boolean;
             end;

const vItems=6;
const vNames:array[1..vItems] of string20=
            ('Adresa videopameti',
             'Pocet radku',
             'Osetrit snezeni ?',
             'Zapnuty kurzor',
             'Vypnuty kurzor',
             'Velky kurzor');
const vTypes:array[1..vItems] of types=
            (H2,D3,B,H2,H2,H2);
type  vCodes=record vseg:word;
                    TxtRows : byte ;
                    chks:boolean;
                    on,off,big:word;
             end;
var   vMono,vCGA,vEGA:vCodes;
var   vCurr:^vCodes;

const FItems=3 ;
const fNames:array[1..fItems] of string20=
             ('Kod',
              'Pouzit fonty FANDu',
              'Monitor bez diakr.');
const fTypes:array[1..fItems] of types=(H1,B,B) ;
type fCodes=record VFont:byte;
                   LoadVideoAllowed,NoDiakrSupported:boolean ;
            end ;
var Fonts:fCodes ;

const cItems=54;
const cNames:array[1..cItems] of string30=
             ('uzivatelska barva c.0',
              '                  c.1',
              '                  c.2',
              '                  c.3',
              '                  c.4',
              '                  c.5',
              '                  c.6',
              '                  c.7',
              '                  c.8',
              '                  c.9',
              '                  c.10',
              '                  c.11',
              '                  c.12',
              '                  c.13',
              '                  c.14',
              '                  c.15',
              'MENU normalni text',
              '     aktivni volba',
              '     prvni pismeno',
              '     nepovolena volba',
              'VYBER normalni text',
              '      aktivni polozka',
              '      maska pro soubor',
              'ZADANI zadavany text',
              '       napoveda',
              'HLASENI na poslednim radku',
              'POSLEDNI RADEK napoveda',
              '               symboly klaves',
              '               prepinace',
              'PRVNI RADEK system.informace',
              'EDITOR TEXTU text',
              '             ctrl-znaky',
              '             blok',
              'DRUHY PISMA podtrzeny text',
              '            kurziva',
              '            siroky text',
              '            dvojity text',
              '            mastny text',
              '            zhusteny text',
              '            pismo elite',
              'EDITOR DAT data',
              '           datovy kurzor',
              '           podmnozina',
              '           ostatni text',
              '           zrusene vety',
              '           vyber (F8,ShiftF8)',
              'UZIVATELSKA OBRAZOVKA',
              'NAPOVEDA text',
              '         vybrane tema',
              '         ostatni temata',
              '         zvyrazneny text',
              'KRATKA NAPOVEDA',
              'OKNA  stiny',
              'DESKTOP');
const cTypes:array[1..cItems] of types=
            (H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,
             H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,
             H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1,H1);
type  cCodes=array[1..cItems]of byte;
var   cMono,cCGA,cEGA:cCodes;
var   cCurr:^cCodes;

{$I CHARTABS}

const pItems=32;
const pNames:array[1..pItems] of string[16]=
                        ('^S Podtrzeni zap','Podtrzeni vyp', {^S Underline}
                         '^W Kurziva   zap','Kurziva   vyp', {^W Italic}
                         '^Q Siroky    zap','Siroky    vyp', {^Q Double-width}
                         '^D Dvojity   zap','Dvojity   vyp', {^D Double-strike}
                         '^B Mastny    zap','Mastny    vyp', {^B Emphasised}
                         '^E Zhusteny  zap','Zhusteny  vyp', {^E Compressed}
                         '^A Elite     zap','Elite     vyp', {^A Elite}
                         'Reset tiskarny',
                         'Delka strany',
                         'Uk.retezec/Laser',
                         'Odsazeni zleva','Uk.odsaz.zleva',
                         '^X Uziv.1    zap','Uziv.1    vyp', {^X User 1}
                         '^V Uziv.2    zap','Uziv.2    vyp', {^V User 2}
                         '^T Uziv.3    zap','Uziv.3    vyp',{^T User 3}
                         'Radkovani n/72"' ,'Radkovani n/216"', {Tisk grafu}
                         'Gr.hustota 60'   ,'Gr.hustota 120',
                         'Gr.Hustota 240'  ,'Nastaveni barev',
                         'Zaver. sekvence');
const pTypes:array[1..pItems] of types=
            (H127,H127,H127,H127,H127,H127,H127,H127,H127,H127,H127,
             H127,H127,H127,H127,H127,H127,H127,H127,H127,H127,H127,
             H127,H127,H127,H127,H127,H127,H127,H127,H127,H127);


var   prMax:byte;
      printers : array [0..9] of
                record pName:string8;
                       pCodes:array[1..pItems] of string127;
                       pType:char ;  { 'M'-mozaikova,'C'-moz.barevna,'L'-Laser HP }
                       pCode:char;   { ' '-zustava, 'L'-LK, 'l'-LN, 'K'-KL, 'k'-KN }
                       pNo:byte ;    { in [1,2,3] - cislo portu tiskarny LPT1,...  }
                       pTmOut:byte;  { time out pro tiskarnu }
                       end ;

type  WDaysTabType=array[1..1]of record Typ:byte;Nr:word end;

var   NWDaysTab:word; WDaysFirst,WDaysLast:real;
      WDaysTab:^WDaysTabType;

var   aCode:byte; {1-Kamenickych 2-Latin2 3-StandardIBM}

procedure sInst;
  begin MyWindow(1,2,50,TxtRows-2,_abs); Frame(_single,'','');
        writeln('KONSTANTY: '); writeln;
        with sCodes do begin write(sNames[1]:38,' : ');Edit(D3,UpdCount);Disp(D3,UpdCount);writeln;
                             writeln; writeln('SESTAVA':38);
                             write(sNames[2]:38,' : ');Edit(D3,AutoRprtWidth);Disp(D3,AutoRprtWidth);writeln;
                             write(sNames[3]:38,' : ');Edit(D3,AutoRprtLimit);Disp(D3,AutoRprtLimit);writeln;
                             write(sNames[4]:38,' : ');Edit(D3,CpLines);Disp(D3,CpLines);writeln;
                             write(sNames[5]:38,' : ');Edit(B,AutoRprtPrint);Disp(B,AutoRprtPrint);writeln;
                             write(sNames[6]:38,' : ');Edit(B,ChoosePrMsg);Disp(B,ChoosePrMsg);writeln;
                             writeln; writeln('ODDELENI STRANEK V TEXTU':38) ;
                             write(sNames[7]:38,' : ');Edit(B,TxtInsPg);Disp(B,TxtInsPg);writeln;
                             write(sNames[8]:38,' : ');Edit(U1,TxtCharPg);Disp(U1,TxtCharPg);writeln;
                             writeln;
                             write(sNames[9]:38,' : ');Edit(B,ESCverify);Disp(B,ESCverify);writeln;
                             write(sNames[10]:38,' : ');Edit(B,NoPrompt158);Disp(B,NoPrompt158);writeln;
                             write(sNames[11]:38,' : ');Edit(B,F10Enter);Disp(B,F10Enter);writeln;
                             write(sNames[12]:38,' : ');Edit(B,RDBcomment);Disp(B,RDBcomment);writeln;
                             write(sNames[13]:38,' : ');Edit(U1,CPMdrive);Disp(U1,CPMdrive);writeln;
                             writeln; writeln('SITE LAN':38);
                             write(sNames[14]:38,' : ');Edit(D5,RefreshDelay);Disp(D5,RefreshDelay);writeln;
                             write(sNames[15]:38,' : ');Edit(D5,NetDelay);Disp(D5,NetDelay);writeln;
                             write(sNames[16]:38,' : ');Edit(D3,LockDelay);Disp(D3,LockDelay);writeln;
                             write(sNames[17]:38,' : ');Edit(D3,LockRetries);Disp(D3,LockRetries);writeln;

                             write(sNames[18]:38,' : ');Edit(B,LockBeepAllowed);Disp(B,LockBeepAllowed);writeln;
                             writeln;
                             write(sNames[19]:38,' : ');Edit(B,Beep);Disp(B,Beep);writeln;
                             write(sNames[20]:38,' : ');Edit(D5,XMSMaxKb);Disp(D5,XMSMaxKb);writeln;
                             write(sNames[21]:38,' : ');Edit(B,NoCheckBreak);Disp(B,NoCheckBreak);writeln;
                             write(sNames[22]:38,' : ');Edit(D1,KbdTyp);Disp(D1,KbdTyp);writeln;
                             writeln; writeln('PODPORA MYSI':38);
                             write(sNames[23]:38,' : ');Edit(B,NoMouseSupport);Disp(B,NoMouseSupport);writeln;
                             write(sNames[24]:38,' : ');Edit(B,MouseReverse);Disp(B,MouseReverse);writeln;
                             write(sNames[25]:38,' : ');Edit(D2,DoubleDelay);Disp(D2,DoubleDelay);writeln;
                             write(sNames[26]:38,' : ');Edit(D2,RepeatDelay);Disp(D2,RepeatDelay);writeln;
                             writeln;
                           { write(sNames[27]:38,' : ');Edit(D2,CtrlDelay);Disp(D2,CtrlDelay);writeln;}
                             writeln; writeln('ZALOHOVANI':38);
                             write(sNames[28]:38,' : ');Edit(B,OverwrLabeledDisk);Disp(B,OverwrLabeledDisk);writeln;
                             writeln;
                             write(sNames[29]:38,' : ');Edit(D5,ScreenDelay);Disp(D5,ScreenDelay);writeln;
                             write(sNames[30]:38,' : ');Edit(D2,OffDefaultYear);Disp(D2,OffDefaultYear);writeln;
                             write(sNames[31]:38,' : ');Edit(B,WithDiskFree);Disp(B,WithDiskFree);writeln;
                             writeln;
                             writeln(#7,'konec konstant - stisknete ESC');
                             while ReadKey<>#27 do ;
                       end;
        writeln;
        WorkPlace;
  end;

procedure vInst;
  var s:ScreenStr;
  begin if cCurr=@cMono then s:='Monochrom' else
        if cCurr=@cCGA then s:='CGA' else s:='EGA';
        writeln('MONITOR: ',s); writeln;
        with vCurr^ do begin write(vNames[1]:20,' : ');Edit(H2,vseg);Disp(H2,vseg);writeln;
                             write(vNames[2]:20,' : ');Edit(D3,TxtRows);Disp(D3,TxtRows);writeln;
                             write(vNames[3]:20,' : ');Edit(B,chks);Disp(B,chks);writeln;
                             write(vNames[4]:20,' : ');Edit(H2,on);Disp(H2,on);writeln;
                             write(vNames[5]:20,' : ');Edit(H2,off);Disp(H2,off);writeln;
                             write(vNames[6]:20,' : ');Edit(H2,big);Disp(H2,big);writeln;
                             write(fNames[2]:20,' : ');Edit(B,Fonts.LoadVideoAllowed);
                                                       Disp(B,Fonts.LoadVideoAllowed); writeln ;
                             write(fNames[3]:20,' : ');Edit(B,Fonts.NoDiakrSupported);
                                                       Disp(B,Fonts.NoDiakrSupported); writeln ;
                     end;
        writeln;
        writeln(#7,'konec nastaveni monitoru - stisknete ESC');
        while ReadKey<>#27 do ;
  end;

procedure cInst;
  var i:word; a:byte; p:pointer;
  begin
        p:=markWw;
        PushWw(45,2,34,18); Frame(_double,'',''); AppendLine;
        for i:=0 to 127 do begin TextAttr:=i; write(' ',hexb(i),' '); end;
        SwitchWw(p);
        writeln('BARVY:'); writeln; a:=TextAttr;
        for i:=1 to cItems do begin write(cNames[i]); gotoxy(31,whereY); write(':');
                                    TextAttr:=byte(cCurr^[i]); write(' XXX ');
                                    TextAttr:=a; write(' '); Edit(cTypes[i],cCurr^[i]);
                                    TextAttr:=byte(cCurr^[i]); gotoxy(32,whereY); write(' XXX ');
                                    TextAttr:=a; write(' '); Disp(cTypes[i],cCurr^[i]);
                                    writeln;
                              end;
        writeln;
        writeln(#7,'konec nastaveni barev - stisknete ESC');
        while ReadKey<>#27 do ;
        SwitchTopWw; PopWw;
  end;

procedure aInst;
  const s:ScreenStr='Kamenickych/Latin 2/Standard IBM//';
  var i,m:word; str:string;
  begin mm.PopUp:=true; mm.x:=$FF; mm.y:=$FF; mm.StrUp:='Abeceda';
        PushMenu(s[1]);
        for i:=2 to aCode do AppKeyBuff(#0+_down);
        m:=menu; if m<>0 then begin aCode:=m; Fonts.VFont:=3-m; end ;
        case aCode of 1:str:='KAMENICKYCH'; 2:str:='LATIN 2'; 3:str:='STANDARD IBM' end;
        writeln('ABECEDA: ',str); writeln;
  end;

procedure pInst ;
  var p:^string ;  kod,n,j,cursor,i:byte ;
      SavedAttr : word;  konec: boolean;
  procedure pInstalace(j:byte) ;
  const st:ScreenStr='Standardni (LPT1-4)/Pres "logicky" port (dos.LPT1-9)/Nahradni tisk(manazer)//';
  var   i,m,tmout:word; s:string;
  begin
        mm.PopUp:=true; mm.x:=$FF; mm.y:=$FF; mm.StrUp:='Zpusoby tisku';
        PushMenu(st[1]);
        {for i:=2 to aCode do AppKeyBuff(#0+_down);}
        if Printers[j].pTmOut in [253,254] then AppKeyBuff(#0+_down);
        if Printers[j].pTmOut=253 then AppKeyBuff(#0+_down);
        m:=menu;
        if m<>0 then begin tmOut:=m;
           case tmOut of 1: begin s:='Standardni pres (LPT1-4)';
                         if Printers[j].pTmOut in [253,254] then
                            Printers[j].pTmOut:=255 end;
                      2: begin s:='Pres "logicky" port (LPT1-9)';
                         Printers[j].pTmOut:=254 end;
                      3: begin s:='Nahradni tisk(manazer)';
                         Printers[j].pTmOut:=253 end
                      end;
           writeln('Zpusob tisku: ',s); writeln;
           end
         else exit;


        MyWindow(1,2,80,TxtRows-2,_abs); Frame(_single,'','');
        write(' TISKARNA: ');
        s:=printers[j].pName; EditLine(s,1,8,8,_any); printers[j].pName:=s;
        writeln; writeln;

        if Printers[j].pTmOut=253 then begin
            write('     Nazev kopie:'); s:=printers[j].pCodes[15];
            EditLine(s,1,255,255,_any); printers[j].pCodes[15]:=s; writeln;
            write('  Nazev programu:'); s:=printers[j].pCodes[16];
            EditLine(s,1,255,255,_any); printers[j].pCodes[16]:=s; writeln;
            write(' Param. programu:'); s:=printers[j].pCodes[17];
            EditLine(s,1,255,255,_any); printers[j].pCodes[17]:=s; writeln;
            end
        else begin
          gotoxy(14,WhereY); write(' Typ: '); s:='M' ;
          repeat gotoxy(19,WhereY); s[1]:=printers[j].pType; EditLine(s,1,1,1,_any); printers[j].pType:=s[1];
          until (printers[j].pType in ['M','C','L']) ;
          gotoxy(19,WhereY); writeln(printers[j].pType);
          write(' Kod(KL,KN,LK,LN):');
          case printers[j].pCode of
              'K': s:='KL';
              'k': s:='KN';
              'L': s:='LK';
              'l': s:='LN';
              else s:=' ';
          end ;
          EditLine(s,1,2,2,_any);
          for i:=1 to length(s) do s[i]:=upcase(s[i]);
          gotoxy(19,whereY);  writeln(s:2);
          if s='KL' then printers[j].pCode:='K' else if s='KN' then printers[j].pCode:='k' else
          if s='LK' then printers[j].pCode:='L' else if s='LN' then printers[j].pCode:='l' else printers[j].pCode:=' ';

          write('      Cislo portu: ');
          repeat gotoxy(21,WhereY); Edit(D1,Printers[j].pNo);Disp(D1,Printers[j].pNo);
          until ( (printers[j].pNo in [1..4]) or ((printers[j].pNo in [5..9]) and (tmOut=2) ) ) ;
          writeln('');

          {if printers[j].pNo>4 then Printers[j].pTmOut:=254;}
          if tmOut=1 then begin
             write('     timeout(sec):');
             repeat gotoxy(21,WhereY);
               Edit(D3,Printers[j].pTmOut);Disp(D3,Printers[j].pTmOut);
             until Printers[j].pTmOut in [0..14,255] ;
             writeln;
             end;

          for i:=1 to pItems do begin write(pNames[i]:17,':');
                                    Edit(pTypes[i],printers[j].pCodes[i]);
                                    Disp(pTypes[i],printers[j].pCodes[i]);
                                    writeln;
                                end;
          end;
        writeln;
        writeln(#7,'konec tiskarny - stisknete ESC');
        while ReadKey<>#27 do ;
        end;

  begin
  n:=1 ; cursor:=0;
  GetMem(p,sizeof(printers[1]));
  repeat
    LastLine(' (Enter)  (Esc)  ('+upCh+')  ('+downCh+
             ')   (Ins) nova tiskarna    (Del) zrusit   (Ctrl'+leftCh+') (Ctrl'+rightCh+') zmena poradi');
    MyWindow(1,2,80,TxtRows-2,_abs); Frame(_single,'','');
    writeln(' PREHLED INSTALOVANYCH TISKAREN:');
    SavedAttr:=TextAttr;
    TextAttr:=Colors.mItem; gotoxy(10,3);writeln('                ');
    gotoxy(10,prMax+4);writeln('                ');
    repeat
      for j:=0 to prMax-1 do begin
         if j=cursor then TextAttr:=Colors.mSel else TextAttr:=Colors.mItem;
         gotoxy(10,j+4);writeln('  ',(j+1):2,'. ',printers[j].pName:8,'  '); end ;
      TextAttr:=SavedAttr;
      konec:=false ;
      repeat repeat kl:=ReadKey; until kl in ['0'..'9',#27,#13,#0] ;
             if kl=#0 then kl:=ReadKey ;
      until kl in ['0'..'9',#27,#13,#72,#80,#82,#83,#115,#116] ;
      case kl of
        #72  : cursor := ( Cursor+prMax-1 ) MOD prMax ;
        #80  : cursor := ( Cursor+1 ) MOD prMax ;
        #115 : if cursor>0 then begin
                  move(printers[cursor],p^,sizeof(printers[cursor]));
                  printers[cursor]:=printers[cursor-1];
                  move(p^,printers[cursor-1],sizeof(printers[cursor]));
                  inc(cursor,-1); end ;
        #116 : if (prMax>1) and (cursor<prMax-1) then begin
                  move(printers[cursor],p^,sizeof(printers[cursor]));
                  printers[cursor]:=printers[cursor+1];
                  move(p^,printers[cursor+1],sizeof(printers[cursor]));
                  inc(cursor); end ;
      end ;
    until not(kl in [#72,#80,#115,#116]) ;
    LastLine('  pouzijte klavesy  (Enter)  (Esc)  ('+leftCh+')  ('+rightCh+')   uvnitr okna muzete editovat');
    case kl of
        #27  : konec:= true ;
        #13  : pInstalace(cursor) ;
        #82  : begin if prMax<10 then begin
                        inc(prMax); printers[prMax-1]:=printers[cursor] ;
                        cursor:=prMax-1; pInstalace(cursor); end
                     else message($FF,$FF,'maximalne 10 tiskaren'); end ;
        #83  : begin if prMax>1 then begin
                    if verify($FF,$FF,'Opravdu zrusit tiskarnu ?') then begin
                    for i:=cursor to prMax-2 do printers[i]:=printers[i+1] ;
                    inc(prMax,-1); if cursor=prMax then cursor:=prMax-1; end; end
               else message($FF,$FF,'posledni tiskarnu nelze zrusit'); end ;
      end ;
  until konec ;
  FreeMem(p,sizeof(printers[1]));
  WorkPlace ;
  end ;



procedure dInst;
  var f:file; RecNum:LongInt; RecLen:word; Typ:byte; Nr:word; x,y:real; i:word;
  begin if not(verify($FF,$FF,'Nahrat tabulku kalendarnich vyjimek ze souboru DNY')) then exit;
        assign(f,'DNY.000'); {$I-} reset(f,1); {$I-}
        if IOResult<>0 then begin message($FF,$FF,'Soubor DNY.000 nenalezen'); exit end;
        blockread(f,RecNum,4);
        blockread(f,RecLen,2);
        NWDaysTab:=RecNum; GetMem(WDaysTab,NWDaysTab*3);
        for i:=1 to RecNum do begin blockread(f,y,6); if i=1 then x:=y;
                                    blockread(f,Typ,1);
                                    Nr:=round(y-x);
                                    WDaysTab^[i].typ:=typ; WDaysTab^[i].nr:=nr;
                              end;
        WDaysFirst:=x; WDaysLast:=y;
        close(f);
        writeln('PRACOVNI DNY: tabulka ma ',NWDaysTab,' polozek');
        writeln;
  end;

procedure RdCFG;
  var p:^string; ofsP:word absolute p; l:word;
  procedure RdSPEC;
    begin move(p^,sCodes,sizeof(sCodes)); inc(ofsP,sizeof(sCodes));
    sCodes.ScreenDelay:=sCodes.ScreenDelay div 18 ;
    end;
  procedure RdMON;
    begin move(p^,vMono,sizeof(vMono)); inc(ofsP,sizeof(vMono));
          move(p^,cMono,sizeof(cMono)); inc(ofsP,sizeof(cMono));
          move(p^,vCGA,sizeof(vCGA)); inc(ofsP,sizeof(vCGA));
          move(p^,cCGA,sizeof(cCGA)); inc(ofsP,sizeof(cCGA));
          move(p^,vEGA,sizeof(vEGA)); inc(ofsP,sizeof(vEGA));
          move(p^,cEGA,sizeof(cEGA)); inc(ofsP,sizeof(cEGA));
    end;
  procedure RdFonts;
    begin move(p^,Fonts,sizeof(Fonts)) ; inc(ofsP,sizeof(Fonts)) ;
    end;
  procedure RdABC;
    begin inc(ofsP,sizeof(CharTable)+sizeof(CharTable));
    end;
  procedure RdLPT1;
    var i,j:word;
    begin move(p^,prMax,sizeof(prMax)); inc(ofsP,sizeof(prMax));
          for j:=0 to prMax-1 do begin
             printers[j].pName:=p^; inc(ofsP,succ(length(p^))) ;
             for i:=1 to pItems do begin printers[j].pCodes[i]:=p^; inc(ofsP,succ(length(p^))) end;
             inc(ofsP);
             move(p^,printers[j].pType,sizeof(printers[j].pType)); inc(ofsP,sizeof(printers[j].pType));
             move(p^,printers[j].pCode,sizeof(printers[j].pCode)); inc(ofsP,sizeof(printers[j].pCode));
             move(p^,printers[j].pNo,sizeof(printers[j].pNo)); inc(ofsP,sizeof(printers[j].pNo));
             move(p^,printers[j].pTmOut,sizeof(printers[j].pTmOut)); inc(ofsP,sizeof(printers[j].pTmOut));
             end ;
    end;
  procedure RdWDays;
    begin move(p^,NWDaysTab,sizeof(NWDaysTab)); inc(ofsP,sizeof(NWDaysTab));
          move(p^,WDaysFirst,sizeof(WDaysFirst)); inc(ofsP,sizeof(WDaysFirst));
          move(p^,WDaysLast,sizeof(WDaysLast)); inc(ofsP,sizeof(WDaysLast));
          pointer(WDaysTab):=p; inc(ofsP,NWDaysTab*3);
    end;
  procedure RdNames;
    begin aCode:=byte(p^[0]); inc(ofsP);
    end;
  var f:file;  w:word;
  begin CfgPath:=GetEnv('fandcfg');
        if CfgPath='' then GetDir(0,CfgPath) ;
        if CfgPath[length(CfgPath)]<>'\' then CfgPath:=CfgPath+'\';
        CfgPathFull:=CfgPath+'FAND.CFG';
      { message($FF,$FF,'Zpracuji soubor: '+CfgPath);}
        AttrCfg:= GetFileAttr ;
        SetFileAttr( AttrCfg and $FE ) ;
        Assign(f,CfgPathFull); {$I-} Reset(f,1); {$I+}
        if IOResult<>0 then begin message($FF,$FF,'FAND.CFG not found'); halt end;
        l:=FileSize(f); GetMem(p,l); BlockRead(f,p^,l); Close(f);
        move(p^,version,sizeof(version)); inc(ofsP,sizeof(version));
        if version<>ver then begin
           if version='4.10' then
             message($FF,$FF,'Pozor !!! verze souboru FAND.CFG je 4.10, pri zapisu bude prepsana na '+ver)
           else begin
             message($FF,$FF,'Spatna verze ... FANDINST:'+ver+' ...FAND.CFG:'+version);
             halt;
             end;
           end;
        RdSPEC; RdMON; RdFonts; RdABC; RdLPT1; RdWDays; RdNames;
  end;

procedure WrCFG;
  var i,j:word; f:file; p,q:pointer;
      ch:char;
  procedure WrZero;
    const b:byte=0; begin BlockWrite(f,b,1) end;
  procedure WrFF;
    const b:byte=$FF; begin BlockWrite(f,b,1) end;
  begin assign(f,CfgPath+'FANDCFG.BAK'); {$I-} erase(f); {$I+} i:=IOresult;
        assign(f,CfgPathFull);
        rename(f,CfgPath+'FANDCFG.BAK');
        assign(f,CfgPathFull); rewrite(f,1);
        version:=ver;
        BlockWrite(f,version,sizeof(version));
        sCodes.ScreenDelay:=sCodes.ScreenDelay*18 ;
        BlockWrite(f,sCodes,sizeof(sCodes));
        BlockWrite(f,vMono,sizeof(vMono)); BlockWrite(f,cMono,sizeof(cMono));
        BlockWrite(f,vCGA,sizeof(vCGA)); BlockWrite(f,cCGA,sizeof(cCGA));
        BlockWrite(f,vEGA,sizeof(vEGA)); BlockWrite(f,cEGA,sizeof(cEGA));
        BlockWrite(f,Fonts,sizeof(Fonts)) ;
        case aCode of 1:begin p:=addr(OrdKam); q:=addr(UpcKam) end;
                      2:begin p:=addr(OrdLat); q:=addr(UpcLat) end;
                      3:begin p:=addr(OrdIBM); q:=addr(UpcIBM) end;
                   else halt;
        end;
        BlockWrite(f,p^,sizeof(CharTable));
        BlockWrite(f,q^,sizeof(CharTable));
        BlockWrite(f,prMax,sizeof(prMax));
        for j:=0 to prMax-1 do begin
            BlockWrite(f,printers[j].pName[0],succ(byte(printers[j].pName[0])));
            for i:=1 to pItems do BlockWrite(f,printers[j].pCodes[i,0],succ(byte(printers[j].pCodes[i,0])));
            WrFF;
            blockwrite(f,printers[j].pType,sizeof(printers[j].pType));
            blockwrite(f,printers[j].pCode,sizeof(printers[j].pCode));
            blockwrite(f,printers[j].pNo,sizeof(printers[j].pNo));
            blockwrite(f,printers[j].pTmOut,sizeof(printers[j].pTmOut));
            end ;
        blockwrite(f,NWDaysTab,sizeof(NWDaysTab));
        blockwrite(f,WDaysFirst,sizeof(WDaysFirst));
        blockwrite(f,WDaysLast,sizeof(WDaysLast));
        blockwrite(f,WDaysTab^,NWDaysTab*3);
        BlockWrite(f,aCode,sizeof(aCode));
        close(f);
  end;

procedure Print;
  var i,j,k : word; str:ScreenStr; s:string[2] ;
  begin if not(verify($FF,$FF,'Je tiskarna zapnuta')) then exit;
        writeln('VYPIS: instalovanych hodnot na tiskarne');
        writeln;
        writeln(LST,'FAND.CFG - instalacni soubor pro PC FAND verze ',ver);
        writeln(LST);
      writeln(LST,'******** KONSTANTY ********');
      with sCodes do begin writeln(LST,sNames[1]:38,' : ',UpdCount);
                           writeln(LST);
                           writeln(LST,'SESTAVA':38);
                           writeln(LST,sNames[2]:38,' : ',AutoRprtWidth);
                           writeln(LST,sNames[3]:38,' : ',AutoRprtLimit);
                           writeln(LST,sNames[4]:38,' : ',CpLines);
                           writeln(LST,sNames[5]:38,' : ',AutoRprtPrint);
                           writeln(LST,sNames[6]:38,' : ',ChoosePrMsg);
                           writeln(LST);
                           writeln(LST,'ODDELENI STRANEK V TEXTU');
                           writeln(LST,sNames[7]:38,' : ',TxtInsPg);
                           writeln(LST,sNames[8]:38,' : ',TxtCharPg);
                           writeln(LST);
                           writeln(LST,sNames[9]:38,' : ',ESCverify);
                           writeln(LST,sNames[10]:38,' : ',NoPrompt158);
                           writeln(LST,sNames[11]:38,' : ',F10Enter) ;
                           writeln(LST,sNames[12]:38,' : ',RDBcomment);
                           writeln(LST,sNames[13]:38,' : ',CPMdrive);
                           writeln(LST);
                           writeln(LST,'SITE LAN':38);
                           writeln(LST,sNames[14]:38,' : ',RefreshDelay);
                           writeln(LST,sNames[15]:38,' : ',NetDelay);
                           writeln(LST,sNames[16]:38,' : ',LockDelay);
                           writeln(LST,sNames[17]:38,' : ',LockRetries);
                           writeln(LST,sNames[18]:38,' : ',LockBeepAllowed);
                           writeln(LST);
                           writeln(LST,sNames[19]:38,' : ',Beep);
                           writeln(LST,sNames[20]:38,' : ',XMSMaxKb);
                           writeln(LST,sNames[21]:38,' : ',NoCheckBreak);
                           writeln(LST,sNames[22]:38,' : ',KbdTyp);
                           writeln(LST);
                           writeln(LST,'PODPORA MYSI':38);
                           writeln(LST,sNames[23]:38,' : ',NoMouseSupport);
                           writeln(LST,sNames[24]:38,' : ',MouseReverse);
                           writeln(LST,sNames[25]:38,' : ',DoubleDelay);
                           writeln(LST,sNames[26]:38,' : ',RepeatDelay);
                           writeln(LST);
                           {writeln(LST,sNames[27]:38,' : ',CtrlDelay);}
                           writeln(LST,sNames[28]:38,' : ',OverwrLabeledDisk);
                           writeln(LST,sNames[29]:38,' : ',ScreenDelay);
                           writeln(LST,sNames[30]:38,' : ',OffDefaultYear);
                           writeln(LST,sNames[31]:38,' : ',WithDiskFree);
                     end;
      writeln(LST); writeln(LST);
      writeln(LST,'********* MONITOR *********');
      if cCurr=@cMono then str:='Monochrom' else
      if cCurr=@cCGA then str:='CGA' else str:='EGA';
      writeln(LST,'typ':38,' : ',str);
      with vCurr^ do begin writeln(LST,vNames[1]:38,' : ',hexw(vseg));
                           writeln(LST,vNames[2]:38,' : ',TxtRows);
                           writeln(LST,vNames[3]:38,' : ',chks);
                           writeln(LST,vNames[4]:38,' : ',hexw(on));
                           writeln(LST,vNames[5]:38,' : ',hexw(off));
                           writeln(LST,vNames[6]:38,' : ',hexw(big));
                     end;
      with Fonts do begin  writeln(LST,fNames[2]:38,' : ',LoadVideoAllowed);
                           writeln(LST,fNames[3]:38,' : ',NoDiakrSupported);
                    end ;

      write(LST,^L);
      message($FF,$FF,'Nastav novou stranku');
      writeln(LST);
      writeln(LST,'********** BARVY **********');
      for i:=1 to cItems do begin write(LST,cNames[i]:30,' : ',hexb(byte(cCurr^[i])),'     ');
                                  if not(odd(i)) then writeln(LST);
                            end;

      if odd(i) then writeln(LST);
      writeln(LST); writeln(LST);
      case Fonts.Vfont of 2:str:='KAMENICKYCH'; 1:str:='LATIN 2'; 0:str:='STANDARD IBM' end;
      writeln(LST,'ABECEDA':20,' : ',str);
      writeln(LST,^L);
      message($FF,$FF,'Nastav novou stranku');
      writeln(LST);

      for j:=0 to prMax-1 do begin
          writeln(LST,'TISKARNA':20,' : ',printers[j].pName);
          writeln(LST,'     Typ':20,' : ',printers[j].pType);
          case printers[j].pCode of
               'K': s:='KL';
               'k': s:='KN';
               'L': s:='LK';
               'l': s:='LN';
               else s:=' ';
               end ;
          writeln(LST,'Kod tiskarny':20,' : ',s);
          writeln(LST,'Cislo portu':20,' : ',printers[j].pNo);
          writeln(LST,'    TimeOut':20,' : ',printers[j].pTmOut);
          for i:=1 to pItems do begin write(LST,pNames[i]:20,' : ');
                               for k:=1 to length(printers[j].pCodes[i]) do begin
                                 if (k MOD 19 = 1) and (k>2) then begin
                                     writeln(LST);write(LST,'                       ');
                                     end ;
                                 write(LST,hexb(byte(printers[j].pCodes[i,k])):3);
                                 end ;
                               writeln(LST);
                               end;
          write(LST,^L);
          message($FF,$FF,'Nastav novou stranku');
          end ;
      write(LST,^L);
  end;

procedure MainMenu;
  const s1:ScreenStr='/Barvy/Tiskarna/Konstanty/Abeceda/Monitor/Dny/Vypis/Ukonceni//';
        s2:ScreenStr='S nahranim zmen na disk/Beze zmen//';
  var m:word;
  begin mm.x:=1; mm.y:=1; PushMenu(s1[1]);
        repeat beep; m:=menu; MenuOff;
               case m of 1:cInst;
                         2:pInst;
                         3:sInst;
                         4:aInst;
                         5:vInst;
                         6:dInst;
                         7:print;
                         8:begin mm.x:=$FF; mm.y:=$FF; mm.StrUp:='Ukonceni';
                                 PushMenu(s2[1]);
                                 m:=menu;
                                 case m of 1:begin WrCFG; exit end;
                                           2:exit;
                                 end;
               end         end;
        until  false;
  end;

begin RdCFG;
      if LastMode=mono then begin vCurr:=@vMono; cCurr:=@cMono end else
         if EGApresent then begin vCurr:=@vEGA; cCurr:=@cEGA end else
                            begin vCurr:=@vCGA; cCurr:=@cCGA end;
      WorkPlace ;
      LastLine('  pouzijte klavesy  (Enter)  (Esc)  ('+leftCh+')  ('+rightCh+')   uvnitr okna muzete editovat');
      MainMenu;
      SetFileAttr( AttrCfg ) ;
end.
