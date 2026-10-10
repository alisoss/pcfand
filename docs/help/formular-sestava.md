# Formulář, sestava, transformace (E, R, M)

<a name="t241"></a>
## barvy

<pre>
                                   <b>BARVY</b>
 ────────────────────────────────────────────────────────────────────────────
  ╔═══════════════════════════════════════════════════════════════╦══════════╗
  ║ <b>písmo</b>   černá  modrá zelená  cyan  červená fialová hnědá šedá ║  <b>pozadí</b>  ║
  ╠═══════╦═══════════════════════════════════════════════════════╣          ║
  ║ <b>jas</b>  L║  ***    001    002    003    004    005    006    007 ║  černá   ║
  ║      <b>H</b>║<b>  008    009  <i>^s</i>010  <i>^w</i>011  <i>^d</i>012  <i>^b</i>013  <i>^a</i>014    015</b> ║          ║
  ║      L║  016    017    018    019    020    021    022    023 ║  modrá   ║
  ║      <b>H</b>║<b>  024    025    026    027    028    029  <i>^q</i>030    031</b> ║          ║
  ║      L║  032    033    034    035    036    037    038    039 ║  zelená  ║
  ║      <b>H</b>║<b>  040    041    042    043    044    045    046    047</b> ║          ║
  ║      L║  048    049    050    051    052    053    054    055 ║  cyan    ║
  ║      <b>H</b>║<b>  056    057    058    059    060    061    062    063</b> ║          ║
  ║      L║  064    065    066    067    068    069    070    071 ║  červená ║
  ║      <b>H</b>║<b>  072    073    074    075    076    077  <i>^e</i>078    079</b> ║          ║
  ║      L║  080    081    082    083    084    085    086    087 ║  fialová ║
  ║      <b>H</b>║<b>  088    089    090    091    092    093    094    095</b> ║          ║
  ║      L║  096    097    098    099    100    101    102    103 ║  hnědá   ║
  ║      <b>H</b>║<b>  104    105    106    107    108    109    110    111</b> ║          ║
  ║      L║  112    113    114    115    116    117    118    119 ║  šedá    ║
  ║      <b>H</b>║<b>  120    121    122    123    124    125    126    127</b> ║          ║
  ╚═══════╩═══════════════════════════════════════════════════════╩══════════╝
  <b>blikání</b> = hodnota + 128
  <a href="editory.md#t242">barvy a typy písma</a>
</pre>

<a name="t361"></a>
## formulář

*Též:* `E`

<pre>
                           <b>FORMULÁŘ - kapitola E</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Formulář</b> definuje  rozvrh  obrazovky  pro editaci souboru datovým editorem
   ( hlavičku  obrazovky, pořadí a  rozmístění  údajů jedné věty a doprovodné
   texty).

   ██ Syntaxe:          <b>{</b> Komentář <b>}</b>
                        [ TextHlavičky ]
                        <b>#_</b>NázevSouboru PopisnáČást <b>;</b>
                        ZobrazovacíČást

   ■ <b>{Komentář}</b>........ (volitelně) nezahrne se do hlavičky. Pro programátora.
   ■ <b>TextHlavičky</b>...... (volitelně): na obrazovce bude jednou před první větou
                        Hlavička může mít i několik řádků.
   ■ <b>NázevSouboru</b> ..... název kapitoly F, kde je soubor deklarován. V příkazu
                        edit lze použít tento formulář i pro jiný soubor.
   ■ <b>PopisnáČást</b> ...... seznam editovaných údajů (příp. s <b>pořadovým číslem</b>).
                        [číslo<b>:</b>] NázevÚdaje { <b>,</b>[číslo<b>:</b>] NázevÚdaje }
                        Může obsahovat uložené nebo vypočítané údaje. 
                        
                        Pořadí editace (<b>číslo:údaj</b>) se uvádí, jestliže bude 
                        editace probíhat v jiném pořadí než zleva doprava 
                        a odshora dolů. Editace probíhá podle vzestupného pořadí 
                        čísel, které nemusí být souvislé (1,10,19,31,...).
                        Pokud není číslo uvedeno, dosadí se implicitní podle 
                        pořadí od začátku seznamu.

   ■ <b>ZobrazovacíČást</b>... předloha pro editaci jedné věty datovým editorem
                        (na obrazovce vystoupí tolik vět, kolik se jich vejde)
                        Jde o libovolný text (včetně rámečků a barev), který 
                        se při editaci opíše na obrazovku. Na místech, kde 
                        vystupují údaje souboru, jsou v textu <b>masky</b> pro 
                        editaci údajů. '<b>\</b>' v zobrazovací části znamená 
                        odstránkování (přechod na další obrazovku u dlouhých 
                        formulářů).

   <b>Maska</b>
   Je řetězec podtržítek '<b>_</b>', jehož  délka  je dána  deklarací  údaje. Seznam
   údajů v popisné části musí odpovídat z maskám v zobrazovací části ( stejný
   počet, odpovídající  délky  a  pořadí).
          Typ údaje        maska(počet znaků '_')
          F,n.m            n+m+2  je-li m&gt;0
                           n+m+1  je-li m=0
          R                17
          A,n              k      kde k&lt;=n
          N,n              n
          D                podle délky masky (např. ________ při 'DD.MM.YY')
          B                1
          T                1
   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t282">edit</a>     <a href="soubory.md#t350">uživatelské pohledy</a>      <a href="#t241">barvy</a>     <a href="editory.md#t232">rámečky</a>      <a href="#t362">░ formulář</a>     <a href="#t786">»E</a>
</pre>

<a name="t362"></a>
## ░ formulář

<pre>
                             <b>PŘÍKLAD - FORMULÁŘ</b>
 ──────────────────────────────────────────────────────────────────────────────
 ░ {tento komentář bude zobrazen v projektantském prostředí u seznamu kapitol}
 ░                    ╔══════════════════╗
 ░                    ║ SEZNAM ZÁKAZNÍKŮ ║                           {hlavička}
 ░                    ╚══════════════════╝
 ░ #_ADRESÁŘ Firma,50:Ičo,2:Ulice,60:Telefon,3:Psč,40:Místo,70Fax;{popisná část}
 ░                   ┌─────────────────┐                     {zobrazovací část}
 ░             Firma │ _______________ │       IČO: _________
 ░                   └─────────────────┘
 ░             Ulice:  _______________     Telefon: _____________
 ░  PSČ: _____ Místo:  _______________     Telefax: _____________
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t365"></a>
## sestava

*Též:* `R`

<pre>
                            <b>SESTAVA - kapitola R</b>
   ───────────────────────────────────────────────────────────────────────────
   Definice <b>tiskové sestavy</b> pro prohlížení na obrazovce textovým editorem nebo
   tisk na tiskárně. V deklaraci je třeba uvést název  souboru a alespoň jednu
   výstupní úroveň, všechny ostatní konstrukce jsou nepovinné.

   █ <a href="#t391">DeklaraceLokálníchProměnných</a> ... pomocné proměnné použité v sestavě
   █ <a href="#t368">Stránkování</a> .................... změna implicitního stránkování
   █ <a href="#t369">VstupSestavy</a> ................... vstupní soubor(y),třídění,vstupní filtry
   █ <a href="#t378">VýstupSestavy</a> .................. textová předloha pro generování sestavy

   Vytváření sestavy lze přerušit klávesou <b>Esc</b>.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t556">report</a>     <a href="prostredi.md#t248">automatická sestava</a>     <a href="#t386">░ sestava</a>     <a href="#t787">»R</a>
</pre>

<a name="t368"></a>
## Stránkování

*Též:* `pagesize`, `pagelimit`

<pre>
                           <b>STRÁNKOVÁNÍ V SESTAVĚ</b>
   ──────────────────────────────────────────────────────────────────────────
   Stránkování v sestavě je určeno pro změnu implicitních hodnot:
    
   ■ <b>fyzická strana</b> ... délka jedné strany papíru (<b>pagesize</b>)
   ■ <b>logická strana</b> ... počet řádek využitých sestavou na straně (<b>pagelimit</b>)

   Implicitní nastavení fyzické  strany je <b>72</b> řádků (12 palců), délka logické
   strany je dána instalačním programem (parametr <b>automatická sestava - délka</b>,
   obvykle <b>69</b> řádků). Délku  fyzické  i  logické  strany v sestavě lze změnit
   přiřazením na začátku deklarace sestavy (před názvem vstupního souboru).

   █ <b>.pagesize:=ČísVýraz</b> ... řízení stránkování na začátku sestavy (kap. R)
   █ <b>.pagelimit:=ČísVýraz</b>
   █ <b>pagelimit</b> ............. také funkce v popisné části výstupní úrovně reportu
 
   ░ .pagesize:=43;                  {tisk do kratších formulářů}
   ░ #I_FAKTURY
   ░ ... atd ...
   ░ .pagelimit:=55;                 {tisk na volné listy}
   ░ .pagelimit:=PARAM.DélkaStrany   {vlastní instalace v programu}
   ──────────────────────────────────────────────────────────────────────────
   <a href="jazyk.md#t383">page</a>     <a href="procedury.md#t293">line</a>
</pre>

<a name="t369"></a>
## VstupSestavy

<pre>
                               <b>VSTUP SESTAVY</b>
 ─────────────────────────────────────────────────────────────────────────────
 █ <b>#I_NázevSouboru</b> .... název kapitoly F, kde je soubor deklarován
 █ <b>#Ii_NázevSouboru</b> ... pro více vstupních souborů (i=1 až 9)

 Každý vstupní soubor sestavy může být doplněn:
 █ ...NázevSouboru <b>! (LogVýraz) ŘídícíÚdaje;TřídícíÚdaje</b> ... <b>upřesnění vstupu</b>
 ■ <b>/@</b> nebo <b>/NázevKlíče</b> za názvem souboru ... vstup vět v pořadí podle vlastního
      nebo alternativního vlastního <b>klíče</b> (jinak fyzické pořadí vět)
 ■ <b>!</b> ... <b>dočasné třídění</b> podle řídících a třídících údajů (v tomto pořadí)
         (pouze pro pořadí vět v sestavě, nemá vliv na data)
 ■ <b>LogVýraz</b> ..... vstupní logická podmínka (filtr) - vstoupí jen podmnožina vět
                       v podmínce lze použít <a href="jazyk.md#t440">key in</a> - rychlý výběr podle indexu
 ■ <b>?</b> místo LogVýrazu .. podmínku  zadá uživatel  nebo volající procedura <b>report</b>
 ■ <b>ŘídícíÚdaje</b> ........ rozdělují  soubor do skupin  vět se stejnými  hodnotami
                        řídících údajů, je nutné setřídění podle řídících údajů
 ■ <b>TřídícíÚdaje</b> ....... pro třídění (<b>!</b>) uvnitř jedné skupiny, mohou být použity
                        jako individuální řídící údaje pro <b>i</b>-tý soubor (CHi,CFi)
 ■ <b>&gt;</b> před řídícím (třídícím) údajem ... <b>sestupné třídění</b>
 ■ <b>~</b> před řídícím (třídícím) údajem ... <b>lexikální třídění</b>
 Více vstupních souborů: řídící údaje u všech vstupů mají stejný typ a délku

 ┌──────────────────────────────────────────────────────────────────────────┐
 │ Řídící a třídící údaje vytvářejí společný seznam údajů, podle kterých    │
 │ musí být vstupní soubor setříděn. Určitý údaj proto může být buďto řídící│
 │ nebo třídící, nemá smysl ho uvádět v obou skupinách.                     │
 └──────────────────────────────────────────────────────────────────────────┘
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t378">VýstupSestavy</a>       <a href="soubory.md#t336">klíče</a>     <a href="jazyk.md#t668">lexikální třídění</a>     <a href="prostredi.md#t249">řídící a třídící údaje</a>
 <a href="#t386">░ sestava</a>           <a href="#t787">»R</a>
</pre>

<a name="t378"></a>
## VýstupSestavy

*Též:* `RH`, `RF`, `PH`, `PF`, `CH`, `CF`, `DH`, `DE`

<pre>
                               <b>VÝSTUP SESTAVY</b>
 ──────────────────────────────────────────────────────────────────────────────
 Výstup sestavy je členěn do <b>úrovní výstupu</b>:
 ■ <b>#RH</b> ...(zkratka Report Heading) začátek celé sestavy (vystoupí v sestavě..)
 ■ <b>#RF</b> ...(Report Footing) ukončení celé sestavy           (..pouze jednou)
 ■ <b>#PH</b> ...(Page Heading) začátek tiskové strany            (vystoupí..)
 ■ <b>#PF</b> ...(Page Footing) ukončení tiskové strany, <a href="jazyk.md#t383">notatend</a> (..na každé straně)
 ■ <b>#CH_ŘídícíÚdaj</b> ...(Control Heading) začátek skupiny     (stejné hodnoty..)
 ■ <b>#CHi_TřídícíÚdaj</b>... začátek skupiny pro individuální řídící úroveň daného
          vstupu s číslem <b>i</b>. Třídící údaje jsou v podstatě individuální řídící
          údaje pro dané číslo vstupního souboru. Pozor ! CH1 není to samé 
          jako CH, pro srovnání viz. úroveň DE.
 ■ <b>#CF_ŘídícíÚdaj</b> ...(Control Footing) ukončení skupiny    (..řídícího údaje)
 ■ <b>#CFi_TřídícíÚdaj</b>... analogicky jako CHi.
 ■ <b>#DH</b> ...(Detail Heading) hlavička skupiny vět (po odstránkování, při změně 
          řídících údajů nebo vstupního souboru pro DE) ... <a href="jazyk.md#t383">notsolo</a>
 ■ <b>#DEi</b>...(DEtail) výstup jedné věty souboru (vystoupí pro každou větu) pro
          odpovídající vstupní soubor (<b>i</b>). Pokud <b>i</b> chybí, použije se <b>i=1</b>.

 Pořadí deklarace úrovní a jejich počet jsou libovolné. V sestavě  může  být  i
 několik úrovní stejného typu. Každá úroveň začíná znakem '<b>#</b>' a zkratkou (příp.  
 pořadovým číslem) a má následující syntaxi:

 █ <b>#Zkratka (LogVýraz)</b> ..... označení <b>úrovně</b>, volitelně <b>výstupní podmínka</b>
 █ <a href="prostredi.md#t379">PopisnáČástÚrovně</a><b>;</b> ...... popis vystupujících dat (seznam výrazů), výpočty
 █ <a href="prostredi.md#t384">ZobrazovacíČástÚrovně</a> ... textová předloha pro generování výstupu
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t369">VstupSestavy</a>     <a href="editory.md#t240">tisk textu</a>     <a href="editory.md#t242">barvy a typy písma</a>     <a href="#t386">░ sestava</a>     <a href="#t787">»R</a>
</pre>

<a name="t386"></a>
## ░ sestava

<pre>
                             <b>PŘÍKLAD - SESTAVA</b>
──────────────────────────────────────────────────────────────────────────────
 ░ #I_DENIK (Datum in [1.1.92..31.12.92])
 ░ #RH today;
 ░            ÚČETNÍ DENÍK           dne: __.__.__
 ░ #PH ;
 ░ Datum    Text         Kč     Daň
 ░ #DE Datum, Text, Částka, ProcDaň, Částka*ProcDaň/100;
 ░ __.__.__ __________ _____.__ ___ % tj _____.__ ,-
 ░ #PF page;
 ░            strana: ___
 ░ #RF sum(Kč ), sum(Částka*ProcDaň/100);
 ░            Kč  celkem: ________ Kč
 ░            Daň celkem: ________ Kč
 ░
 ░             vypracoval ................
 ░            kontroloval ................
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t389"></a>
## transformace

*Též:* `M`

<pre>
                         <b>TRANSFORMACE - kapitola M</b>
 ──────────────────────────────────────────────────────────────────────────────
 <b>Transformace</b> definuje sekvenční operace s datovými soubory:

 █ <a href="#t391">DeklaraceLokálníchProměnných</a> ... pomocné proměnné použité v transformaci
 █ <a href="#t392">VstupTransformace</a> .............. vstupní soubor(y)
 █ <a href="#t395">VýstupTransformace</a> ............. výstupní soubor(y) a způsob transformace

 Deklarace proměnných je volitelná, transformace však  musí  obsahovat  alespoň
 jeden vstupní a jeden výstupní soubor.

 Algoritmus transformace: čtou se vstupní soubory (případně jen část vět  podle
 vstupního filtru), podle definice výstupu přitom vystupují věty do  výstupních
 souborů.
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t566">merge</a>     <a href="#t788">»M</a>
</pre>

<a name="t391"></a>
## DeklaraceLokálníchProměnných

*Též:* `lokální proměnné`

<pre>
                       <b>DEKLARACE LOKÁLNÍCH PROMĚNNÝCH</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Lokální proměnné</b> slouží pro  uchovávání mezivýsledků uvnitř jedné <b>sestavy</b>,
   <b>transformace</b>, <b>procedury</b> nebo <b>funkce</b>.  Jejich typy odpovídají <b>typům výrazů</b>.
   Pouze v proceduře lze navíc použít<a href="#t407"> record</a> proměnnou (věta souboru),
   proměnnou typu pracovní <a href="procedury.md#t414">index</a> nebo lokální soubor (typ <a href="soubory.md#t417">file</a>) .

   █ <b>var SeznamProměnných:Typ</b> .............. lokální proměnná
   █ var SeznamProměnných:Typ<b>=PočHodnota</b> ... s počáteční hodnotou (konstanta)

   ░ var i,j,k:real;                                    i:=j+k;
   ░     max:real=999999;                               max+=1;
   ░     s1,s2:string;                                  write(s1+s2);
   ░     Hlášení:string='Čekejte, probíhá výpočet !';   message(Hlášení);
   ░     OK:boolean;                                    if OK then exit;
   ░     x,y,z:boolean=true;                            x:=y&lt;=&gt;z;
   ──────────────────────────────────────────────────────────────────────────
   <a href="jazyk.md#t658">typy výrazů</a>     <a href="procedury.md#t442">přiřazovací příkaz</a>     <a href="#t407">record proměnná</a>     <a href="#t787">»R</a>    <a href="#t788">»M</a>     <a href="jazyk.md#t789">»P</a>
</pre>

<a name="t392"></a>
## VstupTransformace

<pre>
                             <b>VSTUP TRANSFORMACE</b>
──────────────────────────────────────────────────────────────────────────────
 <b>Vstup  transformace</b>  tvoří  seznam  jednoho  až   devíti   vstupních   souborů
 transformace, každý vstupní soubor může být doplněn:
 █ <b>#Ii_NázevSouboru ! (LogVýraz) ŘídícíÚdaje;TřídícíÚdaje</b> ... vstup merge
 ■ <b>i</b> ... číslo od 1 do 9, vstupní soubory se očíslují pro odkazy ve výstupu
 ■ <b>/@</b> nebo <b>/NázevKlíče</b> za názvem souboru... vstup vět v pořadí podle vlastního
      nebo alternativního vlastního <b>klíče</b> (jinak fyzické pořadí vět)
 ■ <b>!</b> ... <b>dočasné třídění</b> podle spojeného seznamu řídících a třídících údajů
   (pořadí vstupních vět, nemění původní soubor, pokud není zároveň výstupním)
 ■ <b>LogVýraz</b> ....... vstupní logická podmínka (filtr) - vstoupí podmnožina vět
                    v podmínce lze použít <a href="jazyk.md#t440">key in</a> - rychlý výběr podle indexu
 ■ <b>ŘídícíÚdaje</b> .... rozdělují soubor do skupin vět se stejnými hodnotami
                    řídících údajů
 ■ <b>TřídícíÚdaje</b> ... pro třídění (<b>!</b>) uvnitř jedné skupiny
 ■ <b>&gt;</b> před řídícím (třídícím) údajem ... <b>sestupné třídění</b>
 ■ <b>~</b> před řídícím (třídícím) údajem ... <b>lexikální třídění</b>

 Vstupní soubor musí být <b>setříděn podle řídících  údajů</b>  nebo  vstupovat  podle
 příslušného  indexu.  Při  více  vstupních  souborech  si  musí  řídící  údaje
 odpovídat (stejný počet a typy). Soubory se  čtou  v  pořadí  podle  deklarace
 vstupu buď po skupinách vět (když jsou definované řídící údaje) nebo celé.
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t395">VýstupTransformace</a>     <a href="soubory.md#t336">klíče</a>     <a href="prostredi.md#t249">řídící a třídící údaje</a>   <a href="#t401">░ transformace</a>   <a href="#t788">»M</a>
</pre>

<a name="t395"></a>
## VýstupTransformace

*Též:* `O`, `dummy`

<pre>
                            <b>VÝSTUP TRANSFORMACE</b>
   ──────────────────────────────────────────────────────────────────────────
   Určuje  <b>výstupní  soubory</b>  transformace, způsob  naplnění výstupních dat a
   intervaly zápisu do  výstupních  souborů.  Druhy výstupu určují, ve kterém
   okamžiku čtení vstupu se provede výstup:

   █ <b>#Oi_NázevSouboru</b> ... <b>detailní výstup</b> (vystoupí za každou vstupní větu)
                          <b>i</b> je pořadové číslo odpovídajícího vstupního souboru
   █ <b>#O_NázevSouboru</b> .... <b>skupinový výstup</b> (vystoupí jednou za skupinu)
                          skupina vět se stejnými řídícími údaji
   █ <b>#O*_NázevSouboru</b> ... <b>násobný výstup</b> (vystoupí pro každou kombinaci vět)
                          součin vstupních souborů po skupinách
   ■ klíčové slovo <b>dummy</b> místo NázvuSouboru ..... neprobíhá fyzický výstup do
                                       souboru, pouze přiřazení do proměnných
   Každý výstup může být doplněn:
 
   █ #O1_NázevSouboru <b>+ (LogVýraz) Přiřazení;</b> ... rozšíření výstupu
   ■ <b>+</b> ........ nové věty (i indexy) připojí za konec souboru (jinak přepsání)
                  připojovat nelze za soubor, který je zároveň vstupní
   ■ <b>LogVýraz</b> ... výstupní logická podmínka (výstup se provede, je-li splněna)
   ■ <b>Přiřazení</b>... přiřazovací příkazy pro naplnění výstupní věty
                  (výpočetní část výstupu)
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t396">PříkazováČástVýstupu</a>     <a href="#t392">VstupTransformace</a>     <a href="#t401">░ transformace</a>     <a href="#t788">»M</a>
</pre>

<a name="t396"></a>
## PříkazováČástVýstupu

<pre>
                    <b>PŘÍKAZOVÁ ČÁST VÝSTUPU TRANSFORMACE</b>
 ──────────────────────────────────────────────────────────────────────────────
 <b>Příkazová část</b> (seznam  přiřazovacích  příkazů)  definuje  obsah  vět  souborů
 vystupujících  v  transformaci.  Pokud  chybí,  údaje  se   naplní   hodnotami
 stejnojmenných údajů ze vstupních souborů podle <b>implicitních  pravidel</b>,  příp.
 zůstanou prázdné. Jinak obsahuje seznam <b>přiřazovacích příkazů</b>  (<b>:=</b>  nebo  <b>+=</b>),
 lze použít i konstrukci <b>if-then-else</b>.

 <b>Objekty přiřazení</b> v přiřazovacích  příkazech  jsou  <b>uložené  údaje</b>  výstupního
 souboru, <b>lokální proměnné</b> transformace nebo <b>globální proměnné</b> úlohy. <b>Výrazy</b> na
 pravé straně přiřazovacího příkazu mohou obsahovat údaje ze vstupních  souborů
 (<b>NázevÚdaje</b>), příp. ze souborů s nimi spojených klíčem (<b>NázevKlíče.NázevÚdaje</b>,
 <b>NázevSouboru.NázevÚdaje</b> nebo <b>NázevRole.NázevÚdaje</b>).  Všechny  tyto  konstrukce
 mohou být opatřeny prefixem:

 ■ <b>Ii.</b>NázevÚdaje ... <b>i</b> udává číslo vstupního souboru
   (také <b>Ii.NázevSouboru.NázevÚdaje</b>, příp. <b>Ii.NázevKlíče...</b>, <b>Ii.NázevRole...</b>)
 ■ <b>O.</b>NázevÚdaje .... údaj z právě připravované věty výstupního souboru
 ■ <b>implicitní doplnění prefixu</b>: <b>detailní výstup</b> (<b>#Oi</b>): #Ii, #I1..#Ii-1
              <b>skupinový</b> nebo <b>násobný výstup</b> (<b>#O</b>, <b>#O*</b>): #I1..#In
 ──────────────────────────────────────────────────────────────────────────────
 <a href="soubory.md#t400">speciální funkce v transformaci</a>     <a href="procedury.md#t442">přiřazovací příkaz</a>     <a href="#t401">░ transformace</a>   <a href="#t788">»M</a>
</pre>

<a name="t401"></a>
## ░ transformace

<pre>
                           <b>PŘÍKLAD - TRANSFORMACE</b>
──────────────────────────────────────────────────────────────────────────────
 ░ #I1_PRIJEMKY
 ░ #O1_PRIJEMKY (Datum =&gt; 1.1.93)                           {výstupní podmínky}
 ░ #O1_ARCHIV + (Datum &lt; 1.1.93) Vyřazeno:=today;                   {připojení}

 ░ #I1_JIZDY !  Auto                 {automatické třídění podle řídícího údaje}
 ░ #O_JIZDY     Km:=sum(Km);                                         {kumulace}
 ░              Spotřeba:=sum(Spotřeba);

 ░ #I1_DATA/OsČíslo  Číslo                                   {hledání duplicit}
 ░ #O1_DUPL (I1.Count&gt;1)

 ░ #I1_SKLAD                                                {rozdělení souboru}
 ░ #O1_SKLAD1 (Sklad='01')
 ░ #O1_SKLAD2 (Sklad='02')
 ░ #O1_SKLAD3 (Sklad='03')

 ░ #I1_SKLAD1                                                 {spojení souborů}
 ░ #I2_SKLAD2
 ░ #O1_SKLAD  Sklad:='01'
 ░ #O2_SKLAD  Sklad:='02'
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t407"></a>
## record proměnná

*Též:* ` record`

<pre>
                              <b>RECORD PROMĚNNÁ</b>
 ──────────────────────────────────────────────────────────────────────────────
 <b>Record proměnná</b> je použitelná jako lokální proměnná nebo parametr v  proceduře
 pro uchování jedné věty datového souboru.  Pohodlněji než přímým  přístupem do
 jednotlivých údajů umožňuje  číst, zapisovat a přiřazovat  celé věty najednou.
 Použití record proměnné má vliv i na rychlost programu, záleží na kontextu.

 █ <b>var RecordProměnná:record of NázevSouboru</b> ... deklarace record proměnné

 █ <b>RecordProměnná.NázevÚdaje</b>:=Výraz ... přiřazení do údaje věty
 █ <b>RecordProměnná.NázevÚdaje</b> .......... přístup k údajům ve výrazech
 █ <b>RecordProměnná.NázevSpojení.NázevÚdaje</b> ... <a href="jazyk.md#t346">viditelný údaj</a> z nadřízeného 
                                              souboru, i vícenásobně
 █ <b>RecordProměnná.</b><a href="jazyk.md#t438">owned</a>(...) .......... přístup k podřízeným větám

 █ RecordProměnná<b>:=</b>RecordProměnná ..... přiřazení celé věty
   Nemusí mít stejnou strukturu, stejnojmenné údaje se přenášejí jako v merge.
   Položky na levé straně, které nejsou implicitně naplněny (není stejnojmenný
   údaj na pravé straně) se <b>nenulují</b>.  To mimo jiné znamená, že je možno takto
   postupně "skládat" jednu větu z několika jiných - navzájem různých.
   Jsou-li ovšem názvy položky stejné a typy nekompatibilní, bude takový údaj
   "vynulován".

 Další použití: <a href="procedury.md#t441">forall</a> cyklus, vazba <a href="soubory.md#t437">owner</a>.
 ─────────────────────────────────────────────────────────────────────────────
 <a href="procedury.md#t403">readrec</a>     <a href="procedury.md#t572">deleterec</a>     <a href="jazyk.md#t789">»P</a>       <a href="#t408">░ record proměnná</a>     <a href="soubory.md#t348">aditivní změny</a>
 <a href="procedury.md#t404">writerec</a>    <a href="procedury.md#t572">isdeleted</a>     <a href="jazyk.md#t573">keyof</a>    <a href="procedury.md#t442">přiřazovací příkaz</a>    <a href="procedury.md#t568">přímý přístup k datům</a>
 <a href="procedury.md#t574">linkrec</a>
</pre>

<a name="t408"></a>
## ░ record proměnná

<pre>
                         <b>PŘÍKLAD - RECORD PROMĚNNÁ</b>
 ──────────────────────────────────────────────────────────────────────────────
 ░ var Věta:record of DATA;                         {deklarace record proměnné}
 ░     V1,V2:record of ČÍSELNÍK;

 ░ readrec(V2,i);                                             {přístup k větám}
 ░ V1:=V2;
 ░ V1.Datum:=today;
 ░ writerec(V2,i,+);

 ░ forall Věta do ...                                          {forall a owner}
 ░ forall Věta owner ROLE1(V) do ...
 ░ edit(DATA,(),owner=Věta)
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t538"></a>
## seznam údajů

<pre>
                           <b>SYNTAXE SEZNAMU ÚDAJŮ</b>
   ──────────────────────────────────────────────────────────────────────────

   V parametrech <a href="prostredi.md#t519">dupl</a>, <a href="prostredi.md#t519">noed</a>, <a href="prostredi.md#t519">tab</a>, <a href="procedury.md#t288">exit</a> příkazu <a href="prostredi.md#t282">edit</a> se může vyskytovat seznam
   údajů daného souboru, který má jednotnou syntaxi.

   ██ syntaxe :    [<b>^</b>] <b>(</b> [ NázevÚdaje [ {<b>,</b>NázevÚdaje }]] <b>)</b>

   Jak vidno, seznam  údajů je vždy uzavřen  v kulatých závorkách , jednotlivé
   údaje jsou odděleny čárkou. Prázdný seznam <b>()</b> znamená všechny údaje souboru. 
   Znak <b>^</b> před seznamem znamená <b>doplněk</b> - tj. všechny neuvedené <b>uložené</b> údaje.

   Poznámka:  <b>()</b> a <b>^()</b> mají totožný význam - tj. všechny údaje

   ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t556"></a>
## volání sestavy

*Též:* `report`

<pre>
                              <b>TISKOVÁ SESTAVA</b>
   ───────────────────────────────────────────────────────────────────────────
   Vyvolání tiskové sestavy podle kapitoly R nebo automatické sestavy (PC FAND
   sám umístí údaje v sestavě a doplní standardní hlavičku a ukončení).

   ██ syntaxe:  <b>REPORT (</b> Soubor [<b>/</b>Klíč] [<b>(</b>LogVýraz<b>)</b>] nebo <b>(</b>SeznamSouborů<b>)</b>
                          <b>,</b> NázevSestavy
                          [ <a href="prostredi.md#t560">Parametry sestavy</a> ]
                          [ <a href="prostredi.md#t562">Parametry automatické sestavy</a> ]  <b>)</b>
                <b>REPORT (</b> RecordProměnná<b>,</b> ....<b>)</b>
                <b>REPORT (</b> SeznamSouborů <b>,[</b> DynamickáDeklarace <b>],</b>...<b>)</b>

   Základní varianty:
   <b>report</b>( ,<b>NázevSestavy</b>,...) .............. sestava přesně podle kapitoly R
                                             v dalších parametrech (cond) nelze 
                                             použít údaje souboru.
   <b>report</b>(NázevSouboru,<b>NázevSestavy</b>) ....... sestava podle kapitoly R
   <b>report</b>(NázevSouboru,(<a href="#t538">Seznam Údajů</a>)) ..... automatická sestava
   <b>report</b>(NázevSouboru,(<b>?</b>)) ................ ruční výběr údajů ze souboru
   <b>report</b>(NázevSouboru,(<b>SeznamÚdajů ?</b>)) .... omezený výběr ze seznamu
   <b>report</b>((Soubor1/@,Soubor2/AlterKlíč),...) seznam vstupních souborů
   <b>report</b>( <b>RecordProměnná</b>, ... ) ........... místo seznamu souborů lze zadat                                              
                                             tisk <a href="procedury.md#t405">record</a> - proměnné
   <b>report(</b>NázevSouboru<b>,[</b>DynamickáDeklarace<b>])</b>... místo kapitoly R textový výraz
                                             obsahující deklaraci sestavy

   ■  <b>Soubor</b> ... Název kapitoly F, lze uvést i jiný než v deklaraci sestavy,
                 pokud obsahuje údaje, použité v deklaraci sestavy.
                 
   ■  <b>SeznamSouborů</b> ... Pokud vstupuje do reportu více vstupních souborů,
                 lze je předefinovat pomocí seznamu souborů. Pro každý
                 vstupní soubor lze uvést pořadí dle vlastního klíče a 
                 vstupní  filtr. Seznam je uzavřen v kulatých závorkách.
                 <b>(</b> Soubor[<b>/</b>Klíč][<b>(</b>LogVýraz<b>)</b>] { <b>,</b>Soubor[<b>/</b>Klíč][<b>(</b>LogVýraz<b>)</b>]} <b>)</b>

                 Libovolný soubor v seznamu může být nahrazen i record 
                 proměnnou odpovídajícího typu.

      Soubor nebo SeznamSouborů nemusí být uveden, pokud se report má provést
      přesně podle deklarace (kap.R nebo dynamické) bez přesměrování vstupů.
      Je však nutno uvést alespoň čárku, viz. první varianta.

   ■  <b>/Klíč</b> .... Věty vstupují do sestavy v pořadí podle (alternativního)
                 vlastního klíče (jinak fyzické pořadí)

   ■  <b>NázevSestavy</b> ... Lze použít název kapitoly R - tzv. <b>definovaná sestava</b>
                       nebo seznam údajů v závorkách - <b>automatická sestava</b>
                       Uvedením znaku <b>?</b> v seznamu pro autom. sestavu umožníme
                       uživateli ruční výběr údajů do sestavy.

   ■  <b>DynamickáDeklarace</b> ... Místo statické (ale odladěné !!!) kapitoly R lze
                             odpovídající deklaraci sestavy vložit dynamicky
                             do textového výrazu podle kontextu úlohy. 
                             <b>Pouze pro pokročilé !!!</b>. Viz. <a href="soubory.md#t554">Dynamické deklarace</a>

   Již z názvu vyplývá, že další parametry sestavy se dělí do dvou skupin.
   Za prvé to jsou obecně použitelné <b>parametry sestavy</b> a do druhé skupiny
   patří <b>parametry automatické sestavy</b> jako určitá náhrada za kapitolu R.

   další volitelné <a href="prostredi.md#t560">parametry sestavy</a>         <a href="prostredi.md#t562">parametry automatické sestavy</a>
   <b>cond</b> ........ výběr vět ze souboru        <b>mode</b> .... typ autoreportu
   <b>assign</b> ...... přesměrování výstupu        <b>sort</b> .... třídící údaje
   <b>times</b> ....... počet výtisků               <b>ctrl</b> .... řídící údaje
   <b>edit</b> ........ povolená editace sestavy    <b>sum</b> ..... součtované údaje
   <b>printctrl</b> ... dekódování řídících znaků   <b>width</b> ... šířka sestavy
   <b>check</b> ....... kontrola syntaxe            <b>style</b> ... druh písma
                                             <b>head</b> .... hlavička strany

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t365">sestava</a>            <a href="prostredi.md#t248">automatická sestava</a>         <a href="jazyk.md#t789">»P</a>         <a href="#t563">░ volání sestavy</a>
</pre>

<a name="t563"></a>
## ░ volání sestavy

<pre>
                          <b>PŘÍKLAD - VOLÁNÍ SESTAVY</b>
   ──────────────────────────────────────────────────────────────────────────

   F hlavicky.x  *   cislo : N,3 ;     nazev : A,10 ;
                     #K @ cislo ;
   F data.x      *   cislo : N,3 ;   polozka : N,2 ;     kc : F,5.2 ;
                     #K @ cislo, polozka ;
                        Obracene(@) cislo,&gt;polozka
                     #L kc&gt;=0 ;
   R sestava1    *   #I_hlavicky  cislo
                     #RH ;
                     Výpis obsahu:
                     #CF_cislo cislo, I1.Count ;
                                    ___   počet vět ____
   R sestava2    *   #I1_hlavicky    cislo
                     #I2_data        cislo
                     #CH_klic  cislo, nazev ;
                     Klíč : ___
                     Název: _
                     Seznam položek:
                     #DE2 polozka, kc ;
                                      __  ________

   ................. příklady volání sestavy v proceduře ....................

   VAR h : record of hlavicky ;
   BEGIN
   ....
   report( hlavicky, sestava1 );                  {bez speciálních požadavků}
   report( , sestava1 ) ;                         {jako předchozí,méně psaní}
   report( hlavicky, sestava1, edit );              {možnost editace sestavy}
   report( hlavicky, sestava1, 
           cond=(cislo&gt;'003') );                  {s podmínkou pro výběr vět}
   report( hlavicky, sestava1, 
           assign='LPT1');                         {výstup přímo na tiskárnu}
   report( hlavicky, sestava1,                               {tiskový soubor}
           assign='TISK.PRN', printctrl);        {- např. pro tisk na pozadí}

   report( data, sestava1 ) ;                     { změna vstupního souboru }

   readrec(h,1) ;
   report(h,(cislo,nazev)) ;              { tisk fyzicky první věty souboru }

   menuloop 'AUTOMATICKÉ SESTAVY' of
     'Opis vět':           report(DATA,(cislo,polozka,kc));
     'Součtovaná sestava': report(DATA,(cislo,kc),sum=(kc));
     'Totály':             report(DATA,(cislo,polozka,kc),ctrl=(cislo),
                                  sum=(kc),mode=onlysum);
     'Chybné věty':        report(DATA,(),mode=errcheck);
     'Ruční výběr':        report(DATA,(?));
   end;

   report((hlavicky/@,data/@),sestava2);  {sestava ze dvou vstupních souborů}

   report((hlavicky/@,data/Obracene),sestava2);   { změna třídění pro druhý }
                                                           { vstupní soubor }

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t566"></a>
## volání transformace

*Též:* `merge`

<pre>
                            <b>VOLÁNÍ TRANSFORMACE</b>
   ──────────────────────────────────────────────────────────────────────────

   Spuštění transformace lze provést ve dvou variantách. Základem je <b>první</b>
   <b>varianta</b>, která znamená statické spuštění předem připravené a hlavně řádně
   zkompilované kapitoly M.
   <b>Druhá varianta</b> přináší <b>zkušenějším</b> programátorům možnost dynamické definice
   kódu transformace v textovém výrazu. V tomto případě si programátor přebírá 
   veškerou odpovědnost za riziko běhových chyb při provedení.

   ██ Syntaxe:    varianta1        <b>MERGE (</b> NázevTransformace <b>)</b>
                  varianta2        <b>MERGE ( [</b> TextovýVýraz <b>]  )</b>

   ■  <b>NázevTransformace</b> ... Název kapitoly M.
   ■  <b>TextovýVýraz</b> ........ Obsahuje dynamickou deklaraci zdrojového kódu
                            transformace.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t389">transformace</a>          <a href="soubory.md#t554">dynamické deklarace</a>
</pre>

<a name="t786"></a>
## »E

<pre>
                       <b>SYNTAXE kapitoly E - FORMULÁŘ</b>
 ──────────────────────────────────────────────────────────────────────────────
                                                 <a href="#t361">formulář</a>
 PopisFormuláře ::=                              
      Komentář
      [ TextHlavičky ]
      #_ NázevSouboru PopisnáČástFormuláře ;
      ZobrazovacíČástFormuláře
 
 PopisnáČástFormuláře ::=
      [ Číslo : ] NázevÚdaje [{ , [ Číslo : ] NázevÚdaje }]
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t787"></a>
## »R

<pre>
                          <b>SYNTAXE kapitoly R - SESTAVA</b>                   PageDn
 ───── Ctrl-Home ──────────────────────────────────────────────────────────────
                                                 <a href="#t365">sestava</a>
 PopisSestavy ::=                       
      [ DeklaraceLokálníchProměnných ]
      [ .pagesize:= ČísVýraz ] [ ; ] [ .pagelimit:= ČísVýraz ]    <a href="#t368">stránkování</a>
      { Vstup }
      { Výstup }

 DeklaraceLokálníchProměnných ::=                <a href="#t391">DeklaraceLokálníchProměnných</a>
      var { NázevProměnné [{ , NázevProměnné }]
            : TypProměnné [ = Konstanta ] ; }

 Vstup ::=                                       <a href="#t369">VstupSestavy</a>
      #I [ Číslo ] _ NázevSouboru [ Setřídění ] [ ( PodmínkaVýběru ) ]
         [ ŘídícíÚdaje ] [ ; TřídícíÚdaje ]
 
 PodmínkaVýběru ::=
      LogVýraz | ?
 
 ŘídícíÚdaje ::=
      [ &gt; ] [ ~ ] NázevÚdaje [{ , [ &gt; ] [ ~ ] NázevÚdaje }]
 
 Setřídění ::=
      ! | /@ | / NázevKlíče
 
 TřídícíÚdaje ::=
      [ &gt; ] [ ~ ] NázevÚdaje [{ , [ &gt; ] [ ~ ] NázevÚdaje }]
 
 Výstup ::=                                      <a href="#t378">VýstupSestavy</a>
      # TypÚrovně [ ( LogVýraz ) ]
      [ VýpočetníČást, ]
      [ .page:= ČísVýraz ] [ , ] [ .line:= ČísVýraz | .line&lt;= ČísVýraz ]
      [ .notatend, ]
      [ .notsolo, ]
      [ PopisnáČást ]
      [ , VýpočetníČást ] ;
      ZobrazovacíČást                            <a href="prostredi.md#t384">ZobrazovacíČástÚrovně</a>
 
 TypÚrovně ::=
      RH | RF |
      PH | PF |
      CH [ Číslo ] _ NázevÚdaje | CF [ Číslo ] _ NázevÚdaje |
      DH | DE [ Číslo ]
 
 VýpočetníČást ::=                               <a href="prostredi.md#t379">PopisnáČástÚrovně</a>
      begin Přiřazení [{ ; Přiřazení }] end

 Přiřazení ::=
      LokálníNeboGlobálníProměnná := Výraz  |
      begin { Přiřazení ; } end |
      if LogVýraz then Přiřazení [ else Přiřazení ]

 PopisnáČást ::=
      Výraz [{ , Výraz }]

 ───── Ctrl-End ───────────────────────────────────────────────────────────────
                                                                         PageUp
</pre>

<a name="t788"></a>
## »M

<pre>
                       <b>SYNTAXE kapitoly M - TRANSFORMACE</b>                 PageDn
 ───── Ctrl-Home ──────────────────────────────────────────────────────────────
                                                 <a href="#t389">transformace</a>
 PopisTransformace ::=
      [ DeklaraceLokálníchProměnných ]
      { Vstup }
      { Výstup }                        
 
 DeklaraceLokálníchProměnných ::=                <a href="#t391">DeklaraceLokálníchProměnných</a>
      var { NázevProměnné [{ , NázevProměnné }]
            : TypProměnné [ = Konstanta ] ; }
 
 TypProměnné ::=                                 <a href="jazyk.md#t658">typy výrazů</a>
      real | string | boolean
 
 Vstup ::=                                       <a href="#t392">VstupTransformace</a>
      #I Číslo _ NázevSouboru [ Setřídění ]
        [ ( LogVýraz ) ] [ ŘídícíÚdaje ] [ ; TřídícíÚdaje ]
 
 ŘídícíÚdaje ::=                                 <a href="prostredi.md#t249">řídící a třídící údaje</a>
      [ &gt; ] [ ~ ] NázevÚdaje [{ , [ &gt; ] [ ~ ] NázevÚdaje }]
 
 Setřídění ::=
      ! | /@ | / NázevKlíče
 
 TřídícíÚdaje ::=
      [ &gt; ] [ ~ ] NázevÚdaje [{ , [ &gt; ] [ ~ ] NázevÚdaje }]
 
 Výstup ::=                                      <a href="#t395">VýstupTransformace</a>
      DetailníVýstup |
      SkupinovýVýstup  |
      NásobnýVýstup
 
 DetailníVýstup ::=
      #O Číslo _dummy [ ( LogVýraz ) ] { PříkazVýstupu ; } |
      #O Číslo _ NázevSouboru [ + ] [ ( LogVýraz ) ]
         { PříkazVýstupu ; }
 
 SkupinovýVýstup ::=
      #O_dummy [ ( LogVýraz ) ] { PříkazVýstupu ; } |
      #O_ NázevSouboru [ + ] [ ( LogVýraz ) ]
          { PříkazVýstupu ; }
 
 NásobnýVýstup ::=
      #O*_dummy [ ( LogVýraz ) ] { PříkazVýstupu ; } |
      #O*_ NázevSouboru [ + ] [ ( LogVýraz ) ]
           { PříkazVýstupu ; }
 
 PříkazVýstupu ::=                               <a href="#t396">PříkazováČástVýstupu</a>
      ObjektPřiřazení := Výraz  |
      begin { PříkazVýstupu ; } end |
      if LogVýraz then PříkazVýstupu [ else PříkazVýstupu ]

 ───── Ctrl-End ───────────────────────────────────────────────────────────────
                                                                         PageUp
</pre>
