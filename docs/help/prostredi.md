# Prostředí, katalog, instalace

<a name="t206"></a>
## nápověda

<pre>
                                 <b>NÁPOVĚDA</b>
  ──────────────────────────────────────────────────────────────────────────
   Právě si prohlížíte <b>nápovědu PC FANDu</b>. Kromě toho může programátor opatřit
   projekt vlastní  <b>nápovědou  pro  konkrétní úlohu</b>.  Nápověda  obsahuje řadu
   spojených  obrazovek,  ve  kterých  se  můžete  pohybovat  prostřednictvím
   zvýrazněných <b>hesel</b>. Nápověda se vyvolává z projektu klávesami:

   <b>Uživatelské prostředí:</b>
   ■ <b>F1</b> ....... v datovém a textovém editoru -&gt; <b>ovládání editoru</b> (help FANDu)
                v menu -&gt; <b>nápověda k volbě</b> (help FANDu nebo úlohy dle kontextu)
   ■ <b>Ctrl-F1</b> .. v datovém editoru (mód F1,F124) -&gt; nápověda k <b>aktuálnímu údaji</b>
                                                   (help úlohy)
   ■ <b>Alt-F10</b> .. aktivuje poslední volaný help (nezaměnit s F10).

   <b>Projektantské prostředí:</b>
   Další možnosti navíc oproti běžnému uživatelskému prostředí.
   ■ <b>Alt-F1</b> ... v projektantském prostředí uvnitř kapitoly -&gt; <b>syntaxe kapitoly</b>
   ■ <b>Ctrl-F1</b> .. uvnitř kapitoly, kurzor ukazuje na jméno -&gt; <b>syntaxe funkce</b>
   ■ <b>Alt-F2</b> ... z editace úlohy -&gt; vyvolání helpu úlohy
                z editace souboru -&gt; vyvolání odpovídající položky helpu úlohy
                z menu -&gt; vyvolání odpovídající položky helpu úlohy, pokud
                          není, založí

   Během prohlížení nápovědy lze použít klávesy:

   ■ <b>šipky</b> ............ pohyb po zvýrazněných heslech (tématech)
   ■ <b>Enter</b> ............ přechod k další obrazovce popisující zvýrazněné heslo
   ■ <b>F10</b> .............. návrat k předchozí obrazovce
   ■ <b>F1</b> ............... přechod k první obrazovce nápovědy (help index)
   ■ <b>F6</b> ............... tisk aktuálního textu helpu
   ■ <b>Esc</b> .............. ukončení nápovědy a návrat do programu
   ■ <b>PageUp</b>, <b>PageDn</b> ... listování v několikastránkových nápovědách
   ■ <b>CtrlHome</b>,<b>CtrlEnd</b> . lineární pohyb po helpu
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t322">konstrukce nápovědy</a>
</pre>

<a name="t216"></a>
## SET-parametry

*Též:* `fandcat`, `fanddata`, `fandcfg`, `fandres`, `lannode`, `fandovr`, `fandovrb`, `fandwork`

<pre>
                               <b>SET-parametry</b>
    ────────────────────────────────────────────────────────────────────────
    Tyto parametry interpretuje  PC FAND  při startu  programu a zadávají se
    do oblasti systémových proměnných (environment) příkazem
                       <b>SET PARAMETR=hodnota</b>.
    Všechny parametry jsou nepovinné (při jednouživatelském režimu) a nejsou
    -li zadány, platí implicitní nastavení.

    ■ <b>FANDRES</b> .... cesta k souboru FAND.RES (message,fonty,drivery)
    ■ <b>FANDCFG</b> .... cesta k souboru FAND.CFG (instalační parametry PC FANDu)
    ■ <b>FANDOVR</b> .... cesta k souboru FAND.OVR nebo UFAND*.OVR (overlay)
                   použití hlavně v LAN.
    ■ <b>FANDWORK</b> ... pracovní soubory FANDWORK.$$$, FANDWORK.X$$,FANDWORK.T$$
                   (realizace ClipBoardu, dočasné třídění, podmnožiny,...)
                   a PRINTER.TXT - tiskové sestavy. Význam hlavně v LAN.
    Implicitní hodnota všech výše uvedených parametrů je adresář PC FANDU.
   
    ■ <b>LANNODE</b> .... číslo stanice v síti ( unikátní pro každou stanici ! )
                   Hodnoty 0..255. <b>Povinný</b> při sdílení datových souborů !
    ■ <b>FANDDATA</b> ... jiná cesta pro datové soubory a katalog
    ■ <b>FANDCAT</b> .... adresář pro <a href="#t641">katalog</a> úlohy (má přednost před FANDDATA)
    ■ <b>FANDOVRB</b> ... zvětšení operační paměti pro úlohu, zadá se počet KB,
                   které má použít PC FAND pro oblast overlay místo optimální
                   velikosti (124 KB). Důsledkem je ovšem zpomalení práce
                   PC FANDu (zvláště volání exit-procedur).
                   Přípustné hodnoty parametru jsou v intervalu 80..134 KB,
                   jinak PC FAND použije 124 KB.
    
    ────────────────────────────────────────────────────────────────────────
    <a href="#t829">lokální sítě</a>         <a href="jazyk.md#t734">getenv</a>
</pre>

<a name="t219"></a>
## instalace

*Též:* `cache`, `instalace systému`

<pre>
                                <b> INSTALACE </b>
   ─────────────────────────────────────────────────────────────────────────
   Instalací rozumíme nastavení různých parametrů,  které mají vliv na práci
   PC FANDu a nastavují  se před jeho spuštěním.  V zásadě  lze rozlišit dvě
   skupiny : prostředky operačního systému (MS-DOSu) a vnitřní globální pro-
   měnné PC FANDu.

   <b>■</b>  Vnitřní globální proměnné, se inicializují při startu PC FANDu
      a jejich inicializační hodnoty jsou uloženy v souboru <b>FAND.CFG</b>.
      Pro jejich nastavení lze použít <a href="#t220">instalační program</a> <b>FANDINST.EXE</b>.

   <b>■</b>  Prostředky oper.systému a jeho konfigurace ovlivňují činnost programů
      obecně. Sem patří:

   <b>-</b>  <b>Parametr FILES=</b> systémového souboru <b>CONFIG.SYS</b>
      kterým je nutno zajistit dostatek místa pro najednou otevřené soubory
      konkrétní úlohy. Při nedostatečném nastavení tohoto parametru není úloha
      provozovatelná a havaruje s chybovým hlášením "<b>příliš mnoho otevřených</b>
      <b>souborů ...</b>." . Toto je v běžných podmínkách jediný podmiňující
      parametr pro běh úloh. Další parametry mají význam především pro
      optimalizaci (většinou urychlení) práce.

   <b>-</b>  Proměnné prostředí MS-DOSu, tzv. <a href="#t216">SET-parametry</a>

   <b>-</b>  <b>EMS paměť</b> lze použít pro umístění overlay modulu (FAND.OVR), což
      se děje automaticky, pokud je dostatek volné EMS paměti.  
      Dle praktických zkušeností to nemá význam pro urychlení práce. Výhodnější 
      je použít volnou paměť jako XMS pro cache či pro umístění rezidentních
      programů v horní paměti (viz. EMM386, loadhigh)

   <b>-</b>  <b>XMS paměť</b> (driver HIMEN.SYS v CONFIG.SYS) je možno s výhodou použít
      pro cache programy (SMARTDRIVE). Pozor - PC FAND obsahuje vlastní cache
      která standardně alokuje 200KB XMS-paměti (viz.<a href="#t223">instalace konstant</a>).
      <b>!!!</b> doporučujeme <b>ne</b>kombinovat FAND-cache s jinou (běžně SMARTDRIVE).

   ──────────────────────────────────────────────────────────────────────────────
   <a href="#t829">lokální sítě</a>     <a href="#t641">katalog</a>
</pre>

<a name="t220"></a>
## instalační program

<pre>
                        <b>INSTALAČNÍ PROGRAM FANDINST</b>
   ──────────────────────────────────────────────────────────────────────────
   Program <b>FANDINST.EXE</b> naplňuje soubor instalačních parametrů <b>FAND.CFG</b>,který
   se zohledňuje  během startu PC FANDu při naplňování globálních proměnných.
   Instalace je rozdělena do několika bloků:

   Program zpracuje soubor FAND.CFG dle nastavení SET parametru FANDCFG. 
   Pokud není, hledá se v aktuálním adresáři.

   ■ <b>Barvy</b> ....... barvy pro datový a textový editor, nápovědu, nabídky atd.
   ■ <b>Tiskárna</b> .... kódy pro přepínání druhů písma a ostatní escape sekvence
                   pro tiskárny, změna kódování diakritiky pro tiskárnu
                   lze instalovat až 10 tiskáren
   ■ <b>Konstanty</b> ... hodnoty všech typů ovlivňující chování PC FANDu
   ■ <b>Monitor</b> ..... adresa videopaměti, tvar kurzoru, ošetření sněžení
   ■ <b>Abeceda</b> ..... výběr národní abecedy pro lexikální třídění
                   připravené tabulky <b>Kamenických</b> a <b>Latin2</b>
   ■ <b>Dny</b> ......... tabulka výjimečných dnů v roce (svátky,...)
                   pro zabudovaný kalendář
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t221">instalace barev</a>           <a href="#t222">instalace tiskárny</a>            <a href="#t223">instalace konstant</a>
   <a href="#t225">instalace abecedy</a>         <a href="#t226">instalace kalendáře</a>           <a href="#t224">instalace monitoru</a>
   Ctrl-End
</pre>

<a name="t221"></a>
## instalace barev

<pre>
   Ctrl-Home                    <b>INSTALACE BAREV</b>
   ──────────────────────────────────────────────────────────────────────────
   Každá barva se zadává hexadecimálním číslem atributu podle nápovědy.

   ■ <b>uživatelská barva č.0-15</b>.. uživatelské barvy přístupné funkcí <a href="jazyk.md#t722">color</a>
   ■ <b>MENU normální text</b> ....... jednotlivé (neaktivní) volby nabídky
   ■ <b>     aktivní volba</b> ....... zvýrazněná volba, na které je kurzor
   ■ <b>     první písmeno</b> ....... zvýrazněné písmeno pro rychlou volbu
   ■ <b>     nefunkční volba</b> ..... zobrazená ale nefunkční volba

   ■ <b>VÝBĚR normální text</b> ...... výběr z prvků seznamu
   ■ <b>      aktivní položka</b> .... zvýrazněná položka, na které je kurzor
   ■ <b>      maska pro soubor</b> ... maska pro výběr souboru z disku

   ■ <b>ZADÁNÍ zadávaný text</b> ............ prompt na posledním řádku
   ■ <b>       nápověda</b> ................. vysvětlující text k promptu
   ■ <b>HLÁŠENÍ na posledním řádku</b> ...... F10 message
   ■ <b>POSLEDNÍ ŘÁDEK nápověda</b> ......... nápověda v datovém a textovém editoru
   ■ <b>               symboly kláves</b> ... označení funkčních kláves v nápovědě
   ■ <b>               přepínače</b> ........ vypnutí aditivních změn a kontrol
   ■ <b>PRVNÍ ŘÁDEK systém.informace</b> .... první řádek uživatelské obrazovky

   ■ <b>EDITOR TEXTU text</b> ............... editovaný text
   ■ <b>             ctrl-znaky</b> ......... řídící znaky (kromě přepínačů barev)
   ■ <b>             blok</b> ............... označení bloku
   ■ <b>DRUHY PÍSMA podtržený text</b> ...... Ctrl-S
   ■ <b>            kurzíva</b> ............. Ctrl-W
   ■ <b>            široký text</b> ......... Ctrl-Q
   ■ <b>            dvojitý text</b> ........ Ctrl-D
   ■ <b>            mastný text</b> ......... Ctrl-B
   ■ <b>            zhuštěný text</b> ....... Ctrl-E
   ■ <b>            písmo elite</b> ......... Ctrl-A

   ■ <b>EDITOR DAT data</b> ............ editovaná data
   ■ <b>           datový kurzor</b> ... zvýrazněný údaj, na kterém je kurzor
   ■ <b>           podmnožina</b> ...... vybraná podmnožina (F6-akce, F5-přepínače)
   ■ <b>           ostatní text</b> .... názvy údajů apod.
   ■ <b>           zrušené věty</b> .... věty, zrušené jinou stanicí (LAN)
   ■ <b>           výběr(F8,ShiftF8)</b> podmnožina dle pracovního indexu <a href="editory.md#t529">sel</a>

   ■ <b>UŽIVATELSKÁ OBRAZOVKA</b> ...... základní atribut pro write apod.

   ■ <b>NÁPOVĚDA text</b> .............. běžný text nápovědy
   ■ <b>         vybrané téma</b> ...... zvýrazněný název, na kterém je kurzor
   ■ <b>         ostatní témata</b> .... ostatní témata, na která lze nastavit kurzor
   ■ <b>         zvýrazněný text</b> ... nadpisy apod.
   ■ <b>KRÁTKÁ NÁPOVĚDA</b> ............ jednořádková nápověda na (před)posledním 
                                  řádku

   ■ <b>OKNA stíny</b> ............. stíny oken ( menu, with window, ww )
   ■ <b>DESKTOP</b> ................ barva "pracovního" stolu (hlavní menu PC FANDu)
   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End         <a href="editory.md#t242">barvy a typy písma</a>
</pre>

<a name="t222"></a>
## instalace tiskárny

<pre>
   Ctrl-Home                    <b>INSTALACE TISKÁRNY</b>
   ────────────────────────────────────────────────────────────────────────────
   Při generování tiskových sestav a jejich tisku na tiskárně odpovídají nároky
   PC FANDu na tiskárnu zhruba možnostem textového editoru PC FANDu. To znamená,
   že se používá neproporcionální písmo, diakritika v kódování Kamen nebo Latin 
   a základní typy písma. Tento přístup k tiskárně vychází z možností jehličko-
   vých tiskáren, především ohledně typů písem (zhuštěné, elite, široké, ...).

   <b>██  TYPY TISKÁREN :</b>
   <b>Jehličkové</b>  Jsou ovládány z programu v tzv. <b>módu EPSON</b>. V rámci tiskáren s
               tímto způsobem ovládání vládne široká kompatibilita a většinou
               lze použít řídící sekvence "STANDARD".

   <b>Inkoustové, laserové</b>  
               Novější typy tiskáren, které mají většinou odlišný způsob 
               ovládání, nejčastěji jazyk <b>PCL</b>, bývají také označovány 
               jako HP-kompatibilní. Tyto tiskárny mají obecně vyšší kvalitu 
               tisku než jehličkové, avšak z hlediska tisku z PC FANDu 
               je jejich kompatibilita - vzájemná nebo na jehličkové - často 
               problematická.
               Z výběru tiskáren v distribuci je nejobecněji použitelná
               sada řídících sekvencí pro HP DJ520 (jazyk PCL 3).

   <b>Bublinkové</b>  Podle konkrétního typu lze často použít buď standardní 
               sekvence jako pro jehličkové tiskárny (mód EPSON) nebo
               přepnout tiskárnu do IBM-modu (DIP přepínače, ovládací
               program) a použít přednastavené sekvence IBM PRO (IBM 
               proprinter).

   Pro řízení všech typů tiskáren se používají tzv. <b>escape sekvence</b>, které 
   se zadávají v hexadecimálním tvaru (např. 1B=Esc) podle manuálu tiskárny.

   Řízení tiskárny zahrnuje dva hlavní okruhy problémů:
   ■ <b>Podpora diakritiky</b>  Na rozdíl od klávesnice a obrazovky PC FAND neobsa-
                         huje podporu diakritiky pro tiskárnu. Proto musí
                         tiskárna umět češtinu sama (hardwarová podpora, lepší
                         varianta) nebo musí být použita systémová podpora
                         (rezidentní program). Důležité je to, že lze tisknout
                         na tiskárnu v jiném kódování diakritiky než probíhá
                         zpracování na počítači, případně může být tiskárna bez
                         podpory češtiny, viz. parametr <b>Kód</b>.
                         Pokud tiskárna obsahuje hardwarovou podporu češtiny,
                         je často třeba ji aktivovat, nejčastěji přidáním
                         speciální ESC-sekvence k <b>reset</b>u.
                         U tiskáren s jazykem PCL-3 se pro nastavení kódové
                         stránky 852 (Latin2) používá sekvence 1B 28 31 37 55
                         pro kód Kamenických sekvence 1B 28 31 38 55.

   ■ <b>Typy písma</b>          V drtivé většině případů lze odpovídající typy písma
                         nainstalovat pomocí ESC-sekvencí. Pro každý typ písma
                         se zadává kód pro zapnutí a kód pro vypnutí písma. 
                         Délka sekvence je max. 254 znaků.


   Popis jednotlivých řídicích sekvencí:

   Po vybrání tiskárny ze seznamu se objeví menu s možnostmi výběru
   způsobu tisku. Jsou 3:
     <b>Standardní (LPT1-3)</b>
     <b>Přes logický port (dos.LPT1-9)</b>
     <b>Náhradní tisk (manažer)</b>
   Rozlišuje se podle hodnoty <b>timeout</b>. Viz.další popis.

   ■ <b>TISKÁRNA</b> (název)
     Použije se pro generování menu pro výběr tiskárny v PC FANDU (klávesa 
     <b>Alt-F6</b>) - lze instalovat až 10 tiskáren. Viz. <a href="procedury.md#t588">SetPrinter</a>, <a href="procedury.md#t588">CPrinter</a>.
     Standardní instalace od distributora obsahuje tyto typy tiskáren:
     Název dle FANDINST  určeno pro tiskárny         kódování češtiny
     ─────────────────────────────────────────────────────────────────
       EPS-bez           EPSON a kompatibilní       bez kódování češtiny
       EPS-kam           EPSON a kompatibilní           Kamenických
       EPS-lat           EPSON a kompatibilní             Latin 2
       DJ-PCL3           inkoustové (s jazykem PCL3)      Latin 2
       LJ-PCL5           laserové (s jazykem PCL5)        Latin 2
       IBM-bez           bublinkové                 bez kódování češtiny
       IBM-lat           bublinkové                       Latin 2
       Výjimky a známé nastavení pro další konkrétní typy tiskáren jsou
       dostupné na WEBu firmy alis <b>www.alis.cz</b>

   ■ <b>Typ</b> 
     Typ tiskárny (<b>M</b>-mozaiková,<b>C</b>-mozaiková-barva,<b>L</b>-laserová HP LaserjetII)
     Uplatní se při tisku grafu, při tisku textu jen pro interpretaci ESC
     sekvencí s proměnnou částí, viz. <b>délka stránky</b> a <b>odsazení zleva</b>.

   ■ <b>kód(KL,KN,LK,LN)</b>
     Tento parametr umožňuje překódování výstupu na tiskárnu a tak pro tisk
     použít jiné kódování diakritiky než pro vlastní zpracování. Častá
     kombinace je pro PC (data) kódování Kamen a pro tisk Latin2. 
     Kód je dvouznaková konstanta:
                                   KL     kamen -&gt; latin
                                   KN     kamen -&gt; bez diakritiky
                                   LK     latin -&gt; kamen
                                   LN     latin -&gt; bez diakritiky
                             nezadáno     bez překódování
     Viz. také <a href="#t609">CopyFile</a>.
     
   ■ <b>Číslo portu tiskárny (1 až 9)</b>
     Označení paralelního portu pro výstup : LPT<b>1</b> ... LPT<b>9</b>. Implicitně 1.
     Lze tisknout i na fyzicky neexistující porty, na které jsou mapovány
     síťové tiskárny.
     Porty <b>4-9</b> lze použít jen při tisku na "logický" port, viz další
     parametr.

   ■ <b>Čekání na odezvu tiskárny</b> 
     Tzv. timeout. Zadává se v sekundách, význam nastavení hodnot:
     <b>1 až 14</b> ... Při zahájení i v průběhu tisku sleduje PC FAND odezvy
                 tiskárny a pokud doba přesáhne stanovený limit, vydá
                 hlášku "F10! (ESC)  připojte tiskárnu!
                 Zadaná konstanta se zapisuje do oblasti BIOSu, 
     <b>0</b> ......... Neprovede se žádný zápis a použije se implicitní hodnota 
                 BIOSu na vašem PC.
                 Pokud se u širokých tiskáren formátu A3, např. STAR LC-15
                 objeví (neoprávněně) výše uvedená hláška v průběhu tisku
                 použijte hodnotu 14 nebo 0.
                 Hodnotu 0 použijte i při tisku pod <b>Windows 95</b>.
     <b>253</b> ....... Speciální tisk. Podpora tisku přes tiskový manažer 
                 (Win95/98/NT), viz. <b>www.alis.cz</b>.
                 Provedou se dvě akce:
                 <b>1.</b> Kopie souboru PRINTER.TXT do souboru se jménem podle
                    parametru <b>Název kopie</b>(uloženo v sekvenci "RESET").
                    Jméno souboru může obsahovat znak <b>#</b>, který bude nahrazen
                    pořadovým číslem 1..99.
                 <b>2.</b> Spustí se externí program (<a href="procedury.md#t626">EXEC</a>) podle parametru
                    <b>Název programu</b> (uloženo v sekvenci "Délka strany").
                    Jako parametry programu se připojí obsah dalšího
                    parametru instalace tiskárny <b>Parametry programu</b>
                    (uloženy v sekvenci "Uk.řetězec/laser").

     <b>254</b> ....... Tisk přes tzv. "logický" port. To znamená že tisk se
                 z PC FANDu posílá do souboru s názvem <b>LPTx</b> (kde x=1..9)
                 jehož přesměrování na tiskárnu je již v režii oper. systému
                 MS DOS. Porty 4-9 lze použít jen pod WIN95/98/NT.
     <b>255</b> ....... Má význam pouze u síťových tiskáren v těch typech sítí,
                 kde se na vlastní tisk musí čekat delší dobu, případně
                 až do ukončení aplikace (např. ve WFW nelze parametrizovat,
                 v Novellu 3.xy program CAPTURE - parametr TI).
                 PC FAND provede po ukončení tisku formální otevření 
                 a uzavření handle pro odpovídající tiskový port.
     Implicitně je nastaveno 7 sec.

   ■ <b>(^S) Podtržení</b> ........ ^S Underline
   ■ <b>(^W) Kurzíva</b> .......... ^W Italic
   ■ <b>(^Q) Široký</b> ........... ^Q Double-width
   ■ <b>(^D) Dvojitý</b> .......... ^D Double-strike
   ■ <b>(^B) Mastný</b> ........... ^B Emphasised
   ■ <b>(^E) Zhuštěný</b> ......... ^E Compressed
   ■ <b>(^A) Elite</b> ............ ^A Elite
     Pro jednotlivé typy písma se instaluje řídicí sekvence pro zapnutí 
     (lichý výskyt) a pro vypnutí (sudý výskyt přepínače). Tyto sekvence
     jsou při tisku textu substituovány na místo výskytu odpovídajících
     přepínačů. Na jehličkových tiskárnách lze jednotlivé typy kombinovat,
     což však může činit obtíže na některých typech tiskáren s jazykem 
     PCL (HP-kompatibilní).


   ■ <b>Reset tiskárny</b>
     Tato řídicí sekvence se do tiskárny vyšle vždy na začátku každého tisku
     (po potvrzení F6, CtrlF6,...). V tomto smyslu jde o "startovací" řídicí
     sekvenci pro tisk. Nejčastěji obsahuje vlastní ESC sekvenci pro reset
     daného typu tiskárny, která nastavuje tiskárnu do výchozího stavu.
     Někdy (u některých typů podpory diakritiky) dojde při tom ke "shození"
     této podpory. V takovém případě je nutno sekvenci pro reset zrušit
     (vyprázdnit).
     U tiskáren s HP-módem (laserové,inkoustové HP-DJxyz) je naopak často
     potřeba sekvenci pro reset rozšířit o nastavení znakové sady, nejlépe
     neproporcionálního fontu s diakritikou. Dále zde lze nastavit i velikost
     písma, řádkování apod. To je nutné proto, že i tiskárna, která "umí"
     češtinu nemusí mít tuto vlastnost aktivní po "čistém" resetu.

   ■ <b>Délka strany</b>
     Pokud je tato sekvence v instalaci zadána, PC FAND ji vždy vyšle na
     tiskárnu při zahájení tisku, ihned po sekvenci pro reset. Rozumí se
     fyzická délka strany a zadává se v počtu řádků na stranu. Prioritně 
     se počet řádků vyhodnotí podle tečkového příkazu .pl. Pokud není zadán,
     použije se hodnota z instalace konstant - sečte se logická délka a
     ukončení strany. Implicitní hodnota je 72.
     Zpracování sekvence je různé podle nastavení typu tiskárny. Pro typ
     <b>M</b> a <b>C</b> se k instalované sekvenci délky strany připojí hodnota délky
     v binárním kódování (1 byte, např. znak s hodnotou 72, tj. 'H') a
     vyšle na tiskárnu. 
     U typu <b>L</b> se údaj délky kóduje znakově, to jest např. znaky '7' a '2'.
     Ovšem na konec se připojí ukončovací sekvence, viz. následující parametr
     instalace.

   ■ <b>Uk.řetězec/LASER</b>
     Ukončující řídící sekvence pro délku strany, používá se pro laserové
     tiskárny (<b>Typ=L</b>). Viz. komentář k parametru délka strany.

   ■ <b>Odsazení zleva</b>
   ■ <b>Uk.odsaz.zleva</b>
     Používá se pro realizaci tečkového příkazu <b>.PO</b>. Zadává se počet
     sloupců zleva. Způsob zpracování proměnné části počtu sloupců je
     stejný jako pro délku strany včetně ukončující sekvence.
     (<a href="editory.md#t240">tečkové příkazy</a>)

   ■ <b>(^X) Uživatelský kód 1</b>
   ■ <b>(^V) Uživatelský kód 2</b>
   ■ <b>(^T) Uživatelský kód 3</b>
     Další typy písma nebo využití speciálních vlastností konkrétní 
     tiskárny (řádkování, barvy, NLQ, ...). Pro tiskárnu typu STANDARD
     jsou tyto typy nastaveny takto: ^X ... dvojnásobné písmo
                                     ^V ... čtyřnásobné písmo
                                     ^T ... husté řádkování (1/8")


   ■<b> PARAMETRY PRO GRAFICKÝ TISK</b>

     <b>Řádkování n/72</b>, ....... dle možností tiskárny ──┐
     <b>Řádkování n/216</b>                                 │   lze ovlivnit
     <b>Gr.hustota 60</b>, ........ grafická hustota        ├   proporce
     <b>Gr.hustota 120</b>,                                 │   tisku
     <b>Gr.hustota 240</b>                                ──┘   <a href="rozhrani.md#t782">tisk grafu</a>
     Tyto parametry se nastavují jen u typu tiskárny 'M' a 'C'. Pro typ 'L'
     nemají význam.

     <b>Nastavení barev</b>
     Pouze prefix, PC FAND doplní číslo barvy. Použije se u typu tiskárny C.


   ■ <b>Závěr.sekvence</b> 
     Závěrečná sekvence pro ukončení tisku. Má význam jen při speciálních
     případech tisku.
   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End          <a href="editory.md#t242">barvy a typy písma</a>          <a href="editory.md#t240">tisk textu</a>             <a href="rozhrani.md#t753">grafy</a>
</pre>

<a name="t223"></a>
## instalace konstant

<pre>
   Ctrl-Home                  <b>INSTALACE KONSTANT</b>
 ──────────────────────────────────────────────────────────────────────────────

 ■ <b>Počet vět k uložení v editoru</b>
   Po pořízení či změně daného počtu vět v datovém editoru dojde k uložení vět
   a uvedení souboru do definovaného stavu. Tj. provedou se celkem tři akce:
       aktualizuje se hlavička souboru
       data se z cache uloží na disk
       aktualizují se údaje v adresáři disku (fyzická délka souboru).
   V případě havárie, např. přerušení přívodu el.proudu, případná automatická
   oprava PC FANDem vede k návratu k tomuto stavu. Implicitní hodnota je 20 
   vět. Nižší hodnota znamená vyšší bezpečnost ale možné zdržování při práci.
   Toto nastavení platí globálně pro všechny spuštěné datové editory, indivi-
   duálně lze interval uložení nastavit parametrem <a href="#t519">saveafter</a> příkazu <b>edit</b>.

 ■ <b>Automatická sestava - šířka</b>
   Základní parametr automatické sestavy, který udává počet znaků na řádce
   při generování automatické sestavy. Implicitně 80 znaků. Šířka sestavy 
   se odvozuje podle normální velikosti písma (10 CPI). Jde vlastně o impli-
   citní hodnotu parametru <a href="#t292">width</a> příkazu <b>report</b>.

 ■ <b>Automatická sestava - délka</b>
 ■ <b>                    - ukončení strany</b>
   Délka udává počet řádků sestavy, které se mají generovat na jednu tiskovou
   stranu. Jde tedy o logickou délku strany. Přičtením počtu řádků pro
   ukončení strany se získá údaj o fyzické délce strany. Uplatní se jak při
   automatické tak i při definované sestavě. V definované sestavě lze fyzickou
   i logickou délku strany upravit pomocí parametrů sestavy <b>.</b><a href="formular-sestava.md#t368">pagesize</a> a
   <b>.</b><a href="formular-sestava.md#t368">pagelimit</a>.
   Implicitní hodnoty jsou 69 a 3. Celkem 72 řádků = implicitní fyzická délka
   stránky. Vždy na začátku tisku vyšle PC FAND do tiskárny ESC-sekvenci pro
   nastavení fyzické délky stránky (pokud je tato sekvence uvedena viz.
   <a href="#t222">instalace tiskárny</a> a fyzická délka strany je jiná než 72). Fyzickou délku 
   strany lze definovat také tečkovým příkazem <b>.pl</b>.
   Ukončení strany lze definovat také tečkovým příkazem <b>.cp</b>.
   Při tisku na jednotlivé listy (podavač) doporučujeme nastavit hodnoty
   těchto parametrů na 64 (nebo kolik řádků se skutečně vejde na list) a
   ukončení strany doplníme na součet 72, tedy 8.
   Důležitý je součet 72, při jiné hodnotě se mohou některé tiskárny chovat
   "podivně", navíc to až na speciální typy sestav (složenky) nemá význam.


 ■ <b>Sestava - výstup rovnou na tiskárnu ?</b>
   Při generování automatické sestavy pomocí volby datového editoru F6 - opis
   sestava vystupuje rovnou na tiskárnu, nikoliv nejdříve do tiskového souboru
   PRINTER.TXT. Možné hodnoty A/N, implicitně N. Viz. parametr<a href="#t560">  assign</a> příkazu 
   <b>report</b>.

 ■ <b>Výběr tiskárny před tiskem ?</b>    A/N (N)
   Po vyvolání tisku (F6,CtrlF6,CtrlK-P,příkaz printtxt) se místo hlášky
             F10! (ESC)  nastavte tiskárnu!
   objeví seznam instalovaných tiskáren (jako při AltF6) s nastavenou aktivní
   tiskárnou a možností změny.

 ■ <b>Graficky oddělit stránky ?</b>
 ■ <b>-        znak pro oddělení</b>
   Při zapnuté klávese <b>ScrolLock</b> se v textovém editoru oddělovače stránek
   (CtrlP-CtrlL) zobrazí jako řada teček (znaků pro oddělení, implicitně
   znaku s kódem 250) a tak se "graficky" oddělí stránky. Není zohledněno
   nastavení logické délky strany. Implicitně je grafické oddělení zapnuto.

 ■ <b>Potvrdit ESC z editoru ?</b>   A/N (N)
   Při opuštění datového editoru aktivuje kontrolní dotaz pro uživatele
      opustit editor A/N ?
   Neuplatní se při přerušení pořízení nové věty - viz. následující parametr.
   Individuálně lze toto nastavit parametrem <b>mode='??'</b> příkazu <b>edit</b>, 
   viz. <a href="#t530">módy datového editoru</a>.

 ■ <b>Potvrdit ESC z nedokončené věty?</b> A/N (N)
   Parametrizace přerušení pořízení nové věty v datovém editoru. Při nastavení 
   na <b>A</b> musí obsluha přerušení pořízení potvrdit na hlášku:
          Nedokončená věta bude zrušena! Přesto opustit A/N?
   V opačném případě je "rozeditovaná" věta bez varování nenávratně ztracena.
   Individuálně lze toto nastavit parametrem <b>mode='?N'</b> příkazu <b>edit</b>, 
   viz. <a href="#t530">módy datového editoru</a>.

 ■ <b>Potvrzení hlášky ENTER místo F10</b>  A/N (N)       
   Pro potvrzení hlášek PC FANDu (včetně příkazu <a href="#t457">message</a>) lze kromě klávesy
   F10 použít i <b>ENTER</b>. Možné hodnoty A/N, implicitně N.

 ■ <b>Komentář k RDB při ladění ?</b>  A/N (A)
   V programátorském prostředí se zobrazí první řádek textu.

 ■ <b>CP/M šachta (např. D,E,F...)</b>
   Pro účely převodu dat z 8-bitové verze PC FANDu (oper.systém CP/M) je možno
   speciálním driverem instalovat možnost čtení disket ve formátu CP/M.
   V tomto parametru se uvede, kam bylo čtení CP/M disket driverem mapováno.

 ■ <b>Sítě LAN - obnovení obrazovky</b>
   Při editaci sdíleného datového souboru dochází k automatické pravidelné 
   obnově obrazovky - načtením dat z disku. Interval obnovy se zadává v se-
   kundách, implicitně 5 sec. Nižší hodnota parametru znamená lepší přehled 
   obsluhy o akcích ostatních stanic avšak vyšší zátěž serveru.
   Individuálně lze zadat interval parametrem <a href="#t519">refresh</a> příkazu <b>edit</b>.
   <b>lokální sítě</b> - <a href="#t843">používané pojmy</a>.

 ■ <b>Sítě LAN - opak. blokovaného přístupu</b>  (<a href="#t843">net delay</a>)
   V lokální síti mohou být soubory, požadované pro určitou akci, dočasně
   nepřístupné z důvodu práce ostatních stanic. Proto je třeba odmítnutý
   pokus o práci s nimi po určitém čase automaticky znovu opakovat. Tento
   časový interval se zadává v jednotkách CPU (z důvodu jemnějšího odlišení
   jednotlivých stanic), implicitně 54 CPU. ( 1 sec ≈ 18 CPU )

 ■ <b>Sítě LAN - pípání při opakování blokování</b>  A/N (A)
   Viz. předchozí parametr. Nepřístupnost souboru je signalizována vizuální
   hláškou a pípnutím, které je možno vypnout, ruší-li obsluhu.

■ <b>První přístup - prodleva (CPU)</b>     0 .. 255 (0)
  <b>              - počet opakování</b>    0 .. 255
  Je-li parametr "počet opakování" &gt; 0, potom PC FAND při odmítnutí prvního
  pokusu o systémový zámek čeká dobu podle prodlevy v CPU a zkusí to znovu
  aniž by hlásil uživateli blokovací hlášku. Čekací doba mezi pokusy
  a počet pokusů bez hlášení jsou dány těmito parametry.
  Jde o analogii parametrů Lock Delay a Lock Retries ze souboru NET.CFG pro
  konfiguraci stanice v síti Novell. Vhodné nastavení těchto parametrů může
  zlepšit chování aplikace při větší zátěži - urychlit a méně hlášek.
  Konkrétní hodnoty je třeba ověřit v praxi, používají se spíše nižší.

 ■ <b>Pípání při chybách ?</b>   A/N (A)
   Chybová a některá další hlášení doprovází zvukový signál, který lze
   zde vypnout / zapnout. Potlačí i efekt příkazu <a href="procedury.md#t487">beep</a>, ovšem nepotlačí 
   příkaz <a href="procedury.md#t487">sound</a>.

 ■ <b>Max.velikost XMS pro PC FAND</b>
   PC FAND obsahuje vlastní diskovou cache. Implicitně alokuje 200 KB paměti
   XMS. Nedoporučuje se kombinovat s jiným typem cache (např. SMARTDRIVE),
   jsou možné kolize a dvojí "kešování" nemá smysl.

 ■ <b>Potlačit Ctrl-Break ?</b>  A/N (N)
   Možnost ukončení programu kombinací kláves Ctrl-Break.

 ■ <b>Číslo implicitní klávesnice</b> ..... 0 až 4 (0)    
   PC FAND obsahuje podporu národního prostředí pro klávesnici. Celkem
   5 typů klávesnic v určitém pořadí - viz. klávesa AltF8 v PC FANDu. 
   Tento parametr určuje číslo aktivní klávesnice po startu PC FANDU. 
   Možné hodnoty 0 - 4, implicitně 0.

 ■ <b>PARAMETRIZACE MYŠI</b>
   <b>Myš - podpora vypnuta</b>   A/N (N)
   Možnost vypnutí podpory myši.

   <b>- výměna kláves</b>   A/N (N)
   Výměna levé a pravé klávesy.

   <b>- interval pro dvojitý stisk</b>
   Zadává se v CPU, implicitně 8.

   <b>- interval pro opakování</b>
   Automat.opakování stisku myši. Zadává se v CPU, implicitně 8.

 ■ <b>Prodleva Ctrl,Alt,Shift</b> 
   Interval pro vyvolání alternativ. nápovědy po stisku přeřazovací klávesy.
   Zadává se v CPU, implicitně 15.

 ■ <b>Automat.přepsat návěští diskety</b> . A/N (A)       
   Při zálohování na disketu (příkazy <a href="procedury.md#t613">backup</a>, <a href="procedury.md#t620">backupm</a>) lze při nesouhlasu
   návěští diskety toto návěští po dotazu obsluze přepsat. Je-li tento parametr
   nastaven na <b>N</b>, PC FAND striktně vyžaduje disketu s návěštím podle katalogu.

 ■ <b>Interval zhasnutí obrazovky</b>
   Aktivuje šetřič obrazovky, tj. délku intervalu v sekundách po kterém při
   nepoužití libovolné klávesy zhasne obrazovka. Je-li hodnota 0 (implicitně),
   obrazovka nezhasíná.
   Maximální povolená hodnota je 3640 sec (asi 1 hodina).

 ■ <b>Posun implicitního století</b> (roky, 0)
   Parametrizuje dosazení století při zadávání datumu s dvojmístným číslem pro 
   rok, tj. například při standardní masce 'DD.MM.YY'. Důležité hlavně na
   přelomu století. Po zadání datumu se provede test nerovnosti
          <b>YY &lt; (AR + konstanta) MOD 100</b>
   kde:         AR ... aktuální rok
                YY ... zadané dvojčíslí roku
         konstanta ... zadaná hodnota parametru posunu století
   Pokud je splněna, přičte se k zadanému roku 2000, výsledný rok bude 20YY.
   V opačném případě 19YY. Pokud je konstanta=0 (implicitně), použije se
   vždy aktuální století (tj. neprovádí se vyhodnocení nerovnosti).
   Pozor na chybné nastavení konstanty, např. by nemělo být AR+konstanta=99.
   Tato parametrizace se projeví na všech místech v PC FANDu, kde lze použít
   nějakou formu krátkého tvaru datumu '...YY', přičemž pro další zpracování
   se vždy převádí na plný tvar '...YYYY'.
   <a href="#t691">strdate</a>   <a href="#t691">valdate</a>   <a href="jazyk.md#t659">konstanty</a>   <a href="editory.md#t327">uložené údaje</a>  <a href="#t609">copyfile</a>

 ■ <b>Kontrola volného místa na disku</b>  A / N (A)
   Při práci na diskových polích o velikosti větší než 2 GB nastavíme na '<b>N</b>'
   a tak se můžeme vyhnout neoprávněným hlášením o nedostatku  volného místa 
   na disku.

──────────────────────────────────────────────────────────────────────────────
 Ctrl-End    <a href="formular-sestava.md#t556">report</a>       <a href="#t248">automatická sestava</a>         <a href="#t641">katalog</a>        <a href="rozhrani.md#t58">klávesnice</a>
</pre>

<a name="t224"></a>
## instalace monitoru

<pre>
   Ctrl-Home                   <b>INSTALACE MONITORU</b>
   ──────────────────────────────────────────────────────────────────────────
   1.blok - systémové konstanty,  které  je třeba  měnit pouze ve výjimečných
            případech na nestandardních konfiguracích PC. Implicitní hodnoty
            se liší podle typu monitoru.

   ■ <b>Adresa videopaměti</b> ........ automaticky nastavena pro Hercules/VGA apod.
   ■ <b>Počet řádků monitoru</b> ...... nutno nastavit v některých případech, kdy
                                 systém vrací chybnou hodnotu. Pokud = 0,
                                 (implic.) použije hodnotu vrácenou systémem.
   ■ <b>Ošetřit sněžení ?</b> ......... odstranění "sněžení" na levnějších monitorech
                                 zpomaluje odezvu při zápisu na obrazovku
   ■ <b>Zapnutý kurzor</b> ............ tvary kurzoru (horní a dolní řádek kurzoru)
   ■ <b>Vypnutý kurzor</b>              viz. dokumentace BIOSu, např. TECHHELP
   ■ <b>Velký kurzor</b>

   2.blok - podpora češtiny na monitoru
   ■ <b>Použít fonty FANDu ?</b> ...... na EGA/VGA automaticky použije font s dia-
                                 kritikou podle instalace abecedy, implic.<b>A</b>
   ■ <b>Monitor bez diakritiky ?</b>... pokud je nastaveno <b>A</b>, bude z hlášení pro 
                                 komunikaci s uživatelem odstraněna před výpi-
                                 sem na monitor diakritika, implicitně <b>N</b>.
   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End          <a href="#t225">instalace abecedy</a>
</pre>

<a name="t225"></a>
## instalace abecedy

<pre>
   Ctrl-Home                   <b>INSTALACE ABECEDY</b>
   ──────────────────────────────────────────────────────────────────────────
   Pro podporu národního prostředí PC FAND obsahuje driver klávesnice a fonty 
   pro obrazovku. Neřeší pouze  fonty pro tiskárnu. Při  instalaci  se vybírá
   kódová tabulka pro <b>lexikální třídění</b> z těchto možností:
 
   ■ <b>IBM</b> ........... počítač bez instalované národní abecedy
   ■ <b>Kamenických</b>
   ■ <b>Latin2</b> ........ dvě používané varianty kódování češtiny a slovenštiny
                     (převod mezi oběma kódy procedurou <a href="#t609">copyfile</a>(...mode...)
 
   Výstupy na tiskárnu lze překódovat - viz. <a href="#t222">instalace tiskárny</a>

   Podle instalované abecedy jsou v PC FANDu podporované funkce:
 
   ■ <a href="jazyk.md#t710">upcase</a> .............. převod malých písmen na velká
   ■ <a href="jazyk.md#t710">lowcase</a> ............. převod velkých písmen na malá
   ■ <a href="jazyk.md#t710">nodiakr</a> ............. odstranění diakritiky z textu
   ■ <a href="jazyk.md#t668">lexikální třídění</a> ... třídění zohlední diakritiku
   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End         <a href="#t224">instalace monitoru</a>
</pre>

<a name="t226"></a>
## instalace kalendáře

<pre>
   Ctrl-Home               <b>INSTALACE KALENDÁŘE</b>
   ──────────────────────────────────────────────────────────────────────────
   Tabulka kalendářních výjimek (svátky,...) se používá v datumové aritmetice
   při počítání s pracovními dny (funkce <b>typeday</b>, <b>addwdays</b>, <b>difwdays</b> ). Pokud
   není tabulkou určeno jinak, počítá se funkce <b>typeday</b> podle dne v týdnu.

   Instalace pobíhá ve dvou krocích : 
   Před spuštěním instalačního programu volejte pod PC FANDem úlohu <b>INST.RDB</b>, 
   kde editujete kalendářní výjimky (datový  soubor <b>DNY.000</b>). Pořizujete vždy 
   dva údaje, datum a nový typ dne (0 až 3 podle požadovaného nového výsledku  
   funkce typeday). 
   V druhém kroku zvolte v instalačním programu <b>FANDINST</b> volbu <b>Dny</b>.
   ──────────────────────────────────────────────────────────────────────────
   <a href="jazyk.md#t692">funkce pro datum a čas</a>
</pre>

<a name="t248"></a>
## automatická sestava

<pre>
                            <b>AUTOMATICKÁ SESTAVA</b>
   ──────────────────────────────────────────────────────────────────────────
   Umožňuje vytisknout obsah datového souboru  nebo jeho části s automatickým
   rozvrhem strany. Vyvolává se z datového editoru (<b>F6-akce</b>, <b>Opis</b>) nebo přímo
   v projektu procedurou <b>report</b>. Dovoluje výběr podmnožiny,  dočasné třídění,
   součtování a dělení souboru do skupin.
   ┌───────────────┐
   │ <b>O</b>pis vět      │ opis datového souboru nebo vybrané podmnožiny
   │ <b>S</b>oučt.sestava │ opis doplněný součty vybraných údajů
   │ <b>T</b>otály        │ součtovaná sestava bez jednotlivých vět (jen součty)
   │ <b>C</b>hybné věty   │ opis chybných vět souboru (logické kontroly)
   └───────────────┘
   Postup komunikace v autoreportu: před generováním sestavy zadáváte údaje:

   ■ <b>součtované údaje</b>... číselné údaje, na konci sestavy budou sečteny
   ■ <b>řídící údaje</b> ...... rozdělují soubor do skupin se stejnými řídícími údaji
                         součty jsou potom prováděny i po skupinách
   ■ <b>třídící údaje</b> ..... věty budou vystupovat v pořadí podle třídících údajů
                         třídění je dočasné a nemá vliv na soubor
   ■ <b>podmnožina</b> ........ když zadáte logický výraz, do sestavy vystoupí pouze
                         věty splňující tuto podmínku
   ──────────────────────────────────────────────────────────────────────────
   <a href="editory.md#t247">akce</a>     <a href="formular-sestava.md#t556">report</a>     <a href="#t249">řídící a třídící údaje</a>
</pre>

<a name="t249"></a>
## řídící a třídící údaje

<pre>
                           <b>ŘÍDÍCÍ A TŘÍDÍCÍ ÚDAJE</b>
   ──────────────────────────────────────────────────────────────────────────
   V <b>sestavách</b>  ( automatické  sestavy  volané  z  datového editoru i sestavy
   deklarované v kapitole R ) a v  <b>transformacích</b>  můžete  definovat <b>řídící</b> a
   <b>třídící údaje</b>.

   ■ <b>Řídící údaje</b>: rozdělují soubor do skupin po sobě jdoucích vět se stejnými
     hodnotami řídících  údajů ( a to i v několika úrovních ).  Na konci každé
     skupiny lze provést požadovanou akci (např. součtování). V transformaci i  
     sestavě  je třeba, aby  soubory vstupovaly  setříděné  dle řídících údajů
     ( může být  zajištěno automatickým  dočasným tříděním  nebo  čtením podle
     <b>indexu</b> ). Pokud  vstupuje do sestavy jen jeden  soubor, nemusí být tříděn
     dle řídících údajů (za výsledek je ovšem odpovědný programátor).

   ■ <b>Třídící  údaje</b>:  uvnitř  skupin  vět  dle  řídících  údajů lze věty ještě
     setřídit podle třídících údajů.
 
   Oba typy  údajů  mohou  být  definovány  najednou,  ale  mají smysl i každý
   samostatně. Třídění podle řídících nebo třídících údajů (znak <b>!</b> v deklaraci)
   je dočasné a nemá vliv na soubor.  Při definici řídících  a třídících údajů
   lze využít sestupné (<b>&gt;</b>) a lexikální (<b>~</b>) třídění.
   ───────────────────────────────────────────────────────────────────────────
   <a href="#t248">automatická sestava</a>     <a href="formular-sestava.md#t556">report</a>     <a href="formular-sestava.md#t369">VstupSestavy</a>     <a href="formular-sestava.md#t392">VstupTransformace</a>
   <a href="jazyk.md#t668">lexikální třídění</a>
</pre>

<a name="t253"></a>
## projektantské prostředí

<pre>
             <b>PROJEKTANTSKÉ PROSTŘEDÍ</b>             HOT - klávesy
   ─────────────────────────────────────────────────────────────────────────
   Program v PC FANDu je <a href="#t320">projektový soubor</a> - tj. speciální  případ datového
   souboru. Programátor při ladění používá <a href="editory.md#t244">datový editor</a> obohacený o některé
   další funkce:

   ■ <b>Ctrl-F8</b> .... <b>diagnostika</b> a syntaktická kontrola projektu
   ■ <b>Ctrl-F9</b> .... <b>ladění</b> (kontrola a spuštění) jedné kapitoly projektu.
                  Úloha jako celek musí být syntakticky v pořádku.
   ■ <b>Alt-F9</b> ..... start procedury MAIN z libovolného místa v úloze
   ■ <b>Ctrl-F10</b> ... <b>uzavření</b> (návratné zaheslování) úlohy
                  Na kontrolní dotaz "chcete úlohu zaheslovat (A/N)?" jsou 
                  možné 3 odpovědi: <b>A</b> ......... návratné zaheslení
                                    <b>N</b> ......... zrušení
                                    <b>Alt-F10</b> ... nenávratné zaheslení
                  <b>Nenávratné zaheslení úlohy</b>
                  Před tím však PC FAND nabídne zálohování zdrojových textů.
                  Z nenávratně zaheslené úlohy jsou vypuštěny poznámky,
                  nevýznamné oddělovače (mezery,...) a neplatné úseky dle 
                  direktiv kompilátoru. Dále jsou provedeny některé přesuny, 
                  takže ani případným dekódováním nelze získat původní
                  kompletní zdroje.
   ■ <b>Shift-F1</b> ... navigace po projektu - přechod k první kapitole s názvem,
                  který je pod kurzorem návrat klávesou <b>F10</b> - až 10 vnoření.
                  Pozor na to, že znak podtržítko může být nyní součástí
                  identifikátoru. Proto v kapitolách E,M,R píšeme místo 
                  <b>#I_soubor</b>  lépe <b>#I_ soubor</b> a lze navigovat Shift-F1.

   ■ <b>Alt-F3</b> ..... vyvolání editace katalogu úlohy

   <b>Podpora pro vytvoření helpu úlohy</b>
   ■ <b>Alt-F2</b> ..... přechod do aktuálního helpu úlohy dle kontextu:
                  z editace úlohy (RDB) - editace helpu
                  z editace dat.souboru - přechod na položku helpu, která
                                          odpovídá danému údaji, popř. ji
                                          vytvoří (viz. <a href="#t283">mode</a>='F1'nebo'F124')
                  z uživatelského menu  - přechod na položku helpu, která
                                          odpovídá dané volbě menu,
                                          pokud dosud není - vytvoří ji
   ■ <b>F3</b> ......... při editaci helpu, hledání názvu

   <b>Ladění</b>
   spustí kapitolu, na které je kurzor, což znamená otevření datového
   editoru pro soubor (kapitola <b>F</b>  a  <b>E</b>),  vygenerování  sestavy  (<b>R</b>),
   provedení transformace (<b>M</b> - pozor - pracuje s daty) nebo spuštění
   procedury (<b>P</b>). Při překladu se mohou některé úseky zdrojového textu
   vynechat podle nastavení direktiv kompilátoru - viz. <a href="#t262">podmíněný překlad</a>

   Při nalezení první  syntaktické chyby  se kurzor  nastaví na místo chyby
   a je vydáno chybové hlášení, které je třeba potvrdit libovolnou klávesou.
   U syntakt. odladěných programů <b>Diagnostika</b> vypíše velikost obsazené
   a volné paměti.

   Další vlastnosti překladu:
   Při Ctrl-F10 se provede překlad všech kapitol úlohy, při ostatních typech
   překladu se přeloží jen kapitoly, které byly změněny od posledního 
   překladu (viz. <a href="#t320">Projektový soubor</a> - údaj "overit").

   Některé změny v úloze jsou natolik závažné, že se po nich provede vždy
   kompletní překlad celé úlohy (jako při Ctrl-F10) a navíc se uloží 
   kontrolní datum a čas posledního překladu. Jsou to tyto změny:
   ■ změna v kapitolách typu F,D,E nebo I
   ■ přehozeni pořadí kapitol

   Příkazy datového editoru důležité v projektantském prostředí:
   ■ <b>Ctrl-&lt;-</b> resp. <b>Ctrl--&gt;</b> ... výměna sousedních kapitol při špatném pořadí
   ■ <b>Ctrl-F7</b> a <b>Shift-F7</b> .......... přenos bloků mezi kapitolami (<b>Clipboard</b>)
   ■ <b>F6-Opis-Opis Vět</b> ............... <b>listing</b> (opis zdrojového textu úlohy)
   ────────────────────────────────────────────────────────────────────────
   <a href="#t256">struktura projektu</a>
</pre>

<a name="t256"></a>
## struktura projektu

*Též:* `komentář`, `main`

<pre>
                             <b>STRUKTURA PROJEKTU</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Projektový soubor</b> je soubor s předdeklarovanou strukturou věty. Každá věta
   obsahuje jednu  kapitolu projektu. Popis struktury viz. <a href="#t320">projektový soubor</a>.
   Hlavní program ( start úlohy ) je v proceduře <b>MAIN</b>. Při programování v
   projektantském editoru jsou viditelné údaje:

   ■ <b>Typ</b> ..... typ kapitoly (<b>F</b>ile, <b>E</b>dit, <b>R</b>eport, <b>M</b>erge, <b>P</b>roc, <b>U</b>ser, <b>I</b>nclude,
               <b>D</b>eklaration(funkce)).
               Jedno písmeno, <b>mezera</b>=komentář a věta se při překladu ignoruje.
   ■ <b>Nazev</b> ... jednoznačný název kapitoly pro její volání z projektu.
               Max.12 písmen, název musí být identifikátor (bez mezer apod.).
               Jednoznačnost názvu se kontroluje v rámci každého typu kapitoly,
               nebo-li dvě kapitoly s odlišným typem mohou mít stejný název.
               Ovšem, zachování jednoznačnosti v celé úloze je vhodné nejen pro
               přehlednost ale i pro mechanismus navigace <b>ShiftF1</b> po úloze.
   ■ <b>Text</b> .... volný text s vlastní deklarací kapitoly
               Editace volného textu: <b>Ins</b> dovnitř, <b>Esc</b> zpět do datového
               editoru. Komentáře uzavíráme do složených závorek <b>{</b> komentář <b>}</b>. 
               Komentáře mohou být i vnořené. <b>{ {</b> komentář1 <b>} {</b> aaa <b>} neco; }</b>

   <b>Typy kapitol:</b>
   ■ <a href="jazyk.md#t310">D</a> ... deklarace uživatelských funkcí
   ■ <a href="soubory.md#t313">F</a> ... deklarace souboru, uložené a vypočítané údaje, klíče, aditivní změny,
           kontroly, implicitní přiřazení, uživatelské pohledy, závislosti;
           vzhledem k deklarativnímu programování je kapitola F těžištěm projektu
   ■ <a href="formular-sestava.md#t361">E</a> ... editační formulář pro jeden soubor, rozvrh obrazovky
   ■ <a href="formular-sestava.md#t365">R</a> ... tisková sestava, stránkování, součtování, výběr vět, typy písma atd.
   ■ <a href="formular-sestava.md#t389">M</a> ... sekvenční transformace až devíti vstupních souborů, výpočty
   ■ <a href="procedury.md#t411">P</a> ... procedura pro řazení kroků projektu a dialog s uživatelem
   ■ <a href="#t270">U</a> ... seznam oprávněných uživatelů a jejich hesel
   ■ <a href="#t269">I</a> ... vkládané kapitoly (Include)
   ■ <a href="dalsi-kapitoly.md#t887">L</a> ... logický programovací jazyk typu PROLOG
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t253">projektantské prostředí</a>           <a href="#t262">podmíněný překlad</a>
</pre>

<a name="t262"></a>
## podmíněný překlad

*Též:* `endif`, `ifdef`, `ifndef`, `define`, `direktivy`

<pre>
                 <b>PODMÍNĚNÝ PŘEKLAD - direktivy kompilátoru</b>
   ─────────────────────────────────────────────────────────────────────────
   Často je nutno  udržovat několik  verzí programu,  např. pro několik typů
   uživatelů.  Tyto situace lze  řešit použitím direktiv kompilátoru, pomocí
   kterých lze při překladu vynechat  části  zdrojového textu  dle zvoleného
   kontextu.

   <b>{$define}</b>   |  <b>{$define X ... X}</b>              X je velké písmeno A ... Z
   <b>{$ifdef X}</b>  |  <b>{$ifndef X}</b>
   <b>{$else}</b>
   <b>{$endif}</b>

   Příkaz <b>define</b>  zneplatní  dosavadní definice a určí  množinu definovaných
   znaků. Maximálně lze definovat 20 znaků.Takto definované znaky lze použít
   v podmínce k označení bloku  zdrojového textu, který je vázán na definici
   daného znaku (<b>ifdef</b>), nebo naopak na to, že znak definován není (<b>ifndef</b>).

   Direktiva <b>else</b> uvozuje blok zdrojového textu, který se přeloží, pokud byl
   ignorován text za předchozí direktivou <b>If</b>xxx.
   Direktiva <b>endif</b> podmíněný blok zdrojového textu ukončí.
   Lze s výhodou použít analogii s příkazem procedury <b>if</b> - ale nezaměňovat !

   ■ Direktivy se mohou vyskytovat všude, kde jsou povoleny komentáře.
   ■ Direktivy mohou být vnořené jedna do druhé.
   ■ Při nenávratném zaheslení se vypustí větve programu, které nejsou podle
     nastavených přepínačů platné.
   ■ Časové zdržení překladu podmíněným textem je stejné jako komentářem 
     této délky.
   ■ V kapitole F lze použít podmíněnou kompilaci za předpokladu, že je v 
     první (uložené údaje,#K,#C,#A) a druhé části (#U,#D,#L,#I) použita
     samostatně. Druhá část kapitoly F nesmí začínat direktivou.
     Direktivy nelze použít uvnitř pohledu (#U).
   ■ Speciální direktiva <b>{$include NázevKapitolyI}</b> vloží do textu kapitoly
     text kapitoly typu I. (<a href="dalsi-kapitoly.md#t267">vkládané texty</a>)

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██  Program manipuluje s velkými částkami a chceme rozlišit standardní
       verzi a verzi pro koprocesor, která zpracuje větší čísla (ale při
       emulaci pomaleji). Podmíníme přímo strukturu datového souboru.

       F   Soubor       ČísloÚčtu : N,6 ;
                     {$ifndef K}
                         CelkemKc : F,8.2 ;           {standardní verze}
                     {$else}
                         CelkemKc : F,12.2 ;          {pro koprocesor}
                     {$endif}

   ─────────────────────────────────────────────────────────────────────────
</pre>

<a name="t269"></a>
## I

<pre>
                        <b>I</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>#I</b> ..... <a href="soubory.md#t358">implicitní hodnoty</a>: odstavec deklarace datového souboru (kap.F)
   ■ <b>#I_</b> .... <a href="formular-sestava.md#t369">VstupSestavy</a>: vstupní soubor sestavy (kap.R) - může být i <b>#I1_</b>
   ■ <b>#I1_</b> ... <a href="formular-sestava.md#t392">VstupTransformace</a>: vstupní soubor transformace (kap.M)
   ■ <b>I</b> ...... <a href="dalsi-kapitoly.md#t267">kapitola I</a> : kapitola typu I - vkládané texty

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t270"></a>
## U

<pre>
                        <b>U</b> - duplicitní klíčové slovo
   ───────────────────────────────────────────────────────────────────────────

   ■ <b>#U</b> ..... <a href="soubory.md#t350">uživatelské pohledy</a>: odstavec deklarace datového souboru (kap.F)
   ■ <b>U</b> ...... <a href="dalsi-kapitoly.md#t644">seznam uživatelů</a>: kapitola U - definice přístupových práv

   ───────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t273"></a>
## with

*Též:* `do`

<pre>
                   <b>WITH</b>, <b>DO</b> - duplicitní klíčová slova
   ──────────────────────────────────────────────────────────────────────────

   ■ <a href="procedury.md#t427">while</a> <b>do</b> ......... while cyklus podle podmínky
   ■ <a href="procedury.md#t441">forall</a> <b>do</b> ........ forall cyklus přes věty souboru nebo podmnožinu
   ■ <b>with</b> <a href="procedury.md#t474">window</a> <b>do</b> ... definice okna pro výstup na obrazovku
   ■ <b>with</b> <a href="procedury.md#t751">graphics</a> <b>do</b> . přepnutí do grafického módu
   ■ <b>with</b> <a href="#t867">shared</a> <b>do</b> ... určení režimu blokování (LAN)
   ■ <b>with</b> <a href="#t871">locked</a> <b>do</b> ... uzamčení věty (LAN)

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t282"></a>
## edit

<pre>
                      <b>EDIT</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>edit(...)</b> ........  <a href="procedury.md#t510">volání editoru</a>: editace datového souboru
   ■ report(...<b>edit</b>) ... <a href="#t560">parametry sestavy</a>: prohlížení sestavy s možností 
                                            editace

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t283"></a>
## mode

<pre>
                      <b>MODE</b> - duplicitní klíčové slovo
   ───────────────────────────────────────────────────────────────────────────

   ■ edit(...<b>mode</b>) ........ <a href="#t530">módy datového editoru</a>: omezení při editaci
   ■ report(...<b>mode</b>) ...... <a href="#t562">parametry automatické sestavy</a>: typ autoreportu
   ■ <a href="jazyk.md#t737">selectstr</a>(...<b>mode</b>) ... parametrizace výběru z pole stringů
   ■ <a href="#t609">copyfile</a>(...,<b>mode</b>) ... převod diakritiky při kopírování textových souborů

   ───────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t284"></a>
## head

<pre>
                      <b>HEAD</b> - duplicitní klíčové slovo
   ───────────────────────────────────────────────────────────────────────────

   ■ edit(...<b>head</b>) ....... <a href="#t519">parametry editace</a>: první řádek obrazovky datového
                                              editoru
   ■ <a href="editory.md#t595">edittxt</a>(...<b>head</b>) .... první řádek obrazovky textového editoru
   ■ graph(...<b>head</b>) ...... hlavička grafu, <a href="rozhrani.md#t758"> head</a>
   ■ report(...<b>head</b>) ..... <a href="#t562">parametry automatické sestavy</a>: hlavička strany
   ■ <a href="jazyk.md#t737">selectstr</a>(...<b>head</b>)... text pro horní hranu okna při výběru z pole stringů
   ■ <a href="#t609">copyfile</a>(...<b>head</b>) ... hlavička textového souboru při exportu dat do textu

   ───────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t290"></a>
## count

*Též:* `group`

<pre>
                 <b>GROUP</b>, <b>COUNT</b> - duplicitní klíčová slova
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>group</b> ............ pořadí aktuální zpracovávané skupiny vět
   ■ <b>Ii.count</b> ......... pořadí věty vstupního souboru v aktuální skupině vět
                        (rozdělení vstupů sestavy nebo transformace do skupin 
                        podle řídících údajů)

   <a href="jazyk.md#t383">speciální funkce v sestavě</a>                 <a href="soubory.md#t400">speciální funkce v transformaci</a>
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t292"></a>
## width

<pre>
                      <b>WIDTH</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ report(..<b>width</b>..)... <a href="#t562">parametry automatické sestavy</a>: počet znaků na řádek
   ■ graph(...<b>width</b>..)... parametr příkazu <b>graph</b>: šířka objektů v procentech
                           viz. <a href="rozhrani.md#t773"> width</a>
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t294"></a>
## append

<pre>
                     <b>APPEND</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <a href="procedury.md#t721">PutTxt</a>(...,<b>append</b> ...) .... připojení textu k souboru
   ■ <a href="#t609">CopyFile</a>(...,<b>append</b>...) ... připojení jednoho souboru k jinému.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t295"></a>
## assign

<pre>
                     <b>ASSIGN</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>Report</b>(...,<b>assign</b>=TextVýraz ...) ... přesměrování tiskového výstupu
                                          <a href="#t560">parametry sestavy</a>
   ■ <a href="rozhrani.md#t753">Graph</a>(...,<b>assign</b>=TextVýraz...) ..... přesměrování souboru .PCX pro zápis

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t296"></a>
## var

<pre>
                       <b>VAR</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>VAR</b> i : real ; .................. <a href="formular-sestava.md#t391">lokální proměnné</a> - deklarace
   ■ copyfile(Soub1,Soub2/<b>VAR</b>,...) ... <a href="#t609">copyfile</a>, převod souborů

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t297"></a>
## txt

<pre>
                       <b>TXT</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <a href="rozhrani.md#t753">graph</a>(...,<b>TXT</b>=...,).................. příkaz graph - <a href="rozhrani.md#t777">okno pro graf</a>
   ■ <a href="#t609">copyfile</a>(Soub1,Soub2/<b>TXT</b>,...) ....... převod textových souborů

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t298"></a>
## txtxy

<pre>
                      <b>TXTXY</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■<a href="editory.md#t591"> txtxy</a> ........................ pozice kurzoru po ukončení editace textu
   ■ <a href="editory.md#t595">edittxt</a>(...,<b>TxtXY</b>=,...) ...... nastavení pozice kurzoru v editačním okně

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t304"></a>
## del

<pre>
                      <b>DEL</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <a href="#t867">with shared</a> Soubor(<b>Del</b>) ... mód blokování souboru, 
                                 viz. <a href="#t858">módy blokování vyšší</a>

   ■<a href="dalsi-kapitoly.md#t930"> del</a>_ ..... FAND - PROLOG, implicitní predikát pro rušení prvku v seznamu

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t306">duplicitní klíčová slova</a>
</pre>

<a name="t306"></a>
## duplicitní klíčová slova

<pre>
                          <b>DUPLICITNÍ KLÍČOVÁ SLOVA</b>
  ──────────────────────────────────────────────────────────────────────────

     <a href="#t295">assign</a>               <a href="procedury.md#t275">end</a>             <a href="#t283">mode</a>               <a href="jazyk.md#t299">txtpos</a>
     <a href="#t294">append</a>               <a href="procedury.md#t278">else</a>            <a href="procedury.md#t300">nocancel</a>           <a href="#t298">txtxy</a>
     <a href="jazyk.md#t268">archives</a>             <a href="procedury.md#t288">exit</a>            <a href="jazyk.md#t291">nrecs</a>              <a href="#t270">U</a>
     <a href="procedury.md#t275">begin</a>                <a href="#t290">group</a>           <a href="jazyk.md#t285">recno</a>              <a href="#t296">var</a>
     <a href="dalsi-kapitoly.md#t305">call</a>                 <a href="#t284">head</a>            <a href="procedury.md#t303">save</a>               <a href="#t292">width</a>
     <a href="jazyk.md#t279">cond</a>                 <a href="#t269">I</a>               <a href="procedury.md#t286">sort</a>               <a href="#t273">with</a>
     <a href="rozhrani.md#t287">ctrl</a>                 <a href="procedury.md#t278">if</a>              <a href="jazyk.md#t271">sum</a>                <a href="procedury.md#t302">write</a>
     <a href="#t304">del</a>                  <a href="jazyk.md#t280">in</a>              <a href="procedury.md#t278">then</a>               <a href="procedury.md#t302">writeln</a>
     <a href="#t273">do</a>                   <a href="procedury.md#t293">line</a>            <a href="#t297">txt</a>                <a href="soubory.md#t281">ww</a>
     <a href="#t282">edit</a>

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t320"></a>
## projektový soubor

<pre>
                             <b>PROJEKTOVÝ SOUBOR</b>
   ──────────────────────────────────────────────────────────────────────────
   V PC FANDu lze využít  fakt, že projektový soubor ( <b>program</b> ) je zvláštním
   datovým souborem.  Celý projekt je možné zpracovávat z jiné pomocné úlohy,
   jako by to byl standardní datový soubor (tvorba dokumentace, údržba velkých  
   projektů, instalace úlohy, automatické generování programu).

   Deklarace projektového souboru: ve stejném adresáři založíte pomocnou úlohu 
   a deklarujete kapitolu typu <b>F</b> s názvem projektového souboru (včetně přípony
   <b>.RDB</b>). Text deklarace je třeba nechat prázdný, deklarace se převezme
   z interních tabulek. Takto lze zpracovávat pouze <b>otevřené</b> úlohy.

   <b>Údaje projektového souboru:</b>

   ■ <b>TxtPos</b>:F,4.0... pozice v textu při poslední editaci kapitoly
   ■ <b>Overit</b>:B ...... příznak, zda již prošla kapitola syntaktickou kontrolou
   ■ <b>StText</b>:T ...... používá se pouze u kapitol typu F, obsahuje přeloženou
                     aktuální deklaraci souboru po změnách (umožňuje 
                     automatickou transformaci podle nové deklarace)
   ■ <b>Typ</b>   :A,'!'... typ kapitoly
   ■ <b>Nazev</b> :A,12 ... název kapitoly
   ■ <b>Text</b>  :T ...... vlastní deklarace ve volném textu
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t256">struktura projektu</a>     <a href="#t253">projektantské prostředí</a>
</pre>

<a name="t322"></a>
## konstrukce nápovědy

*Též:* `soubor pro nápovědu`

<pre>
                            <b>KONSTRUKCE  NÁPOVĚDY</b>
   ─────────────────────────────────────────────────────────────────────────
   Každá úloha  může mít jeden  <b>help-soubor</b> ( deklarovaný v kapitole F ) pro
   nápovědu k projektu.  Věta help-souboru obsahuje  <b>Heslo</b>  pro identifikaci
   konkrétní nápovědy a volný  <b>Text</b> obsahující vlastní nápovědu  ( obrazovku
   textu ).  Soubor pro  nápovědu má v  <b>NázvuSouboru</b> příponu <b>.HLP</b> a obsahuje
   povinně dva údaje:

   ■ <b>Heslo</b>:A,n ... deklarace .HLP souboru
   ■ <b>Text</b>:T        délka hesla i názvy identifikátorů jsou libovolné

   Pomocí <b>Hesla</b> lze obrazovku <b>Text</b> vyvolat na různých místech projektu:

   ■ <a href="procedury.md#t451">menu</a>, <a href="procedury.md#t451">menuloop</a>, <a href="procedury.md#t451">menubar</a>: nápověda k jednotlivým volbám nabídky
   ■ <a href="soubory.md#t354">logické kontroly</a>: bližší vysvětlení chyby pro uživatele v datovém editoru
   ■ <a href="#t457">message</a>: bližší specifikace chybové zprávy
   ■ <a href="#t282">edit</a>: editace s módem '<b>F1</b>' nebo '<b>F124</b>' - nápověda k jednotlivým údajům
         souboru pro každý údaj existuje nápověda s heslem 
                           <b>NázevSouboru.NázevÚdaje</b>
         Hesla mohou obsahovat masku (se znaky <b>*</b>, <b>?</b>) - společná nápověda.
         Při pohybu po souboru se na posledním řádku ( na 24.řádku při F124)
         zobrazí první řádek z textu helpu. Klávesa <b>Ctrl-F1</b> vyvolá nápovědu 
         na celou obrazovku. Pokud první řádek textu uzavřeme do <b>{</b>...<b>}</b>,
         tak se zobrazí jen jako krátká nápověda, ale vynechá se při
         zobrazení celé obrazovky.
   ■ <a href="#t457">help</a>: příkaz pro vyvolání nápovědy kdekoliv v proceduře.
   ■ <a href="#t457">display</a> : pouze zobrazí zvolený text, ale bez mechanismu navigace.

   <b>Nápověda v podúlohách</b>
   V podúloze může být definována vlastní nápověda. Při použití stejného 
   hesla se vybere heslo z podúlohy. Pokud není nalezeno heslo v nápovědě
   úlohy, hledá se v helpu nadřízené úlohy.

   <b>Navigace po nápovědě</b>
   Jednotlivé obrazovky (věty help-souboru) mohou být propojeny pomocí hesel.
   Heslo uvnitř  textu je ohraničeno mezi  znaky <b>^S</b> , zvýrazněné  části textu
   mezi <b>^B</b>.  Potom lze  nápovědu  procházet podle  obvyklého  schématu. Hesla
   mohou obsahovat masky (<b>*</b>, <b>?</b>).  Úvodní obrazovka nápovědy ( help index ) má
   heslo <b>root</b> - naviguje se na něj dvojím stiskem F1.

   <b>Heslo s prázdným textem</b>: jako odpovídající obrazovka (<b>Text</b>) se vezme první
   neprázdný text za tímto heslem.Tak může mít několik hesel stejnou nápovědu.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t206">nápověda</a>
</pre>

<a name="t330"></a>
## výjimky v typech údajů

*Též:* `maska pro datum`, `maska pro text`

<pre>
                           <b>VÝJIMKY V TYPECH ÚDAJŮ</b>
   ──────────────────────────────────────────────────────────────────────────
   Pro speciální případy jsou určeny následující typy údajů:

   █ <b>F,</b>m<b>,</b>n   ... (čárka místo tečky) číslo s <b>myšlenou desetinnou čárkou</b>,
                 číslo bude interně uloženo jako <b>celé</b>, což odstraňuje potíže
                 se zaokrouhlováním desetinné části v binární soustavě

   █ <b>A,</b>n<b>R</b>    ... znakový řetězec bude při pořízení <b>zarovnaný doprava</b>
   █ <b>A,</b>...<b>!</b>  ... Znak <b>!</b> na konci deklarace znamená, že údaj bude na disku
                 uložen v zakódované formě.
   █ <b>A,'</b>Maska<b>'</b>.. Maska pro alfanumerický řetězec,text. konstanta v apostrofech.
                 Délka údaje odpovídá největší možné délce podle masky a údaj 
                 je vždy uložen <b>se zarovnáním doleva</b>. Význam znaku masky:
                 <b>#</b> číslice
                 <b>9</b> číslice
                 <b>@</b> písmeno
                 <b>?</b> libovolný znak
                 <b>$</b> písmeno s automatickou změnou na velké (upcase)
                 <b>!</b> libovolný znak s automatickou změnou na velké (upcase)
                 <b>[</b>....<b>]</b>  volitelná skupina znaků
                 <b>(</b>...<b>|</b>...<b>|</b>...<b>)</b> alternativní skupiny znaků příp. různé délky
                 Ostatní znaky musí přesně souhlasit se vstupem (zadávají se 
                 a negenerují). Není povoleno vnoření závorek. 
                 Kontrola (a upcase) je až po Enter na údaji.

   █ <b>N,</b>n<b>L</b>    ... číselný řetězec bude při pořízení <b>zarovnaný doleva</b>

   █ <b>D,'</b>Maska<b>'</b>.. datum se speciální maskou
                 <b>Maska</b> pro datum je libovolný řetězec v apostrofech obsahující
                 kromě oddělovačů také speciální znaky pro jednotky <b>datumu</b> a
                 <b>času</b>. Tyto znaky budou ve výsledku nahrazeny skutečnými
                 hodnotami. Maska určuje způsob zobrazení datumu.
                 ■ <b>Y</b>-rok, <b>M</b>-měsíc, <b>D</b>-den, 
                 ■ <b>h</b>-hodina, <b>m</b>-minuta, <b>s</b>-vteřina, <b>t</b>-setina vteřiny
                 <a href="#t223">instalace konstant</a>

   █ <b>T,</b>n     ... volný text, při editaci se zobrazí také <b>n</b> znaků prvního řádku
   █ <b>T,</b>...<b>!</b>  ... Znak <b>!</b> na konci deklarace znamená, že údaj bude na disku
                 uložen v zakódované formě.

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

           Cena: F,9,2;                                {myšlená řádová čárka}
          Nazev: A,20R;                                   {zarovnané doprava}
     RodneCislo: A,'######/###(#| )';                  {maska s alternativou}
      EvidCislo: N,6L;            {zarovnané doleva a doplněné nulami zprava}
         Obdobi: D,'YMM';                             {různé varianty datumu}
            Cas: D,'mm:ss.tt';
    UplnyRozsah: D,'DD.MM.YYYY hh:mm:ss';
   PopisVyrobku: T,40;                     {volný text s viditelným začátkem}

   ──────────────────────────────────────────────────────────────────────────
   <a href="soubory.md#t785">»F</a>
</pre>

<a name="t379"></a>
## PopisnáČástÚrovně

<pre>
                            <b>POPISNÁ ČÁST ÚROVNĚ</b>
 ──────────────────────────────────────────────────────────────────────────────
 Na rozdíl od formuláře (E) v sestavě (R) vystupují nejen  údaje  souboru,  ale
 obecně <b>výrazy</b>. <b>Popisná část</b> obsahuje <b>seznam výrazů</b> vyhodnocovaných v  kontextu
 jedné věty vstupního souboru (uložené a vypočítané údaje, operátory a  funkce,
 lokální a globální  proměnné,  údaje  z  nadřízených  souborů  pomocí  tečkové
 notace). Při více vstupech platí pro  stejnojmenné  údaje  obdobná  implicitní
 pravidla jako v transformaci, silnější je použití prefixu <b>Ii.NázevÚdaje</b>.

 █ <b>SeznamVýrazů</b> ... popisná část výstupní úrovně reportu bez přiřazení

 █ <b>begin SeznamPřiřazovacíchPříkazů end</b> SeznamVýrazů ... s přiřazením do
 █ SeznamVýrazů <b>begin SeznamPřiřazovacíchPříkazů end</b>          proměnných
 Před  seznamem  a/nebo  po  seznamu  vystupujících  výrazů  může  být  uvedena
 výpočetní  část  -  seznam  přiřazovacích  příkazů  v  programových  závorkách
 <b>begin</b>...<b>end</b> (uvnitř případně s větvením <b>if</b>-<b>then</b>-<b>else</b>). Objektem přiřazení jsou
 <b>lokální</b> a <b>globální proměnné</b>. Před a/nebo po  výstupu  se  provedou  definovaná
 přiřazení.

 Popisná část může být i prázdná, potom vystoupí na tomto místě beze změny text
 definovaný v zobrazovací části (zobrazovací část je  bez  masek).  Pokud  celá
 úroveň obsahuje jen přiřazení, provedou se jen výpočty bez výstupu.
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t384">ZobrazovacíČástÚrovně</a>     <a href="jazyk.md#t383">speciální funkce v sestavě</a>     <a href="jazyk.md#t654">výrazy</a>     <a href="formular-sestava.md#t386">░ sestava</a>
</pre>

<a name="t384"></a>
## ZobrazovacíČástÚrovně

<pre>
                          <b>ZOBRAZOVACÍ ČÁST ÚROVNĚ</b>
 ──────────────────────────────────────────────────────────────────────────────
 <b>Zobrazovací část</b> určuje, v jaké podobě budou vystupovat výrazy z popisné části
 úrovně. Obsahuje text, který se opíše do výsledné sestavy. V textu jsou  <b>masky</b>
 (řetězce  speciálních  znaků)  na  místech,  kde  budou  vystupovat  konkrétní
 hodnoty. Masky mají odpovídající délku podle předpokládaného výsledku:

   typ výrazu     maska
 ■ <b>real</b> (F,D)     <b>___</b>    číslo (zaokrouhlené na celé)       
                  <b>___.__</b> číslo s desetinnou částí       
                  <b>@@@.@@</b> číslo s potlačením výstupu nulových hodnot
                  <b>___,__</b> výstup čísel s myšlenou desetinnou čárkou
                  <b>__.__.__</b> nebo <b>__.__.____</b> datum
                  <b>____:__:__.__</b> čas
 ■ <b>string</b> (A,N,T) <b>___</b>    text přesně podle výrazu v popisné části
                  <b>@@@</b>    text formátovaný do sloupce podle šíře masky
                  <b>P</b>      (Ctrl-P,Ctrl-P) do výstupu se předá binární hodnota
                         řetězce, má význam při tisku specialit, výsledkem
                         nemusí být textový soubor ve formátu PC FANDu (PRINTER
                         .TXT), proto je třeba použít v příkazu <b>report</b> parametry
                         <b>printctrl</b>, resp. <b>assign='lpt1'</b>.
 ■ <b>boolean</b> (B)    <b>_</b>      vystoupí <b>A</b> nebo <b>N</b> (Ano/Ne)
 
 Zobrazovací část musí odpovídat popisné  části  (stejně  výrazů  jako  masek),
 nebo může být násobkem popisné části (předloha pro několik vět).  Po  skončení
 úrovně se odřádkuje, pokud další úroveň nezačíná od vyššího sloupce, než končí
 předcházející úroveň. Znak '<b>\</b>' na začátku nebo  na  konci  zobrazovací  úrovně
 vyvolá odstránkování před nebo po výstupu úrovně.
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t379">PopisnáČástÚrovně</a>     <a href="editory.md#t237">formátování textu</a>     <a href="#t385">░ masky v sestavě</a>     <a href="formular-sestava.md#t386">░ sestava</a>
</pre>

<a name="t385"></a>
## ░ masky v sestavě

<pre>
                         <b>PŘÍKLAD - MASKY V SESTAVĚ</b>
 ──────────────────────────────────────────────────────────────────────────────
 ░ hodnota          maska               výstup

 čísla
 ░ 3.14             __.__                3.14
 ░ -7.84951         __.__               -7.85           {automaticky formátuje}
 ░ 0                __.__                0.00
 ░ 0                @@.@@                 .
 ░ 726324           __.__.__            10.08.89
 ░ 726324           __.__               726324.00
 ░ 0.5              __:__               12:00
 ░ 0.38352858796    __:__                9:12
 ░ 0.38352858796    ____:__:__             9:11:35
 ░ 0.38352858796    _____                   0

 texty
 ░ 'abcdef'         ______XX             abcdefXX
 ░ 'abcdef'         ________XX           abcdef  XX
 ░ 'abcdef'         _       XX           abcdef       XX              {přeteče}
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t457"></a>
## message

*Též:* `display`, `help`, `headline`

<pre>
                            <b>VÝSTUP NA OBRAZOVKU</b>
   ──────────────────────────────────────────────────────────────────────────
   Příkaz  <b>display</b>  zobrazí do  <b>aktuálního okna</b>  help  jako běžný text včetně
   interpretace  barev a formátování textu  podle  šířky  okna.  Příkaz  <b>help</b>
   zobrazí ( do aktuálního okna ) odpovídající  heslo v konvencích helpu, tj.
   včetně navigace na další hesla.

   ██ syntaxe :       <b>DISPLAY(</b> HesloHelpu <b>)</b>
                         <b>HELP(</b> HesloHelpu <b>)</b>
   ────────────
   Zápis do prvního (systémového) řádku obrazovky, výpis je centrován.
   Zprava se doplní aktuální datum (od verze 4.2 ve formátu DD.MM.YYYY).

   ██ syntaxe :       <b>HEADLINE( </b>TextVýraz <b>)</b>
   ────────────

   Hlášení v posledním řádku potvrzované klávesou F10.

   ██ syntaxe:        <b>MESSAGE(</b> SeznamPrvků [<b>,help=</b>HesloHelpu] <b>)</b>
   
   ■  <b>SeznamPrvků</b> ... viz. příkaz <a href="procedury.md#t302">write</a>
   ■  <b>HesloHelpu</b> .... Textový výraz pro heslo help-souboru. Před potvrzením
                      hlášky klávesou F10 může uživatel klávesou <b>F1</b> vyvolat
                      odpovídající text helpu.
   ──────────────────────────────────────────────────────────────────────────
   <a href="procedury.md#t444">komunikační příkazy</a>          <a href="procedury.md#t474">okna pro výstup</a>           <a href="#t322">soubor pro nápovědu</a>
</pre>

<a name="t519"></a>
## parametry editace

*Též:* `tab`, `noed`, `dupl`, `journal`, `saveafter`, `watch`, `refresh`

<pre>
                             <b>PARAMETRY EDITACE</b>
   ──────────────────────────────────────────────────────────────────────────
   Parametry příkazu <b>edit</b>, lze  je použít i v definici <b>uživatelského pohledu</b>.
 
   ██ <b>tab</b>=(SeznamÚdajů) .... nastavení <b>tabelátorů</b> , viz. <a href="formular-sestava.md#t538">seznam údajů</a>
   ██ <b>dupl</b>=(SeznamÚdajů) ... nastavení automatické <b>duplikace</b>
   ██ <b>noed</b>=(SeznamÚdajů) ... zadané údaje nelze editovat (měnit), ani při 
                             pořízení

   ██ <b>mode</b>='SeznamMódů' .... mód editace (parametrizace některých funkcí 
                             datového editoru) viz. <a href="#t530">módy datového editoru</a>

   ██ <b>cond</b>=(LogVýraz) ...... podmínka pro výběr vět k editaci, v podmínce 
                             lze použít <a href="jazyk.md#t440">key in</a> - výběr podle indexu
   ██ <b>journal</b>=NázevSouboru . soubor pro uchování změn při editaci (<a href="soubory.md#t315">journalof</a>)
   ██ <b>saveafter</b>=ČísVýraz ... aut.uložení na disk po daném počtu změněných vět.
                             Zajistí korektní stav editovaného souboru. Menší
                             saveafter znamená vyšší bezpečnost při výpadku
                             proudu či jiné havárii.
   ██ <b>ww</b>=(...) ............. <a href="procedury.md#t524">editace v okně</a>
   ██ <b>exit</b>=(...) .     ..... <a href="editory.md#t536">přerušení editace</a> (volání vnořené procedury)

   ██ <b>head</b>=TextVýraz ... nestandardní první (systémový) řádek editoru, může
                         obsahovat masku pro  <b>číslo věty</b> _____ , (a) nebo
                         masku pro dnešní <b>datum</b> __.__.__)

   ██ <b>watch</b>=ČísVýraz ... časový interval (sek.), pokud tak dlouho není  
                         stisknuta klávesa,  zvukově varuje, čekání 3 x opakuje 
                         a potom ukončí automaticky editaci. Je to prevence 
                         proti zablokování sítě uživatelem (<a href="jazyk.md#t550">edbreak</a>=11)
                         Pouze pro sdílené soubory ( #, #R v katalogu ).

   ██ <b>refresh</b>=ČísVýraz.. časový interval (sek.) pro automatickou obnovu
                         obrazovky při editaci sdílených souborů v LAN.

   ██ <b>last</b>,<b>Ctrl</b>,<b>Shift</b>,<b>Alt</b> ... <a href="rozhrani.md#t523">alternativní nápověda</a>

   ─── Ctrl-End ─────────────────────────────────────────────────────────────
   <a href="editory.md#t529">procedurální parametry editace</a>     <a href="procedury.md#t551">░ volání editoru</a>    <a href="soubory.md#t350">uživatelské pohledy</a>
</pre>

<a name="t530"></a>
## módy datového editoru

<pre>
                           <b>MÓDY DATOVÉHO EDITORU</b>
   ──────────────────────────────────────────────────────────────────────────
   Parametr procedury edit <b>mode</b>='SeznamMódů' ovlivňuje  chování editoru během
   editace.  Zadává se  v textové  konstantě nebo v textovém výrazu, složeným
   z dvoupísmenných zkratek. Textový výraz se vyhodnotí až při provedení.
   Případné neplatné kombinace se ignorují.

   ■ <b>^Y</b> ... zákaz rušení vět
   ■ <b>?Y</b> ... rušení vět s potvrzením A/N
   ■ <b>^N</b> ... zákaz vytváření vět ( mode='^y^n' - nelze ani třídit )
   ■ <b>F2</b> ... režim pořízení
   ■ <b>01</b> ... editace jedné věty ('F201'...pořízení jedné věty)
   ■ <b>!!</b> ... editace na tabelátorech (ostatní údaje jen na prohlížení)
   ■ <b>F3</b> ... hledání podle vlastního klíče (<a href="soubory.md#t338">vlastní klíč</a>)
   ■ <b>&lt;=</b> ... potlačí hlášení F10 - věta neexistuje při hledání F3
   ■ <b>??</b> ... konec editace bez dotazu (<a href="#t223">instalace konstant</a>)
   ■ <b>?E</b> ... opak-vyžaduje potvrzení výskoku Esc dotazem opustit editor ? (A/N)
   ■ <b>#A</b> ... zákaz vypnutí aditivních změn (<a href="soubory.md#t348">aditivní změny</a>)
   ■ <b>#L</b> ... zákaz vypnutí logických kontrol (<a href="soubory.md#t354">logické kontroly</a>)
   ■ <b>R2</b> ... použije 2.řádek obrazovky k vypsání výrazu pro aktuální podmnožinu
   ■ <b>F1</b> ... zobrazuje nápovědu z .HLP souboru pod heslem NázevSouboru.NázÚdaje
            při pohybu po souboru jsou na posledním řádku jednořádkové nápovědy
            <b>Ctrl-F1</b> vyvolá celoobrazovkovou nápovědu. Viz.<a href="#t322">soubor pro nápovědu</a>
   ■ <b>F124</b>.. Nápověda se zobrazí na 24.řádku   (<a href="#t221">instalace barev</a>)
   ■ <b>WX</b> ... při editaci sdíleného .X souboru vytváří pracovní index
            a tím umožňuje ostatním stanicím mazat věty (<a href="#t829">lokální sítě</a>)
   ■ <b>S7</b> ... potlačení chybového hlášení s ShiftF7, přímý přechod do editace
            nadřízeného souboru s možností převzetí údaje
   ■ <b>CO</b> ... podmínka COND slouží jen k výběru podmnožiny, nebude kontrolována
            při aktualizaci resp. pořízení nových vět
   ■ <b>LI</b> ... Při hledání pomocí F3, pro údaje typu A,N se nastavuje kurzor
            editoru již v průběhu zadávání.  <a href="soubory.md#t336">klíč</a>
   ■ <b>^M</b> ... Double click levé klávesy myší není INS ale ENTER.
            <a href="rozhrani.md#t203">myš v datovém a textovém editoru</a>
   ■ <b>-&gt;</b> ... Klávesa <b>Up</b> generuje <b>&lt;-</b>, klávesa <b>Down</b> generuje <b>-&gt;</b>.
   ■ <b>EX</b> ... exit-procedury vyvolané klávesou lze vyvolat i z editace údaje
            typu T. Parametr aktuální věty se předává stejně jako při volání 
            z jiných míst. Rovněž nastavuje <a href="jazyk.md#t550">edfield</a>,<a href="jazyk.md#t550">edirec</a>,... Navíc nastavuje
            i <a href="editory.md#t591"> txtpos</a>, který bude pro ostatní typy přerušení -1.
            Explicitní nápovědné řádky ( last=, shift=,...) se zobrazí 
            i v textovém editoru.  (viz. <a href="editory.md#t596">přerušení editace textu</a>)
   ■ <b>SL</b> ... Při použití parametru <a href="editory.md#t529">sel</a> lze klávesou ENTER v existující větě
            ukončit editaci. Pokud byl zatím pracovní index prázdný, bude 
            zároveň aktuální věta vybrána. V opačném případě se index nezmění.
   ■ <b>?N</b> ... Při přerušení pořizování nové věty klávesou ESC nebo F2 je od
            obsluhy vyžádáno potvrzení dotazem "Nedokončená věta bude zrušena! 
            Přesto opustit A/N?" (<a href="#t223">Instalace konstant</a>).

 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t519">parametry editace</a>              <a href="procedury.md#t551">░ volání editoru</a>              <a href="jazyk.md#t789">»P</a>
</pre>

<a name="t560"></a>
## parametry sestavy

*Též:* `  assign`, `times`, `printctrl`

<pre>
                             <b>PARAMETRY SESTAVY</b>
   ──────────────────────────────────────────────────────────────────────────
   Následující nepovinné parametry  se používají pro sestavy podle kapitoly R
   i pro automatické sestavy.

   ■ <b>cond</b>=(LogVýraz) ...... logická podmínka pro výběr vět souboru do sestavy
                            v podmínce lze použít <a href="jazyk.md#t440">key in</a> - výběr podle indexu
                            uplatní se pouze pro první vstupní soubor (#I1_..)

   ■ <b>cond=( ? )</b> ........... podmínku zadá uživatel před generováním sestavy

   ■ <b>assign</b>=UrčeníVýstupu ... přesměrování výstupu <b>na tiskárnu</b> 'LPT1'
                            nebo <b>do souboru</b>: jméno text.souboru včetně cesty
                            nebo název dle katalogu. Implicitně se sestava 
                            vygeneruje do textu 'PRINTER.TXT' a edituje se
                            <b>assign</b> potlačí editaci výsledné sestavy

   ■ <b>times</b>=ČísVýraz ....... počet výtisků při tisku sestavy
                            (vygeneruje na začátek tečkový příkaz <b>.ti</b>)

   ■ <b>edit</b> ................. standardně se sestava edituje jen v módu prohlížení
                            <b>edit</b> dovolí úplnou editaci (změny) sestavy

   ■ <a href="soubory.md#t554">check</a> ..............   příkaz report provede jen kontrolu syntaxe
                            dynamické deklarace kapitoly R.

   ■ <b>printctrl</b> ............ výstup do <b>tiskového souboru</b>
     (místo řídících znaků pro přepínání typů písma se do sestavy generují
     rovnou kódy pro tiskárnu podle instalace - např.pro tisk mimo PC FAND,
     většinou  ve spojení s <b>assign</b>, POZOR - nezpracuje tečkové příkazy).
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t562">parametry automatické sestavy</a>     <a href="editory.md#t240">tečkové příkazy</a>     <a href="jazyk.md#t789">»P</a>
</pre>

<a name="t562"></a>
## parametry automatické sestavy

*Též:* `style`

<pre>
                       <b>PARAMETRY AUTOMATICKÉ SESTAVY</b>
   ──────────────────────────────────────────────────────────────────────────
   ■ <b>mode=errcheck</b> ......... automatická sestava "Chybné věty"
   ■ <b>mode=onlysum</b> .......... automatická sestava "Jen součty"
   ■ <b>sort</b>=(TřídícíÚdaje) ... dočasné setřídění souboru
                             (nemá vliv na data po ukončení sestavy)
   ■ speciální znaky před názvy třídících údajů: <b>&gt;</b> ... sestupné setřídění
                                                 <b>~</b> ... lexikální setřídění
   ■ <b>ctrl</b>=(SeznamÚdajů) .... řídící údaje součtované sestavy
                             (nejsou automaticky zároveň třídícími údaji)
   ■ <b>sum</b>=(SeznamÚdajů) ..... součtované údaje sestavy

   ■ <b>width</b>=ČísVýraz ........ počet znaků na řádek podle šířky papíru
                             (implicitně podle instalace)
   ■ <b>style=compressed</b> ...... tisk úzkým písmem (17cpi)
   ■ <b>style=normal</b> .......... tisk normálním písmem (10 cpi)
   ■ <b>head</b>=TextVýraz ........ vlastní hlavička strany autoreportu (řádek textu)
                             maska pro <b>číslo strany</b> _____ ,
                             maska pro dnešní <b>datum</b> __.__.__
                             V textu hlavičky se nesmí vyskytovat znak '\'

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t248">automatická sestava</a>     <a href="#t560">parametry sestavy</a>     <a href="#t220">instalační program</a>     <a href="jazyk.md#t789">»P</a>
</pre>

<a name="t609"></a>
## kopírování a převody

*Též:* `fix`, `copyfile`

<pre>
                            <b>KOPÍROVÁNÍ A PŘEVODY</b>
   ───────────────────────────────────────────────────────────────────────────
   Příkaz procedury <b>CopyFile</b> provádí kopírování souborů. Může jít jak o prosté
   kopírování , připojení tak i o převody - datové struktury , diakritiky nebo
   formátu. Vlastní proces kopírování  dat má buď charakter fyzického ( binár-
   ního ) přenosu, tak i strukturovaného přenosu definovaných dat - v podstatě
   <a href="formular-sestava.md#t566">merge</a> včetně změny pořadí vět. Charakter operace se odvozuje od typu použi- 
   tých souborů a dalších parametrů příkazu.

   ██ Syntaxe:    <b>COPYFILE (</b> Soubor1[<b>/</b>Klíč | Formát | <b>.X</b>], Soubor2 [<b>/</b>Formát]
                             [ <b>,head=</b>TextovýVýraz ]
                             [ <b>,nocancel</b> ] [ <b>,append</b> ] [ <b>,mode='</b>Převod<b>'</b>] <b>)</b>

   ■  <b>Soubor1</b>,<b>Soubor2</b> ... Vstupní a výstupní soubor. Lze zadat buď logické
                          jméno - tj. <b>název kapitoly F</b> nebo <b>jméno dle katalogu</b>,
                          nebo <b>fyzické jméno</b> ( v apostrofech, včetně cesty ).
                          Použitý odkaz na vstupní a výstupní soubor má zásadní
                          vliv na způsob kopírování - viz. níže <b>varianty</b>
   ■  <b>Klíč</b> .............. Název klíče indexového souboru, věty se kopírují v
                          pořadí dle klíče. Pokud není použit, kopíruje se
                          dle fyzického pořadí.
   ■  <b>Formát</b> ............ Definuje typ souboru při změně formátu. Lze použít:
                          <b>FIX</b>  textový soubor v pevném formátu
                          <b>VAR</b>  textový soubor ve volném formátu (viz.<a href="#t611">formát</a>)
                          <b>TXT</b>  jen pro vstupní soubor, převod textového souboru
                               PC FANDu do standardního tvaru (^M -&gt; ^M^J)
                          <b>POZOR !!!</b>
                          Při použití /FIX nebo /VAR pozor na převod údajů
                          typu D, zvláště při implicitní masce DD.MM.YY.
                          Implicitní století se dosadí podle obecných
                          pravidel, viz. <b>Posun implicitního století</b>,
                          <a href="#t223">Instalace konstant</a>
   ■  <b>.X</b> ................ Kopíruje se včetně indexů (soubor .X00)
                          !!! Při variantě copyfile(A.x,B), kde A i B jsou
                              názvy kapitol F, se indexy nekopírují (interně
                              jde o merge).
   ■  <b>head</b>=GlobProměnná.. Při převodech <b>/fix</b> a <b>/var</b>, vytvoří první řádek
                          (hlavičku) textového souboru. Při importu textového 
                          souboru to musí být uložený údaj.
   ■  <b>append</b> ............ <b>Soubor1</b> se připojí za konec <b>Souboru2</b>. Zde pozor na 
                          rozdíl mezi fyzickým a logickým připojením (viz.níže).
   ■  <b>mode</b>='Převod' ..... Převod mezi různými kódy diakritiky. Má význam pouze
                          pro <b>textové soubory</b>. Jde o dvouznakovou konstantu.
                          Kódy: <b>K</b> - Kamenických 
                                <b>L</b> - Latin2
                                <b>W</b> - Windows (CP1250)
                                <b>N</b> - bez diakritiky      ┌─────────────────────┐
                          Povolené kombinace pro <b>převod</b>:│<b>KL</b>,<b>LK</b>,<b>KN</b>,<b>LN</b>,<b>KW</b>,<b>LW</b>,<b>WL</b> │
                                                        └─────────────────────┘
   ■  <b>nocancel</b> ... Potlačí ukončení úlohy při chybě, nastaví kód chyby <a href="procedury.md#t626">exitcode</a>


 ██ <b>POZNÁMKY</b>
    Pokud použijeme jako <b>Soubor1</b> nebo <b>Soubor2</b> název <a href="jazyk.md#t652">identifikátor</a>u, chápe se
    prioritně jako název kapitoly F. Pokud taková neexistuje, hledá se odpo- 
    vídající název dle katalogu. Není-li nalezen, je to chyba syntaxe. Nelze
    použít lokální proměnnou typu string. Fyzickou cestu lze přímo v příkazu 
    zadat jen jako konstantu ( v apostrofech ). Kopírování  s možností změny
    cesty lze realizovat přes katalog viz. příklad.


 ██ <b>VARIANTY:</b>

    <b>Soubor1 i Soubor2 jsou kapitoly F</b>
    V tomto případě se příkaz copyfile interně realizuje jako implicitní <a href="formular-sestava.md#t566">merge</a>.
    #I1_Soubor1
    #O1_Soubor2 [+] { <b>+</b> se použije když je <b>append</b> }
    Z toho plynou tyto podstatné skutečnosti:
           - kopírování se provádí v kontextu logické struktury souborů. Pokud
             není deklarace souborů stejná, dojde ke konverzi "by-name" podle
             pravidel merge. Může se tedy i změnit fyzická velikost souboru.
           - jde o jediný případ, kdy lze kopírovat <b>do sebe</b> - inplace.
             POZOR - inplace nelze obecně syntakticky odhalit - jestliže 
             v jiných případech "projde", nemusí být výsledek korektní.

    <b>Jeden ze souborů je fyzický soubor</b>
    Tj. název dle katalogu nebo konstanta v apostrofech. Pokud nejde o export
    nebo import (není <b>VAR</b> nebo <b>FIX</b>) kopíruje se fyzicky, i když je odkaz na
    ten druhý soubor proveden názvem kapitoly F.

    <b>Append - připojení fyzické a logické</b>
    Při fyzickém připojení se bez ohledu na vnitřní strukturu dva soubory
    prostě "srazí" k sobě. To znamená, že výsledek nemusí odpovídat požadavku
    na strukturu dat. A pro FANDovské datové soubory také neodpovídá.
    Oproti tomu při logickém spojení dochází navíc k aktualizaci <b>prefixu</b>
    souboru a k připojení jednotlivých vět. Zvláště zřejmé je to pro soubor
    volných textů (.T00).

    ──────────────────────────────────────────────────────────────────────────
    <a href="#t641">katalog</a>     <a href="#t610">░ copyfile</a>     <a href="jazyk.md#t789">»P</a>
</pre>

<a name="t610"></a>
## ░ copyfile

<pre>
                       <b>PŘÍKLAD - KOPÍROVÁNÍ A PŘEVODY</b>
   ──────────────────────────────────────────────────────────────────────────

   ██  <b>základní varianty</b>
   
       copyfile(DATA1,DATA2);       {Jeden datový soubor projektu do druhého}
       copyfile(DATA,'C:\ARCHIV\KOPIE.000');        {Vytvoření záložní kopie}
       copyfile('A:\KOPIE.000',DATA);              {Obnovení souboru z kopie}
       copyfile('DOP.TXT','DOP.BAK');               {Kopie textového souboru}
       copyfile(DATA1,DATA2,append);            {Logické připojení za soubor}
       copyfile(DATA,'EXPORT'/var);         {Export dat do textového souboru}
       copyfile('\WORK\DATA.TXT'/var,DATA); {Import dat např.z jiné databáze}
       copyfile('T1.TXT','T2.TXT',mode='KN');         {Odstranění diakritiky}


   ██  <b>převod úlohy .RDB z kódu Kamenický do Latin2</b>
       { nebo obráceně, podobně i převod diakritiky v textech datového souboru }

       Úloha musí být "otevřená" (nezaheslená). Sestavíme pomocnou úlohu, ve
       které deklarujeme převáděnou jako soubor typu .RDB

       F  Uloha.RDB   *
       M  NullOldTxt  *  #I1_Uloha               { vynuluje starou deklaraci }
                         #O1_Uloha  StText:='' ; { viz. kapitoly F           }
       P  Prevod      *  BEGIN
                         merge(NullOldTxt) ;
                         copyfile( Uloha,'pomocny.txt'/VAR) ;
                         copyfile( 'pomocny.txt','prevedeno.txt',mode='KL') ;
                         copyfile( 'prevedeno.txt'/VAR, Uloha ) ;
                         END ;
                         { formát VAR je nutný kvůli údajům typu volný text}

   Pro převod datových souborů bez volných textů lze použít i formát FIX.

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t611"></a>
## formát

<pre>
                   <b>FORMÁT</b> uložení dat v textovém souboru
   ──────────────────────────────────────────────────────────────────────
   Při transportu dat z jiných databází , obecně libovolných programů, je
   nejvýhodnější použít  standardní  <a href="soubory.md#t324">soubor formátu .DBF</a> , které  PC FAND
   podporuje.  Pokud to z nějakého důvodu nelze , je  možno použít převod
   prostřednictvím textového (ASCII) souboru. Ovšem uložení dat v takovém 
   souboru musí odpovídat  určitým konvencím, aby je bylo možno automati-
   zovaně zpracovat. PC FAND podporuje dva formáty:

   <b>FIX</b> - Jednotlivé údaje jsou uloženy v textové representaci vedle sebe 
         v jednom  řádku , bez oddělovačů. Vždy  v maximální deklarované
         délce. Každý údaj má tedy  pevně danou pozici od počátku řádku.
         Co věta, to řádek. Údaje typu T se ignorují.

         <b>typ PC FANDU        F,n.m    R    A,n     N,n     D,Maska       B</b>
         délka ve FIX    m&gt;0 n+m+2   17    n       n       délka masky   1
         formátu         m=0 n+1

   <b>VAR</b> - Jednotlivé hodnoty jsou od sebe odděleny čárkou. Hodnota každého 
         <b>textového</b> údaje (A,T) je uzavřena v apostrofech. Může  tedy míti
         proměnnou  délku a lze  proto  transportovat i údaje typu  volný
         text (T). Pokud apostrofy chybí, bere se úsek mezi dvěma čárkami.
         Apostrof uvnitř textu  je zapsán dvojitě.  Jedna  věta může býti
         uložena ve více řádcích. Další věta začíná vždy na novém řádku.

   V obou formátech může první řádek obsahovat hlavičku souboru (param. <b>head</b>).
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t609">copyfile</a>
</pre>

<a name="t616"></a>
## zálohování

*Též:* `archivace`

<pre>
                                 <b>ZÁLOHOVÁNÍ</b>
   ──────────────────────────────────────────────────────────────────────────
   PC FAND zahrnuje podporu pro zálohování (archivaci) datových  souborů,
   přičemž existují dva možné přístupy:

   1.  <b>logické</b> zálohování souborů, jejichž seznam se uveden v katalogu
               příkazy <a href="procedury.md#t613">backup</a> a <a href="procedury.md#t614">restore</a>
   2.  <b>fyzické</b> zálohování souborů z adresáře podle masky (analogie ARJ,ZIP,...)
               příkazy <a href="procedury.md#t620">backupm</a> a <a href="procedury.md#t620">restorem</a>

   <b>Zásady logické archivace:</b>

   ■  Pro účely zálohování má základní význam <a href="#t641">katalog</a>  úlohy. Pomocí katalogu
      lze definovat  určité úrovně archivace ( např. denní, měsíční,...) 
      a rozdělení souborů do těchto úrovní.

   ■  Pro každý soubor, který chceme zálohovat musí v katalogu existovat
      položka s cestou k souboru a číslem archivace (Ar). Pomocí údaje kata-
      logu <b>číslo archivace</b> (<b>ar</b>) lze určit do které úrovně archivace patří.

   ■  Pro každou úroveň archivace lze speciální položkou katalogu určit
      <b>název archivace</b> a adresář pro zálohovaná data. Pod jeden název archivace
      lze spojit více úrovní archivace:

      NazUlohy   NazSouboru      Ar         Cesta                   Navesti
      <b>ARCHIVES</b>   <b>NázevArchivace</b>  <b>ČísloArch.</b> <b>AdresářArchívu</b>   <b>Návěští 1.diskety</b>

      <b>ARCHIVES</b> ......... povinné jméno úlohy
      <b>NázevArchivace</b> ... použije se v příkazech <a href="procedury.md#t613">backup</a> a <a href="procedury.md#t614">restore</a>.
      <b>ČísloArchivace</b> ... určení úrovně archivace, další úrovně mohou být
                         zadány v položce cesta - viz. dále
      <b>AdresářArchívu</b> ... musí končit znakem '<b>\</b>', za alespoň jednou mezerou
                         lze psát další <b>ČíslaArchivace</b>  (např. A:\  1,3,4 )

   ■  Na jednotlivé soubory se uplatní kompresní algoritmus.

   ■  Zálohování se provede příkazem <a href="procedury.md#t613">backup</a>, obnova dat příkazem <a href="procedury.md#t614">restore</a>.

   ■  Pokud je to nutné (nejsou čitelné), jsou při zálohování (volitelně)
      formátovány cílové diskety, včetně zápisu návěští.
      Zahrnuta je i podpora více disket dle konvencí příkazu <a href="#t609">copyfile</a>.

   ■  Název zálohovaného souboru se rovná názvu originálu bez cesty.Rozlišení
      pomocí přípony :  .0xx resp. Txx       xx je pořadové číslo během 
                                                kopírování x je 0..9,A..Z
                        .5xx resp. Yxx       pokud pokračuje na další disketě
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t223">instalace konstant</a>
</pre>

<a name="t633"></a>
## catalog

*Též:* `resetcatalog`

<pre>
                                  <b>CATALOG</b>
   ──────────────────────────────────────────────────────────────────────────
   V každé úloze je  <a href="#t641">katalog</a> úlohy  implicitně  deklarován jako  běžný datový
   soubor s názvem <b>catalog</b>.  Pokud tento název použijeme pro deklaraci jiného
   souboru (kapitoly F) bude implicitní deklarace lokálně překryta.

   ██ <b>Struktura katalogu</b>         NazUlohy : A,8 ;
                               NazSouboru : A,8 ;
                                       Ar : N,2 ;
                                    Cesta : A,79 ;
                                  Navesti : A,11 ;

   Catalog lze v zásadě používat jako každý jiný soubor. Díky výsadnímu posta-
   vení katalogu je však třeba řešit důsledky některých akcí, které mají vliv
   na právě otevřené datové soubory. K tomu slouží příkaz <b>resetcatalog</b>.

   ██ Syntaxe:          <b>RESETCATALOG</b> 
   
      Příkaz procedury, který je nutno použít vždy když:
             - jsou zrušeny věty katalogu
             - jsou doplněny nové věty jinde než za koncem katalogu.
      Resetcatalog zajistí korektní promítnutí změn katalogu do systému 
      aktuálních (otevřených) souborů. Kromě aktivních .RDB uzavře všechny 
      datové soubory. Ošetřeny jsou i <a href="sql.md#t885">SQL</a>-soubory.

   <b>POZOR</b> - před jakýmikoliv změnami  katalogu je účelné  pomocí příkazu <b>close</b>
           uzavřít  soubor resp. soubory, kterých  se změna týká. Vyhneme  se
           tak určitým úskalím použití příkazu <b>resetcatalog</b>.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t638">path</a>     <a href="#t638">volume</a>      <a href="#t638">generation</a>       <a href="#t638">katalog v proceduře</a>
</pre>

<a name="t638"></a>
## katalog v proceduře

*Též:* `generation`, `path`, `volume`, ` archives`

<pre>
                            <b>KATALOG V PROCEDUŘE</b>
  ───────────────────────────────────────────────────────────────────────────
  Z procedury lze <b>korektně</b> zpracovat údaje katalogu metodami přímého přístupu.
  Požadovaná věta  katalogu se  určí názvem souboru ( nikoliv fyzickým číslem
  věty jako u běžného datového souboru ).  Tyto výrazy se mohou vyskytovat na
  levé i pravé straně přiřazovacího příkazu, kromě odkazu <b>.generation</b>, který
  se může vyskytnout pouze na pravé straně (generaci souboru lze měnit pouze 
  příkazem <a href="procedury.md#t640">TurnCat</a>.).

  ██ <b>NázevDleKatalogu.PATH</b> ....... Cesta (úplná fyzická cesta datového souboru)
  ██ <b>NázevDleKatalogu.VOLUME</b> ..... Návěští (volume diskety)
  ██ <b>NázevDleKatalogu.GENERATION</b>.. Číslo generace
  ██ <b>NázevDleKatalogu.ARCHIVES</b>.... Číslo úrovně archivace


  Jiný způsob práce s katalogem je založen na využití implicitně deklarovaného
  souboru <a href="#t633">catalog</a>.

  ────────────────────────────────────────────────────────────────────────────
  <a href="#t641">katalog</a>       <a href="#t642">soubory v katalogu</a>       <a href="procedury.md#t640">generační soubory</a>       <a href="jazyk.md#t789">»P</a>
</pre>

<a name="t641"></a>
## katalog

<pre>
                                  <b>KATALOG</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Katalog</b> je nepovinný soubor přiřazený k úloze (stejné jméno, přípona <b>.CAT</b>,
   kterým lze  měnit ve speciálních případech některé implicitní  vlastnosti,
   především <b>jména fyzických souborů</b> na disku.  Jedna věta katalogu  odpovídá
   jednomu souboru v úloze (soubor je pak <b>katalogizovaný</b>) a obsahuje údaje:

   ■ <b>Název úlohy</b> ..... Úloha, v níž je soubor popsán a používán.
   ■ <b>Název souboru</b> ... Logické jméno souboru používané ve zdrojových textech
                       úlohy (pozor - nemusí jít nutně o kapitolu F).
                       Název kapitoly F se uvádí bez přípony (.X,.RDB,.DBF)
   ■ <b>Číslo archivace</b> . Zařazení souboru do úrovně archivace (viz.<a href="#t616">zálohování</a>)
   ■ <b>Cesta</b> ........... Fyzické jméno souboru (<b>Jednotka,Cesta,Jméno,Přípona</b>).
                       Cesta nemusí být úplná, platí běžné konvence MS-DOSu
                       pro kompletaci cesty s tím, že jako aktuální adresář
                       platí adresář úlohy.
   ■ <b>Návěští</b> ......... Volume diskety, pokud se soubor nachází na disketě
                       nebo znak <b>#</b>, <b>#R</b>, <b>SQL</b> (viz. <a href="#t829">lokální sítě</a>, <a href="sql.md#t885">SQL</a>)

   ██ <b>Založení a úprava katalogu</b>
      Katalog se zakládá a upravuje z hlavního menu PC FANDu volbou <b>Instalace</b>
      <b>úlohy</b> (i v uživatelské verzi).

   ██ <b>Dynamická změna katalogu aktivní úlohy</b>
      Ze spuštěné úlohy je možno dynamicky zasáhnout do katalogu vlastní
      úlohy. V každém případě však se zvýšenou opatrností. Jsou v zásadě
      dvě možnosti:
      ■  Přímý přístup k údajům věty katalogu z procedury - <a href="#t638">path</a>, <a href="#t638">volume</a>,
                                                            <a href="#t638">generation</a>.
      ■  Katalog je implicitně deklarovaný datový soubor s názvem <a href="#t633">catalog</a>,
         se kterým lze pracovat jako s běžným souborem.

   V katalogu  rozlišujeme  <b>interní soubory</b>  ( datové  soubory  deklarované v
   projektu v kapitole F ) a <b>externí soubory</b> ( texty, programy, záložní kopie
   atd. uvedené pouze v katalogu, v projektu se vyskytují bez udání struktury  
   jako  parametry procedur) - viz. <a href="#t642">soubory v katalogu</a>.

   <b>Poznámka </b>Mezi verzí PC FANDu 3.0 (3.01) a 3.2 došlo ke změně struktury
            katalogu (položka Ar). Starý formát se na nový převede automaticky,
            Převod nového formátu na starý lze provést programem <b>OldCatlg.EXE</b>.
            (Samozřejmě lze použít i vhodný MERGE).
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t638">katalog v proceduře</a>      <a href="#t829">lokální sítě</a>      <a href="kontextova-napoveda.md#t9">instalace úlohy</a>      <a href="#t616">zálohování</a>
</pre>

<a name="t642"></a>
## soubory v katalogu

<pre>
                             <b>SOUBORY V KATALOGU</b>
   ───────────────────────────────────────────────────────────────────────────
   <b>Katalogizovat</b> lze následující soubory:

   ■ <b>Datový soubor</b> ... jiné fyzické <b>jméno</b> (nebo <b>přípona</b>) než podle kapitoly F
                   ... soubor je umístěn v jiném <b>adresáři</b> (disku) než úloha
                   ... soubor je na disketě (automatická práce s <b>návěštím</b>)
                   ... sdílený soubor v síti (<a href="#t829">lokální sítě</a>)
                   ... soubor <a href="sql.md#t885">SQL</a>
                   ... <a href="procedury.md#t640">generační soubory</a>
                   ... <b>archivní soubory</b> pouze pro záložní kopie (<a href="#t609">copyfile</a>)
                       bez deklarace v kapitole F (externí soubory)
                   ... <b>vícedisketové soubory</b> (<a href="#t609">copyfile</a>)
   v katalogu je <b>návěští</b> první diskety, ostatní mají ASCII kód posledního znaku
   o 1 vyšší než předchozí disketa;   pouze externí soubory
   <b>přípona</b>: 2. a 3. znak jsou v rozsahu '00'..'49'

   ■ <b>Textový soubor</b> ...... procedury pro zpracování textu <a href="editory.md#t595">edittxt</a>, <a href="procedury.md#t588">printtxt</a>
   ■ <b>Externí program</b> ..... obecné volání programu <a href="procedury.md#t626">exec</a>
   ■ <b>Projektový soubor</b> ... katalogizace subúloh (subúloha není v podadresáři)
                           <b>NázÚlohy</b>: volající,<a href="procedury.md#t436"> call</a>(<b>NázSouboru</b>): volaná úloha
   ───────────────────────────────────────────────────────────────────────────
   <a href="#t638">katalog v proceduře</a>
</pre>

<a name="t691"></a>
## valdate

*Též:* `today`, `currtime`, `strdate`

<pre>
                       <b>AKTUÁLNÍ DATUM A ČAS, KONVERZE</b>
   ──────────────────────────────────────────────────────────────────────────

   Funkce <b>today</b> vrací aktuální (dnešní) datum podle systémových hodin.Podobně
   funkce  <b>currtime</b> vrací aktuální čas. Obě návratové hodnoty jsou ve formátu
   pro typ údaje D PC FANDu.

   ██ Syntaxe:             <b>TODAY</b> : real
                        <b>CURRTIME</b> : real


   <b>Rok 2000 !!!</b>   Speciální přiřazení <a href="#t700">Today:=</a>real;

   ─────────────────────

   Konverzní funkce <b>strdate</b> převede datum a čas v interním formátu na string.
   Funkce <b>valdate</b> provede pravý opak. Obě funkce provedou převod podle masky.
   <a href="#t330">Maska pro datum</a> formátuje textové vyjádření datumu a času.

   ██ Syntaxe:     <b>STRDATE (</b> ČísVýraz <b>,</b>Maska <b>)</b>  : string
                   <b>VALDATE (</b> TextVýraz <b>,</b>Maska <b>)</b> : real

   ■  <b>ČísVýraz</b> .... Datum a čas v interním formátu PC FANDu (real)
   ■  <b>TextVýraz</b> ... Datum a čas v textové presentaci.
   ■  <b>Maska</b> ....... Uvedení dnů bez měsíce a roku se nechápe jako datum ale
                    jako počet dní.
   ──────────────────────────────────────────────────────────────────────────
   <a href="jazyk.md#t692">funkce pro datum a čas</a>                       <a href="#t701">░ datum a čas</a>
</pre>

<a name="t700"></a>
## Rok 2000

*Též:* `today:=`

<pre>
                          <b>Problém roku 2000 v PC FANDu</b>
   ──────────────────────────────────────────────────────────────────────────

   Problém roku 2000 v PC FANDu teoreticky <b>neexistuje</b>, neboť datumový typ
   v PC FANDu ukládá interně vždy plný tvar datumu (formát <b>DD.MM.YYYY</b>).
   Z  hlediska vnitřního uložení tedy není rok 2000 nijak významný. Dále
   je podstatné to, že se rok 2000 zpracovává jako přechodný.

   Celkově a podrobně byl problém roku 2000 ve vztahu k PC FANDu popsán
   ve zpravodaji ALIS MAIL číslo 1 a 4 / 1998. Podobné texty jsou dostupné
   i na firemním WEBu <b>www.alis.cz</b>.

   V praxi však vznikají obtíže v konkrétních úlohách, jejichž zdroje
   jsou nejčastěji tyto:

   <b>■</b> Zobrazení datumu ve formuláři nebo sestavě je v krátkém formátu DD.MM.YY
     Interně je sice v plném formátu, ale uživatel ho nevidí.

   <b>■</b> Z důvodů pohodlnosti zadávání se datum zadá ve zkráceném tvaru, přičemž
     správný zbytek si již má aplikace (potažmo PC FAND) "domyslet". Otázka
     potom zní jak by měl ten "zbytek" datumu, nejčastěji století, vypadat.


   <b>■</b> Přes funkci <a href="#t691">Today</a>, která čte aktuální datum z DOSu (a potažmo z CRT)
     se do aplikace PC FANDu může "zanést" případná chyba daného počítače.


   <b>██ Přehled pomůcek k roku 2000</b>
   <b>──────────────────────────────</b>

   Postupně byly zabudovány do PC FANDu různá vylepšení a novinky, které 
   ulehčují práci programátorům i uživatelům při zpracování roku '00'.

   ■ Speciální instalační konstanta <b>Posun implicitního století</b>,
     viz. <a href="#t223">instalace konstant</a>, řeší převod dvoumístného roku '...YY' na
     plný formát '...YYYY'.
     Zde se uplatní i možnost zadání dvoumístného roku do plné masky.
     Vesměs jde "jen" o řešení pohodlnosti zadávaní datumu pro uživatele.

   ■ <b>!!!</b>
     Přiřazení <b>Today:=</b>ČíselnýVýraz
     Možnosti:
     číselný výraz&gt;0 ... Místo čtení datumu ze systémových hodin se
                         v celém PC FANDu (i interně) použije pro
                         datum tato hodnota.
     číselný výraz=0 ... Návrat k načtení datumu ze systémových hodin.

     Toto by měl být nástroj na řešení případných problémů s rokem 2000
     na úrovni hardware - oper. systém (DOS).

   ■ Možnost pohodlného zadání masky __.__.____ pro plný tvar roku:
     - Deklarace reportu (<a href="#t384">ZobrazovacíČástÚrovně</a>)
     - Parametr <a href="#t284">head</a> příkazu <a href="#t282">edit</a>

   ■ Všechna místa, kde PC FAND automaticky používal "krátký" formát
     datumu 'DD.MM.YY' jsou nahrazeny "plným" formátem (kromě deklarace
     údaje typu D v kapitole <a href="soubory.md#t313">F</a>). To jest:
     - Implicitní <a href="#t457">headline</a> datového editoru.
     - <a href="#t248">Automatická sestava</a> - hlavička.
     - Příkaz procedury <a href="#t457">headline</a>.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t609">copyfile</a>
</pre>

<a name="t701"></a>
## ░ datum a čas

<pre>
                           <b>PŘÍKLAD - DATUM A ČAS</b>
 ──────────────────────────────────────────────────────────────────────────────
 ░ StrDate(Today+CurrTime,'DD.MM.YYYY hh:mm:ss')
 ░ ValDate(Datum,'YMM')
 ░ AddWDays(den,3)
 ░ TypeDay(AddMonth(today,-1))
 ░ DifWDays(1.1.90,31.12.90)
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t829"></a>
## lokální sítě

*Též:* `LAN`

<pre>
                             <b>LAN - LOKÁLNÍ SÍTĚ</b>
   ──────────────────────────────────────────────────────────────────────────
   V lokální  počítačové síti  je možné sdílet FAND , úlohu i datové soubory.
   Před použitím  FANDu v síti je  nutné  nejprve  v  závislosti  na předmětu
   sdílení provést potřebné zásahy do instalace.

   V síťovém provozu FAND automaticky určuje režim sdílení (blokování) 
   datových souborů a koordinuje práci datových editorů.

   Při programování síťových aplikací je možné použít také příkazy with shared
   a with locked.

   Pro sdílené soubory (#,#R) se kromě módu sdílení <a href="#t858">Excl</a> nepoužívá cache paměť
 
   <b>Instalace FANDu v síti</b>        <b>Technika sdílení souborů</b>      <b>Síťové příkazy</b>

   <a href="#t831">instalace sítě</a>                <a href="#t837">technika sdílení</a>                 <a href="#t867">with shared</a>
   <a href="#t833">parametry programu SHARE</a>      <a href="#t845">technika blokování souboru</a>       <a href="#t871">with locked</a>
   <a href="#t834">sdílení FANDu</a>                 <a href="#t846">módy blokování souboru</a>
   <a href="#t835">sdílení úlohy</a>                 <a href="#t847">tabulka módů blokování</a>
   <a href="#t836">sdílení datových souborů</a>      <a href="#t860">technika zamykání vět</a>
   <a href="#t843">používané pojmy</a>               <a href="#t863">sdílená editace</a>
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t831"></a>
## instalace sítě

<pre>
                            <b>LAN - INSTALACE SÍTĚ</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Požadavky na software:</b>
   ■ MS DOS 3.3 nebo vyšší
   ■ síťový operační systém, který využívá MS DOS open-share módy a lock bytů
     i mimo fyzický rozsah souboru (většina dostupných sítí)
 
   <b>Instalace síťového prostředí</b>:
   ■ Adresáři s datovými soubory  přidělujeme přístupová  práva <b>RWOCDM</b> (read/
     write/ open/ create/ delete/ modify attributes).
     Totéž platí pro pracovní soubory PC FANDu ( FANDWORK.?$$ , PRINTER.TXT).

   ■ Pro již existující plně sdílené soubory (# v katalogu) stačí pouze <b>RWO</b>.

   ■ Pro existující sdílené soubory, které budou některé stanice v síti pouze
     číst ( v katalogu  těchto  stanic označeny #R ), stačí adresáři přidělit
     práva <b>RO</b>.  Do této skupiny  lze zařadit  i moduly FAND.* , (U)FANDHLP.*,
     datové soubory jen pro čtení - s DOS-atributem RdOnly  a odladěnou ( za-
     heslenou) úlohu.

   ■ Pro úlohu, kterou chceme ještě instalovat nebo ladit, potřebujeme práva
     <b>RWOM</b> .

   <b>Závěr:</b> V praxi se většinou do jednoho adresáře instalují soubory s různými
          nároky na přístupová práva  ( FAND.* , FANDWORK.* ). Potom se samo-
          zřejmě uplatní nejvyšší z nich.  Na rozsáhlejších sítích ( <a href="#t874">Novell</a> )
          se vyplatí tyto záležitosti neignorovat.

   ■ Je nutné instalovat dostatečně velký maximální počet současně otevřených
     handlů pro otevírané soubory a také maximální počet současně provedených
     locků na serveru.  Některé  sítě vyžadují  na serveru spuštění  programu
     <b>SHARE</b> ( součást operačního systému MS DOS ) s příslušnými  parametry,  u
     jiných je třeba postupovat dle instalačního návodu.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t833">parametry programu SHARE</a>       <a href="#t834">sdílení FANDu</a>       <a href="#t835">sdílení úlohy</a>
   <a href="#t836">sdílení datových souborů</a>       <a href="#t843">lock</a>
</pre>

<a name="t833"></a>
## parametry programu SHARE

*Též:* `share`

<pre>
                       <b>LAN - PARAMETRY PROGRAMU SHARE</b>
   ──────────────────────────────────────────────────────────────────────────
   Některé  sítě  vyžadují  na  serveru spuštění programu <b>SHARE</b>.  Pro správné
   fungování PC FANDu je  nutné  věnovat pozornost nastavení parametrů tohoto
   programu:

   █ <b>SHARE /F:n /L:m</b>

   ■ <b>parametr /F:n</b> - prostor pro tabulku názvů otevřených souborů. Při určení
                     hodnoty   parametru   doporučujeme   následující  postup
                     (přetečení program SHARE nehlásí a může dojít k přepsání
                     paměti):

                     n = 71 * FILES      71 = nejdelší MS DOS cesta + 11 bytů
                                      FILES = maxim. počet otevřených souborů,
                                              uvedený například v CONFIG.SYS

   ■ <b>parametr /L:m</b> - počet locků (zámek souboru nebo věty),  které může SHARE
                     najednou provést. Pro parametr "m" lze doporučit dvojná-
                     sobek maximálního  počtu  najednou   otevřených  souborů   
                     (např. parametr FILES z CONFIG.SYS)
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t831">instalace sítě</a>               <a href="#t834">sdílení FANDu</a>     <a href="#t835">sdílení úlohy</a>
   <a href="#t836">sdílení datových souborů</a>     <a href="#t843">lock</a>
</pre>

<a name="t834"></a>
## sdílení FANDu

<pre>
                            <b>LAN - SDÍLENÍ FANDU</b>
   ──────────────────────────────────────────────────────────────────────────
   Pro sdílení  FANDu  je třeba  nejprve nastavit DOS - atribut ReadOnly  pro
   soubory <b>FAND.EXE</b> a <b>FAND.OVR</b>.

   Před sdíleným  spuštěním  FANDu  je nutné instalovat následující parametry
   operačního systému (<a href="#t216">SET-parametry</a>):

   ■ <b>SET FANDWORK=</b> Cesta k pracovním souborům
     - každý účastník má vlastní adresář na lokálním počítači nebo na serveru

   Soubory FAND.OVR, FAND.CFG, FAND.RES a FANDHLP.* jsou  standardně  hledány  
   ve stejném adresáři, kde je FAND.EXE. Dalšími parametry operačního systému  
   lze standardní hledání změnit:  <b>SET FANDOVR=</b>,  <b>SET FANDCFG=</b>, <b>SET FANDRES=</b>.
   Programem FANDINST je možné dále nastavit "síťové" parametry:
   (viz. <a href="#t843">používané pojmy</a>)

     <b>net delay</b> - opakování blokovaného přístupu
     <b>refresh</b>   - obnova obrazovky v datovém editoru (zvýšení implicitní 
                 hodnoty 5 sec. zmenší zatížení disku serveru a sítě)
 
  ──────────────────────────────────────────────────────────────────────────
   <a href="#t831">instalace sítě</a>     <a href="#t835">sdílení úlohy</a>     <a href="#t836">sdílení datových souborů</a>
</pre>

<a name="t835"></a>
## sdílení úlohy

<pre>
                            <b>LAN - SDÍLENÍ ÚLOHY</b>
 ──────────────────────────────────────────────────────────────────────────────
 Sdílení úlohy je  možné  pouze  v  režimu  provádění  (nelze  sdíleně  ladit).
 Projektové soubory (<b>*.RDB</b> a <b>*.TTT</b>) a soubory s nápovědou k úloze  (.HLP)  musí
 mít DOS-atribut ReadOnly. Pro úlohu FAND nastavuje tento  atribut  automaticky
 při uzavření na heslo, zruší je při ladění nebo při instalaci (na dobu úprav).

 !!!
 Od verze PC FANDu 4.0 se úlohy v režimu provedení otevírají automaticky
 RdOnly, není potřeba si toto vynucovat označením DOS-atributem.

 Pro práci s katalogem úlohy existují dvě varianty:
 ■ <b>společný katalog</b> - v případě, že úloha do katalogu  nepíše,  stačí  přiřadit
     (ručně) katalogu DOS-atribut  ReadOnly  (při  instalaci  atribut  není  na
     závadu). Katalog může být umístěn ve stejném adresáři jako  sdílená  úloha
     nebo dle nastavení: <b>SET FANDCAT=</b> nebo <b>SET FANDDATA=</b>.
 ■ <b>každý účastník má vlastní katalog</b> - před spuštěním FANDu je  třeba  nastavit
     pro každého účastníka do zvláštního adresáře (na lokálním počítači nebo na
     serveru) parametr:  <b>SET FANDCAT=</b> nebo <b>SET FANDDATA=</b>.

 <b>SET FANDDATA=</b> - určuje umístění katalogu a datových souborů,  které  nejsou  v
     katalogu uvedeny. Implicitně jsou data umístěna v adresáři, kde je úloha.
 <b>SET FANDCAT=</b> - určuje pouze umístění katalogu, uplatní se i při SET FANDDATA
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t831">instalace sítě</a>     <a href="#t834">sdílení FANDu</a>     <a href="#t836">sdílení datových souborů</a>       <a href="#t874">Novell</a>
</pre>

<a name="t836"></a>
## sdílení datových souborů

<pre>
                       <b>LAN - SDÍLENÍ DATOVÝCH SOUBORŮ</b>
   ───────────────────────────────────────────────────────────────────────────

   Sdílení datových souborů je možné pouze při instalovaném síťovém softwaru !
   ┌─────────────────────────────────────────────────────────────────────────┐
   │  Před spuštěním FANDu je nutné instalovat parametr operačního systému:  │
   │  ■ <b>SET LANNODE=</b> Unikátní číslo stanice v síti (0 až 255)                │
   └─────────────────────────────────────────────────────────────────────────┘
   Datové soubory i  <a href="#t641">katalog</a> jsou implicitně umístěny ve stejném adresáři, kde
   je úloha.  Rozmístění datových souborů je možné změnit v katalogu. Umístění
   katalogu a datových souborů v něm neuvedených lze změnit před spuštěním 
   FANDu nastavením <b>SET FANDDATA</b>. Určení přístupových práv viz. <a href="#t831">instalace sítě</a>.


   <b>Sdílené datové soubory</b> je nutné při instalaci úlohy v katalogu všech stanic
                          označit v údaji "Návěští" dle požadavku na sdílení:

   ■ <b>všichni účastníci soubor pouze čtou</b> - (úloha,.HLP) soubor má DOS-atribut
                                           ReadOnly , nemusí  být  v katalogu
                                           uveden nebo je  návěští v katalogu
                                           prázdné.
   ■ <b>někteří účastníci soubor pouze čtou</b> - v katalogu těchto stanic je soubor
                                           označen '<b>#R</b>'
   ■ <b>soubor je určen pro čtení i zápis</b>   - návěští v katalogu obsahuje '<b>#</b>'.

   Deklarace sdílených souborů není možné měnit ! Zápis do souboru určeného 
   pouze pro čtení je vyhodnocen jako běhová chyba s ukončením úlohy !
   ──────────────────────────────────────────────────────────────────────────────
   <a href="#t834">sdílení FANDu</a>      <a href="#t835">sdílení úlohy</a>      <a href="#t837">technika sdílení</a>      <a href="#t863">sdílená editace</a>
</pre>

<a name="t837"></a>
## technika sdílení

<pre>
                           <b>LAN - TECHNIKA SDÍLENÍ</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Blokování souboru</b>    - FAND  realizuje  přístup ke sdíleným souborům zcela
                          automaticky.  Pro umožnění  postupného  přechodu od
                          režimu sdílení souboru k jeho výlučnému (exclusive)  
                          blokování pro jednoho účastníka je použita technika  
                          blokování souboru s řadou módů blokování.
 
   <b>Zamykání vět souboru</b> - technika zamykání vět je použita pro  koordinaci  
                          práce datových editorů.

   Obě uvedené techniky je možné využít také  v procedurách. K dispozici jsou   
   příkazy <b>with shared</b> (blokování souboru) a <b>with locked</b> (zamčení věty).
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t845">technika blokování souboru</a>         <a href="#t843">exclusive</a>         <a href="#t867">with shared</a>
   <a href="#t860">technika zamykání vět</a>                                <a href="#t871">with locked</a>
</pre>

<a name="t843"></a>
## používané pojmy

*Též:* `exclusive`, `net delay`, `lock`, `deadlock`

<pre>
                      <b>LAN - PŘEHLED POUŽÍVANÝCH POJMŮ</b>
   ──────────────────────────────────────────────────────────────────────────

   <b>Exclusive</b>.. výlučné, výhradní otevření souboru jedním účastníkem, ostatní
               účastníci nemají povolen žádný přístup k tomuto souboru.
               POZOR - odlišovat od módu <b>Excl</b>

   <b>Shared</b> .... sdílený přístup k datovému souboru několika účastníky najednou

   <b>Módy blokování souboru</b>.. pro práci se  sdílenými soubory FAND používá řadu
               módů blokování souboru. Tyto módy umožňují postupný  přechod  
               mezi režimem Shared a Exclusive.

   <b>Lock</b> ...... uzamčení, blokování  věty datového  souboru po dobu  provádění
               akce.  Automatické zamykání vět slouží  ke koordinaci datových
               editorů, v proceduře je k dispozici také příkaz <b>with locked</b>

   <b>Deadlock</b>... zablokování, uváznutí  programu  v síti je nežádoucí  situace,
               kdy si  účastníci v síti  navzájem  blokují  přístup k souboru
               (větě). K uváznutí nedochází často, ale nebezpečí jeho výskytu  
               vzrůstá s časem, po který jsou soubory (věty) blokovány. 
               Uváznutí je třeba na jedné za zablokovaných stanic uvolnit 
               klávesami <b>Ctrl-Break</b>.

   <b>Net delay</b>.. perioda pro opakování  blokovaného přístupu (čas v CPU).
               Konstantu lze instalovat programem FANDINST.

   <b>Refresh</b> ... perioda obnovy obrazovky v datovém editoru při sdílené editaci
               (čas v sekund.). Konstantu lze  instalovat programem FANDINST.
               Okamžitou obnovu obrazovky je možné vyvolat ručně <b>CTRL-F2</b>.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t846">módy blokování souboru</a>       <a href="#t860">technika zamykání vět</a>       <a href="#t871">with locked</a>
</pre>

<a name="t845"></a>
## technika blokování souboru

<pre>
                      <b>LAN - TECHNIKA BLOKOVÁNÍ SOUBORU</b>
   ──────────────────────────────────────────────────────────────────────────
   Po otevření sdíleného souboru používá FAND na nezbytně nutnou dobu jeden z
   módů blokování (vyšší zahrnuje vždy i  nižší).
 
   Při sekvenčním zpracování  souboru  (sort, merge, report, indexace, cyklus
   forall, obnovení obrazovky v datovém editoru - <b>refresh</b>) zůstane nárokovaný
   mód blokování pro určitý soubor v platnosti do konce akce.

   Pro práci jednotlivých účastníků je vždy rozhodující nejvyšší mód bloková- 
   ní, který je nárokován ostatními účastníky v okamžiku provedení akce.Platí  
   tedy, že já mohu provádět vše, pokud mi to někdo jiný právě nezakazuje.

   Dočasná nedostupnost souboru je hlášena uživateli.  Pokus o práci s bloko-
   vaným souborem je opakován v pravidelných  intervalech určených parametrem
   <a href="#t843">net delay</a>. Možnost přerušit čekání má uživatel pouze v některých případech  
   při práci datového  editoru ( fáze startu  datového editoru, pokus o změnu
   právě blokované věty,... ), hlášení o blokování  souboru je potom doplněno
   o možnost  přerušení  klávesou  <b>ESC</b>.  Při  přerušení  fáze startu datového
   editoru nastaví <a href="jazyk.md#t550">edbreak</a><b>=15</b>.
   ──────────────────────────────────────────────────────────────────────────────
   <a href="#t846">módy blokování souboru</a>      <a href="#t863">sdílená editace</a>      <a href="#t860">technika zamykání vět</a>
</pre>

<a name="t846"></a>
## módy blokování souboru

<pre>
                        <b>LAN - MÓDY BLOKOVÁNÍ SOUBORU</b>
  ──────────────────────────────────────────────────────────────────────────
   Po otevření sdíleného souboru používá FAND na nezbytně nutnou dobu jeden z
   uvedených módů blokování (vyšší zahrnují vždy i nižší).

   <a href="#t854">Null</a>   - <b>nejnižší mód blokování</b>, bez omezení pro ostatní
   <a href="#t854">NoExcl</a> - zákaz módu Excl pro ostatní, povoluje vše kromě celkové reorganizace
   <a href="#t854">NoDel</a>  - zákaz rušení vět
            ostatní nemohou spustit akci, která vyžaduje blokování Del nebo Excl
   <a href="#t854">NoCr</a>   - zákaz vytváření vět
            ostatní nemohou blokovat Cr, Del nebo Excl
   <a href="#t854">Rd</a>     - čtení na vlastní stanici
            ostatní nemohou blokovat Wr, Cr, Del nebo Excl
   <a href="#t858">Wr</a>     - psaní na vlastní stanici, 
              ostatní nemohou Rd, Wr, Cr, Del nebo Excl
   <a href="#t858">Cr</a>     - vytváření nových vět
            ostatní nemohou NoCr, Rd, Wr, Cr, Del nebo Excl
   <a href="#t304">Del</a>    - rušení vět
            ostatní nemohou NoDel, NoCr, Rd, Wr, Cr, Del, Excl
   <a href="#t858">Excl</a>   - <b>nejvyšší mód blokování</b>, výstup do souboru resp. celková reorganizace
            ostatní účastníci nemohou se souborem pracovat
   ──────────────────────────────────────────────────────────────────────────────
   <a href="#t845">technika blokování souboru</a>    <a href="#t854">módy blokování nižší</a>    <a href="#t848">░ módy blokování souboru</a>
   <a href="#t847">tabulka módů blokování</a>        <a href="#t858">módy blokování vyšší</a>    <a href="#t869">░ test módu blokování</a>
</pre>

<a name="t847"></a>
## tabulka módů blokování

<pre>
                        <b>LAN - TABULKA MÓDŮ BLOKOVÁNÍ</b>
   ──────────────────────────────────────────────────────────────────────────
                 Tabulka obsahuje přehled všech kombinací, které mohou nastat 
                 při současné práci stanic A (řádky) a B (sloupce).
      Módy       ┌────────┬──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
   blokování     │ A \ B  │<b>NoExcl</b>│<b>NoDel</b>│ <b>NoCr</b>│ <b>Rd</b>  │ <b>Wr</b>  │ <b>Cr</b>  │ <b>Del</b> │ <b>Excl</b>│
                 ├────────┼──────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
    <b>nejnižší</b>     │ <a href="#t854">Noexcl</a> │ Ano  │ Ano │ Ano │ Ano │ Ano │ Ano │ Ano │     │
       ↑         │ <a href="#t854">NoDel</a>  │ Ano  │ Ano │ Ano │ Ano │ Ano │ Ano │     │     │
       │         │ <a href="#t854">NoCr</a>   │ Ano  │ Ano │ Ano │ Ano │ Ano │     │     │     │
       │         │ <a href="#t854">Rd</a>     │ Ano  │ Ano │ Ano │ Ano │     │     │     │     │
       │         │ <a href="#t858">Wr</a>     │ Ano  │ Ano │ Ano │     │     │     │     │     │
       │         │ <a href="#t858">Cr</a>     │ Ano  │ Ano │     │     │     │     │     │     │
       ↓         │ <a href="#t304">Del</a>    │ Ano  │     │     │     │     │     │     │     │
    <b>nejvyšší</b>     │ <a href="#t858">Excl</a>   │      │     │     │     │     │     │     │     │
                 └────────┴──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘
   Z popisu módů blokování je zřejmé, že při blokování v  módu Wr nebo vyšším,
   není možné na jiné stanici s tímto souborem pracovat (nelze číst ani psát).

 ░ Stanice A blokuje  soubor NoCr ( např. editace podmnožiny vět souboru bez
 ░ indexů). Stanice B může současně nárokovat blokování téhož souboru v módu
 ░ WR nebo nižším. Nemůže přidávat a rušit věty ani provádět reorganizaci.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t846">módy blokování souboru</a>        <a href="#t869">░ test módu blokování</a>        <a href="#t863">sdílená editace</a>
</pre>

<a name="t848"></a>
## ░ módy blokování souboru

<pre>
                      <b>LAN - PŘÍKLAD BLOKOVÁNÍ SOUBORU</b>
  ────────────────────────────────────────────────────────────────────────────
  ░ Na stanici A je běží cyklus forall, který pro  zjednodušení  pouze čte (!)
  ░ údaje ze souboru DATA. Po dobu provádění cyklu  tato  stanice tedy blokuje
  ░ soubor DATA v módu Rd. NA ostatních stanicích je možné tento soubor sdílet
  ░ s tím,  že není možné do souboru  DATA  zapisovat (Wr), přidávat nové věty
  ░ (CR),  rušit  věty (Del)  a samozřejmě ani  provádět  celkovou aktualizaci
  ░ souboru (Excl).  Případný požadavek  na provedení takové akce  ( například
  ░ přidání věty k souboru) je odmítnut, je vydáno hlášení:
  ░
  ░    <b>"&gt;&gt;&gt; soubor DATA je blokován (Cr), zkouším dále"</b>
  ░
  ░ V závorce je vždy uveden  nárokovaný  mód  blokování,  který byl odmítnut.
  ░ Hlášení  může  být  v  některých  případech  doplněno  o možnost potlačení
  ░ spouštěné akce klávesou  <b>ESC</b>.  Slova 'zkouším dále' signalizují uživateli,
  ░ že pokus o provedení požadované akce bude  opakován za dobu  určenou para-
  ░ metrem <b>net delay</b>.
  ░
  ░ Po ukončení cyklu je blokování souboru DATA v módu Rd stanicí A ukončeno a
  ░ pro další práci s tímto  souborem  je  rozhodující  případný mód blokování
  ░ nárokovaný jiným účastníkem.
  ────────────────────────────────────────────────────────────────────────────
  <a href="#t846">módy blokování souboru</a>     <a href="#t869">░ test módu blokování</a>     <a href="#t843">net delay</a>
</pre>

<a name="t854"></a>
## módy blokování nižší

*Též:* `null`, `noexcl`, `nodel`, `nocr`, `rd`

<pre>
                    <b>LAN - POUŽITÍ NIŽŠÍCH MÓDŮ BLOKOVÁNÍ</b>
  ────────────────────────────────────────────────────────────────────────────
  <b>Null</b>   ■ implicitně při otevření souboru, neklade žádné  omezení pro ostatní
           účastníky

  <b>NoExcl</b> ■ uplatní se pouze pro indexovaný soubor při:
         ■ edit s použitím cond, owner, mode='F2', mode='wx' 
           nebo při editaci podle pracovního indexu (editace podmnožiny)
         ■ editace podle pracovního indexu ... edit(Soubor/PracIndex,...
         ■ report - ve fyzickém pořadí bez automatického třídění
                  - s automatickým tříděním - jen část generování sestavy
                  - třídění vstupu podle pracovního indexu
         ■ vstupní soubor merge ve fyzickém pořadí bez automatického třídění

  <b>NoDel</b>  ■ editace celého souboru (kromě cond, owner, mode='F2' nebo mode='wx')

  <b>NoCr</b>   ■ edit souboru bez indexů při použití cond nebo mode='F2' (podmnožina)
         ■ report souboru bez indexů, bez automatického třídění
         ■ vstupní soubor merge bez indexů, bez automatického třídění

  <b>Rd</b>     ■ datový editor - fáze startu datového editoru 
                         - další čtení vět souboru
                         - obnova obrazovky v datovém editoru (<b>refresh</b>)
                         - referenční integrita, varianta s !!
         ■ blokování nadřízených souborů při čtení viditelných údajů  
         ■ report (ostatní případy)
         ■ vstupní soubor merge (ostatní případy)
         ■ údaje nebo věty ve výrazech kromě přiřazení (recno,link,nrecs,
           nrecsabs,lastupdate,isdeleted,owned)
         ■ readrec
         ■ help
         ■ close (uzavření souboru, kvůli trunc)
         ■ automatické vytváření indexu, příkaz getindex
         ■ cyklus forall (akce v průběhu cyklu mohou blokování zesílit)
         ■ backup (zálohování)
         
  ────────────────────────────────────────────────────────────────────────────
  <a href="#t858">módy blokování vyšší</a>      <a href="#t846">módy blokování souboru</a>       <a href="#t869">░ test módu blokování</a>
</pre>

<a name="t858"></a>
## módy blokování vyšší

*Též:* `wr`, `cr`, `excl`

<pre>
                    <b>LAN - POUŽITÍ VYŠŠÍCH MÓDŮ BLOKOVÁNÍ</b>
  ────────────────────────────────────────────────────────────────────────────
  <b>Wr</b>     ■ zápis změněné věty v datovém editoru
         ■ #A += , provedení aditivní změny bez přidání vět
         ■ změna hodnot klíčů podříz. věty (referenční integrita)
         ■ writerec
         ■ Soubor.údaj:=, Soubor[Věta].údaj:=
         ■ RecordProměnná:= (pokud obsahuje volné texty)

  <b>Cr</b>     ■ zápis nové věty v datovém editoru (pořízení)
         ■ výstup merge s připojením vět (+)
         ■ #A !+=, #A !!+= provedení aditivní změny
         ■ writerec(,0) - přidání věty
         ■ recallrec
         ■ appendrec
         ■ zápis do journalu
  <b>Del</b>    ■ rušení vět editorem ( i podříz. souboru při refer. integritě)
         ■ deleterec
         ■ Soubor.nrecs:=,
         ■ starý formát (FAND verze 2.x) exit=(..proc())
  <b>Excl</b>   ■ merge výstup bez připojení vět
         ■ sort
         ■ indexfile
         ■ restore (obnova zálohovaných souborů)
  ────────────────────────────────────────────────────────────────────────────
  <a href="#t854">módy blokování nižší</a>        <a href="#t869">░ test módu blokování</a>       <a href="#t519">journal</a>
</pre>

<a name="t860"></a>
## technika zamykání vět

<pre>
                        <b>LAN - TECHNIKA ZAMYKÁNÍ VĚT</b>
   ──────────────────────────────────────────────────────────────────────────
   Ke koordinaci  datových editorů  na různých  stanicích  v síti  je použita
   technika zamčení ( <b>lock</b> ) věty. Lock je  vydán  před  první  změnou  údaje
   dané věty a případné  čekání v případě  blokování  věty  jiným  účastníkem
   ( hlášení s <b>ESC</b> ) lze  přerušit a tím  odstoupit od záměru změny.  Lock je
   zrušen po úspěšném  ukončení věty nebo po její obnově <b>Ctrl-U</b>.

   Aby bylo možné vyhnout se dlouhému čekání a případnému  <b>deadlock</b>, není při
   lock zakázáno přepsání věty jiným programem (mimo  datový  editor a příkaz
   "with locked ...").  Před zápisem změněné věty PC FAND zjistí, zda mezitím
   nedošlo  k takovému přepsání a případně  po hlášení  přečte a zobrazí stav
   věty v souboru (anuluje změny provedené v editoru).

   <b>Při přímém přístupu k větě datového souboru  z procedury není zamykání vět</b>
   <b>automaticky  prováděno !</b>
 
   Pro aktualizaci  údajů nebo rušení věty příkazem deleterec v proceduře je
   tedy vhodné použít příkaz <b>with locked</b>. Není-li uvedena  větev  "else ..",
   pak v případě  blokování některé  ze zamykaných vět jiným účastníkem čeká
   FAND na její uvolnění bez možnosti čekání přerušit.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t861">zamčení věty souboru</a>        <a href="#t863">sdílená editace</a>       <a href="#t871">with locked</a>
   <a href="#t843">lock</a>                        <a href="#t843">deadlock</a>
</pre>

<a name="t861"></a>
## zamčení věty souboru

<pre>
                         <b>LAN - ZAMČENÍ VĚTY SOUBORU</b>
  ────────────────────────────────────────────────────────────────────────────
  <b>Lock věty se vydává v těchto situacích:</b>
    ■ v editoru při: - <b>Ins</b> nebo vstup znaku do údaje
                     - duplikaci údaje z předchozí věty <b>F4</b>
                     - nasazení hodnot dle odstavce #D (závislosti)
                     - <b>Ctrl-F4</b> z vnořené editace nebo kalkulátoru
                     - standardním použití <b>Shift-F7</b>
                     - během exit=(..NázevProcedury..)
    ■ po dobu provádění příkazu <b>with locked</b>

  <b>Lock věty je zrušen v situacích:</b>
    ■ v editoru při: - opuštění editoru s výjimkou exit=(..NázevProcedury..)
                     - úspěšném přechodu na další větu
                     - obnovení původního obsahu věty <b>Ctrl-U</b>
                     - návratu z exit=(..NázevProcedury..), pokud v proceduře
                       nedošlo ke změně editované věty
                     - po návratu z nezměněného volného textu v případě, že
                       nebyl dosud změněn žádný údaj věty
                     - po <b>ESC</b> ze změny údaje (mimo volný text), nebyla-li věta
                       již předtím změněna
    ■ po ukončení příkazu <b>with locked</b>
    <b>■ jako vedlejší efekt příkazu <a href="procedury.md#t626">exec</a></b> - POZOR ! zámek věty se neobnoví ani
                                         po ukončení EXEC
  ────────────────────────────────────────────────────────────────────────────
  <a href="#t860">technika zamykání vět</a>          <a href="#t871">with locked</a>
</pre>

<a name="t863"></a>
## sdílená editace

<pre>
                           <b>LAN - SDÍLENÁ EDITACE</b>
  ────────────────────────────────────────────────────────────────────────────
  Při práci se sdíleným souborem lze dělat  vše  (editovat,  pořizovat a rušit
  věty), pokud to některá  z dalších stanic nezakazuje!  Při konkurenční práci
  více účastníků je pro  každého  z  nich  rozhodující  nejvyšší mód blokování
  nárokovaný ostatními účastníky. Pro koordinaci  datových  editorů je použita
  <b>technika zamykání vět</b>.

  Mód blokování při editaci  je  jednoznačně  určen  způsobem  editace a právě
  probíhající akcí:
  <b>NoExcl</b> - editace podmnožiny vět souboru s  indexovou  podporou (cond, owner,
           mode='F2' nebo mode='wx')
  <b>NoDel</b>  - editace celého souboru (kromě cond, owner, mode='F2' nebo mode='wx')
  <b>NoCr</b>   - editace podmnožiny vět souboru bez indexů (cond nebo mode='F2')
  Použití módů Rd a vyšších je časově omezeno na provedení konkrétní akce 
  (čtení souboru, zápis věty, přidání věty, aditivní změna atd.).

  Při editaci podmnožiny vět (NoExcl, NoCr) pravidelný  refresh vypisuje pouze
  aktuální stav vět vybraných  při  vytváření  podmnožiny.  Věty zrušené jiným
  účastníkem jsou při refresh zobrazeny barevně odlišně, nově pořízené věty se
  nezobrazí a při změně hodnoty klíče je hledání <b>F3</b> ještě podle starých hodnot
  klíče. Obnovení podmnožiny lze vyvolat kombinací kláves <b>Ctrl-F2</b>.

  Jsou-li deklarovány rozsáhlejší aditivní změny a především referenční 
  integrita, blokují se při potvrzení věty v odpovídajících módech blokování 
  všechny nadřízené, resp. podřízené soubory. Celá akce tvoří jakousi transakci,
  která se provede buďto celá nebo vůbec. V případě nepřístupnosti souborů
  ji lze odvolat (ESC).
  ────────────────────────────────────────────────────────────────────────────
  <a href="#t860">technika zamykání vět</a>     <a href="#t846">módy blokování souboru</a>     <a href="#t864">tabulka sdílené editace</a>
</pre>

<a name="t864"></a>
## tabulka sdílené editace

<pre>
                       <b>LAN - TABULKA SDÍLENÉ EDITACE</b>
   ──────────────────────────────────────────────────────────────────────────
   V tabulce  jsou zachyceny  základní kombinace  módů  blokování při sdílené
   editaci souboru  dvěma stanicemi.  Kombinace  NoExcl - NoCr,  která nemůže
   nastat  (soubor buď indexy má nebo ne) není uvedena.
   ┌───────────────────────┬────────────────────────────────┐
   │    Mód blokování      │      Možnost provádění         │ Legenda:
   ├───────────┬───────────┼────────────────┬───────────────┤
   │           │           │   Stanice A    │   Stanice B   │ Edit-aktualizace
   │ Stanice A │ Stanice B ├────────────────┼───────────────┤  ^N -pořízení
   │           │           │ Edit  ^N   ^Y  │ Edit  ^N   ^Y │  ^Y -rušení vět
   ├───────────┼───────────┼────────────────┼───────────────┤
   │  <a href="#t854">NoExcl</a>   │  NoExcl   │  Ano  Ano  Ano │  Ano  Ano  Ano│
   │  NoExcl   │  NoDel    │  Ano  Ano      │  Ano  Ano  Ano│
   │  <a href="#t854">NoDel</a>    │  NoDel    │  Ano  Ano      │  Ano  Ano     │
   │  NoDel    │  NoCr     │  Ano           │  Ano  Ano     │
   │  <a href="#t854">NoCr</a>     │  NoCr     │  Ano           │  Ano          │
   └───────────┴───────────┴────────────────┴───────────────┘
   Při pokusu o provedení právě blokované akce  vydá FAND hlášení o blokování
   souboru. Hlášení obsahuje možnost potlačení požadované akce klávesou (ESC).
   Pro snížení zátěže sítě je vhodné v příkazu edit použít parametr <a href="#t519">watch</a>.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t863">sdílená editace</a>            <a href="#t846">módy blokování souboru</a>
</pre>

<a name="t867"></a>
## with shared

*Též:* `shared`

<pre>
                          <b>LAN - PŘÍKAZ WITH SHARED</b>
  ────────────────────────────────────────────────────────────────────────────
  V některých případech může být žádoucí  změnit automatické  chování PC FANDu
  při blokování sdílených souborů. V proceduře je pro  určení režimu blokování
  jednoho nebo více souborů k dispozici příkaz:
  
  █ <b>with shared</b> NázevSouboru(MódSdílení) {,NázevSouboru(MódSdílení)} <b>do</b>
  █   Příkaz1
  █ [ <b>else</b> Příkaz2 ]<b>;</b>

  Mód sdílení je textová konstanta, určující způsob práce se souborem a může
  nabývat hodnot: <a href="#t854">NoExcl</a>, <a href="#t854">NoDel</a>, <a href="#t854">NoCr</a>, <a href="#t854">Rd</a>, <a href="#t858">Wr</a>, <a href="#t858">Cr</a>, <a href="#t304">Del</a>, <a href="#t858">Excl</a>.

  Po dobu provedení "Příkaz1" je blokován  soubor  (případně  více souborů) ve
  zvoleném režimu blokování. Přístup k blokovaným souborům jiným účastníkem je
  povolen jen v případě, že nárokuje mód blokování, který je možné s aktuálním
  blokováním kombinovat. V opačném případě je přístup k  souboru odmítnut. Při
  požadavku na blokování více souborů najednou  blokuje  všechny současně nebo
  žádný. Není-li  uvedena  větev  "else  ..",  čeká  FAND  v případě blokování
  některého z uvedených souborů jiným účastníkem na jeho uvolnění bez možnosti
  čekání přerušit. Periodu opakování přístupu je možné instalovat (<b>net delay</b>).
  ────────────────────────────────────────────────────────────────────────────
  <a href="#t845">technika blokování souboru</a>        <a href="#t843">net delay</a>        <a href="#t868">░ with shared</a>
 <a href="#t869">░ test módu blokování</a>
</pre>

<a name="t868"></a>
## ░ with shared

<pre>
                     <b>LAN - PŘÍKLAD POUŽITÍ WITH SHARED</b>
  ────────────────────────────────────────────────────────────────────────────
  ░ Příklad aktualizace síťového souboru v proceduře:
  ░
  ░ a) automatické blokování:
  ░
  ░    forall i in DATA do
  ░        DATA[i].Datum:=today;
  ░
  ░    Cyklus forall navodí blokování <b>Rd</b>, ale přímým přiřazením do tohoto
  ░ souboru je blokování zesíleno na mód <b>Wr</b>.
  ░
  ░ b) celý soubor je blokován Exclusive po celou dobu cyklu:
  ░
  ░    with shared DATA (Excl) do
  ░      forall i in DATA do
  ░        DATA[i].Datum:=today
  ░    else
  ░      message('Soubor DATA používá jiný účastník');
  ░
  ░    Akce proběhne jen pokud není soubor blokován jiným účastníkem. Po celou
  ░ dobu akce je jakýkoliv přístup k souboru pro ostatní účastníky blokován.
  ────────────────────────────────────────────────────────────────────────────
  <a href="#t867">with shared</a>         <a href="#t845">technika blokování souboru</a>       <a href="#t843">exclusive</a>
</pre>

<a name="t869"></a>
## ░ test módu blokování

<pre>
                     <b>LAN - TEST MÓDU BLOKOVÁNÍ SOUBORU</b>
  ────────────────────────────────────────────────────────────────────────────
  ░ Opakovaným použitím příkazu <b>with shared</b> je možné zjistit nejvyšší aktuálně
  ░ možný mód blokování a z něj podle tabulky  odvodit  aktivní mód blokování,
  ░ nárokovaný ostatními účastníky:
  ░
  ░ begin
  ░   write('SOUBOR je blokován v módu: ');
  ░   with shared SOUBOR (Excl)   do writeln('Null / bez omezení') else
  ░   with shared SOUBOR (Del)    do writeln('NoExcl') else
  ░   with shared SOUBOR (Cr)     do writeln('NoDel ') else
  ░   with shared SOUBOR (Wr)     do writeln('NoCr  ') else
  ░   with shared SOUBOR (Rd)     do writeln('Rd    ') else
  ░   with shared SOUBOR (NoCr)   do writeln('Wr    ') else
  ░   with shared SOUBOR (NoDel)  do writeln('Cr    ') else
  ░   with shared SOUBOR (NoExcl) do writeln('Del   ') else
  ░                                  writeln('Excl / nelze sdílet');
  ░ end;
  ────────────────────────────────────────────────────────────────────────────
  <a href="#t867">with shared</a>     <a href="#t847">tabulka módů blokování</a>     <a href="#t848">░ módy blokování souboru</a>
</pre>

<a name="t871"></a>
## with locked

*Též:* `locked`

<pre>
                          <b>LAN - PŘÍKAZ WITH LOCKED</b>
  ────────────────────────────────────────────────────────────────────────────
  Při přímém přístupu k větě datového souboru  z  procedury  není zamykání vět
  automaticky prováděno !  Pro  aktualizaci  údajů  nebo  rušení věty příkazem
  deleterec  v proceduře je tedy vhodné  použít  pro  koordinaci  práce v síti
  příkaz pro blokování jedné nebo více vět datového souboru:

  █ <b>with locked</b> NázevSouboru[VýrazČíslaVěty] {,NázevSouboru[VýrazČíslaVěty]} <b>do</b>
  █   Příkaz1
  █ [ <b>else</b> Příkaz2 ]<b>;</b>

  Po dobu provedení "Příkaz1" je zamčena věta (několik vět)  datového souboru.
  Mód  blokování  souboru  je  určen  automaticky  samostatně  pro každou akci
  prováděnou v "Příkaz1" nebo je možné jej stanovit příkazem <b>with shared</b>.
  
  Pokus o aktualizaci zamčené věty  jiným  účastníkem  (z  datového editoru, z
  procedury s  uvedením  "with  locked  ...")  je  odmítnut.  Při požadavku na
  zamčení více vět najednou zamkne všechny věty současně  nebo žádnou. Není-li
  uvedena  větev  "else ..",  pak v případě blokování  některé z uvedených vět
  jiným účastníkem čeká FAND na  její uvolnění  bez možnosti přerušení  čekání.
  Periodu opakování přístupu je možné instalovat (net delay).
  ────────────────────────────────────────────────────────────────────────────
  <a href="#t860">technika zamykání vět</a>         <a href="#t846">módy blokování souboru</a>       <a href="#t867">with shared</a>
  <a href="#t843">net delay</a>                     <a href="#t872">░ with locked</a>
</pre>

<a name="t872"></a>
## ░ with locked

<pre>
                     <b>LAN - PŘÍKLAD POUŽITÍ WITH LOCKED</b>
   ──────────────────────────────────────────────────────────────────────────
   ░ Příklad aktualizace věty síťového souboru v proceduře:
   ░
   ░    with locked DATA[Číslo] do
   ░      begin
   ░        DATA[Číslo].Jmeno:=Jmeno;
   ░        DATA[Číslo].Ulice:=Ulice;
   ░        DATA[Číslo].Misto:=Misto;
   ░      end;
   ░
   ░ Příklad zrušení věty síťového souboru v proceduře:
   ░
   ░    with locked SOUBOR[ČísloVěty] do
   ░      deleterec(SOUBOR,ČísloVěty);
   ░
   ░ Uvedená konstrukce zabrání případné kolizi s editací věty v datovém
   ░ editoru. Pokud je věta právě blokována jiným účastníkem, bude PC FAND
   ░ čekat na její uvolnění (chybí větev "else ...") a to bez možnosti
   ░ přerušení čekání.
   ░
   ░ Po dobu přiřazení do údajů souboru DATA bude soubor blokován v módu <b>Wr</b>.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t871">with locked</a>       <a href="#t860">technika zamykání vět</a>       <a href="#t846">módy blokování souboru</a>
</pre>

<a name="t873"></a>
## Lantastic

<pre>
                              <b>LANTASTIC</b>
   ──────────────────────────────────────────────────────────────────────────
   V praxi je provozováno několik verzí této sítě. Nejsou známy nějaké speci-
   fické problémy při instalaci PC FANDu nebo úloh. Nižší verze LANTASICu 
   používají externí (DOSový) program <a href="#t833">SHARE</a>.

   Verze LANTASTICu 6.0 může alternativně použít i interní SHARE:
   Spustíme program <b>NET_MGR</b>, nabídky <b>Server startup parameters</b> (rolovat dolů)
                                     <b>Internal share</b>

   volby: <b>Internal share</b> ... enabled/disabled (povolen/vypnut interní share)
          <b>Share locks</b> ...... maximální počet zámků (viz. SHARE /L: )
          <b>Name space</b> ....... velikost tabulky otevřených souborů (SHARE /F:)
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t874"></a>
## Novell

<pre>
                                 <b>NOVELL</b>
   ──────────────────────────────────────────────────────────────────────────

   Kromě sítě NetWareLite a Personal NetWare není třeba používat program 
   SHARE.EXE.

   Většinou je nutno upravit konfigurační soubor stanice <b>NET.CFG</b> (ve starších
   verzích ovladačů SHELL.CFG)

   ■ parametr <b>FILE HANDLES</b>= počet souborů, které se otevírají ze serveru.
                            Platí určité kontroly vzhledem k parametru FILES
                            v CONFIG.SYS (viz. dokumentace Novellu).
                            Základní kontrola se provádí takto:
                            <b>file handles +    files    &lt;   255</b>
                             net.cfg        config.sys

   ■ parametr <b>SHOW DOTS</b>=ON  Při prohledávání adresářů v okně se objeví
                            i odkaz na nadřízený adresář, t.j. dvě tečky.

   ■ parametr <b>READ ONLY COMPATIBILITY</b>=OFF
                            nutný pro sdílení RdOnly souborů - např. úloh
                            při použití VLM-ovladačů. Ve verzi PC FANDu 4.0
                            je sdílení úloh již řešeno interně ve FANDu.
                            Tento parametr je v dokumentaci popsán dosti 
                            neurčitě a také v praxi někdy "zabralo" nastavení
                            na ON.

   ■ Při vyšším zatížení sítě, což může být kromě výkonu sítě způsobeno 
     i nevhodným způsobem programování úlohy, může dojít k nadměrnému 
     blokování souborů, zdánlivě  neoprávněnému. Důvodem může být "zahlcení"
     algoritmů pro blokování souborů na systémové úrovni. Často lze chování
     sítě zlepšit nastavením těchto dvou parametrů v NET.CFG (podle dokumentace
     pro Novell 4.x):

     <b>Lock Delay</b>=CPU ... prodleva při pokusu o uzamčení oblasti souboru
                        Povolené hodnoty 0 .. 255, implicitně 1.
     <b>Lock Retries</b>=počet opakování pokusů

   -----------
   Pro přesměrování tiskárny se používá utilita <b>CAPTURE</b>:

   CAPTURE Q=Fronta NB NT NOFORMFEED TI=5 Local=1

   Parametry:
     Q=Fronta          označení síťové fronty
     NB                nebude tištěna hlavička
     NT                není prováděna konverze tabelátoru na mezery
     NOFORMFEED        potlačí odstránkování na konci tisku
     TI=5              tisková úloha vytištěna po 5 s
     Local=1           číslo portu (LPT1)
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t873">Lantastic</a>
</pre>
