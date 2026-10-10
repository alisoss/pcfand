# Hesla

<a name="t18"></a>
## FM0008_2

*Též:* `seznam uživatelů`

<pre>
{ Seznam oprávněných uživatelů a přístupová práva k úloze}
                       <b>SEZNAM UŽIVATELŮ - kapitola U</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Seznam uživatelů</b>  je text  obsahující  seznam oprávněných uživatelů úlohy.
   Každý uživatel má přiřazeno <b>jméno</b> ( text ), <b>kód</b> ( číslo ), <b>heslo</b> a  seznam
   <b>přístupových práv</b>.

   █ '<b>Jméno</b>',<b>Kód</b>,<b>Heslo</b>,(<b>SeznamPřístupovýchPráv</b>); ... jedna položka seznamu
   ■ <b>Jméno</b>:  jméno aktuálního uživatele pro identifikaci v projektu
   ■ <b>Kód</b>:    číslo aktuálního uživatele pro identifikaci v projektu
   ■ <b>Heslo</b>:  tímto heslem se uživatel přihlašuje při startu úlohy
   ■ <b>Seznam</b>: seznam čísel (1..255) pro rozlišení přístupových práv v projektu
             čísla v seznamu jsou oddělena čárkou, mohou být i intervaly

   Pokud je úloha  opatřena kapitolou U , před vstupem do úlohy musí uživatel
   zadat jedno z povolených hesel, jinak úloha  nebude spuštěna.  Po správném
   zadání hesla se naplní <b>jméno</b>, <b>kód</b> a <b>přístupová práva</b>  aktuálního uživatele
   podle nichž lze měnit chování programu v dalším zpracování. Podle kapitoly
   U se řídí i omezení při <b>navigaci po databázi</b> podle  <b>uživatelských pohledů</b>.
   <b>Prázdná kapitola U</b>  má stejný  efekt, jako když  kapitola neexistuje ( lze
   dodatečně instalovat z hlavního menu PC FANDu bez zásahu do programu).
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t19">░ seznam uživatelů</a>
</pre>

<a name="t19"></a>
## ░ seznam uživatelů

<pre>
                         <b>PŘÍKLAD - SEZNAM UŽIVATELŮ</b>
   ──────────────────────────────────────────────────────────────────────────

   'Vladimír Grosmut',1,'VG',(1,3...5);                  {obsah kapitoly U}
   'Luděk Galbavý',2,'LG',(1,3);
   'Iva Jablonská',3,'IJ',(10);

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t54"></a>
## klávesnice

<pre>
                                 <b>KLÁVESNICE</b>
   ──────────────────────────────────────────────────────────────────────────
   V datovém a textovém  editoru  vyvolá  kombinace kláves <b>Alt-F8</b> nabídku pro
   přepínání mezi různým rozložením znaků na klávesnici:

   ■ <a href="kontextova-napoveda.md#t56">klávesnice standardní</a> .......... bez předefinování klávesnice
   ■ <a href="kontextova-napoveda.md#t58">klávesnice česká</a> ............... jako český psací stroj
   ■ <a href="kontextova-napoveda.md#t60">klávesnice česká amatérská</a> ..... mění pouze horní řadu kláves
                                      (malá písmena pod čísly, čárka/háček
                                      a některé úpravy pro slovenčinu)
   ■ <a href="kontextova-napoveda.md#t62">klávesnice slovenská</a> ........... jako slovenský psací stroj
   ■ <a href="kontextova-napoveda.md#t64">klávesnice německá amatérská</a> ... německé rozložení

   Lze instalovat, která klávesnice bude aktivní po startu PC FANDu.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t219">Instalace konstant</a>
</pre>

<a name="t197"></a>
## komunikace

<pre>
                                 <b>KOMUNIKACE</b>
     ────────────────────────────────────────────────────────────────────
     Komunikace v uživatelském a programátorském  režimu je téměř totožná
     což vyplývá především z toho faktu, že program v PC FANDu je vlastně
     speciálním případem datového souboru. Základem programátorského pro-
     středí je datový editor, který je doplněn speciálními prostředky pro
     ladění úloh.

        <b>Základní prvky komunikace:</b>

        <a href="#t200">volba z nabídky</a> .................... pravidla pro práci z menu
        <a href="#t203">hlavní menu PC FANDu</a> ........................ základní nabídka
        <a href="#t201">výběr ze seznamu</a> .......... pravidla pro výběr z množiny prvků
        <a href="#t241">datový editor</a> ................................... práce s daty
        <a href="#t225">textový editor</a> ................................. práce s texty
        <a href="#t252">výrazy</a> ................................ pravidla tvorby výrazů
        <a href="#t54">klávesnice</a> ....................... podpora národního prostředí
        <a href="#t198">myš</a> .................................. podpora myši v PC FANDu
     ────────────────────────────────────────────────────────────────────
</pre>

<a name="t198"></a>
## myš

<pre>
                                    <b>MYŠ</b>
   ──────────────────────────────────────────────────────────────────────────
                                                          <b>┌─────┬─────┬─────┐</b>
          Podpora myši funguje jen pro EGA/VGA            <b>│</b> cli<b> │     │ </b> E<b>  │</b>
       V zásadě dodržuje běžně používané konvence         <b>│</b> ck <b> │     │ </b> S<b>  │</b>
   Funkce myši odpovídají kontextu a poloze kurzoru       <b>│     │     │ </b> C<b>  │</b>
   Význam pravé a levé klávesy lze instalací prohodit     <b>├─────┴─────┴─────┤</b>
                                                          <b>│                 │</b>
   Funkce myši dle kontextu a polohy kurzoru:             <b>│                 │</b>
                                                          <b>│                 │</b>
   ■ <b>menu</b> ... stisk na volbě = vybrat                     <b>│                 │</b>
              stisk na posledním řádku = nápověda         <b>│                 │</b>
                                                          <b>└────────┬────────┘</b>
   ■ <b>dotaz na text. řetězec</b> ... (prompt,název souboru,         <b>┌───┘</b>
       editace údaje): stisk na vymezeném poli = Enter         <b>└─┐</b>

   ■ <b>výběr ze seznamu</b> ..... (názvy souborů, údaje pro třídění atd.):
       stisk na názvu ....  při výběru jednoho názvu = přesun kurzoru a Enter
                            při výběru množiny = přesun kurzoru
                                                 a přepnutí označit/neoznačit
     dvojitý stisk mimo název při výběru množiny: Enter
     držet a přesunout: výměna položek
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t199">myš v datovém a textovém editoru</a>                        <a href="#t219">instalace konstant</a>
</pre>

<a name="t199"></a>
## myš v datovém a textovém editoru

<pre>
                      <b>MYŠ V DATOVÉM A TEXTOVÉM EDITORU</b>
   ──────────────────────────────────────────────────────────────────────────

    ■ <b>datový editor</b> ... stisk na údaji: kurzor přejde k údaji
                        dvojitý stisk na údaji: <b>INS</b> (přechod k editaci údaje)
                                                nebo <b>ENTER</b> (definuje programátor)
                        click na nápovědě(24. nebo 25. řádek) : jako Ctrl-F1

    ■ <b>textový editor</b> ...stisk - přenesení kurzoru na danou pozici

    ■ <b>nápověda kláves</b> ... v posledním řádku obrazovky může být buď standardní
               nebo alternativní ( standardní nebo definovaná programátorem)
               Stisk na zdůrazněném názvu klávesy generuje tuto klávesu
               podržení generuje opakování. V "ikoně" pro myš nemusí být
               uveden název kontrolní klávesy. Např. v <b>Ctrl</b> nápovědě generuje
               ikona <b>F8</b> kombinaci CtrlF8.
               Analogicky funguje při chybovém hlášení také stisk na 'F10'
               resp. 'ShiftF7'

    ■ <b>helpy</b>... stisk na hesle: jako Enter na hesle, další v nápovědě 25.řádek

    ─────────────────────────────────────────────────────────────────────────
</pre>

<a name="t200"></a>
## volba z nabídky

<pre>
                              <b>VOLBA Z NABÍDKY</b>
   ───────────────────────────────────────────────────────────────────────────
   Volbou z <b>nabídky</b> (<b>menu</b>) odstartujete odpovídající příkazy. Nabídka může být
   sloupcová (seznam voleb pod sebou) nebo řádková (vedle sebe).

   ■ <b>šipky</b> ................ pohyb po volbách nabídky
   ■ <b>Home</b> (resp.<b>End</b>) ...... skok na první (poslední) volbu v nabídce
   ■ <b>Enter</b> ................ provede aktuální volbu
   ■ zvýrazněné <b>písmeno</b> ... okamžitě provede odpovídající volbu
   ■ <b>Esc</b> ... návrat o jednu úroveň zpět ve strukturovaných nabídkách
             (může být programátorem zakázáno)
   ■ <b>F1</b> .... nápověda k aktuální volbě
             (pokud je volba opatřena nápovědou v posledním řádku)
   ───────────────────────────────────────────────────────────────────────────
   <a href="#t198">myš</a>
</pre>

<a name="t201"></a>
## výběr ze seznamu

<pre>
                              <b>VÝBĚR ZE SEZNAMU</b>
   ──────────────────────────────────────────────────────────────────────────
   Vybíráte několik prvků z množiny (např. soubory pro editaci, údaje ze sou-
   boru pro třídění). Prvky množiny jsou zobrazeny v okně uprostřed obrazovky.
   Vybrané prvky postupně označíte (<b>F2</b>) a výběr potvrdíte klávesou <b>Enter</b>.

   ■ <b>šipky</b> .................. pohyb po prvcích množiny
   ■ <b>Home</b> (resp.<b>End</b>) ........ skok na první (poslední) prvek množiny
   ■ <b>PageUp</b> (resp.<b>PageDn</b>) ... skok o stranu nahoru (dolů)
                              pokud seznam v okně roluje
   ■ <b>F2</b> ........ vybere a označí aktuální prvek ( znakem <b>&lt;</b> )
   ■ <b>F3</b> ........ zruší označení aktuálního prvku
   ■ <b>Ctrl-F2</b> ... označí všechny prvky
   ■ <b>Ctrl-F3</b> ... zruší označení všech prvků
   ■ <b>&gt;</b> ......... (pouze u <b>třídění</b>) vybere prvek pro <b>sestupné</b> třídění
                 podle údajů vybraných <b>F2</b> se bude třídit <b>vzestupně</b>
   ■ <b>F9</b> ........ zapne/vypne přemisťování prvku v seznamu pomocí <b>šipek</b>
   ■ <b>Enter</b> ..... potvrdí nastavený výběr a spustí akci
   ■ <b>Esc</b> ....... zruší celou akci

   Okamžitý <b>Enter</b> bez označování prvků klávesou <b>F2</b> provede výběr dle kontextu: 
   - při výběru údajů pro editaci nebo sestavu vybere jen uložené údaje,
   - výběr řídících nebo třídících údajů se vynechá.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t198">myš</a>
</pre>

<a name="t202"></a>
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


   Během prohlížení nápovědy lze použít klávesy:

   ■ <b>šipky</b> ............ pohyb po zvýrazněných heslech (tématech)
   ■ <b>Enter</b> ............ přechod k další obrazovce popisující zvýrazněné heslo
   ■ <b>F10</b> .............. návrat k předchozí obrazovce
   ■ <b>F1</b> ............... přechod k první obrazovce nápovědy (help index)
   ■ <b>F6</b> ............... tisk aktuálního textu helpu
   ■ <b>Esc</b> .............. ukončení nápovědy a návrat do programu
   ■ <b>PageUp</b>, <b>PageDn</b> ... listování v několikastránkových nápovědách
   ■ <b>CtrlHome</b>,<b>CtrlEnd</b> . lineární pohyb po helpu


   <b>MYŠ:</b> Pomocí myši se pohybujeme po helpu analogicky jako v textovém editoru.
        <b>Click</b> ( levou klávesou ) na hesle vyvolá toto heslo. Ostatní akce lze
        vyvolat pomocí ikon na 25.řádku.
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t203"></a>
## hlavní menu PC FANDu

<pre>
                            <b>HLAVNÍ MENU PC FANDu</b>
   ──────────────────────────────────────────────────────────────────────────
   Nabídka obsahuje následující volby:

   ■ <b>P</b>rovést úlohu ..... spuštění úlohy v <b>uživatelském režimu</b>
   ■ <b>I</b>nstalace úlohy ... změna <b>katalogu</b> souborů a seznamu oprávněných <b>uživatelů</b>
   ■ <b>E</b>ditace textu ..... vyvolání textového editoru pro textové soubory na disku
   ■ <b>M</b>sDos ............. odskok do operačního systému (návrat příkazem <b>exit</b>)
   ■ <b>K</b>onec ............. ukončení PC FANDu (též <b>Esc</b>)
 
   Spuštěním PC FANDu s parametrem se hlavní menu nezobrazí a spustí se rovnou
   požadovaná akce:

   ■ FAND <b>NázevÚlohy</b> ..... spuštění úlohy v uživatelském režimu
   ■ FAND NázevTextu <b>T</b> ... textový editor pro zadaný textový soubor
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t250">katalog</a>           <a href="#t18">seznam uživatelů</a>            <a href="#t225">textový editor</a>
</pre>

<a name="t212"></a>
## SET-parametry

*Též:* `fandcat`, `fandcfg`, `fanddata`, `fandovr`, `fandovrb`, `fandres`, `fandwork`, `lannode`

<pre>
                               <b>SET-parametry</b>
    ────────────────────────────────────────────────────────────────────────
    Tyto parametry interpretuje  PC FAND  při startu  programu a zadávají se
    do oblasti systémových proměnných (environment) příkazem
                       <b>SET PARAMETR=hodnota</b>.
    Všechny paranetry jsou nepovinné (při jednouživatelském režimu) a nejsou
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
    ■ <b>FANDCAT</b> .... adresář pro <a href="#t250">katalog</a> úlohy (má přednost před FANDDATA)
    ■ <b>FANDOVRB</b> ... zvětšení operační paměti pro úlohu, zadá se počet Kb, 
                   které má použít PC FAND pro oblast overlay místo optimální
                   velikosti (124 Kb). Důsledkem je ovšem zpomalení práce
                   PC FANDu (zvláště volání exit-procedur).
                   Přípustné hodnoty parametru jsou v intervalu 80..134 Kb.
    
    ────────────────────────────────────────────────────────────────────────
    <a href="#t321">lokální sítě</a>
</pre>

<a name="t215"></a>
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
      Pro jejich nastavení lze použít <a href="#t216">instalační program</a> <b>FANDINST.EXE</b>.

   <b>■</b>  Prostředky oper.systému a jeho konfigurace ovlivňují činnost programů
      obecně. Sem patří:

   <b>-</b>  <b>Parametr FILES=</b> systémového souboru <b>CONFIG.SYS</b>
      kterým je nutno zajistit dostatek místa pro najednou otevřené soubory
      konkrétní úlohy. Při nedostatečném nastavení tohoto parametru není úloha
      provozovatelná a havaruje s chybovým hlášením "<b>příliš mnoho otevřených
      souborů ...</b>." . Toto je v běžných podmínkách jediný podmiňující 
      parametr pro běh úloh. Další parametry mají význam především pro
      optimalizaci (většinou urychlení) práce.

   <b>-</b>  Proměnné prostředí MS-DOSu, tzv. <a href="#t212">SET-parametry</a>

   <b>-</b>  <b>EMS paměť</b> lze použít pro umístění overlay modulu (FAND.OVR), což
      se děje automaticky, pokud je dostatek volné EMS paměti.  
      Dle praktických zkušeností to nemá význam pro urychlení práce. Výhodnější 
      je použít volnou paměť jako XMS pro cache či pro umístění rezidentních
      programů v horní paměti (viz. EMM386, loadhigh)

   <b>-</b>  <b>XMS paměť</b> (driver HIMEN.SYS v CONFIG.SYS) je možno s výhodou použít
      pro cache programy (SMARTDRIVE). Pozor - PC FAND obsahuje vlastní cache
      která standardně alokuje 200kB XMS-paměti (viz.<a href="#t219">instalace konstant</a>).
      <b>!!!</b> doporučujeme <b>ne</b>kombinovat FAND-cache s jinou (běžně SMARTDRIVE).

   ──────────────────────────────────────────────────────────────────────────────
   <a href="#t321">lokální sítě</a>     <a href="#t250">katalog</a>
</pre>

<a name="t216"></a>
## instalační program

<pre>
                        <b>INSTALAČNÍ PROGRAM FANDINST</b>
 ──────────────────────────────────────────────────────────────────────────────
 Program <b>FANDINST.EXE</b> naplňuje soubor instalačních parametrů <b>FAND.CFG</b>, který se
 zohledňuje  během  startu  PC  FANDu  při  naplňování  globálních  proměnných.
 Instalace je rozdělena do několika bloků:

 Program zpracuje soubor FAND.CFG dle nastavení SET parametru FANDCFG. Pokud
 není, hledá se v aktuálním adresáři.

 ■ <b>Barvy</b> ....... barvy pro datový a textový editor, nápovědu, nabídky atd.
 ■ <b>Tiskárna</b> .... kódy pro přepínání druhů písma
                 a ostatní escape sekvence pro tiskárnu
                 lze instalovat až 10 tiskáren
 ■ <b>Konstanty</b> ... hodnoty všech typů ovlivňující chování PC FANDu
 ■ <b>Monitor</b> ..... adresa videopaměti, tvar kurzoru, ošetření sněžení
 ■ <b>Abeceda</b> ..... výběr národní abecedy pro lexikální třídění 
                 připravené tabulky <b>Kamenických</b> a <b>Latin2</b>
 ■ <b>Dny</b> ......... tabulka výjimečných dnů v roce (svátky,...) 
                 pro zabudovaný kalendář
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t217">instalace barev</a>           <a href="#t218">instalace tiskárny</a>          <a href="#t219">instalace konstant</a>
 <a href="#t221">instalace abecedy</a>         <a href="#t222">instalace kalendáře</a>         <a href="#t220">instalace monitoru</a>
</pre>

<a name="t217"></a>
## instalace barev

<pre>
 Ctrl-Home                     <b>INSTALACE BAREV</b>
 ──────────────────────────────────────────────────────────────────────────────
 Každá barva se zadává hexadecimálním číslem atributu podle nápovědy.

 ■ <b>uživatelská barva č.0-15</b>.. uživatelské barvy pro individuální použití
                              definované autorem aplikace
 ■ <b>MENU normální text</b> ....... jednotlivé (neaktivní) volby nabídky
 ■ <b>     aktivní volba</b> ....... zvýrazněná volba, na které je nastaven kurzor
 ■ <b>     první písmeno</b> ....... zvýrazněné písmeno pro rychlou volbu
 ■ <b>     nefunkční volba</b> ..... zobrazená ale nefunkční volba

 ■ <b>VÝBĚR normální text</b> ...... výběr z prvků seznamu
 ■ <b>      aktivní položka</b> .... zvýrazněná položka, na které je nastaven kurzor
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
 ■ <b>           výběr(F8,ShiftF8)</b> podmnožina dle pracovního indexu, manuální
                                výběr a rušení uvedenými klávesami

 ■ <b>UŽIVATELSKÁ OBRAZOVKA</b> ...... základní atribut pro write apod.

 ■ <b>NÁPOVĚDA text</b> .............. běžný text nápovědy
 ■ <b>         vybrané téma</b> ...... zvýrazněný název, na kterém je kurzor
 ■ <b>         ostatní témata</b> .... ostatní témata, na která lze nastavit kurzor
 ■ <b>         zvýrazněný text</b> ... nadpisy apod.
 ■ <b>KRÁTKÁ NÁPOVĚDA</b> ............ jednořádková nápověda na (před)posledním řádku

 ■ <b>OKNA stíny</b> ................. stíny oken ( menu, with window, ww )
 ■ <b>DESKTOP</b> .................... barva "pracovního" stolu (hlavní menu PC FANDu)
 ──────────────────────────────────────────────────────────────────────────────
 Ctrl-End         <a href="#t239">barvy a typy písma</a>
</pre>

<a name="t218"></a>
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
               tímto způsobem ovládání vládně široká kompatibilita a většinou 
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
               přepnout tiskárnu do IBM-modu (DIP přepínače) a použít
               přednastavené sekvence IBM PRO (IBM proprinter).

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
     <b>Alt-F6</b>) - lze instalovat až 10 tiskáren. Viz. <b>SetPrinter</b>, <b>CPrinter</b>.
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
                 <b>2.</b> Spustí se externí program (<b>EXEC</b>) podle parametru
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
     (<a href="#t237">tečkové příkazy</a>)

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
     <b>Gr.hustota 240</b>                                ──┘   <a href="#t319">tisk grafu</a>
     Tyto parametry se nastavují jen u typu tiskárny 'M' a 'C'. Pro typ 'L'
     nemají význam.

     <b>Nastavení barev</b>
     Pouze prefix, PC FAND doplní číslo barvy. Použije se u typu tiskárny C.


   ■ <b>Závěr.sekvence</b> 
     Závěrečná sekvence pro ukončení tisku. Má význam jen při speciálních
     případech tisku.
   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End          <a href="#t239">barvy a typy písma</a>          <a href="#t237">tisk textu</a>             <a href="#t318">grafy</a>
</pre>

<a name="t219"></a>
## instalace konstant

<pre>
 Ctrl-Home                     <b>INSTALACE KONSTANT</b>
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

 ■ <b>Automatická sestava - šířka</b>
   Základní parametr automatické sestavy, který udává počet znaků na řádce
   při generování automatické sestavy. Implicitně 80 znaků. Šířka sestavy 
   se odvozuje podle normální velikosti písma (10 CPI).

 ■ <b>Automatická sestava - délka</b>
 ■ <b>                    - ukončení strany</b>
   Délka udává počet řádků sestavy, které se mají generovat na jednu tiskovou
   stranu. Jde tedy o logickou délku strany. Přičtením počtu řádků pro
   ukončení strany se získá údaj o fyzické délce strany. Uplatní se jak při
   automatické tak i při definované sestavě. V definované sestavě lze fyzickou
   i logickou délku strany upravit dalšími programátorskými prostředky.
   Implicitní hodnoty jsou 69 a 3. Celkem 72 řádků = implicitní fyzická délka
   stránky. Vždy na začátku tisku vyšle PC FAND do tiskárny ESC-sekvenci pro
   nastavení fyzické délky stránky (pokud je tato sekvence uvedena viz.
   <a href="#t218">instalace tiskárny</a> a fyzická délka strany je jiná než 72). Fyzickou délku 
   strany lze definovat také tečkovým příkazem <b>.pl</b>.
   Ukončení strany lze definovat také tečkovým příkazem <b>.cp</b>.
   Při tisku na jednotlivé listy (podavač) doporučujeme nastavit hodnoty
   těchto parametrů na 64 (nebo kolik řádků se skutečně vejde na list) a
   ukončení strany doplníme na součet 72, tedy 8.
   Důležitý je součet 72, při jiné hodnotě se mohou některé tiskárny chovat
   "podivně", navíc to až na speciální typy sestav (složenky) nemá význam.

 ■ <b>Sestava - výstup rovnou na tiskárnu ?</b>  A/N (N)
   Při generování automatické sestavy pomocí volby datového editoru F6 - opis
   sestava vystupuje rovnou na tiskárnu, nikoliv nejdříve do tiskového souboru
   PRINTER.TXT.

 ■ <b>Výběr tiskárny před tiskem ?</b>  A/N (N)
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

 ■ <b>Potvrdit ESC z editoru ?</b>  A/N (N)
   Při opuštění datového editoru aktivuje kontrolní dotaz pro uživatele
      opustit editor A/N ?
   Neuplatní se při přerušení pořízení nové věty - viz. následující parametr.

 ■ <b>Potvrdit ESC z nedokončené věty?</b>  A/N (N)
   Parametrizace přerušení pořízení nové věty v datovém editoru. Při nastavení 
   na <b>A</b> musí obsluha přerušení pořízení potvrdit na hlášku:
                     Nedokončená věta bude zrušena! Přesto opustit A/N?
   V opačném případě je "rozeditovaná" věta bez varování nenávratně ztracena.

 ■ <b>Potvrzení hlášky ENTER místo F10</b>  A/N (N)       
   Pro potvrzení hlášek PC FANDu lze kromě klávesy F10 použít i <b>ENTER</b>. 
   Možné hodnoty A/N, implicitně N.

 ■ <b>Komentář k RDB při ladění ?</b>   A/N (A)
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
   <b>lokální sítě</b> - <a href="#t328">používané pojmy</a>.

 ■ <b>Sítě LAN - opak. blokovaného přístupu</b>  (<a href="#t328">net delay</a>)
   V lokální síti mohou být soubory, požadované pro určitou akci, dočasně
   nepřístupné z důvodu práce ostatních stanic. Proto je třeba odmítnutý
   pokus o práci s nimi po určitém čase automaticky znovu opakovat. Tento
   časový interval se zadává v jednotkách CPU (z důvodu jemnějšího odlišení
   jednotlivých stanic), implicitně 54 CPU. ( 1 sec ≈ 18 CPU )

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


 ■ <b>Sítě LAN - pípání při opakování blokování</b>.  A/N (A)
   Viz. předchozí parametr. Nepřístupnost souboru je signalizována vizuální
   hláškou a pípnutím, které je možno vypnout, ruší-li obsluhu.

 ■ <b>Pípání při chybách ?</b>    A/N (A)
   Chybová a některá další hlášení doprovází zvukový signál, který lze
   zde vypnout / zapnout. Nepotlačí (záměrně) příkaz <b>sound</b>.

 ■ <b>Max.velikost XMS pro PC FAND</b>
   PC FAND obsahuje vlastní diskovou cache. Implicitně alokuje 200 kB paměti
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
   <b>Myš - podpora vypnuta</b>  A/N (N)
   Možnost vypnutí podpory myši.

   <b>- výměna kláves</b>    A/N (N)
   Výměna levé a pravé klávesy.

   <b>- interval pro dvojitý stisk</b>
   Zadává se v CPU, implicitně 8.

   <b>- interval pro opakování</b>
   Automat.opakování stisku myši. Zadává se v CPU, implicitně 8.

 ■ <b>Prodleva Ctrl,Alt,Shift</b> 
   Interval pro vyvolání alternativ. nápovědy po stisku přeřazovací klávesy.
   Zadává se v CPU, implicitně 15.

 ■ <b>Automat.přepsat návěští diskety</b> . A/N (A)       
   Při zálohování na disketu (PC FANDovskými příkazy) lze při nesouhlasu
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
   <a href="#t287">valdate</a>    <a href="#t257">konstanty</a>

 ■ <b>Kontrola volného místa na disku</b>  A / N (A)
   Při práci na diskových polích o velikosti větší než 2 GB nastavíme na '<b>N</b>'
   a tak se můžeme vyhnout neoprávněným hlášením o nedostatku  volného místa 
   na disku.

──────────────────────────────────────────────────────────────────────────────
 Ctrl-End     <a href="#t245">automatická sestava</a>         <a href="#t250">katalog</a>        <a href="#t54">klávesnice</a>
</pre>

<a name="t220"></a>
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
   ■ <b>Vypnutý kurzor</b>   
   ■ <b>Velký kurzor</b>

   2.blok - podpora češtiny na monitoru
   ■ <b>Použít fonty FANDu ?</b> ...... na EGA/VGA automaticky použije font s dia-
                                 kritikou podle instalace abecedy, implic.<b>A</b>
   ■ <b>Monitor bez diakritiky ?</b>... pokud je nastaveno <b>A</b>, bude z hlášení pro 
                                 komunikaci s uživatelem odstraněna před výpi-
                                 sem na monitor diakritika, implicitně <b>N</b>.
   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End          <a href="#t221">instalace abecedy</a>
</pre>

<a name="t221"></a>
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
 
   Výstupy na tiskárnu lze překódovat - viz. <a href="#t218">instalace tiskárny</a>

   Podle instalované abecedy jsou v PC FANDu podporované funkce:
 
   ■ <a href="#t305">upcase</a> .............. převod malých písmen na velká
   ■ <a href="#t305">lowcase</a> ............. převod velkých písmen na malá
   ■ <a href="#t305">nodiakr</a> ............. odstranění diakritiky z textu
   ■ <a href="#t267">lexikální třídění</a> ... třídění zohlední diakritiku
   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End         <a href="#t220">instalace monitoru</a>
</pre>

<a name="t222"></a>
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
   <a href="#t289">funkce pro datum a čas</a>
</pre>

<a name="t223"></a>
## Rok 2000

<pre>
                  <b>Problematika roku 2000 a PC FAND</b>
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

   <b>■</b> Přes funkci <a href="#t287">Today</a>, která čte aktuální datum z DOSu (a potažmo z CRT)
     se do aplikace PC FANDu může "zanést" případná chyba daného počítače.


   Přípravu aplikace na rok 2000 a ochranu proti zanesení případných chyb
   z operačního systému (hardware) musí zajistit autor konkrétní aplikace.
   Některé možnosti má však i uživatel (i v "neupravené" aplikaci).

   ■ Speciální instalační konstanta <b>Posun implicitního století</b>,
     viz. <a href="#t219">instalace konstant</a>, řeší převod dvoumístného roku '...YY' na
     plný formát '...YYYY'.
     Zde se uplatní i možnost zadání dvoumístného roku do plné masky.
     Vesměs jde "jen" o řešení pohodlnosti zadávaní datumu pro uživatele.

   ■ Při editaci se zobrazuje datum v krátkém tvaru DD.MM.YY. Potřebujeme 
     zjistit plný tvar včetně století. Pomůžeme si kalkulačkou, potřebujeme 
     k tomu však znát interní název údaje :
     <b>Ctrl-F5</b>, a zadáme výpis datumu pomocí speciální datumové fukce <a href="#t287">strdate</a>
     strdate(NázevÚdajeTypuD,'DD.MM.YYYY')
     Potvrdíme pomocí klávesy &lt;ENTER&gt;.

   ■ Podobný problém jako předchozí, ovšem naopak potřebujeme zaručeně správné 
     datum zadat a máme jen krátkou masku DD.MM.YY. Opět pomůže kalkulačka, 
     nyní ovšem ani nemusíme znát interní název datumového údaje:
     - Přesuneme kurzor na údaj
     - Vyvoláme kalkulačku <b>Ctrl-F5</b>
     Dále máme 2 možnosti:
     - od verze PC FANDu 4.0 a výše zadáme přímo datum v plném tvaru 
       DD.MM.YYYY
     - v nižších verzích si pomůžeme speciální datumovou funkcí
       <a href="#t287">valdate</a>('1.1.2000','DD.MM.YYYY')
                   └─ požadované datum
       Potvrdíme pomocí klávesy &lt;ENTER&gt;.
     - A nyní převezmeme hodnotu pomocí <b>Ctrl-F4</b> do údaje.

     Pokud to připadá "krkolomné" je nutno si uvědomit, že takto je nutno 
     zadat jen některé datumy, které "uletí" z momentálního kontextu
     aplikace.
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t225"></a>
## textový editor

<pre>
                               <b>TEXTOVÝ EDITOR</b>
   ───────────────────────────────────────────────────────────────────────────
   <b>Textový editor</b> se používá zejména pro prohlížení <b>tiskových sestav</b> a editaci
   datových položek typu T ( volné texty ).  Např. programátorské prostředí PC
   FANDu je z velké části totožné s prostředím textového editoru.

   <b>Odchylky</b> PC FANDovského textového editoru oproti standardním zvyklostem:

   ■ mód <b>editace/prohlížení</b> (přepíná se klávesou <b>Scroll-Lock</b>)
   ■ změněný text se při opuštění editoru klávesou Esc  <b>automaticky</b> bez dotazu
     nahraje na disk ( protože navíc do neformátovaných souborů vkládá oddělo-
     vače řádků, nedoporučuje se používat textový editor na netextové soubory)
   ■ Zápis bloku do volné paměti (clipboard) pomocí kláves CtrlF7 (ShiftF7)

   <b>Některé charakteristiky textového editoru:</b>
   Není přímo omezena velikost editovaného souboru. Číslování řádků je omezeno
   hodnotou 99.999.  V paměti není uložen celý  editovaný  soubor ale jen jeho
   určitá část, která se dynamicky (a pro obsluhu neviditelně ) načítá z disku
   a zpět ukládá na disk. Pomocí techniky přerušení editace lze doplnit speci-
   ální funkce textového editoru. (definuje programátor)
   ───────────────────────────────────────────────────────────────────────────
   <a href="#t226">pohyby kurzoru</a>        <a href="#t227">editace textu</a>      <a href="#t231">bloky</a>           <a href="#t233">přepínače</a>
   <a href="#t234">formátování textu</a>     <a href="#t235">vyhledávání</a>        <a href="#t237">tisk textu</a>      <a href="#t239">barvy a typy písma</a>
   <a href="#t229">doplňky editoru</a>                          <a href="#t199">myš v datovém a textovém editoru</a>
</pre>

<a name="t226"></a>
## pohyby kurzoru

<pre>
                      <b>POHYBY KURZORU TEXTOVÉHO EDITORU</b>
   ──────────────────────────────────────────────────────────────────────────
   ■ <b>šipka doleva</b> ......... na předchozí znak
   ■ <b>šipka doprava</b> ........ na následující znak
   ■ <b>šipka nahoru</b> ......... na předchozí řádek
   ■ <b>šipka dolů</b> ........... na následující řádek
   ■ <b>Ctrl</b>-<b>šipka doleva</b> .... na předchozí slovo
   ■ <b>Ctrl</b>-<b>šipka doprava</b> ... na následující slovo
   ■ <b>Home</b> ................. na začátek řádku
   ■ <b>End</b> .................. na konec řádku

   ■ <b>PageUp</b> ............... na předchozí stranu
   ■ <b>PageDown</b> ............. na následující stranu
   ■ <b>Ctrl</b>-<b>PageUp</b> .......... na začátek textu
   ■ <b>Ctrl</b>-<b>PageDown</b> ........ na konec textu
   ■ <b>Ctrl</b>-<b>F3</b> .............. na zadaný řádek
   ■ <b>Ctrl</b>-<b>Q</b> <b>Ctrl</b>-<b>E</b> ........ na první řádek okna
   ■ <b>Ctrl</b>-<b>Q</b> <b>Ctrl</b>-<b>X</b> ........ na poslední řádek okna

   ■ <b>Ctrl</b>-<b>Home</b> ............ na předchozí volný text
   ■ <b>Ctrl</b>-<b>End</b> ............. na následující volný text

   ■ <b>Ctrl</b>-<b>W</b> ... posune obrazovku o jeden řádek dolů (nemění pozici kurzoru)
   ■ <b>Ctrl</b>-<b>Z</b> ... posune obrazovku o jeden řádek nahoru
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t227">editace textu</a>         <a href="#t199">myš v datovém a textovém editoru</a>
</pre>

<a name="t227"></a>
## editace textu

<pre>
                               <b>EDITACE TEXTU</b>
   ──────────────────────────────────────────────────────────────────────────
   ■ <b>Enter</b> ..... ukončení řádku (nebo odstavce při zapnutém formátování)
   ■ <b>Esc</b> ....... ukončení editace s nahráním změn na disk
   ■ <b>Alt</b>-<b>=</b> ..... ukončení editace bez nahrání změn na disk
                 (neplatí pro diskový soubor)

   ■ <b>Del</b> ............. mazání znaku pod kurzorem
   ■ <b>BackSpace &lt;-</b> .... mazání znaku před kurzorem
   ■ <b>Ctrl</b>-<b>T</b> .......... výmaz jednoho slova
   ■ <b>Ctrl</b>-<b>Y</b> .......... výmaz řádku
   ■ <b>Ctrl</b>-<b>Q Ctrl</b>-<b>Y</b> ... výmaz od pozice kurzoru do konce řádku

   ■ <b>Ctrl</b>-<b>U</b> ...... zrušení změn a obnovení textu (neplatí pro diskový soubor)
   ■ <b>Ctrl</b>-<b>Q Ctrl</b>-<b>L</b> ... obnovení obsahu řádku (pouze právě editovaný řádek)
   ■ <b>F9</b> .............. nahrání změn na disk (prevence proti výpadku proudu)

   ■ <b>Ctrl</b>-<b>N</b> ............. vložení nového řádku za kurzor
   ■ <b>Ctrl</b>-<b>I</b> ............. tabelátor
   ■ <b>Ctrl</b>-<b>J</b> ............. tabelace zprava
   ■ <b>Ctrl</b>-<b>O Ctrl</b>-<b>C</b> ...... vycentrování řádku
   ■ <b>Ctrl</b>-<b>P Ctrl</b>-<b>Znak</b> ... zadání řídícího znaku
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t226">pohyby kurzoru</a>
</pre>

<a name="t229"></a>
## doplňky editoru

*Též:* `rámečky`

<pre>
                              <b>DOPLŇKY EDITORU</b>
   ──────────────────────────────────────────────────────────────────────────
                              kreslení čar a rámečků

   ■ <b>Ctrl</b>-<b>Q -</b> ........ jednoduchá čára                       ┌──┐
   ■ <b>Ctrl</b>-<b>Q =</b> ........ dvojitá čára                          │ ╔╪═╗
   ■ <b>Ctrl</b>-<b>Q /</b> ........ maže čáru ve směru pohybu kurzoru     └─╫┘ ║
   ■ <b>šipky</b> ........... kreslení čáry                           ╚══╝
   ■ <b>Esc</b> ............. konec kreslení
   v průběhu kresby rámečku lze použít:
   ■ <b>-</b> ............... přepnutí na jednoduchou čáru
   ■ <b>=</b> ............... přepnutí na dvojitou čáru
   ■ <b>\</b> ............... přepnutí na mazání
   ■ <b>' '</b> ............. mezera - přepnutí na pohyb kurzoru bez kresby či mazání

   ■ <b>F4</b> .............. přidání/odstranění diakritiky ke znaku pod kurzorem
   ■ <b>Alt</b>-<b>F8</b> .......... vyvolá menu pro přepínání rozložení kláves
   ■ <b>Ctrl</b>-<b>F5</b> ......... vyvolání kalkulačky
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t54">klávesnice</a>
</pre>

<a name="t231"></a>
## bloky

*Též:* `clipboard`

<pre>
                          <b>BLOKY TEXTOVÉHO EDITORU</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Blok</b> je část textu. Blok je třeba  nejprve  označit (je barevně odlišen od
   ostatního textu)  a  potom  s  ním  lze  pracovat pomocí blokových operací.
   <b>Clipboard</b> je pomocný text (zásobník) pro přenášení bloků.

   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>N</b> ...... přepínač režimu běžný &lt;-&gt;<a href="#t232">sloupcový blok</a>

   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>B</b> nebo <b>F7</b> ...... označení začátku bloku
   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>K</b> nebo <b>F8</b> ...... označení konce bloku
   ■ <b>Shift</b>-<b>Down</b>,<b>Up</b>,<b>Left</b>,<b>Right</b> ... definice bloku tažením meze bloku
   ■ <b>Shift</b>-<b>PgDn</b>,<b>PgUp</b> ............ posun meze bloku o stránku vpřed (zpět)
   ■ <b>Shift</b>-<b>Home</b>,<b>End</b> ............. posun meze bloku na začátek (konec) řádku

   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>C</b> ........... kopie bloku (max.28kB) na místo kurzoru   <b>c</b>opy
   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>V</b> ....... přenesení bloku (max.28kB) na místo kurzoru   mo<b>v</b>e
   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>Y</b> ....... výmaz bloku

   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>R</b> ... čtení bloku z disku (textový soubor)              <b>r</b>ead
   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>W</b> ... zápis bloku na disk                              <b>w</b>rite

   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>U</b> ... v celém bloku změní malá písmena na velká       <b>u</b>pcase
   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>L</b> ... v celém bloku změní velká písmena na malá      <b>l</b>owcase
   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>F</b> ... formátování bloku                               <b>f</b>ormat
   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>P</b> ... tisk bloku                                       <b>p</b>rint

   ■ <b>Ctrl</b>-<b>F7</b> ..... kopie označeného bloku z textu do Clipboardu
   ■ <b>Shift</b>-<b>F7</b> .... kopie z Clipboardu do textu (přenosy bloků)
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t234">formátování textu</a>     <a href="#t237">tisk textu</a>
</pre>

<a name="t232"></a>
## sloupcový blok

<pre>
                               <b>SLOUPCOVÝ BLOK</b>
   ──────────────────────────────────────────────────────────────────────────
   Běžný blok je tvořen  souvislým  úsekem  textu , který je vymezen  prostým
   začátkem a koncem  bloku. <b>Sloupcový blok</b>  je kromě toho  vymezen i určitým
   počátečním a koncovým sloupcem textu. Z interního hlediska se tedy nejedná
   o souvislý úsek textu. Vizuálně jde o obdélník na obrazovce.
   
   Se sloupcovými bloky se manipuluje stejnými operacemi jako z běžným blokem.
   Režim práce (sloupcový&lt;-&gt;běžný blok) se nastaví přepínačem <b>Ctrl</b>-<b>K Ctrl</b>-<b>N</b>.

   Pro sloupcové bloky nejsou definovány operace  vyhledávání  <b>Ctrl</b>-<b>Q Ctrl</b>-<b>F</b>,
   nahrazení  <b>Ctrl</b>-<b>Q Ctrl</b>-<b>A</b> s  podmínkou <b>l</b> a operace formátování bloku <b>Ctrl</b>-<b>K</b>
   <b>Ctrl</b>-<b>F</b>.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t231">bloky</a>
</pre>

<a name="t233"></a>
## přepínače

<pre>
                        <b>PŘEPÍNAČE TEXTOVÉHO EDITORU</b>
   ──────────────────────────────────────────────────────────────────────────
   Aktuální hodnoty <b>přepínačů</b> jsou vypsány zkratkami v prvním řádku obrazovky.
   Odpovídajícím povelem střídavě zapínáte a vypínáte funkci přepínače.

   přepínač                                     povel           zkratka
   -------------------------------------------------------------zap--vyp------
   <b>vkládání/přepisování</b> při psaní textu         <b>Ins</b>             Vkl  Přep
   <b>odsazování</b> dalšího řádku podle předchozího   <b>Ctrl</b>-<b>Q Ctrl</b>-<b>I</b>   Ods
   automatické <b>formátování</b> na konci řádku       <b>Ctrl</b>-<b>O Ctrl</b>-<b>W</b>   Form
   <b>zarovnávání</b> na pravý okraj při formátování   <b>Ctrl</b>-<b>O Ctrl</b>-<b>J</b>   Zar
   <b>sloupcový blok</b>/běžný blok                    <b>Ctrl</b>-<b>K Ctrl</b>-<b>N</b>   Slp
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t234">formátování textu</a>
</pre>

<a name="t234"></a>
## formátování textu

<pre>
                             <b>FORMÁTOVÁNÍ TEXTU</b>
   ───────────────────────────────────────────────────────────────────────────
   Kvůli formátování v textech PC FANDu rozlišujeme <b>měkké a tvrdé konce řádků</b>.
   Odstavec je pak definován jako úsek textu ukončený tvrdým koncem řádku (při
   editaci klávesou <b>Enter</b> ). Řádky s  tvrdými  konci  jsou označeny znakem <b>&lt;</b> v
   posledním sloupci obrazovky (při zapnutém přepínači pro formátování). 
   Odstavce lze formátovat do sloupce mezi <b>levý a pravý okraj</b> textu.

   ■ <b>Ctrl</b>-<b>O Ctrl</b>-<b>L</b> ... nastavení levého okraje textu (implicitně <b>1</b>)
   ■ <b>Ctrl</b>-<b>O Ctrl</b>-<b>R</b> ... nastavení pravého okraje textu (implicitně <b>78</b>)

   ■ <b>Ctrl</b>-<b>B</b> .......... formátování textu od pozice kurzoru do konce odstavce
   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>F</b> ... formátování celého bloku

   Při nastaveném přepínači  <b>automatické  formátování</b> (<b>Ctrl</b>-<b>O Ctrl</b>-<b>W</b>) se
   text formátuje již během pořizování textu vždy při naplnění řádku. 
   Pokud je navíc nastaven přepínač <b>zarovnávání</b> (<b>Ctrl</b>-<b>O Ctrl</b>-<b>J</b>), bude 
   text při formátování zarovnán na pravý okraj (do řádku se vloží mezery).
 
   Interní uložení oddělovačů v textu: <b>^M^J</b> - tvrdý konec, <b>^M</b> - měkký konec.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t233">přepínače</a>     <a href="#t231">bloky</a>
</pre>

<a name="t235"></a>
## vyhledávání

<pre>
                            <b>VYHLEDÁVÁNÍ V TEXTU</b>
   ──────────────────────────────────────────────────────────────────────────
   ■ <b>Ctrl</b>-<b>Q Ctrl</b>-<b>F</b> ... hledání řetězce v textu
   ■ <b>Ctrl</b>-<b>Q Ctrl</b>-<b>A</b> ... hledání řetězce a nahrazení jiným
   ■ <b>Ctrl</b>-<b>L</b> .......... nové spuštění hledání/nahrazování podle minulého zadání

   Při hledání/nahrazování postupně zadáváte na posledním řádku:

   ■ <b>Najdi</b> ...... hledaný řetězec
   ■ <b>Nahraď</b> ..... řetězec, kterým se bude nahrazovat původní text
   ■ <b>Podmínky</b> ... pro hledání: jedno nebo více písmen podle následující tabulky
   ■ <b>g</b> ... vyhledávání v celém textu (implicitně hledá jednou od současné pozice)
   ■ <b>e</b> ... <b>globální</b> hledání ( jen volné texty, ve všech větách souboru )
   ■ <b>n</b> ... nahrazení probíhá automaticky <b>bez dotazu</b> Ano/Ne na potvrzení
   ■ <b>w</b> ... vyhledává jen <b>celá slova</b>, ne složeniny
   ■ <b>~</b> ... srovnává <b>lexikálně</b> (např. baba=BÁBA)
   ■ <b>u</b> ... srovnává <b>bez rozlišení malých a velkých písmen</b> (např. Praha=PRAHA)
   ■ <b>l</b> ... hledání / nahrazování je omezeno jen na definovaný blok

   Implicitní podmínky pro hledání  (podmínky  se nezadají): pouze jednou, od
   pozice kurzoru, s dotazem na přepsání, bez přeskočení složenin, s rozliše-
   ním malých a velkých písmen a znaků s diakritikou.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t267">lexikální třídění</a>
</pre>

<a name="t237"></a>
## tisk textu

*Též:* `tečkové příkazy`

<pre>
                                 <b>TISK TEXTU</b>
   ──────────────────────────────────────────────────────────────────────────
   ■ <b>F6</b> .............. tisk celého textu
   ■ <b>Ctrl</b>-<b>F6</b> ......... tisk od pozice kurzoru do konce textu
   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>P</b> ... tisk bloku (nebo celého textu, není-li blok definován)
   ■ <b>Alt</b>-<b>F6</b> .......... interaktivní změna tiskárny

   Tisk lze přerušit klávesou <b>Esc</b>. Při tisku se interpretují (netisknou se, 
   ale provedou požadované akce) následující <b>tečkové příkazy</b>, jsou-li
   umístěné v prvních řádcích textu (po jednom na řádku vždy od prvního sloupce):

   ■ <b>.ti n</b> ... počet výtisků (implicitně <b>1</b>)
   ■ <b>.cp n</b> ... automatické stránkování (počet řádek vynechaných na konci strany)
   ■ <b>.pl n</b> ... délka fyzické strany na tiskárně (implicitně <b>72</b> řádek)
   ■ <b>.po n</b> ... odsazení textu od levého okraje (implicitně <b>0</b>)
   ■ <b>.ff</b> ..... potlačí hlášku "nastav tiskárnu" na začátku a odstránkování
               na konci
   ■ <b>.nm</b> ..... jako <b>.ff</b> ale na konci odstránkuje

   ■ <b>.he text</b> ... hlavička strany  (<b>masky</b> uvnitř textu ... <b>___</b> - číslo strany
   ■ <b>.fo text</b> ... ukončení strany                     <b>__.__.__</b> - datum
                                                         <b>__:__</b> - čas)
   <b>n</b> je libovolné číslo, <b>text</b> je jednořádkový řetězec
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t239">barvy a typy písma</a>      <a href="#t231">bloky</a>      <a href="#t218">instalace tiskárny</a>
</pre>

<a name="t238"></a>
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
  <a href="#t239">barvy a typy písma</a>
</pre>

<a name="t239"></a>
## barvy a typy písma

<pre>
                             <b>BARVY A TYPY PÍSMA</b>
   ───────────────────────────────────────────────────────────────────────────
   Barvám na obrazovce  odpovídají  typy  písma  na tiskárně.  Do textu zadáte
   přepínač barvy/písma jako Ctrl-Znak příkazem <b>Ctrl-P Ctrl-Znak</b> v módu editace.
   V módu prohlížení ( <b>Scroll-Lock</b> ) řídící znaky  zmizí a text se obarví. Při
   prvním výskytu přepínače v textu se barva/písmo zapne, při druhém vypne atd.

   ■ Ctrl-<b>S</b> ... <b>podtržení</b> (underline) ... na barevných monitorech <b>zelená</b>
   ■ Ctrl-<b>W</b> ... <b>kurzíva</b> (italic) ........ <b>světle modrá</b>
   ■ Ctrl-<b>B</b> ... <b>tučný tisk</b> (bold) ....... <b>fialová</b>
   ■ Ctrl-<b>D</b> ... <b>dvojitý tisk</b> (double) ... <b>červená</b>

   ■ Ctrl-<b>Q</b> ... <b>široký tisk</b> (wide) ...... <b>žlutá na modrém pozadí</b>
   ■ Ctrl-<b>E</b> ... <b>komprimovaný</b> (compressed) <b>žlutá na červeném pozadí</b>
   ■ Ctrl-<b>A</b> ... užší písmo (<b>elite</b>) ...... <b>žlutá</b>
                                                         původní nastavení
   ■ Ctrl-<b>X</b> ... uživatelem <b>definovatelné kódy</b> pro tisk   dvojité písmo
   ■ Ctrl-<b>V</b> ... (bez barevného odlišení)                 čtyřnásobné písmo
   ■ Ctrl-<b>T</b> ...                                          husté řádkování

   Ctrl-Znaky pro přepínání barev se zohledňují při formátování textu. Všechny
   barvy a řídící kódy pro tiskárnu je možné přeinstalovat programem <b>FANDINST</b>.
   ───────────────────────────────────────────────────────────────────────────
   <a href="#t237">tisk textu</a>     <a href="#t217">instalace barev</a>     <a href="#t218">instalace tiskárny</a>
</pre>

<a name="t241"></a>
## datový editor

<pre>
                               <b>DATOVÝ EDITOR</b>
 ──────────────────────────────────────────────────────────────────────────────
   pohyb po souboru                          pohyb uvnitř údaje
   ----------------                          ------------------
 ■ <b>&lt;-</b> ..... na předchozí údaj ve větě     ■ <b>Ins</b>,<b>CtrlV</b> ......... vkládání/přepis
 ■ <b>-&gt;</b> ..... na následující údaj           ■ <b>&lt;-</b> .................. o znak doleva
 ■ <b>Enter</b> .. na další editovatelný údaj    ■ <b>-&gt;</b> ................. o znak doprava
 ■ <b>Home</b> ... na první údaj ve větě         ■ <b>Home</b> ............. na začátek údaje
 ■ <b>End</b> .... na poslední údaj ve větě      ■ <b>End</b> ................ na konec údaje
 ■ <b>Ins</b> .... vstup do údaje bez přepsání   ■ <b>Del</b> ..... mazání znaku pod kurzorem
 ■ <b>PageUp</b> ......... na předchozí stranu   ■ <b>BackSpace &lt;-</b> ........ před kurzorem
 ■ <b>PageDown</b> ....... na následující stranu ■ <b>Enter</b> ...... ukončení editace údaje
 ■ <b>Ctrl</b>-<b>Home</b> ...... na předchozí větu     ■ <b>Esc</b> ....... návrat bez uložení změn
 ■ <b>Ctrl</b>-<b>End</b> ....... na následující větu   ■ <b>F4</b> ............ diakritika pro znak
 ■ <b>Ctrl</b>-<b>PageUp</b> .... na začátek souboru    ■ <b>Alt</b>-<b>F8</b> ....... přepínání klávesnice
 ■ <b>Ctrl</b>-<b>PageDown</b> .. na konec souboru
 

 Editace <b>volných textů</b> ... <b>Ins</b> - dovnitř, <b>Esc</b> - zpět.  Samotný  text  editujete
 <b>textovým editorem</b>. Na obrazovce znamená <b>*</b> naplněný a <b>.</b> prázdný  text.  Editaci
 datového souboru ukončíte  klávesou  <b>Esc</b>.  Nápověda  popisuje  běžné  ovládání
 editoru, v úloze může programátor některé povely uživateli zakázat.

 Přepínače pro vkládací a přepisovací režim při editaci dat jsou dva. Jeden pro
 <a href="#t225">textový editor</a> ( údaje typu T ) a jeden pro  ostatní typy údajů.  Přepínač pro
 textový editor je globální (nastavení platí i pro editaci dalších textů). 
 Přepínač pro ostatní typy dat je naopak lokální, což znamená,že jeho nastavení
 je platné jen v rámci právě rozpracované editace údaje.  Po vstupu do údaje je
 vždy vkládací režim.

 Editace sdíleného souboru v síti LAN má svá specifika - <a href="#t329">sdílená editace</a>
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t242">funkční a řídící klávesy</a>     <a href="#t243">přepínače datového editoru</a>     <a href="#t244">akce</a>
 <a href="#t245">automatická sestava</a>          <a href="#t248">navigace po databázi</a>           <a href="#t225">textový editor</a>
</pre>

<a name="t242"></a>
## funkční a řídící klávesy

<pre>
                          <b>FUNKČNÍ A ŘÍDÍCÍ KLÁVESY</b>
 ──────────────────────────────────────────────────────────────────────────────
 ■ <b>F2</b> (<b>pořiď</b>/<b>edit</b>) ... přepnutí do módu pořízení/editace
 ■ <b>F3</b> (<b>hledej</b>) ....... vyhledání věty podle vlastního klíče
 ■ <b>F4</b> (<b>duplikace</b>) .... údaj se zkopíruje podle předchozí věty
 ■ <b>F5</b> (<b>přepínače</b>) .... nastavení a zrušení přepínačů
 ■ <b>F6</b> (<b>akce</b>) ......... spuštění operací nad souborem
 ■ <b>F8</b>,<b>ShiftF8</b> (<b>výběr</b>). manuální výběr věty do podmnožiny (resp.zrušení výběru)
 ■ <b>F9</b> (<b>ulož</b>) ......... uloží aktuální stav souboru na disk proti výpadku proudu

 ■ <b>Ctrl</b>-<b>F2</b> ................ obnovení podmnožiny <b>cond</b> (sítě, exit procedury)
 ■ <b>Ctrl</b>-<b>F3</b> (<b>věta</b>) ......... nastavení editoru podle čísla věty
 ■ <b>Ctrl</b>-<b>F5</b> (<b>vypočítej</b>) .... vyvolání <b>kalkulátoru</b>
   (<b>Ctrl</b>-<b>U</b> obnovuje poslední výraz a <b>Ctrl</b>-<b>F4</b> přenáší výsledek zpět do editoru)

 ■ <b>Ctrl</b>-<b>&lt;-</b> resp. <b>Ctrl</b>-<b>-&gt;</b> ... výměna sousedních vět

 ■ <b>Ctrl</b>-<b>Y</b> ... zrušení aktuální věty (nebo celé podmnožiny vět, je-li nastavena)
 ■ <b>Ctrl</b>-<b>N</b> ... vložení věty na místo datového kurzoru
 ■ <b>Ctrl</b>-<b>U</b> ... zrušení změn ve větě a obnovení původního obsahu načtením z disku
 ■ <b>Ctrl</b>-<b>\</b> ... odskok na začátek příští věty.
 ■ <b>Esc</b> ...... ukončení práce datového editoru
 ■ případně (<b>Alt</b>,<b>Ctrl</b>,<b>Shift</b>,)-<b>F1</b> až <b>...-F10</b> ... programátorem definované akce
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t243">přepínače datového editoru</a>     <a href="#t244">akce</a>     <a href="#t248">navigace po databázi</a>
</pre>

<a name="t243"></a>
## přepínače datového editoru

<pre>
                      <b>F5 - PŘEPÍNAČE DATOVÉHO EDITORU</b>
 ──────────────────────────────────────────────────────────────────────────────
 Po <b>F5</b> střídavě <b>přepínače</b> zapínáte a vypínáte volbou z menu:

 ┌─ zapni-vypni ──┐
 │ <b>P</b>odmnožina     │ editace podmnožiny vět nebo celého souboru
 │ <b>D</b>uplikace      │ při pořízení se údaj zkopíruje z předešlé věty
 │ <b>T</b>abelátor      │ po <b>Enter</b> se kurzor zastaví jen na údajích s tabelátorem
 │ <b>A</b>ditivní změny │ vypnutí automatického přičítání do nadřízeného souboru
 │ <b>K</b>ontroly       │ vypnutí logických kontrol na správnost zadaných dat
 │ <b>V</b>arování       │ vypnutí kontrolních varování
 └────────────────┘

 Podmnožina, Aditivní změny a Kontroly  jsou  globální  přepínače  pro  editaci
 souboru, zatímco Tabelátor a Duplikace patří ke každému editovanému údaji.

 Indikace stavu přepínačů: při zapnuté <b>podmnožině</b> jsou vybrané věty  zvýrazněny
 barevně, navíc je v posledním řádku šipka. Vypnutí <b>aditivních změn</b> a <b>logických</b>
 <b>kontrol</b> je vyznačeno v posledním řádku znaky <b>#A</b>, resp. <b>#L</b>. Přepínač aditivních
 změn slouží rovněž i pro  <b>referenční integritu</b> ( zap/vyp ). Zapnuté <b>tabelátory</b>
 a <b>duplikace</b> jsou přímo u odpovídajícího údaje označeny šipkami.

 <b>Přepínač vkládacího a přepisovacího režimu</b>
 Přepínače pro vkládací a přepisovací režim při editaci dat jsou dva. Jeden pro
 <a href="#t225">textový editor</a> ( údaje typu T ) a jeden pro  ostatní typy údajů.  Přepínač pro
 textový editor je globální (nastavení platí i pro editaci dalších textů). 
 Přepínač pro ostatní typy dat je naopak lokální, což znamená,že jeho nastavení
 je platné jen v rámci právě rozpracované editace údaje.  Po vstupu do údaje je
 vždy vkládací režim.
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t244">akce</a>
</pre>

<a name="t244"></a>
## akce

<pre>
                         <b>F6 - AKCE DATOVÉHO EDITORU</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>F6</b> vyvolá <b>akce</b> pro právě editovaný soubor.

   ┌─────────────────┐
   │ <b>O</b>pis            │ generátor <b>automatických sestav</b>
   │ <b>K</b>ontroly        │ od pozice kurzoru až do konce souboru provede <b>kontroly</b>
   │ <b>N</b>ová podmnožina │ zadání logického výrazu pro <b>vybranou podmnožinu</b>
   │ <b>G</b>raf            │ grafické zobrazení dat souboru (interaktivní režim)
   │ <b>T</b>řídit          │ setřídí soubor podle vybraných údajů
   └─────────────────┘


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   Datum&gt;=1.1.92                {příklady zadání výrazu pro novou podmnožinu}
   Ucet in ['300'..'599','999']
   Mzda&gt;5000
   Místo=~'Praha'
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t245">automatická sestava</a>       <a href="#t201">výběr ze seznamu</a>      <a href="#t243">přepínače datového editoru</a>
</pre>

<a name="t245"></a>
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
   <a href="#t244">akce</a>       <a href="#t246">řídící a třídící údaje</a>
</pre>

<a name="t246"></a>
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
     <b>indexu</b>). Pokud je vstupuje do sestavy jen jeden soubor, nemusí být tříděn
     dle řídících údajů (za výsledek je ovšem odpovědný programátor).

   ■ <b>Třídící  údaje</b>:  uvnitř  skupin  vět  dle  řídících  údajů lze věty ještě
     setřídit podle třídících údajů.
 
   ───────────────────────────────────────────────────────────────────────────
   <a href="#t245">automatická sestava</a>       <a href="#t267">lexikální třídění</a>
</pre>

<a name="t248"></a>
## navigace po databázi

*Též:* `navigace`

<pre>
                            <b>NAVIGACE PO DATABÁZI</b>
 ──────────────────────────────────────────────────────────────────────────────
 <b>Navigace</b> umožňuje vyvolat z editace souboru datový  editor  pro  jiný  soubor,
 tedy vloženou editaci s pozdějším návratem do místa vyvolání.  Před  vyvoláním
 editoru z okna vyberete požadovaný <b>soubor</b> nebo <b>uživatelský pohled</b>.

 ■ <b>F7</b> (<b>soubor</b>) ... navigace směrem <b>nahoru</b> z podřízeného souboru do nadřízeného
   nabídnou se pouze k němu nadřízené soubory
   editor najde viditelnou větu (má stejný klíč) nebo nejbližší vyšší
 ■ <b>Ctrl-F7</b> (<b>podřízený soubor</b>) ... navigace směrem <b>dolů</b> (nad.-&gt; pod.)
   nabídnou se pouze podřízené soubory s odpovídajícím vlastním klíčem
   vyberou se jen odpovídající věty podřízeného souboru (se stejným klíčem)
 ■ <b>Alt-F7</b> .............. navigace <b>po celé databázi</b> (všechny soubory)
 ■ <b>Esc</b> ................. návrat z vložené editace
 ■ <b>Ctrl-F4</b> (<b>převzít</b>) ... návrat z vložené editace s převzetím hodnoty do údaje

 ■ <b>Shift-F7</b> ............ zrychlený způsob navigace směrem <b>nahoru</b>
   určeno pro hledání a přenos údaje z nadřízeného souboru
   vyvolá se pouze z klíčového údaje, který definuje vazbu směrem nahoru
   následuje okamžitý odskok do nadřízeného souboru (pouze prohlížení)
   bez výběru souboru nebo uživatelského pohledu,
   <b>Enter</b> znamená návrat s přenosem klíčového údaje, <b>Esc</b> návrat bez přenosu
   Za jistých podmínek (definuje programátor), lze nadřízený soubor také 
   editovat (i pořizovat).
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t250"></a>
## katalog

<pre>
                                  <b>KATALOG</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Katalog</b> je nepovinný soubor přiřazený k úloze (stejné jméno, přípona <b>.CAT</b>,
   kterým lze  měnit ve speciálních případech některé implicitní  vlastnosti,
   především <b>cesty k fyzickým souborům</b> na disku včetně <b>jména</b>. Jedna věta
   katalogu  odpovídá jednomu souboru v úloze (soubor je pak <b>katalogizovaný</b>) 
   a obsahuje údaje:

   ■ <b>Název úlohy</b> ..... Úloha, v níž je soubor popsán a používán
   ■ <b>Název souboru</b> ... Logické jméno souboru používané ve zdrojových textech
                       úlohy (pozor - nemusí jít nutně o kapitolu F).
   ■ <b>Číslo archivace</b> . Zařazení souboru do úrovně archivace
   ■ <b>Cesta</b> ........... Fyzické jméno souboru (<b>Jednotka,Cesta,Jméno,Přípona</b>).
                       Cesta nemusí být úplná, platí běžné konvence MS-DOSu
                       pro kompletaci cesty s tím, že jako aktuální adresář
                       platí adresář úlohy.
   ■ <b>Návěští</b> ......... Volume diskety, pokud se soubor nachází na disketě
                       nebo znak <b>#</b>, <b>#R</b>, <b>SQL</b> (viz. <a href="#t321">lokální sítě</a>)

   ██ <b>Založení a úprava katalogu</b>
      Katalog se zakládá a upravuje z hlavního menu PC FANDu volbou <b>Instalace</b>
      <b>úlohy</b> (i v uživatelské verzi).

   <b>Poznámka </b>Mezi verzí PC FANDu 3.0 (3.01) a 3.2 došlo ke změně struktury
            katalogu (položka Ar). Starý formát se na nový převede automaticky,
            Převod nového formátu na starý lze provést programem <b>OldCatlg.EXE</b>.
            (Samozřejmě lze použít i vhodný MERGE).
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t321">lokální sítě</a>      <a href="kontextova-napoveda.md#t6">instalace úlohy</a>
</pre>

<a name="t252"></a>
## výrazy

<pre>
                                   <b>VÝRAZY</b>
 ──────────────────────────────────────────────────────────────────────────────
 <b>Výrazy</b> jsou naznačené  výpočty  a  objevují  se  na  mnoha  místech  projektu.
 Vytvářejí se obvyklým způsobem z konstant, proměnných, operátorů a funkcí.

 ■ <a href="#t256">typy výrazů</a> ......... číselný - <b>real</b>, textový - <b>string</b> a logický - <b>boolean</b>
 ■ <a href="#t257">konstanty</a> ........... všech typů
 ■ <a href="#t258">operátory</a> ........... číselné, textové, logické, srovnávací

 ■ <a href="#t282">aritmetické funkce</a> ....... reálné, goniometrické, logaritmické
 ■ <a href="#t289">funkce pro datum a čas</a> ... systémové datum a čas, převody mezi datumy a texty
 ■ <a href="#t315">zpracování textu</a> ......... vyhledávání, převod mezi texty a čísly,
                              podřetězce, textové soubory a volné texty
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t268">░ výrazy</a>
</pre>

<a name="t256"></a>
## typy výrazů

*Též:* `real`, `string`, `boolean`

<pre>
                                <b>TYPY VÝRAZŮ</b>
   ──────────────────────────────────────────────────────────────────────────
   (vpravo kompatibilní <b>typy údajů</b> z deklarace souboru)

   ■ <b>real</b> ...... <b>číselný</b> - (pascalský 6-bytový real)-11 platných míst
                           ve verzi pro koprocesor 15 platných míst
   ■ <b>string</b> .... <b>textový</b> - až 65 000 B (jako volný text)
   ■ <b>boolean</b> ... <b>logický</b> - true/false (Ano/Ne)

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t268">░ výrazy</a>
</pre>

<a name="t257"></a>
## konstanty

<pre>
                                 <b>KONSTANTY</b>
   ──────────────────────────────────────────────────────────────────────────
   ■ <b>číselná konstanta</b> ... celé číslo (příp. se znaménkem)
                       ... číslo s pevnou řádovou čárkou
                       ... datum ve tvaru <b>DD.MM.YY</b> nebo <b>DD.MM.YYYY</b>
                       ... čas ve tvaru <b>hh:mm</b>, <b>hh:mm:ss</b> nebo <b>hh:mm:ss.tt</b>

   ■ <b>textová konstanta</b> ... řetězec libovolných znaků v apostrofech
                           apostrof uvnitř konstanty: dva apostrofy <b>''</b>
                           znak zadaný dekadickým kódem znaku: <b>\nnn</b>
  
   ■ <b>logická konstanta</b> ... <b>false</b>, <b>true</b>
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t288">maska pro datum</a>
</pre>

<a name="t258"></a>
## operátory

<pre>
                                 <b>OPERÁTORY</b>
 ──────────────────────────────────────────────────────────────────────────────
 logické                  číselné                      srovnávací
 ───────────────────      ──────────────────────       ──────────────────
 negace      (NOT) <b>^</b>      sčítání              <b>+</b>       rovnost          <b>=</b>
 konjunkce   (AND) <b>&amp;</b>      odčítání             <b>-</b>       nerovnost        <b>&lt;&gt;</b>
 alternativa (OR)  <b>|</b>      násobení             <b>*</b>       větší            <b>&gt;</b>
 implikace         <b>=&gt;</b>     dělení               <b>/</b>       větší nebo roven <b>&gt;=</b>
 ekvivalence      <b>&lt;=&gt;</b>     celočíselné dělení  <a href="#t263">div</a>      menší            <b>&lt;</b>
                          zbytek po dělení    <a href="#t263">mod</a>      menší nebo roven <b>&lt;=</b>
 textové                  zaokrouhlování     <a href="#t263">round</a>     prvek intervalu  <a href="#t263">in</a>
 ──────────────
 zřetězení    <b>+</b>

 ■ srovnávací operátory slouží k porovnání čísel i textů a mohou být doplněny:
 - <b>Operátor.Číslo</b> ... přesnost číselného srovnání (implicitně 5 des.míst)
 - <b>Operátor~</b> ........ lexikální textové srovnání v národní abecedě
                      s ignorováním mezer zprava

 ░ 1.234 &gt; 1.2   ale   1.234 =.1 1.2
 ░ 'Praha' &lt;&gt; 'Praha '   ale   'Praha' =~ 'PRAHA   '       'baba'=~'bÁBa'
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t267">lexikální třídění</a>     <a href="#t264">priorita operátorů</a>     <a href="#t268">░ výrazy</a>
</pre>

<a name="t263"></a>
## speciální operátory

*Též:* `div`, `mod`, `round`, `in`

<pre>
                            <b>SPECIÁLNÍ OPERÁTORY</b>
  ────────────────────────────────────────────────────────────────────────────
  █ ČísVýraz <b>DIV</b> ČísVýraz :real ..... celočíselné <b>dělení</b>
  █ ČísVýraz <b>MOD</b> ČísVýraz :real ..... <b>zbytek</b> po celočíselném dělení

  ░ 10 div 3 = 3
  ░ 10 mod 3 = 1

  █ ČísVýraz1 <b>ROUND</b> ČísVýraz2 :real ... <b>zaokrouhlení</b> ČísVýraz1 na určený počet
                                        míst dle hodnoty ČísVýraz2 (0..10)

  ░ pi round 2 = 3.14
  ░ 1.6 round 0 = 2

  █ Výraz <b>IN</b> [SeznamArgumentů] :boolean ... <b>množinový test</b>
    testuje, zda výraz je prvkem množiny konstant nebo intervalů konstant
    konstanty a výraz jsou stejného typu (real nebo string)
  ■ <b>in.Číslo</b> ...<b>přesnost</b> číselného srovnání (počet des.míst, implicitně 5,
                         maxim. 10)
  ■ <b>in~</b> ....... <b>lexikální</b> textové srovnání v národní abecedě

  ░ Účet in ['300'..'399','520','800'..'999']
  ░ Rok in [1914..1918,1939..1945,1968,1989]
  ────────────────────────────────────────────────────────────────────────────
  <a href="#t265">░ speciální konstrukce</a>
</pre>

<a name="t264"></a>
## priorita operátorů

<pre>
                             <b>PRIORITA OPERÁTORŮ</b>
   ──────────────────────────────────────────────────────────────────────────
   Pokud není uzávorkováním určeno jinak, operátory mají následující priority 
   (od nejvyšší k nejnižší):

                        1.  - (unární mínus)  ^
                        2.  *  /  div  mod  round
                        3.  +  -
                        4.  =  &lt;&gt;  &gt;  &gt;=  &lt;  &lt;=
                        5.  &amp;
                        6.  |
                        7.  =&gt;  &lt;=&gt;
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t265"></a>
## ░ speciální konstrukce

<pre>
                       <b>PŘÍKLAD - SPECIÁLNÍ KONSTRUKCE</b>
 ──────────────────────────────────────────────────────────────────────────────
 ░ přídavky:=cond (děti  = 1:  200,                               {funkce cond}
 ░                 děti  = 2:  430,
 ░                 děti  = 3:  680,
 ░                 děti  = 4: 1240,
 ░                 děti &gt;= 5: 1240+(děti-4)*350);
 ░ cond(Muž:'pan '+jméno,
 ░      else:'paní '+jméno);

 ░ cond(účet in ['100'..'199','450','700'..'999']:MáDáti-Dal,     {operátor in}
 ░      účet in ['200'..'399']:Dal-MáDáti);
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t267"></a>
## lexikální třídění

*Též:* `lexikální porovnání`

<pre>
                             <b>LEXIKÁLNÍ TŘÍDĚNÍ</b>
   ──────────────────────────────────────────────────────────────────────────
   PC FAND může pracovat s instalovatelnou tabulkou  národní abecedy. Podpora
   pro práci s národní abecedou se týká <b>třídění</b> a <b>porovnávání</b> a funkce <a href="#t305">upcase</a> 
   resp. <a href="#t305">lowcase</a>.  Syntakticky  se  v projektu odlišuje  lexikální  relace od
   běžného třídění (podle kódu znaku) uvedením vlnovky<b>~</b> za operátor nebo před
   klíčový (řídící, třídící) údaj.
 
   Při lexikálním <b>třídění</b> je správně zatříděna dvojhláska <b>CH</b>, jsou rovnocenná 
   ( tj. jejich pořadí je náhodné ) <b>malá</b> a <b>velká písmena</b> a písmena s <b>čárkou</b> a
   bez čárky. Naproti tomu písmeno s <b>háčkem</b> je  zatříděno  až  za písmeno bez  
   háčku. Při lexikálním <b>srovnání</b> se ignorují mezery zprava. Lexikální relace
   zasahuje do těchto míst projektu:

   ■ <b>třídění</b> podle národní abecedy (ruční třídění v <b>datovém editoru</b>)
   ■ <b>klíčové údaje</b> typu A v definici <b>klíčů</b>
   ■ <b>řídící</b> a <b>třídící údaje</b> v <b>sestavě</b> a <b>transformaci</b>
   ■ <b>srovnávací operátory</b> pro porovnávání textů, operátor <b>in</b>, funkce <b>pos</b>
   ■ <b>hledání</b> v textovém editoru

   ░ Název=~''   #K @ ~Jméno    #I1_DATA ~ŘídÚdaj;~TřídÚdaj   Místo in~ [...]
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t221">instalace abecedy</a>    <a href="#t258">operátory</a>    <a href="#t201">výběr ze seznamu</a>   <a href="#t235">vyhledávání</a>    <a href="#t305">upcase</a>
</pre>

<a name="t268"></a>
## ░ výrazy

<pre>
                              <b>PŘÍKLAD - VÝRAZY</b>
 ──────────────────────────────────────────────────────────────────────────────
 ░ 3.14                         {číselné výrazy}
 ░ číslo+1      
 ░ Today-DatumNarození   
 ░ množství * číselník.cena
 ░ 12:30:01 + 24.12.89   
 ░ sin(1.23456789)+exp(2*int(x))

 ░ 'Praha'                      {textové výrazy}
 ░ jméno+' '+příjmení   
 ░ copy(upcase(text),5,10)
 ░ str(číselník.cena,6,2)+' Kčs'
 ░ leadchar(' ',Název)

 ░ true                         {logické výrazy
 ░ mzda &gt; 3000   
 ░ (jméno='Novák' &amp; místo='Praha')
 ░ narozen &lt; 1.1.50
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t282"></a>
## aritmetické funkce

*Též:* `abs`, `int`, `frac`, `sqr`, `sqrt`, `random`, `ln`, `exp`, `pi`, `sin`, `cos`, `arctan`

<pre>
                             <b>ARITMETICKÉ FUNKCE</b>
   ──────────────────────────────────────────────────────────────────────────
   Uvedené funkce jsou typu <a href="#t256">real</a>, jejich výsledkem je tedy číselná hodnota.

   █ <b>abs</b>(ČísVýraz) ...... absolutní hodnota
   █ <b>int</b>(ČísVýraz) ...... celá část čísla (před desetinnou tečkou)
   █ <b>frac</b>(ČísVýraz) ..... desetinná část čísla (za desetinnou tečkou)
   █ <b>sqr</b>(ČísVýraz) ...... druhá mocnina
   █ <b>sqrt</b>(ČísVýraz) ..... druhá odmocnina
   █ <b>random</b> ............. náhodné číslo z intervalu &lt;0,1)

   █ <b>ln</b>(ČísVýraz) ....... přirozený logaritmus
   █ <b>exp</b>(ČísVýraz) ...... exponenciální funkce

   █ <b>pi</b> ................. Ludolfovo číslo
   █ <b>sin</b>(ČísVýraz) ...... sinus
   █ <b>cos</b>(ČísVýraz) ...... cosinus
   █ <b>arctan</b>(ČísVýraz) ... arcus tangens
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t287"></a>
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

   Konverzní funkce <b>strdate</b> převede datum a čas v interním formátu na string.
   Funkce <b>valdate</b> provede pravý opak. Obě funkce provedou převod podle masky.
   <a href="#t288">Maska pro datum</a> formátuje textové vyjádření datumu a času.

   ██ Syntaxe:     <b>STRDATE (</b> ČísVýraz <b>,</b>Maska <b>)</b>  : string
                   <b>VALDATE (</b> TextVýraz <b>,</b>Maska <b>)</b> : real

   ■  <b>ČísVýraz</b> .... Datum a čas v interním formátu PC FANDu (real)
   ■  <b>TextVýraz</b> ... Datum a čas v textové presentaci.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t289">funkce pro datum a čas</a>                       <a href="#t296">░ datum a čas</a>
</pre>

<a name="t288"></a>
## maska pro datum

<pre>
                              <b>MASKA PRO DATUM</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Maska</b> pro datum je libovolný řetězec v apostrofech obsahující kromě 
         oddělovačů také speciální znaky pro jednotky <b>datumu</b> a <b>času</b>.
         Tyto znaky budou ve výsledku nahrazeny skutečnými hodnotami.
         Maska určuje způsob zobrazení datumu.
         ■  <b>Y</b>-rok, <b>M</b>-měsíc, <b>D</b>-den,
         ■  <b>h</b>-hodina, <b>m</b>-minuta, <b>s</b>-vteřina, <b>t</b>-setina vteřiny

         Pozor na pravidla dosazení století při použití dvoumístného roku
         v masce, např. ve standardní 'DD.MM.YY', viz. <a href="#t219">instalace konstant</a> -
         parametr <b>Posun implicitního století</b>.
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t289"></a>
## funkce pro datum a čas

<pre>
                           <b>FUNKCE PRO DATUM A ČAS</b>
   ───────────────────────────────────────────────────────────────────────────
   PC FAND obsahuje  datumovou aritmetiku , tj. funkce pro počítání  a převody
   datumu a času. Zahrnuje i práci s interním kalendářem - tabulkou pracovních
   dní, viz. <a href="#t216">Instalační program</a>.  Pokud funkce pracují  s datumem a časem jako
   číselným  výrazem ( typ <b>real</b> ) , jde o stejný  formát jako  FANDovský typ <b>D</b>
   (celá část - pořadové číslo dne od 01.01.0001 a desetinná část - část dne).

   <b>Přehled funkcí:</b>
   ■ <a href="#t287">today</a> ......... aktuální (dnešní) datum
   ■ <a href="#t287">currtime</a> ...... vrací aktuální čas
   ■ <a href="#t287">strdate</a> ....... převod datumu a času na string
   ■ <a href="#t287">valdate</a> ....... převod datumu a času v textové podobě na interní (real)
   ■ <a href="#t293">addwdays</a> ...... přičtení počtu dní daného typu k počátečnímu datu
   ■ <a href="#t293">addmonth</a> ...... k počátečnímu datu přičte zadaný počet měsíců
   ■ <a href="#t295">difwdays</a> ...... počet dní daného typu v časovém období
   ■ <a href="#t295">difmonth</a> ...... počet měsíců v časovém období

   ───────────────────────────────────────────────────────────────────────────
   <a href="#t219">Instalace konstant</a>
</pre>

<a name="t291"></a>
## typeday

*Též:* `typ dne`

<pre>
                                  <b>TYP DNE</b>
   ──────────────────────────────────────────────────────────────────────────

   Funkce <b>typeday</b> vrací typ zadaného dne.

   ██ Syntaxe:     <b>TYPEDAY (</b> Datum <b>)</b> : real

   ■  <b>Datum</b> ...... Číselný výraz, datum v interním formátu PC FANDu.
   ■  <b>Typ dne</b> .... Návratová hodnota :
                   <b>0</b> - pracovní den
                   <b>1</b> - sobota, den pracovního volna
                   <b>2</b> - neděle, den pracovního klidu
                   <b>3</b> - svátek

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t289">funkce pro datum a čas</a>           <a href="#t216">instalační program</a>          <a href="#t296">░ datum a čas</a>
</pre>

<a name="t293"></a>
## addmonth

*Též:* `addwdays`

<pre>
                               <b>PŘIČTENÍ ČASU</b>
   ──────────────────────────────────────────────────────────────────────────

   Funkce <b>addwdays</b>  přičte ( resp. odečte ) k počátečnímu datumu zadaný počet
   dní daného typu.  Obdobně funkce <b>addmonth</b> přičte zadaný počet měsíců.
   Návratové hodnoty jsou v interním datovém formátu PC FANDu.

   ██ Syntaxe:       <b>ADDWDAYS (</b> PočDatum <b>,</b> OKolikDní [<b>,</b>Typ] <b>)</b> : real
                     <b>ADDMONTH (</b> PočDatum <b>,</b> OKolikMěsíců <b>)</b>     : real

   ■  <b>PočDatum</b> ..... Číselný výraz, počáteční datum v interním formátu.
   ■  <b>OkolikDní</b> .... Číselný výraz, o kolik dní (zadaného typu) se má datum
                     posunout. Když je záporný, provede se odečtení.
   ■  <b>OKolikMěsíců</b>.. obdobně
   ■  <b>Typ</b> .......... <a href="#t291">Typ dne</a> o které se má datum posunout, pokud není zadán,
                     použije se <b>typ 0</b> (pracovní den). Na výsledek funkce
                     addwdays má tedy vliv i instalovaná tabulka pracovních 
                     dní viz. <a href="#t216">Instalační program</a>.
   ■  Omezení ...... <b>AddWDays</b> má horní hranici 2020 (obrana proti zacyklení)

   Inverzní funkce : <a href="#t295">difwdays</a>, <a href="#t295">difmonth</a>

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t289">funkce pro datum a čas</a>            <a href="#t296">░ datum a čas</a>
</pre>

<a name="t295"></a>
## difmonth

*Též:* `difwdays`

<pre>
                               <b>ODEČTENÍ ČASU</b>
   ──────────────────────────────────────────────────────────────────────────

   Funkce <b>difwdays</b>  vrací počet pracovních dnů ( resp. dnů daného typu ) mezi
   dvěma daty. Analogicky <b>difmonth</b>  vrací rozdíl mezi daty v měsících. Funkce
   fungují v intervalu 1.1.1910 .. 31.12.2019, nezapočítává Datum1
              
   ██ Syntaxe:       <b>DIFWDAYS (</b> PočDatum <b>,</b>KoncDatum [<b>,</b>Typ] <b>)</b> : real
                     <b>DIFMONTH (</b> PočDatum <b>,</b>KoncDatum <b>)</b>        : real

   ■  <b>PočDatum</b>       Číselné výrazy, počáteční a koncové datum v interním
   ■  <b>KoncDatum</b> .... formátu. Lze i KoncDatum &lt; PočDatum.
   ■  <b>Typ</b> .......... <a href="#t291">Typ dne</a> o které se započítají do výsledku, pokud není 
                     zadán, použije se <b>typ 0</b> (pracovní den). Na výsledek 
                     funkce difwdays má tedy vliv i instalovaná tabulka 
                     pracovních dní viz. <a href="#t216">Instalační program</a>.

   Inverzní funkce:  <a href="#t293">addwdays</a>, <a href="#t293">addmonth</a>

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t289">funkce pro datum a čas</a>             <a href="#t296">░ datum a čas</a>
</pre>

<a name="t296"></a>
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

<a name="t300"></a>
## length

*Též:* `ord`, `char`

<pre>
                               <b>TEXTOVÉ FUNKCE</b>
   ──────────────────────────────────────────────────────────────────────────

   Délka řetězce.

   ██ syntax:   <b>LENGTH ( </b>TextovýVýraz <b>)</b> : real

   ──────────────────────────────

   Práce s ASCII kódem znaků. Funkce CHAR vrací znak s určitým kódem, zohlední
   aktuální tabulku znaků.  Funkce  ORD naopak vrací ASCII kód určeného znaku.
   Tyto funkce umožňují mimo jiné i vyhodnocení obecného BYTE-streamu.

   ██ syntax:   <b>CHAR (</b> Kód <b>)</b>       : string
                <b>ORD  (</b> TextVýraz <b>)</b> : real

   ■  <b>Kód</b> ......... ASCII kód znaku. Přepočte se modulo 256.
   ■  <b>TextVýraz</b> ... Vezme se první znak řetězce a ORD vrátí jeho kód.
                    ord('') = 0  viz. <a href="#t221">instalace abecedy</a>

   <a href="#t315">zpracování textu</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██    char( 65 )  = A  ;          { = char ( 321 ) }
         ord('Jan')  = 74 ;
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t302"></a>
## val

*Též:* `str`

<pre>
                          <b>KONVERZNÍ TEXTOVÉ FUNKCE</b>
   ──────────────────────────────────────────────────────────────────────────
   Funkce <b>val</b> převede text na číslo. Pro převod textového zápisu datumu nebo
   (a) času na číslo použijte <a href="#t287">ValDate</a>.

   ██ syntaxe :         <b>VAL ( </b>TextVýraz <b>)</b> : real
   ────────────

   Převod číselného výrazu na text.

   ██ syntaxe   1:      <b>STR (</b> ČísVýraz<b>,</b> Šířka<b>,</b> PočetDesMíst<b> )</b> : string
                2:      <b>STR (</b> ČísVýraz<b>,</b> Maska <b>)</b> : string

   ■  <b>Šířka</b> ..........  Číselný výraz, celková délka výsledného řetězce.
   ■  <b>PočetDesMíst</b> ...  Číselný výraz, počet míst za desetinnou tečkou.
                        Dle počtu des. míst dojde k případnému zaokrouhlení.
                        Naopak při přetečení před des. tečkou je výsledkem
                        řetězec v plné délce.
                        Je-li -1, vypíše se číslo v exponenciálním tvaru.
   ■  <b>Maska</b> ... Textový výraz. Následující znaky mají speciální význam.
                '<b>,</b>'  desetinná čárka (potlačena, pokud před ní nebudou cifry)
                '<b>.</b>'  tečka oddělující skupiny cifer
                '<b>_</b>'  významné cifry, znaménko zleva nebo mezera
                '<b>-</b>'  mezery, pokud je číslo kladné
            '<b>*</b>','<b>0</b>'  významné cifry nebo znaménko zleva. Všechny '<b>_</b>' vpravo 
                od '<b>*</b>','<b>0</b>' budou nahrazeny cifrou. Pokud je číslo nenulové
                budou cifry vystupovat počínaje nejpozději prvním místem
                před čárkou. Místa za čárkou musí být ve výrazu jako zlomek.
                Nemaskované cifry resp. znaménko se připojí zleva (přetečení).

   <a href="#t315">zpracování textu</a>

   <b>░░░░░░░░░░░░</b>     ██  str(625.128,5,2) = '625.13'
   <b>░░příklady░░</b>         str(625.128,1,0) = '625'
   <b>░░░░░░░░░░░░</b>         str(625,6,0)     = '   625'
                        str(1234.25,'__.___,__') = ' 1.234,25'
                        str(1234,'000.000') = '001.234'
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t305"></a>
## nodiakr

*Též:* `upcase`, `lowcase`

<pre>
                       <b>PRÁCE S DIAKRITIKOU A ABECEDOU</b>
   ──────────────────────────────────────────────────────────────────────────

   Převod malých písmen na velká a naopak. 

   ██ syntaxe:      <b>UPCASE ( </b> TextVýraz <b>)</b> : string
                    <b>LOWCASE( </b> TextVýraz <b>)</b> : string

   ■  Upcase převede všechna malá písmena ve výrazu na velká, lowcase
      naopak velká na malá.
   ─────────────────────────────

   Odstranění diakritiky z textu.

   ██ syntaxe:      <b>NODIAKR( </b> TextVýraz <b>)</b> : string

   <b>Poznámka:</b> Důležité je, že tyto funkce zohledňují instalované národní 
             prodstředí ( kód Kamen. a Latin 2). Tzn. že pro některé znaky 
             s diakritikou jsou výsledky vizuálně shodné ale interní kódy 
             převedených znaků se mohou lišit.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t315">zpracování textu</a>
</pre>

<a name="t307"></a>
## copy

*Též:* `repeatstr`

<pre>
                               <b>TEXTOVÉ FUNKCE</b>
   ──────────────────────────────────────────────────────────────────────────
  
   Výběr podřetězce z textu.Funkce vrátí podřetězec textu <b>TextVýraz</b> délky
   <b>Délka</b>, který se vybere od zadané pozice.

   ██ syntaxe:      <b>COPY (</b> TextVýraz<b> ,</b>OdPozice<b> ,</b>Délka<b> )</b> : string

   ──────────────────────────

   Konstrukce řetězce opakováním.

   ██ syntaxe :     <b>REPEATSTR (</b> TextVýraz <b>,</b>PočetOpakování <b>)</b> : string

   <a href="#t315">zpracování textu</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>           ██   copy('Jan Novák',5,5) = 'Novák'
   <b>░░░░░░░░░░░░</b>
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t310"></a>
## replace

*Též:* `leadchar`, `trailchar`

<pre>
                             <b>NAHRAZENÍ V TEXTU</b>
   ──────────────────────────────────────────────────────────────────────────
   Odstranění nebo nahrazení vedoucích (LEADCHAR) nebo ukončujících
   (TRAILCHAR) znaků v textu.

   ██ syntaxe:      <b>LEADCHAR  ( '</b>Zn1<b>'</b> <b>,</b> Text [<b>,'</b>Zn2<b>'</b> ] ) : string
                    <b>TRAILCHAR ( '</b>Zn1<b>'</b> <b>,</b> Text [<b>,'</b>Zn2<b>'</b> ] ) : string
   ■  <b>Zn1</b>,<b>Zn2</b> ..... Znak Zn1 je odstraněn z textu <b>Text</b>, nebo, pokud je
                    uveden znak Zn2, je tímto nahrazen.

   ──────────────────────────

   Nahrazení podřetězce v textu jiným řetězcem. Na rozdíl od předchozích
   funkcí jsou nahrazeny všechny výskyty, nejen první či poslední.

   ██ syntaxe:      <b>REPLACE (</b> Co<b> ,</b> Kde <b>, </b>Čím [<b>,'</b>Podmínky<b>'</b>] <b>)</b> : string

   ■ <b>Co</b>,<b>Kde</b>,<b>Čím</b> ... Textové výrazy, význam evidentní z názvu.
   ■ <b>Podmínky</b> ..... viz. funkce <a href="#t312">pos</a>


   <a href="#t315">Zpracování textu</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>


   ██   leadchar('x',trailchar('x','xxxAAAAxx')) = 'AAAA'

   ██   Funkce leadchar a trailchar se nejčastěji používají k odstranění
        přebytečných mezer z údajů typu A,n , což jsou textové řetězce
        pevné délky.

        podmínka pro editaci všech Nováků se musí zadat

        trailchar(' ',Prijmeni)='Novák')

        Podmínka Prijmeni='Novák'  by vybrala práznou množinu, neboť při
        porovnání musí souhlasit i délky řetězců. Mezera je také znak.
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t312"></a>
## equmask

*Též:* `pos`

<pre>
                        <b>POROVNÁNÍ TEXTOVÝCH ŘETĚZCŮ</b>
   ──────────────────────────────────────────────────────────────────────────
   Hledání pozice podřetězce textu podle stanovených podmínek porovnání.

   ██ syntaxe:   <b>POS( </b>Podřetězec <b>,</b>Text [<b>,'</b>Podmínky<b>'</b>] [<b>,</b>KterýVýskyt] <b>)</b> : real

   ■  <b>Podřetězec</b> ... Textový výraz, který hledáme v řetězci <b>text</b>.
   ■  <b>Text</b> ......... Textový výraz, který je prohledán.
   ■  <b>Podmínky</b> ..... Textová konstanta - apostrofy se píší (nikoliv výraz).
                     Může obsahovat tyto znaky (jeden nebo kombinace ):
                     <b>w</b> ... jen celá slova, ne složeniny
                     <b>~</b> ... <a href="#t267">lexikální porovnání</a> podle národní abecedy
                     <b>u</b> ... bez rozlišení malých a velkých písmen (upcase)
   ■  <b>KterýVýskyt</b> .. Kolikátý výskyt od počátku hledáme.
   ──────────────────
   Porovnání textového řetězce na masku. Lze použít ve všech kapitolách.

   ██ syntaxe:   <b>EQUMASK( </b>Text <b>,</b> Maska <b>)</b> : boolean

   ■  <b>maska</b> .... Textový výraz. Je to vzor, na který se provede porovnání
                 <b>textu</b>. Maska nemusí být jednoznačná, ale mohou se v ní 
                 vyskytovat speciální znaky (tzv. wildcards) :
                 <b>?</b>   nahradí se libovolným znakem - jen pro danou pozici
                 <b>*</b>   nahradí se libovolnou kombinací znaků (řetězcem)

   <a href="#t315">zpracování textu</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██  pos( 'lhota', 'Dolní Lhota')      = 0 ;
       pos( 'lhota', 'Dolní Lhota','<b>u</b>' ) = 7 ;
       pos( 'dolni', 'Dolní Lhota','<b>u</b>' ) = 0 ;              { vadí <b>i</b> &amp; <b>í</b> }
       pos( 'dolní', 'Dolní Lhota','<b>~</b>' ) = 1 ;
       pos( 'lh', 'Dolní Lhota','<b>w~</b>' )   = 0 ;          { jen celá slova }
       pos( 'lh', 'Dolní Lhota','<b>u</b>' )    = 7 ;
       pos( 'lh', 'Dolní Lhota','<b>u</b>',2 )  = 0 ;    { lh tam je jen jednou }

   ██  equmask( 'Dolní Lhota',  'Lhota' ) = FALSE ;
       equmask( 'Dolní Lhota', '*Lhota' ) = TRUE ;
       equmask( 'Dolní Lhota', '?Lhota' ) = FALSE ;
       equmask( 'Dolní Lhota', '??????Lhota' ) = TRUE ;

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t314"></a>
## linecnt

*Též:* `copyline`

<pre>
                           <b>ZPRACOVÁNÍ ŘÁDKU TEXTU</b>
   ──────────────────────────────────────────────────────────────────────────
   Délka  <b>volných textů</b>  ( a lokálních proměnných typu <b>string</b> ) je omezena na
   65 000 B. Parametry funkcí jsou textové a číselné <b>výrazy</b>.

   Výběr jednoho nebo více řádků z textu od zadaného počátečního řádku.

   ██ syntaxe :        <b>COPYLINE (</b> Text <b>,</b> ČísloŘádku [ <b>,</b>Počet ] <b>)</b> : string

   ─────────────────────────────

   Počet řádků textu.
   
   ██ syntaxe :        <b>LINECNT (</b> TextVýraz <b>)</b> : real

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t315">zpracování textu</a>
</pre>

<a name="t315"></a>
## zpracování textu

<pre>
                <b>ZPRACOVÁNÍ TEXTU</b> - přehled funkcí a příkazů
    ─────────────────────────────────────────────────────────────────────────
    <a href="#t300">Char</a> ............... vrací znak s určitým ASCII kódem
    <a href="#t307">Copy</a> ............... výběr podřetězce
    <a href="#t314">CopyLine</a> ........... načtení určitého řádku z víceřádkového textu
    <a href="#t312">EquMask</a> ............ porovnání řetězce na masku
    <a href="#t310">LeadChar</a> ........... odstranění nebo nahrazení vedoucích znaků
    <a href="#t300">Length</a> ............. délka textu
    <a href="#t314">LineCnt</a> ............ počet řádků textu
    <a href="#t305">LowCase</a> ............ převod velkých písmen na malé v textu
    <a href="#t305">NoDiakr</a> ............ odstranění diakritiky z textu
    <a href="#t300">Ord</a> ................ hodnota ASCII kódu znaku
    <a href="#t312">Pos</a> ................ pozice výskytu podřetězce v textu
    <a href="#t307">RepeatStr</a> .......... generování textu opakováním řetězce (nebo znaku)
    <a href="#t310">Replace</a> ............ nahrazení podřetězce v textu jiným řetězcem
    <a href="#t302">Str</a> ................ převod čísla na text
    <a href="#t287">StrDate</a> ............ převod datumu (času) z číselné do textové formy
    <a href="#t310">TrailChar</a> .......... odstranění nebo nahrazení vedoucích znaků
    <a href="#t305">UpCase</a> ............. převod malých písmen na velké v textu
    <a href="#t302">Val</a> ................ převod textového zápisu čísla na číselný typ (real)
    ─────────────────────────────────────────────────────────────────────────
</pre>

<a name="t318"></a>
## ovládání zobrazení grafu

*Též:* `grafy`

<pre>
                          <b>OVLÁDÁNÍ ZOBRAZENÍ GRAFU</b>
   ──────────────────────────────────────────────────────────────────────────

   Při zobrazení grafu jsou tři možnosti chování:

   ■ <a href="#t319">tisk grafu</a> bez dotazu, po vytištění pokračuje úloha dalším příkazem
   ■ zobrazení s časovou prodlevou (<b>demonstrace</b>), prodloužení stiskem klávesy
   ■ normální zobrazení - čeká na stisk klávesy :

     <b>F6</b>,<b>ShiftF6</b> - tisk grafu, lze přerušit klávesou ESC
     <b>F9</b>,<b>ShiftF9</b> - zápis do souboru GRAPH.PCX (v aktuálním adresáři,formát <b>PCX</b>)
                  Při interaktivní práci lze jméno souboru změnit
                  Konec ukládání je zvukově signalizován.
     <b>F4</b>,<b>Shift4</b>  - graf bude zobrazen inverzně
     <b>ESC</b>        - ukončení zobrazení grafu

   <b>Shift</b> - klávesy se vztahují k aktuálnímu oknu, def. parametrem WW

   Pokud je při zobrazení <b>PCX</b> obraz větší než stanovené okno,lze v okně
   obrazem pohybovat:               <b>kurzor. šipky</b> - malý krok
                                       <b>PgUp</b>, <b>PgPn</b> - o celé okno vertikálně
                             <b>Ctrl-Right</b>,<b>Ctrl-Left</b> - o celé okno horizontálně
   
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t319"></a>
## tisk grafu

<pre>
                                 <b>TISK GRAFU</b>
   ──────────────────────────────────────────────────────────────────────────

   Tisk grafu  lze provést  pomocí prostředků  systému MS-DOS ( PrintScreen +
   utilita graphics ). Výhodnější je použití podpory PC FANDu. Tj. klávesy <b>F6</b>
   pro celou obrazovku nebo  <b>ShifF6</b>  pro tisk aktuálního okna. 
   
   ■ Tisk grafu se provede jako kopie jednotlivých bodů obrazovky (pixelů) na
     tiskárnu v poměru 1:1.  Tj. <b>bod</b> na <b>bod</b>.  Samozřejmě zde hraje roli poměr
     rastru obrazovky na hustotu tisku na tiskárně (viz. <a href="#t218">instalace tiskárny</a>).

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t318">ovládání zobrazení grafu</a>
</pre>

<a name="t321"></a>
## lokální sítě

<pre>
                             <b>LAN - LOKÁLNÍ SÍTĚ</b>
   ──────────────────────────────────────────────────────────────────────────
   V lokální  počítačové síti  je možné sdílet FAND , úlohu i datové soubory.
   Před použitím  FANDu v síti je  nutné  nejprve  v  závislosti  na předmětu
   sdílení provést potřebné zásahy do instalace.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t322">sdílení FANDu</a>                             parametry programu <a href="#t325">SHARE</a>
   <a href="#t323">sdílení úlohy</a>                             <a href="#t326">instalace sítě</a>
   <a href="#t324">sdílení datových souborů</a>                  <a href="#t328">používané pojmy</a>
</pre>

<a name="t322"></a>
## sdílení FANDu

<pre>
                            <b>LAN - SDÍLENÍ FANDU</b>
   ──────────────────────────────────────────────────────────────────────────
   Pro sdílení  FANDu  je třeba  nejprve nastavit DOS - atribut ReadOnly  pro
   soubory <b>FAND.EXE</b> a <b>FAND.OVR</b>.

   Před sdíleným  spuštěním  FANDu  je nutné instalovat následující parametry
   operačního systému (<a href="#t212">SET-parametry</a>):

   ■ <b>SET FANDWORK=</b> Cesta k pracovním souborům
     - každý účastník má vlastní adresář na lokálním počítači nebo na serveru

   Soubory FAND.OVR, FAND.CFG, FAND.RES a FANDHLP.* jsou  standardně  hledány  
   ve stejném adresáři, kde je FAND.EXE. Dalšími parametry operačního systému  
   lze standardní hledání změnit:  <b>SET FANDOVR=</b>,  <b>SET FANDCFG=</b>, <b>SET FANDRES=</b>.
   Programem FANDINST je možné dále nastavit "síťové" parametry:

     <b>net delay</b> - opakování blokovaného přístupu
     <b>refresh</b>   - obnova obrazovky v datovém editoru (zvýšení implicitní 
                 hodnoty 5 sec. zmenší zatížení disku serveru a sítě)
 
  ──────────────────────────────────────────────────────────────────────────
  <a href="#t326">instalace sítě</a>            <a href="#t323">sdílení úlohy</a>           <a href="#t324">sdílení datových souborů</a>
</pre>

<a name="t323"></a>
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
 <a href="#t326">instalace sítě</a>     <a href="#t322">sdílení FANDu</a>     <a href="#t324">sdílení datových souborů</a>
</pre>

<a name="t324"></a>
## sdílení datových souborů

<pre>
                       <b>LAN - SDÍLENÍ DATOVÝCH SOUBORŮ</b>
   ───────────────────────────────────────────────────────────────────────────

   Sdílení datových souborů je možné pouze při instalovaném síťovém softwaru !
   ┌─────────────────────────────────────────────────────────────────────────┐
   │  Před spuštěním FANDu je nutné instalovat parametr operačního systému:  │
   │  ■ <b>SET LANNODE=</b> Unikátní číslo stanice v síti (0 až 255)                │
   └─────────────────────────────────────────────────────────────────────────┘
   Datové soubory i  <a href="#t250">katalog</a> jsou implicitně umístěny ve stejném adresáři, kde
   je úloha.  Rozmístění datových souborů je možné změnit v katalogu. Umístění
   katalogu a datových souborů v něm neuvedených lze změnit před spuštěním 
   FANDu nastavením <b>SET FANDDATA</b>. Určení přístupových práv viz. <a href="#t326">instalace sítě</a>.

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
   <a href="#t322">sdílení FANDu</a>        <a href="#t323">sdílení úlohy</a>          <a href="#t329">sdílená editace</a>
</pre>

<a name="t325"></a>
## SHARE

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

   Síť <a href="#t330">Lantastic</a> může použít interní nebo externí SHARE.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t326">instalace sítě</a>               <a href="#t322">sdílení FANDu</a>     <a href="#t323">sdílení úlohy</a>
   <a href="#t324">sdílení datových souborů</a>
</pre>

<a name="t326"></a>
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
          zřejmě uplatní nejvyšší z nich.  Na rozsáhlejších sítích ( <a href="#t331">Novell</a> )
          se vyplatí tyto záležitosti neignorovat.

   ■ Je nutné instalovat dostatečné velký maximální počet současně otevřených
     handlů pro otevírané soubory a také maximální počet současně provedených
     locků na serveru.  Některé  sítě vyžadují  na serveru spuštění  programu
     <a href="#t325">SHARE</a> ( součást operačního systému MS DOS ) s příslušnými  parametry,  u
     jiných je třeba postupovat dle instalačního návodu.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t322">sdílení FANDu</a>        <a href="#t323">sdílení úlohy</a>         <a href="#t324">sdílení datových souborů</a>
   <a href="#t330">Lantastic</a>
</pre>

<a name="t328"></a>
## používané pojmy

*Též:* `net delay`

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
   <a href="#t219">instalace konstant</a>
</pre>

<a name="t329"></a>
## sdílená editace

<pre>
                              <b>SDÍLENÁ EDITACE</b>
   ──────────────────────────────────────────────────────────────────────────
   Běžný uživatel síťové aplikace by si měl být vědom některých odlišností od 
   chování mono-aplikace. Uvědomělým chováním může ovlivnit průchodnost sítě.
   Nejvýrazněji se konkurenční charakter práce projeví při editaci.

   ■  Již samotný  fakt spuštění datového  editoru znamená nastavení jisté, i
      když minimální, úrovně blokování souboru pro ostatní účastníky. Což pro
      ně může znamenat  omezení při jejich  práci , od zpomalení až po úplnou
      blokaci - dle charakteru konkurenčních činností. Proto je dobré editaci
      sdíleného souboru ukončit, pokud momentálně nepracujeme.

   ■  Programátor může zajistit, že editace se po určité době, kdy uživatel
      nestiskl žádnou klávesu, automaticky ukončí. Před tím však 3x obsluhu
      zvukově varuje.

   ■  Při akcích jako např. změna,zápis či zrušení věty se blokování souboru
      dočasně "zvýší" - dle charakteru operace.

   ■  Datový  editor provádí v pravidelných intervalech  automatické načítání
      sdílených  dat z disku - tzv. <b>refresh</b>.  Pouze  takto lze  zjistit  akce
      ostatních účastníků. I prostý refresh zatěžuje síť ( server ). Proto je
      vhodné upravit refresh dle skutečné potřeby. Ten lze upravit buď obecně
      pro všechny editory programem FANDINST (viz. <a href="#t219">instalace konstant</a> ), nebo
      ho může programátor upravit individuálně při každém vyvolání editoru.
      "Snesitelné" hodnoty závisí na počtu stanic a výkonu sítě a serveru.
      Implicitní hodnota 5 sec. se v praxi ukazuje jako dosti nízká.

   ■  Je-li v síti více stanic a dochází-li k nadměrnému blokování souborů,je
      možno  dosáhnout zlepšení  nastavením různého  intervalu  pro opakování
      blokovaného přístupu - <b>net delay.</b>

   ■  <b>Při editaci podmnožiny</b>  nedochází vlivem  práce  ostatních  účastníků k
      posouvání obrazovky ( nové věty, zrušení). V důsledku změny klíče jinou
      stanicí se může stát, že při hledání věty dle klíče ( <b>F3</b> ) je věta sice
      nalezena ale přístup k ní je odmítnut.

   ■  Obnovení obrazovky ( podmnožiny ) dle aktuálního stavu dosáhneme pomocí
      klávesy <b>Ctrl-F2</b> (nefunguje při editaci podle dynamického indexu).

   ■  Pokud se při editaci  souboru najednou  zobrazily  některé věty barevně
      odlišně znamená to, že byly  zrušeny jinou stanicí ( aktualizace pomocí
      Ctrl-F2).

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t328">používané pojmy</a>
</pre>

<a name="t330"></a>
## Lantastic

<pre>
                              <b>LANTASTIC</b>
   ──────────────────────────────────────────────────────────────────────────
   V praxi je provozováno několik verzí této sítě. Nejsou známy nějaké speci-
   fické problémy při instalaci PC FANDu nebo úloh. Nižší verze LANTASICu 
   používají externí (DOSový) program <a href="#t325">SHARE</a>.

   Verze LANTASTICu 6.0 může alternativně použít i interní SHARE:
   Spustíme program <b>NET_MGR</b>, nabídky <b>Server startup parameters</b> (rolovat dolů)
                                     <b>Internal share</b>

   volby: <b>Internal share</b> ... enabled/disabled (povolen/vypnut interní share)
          <b>Share locks</b> ...... maximální počet zámků (viz. SHARE /L: )
          <b>Name space</b> ....... velikost tabulky otevřených souborů (SHARE /F:)
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t331">Novell</a>
</pre>

<a name="t331"></a>
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
   <a href="#t330">Lantastic</a>
</pre>

<a name="t332"></a>
## Windows

<pre>
                                 <b>WINDOWS</b>
   ──────────────────────────────────────────────────────────────────────────

   Sítě Windows For Workgroups (WFW) a Windows 95, případně smíšené jsou
   typu peer to peer. Přesto lze z důvodů bezpečnosti doporučit vyhrazení 
   jednoho počítače jako datového serveru. Není třeba používat program SHARE.

   Určitou slabinou (oproti Novellu) je nedostatek informací o interních
   strukturách systému (.INI soubory), nutných pro řešení občasných problémů.

   Známé problémy:

   ■ Při tisku na <b>síťovou</b> tiskárnu pod WFW nelze nastavit timeout. Jde o
     analogii parametru TI novelské utility CAPTURE. Implicitně platí 45 sec.
     což znamená, že tisk nastane až zhruba minutu po té, co byl odeslán z
     PC FANDu (aplikace).
     Od verze PC FANDu 4.0 je toto řešeno nastavením FANDINST - TISKARNY -
     - TIMEOUT=255. Viz. <a href="#t218">instalace tiskárny</a>.

     Tento problém lze řešit i úpravou souboru <b>SYSTEM.INI</b>, sekce
     ...
     [network]
     PrintBufTime=PočetSec   (třeba 5 sec)
     ...
     [ifsmgr] 
     PrintBufTime=PočetSec   (třeba 5 sec)


   ■ Občasné problémy při tisku nastávají i ve WIN95. Většinou jde o ztrátu
     několika znaků na konci sestavy. Není pozorován ani dokumentován 
     jednoznačný vliv instalačních parametrů tiskárny ve WIN95 na tyto
     problémy. Většinou pomůže znovu instalovat tiskárnu a při tom si dát
     pozor na povolení tisku z DOS-aplikací.

     Nastavení tiskárny -  volba Start / Settings / Printer / Properties /
                           Details.
     Pozor na:
     Port  Settings / MS-DOS print  job properties   (doporučujeme vypnout)
     Port  Settings /  Check port before printing (zapnout)
     Timeout settings - čekací doby (např. 15 a 45)

   ■ Nastavení HARDWARE, doporučujeme projít nastavení BIOSu pomocí <b>SETUP</b>u.
     U nových desek věnujte pozornost volbě "Peripheral Setup" z voleb "SPP"
     (standard pararel port), "ECP" (extended capabilities port)  vyzkoušejte
     "ECP". (CHIP 2/96,str.144)

   ■ Při připojení stanice s WIN95 k serveru Novell nelze sdílet RdOnly 
     soubory (např. úlohy). Jde o stejný problém, který se řeší v konfigurač-
     ním souboru NET.CFG parametrem READ ONLY COMPATIBILITY=OFF. Ten ovšem 
     nelze v distribuční verzi WIN95 nastavit.
     Řešení : - Ve FANDu 4.0 se úlohy při provedení otevírají rovnou RdOnly
                bez testování atributů souboru.
              - Nové 32-bit. drivery pro Novell pod WIN95 jsou dostupné na
                Internetu. V nich lze parametr Read Only Compatibility nastavit.

   ■ DOS - klient pro sítě WINxy má více variant. Při použití základní verze
     NET START <b>BASIC</b> mohou být problémy s textovými soubory, pokud jsou jinde 
     než na lokálním disku.
     Startujte proto síť pomocí NET START <b>FULL</b> nebo je třeba v souboru
     SYSTEM.INI nastavit správné implicitní hodnoty pro klienta, to jest
     [network]
     AutoStart=Full
     PreferredRedir=Full

   ■ Windows 95 / 98
     Při problémech s datovými soubory zkuste nastavení parametrů:
     Ovládací panely -&gt; Systém -&gt; Výkon -&gt; Systém souborů -&gt;
     Co dělat v nesnázích.
     Zde naleznete 6 zaškrtávacích polí, jejichž nastavením lze ovlivnit
     souborový systém Windows. Obecné optimální nastavení není známo,
     je třeba prostě něco změnit a ověřit chování aplikace.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t331">Novell</a>
</pre>

<a name="t333"></a>
## ODBC

<pre>
                                   <b>ODBC</b>
    ───────────────────────────────────────────────────────────────────────
    Pod zkratkou ODBC (<b>O</b>pen <b>D</b>ata<b>b</b>ase <b>C</b>onnectivity) se skrývá mechanismus
    otevřeného rozhraní (interface) pro práci s databázemi (datovýni soubory).
    
    Pro aplikační program přináší zabudování rozhraní ODBC možnost zpracování
    dat uložených v libovolné databázi, pro kterou existuje tzv. <b>ODBC-ovladač</b>.
    V podstatě jde o technologii zpracování <b>client-server</b>.
    Pro FANDovské datové soubory existuje ODBC ovladač pro platformy
    16 a 32 bit. Windows.
    To znamená, že z libovolné Windows-aplikace, která podporuje ODBC se
    lze "dostat" na datové soubory provozovaných FAND-aplikací. Pro příklad
    uveďme balík Microsoft Office, WinBase602, Delphi, Java atd.

    <b>ODBC ovladač pro soubory PC FANDu</b>
    je samostatný produkt, distribuovaný firmou ALIS spol.s.r.o. 
    Demoverze je dostupná na <b>www.alis.cz</b>.
    Jelikož datové soubory PC FANDu neobsahují deklaraci, je pro přístup
    přes ODBC potřeba textový soubor s deklaracemi FAND-souborů, a to
    v syntaxi jazyka SQL. 
    Je to proto, že celá filosofie ODBC je založena na jazyce <b>SQL</b> jako
    obecném databázovém jazyce.
    Pro tento definiční soubor je vyhrazena přípona <b>RDA</b>. Pro vytvoření
    tohoto souboru je většinou nutná spolupráce s autorem FAND-aplikace,
    jinak není potřeba do úlohy PC FANDu provádět jakékoliv zásahy.
    Kromě deklarace dat obsahuje soubor RDA i definice uživatelů 
    a přístupových práv k souborům - tabulkám.

    Možnosti použití ODBC ovladače vyplývají z obecných vlastností
    technologie ODBC. Z hlediska aplikace PC FANDu je důležité to, že 
    přes ODBC ovladač je možno sdílet datové soubory spuštěné FAND-aplikace.
    Podrobnosti k instalaci jsou uvedeny v dokumetaci, která je i součástí 
    demoverze ODBC-FAND-ovladače.

    ───────────────────────────────────────────────────────────────────────
</pre>
