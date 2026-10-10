# Uživatelské rozhraní

<a name="t58"></a>
## klávesnice

<pre>
                                 <b>KLÁVESNICE</b>
   ──────────────────────────────────────────────────────────────────────────
   V datovém a textovém  editoru  vyvolá  kombinace kláves <b>Alt-F8</b> nabídku pro
   přepínání mezi různým rozložením znaků na klávesnici:

   ■ <a href="kontextova-napoveda.md#t60">klávesnice standardní</a> .......... bez předefinování klávesnice
   ■ <a href="kontextova-napoveda.md#t62">klávesnice česká</a> ............... jako český psací stroj
   ■ <a href="kontextova-napoveda.md#t64">klávesnice česká amatérská</a> ..... mění pouze horní řadu kláves
                                      (malá písmena pod čísly, čárka/háček
                                      a některé úpravy pro slovenčinu)
   ■ <a href="kontextova-napoveda.md#t66">klávesnice slovenská</a> ........... jako slovenský psací stroj
   ■ <a href="kontextova-napoveda.md#t68">klávesnice německá amatérská</a> ... německé rozložení

   Lze instalovat, která klávesnice bude aktivní po startu PC FANDu.
   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t223">Instalace konstant</a>
</pre>

<a name="t201"></a>
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

        <a href="#t204">volba z nabídky</a> .................... pravidla pro práci z menu
        <a href="#t207">hlavní menu PC FANDu</a> ........................ základní nabídka
        <a href="#t205">výběr ze seznamu</a> .......... pravidla pro výběr z množiny prvků
        <a href="editory.md#t244">datový editor</a> ................................... práce s daty
        <a href="editory.md#t228">textový editor</a> ................................. práce s texty
        <a href="jazyk.md#t654">výrazy</a> ................................ pravidla tvorby výrazů
        <a href="#t58">klávesnice</a> ....................... podpora národního prostředí
        <a href="#t202">myš</a> .................................. podpora myši v PC FANDu
     ────────────────────────────────────────────────────────────────────
</pre>

<a name="t202"></a>
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
   <a href="#t203">myš v datovém a textovém editoru</a>     <a href="#t490">myš v proceduře</a>    <a href="prostredi.md#t223">instalace konstant</a>
</pre>

<a name="t203"></a>
## myš v datovém a textovém editoru

<pre>
                      <b>MYŠ V DATOVÉM A TEXTOVÉM EDITORU</b>
   ──────────────────────────────────────────────────────────────────────────

    ■ <b>datový editor</b> ... stisk na údaji: kurzor přejde k údaji
                        dvojitý stisk na údaji: <b>INS</b> (přechod k editaci údaje)
                                                nebo <b>ENTER</b> - viz. <a href="prostredi.md#t283">mode</a>='^m'
                        mode='F1 | F124' a click na nápovědě: jako Ctrl-F1

    ■ <b>textový editor</b> ...stisk - přenesení kurzoru na danou pozici

    ■ <b>nápověda kláves</b> ... v posledním řádku obrazovky může být buď standardní
               nebo alternativní - tj. určená parametry editace <a href="#t523">last</a>, <a href="#t523">alt</a>,
               <a href="#t287">ctrl</a>, <a href="#t523">shift</a>. Názvy kláves musí být uvedeny přesně a zdůrazněny
               ^P^W. Stisk na zdůrazněném názvu klávesy generuje tuto klávesu
               podržení generuje opakování. V "ikoně" pro myš nemusí být
               uveden název kontrolní klávesy. Např. v <b>Ctrl</b> nápovědě generuje
               ikona <b>F8</b> kombinaci CtrlF8.
      Klávesy: F1..F10, AltF1..AltF10, ShiftF1..ShiftF10, CtrlF1..CtrlF10,
               ^Q┘, Enter, Alt, Ctrl, Shift, Esc, CtrlY,
               ↑,↓, PgUp,PgDn,CtrlHome,CtrlEnd, 
               ikona pro tabelátor '\196\16\179' , ShiftTab('\179\17\196')
               analogicky funguje při chybovém hlášení také stisk na 'F10'
               resp. 'ShiftF7'

    ■ <b>helpy</b>... stisk na hesle: jako Enter na hesle, další v nápovědě 25.řádek

    ─────────────────────────────────────────────────────────────────────────
</pre>

<a name="t204"></a>
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
   <a href="#t202">myš</a>
</pre>

<a name="t205"></a>
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
   <a href="#t202">myš</a>
</pre>

<a name="t207"></a>
## hlavní menu PC FANDu

<pre>
                            <b>HLAVNÍ MENU PC FANDu</b>
   ──────────────────────────────────────────────────────────────────────────
   Nabídka obsahuje následující volby:

   ■ <b>L</b>adit úlohu ....... programování úlohy v <b>projektantském prostředí</b>
   ■ <b>P</b>rovést úlohu ..... spuštění úlohy v <b>uživatelském režimu</b>
   ■ <b>I</b>nstalace úlohy ... změna <b>katalogu</b> souborů a seznamu oprávněných <b>uživatelů</b>
   ■ <b>E</b>ditace textu ..... vyvolání textového editoru pro textové soubory na disku
   ■ <b>M</b>sDos ............. odskok do operačního systému (návrat příkazem <b>exit</b>)
   ■ <b>K</b>onec ............. ukončení PC FANDu (též <b>Esc</b>)
 
   Spuštěním PC FANDu s parametrem se hlavní menu nezobrazí a spustí se rovnou
   požadovaná akce:

   ■ FAND <b>NázevÚlohy</b> ..... spuštění úlohy v uživatelském režimu
   ■ FAND NázevÚlohy <b>D</b> ... nastavení programátorského prostředí pro úlohu
   ■ FAND NázevTextu <b>T</b> ... textový editor pro zadaný textový soubor
   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t253">projektantské prostředí</a>     <a href="prostredi.md#t641">katalog</a>     <a href="dalsi-kapitoly.md#t644">seznam uživatelů</a>    <a href="editory.md#t228">textový editor</a>
</pre>

<a name="t287"></a>
## ctrl

<pre>
                      <b>CTRL</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ edit(...<b>ctrl</b>) ..... <a href="procedury.md#t510">volání editoru</a>: <a href="#t523">alternativní nápověda</a>
   ■ report(...<b>ctrl</b>) ... <a href="prostredi.md#t562">parametry automatické sestavy</a>: řídící údaje součt.
                                                        sestavy

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t488"></a>
## kódy řídících a funkčních kláves

<pre>
                      <b>KÓDY ŘÍDÍCÍCH A FUNKČNÍCH KLÁVES</b>
 ──────────────────────────────────────────────────────────────────────────────
 Kódy funkčních kláves se zadávají (např. v proceduře <b>setkeybuf</b>) se znakem  '<b>\</b>'
 a dekadickou hodnotou kódu podle tabulky):

 ┌─────────────────────────────────────────┐ ┌──────────────────────┐
 │ <b>rozšířené klávesy s prefixem \0</b>:        │ │ <b>klávesy bez prefixu</b>: │
 │ Home ....... 71 │ Ctrl-Šipka vlevo  115 │ │ Tab ........  9      │
 │ Šipka nahoru 72 │ Ctrl-Šipka vpravo 116 │ │ Esc ........ 27      │
 │ PgUp ....... 73 │ Ctrl-End ........ 117 │ │ Enter ...... 13      │
 │ Šipka vlevo  75 │ Ctrl-PgDn ....... 118 │ │ ^A až ^Z ... 1 až 26 │
 │ Šipka vpravo 77 │ Ctrl-Home ....... 119 │ └──────────────────────┘
 │ End ........ 79 │ Ctrl-PgUp ....... 132 │
 │ Šipka dolů . 80 │ Shift-Tab .......  15 └──────────┐
 │ PgDn ....... 81 │ F1 až F10 ............ 59 až  68 │
 │ Ins ........ 82 │ Ctrl-F1 až Ctrl-F10 .. 94 až 103 │
 │ Del ........ 83 │ Alt-F1 až Alt-F10 ... 104 až 113 │
 │                 │ Shift-F1 až Shift-F10  84 až  93 │
 │                 │ Ctrl-= ..................... 131 │
 └─────────────────┴──────────────────────────────────┘
 ░ setkeybuf('\0\132\0\80');                             {Ctrl-PgUp Šipka dolů}
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t794">tabulky znaků</a>
</pre>

<a name="t490"></a>
## myš v proceduře

*Též:* `mouseevent`

<pre>
                              <b>MYŠ V PROCEDUŘE</b>
   ──────────────────────────────────────────────────────────────────────────
   Příkazy a konstrukce PC FANDu , které mají interně zabudovánu komunikaci s
   uživatelem, zahrnují i podporu pro práci s myší ( datový a textový editor,
   menu, selectstr, ...).  Ve zbývajících  částech aplikací je možno  dořešit
   komunikaci pomocí myši použitím následujících příkazů a funkcí.

   Čtení událostí myši z fronty.

   ██ syntaxe:      <b>MOUSEEVENT (</b> MaskaUdálostí <b>)</b> : boolean

   ■  <b>MaskaUdálostí</b> Celé číslo, konstanta, udává binární masku druhu události
                    (sčítáme čísla událostí které chceme zpracovat)
                           <b>1</b>     stisk  klávesy
                           <b>2</b>     pustit klávesu
                           <b>4</b>     přesun myši
   ■  Kapitoly .... P,D
      Funkce čte události myši z fronty, a to tak dlouho, až narazí na událost
      zadaného  druhu nebo  je fronta prázdná.  V případě prázdné fronty vrátí
      false. Až po úspěchu funkce mouseevent můžeme událost blíže specifikovat
      pomocí funkcí <a href="jazyk.md#t491">ismouse</a>, <a href="procedury.md#t495">mousein</a>, <a href="procedury.md#t495">mousex</a> a <a href="procedury.md#t495">mousey</a>.

   ──────────────────────────────────────────────────────────────────────────
   <a href="procedury.md#t495">setmouse</a>          <a href="#t202">myš</a>          <a href="#t203">myš v datovém a textovém editoru</a>
</pre>

<a name="t523"></a>
## alternativní nápověda

*Též:* `alt`, `shift`, `last`

<pre>
   Ctrl-Home                  <b>ALTERNATIVNÍ NÁPOVĚDA</b>
   ──────────────────────────────────────────────────────────────────────────

   Pomocí těchto parametrů lze ovládat nápovědní (25.) řádek datového editoru
   Symboly funkčních kláves  (Ctrl,Alt,Shift,*F1..*F10) a dalších kurzorových
   kláves, zvýrazněné pomocí <b>^W</b> generují při stisku (click) myši odpovídající 
   akci.

   ██  <b>last</b>=TextVýraz .... nahradí standardní poslední (25.) řádek editoru

   ██  <b>ctrl</b>=TextVýraz .... alternativní text nápovědy po delším stisku <b>Ctrl</b>
   ██  <b>alt</b>=TextVýraz  ....                 -    "    -                 <b>Alt</b>
   ██  <b>shift</b>=TextVýraz ...                 -    "    -                 <b>Shift</b>

   V textu alternativní nápovědy není třeba zadávat celou "ikonu" ale stačí
   uvést jen vlastní funkční klávesa. Např. v nápovědě <b>Ctrl</b> se bere ikona <b>F8</b>
   jako Ctrl-F8.

   Nápověda kláves v 25. řádku může být "překryta" nápovědou datového editoru
   viz. <b>mode='F1'</b> (<a href="prostredi.md#t530">módy datového editoru</a>). Lepší řešení je použít <b>mode='F124'</b>.
   
   ─── Ctrl-End ─────────────────────────────────────────────────────────────
   <a href="#t203">myš v datovém a textovém editoru</a>          <a href="editory.md#t595">edittxt</a>
</pre>

<a name="t753"></a>
## grafy

*Též:* `graph`

<pre>
                          <b>PŘÍKAZ GRAPH</b>               <a href="#t781">ovládání zobrazení grafu</a>
   ──────────────────────────────────────────────────────────────────────────
   Příkaz procedury pro grafické zobrazení datových souborů (jen při 80x25).

   ██ tři varianty syntaxe:
        <b>GRAPH;</b> ....... interaktivní práce s grafem (bez implicitních hodnot)
        <b>GRAPH (</b><a href="#t780">GF</a><b>=</b>TextVýraz<b>);</b> ........ zobrazení grafického souboru *.PCX

        <b>GRAPH (</b> NázevSouboru [<b>/</b>NázevKlíče ]<b>,</b> ............... graf souboru
              <b>(</b> SeznamÚdajů <b>)</b>
              [<b>,</b> <a href="#t760">Type</a><b>=</b>TextVýraz]   [<b>,</b><a href="#t758"> Head</a><b>=</b>TextVýraz]   [<b>,</b> <a href="#t758">HeadX</a><b>=</b>TextVýraz]
              [<b>,</b> <a href="#t758">HeadY</a><b>=</b>TextVýraz]  [<b>,</b> <a href="#t758">HeadZ</a>n<b>=</b>TextVýraz] [<b>,</b> <a href="#t758">DirX</a><b>=</b>TextVýraz]
              [<b>,</b><a href="#t773"> Width</a><b>=</b>ČísVýraz]   [<b>,</b> <a href="#t762">Grid</a><b>=</b>TextVýraz]   [<b>,</b> <a href="#t765">Print</a><b>=</b>TextVýraz]
              [<b>,</b> <a href="#t773">GrPoly</a><b>=</b>ČísVýraz]  [<b>,</b> <a href="#t762">Fill</a><b>=</b>TextVýraz]   [<b>,</b><a href="#t773"> Cond</a><b>=</b>[<b>(</b>)LogVýraz[<b>)</b>]]
              [<b>,</b><a href="#t773"> Recno</a><b>=</b>ČísVýraz]   [<b>,</b> <a href="#t773">Max</a><b>=</b>ČísVýraz]     [<b>,</b> <a href="#t773">Min</a><b>=</b>ČísVýraz]
              [<b>,</b><a href="#t773"> Nrecs</a><b>=</b>ČísVýraz]   [<b>,</b> <a href="#t773">Interact</a>]         [<b>,</b> <a href="#t779">Palette</a><b>=</b>TextVýraz]
             [{<b>,</b><a href="#t775"> Txt</a><b>=(</b>DefiniceTextu<b>)</b>}]   [{<b>,</b><a href="#t775">TxtWin</a><b>=(</b>DefiniceTextuOkna<b>)</b>}]
              [<b>,</b><a href="#t777"> ww</a><b>=(</b>DefiniceOkna<b>)</b> ]     [{<b>,</b><a href="#t779">RGB</a><b>=(</b>DefiniceBarvy<b>)</b>} ] 
              [<b>,</b><a href="#t765"> assign</a>=TextVýraz] <b>);</b>

   ■  <b>Názevklíče</b>  ... zobrazení dle alternativního klíče
   ■  <b>SeznamÚdajů</b> ... alespoň dva údaje souboru, první pro osu X, druhý a 
                      další pro osu Y. Typy údajů :  osaX  (F,A,N,D),
                                                     osaYi (F,N,D)
   ─── Ctrl-End ─────────────────────────────────────────────────────────────
</pre>

<a name="t758"></a>
## headz

*Též:* `dirx`, ` head`, `headx`, `heady`

<pre>
   Ctrl-Home                   <b>PARAMETRY GRAFU</b>
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>HEAD</b>=TextVýraz ... hlavička grafu,centrována,velkými písmeny, v základní 
                        barvě (implic.bílá), v horní části grafu. <b>head=''</b> 
                        potlačí zobrazení hlavičky. Implicitně se použije 
                        název souboru (není-li head uvedeno).

   ■ <b>HEADX</b>=TextVýraz... popis (název) osy X, implicitně název 1.údaje 
                        ze seznamu. Směr výpisu viz. parametr <b>DirX</b>.

   ■ <b>DIRX</b>=TextVýraz ... směr popisu a názvu osy X, string délky 1 znak
                        <b>H</b> - horizontální  (pro CIRCLE popis uvnitř grafu)
                        <b>V</b> - vertikální    (pro CIRCLE popis v legendě)
                        <b>I</b> - vertik. hlavička X, popis údajů uvnitř objektu
                            platí pro 2DBAR a CIRCLE, pro ostatní jako <b>V</b>)

   ■ <b>HEADY</b>=TextVýraz... Popis osy Y. Pro (2DBAR | CIRCLE | APPROX | POLYREG)
                        implicitně název 2.údaje ze seznamu.

   ■ <b>HEADZn</b>=TextVýraz.. Pro 3DBAR a 3DLINE popis osy Zn, nebo
                        popis legendy pro GROUP, GROUPLINE.
   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End          <a href="#t753">grafy</a>
</pre>

<a name="t760"></a>
## typy grafu

*Též:* `type`

<pre>
   Ctrl-Home                    <b>TYPY GRAFU</b>
   ──────────────────────────────────────────────────────────────────────────
   Typ grafu se zadá  jako textový  řetězec do parametru <b>type</b>. Dle typu grafu
   se řídí použití dalších parametrů.  Do počtu  údajů v seznamu je zahrnut i
   první údaj pro osu X.

   ■ <b>2DBAR</b>...... dvojrozměrný sloupcový graf (max. 3 údaje z věty)
                 základní tvar 2 údaje, třetí údaj jako druhý sloupec
   ■ <b>GROUP</b>...... dvojrozměrný sloupcový graf (max. 11 údajů)
                 pro každou větu se vytvoří skupina sloupců dle počtu údajů
   ■ <b>STACKBAR</b> .. poschoďový srovnávací graf, jako <b>group</b> ale hodnoty údajů
                 věty se zobrazí do jednoho vícevrstevného sloupce
   ■ <b>GROUPLINE</b>.. dvojrozměrný čárový spojnicový graf (max. 11 údajů)
   ■ <b>3DBAR</b>...... trojrozměrný sloupcový graf (max. 11 údajů)
   ■ <b>3DLINE</b>..... trojrozměrný čárový graf (max. 11 údajů)
   ■ <b>CIRCLE</b>..... kruhový nebo koláčový graf (max. 2 údaje)
                 může být dvoj nebo trojrozměrný-dle druhé pozice param. <a href="#t762">fill</a>
   ■ <b>APPROX</b>..... dvojrozměrný čárový aproximační graf (max. 3 údaje)
   ■ <b>POLYREG</b>.... dvojrozměrný čárový graf - polynomická regrese (max.3 údaje)
                 stupeň polynomu dle parametru <a href="#t773">grpoly</a>

   Pokud tento parametr chybí, nebo textový výraz má jinou hodnotu použije se
   hodnota '2DBAR'.
   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End          <a href="#t753">grafy</a>
</pre>

<a name="t762"></a>
## grid

*Též:* `fill`

<pre>
   Ctrl-Home                   <b>PARAMETRY GRAFU</b>
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>FILL</b>=TextVýraz ... dva znaky 'AB', první=typ výplně, druhý=tvar objektů
                        A = <b>N</b>-obrys        <b>C</b>-barvy      <b>F</b>-vzor
                            <b>M</b>-barvy&amp;vzor   <b>O</b>-1 barva
                        B = pro 2DBAR|GROUP|GROUPLINE|CIRCLE|APPROX|POLYREG
                                      <b>1</b>|<b>2</b> - dvojrozměrná verze
                                        <b>3</b> - trojrozměrná verze
                            pro 3DBAR   <b>1</b> - válcové sloupce
                                        <b>2</b> - jehlany
                                        <b>3</b> - kvádry
                            pro 3DLINE  <b>1</b> - lomená plocha
                                        <b>2</b> - plochy mezi úsečkami
                                        <b>3</b> - "kulisy"

   ■ <b>GRID</b>=TextVýraz ... zobrazení rastru, výpis hodnot do grafu
                        <b>Y</b>-yes (pro čárové grafy v ose X i Y, jinak jen X)
                        <b>N</b>-no(impl.)
                        <b>H</b>-hodnota do grafu (2DBAR,3DBAR,3DLINE).
                          Pro <b>H</b> lze dále zadat počet desetinných míst 
                              <b>H:n</b> (<b>n</b> = 0..10)
   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End            <a href="#t753">graph</a>        <a href="#t760">typy grafu</a>
</pre>

<a name="t765"></a>
## pau

*Též:* ` assign`, `print`

<pre>
   Ctrl-Home                   <b>PARAMETRY GRAFU</b>
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>PRINT</b>=TextVýraz... parametry pro tisk grafu : string délky 6 znaků
                        1. (impl.)<b>D</b>|<b>M</b>|<b>H</b>    Kvalita tisku - Draft,Midle,High
                        2. <b>H</b>(impl.) | <b>V</b>    Směr tisku: horizont. nebo vertik.
                        3. <b>0</b>|<b>1</b>|<b>2</b>(impl.)|<b>3</b>  Hustota tisku: 120,60,120,240 dpi
                        4. (impl.)<b>0</b>..<b>9</b>     Posunutí grafu od levé strany v cm
                        5. (impl.)<b>0</b>..<b>9</b>     Počet prázdných řádků před tiskem
                        6. (impl.)<b>Y</b> | <b>N</b>    Odstránkování po tisku grafu
                        (nebo Print='<b>PAU</b>nn': doba zobrazení grafu v sekundách)
                        <a href="#t782">tisk grafu</a>

   ■ <b>ASSIGN</b>=TextVýraz... určí jméno souboru (nebo cestu), do kterého se zapíše
                         graf při uložení do PCX (implicitně GRAPH.PCX,
                         implicitní přípona .PCX)

   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End            <a href="#t753">graph</a>        <a href="#t760">typy grafu</a>
</pre>

<a name="t773"></a>
## grpoly

*Též:* `interact`, ` width`, ` cond`, ` recno`, ` nrecs`, `max`, `min`

<pre>
   Ctrl-Home                   <b>PARAMETRY GRAFU</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Výběr vět ze souboru</b>
   ■ <b>COND</b>=LogVýraz ... výběr vět pro zobrazení
   ■ <b>RECNO</b>=ČísVýraz .. počáteční věta pro zobrazení (podmnožina se akceptuje)
                       Implicitně = 1.
   ■ <b>NRECS</b>=ČísVýraz .. počet vět pro zobrazení
                       maximum pro CIRCLE a POLYREG je 32,pro ostatní typy 64,
                       zobrazí se však počet reálně zobrazitelných vět.
   ...........


   ■ <b>WIDTH</b>=ČísVýraz .. šířka objektů v procentech (1..100%), implicitně 50%
                       Lze použít pro všechny typy kromě CIRCLE. Pro čárové
                       grafy šířka čar-pod 50% tence, jinak tlustá (3pixely)

   ■ <b>MAX</b>=ČísVýraz .... maximum na ose Y │ když <b>Min=-1</b> dosadí se do <b>Max</b> maximum
   ■ <b>MIN</b>=ČísVýraz .... minimum na ose Y │ hodnot+10% a do <b>Min</b> minimum-10%
                       Jiné záporné hodnoty než <b>min=-1</b> nejsou akceptovány.
                       (<a href="dalsi-kapitoly.md#t920">Funkce v L kapitole</a>)
   ■ <b>GRPOLY</b>=ČísVýraz . stupeň aproximačního polynomu pro POLYREG(max. Nrec-1)
   ■ <b>INTERACT</b> ........ interaktivní práce s převzetím zadaných parametrů
   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End        <a href="#t753">graph</a>      <a href="#t760">typy grafu</a>
</pre>

<a name="t775"></a>
## txtwin

*Též:* ` txt`

<pre>
   Ctrl-Home                   <b>PARAMETRY GRAFU</b>
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>TXT</b>=(XZ,YZ,VelikostPísma,BarvaPísma,Text) ......  zobrazení textu
                     XZ,YZ : ČísVýraz - souřad. levého dolního rohu (80x25)
                     VelikostPísma : ČísVýraz - [1..9]
                     BarvaPísma : TextVýraz, kód barvy (<a href="#t779">Palette</a>)
                     Text : text pro zobrazení, zobrazí se co se vejde

   ■ <b>TXTWIN</b>=(XZ,YZ,XK,YK,BarvaPozadí,BarvaPísma,{Text[;]}) .. textové okno
                     XZ,YZ : ČísVýraz - souřad. levého horního rohu (80x25)
                     XK,YK : ČísVýraz - souřadnice pravého dolního rohu
                     BarvaPozadí,BarvaPísma : TextVýrazy, kódy barvy
                     Text : text pro zobrazení, nový řádek dle <b>CR</b>,<b>LF</b> a <b>;</b>

   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End             <a href="#t753">graph</a>      <a href="#t760">typy grafu</a>
</pre>

<a name="t777"></a>
## okno pro graf

*Též:* ` ww`

<pre>
   Ctrl-Home                   <b>PARAMETRY GRAFU</b>
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>WW=(</b> [<b>(</b>] Souřadnice<b>,</b>Rámeček [<b>,</b>Poz<b>,</b>Pop<b>,</b>Ram] [<b>)</b>] <b>)</b>
   
     Obecný popis viz.<a href="procedury.md#t474">window</a>, velikost objektů (txt,txtwin) je redukována dle
     velikosti okna,  implicitně je ww=(1,1,80,25,'','l','W','W')  poněkud se
     odlišuje zadání barev.

     Poz,Pop,Ram ... barvy pozadí, popředí a rámečku, textové výrazy, 
                     používají se konstanty dle parametru <a href="#t779">Palette</a>.

     Není-li parametr <b>ww</b> zadán, použije se pro kreslení rámečku barva podle
     instalace (FANDINST) - "barvy" - "uživatelská obrazovka".

   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End             <a href="#t753">graph</a>      <a href="#t760">typy grafu</a>
</pre>

<a name="t779"></a>
## rgb

*Též:* `palette`

<pre>
   Ctrl-Home                <b>PARAMETRY GRAFU</b>
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>PALETTE</b>=TextVýraz ... pořadí barev pro zobrazení údajů, max 14 znaků
                           <b>B</b>   - modrá       <b>G</b> - zelená     <b>A</b>   - tmavě šedá
                           <b>Y</b>=<b>y</b> - žlutá       <b>R</b> - červená    <b>c</b>   - sv.purpurová
                           <b>r</b>   - sv.červená  <b>b</b> - sv.modrá   <b>W</b>=<b>w</b> - bílá
                           <b>g</b>   - sv.zelená   <b>M</b> - fialová    <b>L</b>=<b>l</b> - černá
                           <b>m</b>   - sv.fialová  <b>C</b> - purpurová
                           <b>O</b>=<b>o</b> - hnědá       <b>a</b> - sv.šedá
                           implicitně BYrgmORGbMCaAc

   ■ <b>RGB</b>=(KódBarvy,R,G,B) ... předefinování barev (jen VGA)
                              KódBarvy: dle <b>Palette</b> nebo '@' - ruční míchání
                              R,G,B : ČísVýrazy, poměr složek R,G,B v %

   ──────────────────────────────────────────────────────────────────────────
   Ctrl-End           <a href="#t753">graph</a>       <a href="#t760">typy grafu</a>
</pre>

<a name="t780"></a>
## GF

<pre>
                     <b>ZOBRAZENÍ SOUBORU VE FORMÁTU .PCX</b>
   ──────────────────────────────────────────────────────────────────────────
   Příkazem <a href="#t753">graph</a> lze zobrazit soubor v grafickém formátu <b>PCX</b> (pixel grafika)

   ██ Syntaxe:    <b>graph ( GF=</b> NázevSouboruPCX <b>,</b> ... <b>)</b>
  

   ■  Z ostatních parametrů lze uvést <a href="#t765">Print</a>,<a href="#t777"> ww</a>,<a href="#t775"> Txt</a>, <a href="#t775">TxtWin</a>, <a href="#t765"> Assign</a>.
  
   ■  Parametrem <b>WW</b> lze obraz .PCX "ořezat". Pokud je obraz větší než použité
      okno, lze oknem rolovat pomocí kurzorových kláves.

   ■  Předefinováním barev v .PCX se změní barvy na celé obrazovce po dobu 
      příkazu graph

   ■  <b>graph( GF='', ... )</b> ... tisk , uložení nebo inverze grafické obrazovky.
                              Dále jsou akceptovány parametry <a href="#t765">Print</a>,<a href="#t777"> ww</a>, <a href="#t775"> Txt</a>,
                              a <a href="#t775">TxtWin</a>.


   <b>Chyby při zobrazení .PCX</b>
   Nastane-li při zobrazení souboru PCX chyba, nahlásí PC FAND
      F10! Chyba obrazení PCX(číslo chyby)
   kde číslo chyby znamená:  1... soubor nelze otevřít (nenalezen)
                             2... nerozpoznán formát PCX souboru
                             3... možné zobrazit pouze na VGA kartě(256 barev)
                             4... pokus o zobrazení PCX v 256 barvách do okna
                                  (lze pouze přes celou obrazovku)
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t760">typy grafu</a>      <a href="#t781">ovládání zobrazení grafu</a>
</pre>

<a name="t781"></a>
## ovládání zobrazení grafu

<pre>
                          <b>OVLÁDÁNÍ ZOBRAZENÍ GRAFU</b>
   ──────────────────────────────────────────────────────────────────────────

   Při zobrazení grafu jsou tři možnosti chování:

   ■ <a href="#t782">tisk grafu</a> bez dotazu, po vytištění pokračuje úloha dalším příkazem
   ■ zobrazení s časovou prodlevou (<b>demonstrace</b>), prodloužení stiskem klávesy
   ■ normální zobrazení - čeká na stisk klávesy :

     <b>F6</b>,<b>ShiftF6</b> - tisk grafu, lze přerušit klávesou ESC
     <b>F9</b>,<b>ShiftF9</b> - zápis do souboru GRAPH.PCX (v aktuálním adresáři,formát <b>PCX</b>)
                  Při interaktivní práci lze jméno souboru změnit.
                  Konec ukládání je zvukově signalizován.
     <b>F4</b>,<b>Shift4</b>  - graf bude zobrazen inverzně
     <b>ESC</b>        - ukončení zobrazení grafu

   <b>Shift</b> - klávesy se vztahují k aktuálnímu oknu, def. parametrem WW

   Pokud je při zobrazení <b>PCX</b> obraz větší než stanovené okno (<a href="#t777"> WW</a>),lze v okně
   obrazem pohybovat:               <b>kurzor. šipky</b> - malý krok
                                       <b>PgUp</b>, <b>PgPn</b> - o celé okno vertikálně
                             <b>Ctrl-Right</b>,<b>Ctrl-Left</b> - o celé okno horizontálně
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t765">Print</a>    <a href="#t780">GF</a>    <a href="#t765">PAU</a>
</pre>

<a name="t782"></a>
## tisk grafu

<pre>
                                 <b>TISK GRAFU</b>
   ──────────────────────────────────────────────────────────────────────────

   Tisk grafu  lze provést  pomocí prostředků  systému MS-DOS ( PrintScreen +
   utilita graphics ). Výhodnější je použití podpory PC FANDu. Tj. klávesy <b>F6</b>
   pro celou obrazovku nebo  <b>ShifF6</b>  pro tisk aktuálního okna. 
   
   ■ Pomocí příkazu graph s parametrem <a href="#t780">GF</a> lze tisknout nejen vlastní graf ale
     obecně libovolnou  obrazovku nebo okno v grafickém módu.
     viz. příkaz <b>graph( GF='', ww=(...), Print=.., Txt=.., TxtWin=..)</b>
     Pro tisk má největší význam parametr <a href="#t765">print</a>.

   ■ Tisk grafu se provede jako kopie jednotlivých bodů obrazovky (pixelů) na
     tiskárnu v poměru 1:1.  Tj. <b>bod</b> na <b>bod</b>.  Samozřejmě zde hraje roli poměr
     rastru obrazovky na hustotu tisku na tiskárně (viz. <a href="prostredi.md#t222">instalace tiskárny</a>).

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t781">ovládání zobrazení grafu</a>
</pre>

<a name="t794"></a>
## tabulky znaků

<pre>
                           <b>TABULKA ZNAKŮ - 1.část</b>
   ──────────────────────────────────────────────────────────────────────────
         ┌────────────────┬──────────────┬──────────────┬──────────────┐
         │ dec hex  znak  │ dec hex znak │ dec hex znak │ dec hex znak │
         ├────────────────┼──────────────┼──────────────┼──────────────┤
         │  0  00  ^@ NUL │ 32  20  SPC  │ 64  40   @   │  96  60   `  │
         │  1  01  ^A SOH │ 33  21   !   │ 65  41   A   │  97  61   a  │
         │  2  02  ^B STX │ 34  22   "   │ 66  42   B   │  98  62   b  │
         │  3  03  ^C ETX │ 35  23   #   │ 67  43   C   │  99  63   c  │
         │  4  04  ^D EOT │ 36  24   $   │ 68  44   D   │ 100  64   d  │
         │  5  05  ^E ENQ │ 37  25   %   │ 69  45   E   │ 101  65   e  │
         │  6  06  ^F ACK │ 38  26   &amp;   │ 70  46   F   │ 102  66   f  │
         │  7  07  ^G BEL │ 39  27   '   │ 71  47   G   │ 103  67   g  │
         │  8  08  ^H BS  │ 40  28   (   │ 72  48   H   │ 104  68   h  │
         │  9  09  ^I HT  │ 41  29   )   │ 73  49   I   │ 105  69   i  │
         │ 10  0A  ^J LF  │ 42  2A   *   │ 74  4A   J   │ 106  6A   j  │
         │ 11  0B  ^K VT  │ 43  2B   +   │ 75  4B   K   │ 107  6B   k  │
         │ 12  0C  ^L FF  │ 44  2C   ,   │ 76  4C   L   │ 108  6C   l  │
         │ 13  0D  ^M CR  │ 45  2D   -   │ 77  4D   M   │ 109  6D   m  │
         │ 14  0E  ^N SO  │ 46  2E   .   │ 78  4E   N   │ 110  6E   n  │
         │ 15  0F  ^O SI  │ 47  2F   /   │ 79  4F   O   │ 111  6F   o  │
         │ 16  10  ^P DLE │ 48  30   0   │ 80  50   P   │ 112  70   p  │
         │ 17  11  ^Q DC1 │ 49  31   1   │ 81  51   Q   │ 113  71   g  │
         │ 18  12  ^R DC2 │ 50  32   2   │ 82  52   R   │ 114  72   r  │
         │ 19  13  ^S DC3 │ 51  33   3   │ 83  53   S   │ 115  73   s  │
         │ 20  14  ^T DC4 │ 52  34   4   │ 84  54   T   │ 116  74   t  │
         │ 21  15  ^U NAK │ 53  35   5   │ 85  55   U   │ 117  75   u  │
         │ 22  16  ^V SYN │ 54  36   6   │ 86  56   V   │ 118  76   v  │
         │ 23  17  ^W ETB │ 55  37   7   │ 87  57   W   │ 119  77   w  │
         │ 24  18  ^X CAN │ 56  38   8   │ 88  58   X   │ 120  78   x  │
         │ 25  19  ^Y EM  │ 57  39   9   │ 89  59   Y   │ 121  79   y  │
         │ 26  1A  ^Z SUB │ 58  3A   :   │ 90  5A   Z   │ 122  7A   z  │
         │ 27  1B  ^[ ESC │ 59  3B   ;   │ 91  5B   [   │ 123  7B   {  │
         │ 28  1C  ^\ FS  │ 60  3C   &lt;   │ 92  5C   \   │ 124  7C   |  │
         │ 29  1D  ^] GS  │ 61  3D   =   │ 93  5D   ]   │ 125  7D   }  │
         │ 30  1E  ^^ RS  │ 62  3E   &gt;   │ 94  5E   ^   │ 126  7E   ~  │
         │ 31  1F  ^_ US  │ 63  3F   ?   │ 95  5F   _   │ 127  7F  DEL │
         └────────────────┴──────────────┴──────────────┴──────────────┘
   ─── CtrlEnd ──────────────────────────────────────────────────────────────
                         <a href="#t795">Tabulka znaků - 2.část</a>         <a href="#t796">speciální znaky</a>
</pre>

<a name="t795"></a>
## tabulka znaků - 2.část

<pre>
                           <b>TABULKA ZNAKŮ - 2.část</b>
   ─── CtrlHome ─────────────────────────────────────────────────────────────
         ┌──────────────┬──────────────┬──────────────┬──────────────┐
         │ dec hex znak │ dec hex znak │ dec hex znak │ dec hex znak │
         ├──────────────┼──────────────┼──────────────┼──────────────┤
         │ 128  80   Č  │ 160  A0   á  │ 192  C0   └  │ 224  E0   α  │
         │ 129  81   ü  │ 161  A1   í  │ 193  C1   ┴  │ 225  E1   ß  │
         │ 130  82   é  │ 162  A2   ó  │ 194  C2   ┬  │ 226  E2   Γ  │
         │ 131  83   ď  │ 163  A3   ú  │ 195  C3   ├  │ 227  E3   π  │
         │ 132  84   ä  │ 164  A4   ň  │ 196  C4   ─  │ 228  E4   Σ  │
         │ 133  85   Ď  │ 165  A5   Ň  │ 197  C5   ┼  │ 229  E5   σ  │
         │ 134  86   Ť  │ 166  A6   Ů  │ 198  C6   ╞  │ 230  E6   µ  │
         │ 135  87   č  │ 167  A7   Ô  │ 199  C7   ╟  │ 231  E7   τ  │
         │ 136  88   ě  │ 168  A8   š  │ 200  C8   ╚  │ 232  E8   Φ  │
         │ 137  89   Ě  │ 169  A9   ř  │ 201  C9   ╔  │ 233  E9   Θ  │
         │ 138  8A   Ĺ  │ 170  AA   ŕ  │ 202  CA   ╩  │ 234  EA   Ω  │
         │ 139  8B   Í  │ 171  AB   Ŕ  │ 203  CB   ╦  │ 235  EB   δ  │
         │ 140  8C   ľ  │ 172  AC   ¼  │ 204  CC   ╠  │ 236  EC   ∞  │
         │ 141  8D   ĺ  │ 173  AD   §  │ 205  CD   ═  │ 237  ED   φ  │
         │ 142  8E   Ä  │ 174  AE   «  │ 206  CE   ╬  │ 238  EE   ε  │
         │ 143  8F   Á  │ 175  AF   »  │ 207  CF   ╧  │ 239  EF   ∩  │
         │ 144  90   É  │ 176  B0   ░  │ 208  D0   ╨  │ 240  F0   ≡  │
         │ 145  91   ž  │ 177  B1   ▒  │ 209  D1   ╤  │ 241  F1   ±  │
         │ 146  92   Ž  │ 178  B2   ▓  │ 210  D2   ╥  │ 242  F2   ≥  │
         │ 147  93   ô  │ 179  B3   │  │ 211  D3   ╙  │ 243  F3   ≤  │
         │ 148  94   ö  │ 180  B4   ┤  │ 212  D4   ╘  │ 244  F4   ⌠  │
         │ 149  95   Ó  │ 181  B5   ╡  │ 213  D5   ╒  │ 245  F5   ⌡  │
         │ 150  96   ů  │ 182  B6   ╢  │ 214  D6   ╓  │ 246  F6   ÷  │
         │ 151  97   Ú  │ 183  B7   ╖  │ 215  D7   ╫  │ 247  F7   ≈  │
         │ 152  98   ý  │ 184  B8   ╕  │ 216  D8   ╪  │ 248  F8   °  │
         │ 153  99   Ö  │ 185  B9   ╣  │ 217  D9   ┘  │ 249  F9   ∙  │
         │ 154  9A   Ü  │ 186  BA   ║  │ 218  DA   ┌  │ 250  FA   ·  │
         │ 155  9B   Š  │ 187  BB   ╗  │ 219  DB   █  │ 251  FB   √  │
         │ 156  9C   Ľ  │ 188  BC   ╝  │ 220  DC   ▄  │ 252  FC   ⁿ  │
         │ 157  9D   Ý  │ 189  BD   ╜  │ 221  DD   ▌  │ 253  FD   ²  │
         │ 158  9E   Ř  │ 190  BE   ╛  │ 222  DE   ▐  │ 254  FE   ■  │
         │ 159  9F   ť  │ 191  BF   ┐  │ 223  DF   ▀  │ 255  FF      │
         └──────────────┴──────────────┴──────────────┴──────────────┘

   ─── CtrlEnd ──────────────────────────────────────────────────────────────
   <a href="#t794">tabulky znaků</a>       <a href="#t796">speciální znaky</a>       <a href="#t488">kódy řídících a funkčních kláves</a>
</pre>

<a name="t796"></a>
## speciální znaky

<pre>
                              <b>SPECIÁLNÍ ZNAKY</b>
 ──────────────────────────────────────────────────────────────────────────────
       speciální znaky pro kreslení čar a rámečků nebo vyplňování obrazovky
                   ┌─────────────────────────┬────────────────┐
                   │   znak    kód           │   znak  kód    │
                   ├─────────────────────────┼────────────────┤
                   │   ┌ ┬ ┐   218 194 191   │     ─   196    │
                   │   ├ ┼ ┤   195 197 180   │     │   179    │
                   │   └ ┴ ┘   192 193 217   │     ═   205    │
                   ├─────────────────────────┤     ║   186    │
                   │   ╔ ╦ ╗   201 203 187   ├────────────────┤
                   │   ╠ ╬ ╣   204 206 185   │     ░   176    │
                   │   ╚ ╩ ╝   200 202 188   │     ▒   177    │
                   ├─────────────────────────┤     ▓   178    │
                   │   ╓ ╥ ╖   214 210 183   │     █   219    │
                   │   ╟ ╫ ╢   199 215 182   ├────────────────┤
                   │   ╙ ╨ ╜   211 208 189   │     ▄   220    │
                   ├─────────────────────────┤     ▌   221    │
                   │   ╒ ╤ ╕   213 209 184   │     ▐   222    │
                   │   ╞ ╪ ╡   198 216 181   │     ▀   223    │
                   │   ╘ ╧ ╛   212 207 190   │                │
                   └─────────────────────────┴────────────────┘
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t794">tabulky znaků</a>      <a href="#t795">tabulka znaků - 2.část</a>     <a href="#t488">kódy řídících a funkčních kláves</a>
</pre>
