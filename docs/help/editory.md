# Textový a datový editor

<a name="t228"></a>
## textový editor

<pre>
                               <b>TEXTOVÝ EDITOR</b>
   ───────────────────────────────────────────────────────────────────────────
   <b>Textový editor</b> se používá zejména pro prohlížení <b>tiskových sestav</b> a editaci
   datových položek typu T (viz.<a href="#t327">volný text</a>). Např. programátorské prostředí PC
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
   a zpět ukládá na disk. Pomocí techniky přerušení editace ( <a href="#t532">exit-procedury</a> )
   lze doplnit speciální funkce textového editoru. (<a href="#t595">příkazy pro editaci textu</a>)
   ───────────────────────────────────────────────────────────────────────────
   <a href="#t229">pohyby kurzoru</a>        <a href="#t230">editace textu</a>      <a href="#t234">bloky</a>           <a href="#t236">přepínače</a>
   <a href="#t237">formátování textu</a>     <a href="#t238">vyhledávání</a>        <a href="#t240">tisk textu</a>      <a href="#t242">barvy a typy písma</a>
   <a href="#t232">doplňky editoru</a>       <a href="rozhrani.md#t794">tabulky znaků</a>      <a href="rozhrani.md#t203">myš v datovém a textovém editoru</a>
</pre>

<a name="t229"></a>
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
   <a href="#t230">editace textu</a>         <a href="rozhrani.md#t203">myš v datovém a textovém editoru</a>
</pre>

<a name="t230"></a>
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
   <a href="#t229">pohyby kurzoru</a>                <a href="#t595">příkazy pro editaci textu</a>
</pre>

<a name="t232"></a>
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
   ■ <b>Ctrl</b>-<b>F5</b> ......... vyvolání kalkulačky (<b>Ctrl-F4</b> - převzetí do textu)
   ──────────────────────────────────────────────────────────────────────────
   <a href="rozhrani.md#t58">klávesnice</a>
</pre>

<a name="t234"></a>
## bloky

*Též:* `clipboard`

<pre>
                          <b>BLOKY TEXTOVÉHO EDITORU</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Blok</b> je část textu. Blok je třeba  nejprve  označit (je barevně odlišen od
   ostatního textu)  a  potom  s  ním  lze  pracovat pomocí blokových operací.
   <b>Clipboard</b> je pomocný text (zásobník) pro přenášení bloků.

   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>N</b> ...... přepínač režimu běžný &lt;-&gt;<a href="#t235">sloupcový blok</a>

   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>B</b> nebo <b>F7</b> ...... označení začátku bloku
   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>K</b> nebo <b>F8</b> ...... označení konce bloku
   ■ <b>Shift</b>-<b>Down</b>,<b>Up</b>,<b>Left</b>,<b>Right</b> ... definice bloku tažením meze bloku
   ■ <b>Shift</b>-<b>PgDn</b>,<b>PgUp</b> ............ posun meze bloku o stránku vpřed (zpět)
   ■ <b>Shift</b>-<b>Home</b>,<b>End</b> ............. posun meze bloku na začátek (konec) řádku

   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>C</b> ........... kopie bloku (max.28KB) na místo kurzoru   <b>c</b>opy
   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>V</b> ....... přenesení bloku (max.28KB) na místo kurzoru   mo<b>v</b>e
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
   <a href="#t237">formátování textu</a>     <a href="#t240">tisk textu</a>
</pre>

<a name="t235"></a>
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
   <a href="#t234">bloky</a>
</pre>

<a name="t236"></a>
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
   <a href="#t237">formátování textu</a>
</pre>

<a name="t237"></a>
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
   <a href="#t236">přepínače</a>     <a href="#t234">bloky</a>
</pre>

<a name="t238"></a>
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
   <a href="jazyk.md#t668">lexikální třídění</a>
</pre>

<a name="t240"></a>
## tisk textu

*Též:* `tečkové příkazy`

<pre>
                                 <b>TISK TEXTU</b>
   ──────────────────────────────────────────────────────────────────────────
   ■ <b>F6</b> .............. tisk celého textu
   ■ <b>Ctrl</b>-<b>F6</b> ......... tisk od pozice kurzoru do konce textu
   ■ <b>Ctrl</b>-<b>K Ctrl</b>-<b>P</b> ... tisk bloku (nebo celého textu není-li blok definován)
   ■ <b>Alt</b>-<b>F6</b> .......... interaktivní změna tiskárny

   Tisk lze přerušit klávesou <b>Esc</b>. Při tisku se interpretují (netisknou se, 
   ale provedou požadované akce) následující <b>tečkové příkazy</b>, jsou-li
   umístěné v prvních řádcích textu (po jednom na řádku vždy od prvního sloupce):

   ■ <b>.ti n</b> ... počet výtisků (implicitně <b>1</b>)
   ■ <b>.cp n</b> ... automatické stránkování (počet řádek vynechaných na konci strany)

   ■ <b>.pl n</b> ... délka fyzické strany na tiskárně (implicitně <b>72</b> řádek)
   ■ <b>.po n</b> ... odsazení textu od levého okraje (implicitně <b>0</b>)
               správná funkce <b>.po</b> a <b>.pl</b> závisí na instalaci odpovídající řídící
               sekvence pro tiskárnu - viz. <a href="prostredi.md#t222">Instalace tiskárny</a>.

   ■ <b>.ff</b> ..... potlačí hlášku "nastav tiskárnu" na začátku a odstránkování
               na konci
   ■ <b>.nm</b> ..... jako <b>.ff</b> ale na konci odstránkuje

   ■ <b>.he text</b> ... hlavička strany  ┐     <b>masky</b> uvnitř textu 
   ■ <b>.fo text</b> ... ukončení strany  ├     <b>___</b>        číslo strany
                                   │     <b>__.__.__</b>   datum
                                   │     <b>__.__.____</b> plné datum (od verze 4.2)
                                   ┘     <b>__:__</b>      čas
   <b>n</b> je libovolné číslo, <b>text</b> je jednořádkový řetězec
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t242">barvy a typy písma</a>      <a href="#t234">bloky</a>
</pre>

<a name="t242"></a>
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
   <a href="#t240">tisk textu</a>     <a href="prostredi.md#t221">instalace barev</a>     <a href="prostredi.md#t222">instalace tiskárny</a>
</pre>

<a name="t244"></a>
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
 <a href="#t228">textový editor</a> ( údaje typu T ) a jeden pro  ostatní typy údajů.  Přepínač pro
 textový editor je globální (nastavení platí i pro editaci dalších textů). 
 Přepínač pro ostatní typy dat je naopak lokální, což znamená,že jeho nastavení
 je platné jen v rámci právě rozpracované editace údaje.  Po vstupu do údaje je
 vždy vkládací režim.

 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t245">funkční a řídící klávesy</a>     <a href="#t246">přepínače datového editoru</a>     <a href="#t247">akce</a>
 <a href="prostredi.md#t248">automatická sestava</a>          <a href="#t251">navigace po databázi</a>           <a href="#t228">textový editor</a>
</pre>

<a name="t245"></a>
## funkční a řídící klávesy

<pre>
                          <b>FUNKČNÍ A ŘÍDÍCÍ KLÁVESY</b>
 ──────────────────────────────────────────────────────────────────────────────
 ■ <b>F2</b> (<b>pořiď</b>/<b>edit</b>) ... přepnutí do módu pořízení/editace
 ■ <b>F3</b> (<b>hledej</b>) ....... vyhledání věty podle vlastního klíče
 ■ <b>F4</b> (<b>duplikace</b>) .... údaj se zkopíruje podle předchozí věty
 ■ <b>F5</b> (<b>přepínače</b>) .... nastavení a zrušení přepínačů
 ■ <b>F6</b> (<b>akce</b>) ......... spuštění operací nad souborem
 ■ <b>F8</b>,<b>ShiftF8</b> (<b>výběr</b>). manuální výběr věty do podmnožiny <a href="#t529">sel</a>
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
 <a href="#t246">přepínače datového editoru</a>     <a href="#t247">akce</a>     <a href="#t251">navigace po databázi</a>
</pre>

<a name="t246"></a>
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
 <a href="#t228">textový editor</a> ( údaje typu T ) a jeden pro  ostatní typy údajů.  Přepínač pro
 textový editor je globální (nastavení platí i pro editaci dalších textů). 
 Přepínač pro ostatní typy dat je naopak lokální, což znamená,že jeho nastavení
 je platné jen v rámci právě rozpracované editace údaje.  Po vstupu do údaje je
 vždy vkládací režim.
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t247">akce</a>       <a href="soubory.md#t348">aditivní změny</a>       <a href="jazyk.md#t346">referenční integrita</a>       <a href="soubory.md#t354">kontroly</a>
</pre>

<a name="t247"></a>
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
   <a href="prostredi.md#t248">automatická sestava</a>     <a href="rozhrani.md#t205">výběr ze seznamu</a>     <a href="#t246">přepínače datového editoru</a>
   <a href="soubory.md#t354">logické kontroly</a>
</pre>

<a name="t251"></a>
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
   bez výběru souboru nebo uživatelského pohledu, viz. <a href="prostredi.md#t530">módy datového editoru</a>(S7)
   <b>Enter</b> znamená návrat s přenosem klíčového údaje, <b>Esc</b> návrat bez přenosu
   Pokud v odpovídajícím pohledu uvedeme parametr <b>tab=()</b>, lze nadřízený soubor
   také editovat (i pořizovat věty).
 ──────────────────────────────────────────────────────────────────────────────
 <a href="soubory.md#t350">uživatelské pohledy</a>     <a href="soubory.md#t336">klíče</a>
</pre>

<a name="t327"></a>
## uložené údaje

*Též:* `typy údajů`, `volný text`

<pre>
                               <b>ULOŽENÉ ÚDAJE</b>
   ──────────────────────────────────────────────────────────────────────────
   Seznam údajů, které budou fyzicky uloženy v souboru, ve tvaru:

   ██  NázevÚdaje : Typ ;

                <b>typy údajů</b> v PC FANDu:

   █ <b>F,</b>m<b>.</b>n ... <b>číslo</b>,  <b>m</b>, <b>n</b> udává počet míst před a za čárkou,
               rozsah čísla je až 18 míst, z toho je <b>11 platných míst</b>
               ve verzi pro matematický koprocesor <b>15</b> platných míst
   █ <b>R</b>     ... <b>reálné číslo v pohyblivé řádové čárce</b>, interně uložen
               jako (Pascal) typ real v 6 bytech.
   █ <b>A</b>,n   ... <b>znakový řetězec</b>, <b>n</b> udává délku (až <b>255 znaků</b>)
   █ <b>N</b>,n   ... <b>číselný řetězec</b>, <b>n</b> udává délku (až <b>79 znaků</b>)
   █ <b>D</b>     ... <b>datum</b> s implicitní maskou 'DD.MM.YY'
   █ <b>B</b>     ... <b>logický typ</b> (pravda=true='<b>A</b>', nepravda=false='<b>N</b>')
   █ <b>T</b>     ... <b>volný text</b> (formátovaný text maxim. délky <b>65 000 B</b>)

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>  Množství: F,6.0;          Cena: F,4.2;        Aktiva: F,9.2;
   <b>░░░░░░░░░░░░</b>     Název: A,12;          Titul: A,30;       Poznámka: A,250;
                      Kód: N,3;             Psč: N,5;             Ičo: N,8;
                    Datum: D;        Proplaceno: B;          Poznámky: T;
   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t330">výjimky v typech údajů</a>             <a href="soubory.md#t785">»F</a>
</pre>

<a name="t529"></a>
## procedurální parametry editace

*Též:* `irec`, `reckey`, `field`, `sel`

<pre>
                       <b>PROCEDURÁLNÍ PARAMETRY EDITACE</b>
   ──────────────────────────────────────────────────────────────────────────
   Další parametry procedury <b>edit</b> použitelné výhradně v proceduře.
 
   ██ <b>recno</b>=ČísVýraz .... počáteční nastavení editoru - číslo věty (<a href="jazyk.md#t550">edrecno</a>)
   ██ <b>irec</b>=ČísVýraz ..... pořadové číslo věty s kurzorem na obrazovce (<a href="jazyk.md#t550">edirec</a>)
   ██ <b>field</b>=TextVýraz ... počáteční poloha kurzoru - název údaje (<a href="jazyk.md#t550">edfield</a>)
   ██ <b>reckey</b>=TextVýraz .. počáteční poloha kurzoru na větě s touto (interní)
                          hodnotou klíče (<a href="jazyk.md#t550">edreckey</a>)

   Pomocí těchto  parametrů lze přesně  definovat, na kterou  větu souboru má
   editor při startu  najet včetně umístění  věty a kurzoru v aktuálním okně.


   ██ <a href="soubory.md#t437">owner</a>= ... editace skupiny vět podřízeného souboru podle viditelné věty

   ██ <a href="#t529">sel</a>=PracovníIndex...pracovní index je lokální proměnná nebo parametr
                          procedury, který slouží pro realizaci výběru podmno-
                          žiny pomocí kláves:
                          <b>F8</b>      vybrat/nevybrat aktuální větu (přepínač)
                          <b>ShiftF8</b> všechny věty do výběru nebo naopak
                          Věty, vybrané do indexu jsou barevně odlišeny viz.
                          <a href="prostredi.md#t221">instalace barev</a> nebo parametr <a href="procedury.md#t524">editace v okně</a>.
                          Třídění pracovního indexu musí odpovídat třídění 
                          editačního indexu (definuje se před <b>edit</b>em).
                          Souvislosti: <b>mode</b>='sl'  viz. <a href="prostredi.md#t530">módy datového editoru</a>

   ██ <a href="soubory.md#t554">check</a> ... příkaz edit provede jen kontrolu syntaxe dynamické deklarace 
                kapitoly E.

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t519">parametry editace</a>      <a href="soubory.md#t336">klíče</a>      <a href="prostredi.md#t829">lokální sítě</a>      <a href="procedury.md#t551">░ volání editoru</a>    <a href="jazyk.md#t789">»P</a>
</pre>

<a name="t532"></a>
## exit-procedury

*Též:* `edok`

<pre>
                              <b>EXIT - PROCEDURY</b>
  ────────────────────────────────────────────────────────────────────────────
  Termínem <b>exit-procedura</b> označujeme proceduru, která byla vyvolána z datového
  editoru  v důsledku akce definované  parametrem <a href="#t536">  exit</a>. Tyto  procedury jsou
  syntakticky shodné s běžnou procedurou.  Určitá specifika a <b>omezení</b>  přináší
  fakt, že jsou volány z <b>přerušeného</b> (!!! nikoliv ukončeného) datového editoru.

  Aktuální stav (rozpracované) věty je do exit-procedury předán prostřednictvím
  parametru procedury typu  <a href="procedury.md#t405">record</a> of soubor. Pokud potřebujeme s rozeditovanou
  větou  pracovat , deklarujeme  odpovídající  record  proměnnou jako  poslední
  parametr procedury. Z record proměnné lze údaje číst ale i do nich zapisovat.
  Případně změněné hodnoty jsou po  ukončení exit-procedury automaticky předány
  zpět do editoru.

  <b>Omezení</b> v exit-procedurách se týkají především zpětných zásahů do editovaného
  souboru. Vyplývají z toho,  že přerušený  editor má rozpracované určité vazby
  na aktuální stav souboru a při necitlivém zásahu může dojít k narušení těchto
  vazeb a potažmo k chybovým stavům. Tato omezení nelze syntakticky vyhodnotit,
  neboť jedna procedura může být volána jako  <a href="procedury.md#t288">exit</a>, ale na jiném místě i běžným
  způsobem (příkaz <a href="procedury.md#t436">proc</a>), kde je kód procedury naprosto korektní.
   ┌──────────────────────────────────────────────────────────────────────────┐
   │Výrazně je nutno upozornit na nekorektnost jakékoliv změny editované věty │
   │pomocí přímého přístupu  (soubor[<a href="jazyk.md#t550">edrecno</a>].údaj, writerec(...,edrecno).    │
   └──────────────────────────────────────────────────────────────────────────┘

  V exit-proceduře vyvolané <b>funkční klávesou</b> ( př.<b>F6</b> ) nebo <b>kurzorovou klávesou</b>
  lze po návratu z procedury vyvolat dále i standardní činnost datového editoru
  pro danou klávesu. Zadáme <b>EdOk:=true</b>. Implicitně je <b>EdOk</b>=false tj. standardní
  procedura se neprovede.

  Pomocí deklarace parametru procedury typu <a href="soubory.md#t417">file</a> lze psát zobecněné exit-proce-
  dury s korektním řešením deklarace předávané věty souboru. Tj.jedna procedura 
  pro více různých souborů.

  ────────────────────────────────────────────────────────────────────────────
  <a href="procedury.md#t411">Procedura</a>
</pre>

<a name="t536"></a>
## přerušení editace

*Též:* `quit`, `newrec`, `  exit`

<pre>
                             <b>PŘERUŠENÍ EDITACE</b>
   ───────────────────────────────────────────────────────────────────────────
   Běžná editace datového souboru může být přerušena požadovanou akcí (vnořená
   procedura, tisková sestava z aktuální věty, nestandardní ukončení editace).
   Spuštění akce je buď  automatické ( vázané na přechod  přes editovaný údaj,
   změnu věty, mazání nebo přidání nové věty ) nebo ruční ( vyvolané funkčními
   nebo kurzorovými  klávesami - viz. <a href="jazyk.md#t550">edbreak</a> ). Po skončení akce se pokračuje
   v editaci souboru.  Pro přerušení editace  z funkčních kláves  lze vytvořit
   alternativní nápovědu pomocí parametrů editace <a href="rozhrani.md#t287">ctrl</a>,<a href="rozhrani.md#t523">alt</a> nebo <a href="rozhrani.md#t523">shift</a>.

   ██ syntaxe:            <b>EXIT=(</b> SeznamPřerušení <b>)</b>

   ■  <b>SeznamPřerušení</b> ... ZpůsobPřerušení <b>:</b> Akce [{ <b>,</b>ZpůsobPřerušení <b>:</b> Akce }]

   ■  <b>ZpůsobPřerušení</b> ... <a href="formular-sestava.md#t538">Seznam údajů</a> | SeznamKláves | <b>record</b> | <b>newrec</b>
                          Seznam událostí se může skládat i z událostí různého
                          typu.  Např. (udaj1,udaj2),F8,record : akce) ...

   ■  <b>Akce</b> .............. Exit-procedura | Report | <b>quit</b> |

      Nutná poznámka:     Čtvrtá možnost je vynechání specifikace akce, což
                          lze použít pro "odstavení" standardních kláves
                          editoru (funkčně rovnocenná je prázdná procedura)

   Každý prvek seznamu přerušení definuje způsob přerušení (typ události) a
   požadovanou akci. Lze definovat více přerušení najednou.

   ■  <b>SeznamKláves</b> ... povolené klávesy viz. <a href="jazyk.md#t550">edbreak</a>, jednotlivé klávesy jsou 
                       oddělené čárkou. Akci vyvolá stisk klávesy.
   ■  <a href="formular-sestava.md#t538">Seznam údajů</a> ... přerušení vyvolá stisknutí <b>Enter</b> nebo <b>F4</b> na údaji nebo 
                       přeskok údaje po tabelátorech           (<a href="jazyk.md#t550">edbreak</a>=<b>12</b>)
   ■  <b>record</b> ......... Toto přerušení může vyvolat pouze exit-proceduru.
                       Procedura se vyvolá před změnou, zapsáním nebo zrušením
                       věty. edbreak=<b>16</b>: nová nebo změněná věta
                             edbreak=<b>17</b>: zrušená věta (po <b>Ctrl-Y</b>)
                       Dokončení započaté akce lze zabránit přiřazením
                       <b>EdOk</b>:=false před návratem z exit-procedury.
   ■  <b>newrec</b> ......... Procedura se vyvolá před začátkem pořízení nové věty,
                       tj. po vytvoření a zobrazení prázdné věty ale před
                       editací prvního údaje. (<a href="jazyk.md#t550">edbreak</a>=<b>18</b>).

   <b>AKCE</b>
   ■  <a href="#t532">Exit-procedury</a>.. NázevProcedury[<b>(</b>SeznamParametrů<b>)</b>] nebo
                       <b>[</b>TextVýraz<b>]</b>[<b>(</b>SeznamParametrů<b>)</b>] ... <a href="soubory.md#t554">Dynamické deklarace</a>
   ■  <b>report</b> ......... <b>report(</b>NázevReportu<b>)</b> 
                       Tisková sestava z aktuální věty. 
                       Mohou následovat parametry reportu<a href="prostredi.md#t560">  assign</a> nebo <a href="prostredi.md#t282">edit</a>
   ■  <b>quit</b> ........... nestandardní ukončení editace místo běžného Esc
                       (quit na údaji není funkční při pořízení)
                                                                            *
   Uvnitř exit - procedur lze použít funkce <a href="jazyk.md#t550">edupdated</a> ( byla věta změněná ?) a
   návratový kód editoru <a href="jazyk.md#t550">edbreak</a> (jak byla přerušena editace ?). Aktuální stav
   právě editované věty je předán  vnořené  proceduře  jako dodatečný poslední
   parametr volané procedury (typu <b>record of</b> editovaný soubor). 
   Vnořená procedura může (ale nemusí) parametr deklarovat (použít). Případné
   změny record proměnné se po návratu promítnou do souboru.

   Další použitelné "stavové" funkce: <a href="jazyk.md#t550">edirec</a>, <a href="jazyk.md#t550">edrecno</a>, <a href="jazyk.md#t550">edfield</a>, <a href="jazyk.md#t550">edreckey</a>

   ──────────────────────────────────────────────────────────────────────────
   <a href="procedury.md#t411">procedura</a>          <a href="#t537">░ přerušení editace</a>         <a href="jazyk.md#t789">»P</a>
</pre>

<a name="t537"></a>
## ░ přerušení editace

<pre>
                        <b>PŘÍKLAD - PŘERUŠENÍ EDITACE</b>
 ──────────────────────────────────────────────────────────────────────────────
 ░ edit(...exit=(ShiftF3,ShiftF5:quit))       {ukončení editace Shift Klávesou}
 ░ edit(...exit=(ShiftF1:Help(edfield)))          {vnořená proc. s parametrem}
 ░ edit(...exit=(ShiftF3:quit,ShiftF1:Help))              {kombinace přerušení}

 ░ edit(...exit=((Množství,Cena),ShiftF10:Součty))         {přerušení na údaji}
 ░ P Součty * ... ( r : record of DATA)
 ░                begin r.Celkem:=r.Cena*r.Množství end;  {změna aktuální věty}

 ░ case edbreak=16 &amp; edrecno&gt;0:     {vnořená procedura volaná record:NázevProc}
 ░        if ^prompt(' Změnit větu ?':B:=false) then edok:=false;
 ░      edbreak=16 &amp; edrecno=0:                  {^n nebo F2 - zápis nové věty}
 ░        if ^prompt(' Zapsat větu ?':B:=false) then edok:=false;
 ░      edbreak=17:                                            {^y mazání věty}
 ░        if ^prompt(' Zrušit větu ?':B:=false) then edok:=false;
 ░ end;

 ░ edit(DATA,(),exit=(record:RecordProc));                      {ošetření změn}
 ░ edit(DATA,(),exit=(ShiftF6:report(Data,edit)));       {sestava z jedné věty}
 ░ edit(DATA,(),exit=(():quit));                       {např.výběr z číselníku}
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t591"></a>
## funkce textového editoru

*Též:* ` txtxy`, ` txtpos`

<pre>
                          <b>FUNKCE TEXTOVÉHO EDITORU</b>
   ──────────────────────────────────────────────────────────────────────────

   ██ syntaxe:         <b>EDUPDATED</b> : boolean
                       <b>CLIPBD</b>    : string
                       <b>TXTPOS</b>    : real
                       <b>TXTXY</b>     : real

   ■ <b>EdUpdated</b> ... Test provedení aktualizace souboru během editace. Udává, 
                   zda byl soubor změněn (true) či nikoliv (false). 
                   Je funkční pro datový i textový editor.
   ■ <b>Clipbd</b> ...... Definována v exit-proceduře, vrací obsah <a href="#t234">clipboard</a>u
   ■ <b>TxtPos</b> ...... Pozice kurzoru v textu při opuštění textového editoru. Pro
                   opětovný vstup do textu lze použít stejnojmenný  parametr 
                   příkazu  <a href="#t595">edittxt</a>. Další význam txtpos viz. funkce <a href="jazyk.md#t740">evalb</a>,
                   <a href="jazyk.md#t740">evalr</a>,<a href="jazyk.md#t740">evals</a>, <a href="procedury.md#t288">exit</a>, <a href="prostredi.md#t283">mode</a>='EX'.
   ■ <b>TxtXY</b> ....... Pozice kurzoru v okně při ukončení textového editoru.
                   Výpočet pozice kurzoru : 
                   <b>txtXY</b> = 65536*pozice v řádku + 256*řádek + sloupec
                   Řádek a sloupec jsou relativní souřad. kurzoru v aktuálním 
                   okně. Pozice v řádku je pozice kurzoru od počátku řádku.
   <b>Poznámka:</b>
   Pomocí funkcí a parametrů <b>TxtPos</b> a <b>TxtXY</b> se lze po opuštění textového
   editoru vrátit na stejné místo textu a obrazovky (okna). Podobně při exit
   proceduře. Tento mechanismus však korektně funguje jen za předpokladu, že
   se kurzor nachází v rámci textu (nikoliv za posledním znakem řádku).
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t595"></a>
## příkazy pro editaci textu

*Též:* `noedit`, `errmsg`, `edittxt`

<pre>
                         <b>PŘÍKAZY PRO EDITACI TEXTU</b>
   ──────────────────────────────────────────────────────────────────────────
   Implicitně je v PC FANDu volán  textový editor v několika standardních si-
   tuacích ( tisk sestav, editace volných textů ).  Explicitně v programu lze
   textový editor vyvolat příkazem  <b>edittxt</b>.  Textovým editorem lze zpracovat
   tři typy textu : <a href="#t327">volný text</a>, diskový textový soubor nebo lokální proměnnou
   procedury typu <a href="jazyk.md#t658">string</a>.  
   Příkazem <a href="#t604">SetEditTxt</a> nastavíme <a href="#t236">přepínače</a> textového editoru.

   ██ syntaxe:      <b>EDITTXT (</b> NázevTextu | LokálníProměnnáTypuString
                            [ <b>,noedit</b> ]
                            [ <b>,txtpos=</b>ČíselnýVýraz ] [<b>,txtxy=</b>ČíselnýVýraz ]
                            [ <b>,ww=(</b>ParametryOkna<b>)</b> ]
                            [ <b>,exit=(</b>SeznamPřerušení<b>)</b> ] 
                            [ <b>,head=</b>TextVýraz ]
                            [ <b>,last=</b>TextVýraz ] [ <b>,ctrl=</b>TextVýraz  ]
                            [ <b>,alt=</b>TextVýraz  ] [ <b>,shift=</b>TextVýraz ] 
                            [ <b>,errmsg=</b>TextVýraz ] <b>)</b>

   ■ <b>NázevTextu</b>: fyzické jméno (příp.včetně cesty) v apostrofech
                 nebo logické jméno (<a href="prostredi.md#t641">katalog</a>)
   ■ <b>noedit</b> .... pouze prohlížení, zákaz editace
   ■ <b>ww</b> ........ editace v okně (souřadnice,orámování,barva) viz. <a href="procedury.md#t474">with window</a>
   ■ <b>txtpos</b>..... počáteční nastavení kurzoru na pozici v textu (funkce <a href="#t591"> txtpos</a>)
   ■ <b>txtxy</b> ..... počáteční pozice kurzoru v okně <a href="#t591"> txtxy</a>
   ■ <b>exit</b> ...... <a href="#t596">přerušení editace textu</a>
   ■ <b>head</b> ...... Text pro hlavičku - 1.řádek (nahradí implicitní text). Text 
                 musí obsahovat masku '___________________________________'
                 (tj.35 podtržítek) pro ukazatel pozice v textu a stav přepí-
                 načů. Je-li maska kratší, přepíše se část textu hlavičky.
   ■ <b>last</b>,<b>ctrl</b>,<b>alt</b>,<b>shift</b> ... <a href="rozhrani.md#t523">alternativní nápověda</a>
   ■ <b>errmsg</b> .... zobrazení chybové zprávy při startu textového editoru

   <b>!!! Editace diskového souboru</b>
   Do verze FANDu 4.0 nebylo možno editovat (ani prohlížet) textový soubor na
   médiu s ochranou proti zapisu. Aktuální je to na CD, ale platí to i 
   v případě diskety s nastavenou ochranou proti zápisu, má-li soubor atribut
   RdOnly nebo je uložen v adresáři s nastavenými právy přístupu (LAN).

   <b>*</b>  Od verze FANDu 4.1 je diskový soubor otevírán vždy jen na čtení,
   <b>*</b>  změna na exclusívní otevření se provede až při pokusu o zápis do
   <b>*</b>  souboru.

   Toto platí jak pro příkaz <b>edittxt</b> tak i pro editaci textového souboru
   z hlavního menu PC FANDu.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t591">funkce textového editoru</a>       <a href="procedury.md#t588">příkazy pro tisk textu</a>       <a href="#t228">textový editor</a>
</pre>

<a name="t596"></a>
## přerušení editace textu

<pre>
                          <b>PŘERUŠENÍ EDITACE TEXTU</b>
   ──────────────────────────────────────────────────────────────────────────
   Práce  textového  editoru  může být  <b>přerušena</b>  jinou činností - tzv. exit
   - procedurou. Syntaxe <a href="#t536">přerušení editace</a> je stejná jako v datovém editoru s
   některými  logickými omezeními a odlišnostmi,  které vyplývají především z
   toho, jaký druh textu je editován.

   ■ Implicitně  se přerušení  textového  editoru  uskuteční v rámci datového
     editoru při editaci položky typu <a href="#t327">volný text</a> a použití <b>mode='EX'</b>. V tomto 
     případě dojde k předání rozpracovaného stavu editované věty do procedury 
     formou parametru procedury typu <a href="procedury.md#t405">record</a> of editovaný soubor, jehož prost-
     řednictvím  jsou změněné  hodnoty v exit - proceduře vráceny do datového
     editoru.  (viz. <a href="prostredi.md#t530">módy datového editoru</a>).

   ■ Explicitně se volá přerušení editace  textu při uvedení  parametru  <b>exit</b>
     příkazu  <b>edittxt</b>. V tomto případě nelze v seznamu  přerušení použít akci
     typu <b>record</b> nebo <b>seznam údajů</b> a dále nelze použít dodatečný parametr
     procedury typu <b>record of soubor</b> (viz. <a href="formular-sestava.md#t407">record proměnná</a>).

   ■ Přerušení editace textu nastavuje <a href="jazyk.md#t550">EdBreak</a>,<a href="#t591"> TxtPos</a>, <a href="prostredi.md#t298">TxtXY</a>

   ■ Přerušení editace <b>textového souboru</b>:
     Před přerušením se aktualizovaný text vypíše zpět do souboru a soubor se
     uzavře. Je tedy v exit-proceduře korektně přístupný. Při jeho změně nelze
     ovšem obecně automaticky zajistit návrat na původní místo.

   ■ Přerušení editace <b>lokální proměnné typu string</b>:
     Rozpracovaný text ( po případných změnách ) je v exit-proceduře dostupný,
     pokud ho explicitně předáme do exit-procedury jako  parametr typu string.

     <b>░░░░░░░░░░░░</b>   var s : string ;
     <b>░░příklady░░</b>   begin
     <b>░░░░░░░░░░░░</b>   edittxt( s, exit=( F10:ExProc(s) ));
                    ...
                    end ;
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t604"></a>
## setedittxt

*Též:* `align`, `colblk`, `wrap`, `left`, `right`, `overwr`, `indent`

<pre>
                                 <b>SETEDITTXT</b>
   ──────────────────────────────────────────────────────────────────────────
   Způsob práce  textového editoru lze  ovlivnit nastavením  přepínačů, které
   se nastavují interaktivně při editaci ( viz. <a href="#t236">přepínače</a> ) nebo <b>před startem</b>
   textového editoru příkazem procedury <b>SetEditTxt</b>.

   Přepínače  jsou  <b>globální v rámci PC FANDu</b>.  Tj. jednou  nastavená hodnota
   platí i po opuštění textového editoru a startu editoru s jiným textem nebo
   i v rámci jiné úlohy, dokud není explicitně nastavena jiná hodnota. 
   Při startu PC FANDu jsou přepínače resetovány na implicitní hodnoty.

   ██ syntaxe:   <b>SETEDITTXT (</b> [ <b>Overwr=</b>LogVýraz<b>,</b> ] [ <b>Indent=</b>LogVýraz<b>,</b>]
                              [ <b>Align=</b>LogVýraz<b>,</b>  ] [ <b>Wrap=</b>LogVýraz<b>,</b>]
                              [ <b>ColBlk=</b>LogVýraz<b>,</b> ]
                              [ <b>Left=</b>ČísVýraz<b>,</b>   ] [ <b>Right=</b>ČísVýraz<b>,</b>] <b>)</b>

   ■  <b>Overwr</b> .... vkládání/přepis         ■ <b>Align</b> ... zarovnání
   ■  <b>Indent</b>..... odsazení                ■ <b>ColBlk</b>... režim sloupcových bloků
   ■  <b>Left</b> ...... levý okraj textu        ■ <b>Wrap</b> .... automat.formátování
   ■  <b>Right</b> ..... pravý okraj textu

   Příkaz nastaví jen uvedené přepínače, ostatním  ponechá dosavadní hodnoty.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t230">editace textu</a>
</pre>
