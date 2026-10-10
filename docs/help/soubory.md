# Deklarace souboru (kapitola F)

<a name="t281"></a>
## ww

<pre>
                       <b>WW</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>ww</b> ................... <a href="procedury.md#t524">editace v okně</a>: obecný popis parametrů okna
   ■ edit(...<b>ww</b>...) ....... <a href="prostredi.md#t519">parametry editace</a>: editace v okně
   ■ edittxt(...<b>ww</b>...) .... <a href="editory.md#t595">příkazy pro editaci textu</a>: editace textu v okně
   ■ graph(...<b>ww</b>...) ...... <a href="rozhrani.md#t777">okno pro graf</a>
   ■ <b>ww</b>=(...) ............. <a href="#t350">uživatelské pohledy</a>: navigace v okně

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t313"></a>
## deklarace souboru

*Též:* `F`

<pre>
                       <b>DEKLARACE SOUBORU - kapitola F</b>
   ──────────────────────────────────────────────────────────────────────────
   Deklarace datového souboru obsahuje povinně uložené  údaje, další odstavce
   kapitoly (začínají speciálním znakem <b>#</b> a písmenem-zkratkou) jsou volitelné:

   ■ <a href="editory.md#t327">uložené údaje</a>              definice fyzické struktury souboru na disku
   ■ <a href="#t333">vypočítané údaje</a>      (<b>#C</b>) pojmenované výrazy pro další použití
   ■ <a href="#t336">klíče</a>                 (<b>#K</b>) vyhledávání, spojení mezi soubory, integrita
   ■ <a href="#t348">aditivní změny</a>        (<b>#A</b>) číselná aktualizace údajů nadřízeného souboru
   ■ <a href="#t350">uživatelské pohledy</a>   (<b>#U</b>) navigace po databázi, přístupová práva
   ■ <a href="#t356">závislosti</a>            (<b>#D</b>) kontextové pořízení a editace dat
   ■ <a href="#t354">logické kontroly</a>      (<b>#L</b>) kontrola správnosti zadaných dat
   ■ <a href="#t358">implicitní hodnoty</a>    (<b>#I</b>) pohodlnější pořizování nových vět

   Názvem kapitoly F (<b>NázevSouboru</b>) se na <b>datový soubor</b> odvoláváme v projektu, 
   výjimečně se v názvu kapitoly F (ne v dalších odkazech) zadává přípona:

   ■ <a href="#t318">indexovaný soubor</a>   (<b>.X</b>  ) automatická indexová podpora pro vlastní klíče
   ■ <a href="prostredi.md#t320">projektový soubor</a>   (<b>.RDB</b>) údržba jiného projektu
   ■ <a href="prostredi.md#t322">soubor pro nápovědu</a> (<b>.HLP</b>) nápověda k úloze (help-soubor)
   ■ <a href="#t324">soubor formátu .DBF</a> (<b>.DBF</b>) datová kompatibilita s databázemi typu XBase
   ■ <a href="sql.md#t885">SQL</a> - tabulka       (<b>.SQL</b>) možnost prezentace souboru jako tabulky na SQL
                                serveru

   ■ <b>speciální deklarace</b>: <a href="#t314">like</a> ...... zjednodušená deklarace stejných souborů
     (odkaz místo textu)  <a href="#t315">journalof</a> ... žurnál pro sledování změn při editaci
   ──────────────────────────────────────────────────────────────────────────
  <a href="#t316">░ deklarace souboru</a>     <a href="#t785">»F</a>  <a href="#t417">lokální soubory</a>
</pre>

<a name="t314"></a>
## like

<pre>
                               <b>LIKE deklarace</b>
  ──────────────────────────────────────────────────────────────────────────
   Deklaraci jednoho  souboru  lze použít  jako základ  pro deklaraci dalšího
   souboru. Přebírají se všechny  uložené údaje i další  odstavce  deklarace.
   Deklarace LIKE může odkazovat na soubor, deklarovaný také like. Vidíme zde
   tedy jisté prvky objektového přístupu.

   ██ <b>Syntaxe:</b>    <b>like</b> NázevSouboru [ <b>(Prefix)</b> ] [ ; <b>RozšířeníDeklarace</b> ]

   ■  <b>RozšířeníDeklarace</b>   Může obsahovat obvyklé syntaktické konstrukce
                           kapitoly F, které rozšiřují původní deklaraci.
                           <b>Omezení:</b> Pokud je originál indexovaný soubor,
                           musí být i like-soubor indexovaný.
   ■  <b>Prefix</b> ............. Pro indexové soubory se musí zajistit jednoznačnost 
                           názvů klíčů, což se provede pomocí prefixu klíče.
                           "Prefix<b>_</b>NázevKlíče". 
                           Implicitně je prefix NázevSouboru.

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>   F  UCTY2    *   like UCTY1 ;
   <b>░░░░░░░░░░░░</b>                   Dodatek : A,10 ;     { další uložený údaj }
                                  #U DalšíPohled (1,2) : () ;
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t313">Deklarace souboru</a>
</pre>

<a name="t315"></a>
## journalof

<pre>
                                 <b>JOURNALOF</b>
  ──────────────────────────────────────────────────────────────────────────
   █ <b>journalof</b> NázevSouboru ... deklarace <b>žurnálu</b> pro sledování změn

   Deklarace souboru pro automatické zaznamenávání změn při editaci původního
   datového souboru <b>NázevSouboru</b> (při volání datového editoru s parametrem
   <b>journal</b>). Žurnál obsahuje následující pomocné údaje pro klasifikaci změn:

   ■ <b>Upd</b>      :A,1 ..... typ změny (<b>+</b> nová věta, <b>-</b> zrušená věta,
                         <b>O</b> a <b>N</b> starý a nový obsah změněné věty)
   ■ <b>RecNr</b>    :F,8.0 ... fyzické číslo věty originálního souboru
   ■ <b>User</b>     :F,4.0 ... kód uživatele, který editoval soubor (<a href="jazyk.md#t649">usercode</a>)
   ■ <b>TimeStamp</b>:D,'DD.MM.YY hh:mm:ss' ... datum a čas provedení změny
   a dále všechny uložené údaje souboru <b>NázevSouboru</b> - kromě údajů typu T.

   Verze: - do verze 3.2 bylo možno deklarovat žurnál pouze pro soubor bez
            volných textů
          - od verze 3.3 toto omezení neplatí, ale místo údajů typu T se
            do žurnálu přenese stejnojmenný údaj typu F,9.0.


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   F ZURNAL   * <b>journalof</b> DATA                 {žurnál pro sledování změn}
   P Editace  * edit(DATA,(),<b>journal</b>=ZURNAL);  {editace s autom.zápisem změn}
   Při změně deklarace souboru DATA bude automaticky upraven i ZURNAL
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t316"></a>
## ░ deklarace souboru

<pre>
                        <b>PŘÍKLAD - DEKLARACE SOUBORU</b>
   ──────────────────────────────────────────────────────────────────────────
        Číslo : N,5;                                          {uložené údaje}
        Jméno : A,20;
      Narozen : D;          {datum narození}
        Mesto : A,30;       {bydliště}
          PSC : N,5;
     ZáklMzda : F,5.0;
         Stav : A,1;
    Příplatky : B;
     Poznámky : T;
                                            {další odstavce deklarace souboru}
   <a href="#t333">#C</a> Příjmení:=copy(Jméno,pos(' ',Jméno)+1,12):A,10;       {vypočítaný údaj}

   <a href="#t336">#K</a> @ Číslo;                                                 {vlastní klíč}
      PŘÍJMENÍ(@) * ~Příjmení; {alternativní vlastní klíč-soubor je indexovaný}
      PodleMesta(@) * ~Mesto;
      MESTA ! Mesto;   {vazba typu owner na nadřízený soubor MESTA s referenční}
                       {integritou}

   <a href="#t333">#C</a> HrubáMzda:=ZáklMzda+cond(Příplatky:1000):F,5.0;{další vypočítané údaje}
      RočníMzda:=HrubáMzda*12:F,6.0;
      PSCtisk:=copy(psc,1,3)+' '+copy(psc,4,2) :A,6;     {PSC ve tvaru XXX YY }

   <a href="#t348">#A</a> PARAM.SoučetZáklMezd+=ZáklMzda;       {aditivní změna do param.souboru}

   <a href="#t350">#U</a> SEZNAM(0): (Číslo,Příjmení),mode='!!^n^y';         {uživatelský pohled}

   <a href="#t354">#L</a> Stav in ['S','Z','V']:'Stav: S-svobodný,Z-ženatý/vdaná,V-ovdovělý(-á)';
      ZáklMzda&gt;=PARAM.MinMzda;                             {logické kontroly}
      Věk&lt;60 ? : 'POZOR - věk adepta nad 60 let';                  {varování}

   <a href="#t358">#I</a> Příplatky:=false;               {implicitní hodnoty pro pořizování dat}
      Stav:='Z';
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t336">klíče</a>     <a href="#t348">aditivní změny</a>
</pre>

<a name="t318"></a>
## indexovaný soubor

*Též:* `indexy`

<pre>
                              <b>INDEXOVANÝ SOUBOR</b>
   ───────────────────────────────────────────────────────────────────────────
   Indexovaný soubor je datový soubor  podporovaný indexy. <b>Indexy</b> jsou fyzicky
   uloženy v pomocném souboru na disku s příponou .X00, PC FAND je automaticky 
   vytváří, při  editaci aktualizuje, v případě potřeby zruší a znovu vytvoří.

   <b>Zavedení indexové podpory:</b>
   K definici souboru v kapitole F doplňte příponu <b>.X</b> za <b>NázevSouboru</b>. Smazání 
   přípony zruší i indexy pro tento datový soubor. Soubor může být indexovaný,
   když má alespoň jeden  <b>vlastní klíč</b>.  Naopak soubor, který má alespoň jeden
   <b>alternativní vlastní klíč</b>, již musí být nutně indexovaný.
   
   <b>Použití indexů:</b>
   Udržuje  soubor  <b>setříděný</b>  bez ohledu na fyzické  pořadí vět, a to i podle
   několika různých hledisek najednou.  Zabraňuje  nechtěným <b>duplicitám klíčů</b>,
   zrychluje odezvu editoru při mazání a vkládání vět (zrušené věty jsou pouze 
   označeny, ne fyzicky mazány). Umožňuje <b>navigaci směrem dolů</b> (Ctrl-F7)- pod-
   řízený soubor musí mít vlastní klíč podporovaný indexy, který je rozšířením
   cizího klíče, podle kterého se hledá (tzv. vazba typu <a href="#t437">owner</a>). 
   Má vliv na celý projekt:

   <a href="editory.md#t251">navigace</a> nahoru a dolů, editace podle klíče, <b>F3</b> hledání v datovém editoru
   <a href="#t338">vlastní klíč</a>, alternativní vlastní klíč, <a href="#t350">uživatelské pohledy</a> v deklaraci 
   soub. <a href="formular-sestava.md#t361">formulář</a>, <a href="formular-sestava.md#t369">VstupSestavy</a>, <a href="formular-sestava.md#t392">VstupTransformace</a>: pořadí vět v souboru podle 
   indexu, hledání <a href="jazyk.md#t285">recno</a> a <a href="jazyk.md#t580">link</a>, volání <a href="procedury.md#t441">forall</a>, <a href="prostredi.md#t282">edit</a> a <a href="formular-sestava.md#t556">report</a> podle indexu 
   v proceduře
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t319">░ indexy</a>
</pre>

<a name="t319"></a>
## ░ indexy

<pre>
                              <b>PŘÍKLAD - INDEXY</b>
 ──────────────────────────────────────────────────────────────────────────────
 ░ #K @ Číslo                                        {deklarace souboru ADRESY}
 ░ #K @ * Číslo                                      {deklarace souboru DOPISY}
 ░    ADRESY Číslo      {takto je umožněna navigace směrem dolů ADRESY-&gt;DOPISY}

 ░ #_DATA/Klíč      {editace ve formuláři podle alternativního vlastního klíče}
 ░ #I_DATA/@                           {pořadí v sestavě podle vlastního klíče}
 ░ #I1_DATA/Klíč   {pořadí v transformaci podle alternativního vlastního klíče}

 ░ i:=recno(DATA/Klíč,s)         {hledání podle alternativního vlastního klíče}
 ░ forall i in DATA/Klíč do ...                      {průchod souborem v cyklu}
 ░ edit(DATA/Klíč,...)                          {pořadí vět při volání editoru}
 ░ report(DATA/@,...)                       {pořadí vět při generování sestavy}

 ░ nadřízený soubor NAD ... #K @ Číslo      {takto definované spojení umožňuje}
 ░ podřízený soubor POD ... #K @ * Číslo                 {navigaci směrem dolů}
 ░                             NAD Číslo           {a konstrukce owner, key in}
 ░ v proceduře ... edit(POD,(),owner=NAD[10]);
 ░                 forall i in POD/@ owner NAD[ČísloVěty] do ... ;
 ░                 forall VětaPod/@ (key in ['100'..'299','500']) do ... ;
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t338">vlastní klíč</a>                  <a href="#t350">uživatelské pohledy</a>
</pre>

<a name="t324"></a>
## soubor formátu .DBF

*Též:* `DBF`

<pre>
                            <b>SOUBOR FORMÁTU .DBF</b>
   ───────────────────────────────────────────────────────────────────────────
   Soubor s příponou .DBF v <b>NázvuSouboru</b> bude na disku uložen ve standardním
   formátu <b>.DBF</b>  (dBASE III,  včetně volných textů <b>.DBT</b> nebo <b>.FPT</b>) a může  být
   přímo  zpracováván databázemi  typu  <b>XBase</b>.  Kromě  formátu  uložení  se  z
   uživatelského hlediska  .DBF soubory neliší od ostatních datových  souborů.
   .DBF soubory však nemohou mít indexovou podporu a nelze je sdílet v lokální
   síti.
   Převodní vztah mezi typy údajů PC FANDu a XBase ukazuje tabulka:
  ┌──────────┬────────────┬───────────────────────────────────────────┐
  │ PC FAND  │  XBase     │  poznámky                                 │
  ├──────────┼────────────┼───────────────────────────────────────────┤
  │ F,n.m    │F,N n+m+1,m │ (totéž platí i pro F,n,m)                 │
  │ F,n.0    │F,N n       │                                           │
  │ A,n      │  C n       │                                           │
  │ D        │  D         │ (bez ohledu na masku zobrazení v PC FANDu)│
  │ T        │  M         │                                           │
  │ B        │  L         │                                           │
  └──────────┴────────────┴───────────────────────────────────────────┘
  <b>Zrušené věty</b>  v .DBF souboru budou zobrazeny jinou barvou a po sort nebo
  merge se ze souboru vypustí. Když  má  deklarace  souboru  .DBF  prázdný  
  <b>Text</b>, při  překladu doplní PC FAND  deklaraci  automaticky  podle  
  hlavičky existujícího  souboru na disku.

  <b>Vypočtené údaje</b> v souboru DBF mohou být i typů <b>N</b> a <b>R</b>, které však nelze použít
  jako typy uložených údajů.
  ─────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t333"></a>
## vypočítané údaje

*Též:* `#C`

<pre>
                       <b>VYPOČÍTANÉ ÚDAJE - odstavec #C</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Vypočítané údaje</b> jsou pojmenované výrazy.  Ve výrazech se mohou vyskytovat
   operátory  a  funkce,  ostatní  uložené i vypočítané  údaje téhož souboru,
   viditelné údaje  z nadřízených souborů a globální proměnné. Seznam vypočí-
   taných údajů je ve tvaru:

   ██ <b>NázevÚdaje:=Výraz:TypÚdaje;</b> ... <b>vypočítaný údaj</b> (pojmenování výrazu)

   Typ údaje udává pouze způsob  zobrazení  v <b>datovém editoru</b> a <b>automatických</b>
   <b>sestavách</b> , výrazy  se interně vyhodnocují a do dalších  výrazů vstupují v
   plném  rozsahu v typech <b>real</b>, <b>string</b>, <b>boolean</b>.  Zarovnání <b>L</b> u typu N resp.
   <b>R</b> u typu  A se uplatní pouze při  zadání  hodnoty údaje  pro hledání podle
   klíče (F3).


   <b>░░░░░░░░░░░░</b>     <b>#C</b>    Cena:=JednCena*Množství:F,4.2;
   <b>░░příklady░░</b>           Jméno:=Křest+' '+Příjm:A,20;
   <b>░░░░░░░░░░░░</b>           Psč6:=copy(Psč,1,3)+' '+copy(Psč,4,2):A,6;
                          Bezdětný:=Děti=0:B;
                          Adresa:=Ulice+'\13\10'+Místo:T;
   Přídavky:=cond(Deti=1: 200,Deti=2: 580,Deti&gt;=3: 1040+(Deti-3)*300):F,4.0;
   ──────────────────────────────────────────────────────────────────────────
   <a href="jazyk.md#t654">výrazy</a>      <a href="editory.md#t327">typy údajů</a>      <a href="jazyk.md#t658">typy výrazů</a>      <a href="#t785">»F</a>
</pre>

<a name="t336"></a>
## klíče

*Též:* `#K`, `klíč`

<pre>
                            <b>KLÍČE - odstavec #K</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Klíč</b> je jeden  nebo  několik  údajů ( uložených nebo vypočítaných ) z věty
   souboru, které postačují k identifikaci celé věty. Používají se k vyhledá-
   vání vět a ke spojování souborů. Rozlišujeme následující typy klíčů:

   ■ <a href="#t338">vlastní klíč</a> ........ slouží k identifikaci věty uvnitř souboru
                           umožňuje rychlé hledání v datovém editoru pomocí
                           <b>F3</b> a připojení podřízených souborů cizím klíčem
   ■ <a href="jazyk.md#t340">parametrický klíč</a> ... definice <b>globálních proměnných</b> úlohy
   ■ <a href="jazyk.md#t346">cizí klíč</a> ........... napojení do nadřízeného souboru (číselníku)
                           umožňuje přebírat údaje z <b>viditelné věty</b>
                           (věta nadřízeného souboru se stejnou hodnotou klíče)
                           a <b>navigaci</b> po databázi směrem nahoru i dolů

   Definice klíčů se mohou v deklaraci střídat s odstavcem <b>vypočítaných údajů</b>. 
   Pomocí parametrických a cizích klíčů se odvoláváme ve výrazech na údaje
   viditelných souborů <b>tečkovou notací</b>:

   █ <b>NázevSouboru.NázevÚdaje</b> ... tečková notace
   █ <b>NázevSpojení.NázevÚdaje</b> ... podle typu spojení nadřízeného souboru
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t318">indexovaný soubor</a>        <a href="editory.md#t251">navigace po databázi</a>          <a href="#t316">░ deklarace souboru</a>
</pre>

<a name="t338"></a>
## vlastní klíč

*Též:* `intervalový klíč`

<pre>
                                <b>VLASTNÍ KLÍČ</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Vlastní klíč</b>  identifikuje zadáním jednoho nebo několika údajů větu soubo-
   ru. Může být  <b>jednoznačný</b> nebo <b>duplicitní</b>. Soubor musí být podle vlastního
   klíče <b>setříděn</b>.  Soubor může mít jeden nebo několik vlastních klíčů, potom
   rozlišujeme  jeden  <b>vlastní klíč</b> ( na něj se odvoláváme jménem souboru ) a
   ostatní  <b>alternativní vlastní klíče</b>,  které musíme pojmenovat. Volné texty
   (textové údaje typu T) nemohou být součástí klíče.

   █ <b>@ KlíčovéÚdaje</b> ............... <b>vlastní klíč</b> (identifikace věty,jen jeden)
   █ <b>NázevKlíče(@) KlíčovéÚdaje</b> ... <b>alternativní vlastní klíč</b>
     (udržuje najednou setřídění souboru podle několika různých hledisek)

   ■ <b>&lt;=</b> před seznamem údajů ... <b>intervalový klíč</b> (klíče pro celé intervaly)
   ■ <b>*</b>  před seznamem údajů ... <b>duplicitní klíč</b> (jinak jsou duplic. zakázány)
   ■ <b>&gt;</b>  před názvem klíčového údaje ... <b>sestupné třídění</b> (jinak vzestupné)
   ■ <b>~</b>  před názvem klíčového údaje ... <b>lexikální třídění</b> (národní abeceda)
                                        lze použít jen pro údaje typu A

   Vlastní klíče  mohou být  podporovány  <b>indexy</b>.  Potom je setřídění souboru
   podle všech vlastních  klíčů  automaticky  zajištěno <b>indexovaným souborem</b>.
   Pokud soubor nemá indexy,může mít nejvýše jeden vlastní klíč a programátor
   musí zajistit, aby byl soubor <b>setříděn</b> podle vlastního klíče.


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   #K @ Název;                           {příklady deklarace vlastních klíčů}
   #K @ Datum,Číslo,~Jméno;                  {klíč s lexikálním vyhledáváním}
   #K @ * Měsíc;                                            {duplicitní klíč}
   #K @ &lt;= TarifPásmo;                                     {intervalový klíč}
   #K ČÍSFAK(@) ČísloFaktury;                     {alternativní vlastní klíč}
 
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t318">indexy</a>         <a href="jazyk.md#t346">cizí klíč</a>        <a href="jazyk.md#t668">lexikální třídění</a>        <a href="#t785">»F</a>
</pre>

<a name="t348"></a>
## aditivní změny

*Též:* `#A`

<pre>
                        <b>ADITIVNÍ ZMĚNY - odstavec #A</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Aditivní změny</b>  umožňují  aktualizovat  během editace  <b>podřízeného souboru</b>
   číselné údaje <b>nadřízeného souboru</b>  podle zadaného  číselného výrazu. Výraz
   se vyhodnotí před a po editaci věty, pokud se změnila jeho hodnota, rozdíl  
   se přičte k aktualizovanému údaji <b>viditelné věty</b> nadřízeného  souboru (pro  
   nové věty se výraz přičte, pro zrušené věty se odečte). Soubory samozřejmě  
   musejí být spojeny <b>klíčem</b>.  Změny se provedou tranzitivně přes více úrovní
   nadřízených souborů.

   ██ Syntaxe:   <b>#A</b> { ViditelnýÚdaj [ <b>(</b> LogVýraz [ <b>:'</b>ChybovéHlášení<b>'</b> ] <b>)</b> ]
                    [ <b>!</b> | <b>!!</b> ] [ <b>+=</b> | <b>:=</b> ] ČísVýraz <b>;</b> }

   ■ při použití <b>+=</b> se provede aditivní změna tak, jak byla výše popsána
   ■ při použití <b>:=</b> se provede prosté přiřazení nové hodnoty (nikoliv rozdíl)
   ■ <b>Viditelný údaj</b> může být údaj nadřízeného souboru nebo globální proměnná
                    (tj. údaj parametrického souboru)

   █ <b>NázevSpojení.NázevÚdaje+=ČísVýraz;</b> ... <b>aditivní změna</b> do nadřízeného 
                                            souboru (minimální varianta)
   ■ <b>(LogVýraz)</b> ........ před aditivní změnou se provede logická kontrola
   ■ <b>'ChybovéHlášení'</b>... chyba vyvolá toto hlášení
   ■ <b>!</b>  ................ dotaz na vytvoření viditelné věty, když neexistuje
   ■ <b>!!</b> ................ automatické vytvoření viditelné věty bez dotazu
                    (naplní se hodnoty klíčových údajů podle podřízené věty)

   Aditivní změna  předpokládá existenci  viditelné věty , jinak hlásí editor
   chybu <b>"není věta pro kumulaci"</b>. Pomocí <b>navigace</b> lze chybějící větu doplnit 
   ručně, uvedení znaku <b>!</b> v deklaraci toto zajistí automaticky. Pokud logická 
   <b>kontrola v nadřízeném souboru</b>  spuštěná  před  provedením  aditivní  změny
   skončí chybou, aditivní změna se neprovede  a uživatel musí opravit edito-
   vanou větu nebo anulovat změny, případně přepínačem vypnout aditivní změny. 
   Logické  kontroly v podřízeném  souboru  lze simulovat  konstrukcí <b>cond</b> v
   číselném výrazu.

   ■ Při editaci souboru může obsluha provádění aditivních změn vypnout.
     Viz. <a href="editory.md#t246">přepínače datového editoru</a>.
   ■ Programátor může toto obsluze zakázat pomocí parametru editace mode='<b>#A</b>'.
     Viz. <a href="prostredi.md#t530">mody datového editoru</a>


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   #A SKLAD.Stav += Množství;            {příklady deklarace aditivních změn}
      SKLAD.Stav += cond(Zaevidováno:Množství);
      SKLAD.Stav (Stav&gt;=PARAM.Min) += Množství;        {s logickou kontrolou}
      SKLAD.Stav (Stav&gt;0:'Není na skladě') += Množství; {s chybovým hlášením}
      SKLAD.Stav ! += Množství;    {s vytvořením chybějící věty pro kumulaci}
      SKLAD.Stav !! += Množství;                           {totéž bez dotazu}
 
   #A PARAM.Počet += 1;                  {kumulace do parametrického souboru}

   #A KUMUL.Zisk !! += cond(Příjmy&gt;Výdaje:Příjmy-Výdaje);
      KUMUL.Ztráta !! += cond(Příjmy&lt;Výdaje:Výdaje-Příjmy);

   ──────────────────────────────────────────────────────────────────────────
   <a href="jazyk.md#t346">cizí klíč</a>       <a href="editory.md#t327">uložené údaje</a>      <a href="#t785">»F</a>
</pre>

<a name="t350"></a>
## uživatelské pohledy

*Též:* `#U`

<pre>
                     <b>UŽIVATELSKÉ POHLEDY - odstavec #U</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Uživatelský  pohled</b> definuje  parametry datového  editoru  (seznam  údajů,
   rozvrh obrazovky,  přepínače, mód  editace, podmnožinu  atd.) pro  editaci
   souboru. Tato  definice se  použije při  <b>navigaci po  databázi</b>, kdy  místo
   výběru souboru a údajů editor ihned přejde do editace podle  uživatelského
   pohledu.  Další použití  <b>uživatelských pohledů</b>  je pro omezení přístupu  k
   souboru některým  třídám uživatelů  (<b>práva  přístupu</b>)  a  volání  datového
   editoru z procedury (parametr <b>U=NázevPohledu</b>).

   ██ Syntaxe:   <b>#U</b> NázevPohledu<b>(</b>PřístupováPráva<b>) :</b> 
                    [<b>/</b>NázevKlíče<b>,</b> ] DruhEditace [ ParametryEditace ]

   ■ <b>PřístupováPráva</b> ... Seznam uživatelských práv přístupu, pro které se
                         pohled nabídne při navigaci (ostatní uživatelé mají
                         přístup blokován). Práva jsou dána kapitolou <b>U</b> 
                         a vstupním <b>heslem</b> nebo pomocí interní proměnné
                         <a href="jazyk.md#t649">accright</a>.   ( <a href="jazyk.md#t649">práva přístupu k datům</a> )
                         Při ladění je kód=<b>0</b> a všechny uživatelské pohledy
                         jsou přístupné (absolutní přístupová práva).
                         Při zadávání seznamu lze použít i interval x..y

   ■ <b>NázevKlíče</b> .... (volitelně) editace podle alternativního vlastního klíče

   ■ <b>DruhEditace</b> ....... <b>Název formuláře</b> (kapitola E) nebo <b>seznam údajů</b> pro
                         automatický rozvrh obrazovky, nebo <b>?</b> pro interaktivní
                         výběr údajů, nebo název <b>formuláře</b>.

   ■ <b>Parametry editace</b>...(volitelně) obdobně jako při volání datového editoru
                         (<a href="prostredi.md#t519">tab</a>, <a href="prostredi.md#t519">dupl</a>, <a href="prostredi.md#t283">mode</a>, <a href="jazyk.md#t279">cond</a>, <a href="prostredi.md#t519">journal</a>, <a href="prostredi.md#t519">saveafter</a>, <a href="#t281">ww</a>, <a href="prostredi.md#t284">head</a>, 
                         <a href="rozhrani.md#t523">last</a>, <a href="prostredi.md#t519">watch</a>, <a href="prostredi.md#t519">refresh</a>, <a href="rozhrani.md#t287">ctrl</a>, <a href="rozhrani.md#t523">alt</a>, <a href="rozhrani.md#t523">shift</a>, <a href="procedury.md#t288">exit</a>)


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   #U POHLED(0): (Název,Cena)                {příklady uživatelských pohledů}
      DATA(1,6..9): ()                        {omezení podle třídy uživatele}
      SEZNAM(3): /KLÍČ (Číslo,Jméno)   {podle alternativního vlastního klíče}
      VÝBĚR (0): (?), mode='!!'         {s výběrem údajů, jen pro prohlížení}
      TELEF(5): Telefony, cond=Tlf&lt;&gt;~''               {formulář a podmnožina}

   edit(ADRESÁŘ, U=TELEF)         {použití uživatelského pohledu v proceduře}
   ──────────────────────────────────────────────────────────────────────────
   <a href="editory.md#t251">navigace</a>     <a href="dalsi-kapitoly.md#t644">seznam uživatelů</a>       <a href="#t785">»F</a>                        <a href="jazyk.md#t733">heslo</a>
   <a href="prostredi.md#t282">edit</a>         <a href="kontextova-napoveda.md#t9">instalace úlohy</a>        <a href="editory.md#t327">uložené údaje</a>
</pre>

<a name="t354"></a>
## logické kontroly

*Též:* `#L`, `varování`, `kontroly`

<pre>
                       <b>LOGICKÉ KONTROLY - odstavec #L</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Logická kontrola</b>  je obecný <b>logický výraz</b>, který musí splňovat každá  věta
   souboru. Jestliže  je výsledkem jeho vyhodnocení hodnota false ( nepravda,
   tzn. došlo  k chybě  integrity dat ), datový editor vypíše <b>chybové hlášení</b>
   a bude požadovat opravu.

   ██ Syntaxe:    <b>#L</b> { LogickýVýraz [<b>?</b>] [ <b>:</b> ChybovéHlášení ] [ <b>,</b>Heslo ] <b>;</b> }

   ■  <b>LogickýVýraz</b> ..... Vlastní logická kontrola.
   ■  <b>?</b> ................ Jde jen o varování, má samostatný přepínač (F5),
                         místo <b>error</b> se použije <a href="#t400">warning</a>
   ■  <b>ChybovéHlášení</b> ... Textový výraz.
                         Pokud věta nesplňuje LogickýVýraz, vypíše se toto
                         chybové hlášení. Není-li uvedeno, vypíše se text
                         logického výrazu.
   ■  <b>Heslo</b> ............ Odkaz do <b>help-souboru</b> při nesplnění log.podmínky.
                         Help může obsahovat podrobnější pokyny pro obsluhu.
                         Pokud heslo není <a href="jazyk.md#t652">identifikátor</a>, musí být uzavřeno
                         v apostrofech (textový výraz). Po stisku <b>F1</b> se 
                         zobrazí jeho Text a vyvolá se <b>nápověda</b>

   Logické kontroly lze vypnout (F5 v datovém editoru) - nevztahuje se na
   varování. Vypnutí logických kontrol obsluhou může programátor zakázat
   použitím mode='#L' (<a href="prostredi.md#t530">módy datového editoru</a>).

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   <b>#L</b> Věk&gt;15 : 'Špatně zadané datum narození';
      Pohlaví in ['M','Z'] : 'Přípustné hodnoty jsou M a Z';
      Mzda&gt;PARAM.MinMzda &amp; Mzda&lt;=10000
      Množství&gt;0: 'Množství musí být kladné', 'Heslo'
      ČÍSELNÍK.exist: 'Tento druh zboží neexistuje v číselníku'
      Množství&gt;ZBOŽÍ.MaxMnož =&gt; Sleva&gt;0
   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t322">soubor pro nápovědu</a>      <a href="#t397">error</a>         <a href="#t333">vypočítané údaje</a>               <a href="#t785">»F</a>
   <a href="prostredi.md#t248">automatická sestava</a>      <a href="#t400">errortext</a>     <a href="editory.md#t246">přepínače datového editoru</a>     <a href="prostredi.md#t283">mode</a>
</pre>

<a name="t356"></a>
## závislosti

*Též:* `#D`

<pre>
                          <b>ZÁVISLOSTI - odstavec #D</b>
  ────────────────────────────────────────────────────────────────────────────
  Pomocí <b>závislostí</b> může programátor řešit <b>kontextové pořízení dat</b>. Na základě
  některých již pořízených údajů (obecně <b>logického výrazu</b>) je možné jiné údaje
  dosadit automaticky nebo vůbec přeskočit,  protože  v  daném kontextu nemají
  význam.

  ██ Syntaxe:       <b>#D</b> { <b>(</b>LogVýraz<b>)</b> NázevÚdaje<b>:=</b>Výraz<b>;</b> }

  Pro  jeden  logický  výraz  (podmínku)   může   následovat  seznam  několika
  přiřazovacích  příkazů. V případě, že je splněn logický výraz, hodnota údaje
  se dosadí a datový kurzor  se posune na další údaj.  Při nesplnění logického
  výrazu je třeba údaj pořídit nebo editovat ručně.  Závislosti se vyhodnocují
  během pořízení i editace pouze při příchodu k údaji klávesou <b>Enter</b>.


  <b>░░░░░░░░░░░░</b>
  <b>░░příklady░░</b>        <b>#D</b> (ADRESY.exist)    Jméno:=ADRESY.Jméno;
  <b>░░░░░░░░░░░░</b>                            Adresa:=ADRESY.Adresa;
                      <b>#D</b> (DruhMzdy='03')   Třída:=''; Sazba:=0;
                         (DruhMzdy='04')    Úkol:=0; Prémie:=Sazba*0.15;
  ───────────────────────────────────────────────────────────────────────────
  <a href="#t333">vypočítané údaje</a>
</pre>

<a name="t358"></a>
## implicitní hodnoty

*Též:* `#I`

<pre>
                      <b>IMPLICITNÍ HODNOTY - odstavec #I</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Implicitní hodnota</b> se definuje pro rychlejší pořizování dat dat. editorem.
   Když při pořizování nových vět datový  kurzor  dojde  až k údaji, který má
   implicitní hodnotu, vypočte se zadaný výraz a  hodnota se zobrazí na místě
   údaje. Obsluha potom hodnotu potvrdí,  edituje nebo zadá jinou. Implicitní
   hodnoty se při pořízení nasazují i do <b>nezobrazených údajů</b> věty.
 
   ██ Syntaxe:     <b>#I</b> {  NázevÚdaje<b>:=</b>Výraz<b>;</b> }

   Při opravách v  <b>módu editace</b>  dosáhne uživatel nasazení implicitní hodnoty
   tak, že původní obsah údaje  zruší  (včetně  mezer)  a  odešle  k  uložení  
   prázdný řetězec.

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   #I Datum:=today+currtime;
      Norma:=cond(Muž:8.5, Žena:7);
      Prémie:=Mzda*0.15 + Soboty*20;
      Jméno:=ADRESÁŘ.Jméno;
   ──────────────────────────────────────────────────────────────────────────
   <a href="editory.md#t327">uložené údaje</a>
</pre>

<a name="t397"></a>
## error

<pre>
                                   <b>error</b>
   ──────────────────────────────────────────────────────────────────────────
   Klíčové slovo <b>error</b> lze použít ve dvou kontextech.

   ■ V transformaci - <a href="#t400">speciální funkce v transformaci</a>.

   ■ Ukončení L kapitoly (<a href="dalsi-kapitoly.md#t907"> error</a>).

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t400"></a>
## speciální funkce v transformaci

*Též:* `errortext`, `warning`

<pre>
                      <b>SPECIÁLNÍ FUNKCE V TRANSFORMACI</b>
 ──────────────────────────────────────────────────────────────────────────────
 Na pravé straně  přiřazovacích příkazů výstupní části transformace mohou kromě
 obvyklých konstrukcí vystupovat <b>speciální funkce</b>:

 █ <b>sum</b> :real .............. součtování (pouze ve výstupech <b>#O</b> a <b>#O*</b>)

 █ <b>group</b> :real ............ pořadí aktuální zpracovávané skupiny vět
 █ <b>Ii.count</b> :real ......... pořadí věty vstupního souboru v aktuální skupině
                            (rozdělení vstupů do skupin podle řídících údajů)

 █ <b>Ii.error</b> :boolean ...... dvojice funkcí, která vrací výsledek logických 
 █ <b>Ii.warning</b> :boolean .... kontrol ve větě. Test se provádí v pořadí podle
                            deklarace #L a skončí na první nesplněné podmínce
                            pro logickou kontrolu !!!  Tj. nesplněná varování
                            "přejde". <b>Warning</b> je v případě jakékoliv nesplněné
                            podmínky vždy TRUE, <b>error</b> jen tehdy, pokud tato
                            podmínka byla logickou kontrolou (nikoliv jen
                            varováním).

                            Výsledek testů věty           Hodnoty
                            Kontroly   Varování        <b>error</b>   <b>warning</b>
                              ANO        ANO           FALSE    FALSE
                              ANO         NE           FALSE    TRUE
                               NE        ANO           TRUE     TRUE
                               NE         NE           TRUE     TRUE

 █ <b>Ii.errortext</b> :string ... je-li ve vstupní větě chyba, text chybového 
                            případně varovného hlášení


 Funkce <b>sum</b>, <b>group</b> a  <b>count</b>  jsou  obdobně  použitelné  také  v  popisné  části
 výstupní úrovně sestavy (kapitola R).
 ──────────────────────────────────────────────────────────────────────────────
 <a href="formular-sestava.md#t396">PříkazováČástVýstupu</a>            <a href="formular-sestava.md#t395">VýstupTransformace</a>      <a href="formular-sestava.md#t401">░ transformace</a>      <a href="formular-sestava.md#t788">»M</a>
 <a href="jazyk.md#t383">Speciální funkce v sestavě</a>
</pre>

<a name="t417"></a>
## file

*Též:* `lokální soubory`

<pre>
                                    <b>FILE</b>
   ──────────────────────────────────────────────────────────────────────────
   Použití typu <b>file</b> (soubor) se pro parametry procedury poněkud liší od 
   použití jako typu lokální proměnné procedury.

   <b>Parametr typu FILE</b>
   <b>──────────────────</b>

   ██  ( ... ; <b>NázevSouboru : FILE ;</b> ... )

   ■ <b>NázevSouboru</b> ...  Při běžném překladu (Ctrl-F8) se bere jako reálný 
                       soubor (název viditelné kapitoly F). Při vyvolání
                       procedury se substituuje aktuálním parametrem, lze
                       použít i dynamickou deklaraci souboru.

   Význam parametru typu file je především pro psaní zobecněných procedur,tj.
   procedur, použitelných pro více souborů. Dynamizací  <b>aktuálního</b> parametru
   typu file lze deklarovat lokální soubor.

   ██          <b>Proc</b>( NázevProcedury,(<b>@</b>NázevSouboru)) ;
               <b>Proc</b>( NázevProcedury,(<b>@[</b>NázevSouboru<b>,</b>TextVýraz<b>]</b>)) ;
   
       V příkazu Proc při volání procedury syntakticky odlišíme parametr FILE
       pomocí znaku '<b>@</b>'.

   ■   Aktuální parametr nemusí mít stejnou  deklaraci se souborem , použitým
       jako formální  parametr, ale musí se samozřejmě shodovat všechny prvky
       deklarace  použité  v proceduře ( údaje, odstavce ).  Tato podmínka je
       např. splněna,  je-li aktuální  parametr  souborem  deklarovaným  <a href="#t314">LIKE</a>
       formální parametr.
       Pro substituci názvů vlastních alternativních klíčů platí konvence:
       Je-li NázevKlíče == [Prefix_]ZbytekNázvu, pak pokud soubor, substi-
       tuovaný místo původního, nemá klíč s tímto názvem, PC FAND zkusí
       klíč s názvem  NázevAktuálníhoParametru_ZbytekNázvu

       <b>Pozor:</b> Při použití nevhodného parametru mohou nastat běhové chyby.

   ■   <b>TextVýraz</b> ... ve druhé  variantě znamená  možnost  <a href="#t554">dynamické deklarace</a>
                     souboru - lokálního v dané proceduře.  Nemůže mít indexy
                     a ignorují se odstavce #U/#D/#L/#I. Slouží jako pracovní
                     soubor, může mít zápis v katalogu.
                     Na konci procedury se lokální soubor na disku automaticky 
                     nezruší. To lze v případě potřeby zajistit explicitním
                     příkazem  <b>NázevSouboru.nrecs:=0</b>
                     Při prvním otevření <b>maže</b> případný starý soubor se stejným 
                     jménem - ale jen tehdy, pokud nesouhlasí s deklarací.
                     
   .........................................................................

   <b>Lokální proměnná procedury typu FILE</b> - lokální soubor
   <b>────────────────────────────────────</b>

   ██ <b>VAR</b>  NázevSouboru <b>: file     [</b>DeklaraceSouboru<b>];</b>
           NázevSouboru <b>: file.X   [</b>DeklaraceSouboru<b>];</b>
           NázevSouboru <b>: file.DBF [</b>DeklaraceSouboru<b>];</b>

   Na rozdíl od parametru typu <b>file</b> je deklarace struktury souboru přímou 
   součástí deklarace lokálního souboru.

   ■ <b>NázevSouboru</b> ... Identifikátor, nesmí se překrývat s žádným názvem 
                      kapitoly typu F (na rozdíl od parametru typu file).
   ■ Lokální soubor může mít indexy nebo může být typu DBF.
   ■ <b>DeklaraceSouboru</b> Nesmí obsahovat odstavce #U, #D, #L, #I. Tedy je povo-
                      lena jen první část kapitoly F. Nepíše se jako textový
                      výraz, např. do apostrofů (na rozdíl od parametru-dyna-
                      micky deklarovaného lokálního souboru).



   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██  Povolená kombinace parametrů:
                 ( <b>Soubor</b> : File; veta : RECORD of <b>Soubor</b> )
   
       Umožňuje psaní obecných <b>exit-procedur</b>. Odkaz na soubor musí být vždy 
       za deklarací.

   ██  Obdobně:  ( <b>Soubor</b> : File; indx : INDEX of <b>Soubor</b> )

   ██  P Ukazka  VAR  Pracovni : FILE     [ Jmeno:A,20; Castka:F,3.0 ] ;
                      Export   : FILE.DBF [ Ucet:A,6; Kc:F,6.2 ] ;
                      PracX    : FILE.X   [ Jmeno:A,20; #K @ ~Jmeno ] ;
                 Begin
                 ....

   ──────────────────────────────────────────────────────────────────────────
   <a href="editory.md#t532">exit-procedury</a>
</pre>

<a name="t437"></a>
## owner

<pre>
                                <b>VAZBA OWNER</b>
   ───────────────────────────────────────────────────────────────────────────
   Obdobou  <b>navigace</b>  směrem dolů v datovém  editoru ( <b>Ctrl-F7</b> ) je při výběru
   podmnožiny vět ze souboru vazba owner. Ke známé větě nadřízeného souboru se
   vyberou všechny věty podřízeného  souboru  se stejným  klíčem. Rychlý výběr
   pomocí vazby owner lze použít za předpokladu, že <b>podřízený soubor má indexy</b>
   a jeho <b>vlastní  klíč je</b> rozšířením (tj. <b>je stejný nebo delší</b>) cizího klíče,
   který definuje vazbu do nadřízeného souboru.

   ■ Při výběru podmnožiny pomocí vazby owner rozlišujeme tři varianty podle
     způsobu identifikace věty v nadřízeném souboru.

  ██ <b>OWNER</b> NázevSpojení[ČísVýraz] ......... definice skupiny podřízených vět
  
  ██ <b>OWNER</b> RecordProměnná 
     <b>OWNER</b> PracovníIndex .................. podle prvního spojení mezi soubory

  ██ <b>OWNER</b> NázevSpojení(RecordProměnná) 
     <b>OWNER</b> NázevSpojení(PracovníIndex) .... jednoznačnost při vícenásobné vazbě

   ■ <b>ČísVýraz</b> ........ udává fyzické pořadové číslo nadřízené věty
   ■ <b>NázevSpojení</b> .... <b>NázevSouboru</b>, <b>NázevKlíče</b> nebo <b>NázevRole</b> (viz. <a href="jazyk.md#t346">cizí klíč</a>)
   ■ <b>RecordProměnná</b>... se použije pro určení hodnoty klíče při hledání
                       nadřízené věty.
   ■ <b>Pracovní index</b>... zobrazí se podřízené věty všech vět v indexu, třídění 
                       pracovního indexu musí odpovídat třídění klíče nadříze-
                       ného souboru použitého ve vazbě


   <b>Použití vazby owner v projektu:</b>
   ■ editace souboru: <a href="prostredi.md#t282">edit</a>(...<b>owner</b>...)
   ■ forall cyklus přes věty souboru: <a href="procedury.md#t441">forall</a> ... <b>owner</b>...
   ■ vytvoření pracovního indexy: <a href="procedury.md#t415">getindex</a>(....<b>owner</b>...
   ■ <a href="jazyk.md#t346">Referenční integrita</a> je podmíněna existencí vazby typu owner mezi soubory.
   ■ Speciální funkce <a href="jazyk.md#t438">owned</a> pro přístup z nadřízeného souboru do podřízeného


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   owner NAD[i]                                      {definice nadřízené věty}
   owner Věta                                                {record proměnná}
   owner ALIAS(Věta)                               {alternativní vlastní klíč}
   owner ROLE1[10]                                     {cizí klíč pomocí role}

   edit(DATA,(),owner=Věta)                               {použití v projektu}
   forall Pod owner Nad do ...;

   P Ukazka  VAR ixn : index of Nadrizeny;
                 ixp : index of Podizeny;
             BEGIN
             edit(Nadrizeny,(),sel=ixn);
             if ixn.nrecs&gt;0 then getindex(ixp,Podrizeny,owner=ixn) ;
             ...
             end;

   ───────────────────────────────────────────────────────────────────────────
</pre>

<a name="t554"></a>
## dynamické deklarace

*Též:* `check`

<pre>
                            <b>DYNAMICKÉ DEKLARACE</b>
   ──────────────────────────────────────────────────────────────────────────
   Klasickým přístupem k programování je spouštění předem odladěných úloh, tj.
   jednotlivých  kapitol.  Některé  konstrukce  PC FANDu  však lze  <b>dynamicky</b>
   sestavit až při běhu úlohy. Odpovídající zdrojový kód vložíme do textového
   výrazu, který se použije při vyvolání místo odpovídající kapitoly. 
   V takovém případě však při provedení těchto konstrukcí může dojít 
   k nepředvídatelným běhovým chybám.

   Syntaktická správnost je plně v moci programátora a proto lze tyto možnosti
   doporučit <b>jen zkušeným programátorům</b>.

   <a href="formular-sestava.md#t556">report</a>( ...,<b>[TextovýVýraz]</b>,...)  .......... dynamická deklarace sestavy
   <a href="formular-sestava.md#t566">merge</a>(<b>[TextovýVýraz]</b>) ..................... dynamická deklarace transformace
   <a href="procedury.md#t436">proc</a>(<b>[TextovýVýraz]</b>,...) .................. dynamická deklarace procedury
   proc(...,(<b>@[NázevSouboru,TextVýraz]</b>,..))... dynamická deklarace pracovního
                                               souboru - viz. parametr <a href="#t417">FILE</a>.

   <a href="prostredi.md#t282">edit</a>(...,<b>[TextovýVýraz]</b>...) ....... dynamická deklarace formuláře editace
   <a href="#t350">#U</a> NázevPohledu ...:<b>[TextovýVýraz]</b>.. formulář pro uživatelský pohled
   <a href="procedury.md#t288">exit</a>=(..:<b>[TextovýVýraz]</b>,...:report(<b>[TextovýVýraz]</b>,..))
                                        dynamická deklarace exit-procedury


   ██ <b>CHECK</b> - ověření syntaktické správnosti před spuštěním:
   edit(...,[TextovýVýraz],<b>check</b>,...)  ....... dynamická deklarace sestavy
   report(...,[TextovýVýraz],<b>check</b>,...) ...... dynamická deklarace formuláře

   S parametrem <b>check</b> se příkazy neprovedou ale zkontrolují syntaxi dynamicky
   deklarovaných kapitol E resp. R. Výsledek:
   <a href="procedury.md#t626">exitcode</a> .... = 0 když O.K.
                 &gt; 0 když chyba, = pozice chyby v textu
   <a href="jazyk.md#t550">edreckey</a> .... text chybového hlášení
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t785"></a>
## »F

<pre>
                     <b>SYNTAXE kapitoly F - DEKLARACE SOUBORU</b>            PageDn
 ───── Ctrl-Home ────────────────────────────────────────────────────────────
                                                   <a href="#t313">deklarace souboru</a>
 PopisSouboru ::=
      VlastníPopisSouboru |                                
      like NázevSouboru [VlastníPopisSouboru] |    <a href="#t314">like</a>
      journalof NázevSouboru                       <a href="#t315">journalof</a>
 
 VlastníPopisSouboru ::=                         
      UloženéÚdaje                               
      [{ Klíče | VypočítanéÚdaje }]              
      [ AditivníZměny ]                          
      [ UživatelskéPohledy ]                     
      [ Závislosti ]                             
      [ LogickéKontroly ]                        
      [ ImplicitníHodnoty ]                      
 
 UloženéÚdaje ::=                                   <a href="editory.md#t327">uložené údaje</a>
      { NázevÚdaje : Typ ; }
 
 Typ ::=                                            <a href="editory.md#t327">typy údajů</a>
      F, Číslo1 . Číslo2 |
      F, Číslo1 , Číslo2 |
      R |
      A, Číslo [ R ] [ ! ] |
      A, 'Maska' [ ! ]  |
      N, Číslo [ L ] |
      D, [ Maska ] |
      B |
      T [ , Číslo ] [ ! ]
 
 Klíče ::=                                                   <a href="#t336">klíče</a>
      #K VlastníKlíč ; | KlíčParamSouboru ; | CizíKlíč ;
         [{ CizíKlíč ; | AlternativníVlastníKlíč ; }]
 
 VlastníKlíč ::=                                      <a href="#t338">vlastní klíč</a>
      @ [ &lt;= | * ] KlíčovéÚdaje
 
 AlternativníVlastníKlíč ::=
      NázevKlíče (@) [ &lt;= | * ] KlíčovéÚdaje
 
 KlíčovéÚdaje ::=
      [ &gt; | ~ ] NázevÚdaje [{ , [ &gt; | ~ ] NázevÚdaje }]
 
 CizíKlíč ::=                                    <a href="jazyk.md#t346">cizí klíč</a>
      NázevSouboru [!] SeznamÚdajů |
      NázevKlíče [! | !!] SeznamÚdajů |
      NázevRole ( NázevSouboru ) [! | !!] SeznamÚdajů |
      NázevRole ( NázevKlíče ) [! | !!] SeznamÚdajů
 
 SeznamÚdajů ::=
      NázevÚdaje [{ , NázevÚdaje }]
 
 KlíčParamSouboru ::=                            <a href="jazyk.md#t340">globální proměnné</a>
      @ @
 
 VypočítanéÚdaje ::=                             <a href="#t333">vypočítané údaje</a>
      #C { NázevÚdaje := Výraz : Typ ; }
 
 AditivníZměny ::=                               <a href="#t348">aditivní změny</a>
      #A { ViditelnýÚdaj
         [ (  LogVýraz [ : ChybovéHlášení ] ) ]
         [ ! | !! ] += ČísVýraz ; }
 
 UživatelskéPohledy ::=                          <a href="#t350">uživatelské pohledy</a>
      #U { NázevPohledu
           ( KódUživatele [{ , KódUživatele }] ) :
           [ / NázevKlíče , ] DruhEditace
           ParametryEditace ; }                      
 
 Závislosti ::=                                  <a href="#t356">závislosti</a>
      #D { ( LogVýraz ) { NázevÚdaje := Výraz ; } }
 
 LogickéKontroly ::=                             <a href="#t354">logické kontroly</a>
      #L { LogVýraz [?] [ : ChybovéHlášení ] [ , Heslo ] ; }
 
 ImplicitníHodnoty ::=                           <a href="#t358">implicitní hodnoty</a>
      #I { NázevÚdaje := Výraz ; }

 ───── Ctrl-End ───────────────────────────────────────────────────────────────
</pre>
