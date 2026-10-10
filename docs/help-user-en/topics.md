# Topics

<a name="t19"></a>
## ░ list of users

<pre>
                        <b>EXAMPLE - LIST OF USERS</b>
 ────────────────────────────────────────────────────────────────────────────
   'John Bok',1,'JB',(1,2,3,4,5);                       {chapter U contents}
   'Jim Connors',2,'Jimmy',(1,3);
   'Margaret Black',3,'Megy',(10);
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t48"></a>
## keyboards

<pre>
                                   <b>KEYBOARDS</b>
 ────────────────────────────────────────────────────────────────────────────
 The  key  combination  <b>Alt-F8</b>  calls up a menu for switching among different
 keyboard layouts in data and text editors:

 ■ <a href="context-help.md#t50">standard keyboard</a> ............ without redefinition
 ■ <a href="context-help.md#t52">Czech keyboard</a> ............... like Czech typewriter
 ■ <a href="context-help.md#t54">Czech amateur keyboard</a> ....... changes  only in the top row of keys (lower
                                  case   characters  under  number  keys  and
                                  diacritics)
 ■ <a href="context-help.md#t56">Slovak keyboard</a> .............. like Slovak typewriter
 ■ <a href="context-help.md#t58">German amateur keyboard</a> ...... German keyboard layout
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t203">Installing constants</a>
</pre>

<a name="t189"></a>
## communication

<pre>
                               <b>COMMUNICATION</b>
 ────────────────────────────────────────────────────────────────────────────
 Communication  in  user's or programmer's modes is nearly identical, because
 the  PC FAND's  program is, in fact, special  case of data file. The base of
 programmer's  enviroment is the data editor, in  addition with special means
 for program developing.

 <b>Basic items of communication:</b>

     <a href="#t192">selecting from menu</a> ....................rules of working with menu
     <a href="#t195">PC FAND main menu</a> .......................................main menu
     <a href="#t193">selecting from list</a> ..............rules of selecting from item set
     <a href="#t224">data editor</a> .....................................working with data
     <a href="#t208">text editor</a> .....................................working with text
     <a href="#t235">expressions</a> .........................rules of creating expressions
     <a href="#t48">keyboards</a>............................. national enviroment support
     <a href="#t190">mouse</a> ................................mouse support in the PC FAND
   ───────────────────────────────────────────────────────────────────────────
</pre>

<a name="t190"></a>
## mouse

<pre>
                                  <b>MOUSE</b>
 ────────────────────────────────────────────────────────────────────────────
                                                        <b>┌─────┬─────┬─────┐</b>
        Mouse  support works only for EGA/VGA. It       <b>│</b> cli<b> │     │ </b> E<b>  │</b>
 keeps, in principle, usual conventions.                <b>│</b> ck<b>  │     │ </b> S<b>  │</b>
 Mouse functions correspond to context and cursor       <b>│     │     │ </b> C<b>  │</b>
 position.  You could  exchange function of right       <b>├─────┴─────┴─────┤</b>
 and left buttons.                                      <b>│                 │</b>
 Mouse  function according to context and  cursor       <b>│                 │</b>
 position:                                              <b>│                 │</b>
 ■ <b>menu</b> ....... click a selection  = to select          <b>│                 │</b>
                  click the last row = help             <b>│                 │</b>
                                                        <b>└────────┬────────┘</b>
 ■ <b>text string inquiry</b> ... (prompt,  file name,              <b>┌───┘</b>
   editing item): click the desired field = Enter            <b>└─┐</b>
            
 ■ <b>selecting from list</b>..(file names, sort conditions etc.):
   click name ......... selecting one name = point cursor and Enter
                        selecting set      = cursor movement and switching
                                             mark/unmark

     selecting set, double click outside names : Enter
     keep and drag : item exchange
 ────────────────────────────────────────────────────────────────────────────
     <a href="#t191">mouse in data and text editors</a>         <a href="#t203">installing constants</a>
</pre>

<a name="t191"></a>
## mouse in data and text editors

<pre>
                     <b>MOUSE IN DATA AND TEXT EDITORS</b>
 ────────────────────────────────────────────────────────────────────────────
  ■ <b>data editor</b> ..... click an item : cursor moves to the item
                      double click an item: <b>INS</b> (editing the item)
                      click help (24th or 25th row): like (Ctrl-F1)

  ■ <b>text editor</b> ..... click - cursor movement to the selected position

  ■ <b>help of keys</b> .... standard  or alternate  help of keys can be on the last
                      row of the screen, (alternate = defined by programmer).
                      Click  the highlighted  key name - generates  this key,
                      keeping generates repetition. The control key name is
                      not  always on  the  mouse  icon. Eg. the icon <b>F8</b> gene-
                      rates in <b>Ctrl</b> help combination CtrlF8.
  Keys : ............ F1..F10,  AltF1..AltF10,  ShiftF1...ShiftF10, CtrlF1...
                      ..CtrlF10,  Enter,  Alt,  Shift, Ctrl, ESC, ↑, ↓, PgUp,
                      PgDn,  CtrlHome,  CtrlEnd tabulator icon '\196\16\179',
                      ShiftTab('\179\17\196')
                      On error message, click  'F10' or 'ShiftF7'works simi-
                      larly.

  ■ <b>helps</b> ..... click keyword : like  Enter on keyword, more in 25th row help

 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t192"></a>
## selecting from menu

<pre>
                             <b>SELECTING FROM MENU</b>
 ────────────────────────────────────────────────────────────────────────────
 When  selecting from <b>menu</b>, you start  corresponding commands. Menu can be in
 columns  (selections  one below  another) or in rows  (selections one beside
 another).

 ■ <b>arrows</b> ................ movement through menu selections
 ■ <b>Home</b> (or <b>End</b>) ......... jump to the first (or the last) menu selection
 ■ <b>Enter</b> ................. executes the current command
 ■ highlighted <b>character</b> . directly executes the corresponding selection
 ■ <b>Esc</b> ................... returns one level back in structured menus
                           ( It can be suppressed by programmer)
 ■ <b>F1</b> .................... help of the current  selection  (if the  selection
                           is provided with help on the last row)
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t190">mouse</a>
</pre>

<a name="t193"></a>
## selecting from list

<pre>
                          <b>SELECTING FROM LIST</b>
 ────────────────────────────────────────────────────────────────────────────
 You can select some items from a set  (e.g.: files to edit, items from a fi-
 le to sort). Items of the set are displayed in the  window in the  middle of
 the screen. Mark the selected items gradually with the (<b>F2</b>) and confirm your
 selection by <b>Enter</b>.

 ■ <b>arrows</b> ................. movement among items of the set
 ■ <b>Home</b> (or <b>End</b>) .......... jump to the first (or the last) item of the set
 ■ <b>PageUp</b> (or <b>PageDn</b>) ..... jump on the previous (next) page
                            if the list is scrolling in the window
 ■ <b>F2</b> ..................... selects and marks current item (by mark <b>&lt;</b>)
 ■ <b>F3</b> ..................... deletes marking of current item
 ■ <b>Ctrl-F2</b> ................ marks all items
 ■ <b>Ctrl-F3</b> ................ deletes marking of all items
 ■ <b>&gt;</b> ...................... ( only at  <b>sorting</b> ) selects item for <b>descending</b>
                            sorting; according to items, selected by <b>F2</b> will
                            be sorted in <b>ascending</b> order
                            
 ■ <b>F9</b> ..................... switches on/off item movement in list by <b>arrows</b>
 ■ <b>Enter</b> .................. confirmes selection and starts action
 ■ <b>Esc</b> .................... cancels the whole action

 If you press  <b>Enter</b> without  marking (by <b>F2</b>), the selection will be executed
 by context:
 - Only stored items will be selected  to edit or to printout.
 - Sorting according to control or sort criteria will be omitted.
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t190">mouse</a>
</pre>

<a name="t194"></a>
## help

<pre>
                                     <b>HELP</b>
 ────────────────────────────────────────────────────────────────────────────
 You are  looking  through  <b>PC FAND help</b>.  Programmer  can additionaly create
 <b>specific help</b> of his specific program. PC FAND help consists of many associ-
 ated help screens,  where you can  move through  by highlighted <b>keywords</b>. To
 call up Help, press these keys :

 <b>User's enviroment</b>
 ■ <b>F1</b> ........in data and text editor  -&gt; <b>editor control</b> (PC FAND help)
              in menu -&gt;<b>help of selecting</b> (PC FAND help or program help
                                according to context)
 ■ <b>Ctrl-F1</b> ...in data editor (mode F1, F123)-&gt; <b>current item help</b> (prog-
              ram help)
 ■ <b>Alt-F10</b> ...activates  the last  called help (do not mix up with F10)

 Looking through help, you can use following keys:

 ■ <b>arrows</b> .............. movement through highlighted keywords (topics)
 ■ <b>Enter</b> ............... movement to the other screen, describing high-
                         lighted keyword
 ■ <b>F10</b> ................. return to the previous screen
 ■ <b>F1</b> .................. jump to the main help screen (help index)
 ■ <b>F6</b> .................. printing current help text
 ■ <b>Esc</b> ................. exiting help and return to program
 ■ <b>PageUp</b>, <b>PageDn</b> ...... scrolling
   <b>CtrlHome</b>, <b>CtrlEnd</b> ... linear movement through help

 <b>MOUSE</b>: You can scan help in  similar way as in the text editor.  <b>Click</b> mouse
        (left key) on keyword calls up this keyword. You can call other acti-
        ons from icons on the 25th row.
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t195"></a>
## PC FAND main menu

<pre>
                              <b>PC FAND MAIN MENU</b>
 ────────────────────────────────────────────────────────────────────────────
 Menu contains commands as follows:

 ■ <b>R</b>un ............... starting program in <b>user's mode</b>
 ■ <b>I</b>nstall ........... change of file <b>katalog</b> and list of <b>authorized users</b>
 ■ <b>E</b>dit text ......... starting text editor
 ■ <b>D</b>OS shell ......... jump in operating system (return by command <b>EXIT</b>
 ■ <b>E</b>xit  ............. exiting PC FAND  (or <b>Esc</b>)

 If you start PC FAND with parameters, PC FAND main menu does not appear;
 desired action starts immediately:

 ■ FAND <b>ProgramName</b> ..... starting program in user's mode
 ■ FAND TextName <b>T</b> ... text editor for desired text file
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t233">catalog</a>     <a href="context-help.md#t18">list of users</a>     <a href="#t208">text editor</a>
</pre>

<a name="t196"></a>
## SET-parameters

<pre>
                               <b>SET-parameters</b>
  ───────────────────────────────────────────────────────────────────────────
  Starting a program, the PC FAND interprets these parameters; they are ente-
  red into enviroment variable area by using the command:
                      <b>SET PARAMETER=value</b>
  All the parameters are optional (in user mode);  when not entered, there is
  default setting.

     <b>FANDRES</b> .... FAND.RES file path (messages, fonts, drivers)
     <b>FANDCFG</b> .... FAND.CFG file path (the PC FAND installation parameters)
     <b>FANDOVR</b> .... FAND.OVR  or UFAND*.OVR file paths (overlay);
                  using mainly in LAN
     <b>FANDWORK</b> ... work files FANDWORK.$$$, FANDWORK.X$$, FANDWORK.T$$
                  (Clipboard implementation, temporary sorting, subsets,..)
                  and PRINTER.TXT - records. Using mainly in LAN.
     Default values of the above parameters are PC FAND directory names.

     <b>LANNODE</b> .... work station number in LAN (unique for every station)
                  Values from 0 to 255. It is <b>required</b> for sharing data
                  file!
     <b>FANDATA</b> .... another path for data files and catalog
     <b>FANDCAT</b> .... directory of the <a href="#t233">catalog</a> file (prefered to FANDDATA)
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t304">LAN</a>
</pre>

<a name="t199"></a>
## installation

*Also:* `cache`, `installing system`

<pre>
                               <b>INSTALLATION</b>
 ────────────────────────────────────────────────────────────────────────────
 Installation is setting various parameters, that influence the PC FAND work.
 Before  starting, they are set in. There is, in principle,  possible to dis-
 tinguish two categories: means of the operating system (MS-DOS) and internal
 global variables of the PC FAND.
   <b>■</b>  Starting the PC FAND, the internal global  variables  are  initialized;
      their values are stored in  the <b>FAND.CFG</b> file. They can be set by using
      the <b>FANDINST.EXE</b> <a href="#t200">installation program</a>.

   <b>■</b> Operating system means and its configuration  influence programs in com-
     mon, including:

   <b>-</b> <b>parameter FILES=</b> in the <b>CONFIG.SYS</b> system file must provide enough place
        for simultaneously opened files. Having  insufficent  setting of this
        parameter,  the program  cannot run and ends with  the error  message
        "<b>Too many files open ...</b>". On normal  conditions, it is the  only one
        limiting  parameter of program running. Other parameters are signifi-
        cant when optimizing work (mostly speeding up).

   <b>-</b> MS-DOS enviroment variables, so called <a href="#t196">SET-parameters</a>

   <b>-</b> in <b>EMS memory</b> the (FAND.OVR) overlay module can be located ; if there is
       enough free EMS memory, it will be done automatically. Practical expe-
       riences  prove,  that there is no  influence on work speeding up. More
       advantageous  is to use  free memory  like XMS for cache, or to locate
       resident programs in high memory (see EMM386, loadhigh)

  <b>-</b> <b>XMS memory</b> (the driver HIMEM.SYS in the CONFIG.SYS) is avantageous to use
      for the cache  programs (SMARTDRIVE). Caution - the PC FAND has its own
      cache, that occupies 200kB of the XMS-memory (see <a href="#t203">installing constants</a>).
      <b>!!!</b>  The FAND-cache is <b>not</b>  recommend to combine with another one (usu-
           ally the SMARTDRIVE).
 ────────────────────────────────────────────────────────────────────────────
  <a href="#t304">LAN</a>      <a href="#t233">catalog</a>
</pre>

<a name="t200"></a>
## installation program

<pre>
                     <b>FANDINST INSTALLATION PROGRAM</b>
 ────────────────────────────────────────────────────────────────────────────
 The <b>FANDINST.EXE</b> program fills <b>FAND.CFG</b> - the file of  installation  parame-
 ters. Upon the PC FAND start, they are used to fill  global  variables. Ins-
 tallation consists of blocks as follows:

 The program  processes the FAND.CFG file by the FANDCFG SET parameter. If the
 parametr is missing, it is searched in the current directory.

 ■ <b>Colors</b> ....... colors for data and text editors, help, menus etc.
 ■ <b>Printer</b> ...... codes for switching fonts on/off
                  and the other escape sequences for printer; there is
                  possible to install up to 10 printers
 ■ <b>Constants</b> .... values of all types, influencing the the PC FAND work
 ■ <b>Monitor</b> ...... the video memory address, cursor shape, snow checking
 ■ <b>Alphabet</b> ..... national alphabet selection for lexical sorting
                  prepared <b>Kamenicky</b> and <b>Latin2</b> code tables.
 ■ <b>Days</b> ......... extraordinary days table (public holidays..)
                  for built-in calendar
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t201">installing colors</a>     <a href="#t202">installing printer</a>      <a href="#t203">installing constants</a>
 <a href="#t205">installing alphabet</a>   <a href="#t206">installing calendar</a>     <a href="#t204">installing monitor</a>
</pre>

<a name="t201"></a>
## installing colors

<pre>
                        <b>INSTALLING COLORS</b>                         PgDn
 ────────────────────────────────────────────────────────────────────────────
 Every color is entered by hexadecimal number according to help.

 ■ <b>MENU normal text</b> ......... non-active selections from menu
 ■ <b>     active item</b> ......... highlighted field that cursor points to
 ■ <b>     first character</b> ..... highlighted character for quick selection

 ■ <b>SELECTING normal text</b> .... item selection from list
 ■ <b>          active item</b> .... highlighted item, that cursor points to
 ■ <b>          file mask</b> ...... mask for file selection from disk

 ■ <b>ENTERING text</b> ............ prompt on the last row
 ■ <b>       help</b> .............. explaining text to prompt
 ■ <b>MESSAGE on last row</b> ...... F10 message
 ■ <b>LAST ROW help</b> ............ help in data and text editor
 ■ <b>         key icons</b> ....... special key markings in help
 ■ <b>         switches</b> ........ switching  off  additive changes  and logical
                                check
 ■ <b>FIRST ROW system. information</b> ... first row of user's screen

 ■ <b>TEXT EDITOR  text</b> .........edited text
 ■ <b>          ctrl-characters</b> .control characters (except color switches)
 ■ <b>          block</b> ...........block marking
 ■ <b>FONTS - underlined text</b> ...Ctrl-S
 ■ <b>      italic</b> ..............Ctrl-W
 ■ <b>      double-width</b> ........Ctrl-Q
 ■ <b>      double-strike</b> .......Ctrl-D
 ■ <b>      emphasized</b> ..........Ctrl-B
 ■ <b>      compressed</b> ..........Ctrl-E
 ■ <b>      elite</b> ...............Ctrl-A

 ■ <b>DATA EDITOR data</b> ..........edited data
 ■ <b>            data cursor</b> ...highlighted item, that cursor points to
 ■ <b>            subset</b> ........selected subset (F6-action, F5-switches)
 ■ <b>            other text</b> ....item names etc.
 ■ <b>USER SCREEN</b> ...............basic attribute for "write" etc.

 ■ <b>HELP text</b> .................common help text
 ■ <b>     selected keyword</b> .....highlighted name, what cursor points to
 ■ <b>     other keywords</b> .......other keywords,  what cursor can be pointed to
 ■ <b>     higlighted text</b> ......titles etc.
 ■ <b>SHORT HELP</b> ................one-row help on the last (but one) row

 ■ <b>WINDOWS shadow</b> ............window shadows (menu, with window, ww)
 ■ <b>DESKTOP</b> ..................."desk" color (the PC FAND main menu)
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t222">colors and fonts</a>                                                   PageUp
</pre>

<a name="t202"></a>
## installing printer

<pre>
                            <b>INSTALLING PRINTER</b>
 ────────────────────────────────────────────────────────────────────────────
 Standard setup can be changed by <b>printer control commands</b> according to prin-
 ter manual (use hexadecimal, for example ESC=1B).  Most Epson and IBM compa-
 tible printers suit settings, installed by producer. For <b>fonts</b> there are set
 switch on/off codes. Command length is max. 128 characters.

 ■ <b>PRINTER</b> (name) ......... is used to call selection menu of printer. In the 
                            PC FAND  (<b>Alt-F6</b> key), is allowed to  install up to
                            10 printers
 ■ <b>Type</b> ................... printer type (<b>M</b>-mosaic, <b>C</b>-color mosaic, <b>L</b>-laser,
                            HP LaserjetII)
                            It is used when printing graph. Upon printing text,
                            it is used only for encoding of page length.
 ■ <b>code(KL,KN,LK,LN)</b> .... code change for printer output, two-character
                          constant:
                            <b>K</b> ......code Kamenicky
                            <b>L</b> ......Latin2
                            <b>N</b> ......IBM standard
                            Eg <b>KL</b> constant means reencoding from Kamenicky to
                            to Latin2.  For  keyboard  and  monitor, there is
                            possible  to use  Kamenicky diacritic and outputs
                            to printer to reencode to Latin 2.
                            
 ■ <b>Printer port number (1 to 3)</b> .... 1/2/3 (1), output to LPT1/LPT2/LPT3
 ■ <b>Waiting on printer response</b> ..... seconds (7), communication PC - printer
 ■ <b>(^S)</b> Underline              ■ <b>(^W)</b> Italic
 ■ <b>(^Q)</b> Double-width           ■ <b>(^D)</b> Double-strike
 ■ <b>(^B)</b> Emphasised             ■ <b>(^E)</b> Compressed
 ■ <b>(^A)</b> Elite
 ■ <b>Printer reset</b> ........ Clears  the print  buffer and returns settings to
                          their initial state.It it is sent to printer upon
                          every  print  beginning.  Delete  it, if  you use
                          <b>download</b> with  a national alfabet.
                       
 ■ <b>Page size</b> .............setting only the command prefix, the  PC FAND  adds
                          number of lines automatically (for TYPE='L' in cha-
                          racters, otherwise in binary form).
 ■ <b>End.string/LASER</b> ......ending control sequence, following page  lenght. It
                          is used for laser printers
 ■ <b>Left margin</b> ...........it is used  for implementation  of dot command <b>.PO</b>.
                    ..    Default setting is 1B6C.
 ■ <b>(^X) User code 1</b> ..... further character fonts or  using  special  printer
                          features

 ■ <b>(^V) User code 2</b>       (setting line spacing, colors, NLQ..)
 ■ <b>(^T) User code 3</b>
 <b>graph print parameters</b>
 ■ <b>Vertical spacing n/72</b> .. if possible      ───┐
   <b>Vertical spacing n/216</b>                       │  print proportions
 ■ <b>Gr.density 60</b> .......... graphic density     ├  can be changed
   <b>Gr.density 120</b>                               │  <a href="#t302">printing graph</a>
   <b>Gr.density 240</b>                            ───┘
 ■ <b>Color setting</b> ......... prefix only, the PC FAND adds number of color
  ─────────────────────────────────────────────────────────────────────────
 Ctrl-End       <a href="#t222">colors and fonts</a>       <a href="#t220">printing text</a>       <a href="#t301">graphs</a>
</pre>

<a name="t203"></a>
## installing constants

<pre>
 Ctrl-Home                   <b>INSTALLING CONSTANTS</b>
 ────────────────────────────────────────────────────────────────────────────
 constant                            unit(def.value)     meaning

 ■ <b>Record number to save in editor</b>.... number(20)  interval  of  <a href="#t199">cache</a> unloa-
                                                   ding  onto  disc;  smaller
                                                   number -&gt; data safety
 ■ <b>Automatic record - width</b> .......... columns(80) valid  for  every report
 ■ <b>                  - size</b> .......... rows (69)   without its own paging
 ■ <b>           - page bottom</b> .......... rows (3)    default value
 ■ <b>- output directly to printer?</b> ..... Y/N (N)     (N).. outputting text into
                                                   disc file
 ■ <b>Graphic page dividing?</b> .............Y/N (N)     page separator when is
                                                   <b>ScrollLock</b> switched on
 ■ <b>Graphic page dividing character</b> ... '·'         char(250)
 ■ <b>Confirm ESC to exit editor?</b>........ Y/N (N)     question on editor exit
 ■ <b>RDB annotation for developing ?</b> ... Y/N (Y)     display the first text row
 ■ <b>CP/M drive (Eg. D,E,F...)</b> ......... drive (' ') compatibil. with CP/M FAND
 ■ <b>LAN - screen refresh</b> .............. seconds (5  screen refresh - <a href="#t304">LAN</a>
 ■ <b>    - locked access repeating</b> ..... CPU (54)    <b>net delay</b>
 ■ <b>    - beeping at repeated locking</b> . Y/N (Y)     if disturbs,can be switch-
                                                   ed off
 ■ <b>Beep on errors?</b> ................... Y/N (Y)     if disturbs,can be switch-
                                                   ed off
 ■ <b>Max. XMS size for PC FAND</b> ......... kB (256)    extended memory for <a href="#t199">cache</a>
 ■ <b>Eliminate Ctrl-Break?</b> ............. Y/N (N)     program exit by Ctrl-Break
 ■ <b>Default keyboard number</b> ........... 0 to 4 (0)  order according to menu
                                                   (Alt-F8)
 ■ <b>Mouse - support switched off</b> ...... Y/N (N)     possibility to switch mou-
                                                   se support off
 ■ <b>           - button exchange</b> ...... Y/N (N)     exchange  of  the left and
                                                   right buttons
 ■ <b>     - double click interval</b> ...... CPU units(8)interval  of double  click
 ■ <b>        - repeating interval</b> ...... CPU units(8) pressing mouse button
                                                    automatically  repeating
  ■ <b>Ctrl, Alt, Shift delays</b> .......... CPU units(15)calling alternative  help
  ■ <b>Automat. ovewrite diskette label</b> . Y/N (Y)      for backup
  ■ <b>Shutting down the screen timeout</b> . sec (0)      when = 0, the screen does
                                                    is not shut down
  ■ <b>Default century change</b> ........... Years(0)     When entering data, mask
                                                    for year ..YY; turn of the
                                                    cetury
 ────────────────────────────────────────────────────────────────────────────
 Ctrl-End    <a href="#t228">automatic report</a>       <a href="#t233">catalog</a>      <a href="#t48">keyboards</a>
</pre>

<a name="t204"></a>
## installing monitor

<pre>
                              <b>INSTALLING MONITOR</b>
 ────────────────────────────────────────────────────────────────────────────
 1st block - system constants, which are necessary to change only in specific
   cases  at  non-standard  PC  configurations.  Default values are installed
   according to the monitor type.

 ■ <b>Video memory address</b> ........ default setting for Hercules / VGA etc.
 ■ <b>Number of monitor rows</b> ..... setting when system returns incorrect value.
                                If value=0 (default), PC FAND uses the value,
                                returned from system.
 ■ <b>Snow checking?</b> ............. snow  checking  can slow  down  displaying on
                                cheeper monitors
 ■ <b>Cursor switched on</b> ......... cursor shapes (top and bottom line of cursor)
 ■ <b>Cursor switched off</b>
 ■ <b>Large cursor</b>

 2nd block - Czech language support on monitor

 ■ <b>Use PC FAND fonts ?</b> .........on EGA/VGA - the PC FAND  uses  automatically
                                diacritic  font  according to installed  code
                                table
 ■ <b>Monitor without diacritics?</b>. diacritics  are  removed  before writing onto
                                monitor
 ────────────────────────────────────────────────────────────────────────────
 Ctrl-End             <a href="#t205">installing alphabet</a>
</pre>

<a name="t205"></a>
## installing alphabet

<pre>
                          <b>INSTALLING ALPHABET</b>
 ────────────────────────────────────────────────────────────────────────────
 The PC FAND contains keyboard driver and screen fonts. Printer fonts are not
 regarded. When   installing  alphabet, the code table for <b>lexical sorting</b> is
 selected from the following possibilities :

 ■ <b>IBM</b> ........... national alphabet not installed
 ■ <b>Kamenicky</b>
 ■ <b>Latin2</b> ........ two variants of encoding Czech and Slovak languages

 Output to printer can be recoded - see <a href="#t202">installing printer</a>

 The PC FAND supports functions according to the installed code table:
 
 ■ <a href="#t287">upcase</a> .............. string conversion to upper case
 ■ <a href="#t287">lowcase</a> ............. string conversion to lower case
 ■ <a href="#t287">nodiakr</a> ............. diacritical marks removed
 ■ <a href="#t250">lexical sorting</a> ..... sorting regards diacritics
 ────────────────────────────────────────────────────────────────────────────
 Ctrl-End  <a href="#t204">installing monitor</a>
</pre>

<a name="t206"></a>
## installing calendar

<pre>
                              <b>INSTALLING CALENDAR</b>
 ────────────────────────────────────────────────────────────────────────────
 When counting with weekdays ( function <b>typeday</b>, <b>addwdays</b>, <b>difwdays</b> ) in data
 arithmetic,  the table of  extraordinary days  (public holidays,..) is used.
 The function  <b>typeday</b> is calculated  according to weekday, if the table does
 not define otherwise.

 Installation consists of the following two steps:
 Before starting the installation program, call the PC FAND program <b>INST.RDB</b>,
 where you will edit the  <b>DAY.000</b> ( extraordinary day file ). You will  enter
 always  two items,  date and a new type of the day (0/1/2/3 according to the
 desired new calculation of the function Typeday).
 In the second step you will select <b>Days</b> in the installation program <b>FANDINST</b>.
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t272">date and time functions</a>
</pre>

<a name="t208"></a>
## text editor

<pre>
                                 <b>TEXT EDITOR</b>
 ────────────────────────────────────────────────────────────────────────────
 The <b>text editor</b> is used when looking through <b>reports</b> and editing data items
 of type T (memo texts). Eg. most of the PC FAND  programmer's enviroment is
 nearly identical to the text editor enviroment.
 
 <b>Differences</b> of the PC FAND text editor in comparison to standard practice:

 ■ <b>edit/view</b> modes (switched over by pressing the <b>Scroll-Lock</b> key)
 ■ Exiting  editor by ESC key, edited text is  <b>automatically</b> saved  onto disc
   without question. (Using the text editor for non-text files is not  recom-
   mended, because the editor adds into unformatted files line separators.)
 ■ Writing block into free memory (clipboard) by pressing the CtrlF7 (ShiftF7)
   keys.
 <b>Some text editor features</b>:
 Size  of edited  file is not  directly limited. Line numbering is limited by
 the value 99,999. Whole edited  file is not  located in memory, but only its
 part, which is  dynamically ( and unvisible to operator ) read from disc and
 saved back onto it. Using the technique of edit-interruption, it is possible
 to add specific functions to the text editor. (Programmer defines.)
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t209">cursor movements</a>    <a href="#t210">editing text</a>    <a href="#t214">blocks</a>           <a href="#t216">switches</a>
 <a href="#t217">formatting text</a>     <a href="#t218">searching</a>       <a href="#t220">printing text</a>    <a href="#t222">colors and fonts</a>
 <a href="#t212">editor facilities</a>             <a href="#t191">mouse in data and text editors</a>
</pre>

<a name="t209"></a>
## cursor movements

<pre>
                        <b>CURSOR MOVEMENT IN TEXT EDITOR</b>
 ────────────────────────────────────────────────────────────────────────────
                            
 ■ <b>left arrow</b> ........... moves cursor to the previous character
 ■ <b>right arrow</b> .......... moves cursor to the next character
 ■ <b>up arrow</b> ............. moves cursor to the previous line
 ■ <b>down arrow</b> ........... moves cursor to the next line
 ■ <b>Ctrl-left arrow</b> ...... moves cursor to the preceding word
 ■ <b>Ctrl-right arrow</b> ..... moves cursor to the next word
 ■ <b>Home</b> ................. moves cursor to the beginning of the line
 ■ <b>End</b> .................. moves cursor to the end of the line

 ■ <b>PageUp</b> ............... moves cursor to the previous page
 ■ <b>PageDown</b> ............. moves cursor to the next page
 ■ <b>Ctrl-PageUp</b> .......... moves cursor to the beginning of the text
 ■ <b>Ctrl-PageDown</b> ........ moves cursor to the end of the text
 ■ <b>Ctrl</b>-<b>F3</b> .............. moves cursor to the defined line
 ■ <b>Ctrl</b>-<b>Q</b> <b>Ctrl</b>-<b>E</b> ........ moves cursor to the window first line
 ■ <b>Ctrl</b>-<b>Q</b> <b>Ctrl</b>-<b>X</b> ........ moves cursor to the window last line

 ■ <b>Ctrl-Home</b> ............ moves cursor to the previous section of the memo
                          (T - type) text
 ■ <b>Ctrl-End</b> ............. moves  cursor to the next section of the memo
                          (T - type) text
 ■ <b>Ctrl-W</b> ............... scrolls  the screen  a line  down  (does not change
                          cursor position)
 ■ <b>Ctrl-Z</b> ............... scrolls the screen a line up
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t210">editing text</a>            <a href="#t191">mouse in data and text editors</a>
</pre>

<a name="t210"></a>
## editing text

<pre>
                                 <b>EDITING TEXT</b>
 ────────────────────────────────────────────────────────────────────────────
 ■ <b>Enter</b> ............... terminates the line  (or paragraph if formatting is
                         switched on)
 ■ <b>Esc</b> ................. editing exit and saving onto disk
 ■ <b>Alt</b>-<b>=</b> ............... editing  exit without saving onto disc (not for disk
                         files)

 ■ <b>Del</b> ................. deleting the character above the cursor
 ■ <b>BackSpace &lt;-</b> ........ deleting the character left from the cursor
 ■ <b>Ctrl</b>-<b>T</b> .............. deleting whole word
 ■ <b>Ctrl</b>-<b>Y</b> ...............deleting line
 ■ <b>Ctrl</b>-<b>Q</b> <b>Ctrl</b>-<b>Y</b> ....... deleting to the end of the line

 ■ <b>Ctrl</b>-<b>U</b> .............. deleting changes  and  recovering text (not for disc
                         file)
 ■ <b>Ctrl</b>-<b>Q</b> <b>Ctrl</b>-<b>L</b> ....... recovering line (only current line)
 ■ <b>F9</b> ...................writing  changes  onto  disk  ( prevention  of power
                         failure)

 ■ <b>Ctrl</b>-<b>N</b> ...............inserting the new line above the cursor
 ■ <b>Ctrl</b>-<b>I</b> .............. tabulator
 ■ <b>Ctrl</b>-<b>J</b> .............. right alignment
 ■ <b>Ctrl</b>-<b>O</b> <b>Ctrl</b>-<b>C</b> ........center line alignment
 ■ <b>Ctrl</b>-<b>P</b> <b>Ctrl</b>-<b>Char</b> .... entering control character
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t209">cursor movements</a>
</pre>

<a name="t212"></a>
## editor facilities

*Also:* `boxes`

<pre>
                          <b>EDITOR FACILITIES</b>
 ────────────────────────────────────────────────────────────────────────────
                      drawing lines and boxes

 ■ <b>Ctrl-Q Ctrl--</b> ... single line                                      ┌──┐
 ■ <b>Ctrl-Q Ctrl-=</b> ... double line                                      │ ╔╪═╗
 ■ <b>Ctrl-Q Ctrl-/</b> ... erases line in the direction of cursor movement  └─╫┘ ║
 ■ <b>arrows</b> .......... drawing lines                                      ╚══╝
 ■ <b>Esc</b> ............. end of drawing

   When drawing box you can use :
 ■ <b>-</b> ............... switch to single line
 ■ <b>=</b> ............... switch to double line
 ■ <b>\</b> ............... switch to erasing
 ■ <b>' '</b> ............. space - switch to cursor movement without drawing
                     or erasing

 ■ <b>F4</b> .............. switching on/off diacritics of character above cursor
 ■ <b>Alt</b>-<b>F8</b> .......... calls menu to switching over the keyboard layout
 ■ <b>Ctrl</b>-<b>F5</b> ......... calculator
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t48">keyboards</a>
</pre>

<a name="t214"></a>
## blocks

*Also:* `clipboard`

<pre>
                            <b>BLOCKS IN TEXT EDITOR</b>
 ────────────────────────────────────────────────────────────────────────────
 A <b>Block</b> is a part of a text. Before working with block, mark it (the marked 
 text has different color). After that you can use block operations. The <b>Clip-
 board</b> is an auxiliary text stack for block transfer.

 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>N</b> ............. switching normal/<a href="#t215">column block</a> modes

 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>B</b> or <b>F7</b> ........marking the beginning of the block
 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>K</b> or <b>F8</b> ........marking the end of the block
 ■ <b>Shift</b>-<b>PgDn</b>,<b>Up</b>,<b>Left</b>,<b>Right</b> ...block definition by pulling block
                               border
 ■ <b>Shift</b>-<b>PgDn</b>,<b>PgUp</b> ........... pulling the block border  one page up (down)

 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>C</b> ............. copying the block (max. 28kB) to the cursor
                               position                                  <b>c</b>opy
 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>V</b> ............. the block transfer(max. 28kB) to the cursor
                               position                                  mo<b>v</b>e
 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>Y</b> ............. deleting the block

 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>R</b> ............. reading the block from a disk (text file)  <b>r</b>ead
                                                                          
 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>W</b> ............. writing the block onto a disk             <b>w</b>rite
 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>U</b> ............. converts lowercase char. into <b>u</b>ppercase
 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>L</b> ............. converts uppercase char. into <b>l</b>owercase

 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>F</b> ............. formatting the block                     <b>f</b>ormat
 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>P</b> ............. printing the block                        <b>p</b>rint

 ■ <b>Ctrl</b>-<b>F7</b> ................... copying the block from text to the Clipboard
 ■ <b>Shift</b>-<b>F7</b> .................. copying from the Clipboard to a text (the block
                               transfer)
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t217">formatting text</a>     <a href="#t220">printing text</a>
</pre>

<a name="t215"></a>
## column block

<pre>
                               <b>COLUMN BLOCK</b>
 ────────────────────────────────────────────────────────────────────────────
  A usual block is a part of a continuous text,defined by its plain beginning
  and end. The  <b>COLUMN BLOCK</b>  is additionally  defined  by the  beginning and
  ending of text columns as well.  From the internal  point of view, it is no
  continuous text part. Visually, it is a rectangle on the screen.

  Working  with column  blocks is similar  to working with usual blocks. Mode
  (column or usual block) is set by the <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>N</b> switch.

  The functions,  which are not defined for column blocks: The  <b>Ctrl</b>-<b>Q</b> <b>Ctrl</b>-<b>F</b>
  search function,  the  <b>Ctrl</b>-<b>Q</b> <b>Ctrl</b>-<b>A</b>  replacing function under condition <b>l</b>,
  and the <b>Ctrl</b>-<b>F</b> block formatting function.
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t214">blocks</a>
</pre>

<a name="t216"></a>
## switches

<pre>
                       <b>TEXT EDITOR SWITCHES</b>
 ────────────────────────────────────────────────────────────────────────────
 The current values of the <b>switches</b> are displayed in abbreviated  form in the
 first row of the screen.  Switching on/off by repeated  pressing of the cor-
 responding key.
 
 switch                                       command       abbreviation
 --------------------------------------------------------------on---off
 when typing text, <b>insert or overwrite</b>          <b>Ins</b>            Ins Ovrwr
 <b>indent</b> next row like previous one          <b>Ctrl</b>-<b>Q</b> <b>Ctrl</b>-<b>I</b>      Ind
 <b>formatting</b> line end automatically          <b>Ctrl</b>-<b>O</b> <b>Ctrl</b>-<b>W</b>      Form
 right <b>alignment</b>                            <b>Ctrl</b>-<b>O</b> <b>Ctrl</b>-<b>J</b>      Align
 <b>Column block</b>/usual block                   <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>N</b>      Colum
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t217">formatting text</a>
</pre>

<a name="t217"></a>
## formatting text

<pre>
                               <b>FORMATTING TEXT</b>
 ────────────────────────────────────────────────────────────────────────────
 In a formatted text, the PC FAND recognizes the  <b>soft return</b>  and the  <b>para-
 graph mark</b>. A <b>paragraph</b> is defined as a text part, ending with the paragraph
 mark (when editing use <b>Enter</b>). Lines, ending with the paragraph mark are mar-
 ked by <b>&lt;</b> in the last column of the screen (when  autoformat is switched on).
 Paragraphs can be formatted into a column between the <b>left and right margin</b>s.

 ■ <b>Ctrl</b>-<b>O</b> <b>Ctrl</b>-<b>L</b> ... setting left margin  (default <b>1</b>)
 ■ <b>Ctrl</b>-<b>O</b> <b>Ctrl</b>-<b>R</b> ... setting right margin (default <b>78</b>)

 ■ <b>Ctrl</b>-<b>B</b> .......... formatting  text from the cursor  position to the end of
                    the paragraph
 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>F</b> ... formatting the whole block

 If the <b>autoformat</b> (<b>Ctrl-O Ctrl-W</b>) is switched on, the text is formatted yet
 after entering of the whole line. If additionally the <b>alignment</b> is switched
 on, (<b>Ctrl-O</b> <b>Ctrl-J</b>) text will be alligned to the right margin (spaces will
 be inserted into the line).

 The internal separators within text: <b>^M^J</b> - paragraph mark, <b>^M</b> - soft return
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t216">switches</a>     <a href="#t214">blocks</a>
</pre>

<a name="t218"></a>
## searching

<pre>
                              <b>SEARCHING IN TEXT</b>
 ────────────────────────────────────────────────────────────────────────────
 ■ <b>Ctrl</b>-<b>Q</b> <b>Ctrl</b>-<b>F</b> ... searching a string in a text
 ■ <b>Ctrl</b>-<b>Q</b> <b>Ctrl</b>-<b>A</b> ... searching and replacing the string
 ■ <b>Ctrl-L</b> .......... searching/replacing the same string repeatedly

 To search/replace type gradually on the last row:

 ■ <b>Search</b> .......... searched string
 ■ <b>Replace</b> ......... the string, which will replace the original text
 ■ <b>Conditions</b> ...... for searching: one or more characters according to the
                     following table
 ■ <b>g</b> ............... searching from the  beginning (default: searching from
                     cursor position to the end)
 ■ <b>e</b> ............... <b>global</b> searching (only free texts,in all file records)
 ■ <b>n</b> ............... autoreplacing  <b>without question</b> Yes/No to comfirm
 ■ <b>w</b> ............... searching only whole <b>words</b>, not parts
 ■ <b>~</b> ............... <b>lexical</b> comparison  (for example baba=BÁBA)
 ■ <b>u</b> ............... comparison <b>without differentiation uppercase/lowercase</b>
                     (example: Prague=PRAGUE)
 ■ <b>l</b> ............... searching/replacing only within the defined block

 Default  conditions  of  searching: only  once, from cursor position, with
 question whether to replace, not leave out compounds, with differentiation
 between uppercase/lowercase characters and with diacritics.
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t250">lexical sorting</a>
</pre>

<a name="t220"></a>
## printing text

*Also:* `dot commands`

<pre>
                               <b>PRINTING TEXT</b>
 ────────────────────────────────────────────────────────────────────────────
 ■ <b>F6</b> .............. printing  the whole text
 ■ <b>Ctrl</b>-<b>F6</b> ......... printing from the cursor position to the end of the text
 ■ <b>Ctrl</b>-<b>K</b> <b>Ctrl</b>-<b>P</b> ... printing the block  (or whole text, if no block defined)
 ■ <b>Alt</b>-<b>F6</b> .......... intractive change of a printer.

 To  cancel printing  press <b>Esc</b>. The following <b>Dot commands</b> control printing,
 if they are placed on the first lines (only one command per line; every com-
 mand begins in the first column).The dot commands are not printed.

 ■ <b>.ti n</b> ...... number of copies ( default <b>1</b> )
 ■ <b>.cp n</b> ...... automatic paging (number of rows, left out at bottom of page)
 ■ <b>.pl n</b> ...... the real lenght of printer's page (default <b>72</b> rows)
 ■ <b>.po n</b> ...... the left margin (default <b>0</b>)
 ■ <b>.ff</b> ........ eliminates message "set printer" at the beginning and form
                feed at the end
 ■ <b>.nm</b> ........ like <b>.ff</b>, but ending with form feed
 ■ <b>.he text</b> ... the page heading (<b>mask</b> in text ....... <b>___</b> - page number
 ■ <b>.fo text</b> ... the page footing                       <b>__.__.__</b> - date
                                                       <b>__:__</b> - time)
 <b>n</b> is any number, the <b>text</b> is one-row string
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t222">colors and fonts</a>     <a href="#t214">blocks</a>     <a href="#t202">installing printer</a>
</pre>

<a name="t221"></a>
## colors

<pre>
                                   <b>BARVY</b>
  ────────────────────────────────────────────────────────────────────────────
  ╔═══════════════════════════════════════════════════════════════╦══════════╗
  ║ <b>ink</b>   black  blue   green  cyan   red   magenta  brown  grey  ║  <b>paper</b>   ║
  ╠═══════╦═══════════════════════════════════════════════════════╣          ║
  ║ <b>int</b>  L║  ***    001    002    003    004    005    006    007 ║  black   ║
  ║      <b>H</b>║<b>  008    009  <i>^s</i>010  <i>^w</i>011  <i>^d</i>012  <i>^b</i>013  <i>^a</i>014    015</b> ║          ║
  ║      L║  016    017    018    019    020    021    022    023 ║  blue    ║
  ║      <b>H</b>║<b>  024    025    026    027    028    029  <i>^q</i>030    031</b> ║          ║
  ║      L║  032    033    034    035    036    037    038    039 ║  green   ║
  ║      <b>H</b>║<b>  040    041    042    043    044    045    046    047</b> ║          ║
  ║      L║  048    049    050    051    052    053    054    055 ║  cyan    ║
  ║      <b>H</b>║<b>  056    057    058    059    060    061    062    063</b> ║          ║
  ║      L║  064    065    066    067    068    069    070    071 ║  red     ║
  ║      <b>H</b>║<b>  072    073    074    075    076    077  <i>^e</i>078    079</b> ║          ║
  ║      L║  080    081    082    083    084    085    086    087 ║  magenta ║
  ║      <b>H</b>║<b>  088    089    090    091    092    093    094    095</b> ║          ║
  ║      L║  096    097    098    099    100    101    102    103 ║  brown   ║
  ║      <b>H</b>║<b>  104    105    106    107    108    109    110    111</b> ║          ║
  ║      L║  112    113    114    115    116    117    118    119 ║  grey    ║
  ║      <b>H</b>║<b>  120    121    122    123    124    125    126    127</b> ║          ║
  ╚═══════╩═══════════════════════════════════════════════════════╩══════════╝
  <b>blinking</b> = value + 128
  <a href="#t222">Colors and fonts</a>
</pre>

<a name="t222"></a>
## colors and fonts

<pre>
                               <b>COLORS AND FONTS</b>
 ────────────────────────────────────────────────────────────────────────────
 Colors on the screen correspond with printer fonts. Insert in the text color/
 /font switch:  Ctrl-Character,  press <b>Ctrl-P Ctrl-Character</b> in edit mode. In
 scroll  mode  (<b>Scroll-Lock</b>)  control  characters disappear and text is colo-
 red.The first occurence of this character switches color/font on, the second
 switches off etc.

  ■ Ctrl-<b>S</b> ... <b>underline</b>... on color monitors <b>green</b>
  ■ Ctrl-<b>W</b> ... <b>italic</b>...... <b>light blue</b>
  ■ Ctrl-<b>B</b> ... <b>bold</b>.......  <b>magenta</b>
  ■ Ctrl-<b>D</b> ... <b>double</b>...... <b>red</b>

  ■ Ctrl-<b>Q</b> ... <b>wide</b> ..... <b>yellow on the blue background</b>
  ■ Ctrl-<b>E</b> ... <b>compressed</b> <b>yellow on the red background</b>
  ■ Ctrl-<b>A</b> ... <b>elite</b>..... <b>yellow</b>
                                                              original setting
  ■ Ctrl-<b>X</b> ... <b>fonts defined</b> by user                        doubled characters
  ■ Ctrl-<b>V</b> ... (without colors)                          quadrupled characters
  ■ Ctrl-<b>T</b> ...                                          condensed line spacing

 Ctrl-Characters (color switch on/off) are regarded when formatting text.
 All colors and control codes can be reinstalled by program <b>FANDINST</b>.
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t220">printing text</a>     <a href="#t201">installing colors</a>     <a href="#t202">installing printer</a>
</pre>

<a name="t224"></a>
## data editor

<pre>
                                 <b>DATA EDITOR</b>
 ────────────────────────────────────────────────────────────────────────────
   movement inside of file             movement inside of item
   -------------------------           -------------------------
 ■ <b>&lt;-</b> .... to the previous item        ■ <b>Ins</b>,<b>CtrlV</b> ... insert/overwrite
           of the record
 ■ <b>-&gt;</b> .... to the next item            ■ <b>&lt;-</b> ..... to the left character
 ■ <b>Enter</b>.. to the next editable item   ■ <b>-&gt;</b> ..... to the right character
 ■ <b>Home</b> .. to the first item in record ■ <b>Home</b> ... to the beginning item
 ■ <b>End</b> ... to the last item in record  ■ <b>End</b> .... to the ending item
 ■ <b>PageUp</b> ...... to the previous page  ■ <b>Del</b> .... deleting the character
                                                  above the cursor
 ■ <b>PageDown</b> .... to the next page      ■ <b>BackSpace &lt;-</b> .. deleting the cha-
                                                  racter left of the cursor
 ■ <b>Ctrl-Home</b> ... to the previous rec.  ■ <b>Enter</b> .. end of editing item
 ■ <b>Ctrl-End</b> .... to the next record    ■ <b>Esc</b> .... return back without
                                                  saving  changes
 ■ <b>Ctrl-PageUp</b> .. to the beginning     ■ <b>F4</b> ..... diacritics for the cha-
                         of the file              racter
 ■ <b>Ctrl-PageDown</b> ... to the end of     ■ <b>Alt-F8</b> . keyboard switch on/off
                           the file
 Editing  <b>free (memo) texts</b> .... <b>Ins</b> - entry, <b>Esc</b> - return.  When editing the
 text itself, you are using the <b>text editor</b>. The <b>*</b> character means item, con-
 taining  text and the  <b>.</b> character -  empty memo (free) text. The <b>ESC</b>  exits
 data  editor. The help describes usual control of the data editor. Some user
 commands can be eliminated by programmer.

 There are two switches to switch insert/overwrite modes. The one for the ty-
 pe T  items of the  <a href="#t208">text editor</a>, the latter for other item types. The switch
 for text editor is global one (its setting is valid even for  editing  other
 items). The switch for other items is local - its setting is valid only  for
 the  current edited item. Default setting is insert mode.

 When editing shared files in LAN, there are specific conditions.
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t225">special and control keys</a>      <a href="#t226">data editor switches</a>       <a href="#t227">actions</a>
 <a href="#t228">automatic report</a>              <a href="#t231">database navigation</a>        <a href="#t208">text editor</a>
 <a href="#t311">shared editing</a>
</pre>

<a name="t225"></a>
## special and control keys

<pre>
                        <b>SPECIAL AND CONTROL KEYS</b>
 ────────────────────────────────────────────────────────────────────────────
 ■ <b>F2</b> (<b>enter</b>/<b>edit</b>) ......... switching to enter/edit mode
 ■ <b>F3</b> (<b>search</b>) ............. searching records according to own key
 ■ <b>F4</b> (<b>duplic</b>) ............. the current item is copied from the prev. record
 ■ <b>F5</b> (<b>switches</b>) ........... setting switches on/off
 ■ <b>F6</b> (<b>action</b>) ............. starting file operations
 ■ <b>F9</b> (<b>save</b>) ............... saving  the current file  state  on disk  (power
                             failure protection)

 ■ <b>Ctrl-F2</b> ................. restoring the <b>cond</b> subset (networks, exit proc.)
 ■ <b>Ctrl-F3</b> (<b>record</b>) ........ setting the editor according to the record number
 ■ <b>Ctrl-F5</b> (<b>calculate</b>) ..... calling <b>calculator</b>

 (<b>Ctrl-U</b> restores the last expression, <b>Ctrl-F4</b> moves result back to editor)

 ■ <b>Ctrl-&lt;-</b> or <b>Ctrl--&gt;</b> ...... the adjacent records exchange

 ■ <b>Ctrl-Y</b> .................. deleting the current record (or the entire subset,
                             if defined)
 ■ <b>Ctrl-N</b> .................. inserting a record above the cursor position
 ■ <b>Ctrl-U</b> .................. deleting changes of the record and restoring the
                             original contents from a disk
 ■ <b>Ctrl-\</b> .................. jump to the beginning of the next record
 ■ <b>Esc</b> ..................... exit the data editor
 ■ (<b>Alt</b>,<b>Ctrl</b>,<b>Shift</b>,) - <b>F1</b> to <b>...-F10</b> ... actions defined by programmer
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t226">data editor switches</a>     <a href="#t227">actions</a>     <a href="#t231">database navigation</a>
</pre>

<a name="t226"></a>
## data editor switches

<pre>
                        <b>F5 - DATA EDITOR SWITCHES</b>
 ────────────────────────────────────────────────────────────────────────────
 Press <b>F5</b>. Now, you can switch on/off the <b>switches</b>, selected from menu:

 ┌─ Switch on/off ─┐
 │ <b>S</b>ubset          │ editing  subset of records or entire file
 │ <b>D</b>uplication     │ item is copied from the previous record
 │ <b>T</b>abulator       │ press <b>Enter</b>, cursor jumps only on tab items
 │ <b>A</b>dditive changes│ switching off additive changes to superior file
 │ <b>L</b>ogical checks  │ switching off logical checks of data
 └─────────────────┘

 The Subset, the Additive changes and the  Logical checks are the global swit-
 ches for editing file, whereas the Tabulator and the Duplication are associ-
 ated with every edited item.

 Indication of the switches state: when the <b>subset</b> is switched on, the selec-
 ted records are highlighted and on the last line is an arrow. When the <b>addi-
 tive  changes</b> and the  <b>logical checks</b> are switched off, on the last line are
 the  characters  <b>#A</b>, or <b>#L</b>. The  additive change switch  switches on/off the
 <b>referential integrity</b>  as well. The switched on  <b>tabelators</b> and  <b>duplication</b>
 are marked by arrows.

 <b>Insert/overwrite switch</b>
 When  switching the insert/overwrite modes, there are two  switches. The one
 for  the type T items of the  <a href="#t208">text editor</a>,  the latter for other item types.
 The  switch for the text editor is  global one (setting is valid for editing
 the other items as well). The switch of other item types is local - its set-
 ting is valid only  for editing current item. Default setting is insert mode.
 ────────────────────────────────────────────────────────────────────────────
  <a href="#t227">actions</a>
</pre>

<a name="t227"></a>
## actions

<pre>
                       <b>F6 - DATA EDITOR ACTIONS</b>
 ────────────────────────────────────────────────────────────────────────────
 Press <b>F6</b>. It calls <b>actions</b> for edited file.

 ┌───────────────────┐
 │ <b>P</b>rint records     │   <b>automatic report</b> generator
 │ <b>C</b>hecks            │   performs <b>checks</b> from cursor to the end of the file
 │ <b>N</b>ew subset        │   entering logical expression for selected <b>subset</b>
 │ <b>G</b>raph             │   graph display of file data (interactive mode)
 │ <b>S</b>ort              │   sorting file by defined items
 └───────────────────┘
   <b>░░░░░░░░░░░░</b>
   <b>░░examples░░</b>
   <b>░░░░░░░░░░░░</b>

 ░ Date&gt;=1/1/92                  {entering new subset expression}
 ░ Account in ['300'..'599','999']
 ░ Wage&gt;5000
 ░ Place=~'Prague'
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t228">automatic report</a>       <a href="#t193">selecting from list</a>       <a href="#t226">data editor switches</a>
</pre>

<a name="t228"></a>
## automatic report

<pre>
                            <b>AUTOMATIC REPORT</b>
 ────────────────────────────────────────────────────────────────────────────
 The automatic report enables printing contents of a data file or its section
 using the automatic  print  layout. It is called from the data editor ( <b>F6 -
 action</b>, <b>report</b>), or  directly from a project by the procedure <b>report</b>. It al-
 lows selecting subset, temporary sorting, totalling and splitting a file in-
 to groups.
 ┌───────────────────┐
 │ <b>P</b>rint report      │ printing record of data file or subset
 │ <b>S</b>umming report    │ printing record including totals of selected items
 │ <b>T</b>otals            │ totalled report without records (only totals)
 │ <b>I</b>correct records  │ incorrect records printing (logical checks)
 └───────────────────┘
 Autoreport communication : before generating report enter items:

  ■ <b>totalled items</b> ...... numeric  items, they will be  summarized at the end
                          of the report
  ■ <b>control items</b> ....... split file into  groups of the same control  items,
                          totals are done per group as well
  ■ <b>sort items</b> .......... sorting records according to sort items
                          sorting is temporary and does not influence the file
  ■ <b>subset</b> .............. if you enter logical expression, report will contain
                          only records, that meet the condition.
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t227">actions</a>     <a href="#t229">control and sort items</a>
</pre>

<a name="t229"></a>
## control and sort items

<pre>
                           <b>CONTROL AND SORT ITEMS</b>
 ────────────────────────────────────────────────────────────────────────────
 You can define  <b>control</b> and <b>sort items</b> in  merge operations and in automatic
 <b>reports</b> (called from data editor, or defined in chapter R).

 ■ <b>Control items</b>:  they  divide  file  into  groups  with  the same values of
   control items  (even in several levels). You can carry out required action
   at the end of every group (Eg.totaling).  Before  entering  transformation
   or report, files must be sorted according to control items (it can be pro-
   vided by  automatic temporary sorting or by reading  according to  <b>index</b>).
   If  only  one  file  enters report, it need not be sorted by control items
   (responsibility for result holds, of course, programmer).

 ■ <b>Sort items</b> :  you can  records,  already  sorted  by control  items, addi-
   tionally sort even according to sort items.
 ────────────────────────────────────────────────────────────────────────────
  <a href="#t228">automatic report</a>     <a href="#t250">lexical sorting</a>
</pre>

<a name="t231"></a>
## database navigation

*Also:* `navigation`

<pre>
                          <b>DATABASE NAVIGATION</b>
 ────────────────────────────────────────────────────────────────────────────
 Navigation  allows  to call  from  editing a file another data file to edit,
 with return the to original file. Before calling the data editor, choose the
 desired <b>file</b> or the <b>user view</b> from window.

 ■ <b>F7</b> (<b>file</b>) ........... when  navigating from the  subordinated to the supe-
                         rior files  are offered only the corresponding supe-
                         rior files; the data editor finds the visible record
                         (has the same key) or the nearest higher
 ■ <b>Ctrl-F7</b> (<b>subordinate file</b>) ..... when navigating from the superior to the
                         subordinated file, only the subordinated  files with
                         the  corresponding  own key are  offered;  there are
                         selected only the records from the subordinated file
                         with the same key
 ■ <b>Alt-F7</b> .............. navigation <b>through whole database</b> (all files)
 ■ <b>Esc</b> ................. return from inserted editing
 ■ <b>Ctrl-F4</b> (<b>transfer</b>)... return  from inserted  editing  with  transfer of an
                         item value
 ■ <b>Shift-F7</b> ............ quick navigation <b>upwards</b> to the superior file can be
                         used to  search and to  transfer an item  value  from
                         the superior file. It can be called only from the key
                         item, defining connection upwards; jump to the supe-
                         rior file (only view) follows  immediately,  without
                         file or user's view selection
   <b>Enter</b> means  return with  transferred  key item value, <b>Esc</b> return  without
   transfer. The superior file can be edited (records  entered as well) under
   conditions, defined by  programmer.
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t233"></a>
## catalog

<pre>
                                <b>CATALOG</b>
 ────────────────────────────────────────────────────────────────────────────
 The <b>catalog</b> is an optional file associated  with a  program  (the same name,
 suffix <b>.CAT</b>) By means of the  <b>catalog</b> can be changed some default  features,
 especially the  <b>real file names</b> on disk. One catalog  record  corresponds to
 one file of program (file is then <b>catalogized</b>) and contains following items:

 ■ <b>ProgramName</b> ......... a program, where is the file described and used in
 ■ <b>FileName</b> ............ a logical file name, used in source texts of program
                         (caution - it is not always chapter F)
 ■ <b>Archivation number</b> ...level of a file archivation
 ■ <b>Path</b> ..,............. a full file path (<b>Unit,Path,Name,Extend</b>)
                         The path need not  be full, there  are  applied  the
                         MS-DOS  conventions  for the path  completation; de-
                         fault directory is the program directory.
 ■ <b>Label</b> ............... a diskette label, if a file is on diskette,
                         or char. <b>#</b>, <b>#R</b> <b>SQL</b> (see <a href="#t304">LAN</a>)

 ██ <b>Creating and editing catalog</b>
    The catalog can be  created and  edited from the PC FAND main menu by se-
    lection the <b>installing program</b> (in user's version as well).
 <b>Note</b>  Upgrading the PC FAND 3.0 (3.01) to 3.2 version, the catalog structure
    was changed (item Ar). An old format is transformed automatically. Trans-
    formation  from a new  format to the old  one  can be  done by  using the
    <b>OldCatlg.EXE</b> program. (Evidently, suitable MERGE can be used, too).
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t304">LAN</a>                   <a href="context-help.md#t6">Installing program</a>
</pre>

<a name="t235"></a>
## expressions

<pre>
                                  <b>EXPRESSIONS</b>
 ────────────────────────────────────────────────────────────────────────────
 <b>Expressions</b>  are  symbols  of  calculations  and  they  appear many times in
 program.  They are made up in usual way - of constants, variables, operators
 and functions.

 ■ <a href="#t239">expression types</a> ......... numerical - <b>real</b>, text type - <b>string</b>
                              logical - <b>boolean</b>
 ■ <a href="#t240">constants</a> ................ all types
 ■ <a href="#t241">operators</a> ................ numerical, text type, logical, relational

 ■ <a href="#t265">arithmetic functions</a> ..... real, trigonometric, logarithmic
 ■ <a href="#t272">date and time functions</a> .. system  date and time, conversion  from date to
                              string and vice versa

 ■ <a href="#t298">processing text</a> .......... searching, conversion from string to number and
                              opposite, substrings, text files and memo texts
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t251">░ expressions</a>
</pre>

<a name="t239"></a>
## expression types

*Also:* `real`, `string`, `boolean`

<pre>
                              <b>EXPRESSION TYPES</b>
 ────────────────────────────────────────────────────────────────────────────
 (on the right side are compatible <b>item types</b> from file declaration )

 ■ <b>real</b> ...... <b>numerical</b> (Pascal 6-byte real)-11 significant digits  <b>F D</b>
 ■ <b>string</b> .... <b>text</b> - up to 65 000B free (memo) text               <b>A N T</b>
 ■ <b>boolean</b> ... <b>logical</b> - true/false (Yes/No)                           <b>B</b>

 ────────────────────────────────────────────────────────────────────────────
 <a href="#t251">░ expressions</a>
</pre>

<a name="t240"></a>
## constants

<pre>
                                   <b>CONSTANTS</b>
 ────────────────────────────────────────────────────────────────────────────
 ■ <b>numeric constant</b> ..... integer ( with sign, if necessary)
                      ... fixed-point number
                      ... date in form  <b>DD.MM.YY</b>
                      ... time in the  <b>hh:mm</b>, <b>hh:mm:ss</b> or <b>hh:mm:ss.tt</b> form

 ■ <b>text constant</b> ........ character string between apostrophes
                          apostroph  inside  of a constant:  double apo-
                          strophe <b>''</b>
                          character entered in decimal ASCII code: <b>\nnn</b>

 ■ <b>logical constant</b> ... <b>false</b>, <b>true</b>
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t271">date mask</a>
</pre>

<a name="t241"></a>
## operators

<pre>
                                <b>OPERATORS</b>
 ────────────────────────────────────────────────────────────────────────────
 logical                 arithmetical               relational
 ───────────────────     ──────────────────────     ────────────────────
 negation    (NOT) <b>^</b>     addition             <b>+</b>     equal              <b>=</b>
 intersection(AND) <b>&amp;</b>     subtraction          <b>-</b>     not equal          <b>&lt;&gt;</b>
 disjunction (OR)  <b>|</b>     multiplication       <b>*</b>     greater than       <b>&gt;</b>
 implication       <b>=&gt;</b>    division             <b>/</b>     greater than 
                                                          or equal     <b>&gt;=</b>
 equivalence      <b>&lt;=&gt;</b>    integer division    <a href="#t246">div</a>    less than          <b>&lt;</b>
                         remainder           <a href="#t246">mod</a>    less or equal      <b>&lt;=</b>
 text                    rounding           <a href="#t246">round</a>   member of set      <a href="#t246">in</a>
 ──────────────
 concatenation (of strings)   <b>+</b>

 ■ relational operators are used for text and number comparison and can be
   completed :
 - <b>Operator.Number</b> .. precision of arithmetical comparison (default 5 decimals)
 - <b>Operator~</b> ........ lexical text comparison according to national alphabet
                      ignoring right spaces

 ░ 1.234 &gt; 1.2   but   1.234 =.1 1.2
 ░ 'Praha' &lt;&gt; 'Praha '   but   'Praha' =~ 'PRAHA   '       'baba'=~'bÁBa'
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t250">lexical sorting</a>         <a href="#t247">operator precedence</a>         <a href="#t251">░ expressions</a>
</pre>

<a name="t246"></a>
## special operators

*Also:* `div`, `mod`, `round`, `in`

<pre>
                            <b>SPECIAL OPERATORS</b>
 ────────────────────────────────────────────────────────────────────────────
 █ NumExpr <b>div</b> NumExpr : real ..... integer <b>division</b>
 █ NumExpr <b>mod</b> NumExpr : real ..... modulo, <b>remainder</b> of integer division
 ░ 10 div 3 = 3
 ░ 10 mod 3 = 1

 █ NumExpr1 <b>round</b> NumExpr2 : real ... <b>rounds</b> value of NumExpr1 to number of
                                      decimals  - given by NumExpr2

 ░ Pi round 2 = 3.14
 ░ 1.6 round 0 = 2

 █ Expr <b>in</b> [ArgumList] : boolean .... <b>test of set elements</b>.  Returns true, if
                                      the  value of the  <b>Expr</b> is a  member of
                                      the set,  defined by the list of  cons-
                                      tants or intervals.
                                      The constant  and  the  expression  are
                                      of the same type (real or string).
 ■ <b>in.Number</b> ..<b>precision</b> of arithmetical comparison  (number of decimals, de-
                                        fault value is 5)
 ■ <b>in~</b> ....... <b>lexical</b> comparison of text, according to national alphabet

 ░ Account in ['300'..'399','520','800'..'999']
 ░ Year in [1914..1918,1939..1945,1968,1989]
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t248">░ special constructions</a>
</pre>

<a name="t247"></a>
## operator precedence

<pre>
                            <b>OPERATOR PRECEDENCE</b>
 ────────────────────────────────────────────────────────────────────────────
 Unless bracketed, the operators have following order of precedence (from the
 highest to the lowest):

                        1.  - (unary minus sign)  ^
                        2.  *  /  div  mod  round
                        3.  +  -
                        4.  =  &lt;&gt;  &gt;  &gt;=  &lt;  &lt;=
                        5.  &amp;
                        6.  |
                        7.  =&gt;  &lt;=&gt;
 ───────────────────────────────────────────────────────────────────────────
</pre>

<a name="t248"></a>
## ░ special constructions

<pre>
                     <b>EXAMPLE - SPECIAL CONSTRUCTIONS</b>
 ────────────────────────────────────────────────────────────────────────────
 ░ bonus := cond (children  = 1:  200,                        {function cond}
 ░                children  = 2:  430,
 ░                children  = 3:  680,
 ░                children  = 4: 1240,
 ░                children &gt;= 5: 1240+(children-4)*350);
 ░ cond( Man:'sir '+name,
 ░      else:'Ms '+name);

 ░ cond(account in ['100'..'199 ','450','700'..'999']: Debit - Credit;
                                                                {operator in}
        account in ['200',,'399']: Credit - Debit;
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t250"></a>
## lexical sorting

*Also:* `lexical comparison`

<pre>
                             <b>LEXICAL SORTING</b>
 ────────────────────────────────────────────────────────────────────────────
 The PC FAND allows to work with installed national code table. Support of na-
 tional code table involves the  <b>sorting</b>, <b>comparising</b>, <a href="#t287">upcase</a> and <a href="#t287">lowcase</a> fun-
 ctions. In project, lexical relations are  distinguished from normal sorting
 (by character code). You must type <b>~</b> behind the operator or before the key
 item (controling or sorting).

 When  <b>sorting</b> lexically, the diphthong  <b>CH</b> is put into its proper place, the
 <b>uppercase</b> and the  <b>lowercase</b> are equivalent, characters with  <b>lenght mark</b> or
 without it are equivalent. Character with the <b>hooklet</b> falls behind character
 without it. When <b>comparising</b> lexically, right spaces are ignored. The lexical
 lation intervene in the following cases:

 ■ <b>sorting</b> according to national alphabet (interactive sorting in <b>data editor</b>)
 ■ <b>key items</b> of type A in <b>key</b> definitions
 ■ <b>control</b> and <b>sort items</b> in <b>report</b> and <b>merge</b>
 ■ <b>comparing operators</b> for text comparison, operator <b>in</b>, function <b>pos</b>
 ■ <b>searching</b> in text editor

 ░ Name=~''   #K @ ~Name   #I1_DATA ~Control Item;~SortItem   Place in~ [...]
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t205">installing alphabet</a>    <a href="#t241">operators</a>    <a href="#t193">selecting from list</a>    <a href="#t218">searching</a>
</pre>

<a name="t251"></a>
## ░ expressions

<pre>
                            <b>EXAMPLES - EXPRESSIONS</b>
 ────────────────────────────────────────────────────────────────────────────
 ░ 3.14                       {numeric expressions}
 ░ number+1
 ░ Today-BirthdayDate
 ░ quantity * pricelist.price
 ░ 12:30:01 + 24.12.89   
 ░ sin(1.23456789)+exp(2*int(x))

 ░ 'Prag'                      {text expressions}
 ░ Name+' '+surname
 ░ copy(upcase(text),5,10)
 ░ str(registry.price,6,2)+' $'
 ░ leadchar(' ',Name);

 ░ true                         {logical expressions}
 ░ pay &gt; 3000
 ░ (Name='Brown' &amp; town ='London')
 ░ born &lt; 1.1.50
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t265"></a>
## arithmetic functions

*Also:* `abs`, `int`, `frac`, `sqr`, `sqrt`, `random`, `ln`, `exp`, `pi`, `sin`, `cos`, `arctan`

<pre>
                             <b>ARITHMETIC FUNCTIONS</b>
 ────────────────────────────────────────────────────────────────────────────

   The following function are of <a href="#t239">real</a> type, therefore their result is numeric
   value.

 █ <b>abs</b>(NumExpr) ....... absolute value
 █ <b>int</b>(NumExpr) ....... integer part of the number (left of decimal point)
 █ <b>frac</b>(NumExpr) ...... fractional part of the number (right od decimal point)
 █ <b>sqr</b>(NumExpr) ....... square
 █ <b>sqrt</b>(NumExpr) ...... square root
 █ <b>random</b> ............. random number in interval &lt;0,1)

 █ <b>ln</b>(NumExpr) ........ natural logarithm
 █ <b>exp</b>(NumExpr) ....... exponential function

 █ <b>pi</b> ................. 3.14159 ...
 █ <b>sin</b>(NumExpr) ....... sine
 █ <b>cos</b>(NumExpr) ....... cosine
 █ <b>arctan</b>(NumExpr) .... arctangent
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t270"></a>
## valdate

*Also:* `today`, `currtime`, `strdate`

<pre>
                     <b>CURRENT DATE AND TIME, CONVERSION</b>
 ────────────────────────────────────────────────────────────────────────────
  The function <b>today</b> returns the current (today's) date according to the sys-
  tem clock. Similarly, the function <b>currtime</b> returns the current time. Both
  return values are of the PC FAND type D.

 ██ Syntax:               <b>TODAY</b> : real
                       <b>CURRTIME</b> : real
 The function <b>strdate</b>  converts  date and time from  internal  format  into a
 string. The function  <b>valdate</b>  does the  opposite.  Both  functions  convert
 according to a mask.
 A <a href="#t271">date mask</a> formats text expression of data and time.

 ██ Syntax:     <b>STRDATE (</b>NumExp<b>, </b>Mask<b>)</b>  : string
                <b>VALDATE (</b>TextExp<b>, </b>Mask<b>)</b> : real

 ■ <b>NumExp</b> .... Date and time in internal format of the PC FAND (real)
 ■ <b>TextExp</b>.... Date and time in text format.
  ───────────────────────────────────────────────────────────────────────────
  <a href="#t272">Date and time functions</a>                      <a href="#t278">░ date and time</a>
</pre>

<a name="t271"></a>
## date mask

<pre>
                                 <b>DATE MASK</b>
 ────────────────────────────────────────────────────────────────────────────
  <b>Mask</b> for  date  is any string  between apostrophes, containing beside sepa-
       rator  special  characters for  <b>date and time units</b>. These characters
       will be, in result, replaced with real values.
       Mask defines the way of date displaying.
       ■ <b>Y</b>-year, <b>M</b>-month, <b>D</b>-day,
       ■ <b>H</b>-hour, <b>m</b>-minute, <b>s</b>-second, <b>t</b>-hundredth of a second
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t272"></a>
## date and time functions

<pre>
                         <b>DATE AND TIME FUNCTIONS</b>
 ────────────────────────────────────────────────────────────────────────────
 The PC FAND  contains  data arithmetic, ie. calculating and conversion fun-
 ctions of date and time. It even allows you to work with the internal calendar
  - working days table, see the <a href="#t200">Installation program</a>. If functions work with
 date and time like with numeric expressions (type <b>real</b>), in question is the
 same format as the FAND's type <b>D</b>.
 ( The  integer  part  is the  consecutive  number of  the day  counted  from
 01.01.0001, its decimal part is part of the day).

<b>Function overview</b>:
 ■ <a href="#t270">today</a> .......... today's date (according to system clock)
 ■ <a href="#t270">currtime</a> ....... returns thecurrent time
 ■ <a href="#t270">strdate</a> ........ date and time conversion into string
 ■ <a href="#t270">valdate</a> ........ date and time conv. from string into internal one (real)
 ■ <a href="#t275">addwdays</a> ....... addition of defined number of days to the initial date
 ■ <a href="#t275">addmonth</a> ....... addition of defined number of months to the initial date
 ■ <a href="#t277">difwdays</a> ....... number of days inside the defined time interval
 ■ <a href="#t277">difmonth</a> ....... number of months inside the defined time interval
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t273"></a>
## day type

<pre>
                                 <b>DAY TYPE</b>
 ────────────────────────────────────────────────────────────────────────────
 The function <b>typeday</b> returns type of the entered day

 ██ Syntax:    <b>TYPEDAY (</b> Date <b>)</b> : real

 ■ <b>Date</b> ........Numeric expression, date in the internal PC FAND format
 ■ <b>Day Type</b> ....Returned values :
                  <b>0</b> - working day
                  <b>1</b> - Saturday
                  <b>2</b> - Sunday
                  <b>3</b> - holiday
 ────────────────────────────────────────────────────────────────────────────
  <a href="#t272">date and time functions</a>    <a href="#t200">installation program</a>       <a href="#t278">░ date and time</a>
</pre>

<a name="t275"></a>
## addmonth

*Also:* `addwdays`

<pre>
                             <b>TIME ADDITION</b>
 ────────────────────────────────────────────────────────────────────────────

 The function <b>addwdays</b> adds (or subtracts) entered number of days to/from the 
 initial date. Similarly, the function <b>addmonth</b> adds entered number of months.
 Returned values are in the internal PC FAND format.

 ██ Syntax:      <b>ADDWDAYS</b> ( IniDate , HowManyDays [,Type] ) : real
                 <b>ADDMONTH</b> ( IniDate , HowManyMonths )       : real

 ■ <b>IniDate</b> .........Numeric expression, initial date in internal format
 ■ <b>HowManyDays</b> .....Numeric  expression, how many days to add. If it is nega-
                    tive one, the number will be subtracted.
                 
 ■ <b>HowManyMonths</b> ...similarly
 ■ <b>Type</b> ............<a href="#t273">Day type</a> - default value is <b>Type 0</b> (working day).
                    The installed  table of working days influences  also re-
                    turned value of the function addwdays.
                    See <a href="#t200">Installation program</a>.
 ■ Limit ...........The <b>addWDays</b> has upper limit 2020 (protection against en-
                    cycling)

 The inverse functions : <a href="#t277">difwdays</a>, <a href="#t277">difmonth</a>
  ───────────────────────────────────────────────────────────────────────────
  <a href="#t272">date and time functions</a>          <a href="#t278">░ date and time</a>
</pre>

<a name="t277"></a>
## difmonth

*Also:* `difwdays`

<pre>
                            <b>TIME SUBTRACTION</b>
  ───────────────────────────────────────────────────────────────────────────

 The function <b>difwdays</b> returns number of working days (or days of the defined
 type)  between two data values. Similarly, the  <b>difmonth</b> returns  difference
 (in months) between the entered data values. The  functions  work in the in-
 terval 1.1.1910 ....... 31.12.2019. Tne IniDate is not counted.

 ██ Syntax:     <b>DIFWDAYS (</b> IniDate <b>,</b>EndDate [<b>,</b>Type] <b>)</b>  : real
                <b>DIFMONTH (</b> IniDate <b>,</b>EndDate <b>)</b>            : real

 ■ <b>IniDate</b>
 ■ <b>EndDate</b> .... Numeric expressions, beginning and ending date, in internal
                format. EndDate&lt;Inidate is allowed too.
 ■ <b>Type</b> ....... <a href="#t273">Day type</a>  - default value is <b>Type 0</b> (working day).
                The installed  table of working days  influences also the re-
                turned value of the function difwdays.
                See <a href="#t200">Installation program</a>.

 The inverse functions: <a href="#t275">addwdays</a>, <a href="#t275">addmonth</a>
  ───────────────────────────────────────────────────────────────────────────
  <a href="#t272">date and time functions</a>            <a href="#t278">░ date and time</a>
</pre>

<a name="t278"></a>
## ░ date and time

<pre>
                           <b>EXAMPLE - DATE AND TIME</b>
 ────────────────────────────────────────────────────────────────────────────
 ░ StrDate(Today+CurrTime,'DD.MM.YYYY hh:mm:ss')
 ░ ValDate(Date,'YMM')
 ░ AddWDays(day,3)
 ░ TypeDay(AddMonth(today,-1))
 ░ DifWDays(1.1.90,31.12.90)
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t282"></a>
## length

*Also:* `ord`, `char`

<pre>
                              <b>TEXT FUNCTIONS</b>
   ──────────────────────────────────────────────────────────────────────────

   Lenght of a string

   ██ syntax:   <b>LENGTH ( </b>TextExpression <b>)</b> : real

   ■  Chapters ........ F,M,R,P,D
   ──────────────────────────────

   Working with ASCII code. The function CHAR returns character, found in the
   used code table. The function ORD, in opposite,  returns ASCII code of the
   entered character.
   These functions allow evaluating of any BYTE-stream.

   ██ syntax:   <b>CHAR (</b> Code <b>)</b>           : string
                <b>ORD  (</b> TextExpression <b>)</b> : real

   ■  <b>Code</b> ............. ASCII code of a character. Counted modulo 256.
   ■  <b>TextExpression</b> ... ORD returns the code of the first string character
                           ord('') = 0  See  <a href="#t205">installing alphabet</a>
   ■  Chapters .... F,M,R,P,D

   <a href="#t298">processing text</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░Examples░░</b>
   <b>░░░░░░░░░░░░</b>

   ██    char( 65 )  = A  ;          { = char ( 321 ) }
         ord('Jan')  = 74 ;
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t284"></a>
## val

*Also:* `str`

<pre>
                          <b>CONVERSION TEXT FUNCTIONS</b>
   ──────────────────────────────────────────────────────────────────────────
   The function  <b>val</b> converts text into number. It can be used in chapters F,
   M,R,P,D. To convert date and/or time from text into number use the <a href="#t270">ValDate</a>.
   

   ██ syntax :         <b>VAL ( </b>TextExpression <b>)</b> : real
   ────────────

   Conversion of numeric expression into text.

   ██ syntax   1:   <b>STR (</b> NumExpression<b>,</b> Width<b>,</b>NoOfDecimals<b>)</b> : string
               2:   <b>STR (</b> NumExpression <b>,</b> Mask <b>)</b>             : string

   ■  <b>Width</b> ......... Numeric expression, full length of string.
   ■  <b>NoOfDecimals</b> .. Numeric expression, number of decimals (right of dec.point)
                      According to number of decimals, it will be rounded. In
                      opposite, at overflow left of decimal point, the result
                      is a string of full length.
   ■  <b>Mask</b> .......... Text expression. The  following characters have special
                      meanings:
        '<b>,</b>'           decimal point (suppressed, if there is no integer part)
        '<b>.</b>'           point, separating groups of digits
        '<b>_</b>'           significant digits, sign from left or space
        '<b>-</b>'           spaces, if the number is positive
        '<b>*</b>','<b>0</b>'...... significant  digits  or sign  from left.  All  the '<b>_</b>',
                      right  of '<b>*</b>', '<b>0</b>' will be replaced  by digits. If  the
                      number is non-zero, digits will put out beginning  with
                      the first digit, left of decimal point (at the latest).
                      The  decimals right of decimal point must be in the ex-
                      pression in the fraction form.
                      The nonmasked digits or sign are appended from the left
                      (overflow).
   ■  Chapters ...... F,M,R,P,D

   <a href="#t298">processing text</a>

   <b>░░░░░░░░░░░░</b>     ██  str(625.128,5,2) = '625.13'
   <b>░░examples░░</b>         str(625.128,1,0) = '625'
   <b>░░░░░░░░░░░░</b>         str(625,6,0)     = '   625'
                        str(1234.25,'__.___,__') = ' 1.234,25'
                        str(1234,'000.000') = '001.234'
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t287"></a>
## nodiakr

*Also:* `upcase`, `lowcase`

<pre>
                   <b>WORKING WITH DIACRITICS AND ALPHABET</b>
   ──────────────────────────────────────────────────────────────────────────

   Conversion of uppercase characters into lowercase characters and vice versa

   ██ syntax:       <b>UPCASE ( </b> TextExpression <b>)</b> : string
                    <b>LOWCASE( </b> TextExpression <b>)</b> : string

   ■ The upcase converts lowercase characters into uppercase  characters, the
     lowcase, in opposite, converts uppercase characters into lowercase ones.
   ■ Chapters .... F,M,R,P,D
   ─────────────────────────────

   Removing diacritics from text. Use in chapters F,M,R,P,D.

   ██ syntax:      <b>NODIAKR( </b> TextExpression <b>)</b> : string

   <b>Note</b> : These  functions use installed national code table  (Code Kamenicky
          and code Latin2).  Some  characters  with diacritics  are  visually
          identical, but codes of converted characters can be different.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t298">processing text</a>
</pre>

<a name="t289"></a>
## copy

*Also:* `repeatstr`

<pre>
                               <b>TEXT FUNCTIONS</b>
   ──────────────────────────────────────────────────────────────────────────

    Selection  of substring from the text. The function returns the substring
    of the  <b>TextExpression</b>,  the  <b>Length</b>  long,  beginning  from the  defined
    position.

   ██ syntax:   <b>COPY (</b> TextExpression<b> ,</b>FromPosition<b> ,</b>Length<b> )</b> : string

   ■  Chapters .... F,M,R,P,D

   ──────────────────────────

   Building string by repetition

   ██ syntax :     <b>REPEATSTR (</b>TextExpression<b>,</b>HowManyTimes<b>)</b> : string

   ■  Chapters .... F,M,R,P,D

   <a href="#t298">processing text</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░examples░░</b>           ██   copy('Jan Novák',5,5) = 'Novák'
   <b>░░░░░░░░░░░░</b>
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t292"></a>
## replace

*Also:* `leadchar`, `trailchar`

<pre>
                             <b>REPLACING IN TEXT</b>
   ──────────────────────────────────────────────────────────────────────────
   Deleting or replacing of leading (LEADCHAR) or trailing (TRAILCHAR)
   characters in text.

   ██ syntax:    <b>LEADCHAR  ( '</b>Char1<b>'</b> <b>,</b> Text [<b>,'</b>Char2<b>'</b> ] ) : string
                 <b>TRAILCHAR ( '</b>Char1<b>'</b> <b>,</b> Text [<b>,'</b>Char2<b>'</b> ] ) : string
   ■ <b>Char1</b>,<b>Char2</b> ... character Char1 is deleted in text the <b>Text</b>, or, if
                         is defined Char2, is Char1 replaced by Char2.
   ■ Chapters .... F,M,R,P,D

   ──────────────────────────

   Replacing substring in text by another string. Unlike previous functions,
   all occurrences are replaced, not only the first or the last.

   ██ syntax: <b>REPLACE (</b>What<b>,</b>Where<b>,</b>ByWhat [<b>,'</b>Conditions<b>'</b>]<b>)</b>: string

   ■ <b>What</b>,<b>Where</b>,<b>ByWhat</b> ... Text expressions
   ■ <b>Conditions</b> ..... see function <a href="#t294">pos</a>


   <a href="#t298">processing text</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░examples░░</b>
   <b>░░░░░░░░░░░░</b>


   ██   leadchar('x',trailchar('x','xxxAAAAxx')) = 'AAAA'

   ██   The functions leadchar a trailchar are mostly used to delete unneces-
        sary spaces from the items of type A,n (constant lenght strings).

        The condition for editing all Smiths must be defined:

        trailchar(' ',Name)='Smith')

        The Name='Smith' condition would select empty set,  because when com-
        paring strings, string lenghts must be identical as well;  a space is
        also a character.
        
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t294"></a>
## equmask

*Also:* `pos`

<pre>
                       <b>COMPARING TEXT STRINGS</b>
   ──────────────────────────────────────────────────────────────────────────
   Searching substring position according to conditions of comparison.

   ██ syntax: <b>POS( </b>Substring<b>,</b>Text [<b>,'</b>Conditions<b>'</b>] [<b>,</b>WhatOccurr<b>)</b>  : real
                                                                     
   ■  <b>Substring</b> .... Text expression, searched in the string <b>text</b>.
   ■  <b>Text</b> ......... Text, where is searched in
   ■  <b>Conditions</b> ... Text constant between apostrophes
          It can contain the following characters (or their combination):

               <b>w</b> ... search only whole words, not their parts
               <b>~</b> ... <a href="#t250">lexical comparison</a> according to national alphabet
               <b>u</b> ... without diferentiation lowercase/uppercase characters
   ■  <b>WhatOccurr</b> ... What occurrence is searched (number, counted from the
                     beginning).
   ■  Chapters ..... F,M,R,P,D
   ──────────────────
   Comparison text string with mask.

   ██ syntax:   <b>EQUMASK( </b>Text <b>,</b> Mask <b>)</b> : boolean

   ■  <b>mask</b> ..........Text expression. It is a pattern, compared with  text
                     items of the  <b>text</b>.  Mask need not be  unique, it can
                     contain special characters (so called wildcards):
                     <b>?</b>  will be  replaced by any  character - only in defi-
                        ned position
                     <b>*</b>  will  be  replaced  by  any  character  combination
                        (string)
   ■  Chapters ..... F,M,R,P,D

   <a href="#t298">processing text</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░examples░░</b>
   <b>░░░░░░░░░░░░</b>

   ██  pos( 'lhota', 'Dolní Lhota')      = 0 ;
       pos( 'lhota', 'Dolní Lhota','<b>u</b>' ) = 7 ;
       pos( 'dolni', 'Dolní Lhota','<b>u</b>' ) = 0 ;              { hinders <b>i</b> &amp; <b>í</b> }
       pos( 'dolní', 'Dolní Lhota','<b>~</b>' ) = 1 ;
       pos( 'lh', 'Dolní Lhota','<b>w~</b>' )   = 0 ;           { only whole words }
       pos( 'lh', 'Dolní Lhota','<b>u</b>' )    = 7 ;
       pos( 'lh', 'Dolní Lhota','<b>u</b>',2 )  = 0 ;  { lh occurres only one time }

   ██  equmask( 'Dolní Lhota',  'Lhota' ) = FALSE ;
       equmask( 'Dolní Lhota', '*Lhota' ) = TRUE ;
       equmask( 'Dolní Lhota', '?Lhota' ) = FALSE ;
       equmask( 'Dolní Lhota', '??????Lhota' ) = TRUE ;

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t296"></a>
## linecnt

*Also:* `copyline`

<pre>
                             <b>TEXT LINE PROCESSING</b>
   ──────────────────────────────────────────────────────────────────────────
   The lenght of <b>memo texts</b> (and local variables of a <b>string</b> type) is limited
   by 65 000 B. Function parameters are text and numerical <b>expressions</b>.

   Selection one or more text lines, beginning from the defined initial line.

   ██ syntax :  <b>COPYLINE (</b> Text <b>,</b> LineNumber[ <b>,</b>HowMany ] <b>)</b> : string
   ■  Chapters ....... F,M,R,P,D
   ─────────────────────────────

   Number of text lines.
   
   ██ syntax :        <b>LINECNT (</b> TextExpression <b>)</b> : real

   ■  Chapters ....... F,M,R,P,D

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t298">processing text</a>
</pre>

<a name="t298"></a>
## processing text

*Also:* `functions for text processing`

<pre>
                <b>PROCESSING TEXT</b> - overview of functions and commands
    ─────────────────────────────────────────────────────────────────────────
    <a href="#t282">Char</a> ............... returns character according to the ASCII code
    <a href="#t289">Copy</a> ............... selecting substring
    <a href="#t296">CopyLine</a> ........... reading in the defined line from multiline text
    <a href="#t294">EquMask</a> ............ string comparison with mask
    <a href="#t292">LeadChar</a> ........... deleting or replacing leading characters
    <a href="#t282">Length</a> ............. text lenght
    <a href="#t296">LineCnt</a> ............ number of text lines
    <a href="#t287">LowCase</a> ............ uppercase character conversion into lowercase one
    <a href="#t287">NoDiakr</a> ............ removing diacritics
    <a href="#t282">Ord</a> ................ returns ASCII code of the character
    <a href="#t294">Pos</a> ................ position of substring occurrence in text
    <a href="#t289">RepeatStr</a> .......... creating text by string repetition (or character)
    <a href="#t292">Replace</a> ............ replacing substring by another substring
    <a href="#t284">Str</a> ................ number conversion into text
    <a href="#t270">StrDate</a> ............ date (time) conversion from numeric into text form
    <a href="#t292">TrailChar</a> .......... deleting or replacing leading characters
    <a href="#t287">UpCase</a> ............. lowercase character conversion into uppercase one
    <a href="#t284">Val</a> ................ number in text form conversion into numeric typ(real)
    ─────────────────────────────────────────────────────────────────────────
</pre>

<a name="t301"></a>
## graph display control

*Also:* `graphs`

<pre>
                          <b>GRAPH DISPLAY CONTROL</b>
 ────────────────────────────────────────────────────────────────────────────

  When displaying graph, there are three possibilities:

  ■ <a href="#t302">Printing graph</a>  without question; when printing is finished, program
    continues by next statement
  ■ displaying with time delay (<b>demonstration</b>), prolongation by pressing
    a key
  ■ normal displaying - waits for pressing key:

    <b>F6</b>,<b>ShiftF6</b> - printing graph, can be interrupted by ESC key
    <b>F6</b>,<b>ShiftF9</b> - saving into the GRAPH.PCX file, (in the default directory,
                 the  <b>PCX</b>  format). In  interactive  mode, file name can be
                 changed. Termination of saving is audible signalized.
    <b>46</b>,<b>Shift4</b>  - inverse graph display
    <b>ESC</b>        - exit graph display

    <b>Shift</b>     - keys are related to current window, def. by parameter WW

    If  the picture is larger than defined window by  <b>PCX</b> display mode, there
    is possibility of moving the picture in the window:
                                  <b>arrow keys</b> - small step
                                   <b>PgUp</b>,<b>PgDn</b> - the whole window vertically
                        <b>Ctrl-Right</b>,<b>Ctrl-Left</b> - the whole window horizontally
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t302"></a>
## printing graph

<pre>
                               <b>PRINTING GRAPH</b>
 ────────────────────────────────────────────────────────────────────────────

  To print a graph you can use MS-DOS means (PrintScreen, function graphics).
  More advantageous is using the PC FAND support, ie. the <b>F6</b> key for the who-
  le screen, or the <b>ShiftF6</b> to print the current window.

 ■ Graph print is a copy of screen pixels onto printer in the scale 1:1, ie.
   <b>point</b> to <b>point</b>. Of course, relation between screen grid and print density
   on printer is important.

 ────────────────────────────────────────────────────────────────────────────
 <a href="#t301">graph display control</a>
</pre>

<a name="t304"></a>
## LAN

<pre>
                                    <b>LAN</b>
   ──────────────────────────────────────────────────────────────────────────
   In local area  network, the FAND, program, even data files can be  shared.
   Before  using the FAND in computer network, ensure that your  installation
   was has been adapted according to shared object.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t305">Sharing FAND</a>                              <a href="#t308">SHARE</a> program parameters
   <a href="#t306">Sharing program</a>                           <a href="#t309">installing network</a>
   <a href="#t307">Sharing data files</a>                        <a href="#t310">LAN concepts overview</a>
</pre>

<a name="t305"></a>
## sharing FAND

<pre>
                               <b>SHARING FAND</b>
   ──────────────────────────────────────────────────────────────────────────
   To share  FAND,  DOS attribute  ReadOnly  for files  <b>FAND.EXE</b> and <b>FAND.OVR</b>
   must be set.

   Before  starting  shared  FAND,  install  operating  system  parameters
   (<a href="#t196">SET-parameters</a>) as follows:

   ■ <b>SET FANDWORK=</b> paths of working files
     - every participant has his own directory in local computer or on server

   Files FAND.OVR, FAND.CFG, FAND.RES and FANDHLP.* are normally searched for
   in the  FAND.EXE directory.  Standard search order can be changed by other
   parameters of operating system: <b>SET FANDOVR=</b>, <b>SET FANDCFG=</b>, <b>SET FANDRES=0</b>.
   "Network" parameters can be set by using program FANDINST:

     <b>net delay</b> - repeating attempts at locked file
     <b>refresh</b>   - screen refresh in data editor
 
  ──────────────────────────────────────────────────────────────────────────
  <a href="#t309">installing network</a>     <a href="#t306">Sharing program</a>         <a href="#t307">Sharing data files</a>
</pre>

<a name="t306"></a>
## sharing program

<pre>
                         <b>SHARING PROGRAM IN LAN</b>
 ────────────────────────────────────────────────────────────────────────────
 Program  can be shared only in  execution mode (is impossible to share deve-
 ping). Project files  (<b>*.RDB</b> and <b>*.TTT</b>)  and files with  program help (.HLP)
 must have  ReadOnly DOS attribute. If locked by password, this  attribute is
 automatically  set for  FAND program. When installing or developing, they
 are reseted (for the time of changes).

 When working with program catalog, there are two variants :
 ■ <b>shared catalog</b> - when catalog does not include program,  will do  setting
   ( from keyboard )  the ReadOnly DOS-attribute ( attribute does not hinder
   during installation).  Catalog can be in the directory of the shared prog-
   ram, or according to the setting:
   <b>SET FANDCAT=</b> or <b>SET FANDDATA=</b>.

 ■ <b>every participant has his own catalog</b> - before starting FAND set parameter
   <b>SET FANDCAT=</b> or <b>SET FANDDATA=</b> for every participant in his own directory.
   (In local computer or on server.)

 <b>SET FANDDATA=</b> - defines location of catalog and data files, not included
                 in catalog. Default location is in the program directory.
 <b>SET FANDCAT=</b>  - defines  only  catalog  location,  used  also  by  using
                 SET FANDDATA
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t309">installing network</a>       <a href="#t305">Sharing FAND</a>       <a href="#t307">Sharing data files</a>
</pre>

<a name="t307"></a>
## sharing data files

<pre>

                       <b>LAN - SHARING DATA FILES</b>
 ────────────────────────────────────────────────────────────────────────────
 Your data files can be shared, only if you have installed  network software.
  ┌─────────────────────────────────────────────────────────────────────────┐
  │ Before starting FAND, parameter of operating system must be installed:  │
  │ ■ <b>SET LANNODE=</b>  Work station unique number in network (0 .. 255)        │
  └─────────────────────────────────────────── ─────────────────────────────┘
 Default  location  of data  files and  <b>catalog</b> is in the  program directory.
 Data  files  dislocation can be changed in catalog. Catalog location and lo-
 cations of data files,  which catalog does not contain can be changed before
 starting FAND by setting <b>SET FANDATA</b>. Acces rights determination see <b>instal-</b>
 <b>ling network</b>.

 Installing program in  catalog, mark  <b>shared data files</b>  in the item "label"
 according to condition of sharing:

 ■ <b>all participants read only</b>     (program, .HLP)  file  has  DOS - attribute
                                  ReadOnly, it need not be written in catalog
                                  or label in catalog is empty.
                                     
 ■ <b>some of participants read only</b> - the file has '<b>#R</b>' mark in station catalog
 ■ Allowed <b>writing even reading</b>   - the file has '<b>#R</b>' mark in station catalog

 Shared  files definitions  cannot be changed!  Writing in file,  marked read
 only, is regarded as error and program is cancelled!
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t305">sharing FAND</a>      <a href="#t306">sharing program</a>        <a href="#t311">shared editing</a>
</pre>

<a name="t308"></a>
## SHARE

<pre>
                       <b>LAN - PROGRAM SHARE PARAMETERS</b>
   ──────────────────────────────────────────────────────────────────────────
   Some networks  require  starting the program  <b>SHARE</b> on server. To  achieve
   correct running the PC FAND,  devote yourself to setting parameters of the
   program:

   █ <b>SHARE /F:n /L:m</b>

   ■ <b>parameter /F:n</b> - area for the table of opened files. To define value of
                      this  parameter,  we recommend following method (SHARE
                      does not report  overflow and so memory might be over-
                      written):

                     n = 71 * FILES  71 = the longest MS DOS path + 11 Bytes
                               FILES =  The hihgest number of simultaneously
                                        opened files, written in CONFIG.SYS)

   ■ <b>parameter /L:m</b> - number  of locks  (file or record lock),  which  SHARE
                      can  run simultaneously. Parameter "m" value should be
                      double of the highest  number of simultaneously opened
                      files (eg. parameter FILES in the CONFIG.SYS).

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t309">installing network</a>            <a href="#t305">sharing FAND</a>     <a href="#t306">sharing program</a>
   <a href="#t307">sharing data files</a>
</pre>

<a name="t309"></a>
## installing network

<pre>
                           <b>LAN - INSTALLING NETWORK</b>
 ────────────────────────────────────────────────────────────────────────────
 <b>Software requirements :</b>
 ■ MS-DOS version 3.3 or later
 ■ Network  operating  system,  which  uses MS-DOS  open-share modes and lock
   Bytes even outside physical file range (most of available networks)

 <b>Installing network enviroment</b>:
 ■ Assign access rights <b>RWOCDM</b> (read/write/open/create/delete/modify attri-
   butes) to data file directory.
   Similarly for the PC FAND working files (FANDWORK.?$$, PRINTER.TXT).

   For fully shared existing files (# in catalog) - <b>RWO</b> will do.

 ■  Existing  shared  files,  which will be  only read from some of stations,
   (in catalog marked #R) - <b>RO</b>  rights will do. Modules FAND.*, (U)FANDHLP.*,
   data files for read only - with DOS-attribute RdOnly and debugged  (closed
   by password) program can belong in this group.

 ■  Program, which you want to install and to develop, requires <b>RWOM</b> rights.

   <b>Conclusion:</b>  Files with different  requirements  to access rights (FAND.*
               FANDWORK.*)  are usually located in the same directory. Then,
               the highest from them aserts. In larger networks (Novell), is
               worthwhile not to ignore these matters.

 ■ To install  sufficient  maximal  number of simultaneously opened "handles"
   for opened files,  and also maximal number of  simultaneously done  "lock-
   ings"  is necessary.  Some  networks  require starting program <b>SHARE</b> (part
   of  MS-DOS  operating  system)  with  corresponding  parameters on server;
   for other  is necessary  to follow  installation  manual ( Eg. for network
   <b>Novell</b>) is necessary to set  parameter "FILE HANDLES=" in SHELL.CFG file
   on server.
 ────────────────────────────────────────────────────────────────────────────
 program <a href="#t308">SHARE</a> parameters    <a href="#t305">sharing FAND</a>        <a href="#t306">sharing program</a>
 <a href="#t307">sharing data files</a>
</pre>

<a name="t310"></a>
## LAN concepts overview

<pre>
                            <b>LAN CONCEPTS OVERVIEW</b>
   ───────────────────────────────────────────────────────────────────────────

   <b>Exclusive</b>.. sole, exclucive file locking by one participant; other partici-
               pants are allowed no access to the file
               CAUTION - distinguish from <b>Excl</b> mode

   <b>Shared</b> .... shared simultaneous access by several participants to data file

   <b>File locking modes</b>..when working with shared files, the FAND uses  several
                       modes of locking file. The modes enable fluent transi-
                       tion between Shared and Exclusive modes.

   <b>Lock</b> ......During action, record of data file is locked.  Automatic record
              locking  is used for data  editors  coordination;  command <b>with
              locked</b> is available in procedure.

   <b>Deadlock</b>...program lockup,  freezing in  network is undesired  state, when
              participants block each other the access to a file (record). It
              does  not  occure  often,  but the risk of its  occurence rises
              with time, for what files (or records)  are locked. To release,
              press on one of the work stations <b>Ctrl-Break</b> keys.

   <b>Net delay</b>..time period for repeated testing,  whether file is still locked.
              (time in seconds).  The  constant  can be  installed by program
              FANDINST.

   <b>Refresh</b> ... refreshing period of screen refresh during shared editing (ti-
               me in seconds ).  The  constant  can be  installed  by program
               FANDINST. To refresh the screen immediately press <b>CTRL-F2</b>.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t203">installing constants</a>
</pre>

<a name="t311"></a>
## shared editing

<pre>
                              <b>SHARED EDITING</b>
 ────────────────────────────────────────────────────────────────────────────
 A user of a network  application  should be aware of some  differences  from
 running mono-application. If you are aware of some facts, you can  influence
 data throughput of the network. In most cases, concurrent work feature shows
 during editing.

 ■ Even starting data  editor means setting up at least minimal level of file
   locking  for  other  participants.  It  can mean  limiting  of their work,
   slowing  down or total  blocking - it depends on  features of  competitive
   actions.  For that reason  it is better to exit editing,  when you are not
   working.

 ■ Programmer can ensure, that editing  automatically ends after defined time
   interval,  if user did not press any key.  Before that, operator is warned
   three times by sound.

 ■ File locking is automatically "enhanced" by actions  like record changing,
   writing or deleting - it depens on the operation type.
 ■ Data editor automatically reads shared data from disc in regular intervals
   - so called <b>refresh</b>. It it the only way to detect other participants acti-
   ons. Even plain refresh  burdens network (server).  It is the reason of ad-
   justing refresh according to real need. It is possible to set it commonly,
   by the  FANDINST program ( see <a href="#t203">installing constants</a> ), or  programmer  can
   change it  individually, at every editor call. Acceptable values depend on
   the number of work stations and network and server efficiency.

 ■ If more stations are in network, and if excessive file  blocking  occurs,
   an improvement can be achieved by setting different interval for repeated
   attempts to access - <b>net delay</b>.

 ■ When <b>editing subset</b>, screen scrolling (new records, deleting) does not oc-
   cure, because of other participants working. If another work station chan-
   ges a key,  it can happen  that report search  by key <b>F3</b> is succesful, but
   access to the recors is denied.

 ■ Refresh of screen or subset according to the updated state can be achieved
   by pressing <b>Ctrl-F2</b> key (suppressed when editing by dynamic index).

 ■ If some  file  records  are displayed with  different color, it means that
   they  were  deleted from  other work station. ( Press Ctrl-F2, if you want
   update).
 ────────────────────────────────────────────────────────────────────────────
 <a href="#t310">LAN concepts overview</a>
</pre>
