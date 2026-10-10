# Help for messages and menus

<a name="t4"></a>
## FM0014_1

<pre>
{ Starting debugged program in user's mode}
                         <b> EXECUTING PROGRAM</b>
 ────────────────────────────────────────────────────────────────────────────
 After selecting  from disk, program  is compiled  and started. <b>Communication</b>
 rules of running program:
 <a href="topics.md#t192">selecting from menu</a> .......... working with menu
 <a href="topics.md#t193">selecting from list</a> .......... selecting program, file, file items etc.
 <a href="topics.md#t194">help</a> ......................... using help screens
 <a href="topics.md#t208">text editor</a> .................. editing text
 <a href="topics.md#t224">data editor</a> .................. entering, modifying and deleting data
 <a href="topics.md#t228">automatic report</a> ............. report called from data editor
 <a href="topics.md#t195">PC FAND main menu</a> ............ menu items at PC FAND start
 <a href="topics.md#t48">keyboards</a> .................... keyboard layouts

 <a href="topics.md#t199">installation</a> ................. installing system, instalation program

 You can start program directly from operating system using command:

 ■ FAND <b>ProgramName</b>
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t6"></a>
## FM0014_2

*Also:* `installing program`

<pre>
{ List of authorized users and program catalog}
                          <b>INSTALLING PROGRAM</b>
 ────────────────────────────────────────────────────────────────────────────
 You can  install  project optional extension by selecting <b>Installing program</b>
 from the PC FAND main menu  :

 <a href="topics.md#t233">catalog</a> .............. location outside the program directory,
                        shared files in LAN
                        multidiskette backup copies
                        generation files
 <a href="#t18">list of users</a> .........list of authorized users and their access rights

 The whole installation  can be  protected from  unprofessional interference.
 The <b>password</b> must be entered twice, for safety reasons.
 ───────────────────────────────────────────────────────────────────────────
</pre>

<a name="t7"></a>
## FM0014_3

<pre>
{ Editing text file on disk or diskette by text editor}
                              <b>EDITING TEXT</b>
 ──────────────────────────────────────────────────────────────────────────────
 You can start  <b>Text editor</b> from the PC FAND main menu.

 <a href="topics.md#t209">cursor movements</a> ...... moving the cursor through text
 <a href="topics.md#t210">editing text</a> .......... inserting or deleting characters, words, lines etc.
 <a href="topics.md#t214">blocks</a>................. block definition, copying, moving, deleting etc.
 <a href="topics.md#t216">switches</a> .............. text editor parameters and their setting
 <a href="topics.md#t217">formatting text</a> ........... formatting paragraphs, alignment
 <a href="topics.md#t218">searching</a> ................ searching and replacing string in text
 <a href="topics.md#t220">printing text</a> ......... printing whole text or its part
 <a href="topics.md#t222">colors and fonts</a> ...... color and font switches
 <a href="topics.md#t212">editor facilities</a> ..... box drawing, diacritics, calculator

 Text editor can be started from the command line. Type command:
 
 ■ FAND TextName <b>T</b>
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t8"></a>
## FM0014_4

<pre>
{Jump into operating system}
                          <b>JUMP INTO MS DOS</b>
 ──────────────────────────────────────────────────────────────────────────

 After selecting <b>Dos</b> from the PC FAND main menu follows jump into MS DOS.
 There you can start other programs (if it allows free RAM capacity)
 To return to PC FAND type <b>EXIT</b> command.

 ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t9"></a>
## FM0014_5

<pre>
{ Exit PC FAND}
</pre>

<a name="t11"></a>
## FM004_1

<pre>
{ automatic report: plain copy of file records}
</pre>

<a name="t12"></a>
## FM004_2

<pre>
{automatic record: record copies in addition with totals of "summarized items"}
</pre>

<a name="t13"></a>
## FM004_3

<pre>
{ automatic report: only totals of "summarized items"}
</pre>

<a name="t14"></a>
## FM004_4

<pre>
{automatic record: List of records, which do not meet logical condition}
</pre>

<a name="t16"></a>
## FM0008_1

<pre>
{ Creating and editing program catalog}
</pre>

<a name="t18"></a>
## FM0008_2

*Also:* `list of users`

<pre>
{ List of authorized users and their access rights }
                         <b>LIST OF USERS - chapter U</b>
 ────────────────────────────────────────────────────────────────────────────
 <b>List of users</b> is a text, containing a list of authorized program users. Eve-
 ry user  is assigned  by <b>name</b>  (text), <b>code</b> (number), <b>password</b>  and list  of
 <b>access rights</b>.
 

 █ '<b>Name</b>', <b>Code</b>, <b>Password</b>, (<b>AccessRightList</b>); ... one item of the list
 ■ <b>Name</b>:     current user name for identification in project
 ■ <b>Code</b>:     current user number for identification in project
 ■ <b>Password</b>: user's password  to apply for program start
 ■ <b>List</b>:     list of numbers (1..255) for differentiation of access rights

 If  program  contains chapter U, the user must type one of the allowed pass-
 words. Program  will not start  otherwise. After typing correct password, PC
 FAND sets <b>Name</b>, <b>Code</b>, <b>Access right</b> of current user. According to them, it is 
 possible to change  program running in  further processing.  Chapter U cont-
 rols  even  limitations in  <b>navigation through database</b>  according to <b>user's</b>
 <b>wiews</b>.  <b>Empty</b>  <b>chapter U</b>  acts as  non-existing  chapter  (it is possible to
 install additionaly from PC FAND main menu without any changes in program).
 ────────────────────────────────────────────────────────────────────────────
 <a href="topics.md#t19">░ list of users</a>
</pre>

<a name="t20"></a>
## FM0008_3

<pre>
{ Loking catalog and list of users by password }
</pre>

<a name="t25"></a>
## FM0100_1

*Also:* `FM0101_1`, `FM0102_1`, `FM0103_1`

<pre>
{ Subset of records, which meet a defined condition}
</pre>

<a name="t29"></a>
## FM0100_2

*Also:* `FM0101_2`, `FM0102_2`, `FM0103_2`

<pre>
{ When entering new records, the previous item is offered again}
</pre>

<a name="t33"></a>
## FM0100_3

*Also:* `FM0101_3`, `FM0102_3`, `FM0103_3`

<pre>
{ Setting Tab on current item}
</pre>

<a name="t35"></a>
## FM0100_4

*Also:* `FM0102_4`

<pre>
{ Switching off additive changes to superior files }
</pre>

<a name="t37"></a>
## FM0100_5

*Also:* `FM0103_4`

<pre>
{ Switching off logical checks at data editing and entering}
</pre>

<a name="t40"></a>
## FM0105_1

*Also:* `FM0106_1`

<pre>
{ Automatic report}
</pre>

<a name="t42"></a>
## FM0105_2

*Also:* `FM0106_2`

<pre>
{ Executing logical checks from cursor position to the end of the file}
</pre>

<a name="t44"></a>
## FM0105_3

*Also:* `FM0106_3`

<pre>
{ Entering logical condition to select subset of file records}
</pre>

<a name="t46"></a>
## FM0105_5

*Also:* `FM0106_4`

<pre>
{Sorting file by selected items}
</pre>

<a name="t50"></a>
## FM0045_1

*Also:* `standard keyboard`

<pre>
{Keyboard without changes according to DOS support or another resident program}
                             <b>STANDARD KEYBOARD</b>
 ────────────────────────────────────────────────────────────────────────────
   ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐                
   │~  │!  │@  │#  │$  │%  │^  │&amp;  │*  │(  │)  │_  │+  │|  │BS │
   │`  │1  │2  │3  │4  │5  │6  │7  │8  │9  │0  │-  │=  │\  │   │
   ├───┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴───┤                
   │ Tab │   │   │   │   │   │   │   │   │   │   │{  │}  │     │
   │     │   │   │   │   │   │   │   │   │   │   │[  │]  │     │
   ├─────┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┘     │                
   │ Caps │   │   │   │   │   │   │   │   │   │   │   │ Enter  │                
   │ Lock │   │   │   │   │   │   │   │   │   │   │   │        │                
   ├──────┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴────────┤                
   │ Shift  │   │   │   │   │   │   │   │   │   │   │  Shift   │                
   │        │   │   │   │   │   │   │   │   │   │   │          │                
   ├─────┬──┴┬──┴──┬┴───┴───┴───┴───┴───┴───┴──┬┴───┴┬───┬─────┤                
   │Ctrl │   │ Alt │                           │ Alt │   │Ctrl │                
   │     │   │     │                           │     │   │     │                
   └─────┘   └─────┴───────────────────────────┴─────┘   └─────┘                
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t52"></a>
## FM0045_2

*Also:* `Czech keyboard`

<pre>
{ Czech typewriter keyboard }
                             <b>CZECH KEYBOARD</b>
 ────────────────────────────────────────────────────────────────────────────
   ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐                
   │;  │1  │2  │3  │4  │5  │6  │7  │8  │9  │0  │%  │hoo│|  │BS │
   │`  │+  │ě  │š  │č  │ř  │ž  │ý  │á  │í  │é  │=  │com│\  │   │
   ├───┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴───┤                
   │ Tab │   │   │   │   │Z  │   │   │   │   │   │/  │(  │     │
   │     │   │   │   │   │z  │   │   │   │   │   │ú  │)  │     │
   ├─────┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┘     │                
   │ Caps │   │   │   │   │   │   │   │   │   │"  │!  │ Enter  │
   │ Lock │   │   │   │   │   │   │   │   │   │ů  │§  │        │
   ├──────┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴────────┤                
   │ Shift  │Y  │   │   │   │   │   │   │?  │:  │_  │  Shift   │
   │        │y  │   │   │   │   │   │   │,  │.  │-  │          │
   ├─────┬──┴┬──┴──┬┴───┴───┴───┴───┴───┴───┴──┬┴───┴┬───┬─────┤                
   │Ctrl │   │ Alt │                           │ Alt │   │Ctrl │                
   │     │   │     │                           │     │   │     │                
   └─────┘   └─────┴───────────────────────────┴─────┘   └─────┘                
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t54"></a>
## FM0045_3

*Also:* `Czech amateur keyboard`

<pre>
{Programmer's keyb.: changes only in the top row of keys and hook/lenght mark}
                            <b>CZECH AMATEUR KEYBOARD</b>
 ────────────────────────────────────────────────────────────────────────────
   ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
   │   │!  │@  │#  │$  │%  │^  │&amp;  │*  │(  │)  │   │hoo│   │BS │
   │   │+  │ě  │š  │č  │ř  │ž  │ý  │á  │í  │é  │   │len│   │   │
   ├───┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴───┤                
   │ Tab │   │   │   │   │   │   │   │   │   │   │   │   │     │
   │     │   │   │   │   │   │   │   │   │   │   │   │   │     │
   ├─────┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┘     │                
   │ Caps │   │   │   │   │   │   │   │   │   │   │   │ Enter  │
   │ Lock │   │   │   │   │   │   │   │   │   │   │   │        │                
   ├──────┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴────────┤                
   │ Shift  │   │   │   │   │   │   │   │   │   │   │  Shift   │
   │        │   │   │   │   │   │   │   │   │   │   │          │                
   ├─────┬──┴┬──┴──┬┴───┴───┴───┴───┴───┴───┴──┬┴───┴┬───┬─────┤                
   │Ctrl │   │ Alt │                           │ Alt │   │Ctrl │                
   │     │   │     │                           │     │   │     │                
   └─────┘   └─────┴───────────────────────────┴─────┘   └─────┘
   characters <b>+</b> and <b>=</b> you get by double click the corresponding key
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t56"></a>
## FM0045_4

*Also:* `Slovak keyboard`

<pre>
{ Slovak typewriter keyboard }
                        <b>SLOVAK KEYBOARD</b>
 ────────────────────────────────────────────────────────────────────────────
   ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
   │ ; │1  │2  │3  │4  │5  │6  │7  │8  │9  │0  │%  │hoo│   │BS │
   │ ` │+  │ĺ  │š  │č  │ť  │ž  │ý  │á  │í  │é  │=  │com│   │   │
   ├───┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴───┤                
   │ Tab │   │   │   │   │   │Z  │   │   │   │   │/  │(  │     │
   │     │   │   │   │   │   │z  │   │   │   │   │ú  │ä  │     │
   ├─────┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┘     │                
   │ Caps │   │   │   │   │   │   │   │   │   │"  │!  │ Enter  │
   │ Lock │   │   │   │   │   │   │   │   │   │ô  │)  │        │
   ├──────┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴────────┤                
   │ Shift  │Y  │   │   │   │   │   │   │?  │:  │_  │  Shift   │
   │        │y  │   │   │   │   │   │   │,  │.  │-  │          │
   ├─────┬──┴┬──┴──┬┴───┴───┴───┴───┴───┴───┴──┬┴───┴┬───┬─────┤                
   │Ctrl │   │ Alt │                           │ Alt │   │Ctrl │                
   │     │   │     │                           │     │   │     │                
   └─────┘   └─────┴───────────────────────────┴─────┘   └─────┘
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t58"></a>
## FM0045_5

*Also:* `German amateur keyboard`

<pre>
{ Keyboard for German text writing}
                          <b>GERMAN AMATEUR KEYBOARD</b>
 ────────────────────────────────────────────────────────────────────────────
   ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐                
   │'  │!  │"  │#  │$  │%  │^  │&amp;  │*  │(  │)  │/  │+  │|  │BS │
   │`  │1  │2  │3  │4  │5  │6  │7  │8  │9  │0  │ß  │=  │\  │   │
   ├───┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴───┤                
   │ Tab │   │   │   │   │   │   │   │   │   │   │Ü  │}  │     │
   │     │   │   │   │   │   │   │   │   │   │   │ü  │]  │     │
   ├─────┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┘     │                
   │ Caps │   │   │   │   │   │   │   │   │   │Ö  │Ä  │ Enter  │
   │ Lock │   │   │   │   │   │   │   │   │   │ö  │ä  │        │
   ├──────┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴────────┤                
   │ Shift  │   │   │   │   │   │   │   │;  │:  │?  │  Shift   │
   │        │   │   │   │   │   │   │   │,  │.  │-  │          │
   ├─────┬──┴┬──┴──┬┴───┴───┴───┴───┴───┴───┴──┬┴───┴┬───┬─────┤                
   │Ctrl │   │ Alt │                           │ Alt │   │Ctrl │                
   │     │   │     │                           │     │   │     │                
   └─────┘   └─────┴───────────────────────────┴─────┘   └─────┘                
 ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t59"></a>
## FM0046_*

<pre>
{ Interactive printer selection}
                           <b>SELECTING PRINTER</b>
 ────────────────────────────────────────────────────────────────────────────

 The  <a href="topics.md#t200">Installation program</a> FANDINST.EXE enables you to install up to 10 prin-
 ter models.  When installing printer,  determine what port will be print di-
 rected to. ( LPT1, LPT2,... )

 Menu for printer  selection exactly  corresponds to the  definition from the
 installation  program ( stored in the file FAND.CFG ). After starting the PC
 FAND, the first defined printer will be always  activated. The order is pos-
 sible to change by the installation program.
 ────────────────────────────────────────────────────────────────────────────
  <a href="topics.md#t202">installing printer</a>
</pre>

<a name="t61"></a>
## FM0220_1

<pre>
{ Entering file and information on displaying           (Recno,NRec,Min,Max)}
</pre>

<a name="t62"></a>
## FM0220_2

<pre>
{Graph type,colors,fill,shape,polynomial grade     (Type,Fill,Width,RGB,GrPoly)}
</pre>

<a name="t63"></a>
## FM0220_3

<pre>
{Switching grid on/off, window definition, output into PCX file    (Grid,ww)}
</pre>

<a name="t64"></a>
## FM0220_4

<pre>
{ Setting print parameters   (Print)}
</pre>

<a name="t65"></a>
## FM0220_5

<pre>
{ Input and output of parameters into file .FGP}
</pre>

<a name="t66"></a>
## FM0220_6

<pre>
{ Graph heading and axis description          (Head, HeadX,HeadY,HeadZ, DirX)}
</pre>

<a name="t67"></a>
## FM0220_7

<pre>
{ Defining texts and text windows in graph                      (Txt,TxtWin)}
</pre>

<a name="t68"></a>
## FM0220_8

<pre>
{ Graph display }
</pre>

<a name="t70"></a>
## FM0221_1

<pre>
{ 2-dimensional bar graph of one or two items                 (Type='2DBAR')}
</pre>

<a name="t71"></a>
## FM0221_2

<pre>
{ Circle or pie graph                                        (Type='CIRCLE')}
</pre>

<a name="t72"></a>
## FM0221_3

<pre>
{ Line approximation graph                                   (Type='APPROX')}
</pre>

<a name="t73"></a>
## FM0221_4

<pre>
{ Polynomial regression graph                               (Type='POLYREG')}
</pre>

<a name="t74"></a>
## FM0221_5

<pre>
{ 2-D bar graph for comparison up to 10 items                 (Type='GROUP')}
</pre>

<a name="t75"></a>
## FM0221_6

<pre>
{ Line graph for comparison up to 10 items                (Type='GROUPLINE')}
</pre>

<a name="t76"></a>
## FM0221_7

<pre>
{ Stacked bar graph for comparison up to 10 items           Type='STACKBAR')}
</pre>

<a name="t77"></a>
## FM0221_8

<pre>
{ 3D bar graph                                                 TYPE='3DBAR')}
</pre>

<a name="t78"></a>
## FM0221_9

<pre>
{ 3D line graph                                              (Type='3DLINE')}
</pre>

<a name="t80"></a>
## FM0229_1

<pre>
{ Entering input file and information on displaying }
</pre>

<a name="t81"></a>
## FM0229_2

<pre>
{ Consecutive number of the first record and number of records   (Recno,NRec)}
</pre>

<a name="t82"></a>
## FM0229_3

<pre>
{ Entering maximal and minimal scaling factor on Y-axis            (MIN,MAX)}
</pre>

<a name="t83"></a>
## FM0230_1

<pre>
{ Information on displaying }
</pre>

<a name="t84"></a>
## FM0230_2

<pre>
{ Number of the first record and quantity of records           (Recno,NRec)}
</pre>

<a name="t85"></a>
## FM0230_3

<pre>
{ Entering maximal and minimal scaling factor on Y-axis            (MIN,MAX)}
</pre>

<a name="t87"></a>
## FM0231_1

<pre>
{ Graph type                                                          (Type)}
</pre>

<a name="t88"></a>
## FM0231_2

<pre>
{ Object color and hatching                                (Fill-1.position)}
</pre>

<a name="t89"></a>
## FM0231_3

<pre>
{ Shape of objects                                         (Fill-2.position)}
</pre>

<a name="t90"></a>
## FM0231_4

<pre>
{ Width of objects                                                  (Width)}
</pre>

<a name="t91"></a>
## FM0231_5

<pre>
{ Approximation polynomial grade for polynomial regression          (GrPoly)}
</pre>

<a name="t92"></a>
## FM0231_6

<pre>
{ Change in order of object colors                                 (Palette)}
</pre>

<a name="t93"></a>
## FM0231_7

<pre>
{ Defining colors by setting components of R G B ratio                 (RGB)}
</pre>

<a name="t95"></a>
## FM0233_1

<pre>
{ Only outlines of objects                                       (Fill='N.')}
</pre>

<a name="t96"></a>
## FM0233_2

<pre>
{ Color displaying of objects - whole filling                    (Fill='C.')}
</pre>

<a name="t97"></a>
## FM0233_3

<pre>
{ Monochromatic pattern of objects                               (Fill='F.')}
</pre>

<a name="t98"></a>
## FM0233_4

<pre>
{ Color hatching of objects                                       (Fill='M.')}
</pre>

<a name="t99"></a>
## FM0233_5

<pre>
{ Objects drawn monochromaticaly                                 (Fill='O.')}
</pre>

<a name="t101"></a>
## FM0235_1

<pre>
{ 3D object shape -  cylinder (3DBAR) or ribbon (3DLINE)         (Fill='.1')}
</pre>

<a name="t102"></a>
## FM0235_2

<pre>
{ 3D object shape - pyramid (3DBAR) or area (3DLINE)               Fill='.2'}
</pre>

<a name="t103"></a>
## FM0235_3

<pre>
{ 3D object shape - bar (3DBAR), scene (3DLINE)                    Fill='.3'}
</pre>

<a name="t105"></a>
## FM0237_1

<pre>
{ 3D object size - 10 % of defined volume                         (Width=10)}
</pre>

<a name="t106"></a>
## FM0237_2

<pre>
{ 3D object size - 20 % of defined volume                         (Width=20)}
</pre>

<a name="t107"></a>
## FM0237_3

<pre>
{ 3D object size - 30 % of defined volume                         (Width=30)}
</pre>

<a name="t108"></a>
## FM0237_4

<pre>
{ 3D object size - 40 % of defined volume                         (Width=40)}
</pre>

<a name="t109"></a>
## FM0237_5

<pre>
{ 3D object size -  50 % of defined volume                         (Width=50)}
</pre>

<a name="t110"></a>
## FM0237_6

<pre>
{ 3D object size -  60 % of defined volume                        (Width=60)}
</pre>

<a name="t111"></a>
## FM0237_7

<pre>
{ 3D object size - 70 % of defined volume                         (Width=70)}
</pre>

<a name="t112"></a>
## FM0237_8

<pre>
{ 3D object size - 80 % of defined volume                         (Width=80)}
</pre>

<a name="t113"></a>
## FM0237_9

<pre>
{ 3D object size - 90 % of defined volume                         (Width=90)}
</pre>

<a name="t114"></a>
## FM0237_10

<pre>
{ 3D object size - 100 % of defined volume                       (Width=100)}
</pre>

<a name="t116"></a>
## FM0241_1

<pre>
{ Graph title                                                         (Head)}
</pre>

<a name="t117"></a>
## FM0241_2

<pre>
{ X-axis description                                                 (HeadX)}
</pre>

<a name="t118"></a>
## FM0241_3

<pre>
{ Y-axis description                                                 (HeadY)}
</pre>

<a name="t119"></a>
## FM0241_4

<pre>
{ Z-axis items names                                                (HeadZn)}
</pre>

<a name="t120"></a>
## FM0241_5

<pre>
{ X-axis direction of description                                     (DirX)}
</pre>

<a name="t122"></a>
## FM0247_1

<pre>
{ Z-axis null item name                                             (HeadZ0)}
</pre>

<a name="t123"></a>
## FM0247_2

<pre>
{ Z-axis 1st item name                                              (HeadZ1)}
</pre>

<a name="t124"></a>
## FM0247_3

<pre>
{ Z-axis 2nd item name                                              (HeadZ2)}
</pre>

<a name="t125"></a>
## FM0247_4

<pre>
{ Z-axis 3rd item name                                              (HeadZ3)}
</pre>

<a name="t126"></a>
## FM0247_5

<pre>
{ Z-axis 4th item name                                              (HeadZ4)}
</pre>

<a name="t127"></a>
## FM0247_6

<pre>
{ Z-axis 5th item name                                              (HeadZ5)}
</pre>

<a name="t128"></a>
## FM0247_7

<pre>
{ Z-axis 6th item name                                              (HeadZ6)}
</pre>

<a name="t129"></a>
## FM0247_8

<pre>
{ Z-axis 7th item name                                              (HeadZ7)}
</pre>

<a name="t130"></a>
## FM0247_9

<pre>
{ Z-axis 8th item name                                              (HeadZ8)}
</pre>

<a name="t131"></a>
## FM0247_10

<pre>
{ Z-axis 9th item name                                              (HeadZ9)}
</pre>

<a name="t133"></a>
## FM0260_1

<pre>
{ switching on/off grid, displaying values in graph                   (Grid)}
</pre>

<a name="t134"></a>
## FM0260_2

<pre>
{ File name for writing graph into .PCX format file}
</pre>

<a name="t135"></a>
## FM0260_3

<pre>
{ Graph window and box definition                                      (ww) }
</pre>

<a name="t137"></a>
## FM0262_1

<pre>
{ Horizontal description on X-axis                                (DirX='H')}
</pre>

<a name="t138"></a>
## FM0262_2

<pre>
{ Vertical description on X-axis                                  (DirX='V')}
</pre>

<a name="t139"></a>
## FM0262_3

<pre>
{ Inside annotations of objects                                   (DirX='I')}
</pre>

<a name="t141"></a>
## FM0264_1

<pre>
{ Grid switched on                                                (Grid='Y')}
</pre>

<a name="t142"></a>
## FM0264_2

<pre>
{ Grid switched off                                               (Grid='N')}
</pre>

<a name="t143"></a>
## FM0264_3

<pre>
{ Writing values into graph                                       (Grid='H')}
</pre>

<a name="t144"></a>
## FM0264_4

<pre>
{ Writing values into graph                             (Grid='H:n', n=0..9)}
</pre>

<a name="t146"></a>
## FM0268_1

<pre>
{ Entering menu }
</pre>

<a name="t147"></a>
## FM0268_2

<pre>
{ Switched on }
</pre>

<a name="t148"></a>
## FM0268_3

<pre>
{ Switched off }
</pre>

<a name="t150"></a>
## FM0272_1

<pre>
{ Entering .PCX file name for writing}
</pre>

<a name="t151"></a>
## FM0272_2

<pre>
{ suppressed writing into .PCX file }
</pre>

<a name="t153"></a>
## FM0275_1

<pre>
{ Print quality                                         (1st Print position)}
</pre>

<a name="t154"></a>
## FM0275_2

<pre>
{ Print graph orientation                               (2nd Print position)}
</pre>

<a name="t155"></a>
## FM0275_3

<pre>
{ Print density                                         (3rd Print position)}
</pre>

<a name="t156"></a>
## FM0275_4

<pre>
{ Graph left margin                                     (4th Print position)}
</pre>

<a name="t157"></a>
## FM0275_5

<pre>
{ Graph top margin                                      (5th Print position)}
</pre>

<a name="t158"></a>
## FM0275_6

<pre>
{ Form feed after printing                              (6th Print position)}
</pre>

<a name="t159"></a>
## FM0275_7

<pre>
{ Delay at graph displaying - without printing                 (Print=PAU..)}
</pre>

<a name="t161"></a>
## FM0277_*

<pre>
{ --- }
</pre>

<a name="t162"></a>
## FM0278_*

<pre>
{ --- }
</pre>

<a name="t163"></a>
## FM0280_*

<pre>
{ --- }
</pre>

<a name="t165"></a>
## FM0282_1

<pre>
{ Print density 60 dpi -  wide print }
</pre>

<a name="t166"></a>
## FM0282_2

<pre>
{ Print density 120 dpi - medium width print}
</pre>

<a name="t167"></a>
## FM0282_3

<pre>
{ Print density 240 dpi - narrow print }
</pre>

<a name="t169"></a>
## FM0283_*

<pre>
{ --- }
</pre>

<a name="t170"></a>
## FM0284_*

<pre>
{ --- }
</pre>

<a name="t171"></a>
## FM0286_*

<pre>
{ --- }
</pre>

<a name="t172"></a>
## FM0287_*

<pre>
{ --- }
</pre>

<a name="t173"></a>
## FM0289_*

<pre>
{ --- }
</pre>

<a name="t175"></a>
## FM0290_1

<pre>
{ Entering file name for parameter reading or writing}
</pre>

<a name="t176"></a>
## FM0290_2

<pre>
{ Reading file of parameters }
</pre>

<a name="t177"></a>
## FM0290_3

<pre>
{ Writing into file of parameters }
</pre>

<a name="t179"></a>
## FM0296_1

<pre>
{ Graph annotation texts                                               (Txt)}
</pre>

<a name="t180"></a>
## FM0296_2

<pre>
{ Text windows, displayed in graph                                  (TxtWin)}
</pre>

<a name="t182"></a>
## FM0328_1

<pre>
{ Dark colors definition }
</pre>

<a name="t183"></a>
## FM0328_2

<pre>
{ Light colors definition }
</pre>

<a name="t184"></a>
## FM0328_3

<pre>
{ Help to find color component ratio of desired color }
</pre>

<a name="t186"></a>
## FM0330_*

<pre>
{ --- }
</pre>

<a name="t187"></a>
## FM0331_*

<pre>
{ --- }
</pre>
