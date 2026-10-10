#!/usr/bin/env python3
"""Convert PC FAND's help file to Markdown (docs/help, docs/help-user*).

    fandhelp2md.py help.xml OUTDIR [--user] [--en]

--user: the user runtime's help (help/UFANDHLP): all topics in one file.
--en:   the English help (help/angl/UFANDHLP): English labels and file names.

help.xml is the help file exported by the Free Pascal port of fand
(its --export, not published yet) from a task whose one chapter declares
the file,

    mkdir t t/src && cp help/FANDHLP.000 help/FANDHLP.T00 t/
    printf 'Name:A,35; Text:T;\n' > t/src/001-F-FANDHLP.txt
    printf 'begin end;\n' > t/src/002-P-main.txt
    cd t && fand t --source-in src && fand t --export help.xml

The help is a FAND data file (help/FANDHLP.000/.T00): one record per topic,
Name A,35 and Text T, the text in the Kamenicky code page. In the text ^B
toggles a highlight, ^D a second one, and ^S..^S is a link to the topic of
that name. Links resolve as the runtime's FindHelpRecNr does: names are
masks (* ?) matched without diacritics or case, the first match wins, and a
name-only record falls through to the next record with text."""
import html, os, re, sys, unicodedata
KAM = "ČüéďäĎŤčěĚĹÍľĺÄÁÉžŽôöÓůÚýÖÜŠĽÝŘťáíóúňŇŮÔšřŕŔ¼§«»"
ARROWS = {0x18: '↑', 0x19: '↓', 0x1A: '→', 0x1B: '←', 0x10: '►', 0x11: '◄', 0x1E: '▲', 0x1F: '▼'}

def to_codes(s):
    """the export's UTF-8 (CP852 reading + control pictures) -> list of byte codes"""
    out = []
    s = s.replace('\r\n', '\n').replace('\r', '\n')
    for ch in s:
        o = ord(ch)
        if 0x2400 <= o <= 0x241F: out.append(o - 0x2400)
        elif ch == '\n': out.append(10)
        else: out.extend(ch.encode('cp852', 'replace'))
    return out

def kam_char(c):
    if 0x80 <= c < 0xB0: return KAM[c - 0x80]
    if c >= 0xB0: return bytes([c]).decode('cp437')
    return chr(c)

def plain(s):
    return ''.join(kam_char(c) for c in to_codes(s) if c >= 32 or c == 10)

def fold(s):
    s = unicodedata.normalize('NFD', s)
    return ''.join(ch for ch in s if not unicodedata.combining(ch)).upper()

def mask_match(mask, text):
    rx = ''.join('.*' if c == '*' else '.' if c == '?' else re.escape(c) for c in mask)
    return re.fullmatch(rx, text, re.S) is not None

LABELS = {
 'cz': {'intro': 'Nápověda programátorské verze PC FANDu 4.2 (soubor `help/FANDHLP`), '
                 'převedená pro čtení na GitHubu. Obsah je původní text nápovědy firmy ALIS; '
                 'odkazy vedou mezi hesly jako v nápovědě ve FANDu.',
        'intro_user': 'Nápověda uživatelské verze PC FANDu 4.2 (soubor `help/UFANDHLP`), '
                      'převedená pro čtení na GitHubu. Obsah je původní text nápovědy firmy ALIS; '
                      'odkazy vedou mezi hesly jako v nápovědě ve FANDu.',
        'title_user': 'Nápověda uživatelské verze PC FANDu 4.2',
        'oddily': 'Oddíly', 'obsah': 'Obsah', 'bez': '(bez názvu)', 'tez': 'Též',
        'index': 'Rejstřík hesel', 'rejstrik': 'Rejstřík podle písmen',
        'kontext': 'Kontextová nápověda k hlášením a nabídkám', 'ostatni': 'Ostatní hesla',
        'hesla': 'Hesla'},
 'en': {'intro': '', 'intro_user': "PC FAND's user runtime help in English (file `help/angl/UFANDHLP`, "
                      "headed as version 3.3), converted for reading on GitHub. The text is ALIS's "
                      "original help; links lead between topics as in FAND's help.",
        'title_user': "PC FAND help - user's version",
        'oddily': 'Sections', 'obsah': 'Contents', 'bez': '(untitled)', 'tez': 'Also',
        'index': 'Index of topics', 'rejstrik': 'Index by letter',
        'kontext': 'Help for messages and menus', 'ostatni': 'Other topics', 'hesla': 'Topics'},
}

def main(src, outdir, user=False, en=False):
    x = open(src, encoding='utf-8').read()
    rows = [(html.unescape(n), html.unescape(t)) for n, t in
            re.findall(r'<field name="Name">(.*?)</field>\s*<field name="Text">(.*?)</field>', x, re.S)]
    names = [plain(n).rstrip(' ') for n, _ in rows]
    texts = [t for _, t in rows]
    has_text = [bool(t.strip()) for t in texts]
    fnames = [fold(n) for n in names]

    def resolve(link):
        f = fold(link)
        for i, n in enumerate(fnames):
            if n and mask_match(n, f):
                while i < len(rows) - 1 and not has_text[i]: i += 1
                return i if has_text[i] else None
        return None

    # topics = records with text; their aliases = the name-only records just before
    topics = [i for i in range(len(rows)) if has_text[i]]
    aliases = {}
    for i in topics:
        a = []; k = i - 1
        while k >= 0 and not has_text[k] and names[k]: a.insert(0, names[k]); k -= 1
        aliases[i] = a

    # files: groups of the root page's links; each topic goes to the group
    # whose start pages reach it in the fewest links (all groups at once)
    root = next(i for i in topics if names[i] == 'root')
    def links_of(i):
        return re.findall('\x13([^\x13]*)\x13', ''.join(map(chr, to_codes(texts[i]))))
    def lab(l): return ''.join(kam_char(ord(c)) for c in l)
    GROUPS = [
      ('prostredi', 'Prostředí, katalog, instalace', ['Nápověda','Projektantské prostředí','Katalog','Instalace','Lokální sítě','Rok 2000']),
      ('rozhrani', 'Uživatelské rozhraní', ['Komunikace','Klávesnice','Myš','Tabulky znaků','Grafy']),
      ('editory', 'Textový a datový editor', ['Textový editor','Datový editor']),
      ('soubory', 'Deklarace souboru (kapitola F)', ['deklarace souboru']),
      ('formular-sestava', 'Formulář, sestava, transformace (E, R, M)', ['formulář','sestava','transformace']),
      ('procedury', 'Procedury (kapitola P) a jejich příkazy', ['procedura','Příkazy procedury']),
      ('jazyk', 'Jazyk: identifikátory, výrazy, funkce', ['Identifikátor','Interní proměnné','Výrazy','Standardní funkce','Operátory','funkce','Syntaktické diagramy']),
      ('dalsi-kapitoly', 'Uživatelé, vkládané texty, PROLOG (U, I, L)', ['seznam uživatelů','vkládané texty','PROLOG']),
      ('sql', 'SQL a ODBC', ['SQL','ODBC']),
    ]
    section_of = {root: 'README'}
    frontier = []
    for g, _, labels in GROUPS:
        for l in labels:
            t = resolve(l)
            if t is not None and t not in section_of: section_of[t] = g; frontier.append(t)
    # the letter pages of the index: their own file, never a start
    for l in links_of(root):
        t = resolve(lab(l))
        if t is not None and t not in section_of and len(names[t].strip()) <= 2:
            section_of[t] = 'rejstrik'
    while frontier:
        nxt = []
        for u in frontier:
            for l2 in links_of(u):
                v = resolve(lab(l2))
                if v is not None and v not in section_of and not names[v].startswith('FM'):
                    section_of[v] = section_of[u]; nxt.append(v)
        frontier = nxt
    for i in topics:
        if i not in section_of:
            section_of[i] = 'kontextova-napoveda' if names[i].startswith('FM') else 'ostatni'
    if user:
        for i in topics:
            if section_of[i] not in ('README', 'rejstrik', 'kontextova-napoveda'): section_of[i] = 'hesla'
    if en:
        ren = {'hesla': 'topics', 'rejstrik': 'index-by-letter',
               'kontextova-napoveda': 'context-help', 'ostatni': 'other'}
        section_of = {k: ren.get(v, v) for k, v in section_of.items()}
    L = LABELS['en' if en else 'cz']
    titles = {g: t for g, t, _ in GROUPS}
    titles.update({'README': L['title_user'] if user else 'Nápověda PC FANDu 4.2',
                   'rejstrik': L['rejstrik'], 'kontextova-napoveda': L['kontext'],
                   'ostatni': L['ostatni'], 'hesla': L['hesla'],
                   'index-by-letter': L['rejstrik'], 'context-help': L['kontext'],
                   'other': L['ostatni'], 'topics': L['hesla']})
    main_sec = 'topics' if en else 'hesla'
    sections = [(main_sec, L['hesla'])] if user else [(g, t) for g, t, _ in GROUPS]
    def render(i):
        out = []; codes = to_codes(texts[i]); k = 0; bold = False; em = False
        while k < len(codes):
            c = codes[k]
            if c == 0x13:
                j = k + 1
                while j < len(codes) and codes[j] != 0x13: j += 1
                label = ''.join(kam_char(b) for b in codes[k+1:j] if b >= 32)
                t = resolve(label)
                esc = html.escape(label, quote=False)
                if t is None: out.append(f'<b>{esc}</b>')
                else:
                    f = section_of[t]; f = '' if f == section_of[i] else f + '.md'
                    out.append(f'<a href="{f}#t{t}">{esc}</a>')
                k = j + 1; continue
            if c == 0x02: out.append('</b>' if bold else '<b>'); bold = not bold
            elif c == 0x04: out.append('</i>' if em else '<i>'); em = not em
            elif c in ARROWS: out.append(ARROWS[c])
            elif c == 10: out.append('\n')
            elif c >= 32: out.append(html.escape(kam_char(c), quote=False))
            k += 1
        if bold: out.append('</b>')
        if em: out.append('</i>')
        return ''.join(out).rstrip()

    files = {}
    for i in topics:
        files.setdefault(section_of[i], []).append(i)
    os.makedirs(outdir, exist_ok=True)
    order = ['README'] + [g for g, _ in sections] + (['index-by-letter', 'other', 'context-help'] if en
                                                      else ['rejstrik', 'ostatni', 'kontextova-napoveda'])
    order = [f for f in order if f in files]
    for f in order:
        if f not in files: continue
        lines = [f'# {titles[f]}', '']
        if f == 'README':
            lines += [L['intro_user'] if user else L['intro'], '',
                      f"**{L['oddily']}:** " + ' · '.join(f'[{titles[s]}]({s}.md)' for s in order[1:] if s in files), '']
        for i in files[f]:
            head = L['obsah'] if i == root else (names[i] or L['bez'])
            lines.append(f'<a name="t{i}"></a>')
            lines.append(f'## {head}')
            if aliases[i]: lines.append(f"\n*{L['tez']}:* " + ', '.join(f'`{a}`' for a in aliases[i]))
            lines += ['', '<pre>', render(i), '</pre>', '']
        if f == 'README':
            idx = sorted(((names[i], i) for i in topics if names[i] and not names[i].startswith('FM')),
                         key=lambda p: fold(p[0]).strip())
            lines += [f"## {L['index']}", '']
            lines.append(' · '.join(f'[{html.escape(n.strip())}]({section_of[i]+".md" if section_of[i]!="README" else ""}#t{i})' for n, i in idx))
            lines.append('')
        open(os.path.join(outdir, f + '.md'), 'w', encoding='utf-8').write('\n'.join(lines))
    print(f'{len(topics)} topics in {len(files)} files: ' + ', '.join(f'{f}({len(files[f])})' for f in order if f in files))

if __name__ == '__main__':
    a = [x for x in sys.argv[1:] if not x.startswith('--')]
    main(a[0], a[1], user='--user' in sys.argv, en='--en' in sys.argv)
