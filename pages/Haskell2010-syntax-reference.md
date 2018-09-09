# 文法リファレンス

### 慣習的表記

以下の慣習的表記が文法を表現するのにつかわれる:

- `[pattern]` 省略可能
- `{pattern}` 0回以上の繰り返し
- `(pattern)` グループ化
- `pat1 | pat2` 選択
- `pat<pat'>` 差(`pat`によって生成された要素で、`pat'`で生成されたものを除いたもの)
- `'fibonacci'` クォートされた終端文法(**[訳注]** 原文ではここは等幅フォントとして表示されているが、文法表現全体を等幅フォントとして表示するようにmarkdownを使う都合上、終端語句はクォートにより表現することにした。以下では紛らわしいケースは逐一注を入れている)

BNFのような文法をレポートを通して用いる。文法の生成は次のような形をしている:

```
nonterm -> alt_1 | alt_2 | .. | alt_n
```

字句文法、文脈自由文法いずれも曖昧さが残るが、これは文法語句をできる限り長く取り、左から右に進む(shift-reduceパースでは、shift/reduceコンフリクトはシフトを取ることで解決する)ことで解決するものとする。字句文法では、これは「最長一致」と呼ばれるルールである。文脈自由文法では、これは条件式やlet式、ラムダ抽象などが右方向に取れるだけ長くとることを表す。

### 字句文法

```
program	→	{ lexeme | whitespace }
lexeme	→	qvarid | qconid | qvarsym | qconsym
        |	literal | special | reservedop | reservedid
literal	→	integer | float | char | string
special	→	'(' | ')' | ',' | ';' | '[' | ']' | '`' | '{' | '}'
 
whitespace  →	whitestuff {whitestuff}
whitestuff  →	whitechar | comment | ncomment
whitechar   →	newline | vertab | space | tab | uniWhite
newline     →	return linefeed | return | linefeed | formfeed
return	    →	a carriage return
linefeed    →	a line feed
vertab      →	a vertical tab
formfeed    →	a form feed
space       →	a space
tab         →	a horizontal tab
uniWhite    →	any Unicode character defined as whitespace
 
comment     → 	dashes [ any⟨symbol⟩ {any} ] newline
dashes      →	'--' {-}
opencom     →	'{-'
closecom    →	'-}'
ncomment    →	opencom ANY seq {ncomment ANY seq} closecom
ANY seq     →	{ANY }⟨{ANY } ( opencom | closecom ) {ANY }⟩
ANY         →	graphic | whitechar
any	    →	graphic | space | tab
graphic	    →	small | large | symbol | digit | special | " | '
 
small       →	ascSmall | uniSmall | _
ascSmall    →	'a' | 'b' | … | 'z'
uniSmall    →	any Unicode lowercase letter
 
large       →	ascLarge | uniLarge
ascLarge    →	'A' | 'B' | … | 'Z'
uniLarge    →	any uppercase or titlecase Unicode letter
symbol      →	ascSymbol | uniSymbol⟨special | _ | " | '⟩
 
ascSymbol   →	'!' | '#' | '$' | '%' | '&' | '⋆' | '+' | '.' | '/' | '<' | '=' | '>' | '?' | '@'
            |	'\' | '^' | '|' | '-' | '~' | :
uniSymbol   →	any Unicode symbol or punctuation
digit       →	ascDigit | uniDigit
ascDigit    →	'0' | '1' | … | '9'
uniDigit    →	any Unicode decimal digit
octit       →	'0' | '1' | … | '7'
hexit       →	digit | 'A' | … | 'F' | 'a' | … | 'f'
 
[訳注] varidとconidに登場するのクォートはクォート記号である
varid       →	(small {small | large | digit | ' })⟨reservedid⟩
conid       →	large {small | large | digit | ' }
reservedid  →	case | class | data | default | deriving | do | else
|	foreign | if | import | in | infix | infixl
|	infixr | instance | let | module | newtype | of
|	then | type | where | _
 
varsym      →	( symbol⟨:⟩ {symbol} )⟨reservedop | dashes⟩
consym      →	( ':' {symbol})⟨reservedop⟩
reservedop  →	'..' | ':' | '::' | '=' | '\' | '|' | '<-' | '->' |  '@' | '~' | '=>'
 
varid	    	                    (variables)
conid	    	                    (constructors)
tyvar	→	varid	            (type variables)
tycon	→	conid	            (type constructors)
tycls	→	conid	            (type classes)
modid	→	{conid '.'} conid   (modules)
 
qvarid	→	[ modid '.' ] varid
qconid	→	[ modid '.' ] conid
qtycon	→	[ modid '.' ] tycon
qtycls	→	[ modid '.' ] tycls
qvarsym	→	[ modid '.' ] varsym
qconsym	→	[ modid '.' ] consym
 
decimal	    →	digit{digit}
octal       →	octit{octit}
hexadecimal →	hexit{hexit}
 
integer	    →	decimal
            |	'0o' octal | '0O' octal
            |	'0x' hexadecimal | '0X' hexadecimal
float	    →	decimal . decimal [exponent]
            |	decimal exponent
exponent    →	('e' | 'E') ['+' | '-'] decimal
 
[訳注] charに登場するクォートはクォート記号である
char	→	' (graphic⟨' | \⟩ | space | escape⟨\&⟩) '
string	→	" {graphic⟨" | \⟩ | space | escape | gap} "
escape	→	'\' ( charesc | ascii | decimal | 'o' octal | 'x' hexadecimal )
charesc	→	'a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\' | '"' | ''' | '&'
ascii	→	^cntrl | 'NUL' | 'SOH' | 'STX' | 'ETX' | 'EOT' | 'ENQ' | 'ACK'
        |	'BEL' | 'BS' | 'HT' | 'LF' | 'VT' | 'FF' | 'CR' | 'SO' | 'SI' | 'DLE'
        |	'DC1' | 'DC2' | 'DC3' | 'DC4' | 'NAK' | 'SYN' | 'ETB' | 'CAN'
        |	'EM' | 'SUB' | 'ESC' | 'FS' | 'GS' | 'RS' | 'US' | 'SP' | 'DEL'
cntrl	→	ascLarge | '@' | '[' | '\' | ']' | '^' | '_'
gap	→	\ whitechar {whitechar} \
```

### レイアウト

セクション2.7(**[訳注]** TODO:リンク)ではレイアウトルールに対する非形式的な議論を見た。このセクションではより正確に定義をする。

Haskellプログラムの意味はその**レイアウト**に依存する場合がある。レイアウトが意味に与える効果は波括弧とセミコロンをレイアウトによって決定される位置に追加することで完全に説明できる。このようにして追加されたプログラムの意味は今やレイアウトによって影響を受けない。

レイアウトがプログラムに対して与える影響は、このセクションで波括弧とセミコロンをどのようについかするかを説明することで指定される。仕様は、プログラムの返還を行う関数`L`の形で与えられる。`L`の入力は次のようなものである:

- Haskellレポートにある字句文法によって定められた語句の列であって、さらに次のような追加の語句を含む:
    - `let, where, do, of`キーワードの後に`{`が続かない場合、トークン{n}がキーワードの後に挿入される。ただしnは、続くトークンがあればそのインデントを表し、ファイルの終端に達した場合は0を表す。
    - モジュールの最初のトークンが`{`でも`module`でもない場合、そのトークンのインデントをnとすると、{n}が先行する。
    - 同じ行で空白のみが最初のトークンに先行する場合、
    