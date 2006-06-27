
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A LaTeX style for books...
% Originally developed by:
% Juha Haataja, CSC (jhaataja@csc.fi).
% Last Revision: Nov 23rd, 1995
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\NeedsTeXFormat{LaTeX2e}[1994/12/01]
\ProvidesClass{elmeropas}
              [1996/02/07 
               CSC:n opastyyli]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{ifthen}

% Option "pdflatex" for using pdflatex instead of latex
\newboolean{usepdflatex}
\setboolean{usepdflatex}{false}
\DeclareOption{pdflatex}{\setboolean{usepdflatex}{true}}

\newboolean{opassize}
\setboolean{opassize}{true}
\DeclareOption{ownsize}{\setboolean{opassize}{false}}

% Option ``usearticlenumbers'' for section-level numbering
\newboolean{usearticlenumbers}
\setboolean{usearticlenumbers}{false}
\DeclareOption{articlenumbers}{\setboolean{usearticlenumbers}{true}}

% Option "textures" for using Textures
\newboolean{usetextures}
\setboolean{usetextures}{false}
\DeclareOption{textures}{\setboolean{usetextures}{true}}

% Option "english" for writing in English...
\newboolean{doenglish}
\setboolean{doenglish}{false}
\DeclareOption{english}{\setboolean{doenglish}{true}}

% Option "cropmark" prints out cropmarks...
\newboolean{docropmark}
\setboolean{docropmark}{false}
\DeclareOption{cropmark}{\setboolean{docropmark}{true}}

% Option "mathtime" uses the Mathtime fonts...
\newboolean{usemathtime}
\setboolean{usemathtime}{false}
\DeclareOption{mathtime}{\setboolean{usemathtime}{true}}

% Option "cmfonts" uses the OT1 CM fonts, 
% otherwise Lucida fonts are used.
% Usage of option: \documentclass[cmfonts]{cscopas}
\newboolean{usecmfonts}
\setboolean{usecmfonts}{false}
\DeclareOption{cmfonts}{\setboolean{usecmfonts}{true}}

% Option "dcfonts" uses the T1 CM fonts, 
% otherwise Lucida fonts are used.
% Usage of option: \documentclass[dcfonts]{cscopas}
\newboolean{usedcfonts}
\setboolean{usedcfonts}{false}
\DeclareOption{dcfonts}{\setboolean{usedcfonts}{true}}

% Option "ecfonts" uses the T1 CM fonts, 
% otherwise Lucida fonts are used.
% Usage of option: \documentclass[ecfonts]{cscopas}
\DeclareOption{ecfonts}{\setboolean{usedcfonts}{true}}

% Option "parskip" uses the parskip between paragraphs
% Usage of option: \documentclass[parskip]{cscopas}
\newboolean{useparskip}
\setboolean{useparskip}{false}
\DeclareOption{parskip}{\setboolean{useparskip}{true}}

% Option "draft" for graphicx
\newboolean{usedraft}
\setboolean{usedraft}{false}
\DeclareOption{draft}{\setboolean{usedraft}{true}}

% Option "amstex" includes the amstex package
% Usage of option: \documentclass[amstex]{cscopas}
\newboolean{useamstex}
\setboolean{useamstex}{false}
\DeclareOption{amstex}{\setboolean{useamstex}{true}}

% Option "amslatex" includes the amslatex package
% Usage of option: \documentclass[amslatex]{cscopas}
\newboolean{useamslatex}
\setboolean{useamslatex}{false}
\DeclareOption{amslatex}{\setboolean{useamslatex}{true}}

% Show the index entries on margins.
% Usage of option: \documentclass[showidx]{cscopas}
\newboolean{doshowidx}
\setboolean{doshowidx}{false}
\DeclareOption{showidx}{\setboolean{doshowidx}{true}}

% Option of Bibtex style natbib
% Usage of option: \documentclass[natbib]{cscopas}
\newboolean{usenatbib}
\setboolean{usenatbib}{false}
\DeclareOption{natbib}{\setboolean{usenatbib}{true}}

% Option "10pt" uses smaller font size
% Usage of option: \documentclass[10pt]{cscopas}
\newboolean{usetenpoint}
\setboolean{usetenpoint}{false}
\DeclareOption{10pt}{\setboolean{usetenpoint}{true}}

% Option "lucidasmallscale" uses smaller font size
% Usage of option: \documentclass[lucidasmallscale]{cscopas}
\newboolean{usesmalllucida}
\setboolean{usesmalllucida}{false}
\DeclareOption{lucidasmallscale}{\setboolean{usesmalllucida}{true}}

% Option "11pt" uses bigger font size
% Usage of option: \documentclass[11pt]{cscopas}
\newboolean{useelevenpoint}
\setboolean{useelevenpoint}{false}
\DeclareOption{11pt}{\setboolean{useelevenpoint}{true}}

% Handle other options to the document class

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{book}}%
\ProcessOptions%

% Use the book document style
\typeout{Using book numbering}
%
% Input the document class using correct font size
\ifthenelse{\boolean{usecmfonts}}
{% Use the OT1 CM fonts:
\LoadClass[11pt,a4paper,twoside,openany]{book}
}{% 
\ifthenelse{\boolean{usedcfonts}}
{% Use the T1 DC fonts:
\LoadClass[11pt,a4paper,twoside,openany]{book}
}{% 
\ifthenelse{\boolean{usemathtime}}
{% Use the Mathtime fonts:
\ifthenelse{\boolean{usetenpoint}}
{% Use 10pt fonts:
\LoadClass[10pt,a4paper,twoside,openany]{book}
}{%
\LoadClass[11pt,a4paper,twoside,openany]{book}
}%
}{% Use the Lucida fonts:
\ifthenelse{\boolean{useelevenpoint}}
{% Use 11pt fonts:
\LoadClass[11pt,a4paper,twoside,openany]{book}
}{%
\LoadClass[10pt,a4paper,twoside,openany]{book}
}}}}%






% Show the index entries on margins.
\ifthenelse{\boolean{doshowidx}}{\RequirePackage{showidx}}{}

% Is Lucida in use or not
\newboolean{nolucida}
\ifthenelse{\boolean{usecmfonts}}{\setboolean{nolucida}{true}}
{\ifthenelse{\boolean{usedcfonts}}{\setboolean{nolucida}{true}}
{\ifthenelse{\boolean{usemathtime}}{\setboolean{nolucida}{true}}
{{\setboolean{nolucida}{false}}}}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Packages to include

% Syntax checking
\ifthenelse{\boolean{usecmfonts}}{}%
{\RequirePackage{syntonly}}

% Font input coding
\ifthenelse{\boolean{usecmfonts}}{}{%
\RequirePackage[latin1]{inputenc}}

% Use the "amstex" package
\ifthenelse{\boolean{useamstex}}
{
\RequirePackage{amstex}
\renewcommand{\Vec}[1]{\mathify{\mathbf{#1}}}
}{}

% Use the "amslatex" package
\ifthenelse{\boolean{useamslatex}}
{
\RequirePackage[intlimits]{amsmath}
\RequirePackage{amsfonts,amssymb}
\renewcommand{\Vec}[1]{\mathify{\mathbf{#1}}}
}{}

% Extended theorem environments
\RequirePackage{amsthm}

% Test which fonts to use
\ifthenelse{\boolean{usecmfonts}}
{% Use the OT1 CM fonts:
\RequirePackage[OT1]{fontenc}
\RequirePackage{latexsym}
\DeclareRobustCommand{\blacksquare}{\mbox{\rule{4mm}{4mm}}}
\DeclareRobustCommand{\myBox}{\fbox{\rule{0.35em}{0em}\rule{0em}{0.35em}}}
\global\let\Box\myBox
\global\let\mathbb\mathbf
\providecommand{\real}{\mathify{\mathbf{R}}}
}{% 
\ifthenelse{\boolean{usedcfonts}}
{% Use the T1 DC fonts:
\RequirePackage[T1]{fontenc}
\RequirePackage{amssymb}
\global\let\Box\square
}{% 
\ifthenelse{\boolean{usemathtime}}
{% Use the Mathtime fonts
\RequirePackage{amssymb}
%\RequirePackage{mathtime}
\RequirePackage[noactivesb]{mt11p}
\def\Vec#1{\mathify{\mathbf{#1}}}
\def\vdots{\vbox{\baselineskip4\p@ \lineskiplimit\z@
    \kern6\p@\hbox{.}\hbox{.}\hbox{.}}}
\def\endash{--}
\def\emdash{---}
\renewcommand{\sfdefault}{cscphv}
\renewcommand{\rmdefault}{ptm}
\renewcommand{\ttdefault}{cscpcr}
%\renewcommand{\ttdefault}{csclbt}
%\RequirePackage[T1]{fontenc}
\providecommand{\at}{{\mbox{@}}}
\providecommand{\blacksquare}{\mbox{\rule{4mm}{4mm}}}
\providecommand{\myBox}{\fbox{\rule{0.3em}{0em}\rule{0em}{0.3em}}}
\global\let\Box\myBox
}{% Use the lucida fonts:
\RequirePackage[T1]{fontenc}
\RequirePackage{csclucida}
\DeclareTextSymbol{\textendash}{T1}{21} %endash
\DeclareTextSymbol{\textemdash}{T1}{22} %emdash
\renewcommand{\sc}{\small\MakeUppercase}
}}}

% If using OT1 fonts, do everything as with T1 fonts
\ifthenelse{\boolean{usecmfonts}}
{\setboolean{usedcfonts}{true}}{}

% Computer listings
\RequirePackage{cscverbatim}

% Calculations in TeX
\RequirePackage{calc}

% Graphics packages
\ifthenelse{\boolean{usetextures}}
{%
\RequirePackage[textures]{graphicx}
\RequirePackage[textures]{color}
}{%
\newif\ifpdf
\ifx\pdfoutput\undefined
  \pdffalse % we are not running PDFLaTeX
\else
  \pdfoutput=1 % we are running PDFLaTeX
  \pdftrue
\fi
%Then use your new variable \ifpdf
% The [dvips] option does not work with pdf-latex!!!!
\ifpdf
  \usepackage[pdftex]{graphicx}
  \usepackage[pdftex]{color}
  \pdfcompresslevel=9
  \DeclareGraphicsExtensions{.jpg,.pdf,.mps,.png} 
%  \graphicspath{{PDF/}}
\else
  \RequirePackage[dvips]{graphicx}
  \RequirePackage[dvips]{color}
  \DeclareGraphicsExtensions{.eps,.ps,.bmp,.tif,.tiff,.tga}  
%  \graphicspath{{EPS/}} 
\fi
\definecolor{miugray}{gray}{0.75}
\definecolor{miudark}{gray}{0.6}
\definecolor{miulight}{gray}{0.85}
}
\graphicspath{{./}{figs/}}
%\RequirePackage{psfrag}

% Numerical arrays etc.
\RequirePackage{array}
\RequirePackage{delarray}

% Index generation
\RequirePackage{makeidx}
\makeindex

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Finnish language
\ifthenelse{\boolean{doenglish}}
{%
\RequirePackage[finnish,english]{babel}
}{%
\RequirePackage[finnish]{babel}
%\allowhyphens
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{cscfloat}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Column alignments
\RequirePackage{dcolumn}

% Spacing in arrays
\setlength{\jot}{6pt}
\setlength{\arraycolsep}{3pt}

% Decimal numbers
\newcolumntype{+}[1]{D{.}{.}{#1}}
\newcolumntype{,}{D{,}{,}{1}}
\newcolumntype{:}[2]{@{\hspace{#1}}D{,}{,\,}{4}@{\hspace{#2}}}
\newcolumntype{.}{+{4}}
\newcolumntype{d}{+{0}}
\newcolumntype{s}{+{0}}

% Math mode
\newcolumntype{C}{>{$}c<{$}}
\newcolumntype{L}{>{$}l<{$}}
\newcolumntype{R}{>{$}r<{$}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% If not using Lucida fonts, use Helvetica for bold...
\ifthenelse{\boolean{nolucida}}
{% Style for sans-serif bold if no Lucida in use
\ifthenelse{\boolean{usecmfonts}}
{\DeclareRobustCommand{\Helvbo}{\fontfamily{cscphv}\fontseries{b}\selectfont}}%
{\DeclareRobustCommand{\Helvbo}{\usefont{T1}{cscphv}{b}{n}}}
\DeclareRobustCommand{\Obf}{\Helvbo}
\message{(Using Helvetica for bold titles...)}
}{%
% Style for sans-serif bold for Lucida font
\DeclareRobustCommand{\Obf}{\sffamily\bfseries}
\message{(Using normal bold for titles...)}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Page size

% this is needed later for esimwidth
\newlength{\opastextwidth}
\setlength{\opastextwidth}{\textwidth}

\ifthenelse{\boolean{opassize}}{%
% Lengths for page dimensions
% Affects headerwidth, default 116mm
\setlength{\opastextwidth}{116mm}
\setlength{\topmargin}{-11mm}
\setlength{\headheight}{8pt}
\setlength{\headsep}{16pt}
%\setlength{\baselineskip}{12.5pt}
\linespread{1}
%\setlength{\footskip}{18pt+\baselineskip+3mm}
\setlength{\footskip}{0pt+\baselineskip+3mm}
\setlength{\textheight}{202mm+\topskip}
\setlength{\textwidth}{\opastextwidth}
\setlength{\oddsidemargin}{0mm}
\setlength{\evensidemargin}{0mm}
%
% Margins and indents
\newlength{\opasmargin}
\setlength{\opasmargin}{6.7mm}
%\setlength{\opasmargin}{0.0mm}
\newlength{\curropasmargin}
\setlength{\curropasmargin}{\opasmargin}
\newlength{\opasexmargin}
\setlength{\opasexmargin}{2\opasmargin}
%
\newlength{\opasparskip}
\ifthenelse{\boolean{useparskip}}
{% Use parskip
\setlength{\opasparskip}{1.5ex plus 0.15ex minus 0.1ex}
%\setlength{\opasparskip}{0.15ex}
\setlength{\parskip}{\opasparskip}
\setlength{\parindent}{0pt}
}{% Dont use parskip
\setlength{\opasparskip}{0pt}
\setlength{\parskip}{\opasparskip}
\setlength{\parindent}{\opasmargin}
}
}


% Hyphenation
\righthyphenmin=2
\lefthyphenmin=2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Specify header style
\RequirePackage{fancyheadings}
%\RequirePackage{fancyhdr}
\pagestyle{fancy}
\lhead[\normalfont\small\Obf\thepage]{\normalfont\small\slshape\rightmark}
\rhead[\small\slshape\lefthead]{\normalfont\small\Obf\thepage}
\setlength{\headrulewidth}{0.4pt}
\ifthenelse{\boolean{usearticlenumbers}}
{}{
\renewcommand{\chaptermark}[1]{\markright{\@chapapp\ \thechapter.\ #1}{}}
}
\renewcommand{\sectionmark}[1]{}
\renewcommand{\subsectionmark}[1]{}
\cfoot{}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% More space for chapter and section headers
% Also changes font to sans serif...

\renewcommand\part{%
  \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
  \thispagestyle{empty}%
  \if@twocolumn
    \onecolumn
    \@tempswatrue
  \else
    \@tempswafalse
  \fi
  \null\vfil
  \secdef\@part\@spart}
\def\@part[#1]#2{%
    \ifnum \c@secnumdepth >-2\relax
      \refstepcounter{part}%
      \addcontentsline{toc}{part}{\thepart\hspace{1em}#1}%
    \else
      \addcontentsline{toc}{part}{#1}%
    \fi
    %\markboth{}{}%
    {\centering
     \interlinepenalty \@M
     \normalfont
     \ifnum \c@secnumdepth >-2\relax
       \huge\Obf \partname~\thepart
       \par
       \vskip 20\p@
     \fi
     \Huge \bfseries #2\par}%
    \@endpart}
\def\@spart#1{%
    {\centering
     \interlinepenalty \@M
     \normalfont
     \Huge \Obf #1\par}%
    \@endpart}
\def\@endpart{\vfil\newpage
              \if@openright
                \null
                \thispagestyle{empty}%
                \newpage
              \fi
              \if@tempswa
                \twocolumn
              \fi}

\ifthenelse{\boolean{usearticlenumbers}}
{
\renewcommand{\chapter}{%\if@openright\cleardoublepage\else\clearpage\fi
                    \global\@topnum\z@
                    \@afterindentfalse
                    \secdef\@chapter\@schapter}
%
\renewcommand{\@makechapterhead}[1]{%
  \vspace{30\p@}%
  {\parindent \z@ \raggedright \normalfont
    \LARGE\Obf \thechapter \hspace*{1em}  #1\par\nobreak
    \vskip 15\p@
  }}
}{
\renewcommand{\chapter}{\if@openright\cleardoublepage\else\clearpage\fi
                    \global\@topnum\z@
                    \@afterindentfalse
                    \secdef\@chapter\@schapter}
%
\renewcommand{\@makechapterhead}[1]{%
  \vspace*{50\p@}%
  {\parindent \z@ \raggedright \normalfont
    \ifnum \c@secnumdepth >\m@ne
      \if@mainmatter
        \huge\Obf \@chapapp\space \thechapter
        \par\nobreak
        \vskip 20\p@
      \fi
    \fi
    \interlinepenalty\@M
    \Huge \Obf #1\par\nobreak
    \vskip 40\p@
  }}
}
%
\renewcommand{\section}{\@startsection
  {section}%
  {1}%
  {\z@}%
  {-4.75ex plus -1ex minus -.2ex}%
  {3.0ex plus .2ex}%
  {\raggedright\normalfont\Large\Obf}}
%
\renewcommand{\subsection}{\@startsection
  {subsection}%
  {2}%
  {\z@}%
  {-3.5ex plus -1ex minus -.2ex}%  
  {1.75ex plus .2ex}%
  {\raggedright\normalfont\large\Obf}}
%
\renewcommand{\subsubsection}{\@startsection
  {subsubsection}%
  {3}%
  {\z@}%
  {-3.5ex plus -1ex minus -.2ex}%
  {1.75ex plus .2ex}%
  {\raggedright\normalfont\normalsize\Obf}}
%
\renewcommand\appendix{\par
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \ifthenelse{\boolean{doenglish}}%
  {%
  \addcontentsline{toc}{chapter}{Appendix}%
  }{%
  \addcontentsline{toc}{chapter}{Liitteet}%
  }%
  \renewcommand\@chapapp{\appendixname}%
  \renewcommand\thechapter{\@Alph\c@chapter}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Numbering for sections and table of contents
\setcounter{secnumdepth}{2}
\setcounter{tocdepth}{1}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Table of contents, index, bibliography etc.

% Command for starting the actual text...

\providecommand{\Startatpage}[1]{%
%\mbox{}\thispagestyle{empty}\clearpage
\pagenumbering{arabic}\c@page=#1}

% Esipuhe

\DeclareRobustCommand{\esipuhe}[1]{\chapter*{\Obf{#1}%
\markboth{\lefthead}{\lefthead}}%
\addcontentsline{toc}{chapter}{#1}}

% Symboliluettelo

\ifthenelse{\boolean{doenglish}}
{%
\DeclareRobustCommand{\symbolit}{\chapter*{\Obf{List of symbols}%
\markboth{Symboliluettelo}{List of symbols}}%
\addcontentsline{toc}{chapter}{List of symbols}}
}{%
\DeclareRobustCommand{\symbolit}{\chapter*{\Obf{Symboliluettelo}%
\markboth{Symboliluettelo}{Symboliluettelo}}%
\addcontentsline{toc}{chapter}{Symboliluettelo}}
}
% Table of contents

\renewcommand{\tableofcontents}{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    {\setlength{\parskip}{0pt}%
     \chapter*{\Obf{\contentsname}
        \@mkboth{\contentsname}{\contentsname}}%
    \@starttoc{toc}}%
    \if@restonecol\twocolumn\fi
    }

\renewcommand*\l@part[2]{%
  \ifnum \c@tocdepth >-2\relax
    \addpenalty{-\@highpenalty}%
    \addvspace{1.8em \@plus\p@}%
    \begingroup
      \setlength\@tempdima{3.4em}%
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      {\leavevmode
       \large \bfseries #1\hfil \hb@xt@\@pnumwidth{\hss #2}}\par
       \nobreak
         \global\@nobreaktrue
         \everypar{\global\@nobreakfalse\everypar{}}%
    \endgroup
  \fi}

\ifthenelse{\boolean{usearticlenumbers}}
{}{
\renewcommand*\l@chapter[2]{%
  \ifnum \c@tocdepth >\m@ne
    \addpenalty{-\@highpenalty}%
    \vskip 0.65em \@plus\p@
    \setlength\@tempdima{1.9em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil \nobreak\hb@xt@\@pnumwidth{\hss #2}\par
      \penalty\@highpenalty
    \endgroup
  \fi}
}
\renewcommand*\l@section{\@dottedtocline{1}{1.9em}{3.3em}}
\renewcommand\@pnumwidth{2.0em}

% List of figures

\renewcommand\listoffigures{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    {\setlength{\parskip}{0pt}\setlength{\parindent}{0pt}%
    \chapter*{\Obf{\listfigurename}
      \@mkboth{\listfigurename}{\listfigurename}}%
    \@starttoc{lof}}%
    \if@restonecol\twocolumn\fi
    }
\renewcommand*\l@figure{\@dottedtocline{1}{2.0em}{2.8em}}

% List of tables

\renewcommand\listoftables{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    {\setlength{\parskip}{0pt}\setlength{\parindent}{0pt}%
    \chapter*{\Obf{\listtablename}
      \@mkboth{\listtablename}{\listtablename}}%
    \@starttoc{lot}}%
    \if@restonecol\twocolumn\fi
    }

\renewcommand*\l@table{\@dottedtocline{1}{2.0em}{2.8em}}

% Index

\renewenvironment{theindex}
               {\if@twocolumn
                  \@restonecolfalse
                \else
                  \@restonecoltrue
                \fi
                \columnseprule \z@
                \columnsep 35\p@
                \twocolumn[\@makeschapterhead{\vspace{8mm}\Obf{\indexname}}]%
                \@mkboth{\indexname}{\indexname}%
                \addcontentsline{toc}{chapter}{\indexname}%
                \parindent\z@
                \parskip\z@ \@plus .3\p@\relax\small
                \let\item\@idxitem}
               {\if@restonecol\onecolumn\else\clearpage\fi}

% Bibliography

\ifthenelse{\boolean{usenatbib}}{
\RequirePackage{natbib}
\@ifundefined{chapter}%
  {\renewcommand\bibsection{\section*{\Obf{\refname}
    \@mkboth{\refname}{\refname}}}}
  {\renewcommand\bibsection{\chapter*{\Obf{\bibname}
   \@mkboth{\bibname}{\bibname}}
    \addcontentsline{toc}{chapter}{\bibname} }}
}{%
\renewenvironment{thebibliography}[1]
     {\chapter*{\Obf{\bibname}
        \@mkboth{\bibname}{\bibname}%
        \addcontentsline{toc}{chapter}{\bibname}}%
      \small\list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy\clubpenalty4000\widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{cscenv}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Some short cuts for text type

\DeclareRobustCommand{\Rm}[1]{\textrm{#1}}  % kursiivi
\DeclareRobustCommand{\Em}[1]{\emph{#1}}    % kursiivi
\DeclareRobustCommand{\emname}[1]{\emph{#1}}    % kursiivi
\DeclareRobustCommand{\En}[1]{{#1}}         % englanninkieliset termit
\DeclareRobustCommand{\Sl}[1]{\textsl{#1}}  % kallistettu
\DeclareRobustCommand{\Sf}[1]{\textsf{#1}}  % sans serif
\DeclareRobustCommand{\Bf}[1]{{\Obf#1}}  % lihavoitu
\DeclareRobustCommand{\Tt}[1]{\texttt{#1}}  % kirjoituskone
\DeclareRobustCommand{\Sc}[1]{\textsc{#1}}  % keppikirjaimet
\DeclareRobustCommand{\Mm}[1]{\ensuremath{#1}}  % matikkamoodi
\DeclareRobustCommand{\Cal}[1]{\ensuremath{\mathcal{#1}}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Placement of PS pictures
% Matlab pictures
\DeclareRobustCommand{\matlabepsf}[1]{\includegraphics[bb=48 270 306 522]{#1}}
\DeclareRobustCommand{\matlabepsftwo}[1]{\includegraphics%
  [bb=48 270 564 522]{#1}}
\DeclareRobustCommand{\matlabepsfup}[1]{\includegraphics%
  [bb=48 396 564 612]{#1}}
\DeclareRobustCommand{\matlabepsfupold}[1]{\includegraphics%
  [bb=80 605 532 756]{#1}}
% Mathematica pictures
\DeclareRobustCommand{\mmaepsf}[1]{\includegraphics%
  [bb=72 230 540 570]{#1}}
% Without border
\DeclareRobustCommand{\illustration}[1]%
  {\centerline{\mbox{\includegraphics{#1}}}}
\DeclareRobustCommand{\epsillu}[1]%
  {\centerline{\mbox{\includegraphics{#1}}}}
\DeclareRobustCommand{\xepsillu}[2]%
  {\centerline{\mbox{\includegraphics[width=#1]{#2}}}}
\DeclareRobustCommand{\yepsillu}[2]%
{\centerline{\mbox{\includegraphics[height=#1]{#2}}}}
% With border
\DeclareRobustCommand{\fillustration}[1]%
  {\centerline{\fbox{\includegraphics{#1}}}}
\DeclareRobustCommand{\fepsillu}[1]%
  {\centerline{\fbox{\includegraphics{#1}}}}
\DeclareRobustCommand{\fxepsillu}[2]%
  {\centerline{\fbox{\includegraphics[width=#1]{#2}}}}
\DeclareRobustCommand{\fyepsillu}[2]%
  {\centerline{\fbox{\includegraphics[height=#1]{#2}}}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% If requested, print cropmarks

\ifthenelse{\boolean{docropmark}}
{\RequirePackage{crop}}{}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\endinput
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
