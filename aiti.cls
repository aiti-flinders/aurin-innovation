\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{aiti}
\LoadClass[%
a4paper,
11pt,
titlepage,
headings=big,
chapterprefix=false,
headsepline,
numbers=noenddot,
]{scrreprt}

% Continuous numbering
\@ifundefined{counterwithout}{
\RequirePackage{chngcntr}
}{}
\RequirePackage{etoolbox}
\RequirePackage[bottom]{footmisc}
\RequirePackage{tablefootnote} % for footnotes within tables
\RequirePackage{zref-savepos}
\RequirePackage{longtable}
\RequirePackage{cmap}
\RequirePackage{scrhack}
\RequirePackage{xcolor}
\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{tikz}
\RequirePackage{adjustbox}
  \usetikzlibrary{positioning}
  \usetikzlibrary{decorations.text}
  \usetikzlibrary{decorations.pathmorphing}
  \usetikzlibrary{patterns,arrows,decorations.pathreplacing} % for waterfalls
\RequirePackage{float}
\RequirePackage{placeins}
\RequirePackage{afterpage}

\iffalse
\RequirePackage[activate=false, expansion, final, kerning=true, spacing=true]{microtype}
\microtypecontext{spacing=nonfrench}
\fi
\RequirePackage[document]{ragged2e}
% ------
% AITI colors
\definecolor{Blue}{RGB}{0, 119, 255}
\definecolor{DarkBlue}{RGB}{0,17,85}
\definecolor{Yellow}{RGB}{253,217,105}  % for boxes
\definecolor{Color1}{RGB}{255,224,127}
\definecolor{Color2}{RGB}{255,195,90}
\definecolor{Color3}{RGB}{246,139,51}
\definecolor{Color4}{RGB}{212,88,42}
\definecolor{Color5}{RGB}{160,34,38}
\definecolor{Color6}{RGB}{98,18,20}
\definecolor{theGrey}{RGB}{106,115,123}
\definecolor{AuthorPage}{RGB}{160,34,38}
\definecolor{AuthorGrey}{RGB}{174,174,174}


% Bullets and numbered items
\RequirePackage{enumitem}
% Eliminate left margin
% \setlist[itemize]{leftmargin=*}
% \setlist[enumerate]{leftmargin=*}

% See http://tex.stackexchange.com/questions/8510/reduce-size-of-bullet-character-in-lists
% Purpose to enlarge and subsequently raise the bullet
% see also \renewcommand{\labelitemi}{\raise .5ex\hbox{\tiny$\bullet$}}
\renewcommand{\labelitemi}{\color{Blue}\parbox[c]{.5em}{\small\ensuremath{\bullet}}{\normalsize\strut}}
\renewcommand{\labelenumi}{\color{Blue}{\bfseries \arabic{enumi}.~}}
\renewcommand{\labelitemii}{{\color{Blue}\bfseries \textendash}}
% ------
\RequirePackage{colortbl}
\RequirePackage{array}
\newcommand{\myhline}{\noalign{\global\arrayrulewidth1pt}\hline
                      \noalign{\global\arrayrulewidth1pt}}


\RequirePackage{graphicx}
\RequirePackage{booktabs}
% No widows
\RequirePackage[all]{nowidow}

\RequirePackage[utf8]{inputenc}
\RequirePackage[T1]{fontenc}
  

\RequirePackage[scaled]{helvet}
  \renewcommand{\familydefault}{\sfdefault}

\RequirePackage[framemethod=TikZ]{mdframed}
\RequirePackage{newfloat}
\RequirePackage{caption}
% Cross references.
\RequirePackage{varioref}
\PassOptionsToPackage{hyphens}{url}
\RequirePackage[
  hypertexnames=false, 
  hidelinks
]{hyperref}
\RequirePackage{cleveref}


\newcommand*{\source}[1]{%
  \captionsetup{format=plain, font={footnotesize, it}, skip=3pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Source:\ #1\@addpunct{.}}
}

\newcommand*{\notes}[1]{%
  \captionsetup{format=plain, font={footnotesize, it}, skip=3pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Notes:\ #1\@addpunct{.}}
}

\newcommand*{\sources}[1]{%
  \captionsetup{format=plain, font={footnotesize, it}, skip=3pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Sources:\ #1\@addpunct{.}}
}

\newcommand*{\note}[1]{%
  \captionsetup{format=plain, font={footnotesize, it}, skip=3pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Note:\ #1\@addpunct{.}}
}

\newcommand*{\noteswithsource}[2]{%
  \ifstrempty{#2}{\ClassWarning{'\noteswithsource' has second argument empty}}{}%
  \captionsetup{format=plain, font={footnotesize, it}, skip=2.5pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Notes:\ #1\@addpunct{.}}
  \caption*{Source:\ #2\@addpunct{.}}
}

\newcommand*{\notewithsource}[2]{%
  \ifstrempty{#2}{\ClassWarning{'\notewithsource' has second argument empty}}{}%
  \captionsetup{format=plain, font={footnotesize, it}, skip=2.5pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Note:\ #1\@addpunct{.}}
  \caption*{Source:\ #2\@addpunct{.}}
}

\newcommand*{\notewithsources}[2]{%
  \ifstrempty{#2}{\ClassWarning{'\notewithsources' has second argument empty}}{}%
  \captionsetup{format=plain, font={footnotesize, it}, skip=2.5pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Note:\ #1\@addpunct{.}}
  \caption*{Sources:\ #2\@addpunct{.}}
}

\newcommand*{\noteswithsources}[2]{%
  \ifstrempty{#2}{\ClassWarning{'\noteswithsources' has second argument empty}}{}%
  \captionsetup{format=plain, font={footnotesize, it}, skip=2.5pt, justification=RaggedRight, singlelinecheck=false, position=below}
  \caption*{Notes:\ #1\@addpunct{.}}
  \caption*{Sources:\ #2\@addpunct{.}}
}

\newcommand*{\boxsources}[1]{\footnotesize\textit{#1}\@addpunct{.}}

% More consistent fonts for quotes:
\AtBeginEnvironment{quote}{\small\justifying}

\setkomafont{disposition}{\color{Blue}}
\addtokomafont{chapter}{\bfseries\Large}
\addtokomafont{section}{\bfseries\normalsize}
\addtokomafont{subsection}{\bfseries\normalsize}
\addtokomafont{subsubsection}{\normalsize}

% Spacing
\RedeclareSectionCommand[
  beforeskip=1\baselineskip,
  afterskip=1\baselineskip]{chapter}
\RedeclareSectionCommand[
  beforeskip=0.5\baselineskip plus 0.3\baselineskip,
  afterskip=0.5\baselineskip]{section}
\RedeclareSectionCommand[
  beforeskip=0.5\baselineskip plus 0.2\baselineskip,
  afterskip=0.5\baselineskip]{subsection}
\RedeclareSectionCommand[
  beforeskip=0.5\baselineskip plus 0.1\baselineskip,
  afterskip=0.5\baselineskip]{subsubsection}
\RedeclareSectionCommand[
  beforeskip=.5\baselineskip,
  afterskip=-1em]{paragraph}
\RedeclareSectionCommand[
  beforeskip=-.5\baselineskip,
  afterskip=-1em]{subparagraph}
  
\renewcommand*{\@seccntformat}[1]{\csname the#1\endcsname\hspace{1.25em}}

% Removes glue around section titles. 
\RequirePackage{xpatch}
\xapptocmd{\sectionlinesformat}{\vspace*{-\parskip}}{}{\PatchFailed}

% Named appendix: Appendix A not just 'A'
\providecommand*\appendixmore{}
\renewcommand*\appendixmore{%
    \renewcommand*{\chapterformat}{%
      \mbox{\appendixname~\thechapter\autodot:\enskip}%
    }%
}

  
% --------------
%
%
%---------------

% N.B. geometry must be after hyperref, and hence cleveref


\RequirePackage[
  portrait,
  left=2.5cm, 
  right=2.5cm,
  top=100pt, 
  headsep=30pt,
  bottom=100pt,
  headheight=25pt, 
  footskip=25pt,
]{geometry}


  
%% ---------------------------------
%  Headers and footers
%% ---------------------------------
\RequirePackage[headsepline=1pt,plainheadsepline,footsepline=1pt,plainfootsepline]{scrlayer-scrpage}

\clearscrheadings
\clearscrplain
\clearscrheadfoot
% -------
% Heads and footers
%
\RequirePackage{eso-pic}
\newcommand{\classification}{}



% Avoid spaces or newlines in the following commands' arguments: they 
% may cause errors.
% Furthermore, optional arguments are required: they appear in Chapter 
% heading pages. Leaving them out will reset the titles on pages with 
% \chapter{}
\lehead[\normalfont\textcolor{theGrey}{\mytitle}{\huge\strut}]{\normalfont\textcolor{theGrey}{\mytitle}{\huge\strut}}
\lohead[\normalfont\textcolor{theGrey}{\mytitle}{\huge\strut}]{\normalfont\textcolor{theGrey}{\mytitle}{\huge\strut}}
\ifoot[{\textcolor{theGrey}{\normalfont AITI \@YEAR\\\emph{\small\thepage} {\Large\strut}}}]{\textcolor{theGrey}{\normalfont AITI \@YEAR\\\emph{\small\thepage}}{\Large\strut}}
\cfoot[\classification]{\classification}
\ofoot[\normalfont\textcolor{theGrey}{\includegraphics[height=20mm]{./logos/aiti_logo_square.png}}]{\normalfont\textcolor{theGrey}{\includegraphics[height=20mm]{./logos/aiti_logo_square.png}}}

\setkomafont{headsepline}{\color{Blue}}
\setkomafont{footsepline}{\color{Blue}}


% Ensure pdfs are used first
\DeclareGraphicsExtensions{%
    .pdf,.PDF,%
    .png,.PNG,%
    .jpg,.mps,.jpeg,.jbig2,.jb2,.JPG,.JPEG,.JBIG2,.JB2}

%% FrontPage options
%% First, the default, plain front page with a title.
\newcommand*{\titleTH}{\begingroup % Create the command for including the title page in the document
\raggedleft % Right-align all text
\vfil
\phantom{.}\hfill 


\vspace{30pt}
\vfil % Whitespace at the top of the page
{\bfseries\textcolor{Blue}{\fontsize{30}{35}\selectfont \mytitle}}\\[\baselineskip] % Title page font size
{\bfseries\fontsize{18}{35}\selectfont \textcolor{Blue}{\mysubtitle}}\\[\baselineskip]

{\LARGE \myauthor}

\vfill% Whitespace at the bottom of the page
\endgroup}
\newcommand{\BackgroundPic}{}

% Alternatively, if FrontPage is declared, the file there is used as the frontpage picture
\DeclareOption{FrontPage}{
  \IfFileExists{./FrontPage/FrontPage.pdf}{%
      %\includegraphics[width=\paperwidth,keepaspectratio]{./FrontPage/FrontPage}%
    }{
      \ClassError{grattan}{%
      './FrontPage/FrontPage.pdf' does not exist
      }{%
      You have asked for a fullpicture option, but we can't find 
      a file at './FrontPage/FrontPage.pdf'. Try placing the image
      there or not using the fullpicture option.
    }
    }
  \renewcommand\BackgroundPic{%
    \put(0,0){%
    \parbox[b][\paperheight]{\paperwidth}{%
    \vfill
    \centering
    % Amend
    \IfFileExists{./FrontPage/FrontPage.pdf}{%
      \includegraphics[width=\paperwidth,keepaspectratio]{./FrontPage/FrontPage.pdf}%
    }{
      \ClassError{grattan}{%
        './FrontPage/FrontPage' does not exist
      }{%
        You have asked for a fullpicture option, but we can't find 
        a file at './FrontPage/FrontPage'. Try placing the image
        there or not using the fullpicture option.
      }
    }
  \vfill
  }}}
  \renewcommand{\titleTH}{} % FrontPage should be entire.

}

\DeclareOption{continuous}{%
  \KOMAoption{listof}{nochaptergap}
  \addtocontents{lof}{\linespread{1.3}\selectfont}% optical
  \addtocontents{lot}{\linespread{1.3}\selectfont}% optical
  \counterwithout{figure}{chapter}
  \counterwithout{table}{chapter}
}

\newcommand*{\@pagetwo}{%
  \begin{minipage}[t][0.85\textheight][t]{\columnwidth}
  \subsection*{Australian Industrial Transformation Institute}
  \subsubsection*{College of Business, Government and Law Flinders University of South Australia}
  
  1284 South Road \\
  Clovelly Park \\
  South Australia 5042
  
  \emph{www.flinders.edu.au/aiti}
  
  \vfil
  
  ISBN: 000-0-0000000-0-0	 
  
  CAT: AITI\@YEAR\@ReportNumber
  
  \vfil
     \setlength{\parskip}{5.5pt plus 2pt}
    \raggedright\@acknowledgements
  
   \end{minipage}
  }

\newcommand*{\@checkReportNumberdefined}{
  \ifdefempty{\@ReportNumber}%
    {\ClassError{You have not provided a Report Number}}%
    {}%
}

\ProcessOptions\relax

% Bibliography
\RequirePackage[english]{babel}
\RequirePackage{csquotes}
\RequirePackage[
  backend=biber,
  singletitle,
  style=authoryear-ibid,
  ibidtracker=constrict, % avoid ibids from figures being confusing
  autocite=footnote, 
  maxcitenames=2, 
  maxbibnames=9, 
  uniquelist=false, 
  uniquename=init,
  sorting=anyt,
  date=year, % else bibliography will be e.g. ATO (Jul. 10, 2016c)
  labelalpha,
  maxalphanames=1
]{biblatex}

\@ifpackagelater{biblatex}{2016/09/01}{%
% Maintain backwards-compatibility between these dates
  \@ifpackagelater{biblatex}{2017/11/04}{
    \DeclareLabelalphaTemplate{\labelelement{\field{labelname}\field{labelyear}}}
    \DeclareSortingTemplate{gratt}{\sort{\field{labelalpha}}\sort{\field{labelyear}}\sort{\field{author}}}
    \ExecuteBibliographyOptions{sorting=gratt}

    \newtoggle{bbx:dowehavemorenames}
    \DeclareNameFormat{dowehavemorenames}{%
     \ifboolexpr{
       test {\ifnumequal{\value{listcount}}{\value{liststop}}}
       and
       test \ifmorenames
     }
       {\global\toggletrue{bbx:dowehavemorenames}}
       {\global\togglefalse{bbx:dowehavemorenames}}}

    \newcounter{mymaxcitenames}
    \AtBeginDocument{%
     \setcounter{mymaxcitenames}{\value{maxnames}}%
    }

    \renewbibmacro*{begentry}{%
      \begingroup
        \defcounter{maxnames}{\value{mymaxcitenames}}%
        \printnames[dowehavemorenames]{labelname}%
        \iftoggle{bbx:dowehavemorenames}
          {\printnames{labelname}%
           \setunit{\printdelim{nameyeardelim}}%
           \usebibmacro{date+extradate}%
           \space\space\newunit\newblock}
          {}%
      \endgroup
      \iftoggle{bbx:dowehavemorenames}{\renewbibmacro*{date+extradate}{}}{}%
    }
  }{
  %% biblatex version 3.7
    \DeclareLabelalphaTemplate{\labelelement{\field{labelname}\field{labelyear}}}
    \DeclareSortingScheme{gratt}{\sort{\field{labelalpha}}\sort{\field{labelyear}}}
    \ExecuteBibliographyOptions{sorting=gratt}

    \newtoggle{bbx:dowehavemorenames}
    \DeclareNameFormat{dowehavemorenames}{%
     \ifboolexpr{
       test {\ifnumequal{\value{listcount}}{\value{liststop}}}
       and
       test \ifmorenames
     }
       {\global\toggletrue{bbx:dowehavemorenames}}
       {\global\togglefalse{bbx:dowehavemorenames}}}

    \newcounter{mymaxcitenames}
    \AtBeginDocument{%
     \setcounter{mymaxcitenames}{\value{maxnames}}%
    }

    \renewbibmacro*{begentry}{%
      \begingroup
        \defcounter{maxnames}{\value{mymaxcitenames}}%
        \printnames[dowehavemorenames]{labelname}%
        \iftoggle{bbx:dowehavemorenames}
          {\printnames{labelname}%
           \setunit{\printdelim{nameyeardelim}}%
           \usebibmacro{date+extrayear}%
           \space\space\newunit\newblock}
          {}%
      \endgroup
      \iftoggle{bbx:dowehavemorenames}{\renewbibmacro*{date+extrayear}{}}{}%
    }
  }
}%
% do nothing
{}

\DeclareLanguageMapping{english}{british-apa}
\DeclareNameAlias{author}{family-given} % for last name, first name in bibliography
\DeclareFieldFormat{type}{\unskip\space} % suppress Tech Rep
\renewbibmacro{in:}{} % suppress In:
\renewbibmacro{bbx:editor}{} % suppress Ed:


%% Bibliography formatting
% separating entries
\setlength\bibitemsep{1.5\itemsep}
% format bibliography
\renewcommand*{\bibfont}{\footnotesize\raggedright}

% Allow URLs to break on any character
  % Increase penalty for page-breaks within entry from 5000 to 10,000 (infinity)
  \patchcmd{\bibsetup}{\interlinepenalty=5000}{\interlinepenalty=10000}{}{}
\let\origbibsetup\bibsetup
\renewcommand{\bibsetup}{%
  \origbibsetup%
  \expandafter\def\expandafter\UrlBreaks\expandafter{\UrlBreaks%  save the current one
    \do\a\do\b\do\c\do\d\do\e\do\f\do\g\do\h\do\i\do\j%
    \do\k\do\l\do\m\do\n\do\o\do\p\do\q\do\r\do\s\do\t%
    \do\u\do\v\do\w\do\x\do\y\do\z\do\A\do\B\do\C\do\D%
    \do\E\do\F\do\G\do\H\do\I\do\J\do\K\do\L\do\M\do\N%
    \do\O\do\P\do\Q\do\R\do\S\do\T\do\U\do\V\do\W\do\X%
    \do\Y\do\Z}
}
\DeclareFieldFormat{url}{\textcolor{blue}{\url{#1}}}


%% Citation tweaking
\DeclareFieldFormat{titlecase}{#1}
\setlength\bibhang{.5in}
\renewcommand*{\bibnamedash}{%
  \ifdimless{\leftmargin}{0.75em}
    {\mbox{\textemdash\space}}
    {\makebox[\leftmargin][l]{%
       \ifdimless{\leftmargin}{1.25em}
         {\textendash}
         {\rule{0.8\bibhang}{.2pt}}}}}
% Hyperlinks entire citation label for most citaiton commands 
% http://tex.stackexchange.com/questions/15951/hyperlink-name-with-biblatex-authoryear-biblatex-1-4b
\DeclareFieldFormat{citehyperref}{%
  \DeclareFieldAlias{bibhyperref}{noformat}% Avoid nested links
  \bibhyperref{#1}}

\DeclareFieldFormat{textcitehyperref}{%
  \DeclareFieldAlias{bibhyperref}{noformat}% Avoid nested links
  \bibhyperref{%
    #1%
    \ifbool{cbx:parens}%
      {\bibcloseparen\global\boolfalse{cbx:parens}}%
      {}}}

\savebibmacro{cite}
\savebibmacro{textcite}

\renewbibmacro*{cite}{%
  \printtext[citehyperref]{%
    \restorebibmacro{cite}%
    \usebibmacro{cite}}}

\newbibmacro*{cite:title}{%
  \ifsingletitle
    {}
    {\printtext[bibhyperref]{%
       \printfield[citetitle]{labeltitle}}}}

\renewbibmacro*{textcite}{%
  \ifboolexpr{%
    ( not test {\iffieldundef{prenote}} and
      test {\ifnumequal{\value{citecount}}{1}} )
    or
    ( not test {\iffieldundef{postnote}} and
      test {\ifnumequal{\value{citecount}}{\value{citetotal}}} )
  }%
    {\DeclareFieldAlias{textcitehyperref}{noformat}}
    {}%
  \printtext[textcitehyperref]{%
    \restorebibmacro{textcite}%
    \usebibmacro{textcite}}}



% The following lines give the correct footcite Author (Year) but make the ibid erroneous
% Author (ibid) % use ifciteibid
\DeclareCiteCommand{\footcite}[\mkbibfootnote]
  {\boolfalse{cbx:parens}}
  {\usebibmacro{citeindex}%
  % https://github.com/grattaninstitute/AP-Housing-affordability-2017/commit/55757ed2c625d4e18ada5a71edfdb5fbd88f56d1
  % Ibid. -> Ibid\adddot
  % Otherwise the . in Ibid. is interprted as the end of a sentence
  % which means the first letter in a postnote will be uppercase
  \ifciteibid{Ibid}{%  % for ibidem
   \iffirstcitekey
     {\setcounter{textcitetotal}{1}}
     {\stepcounter{textcitetotal}%
      \textcitedelim}%
   \usebibmacro{textcite}}%
   }
  {\ifbool{cbx:parens}
     {\bibcloseparen\global\boolfalse{cbx:parens}}
     {}}
  {\usebibmacro{textcite:postnote}}
\newcommand{\gcite}[1]{{\citeauthor{#1} (\citedate{#1})}}
% removes intersentence space after et al.
%\xpatchcmd\citeauthor{\begingroup}{\begingroup\aftergroup\@}{}{}
%\xpatchcmd\textcite{\begingroup}{\begingroup\aftergroup\@}{}{}
\DefineBibliographyStrings{british}{
  ibidem = {ibid},
  andothers = {et al\hskip 0pt}
}


% biblatex ShareLaTeX version control
% 2016/09/17
\@ifpackagelater{biblatex}{2016/09/01}{\ExecuteBibliographyOptions{giveninits=true}}{\ExecuteBibliographyOptions{firstinits=true}}
% Patch for issue 
\AtBeginDocument{%
  \apptocmd\@floatboxreset
    {\booltrue{citetracker}%
     \booltrue{pagetracker}}
    {}
    {\blx@err@patch{floats}}
}

\RequirePackage{multicol}
\newcommand{\logoskip}{\par\vspace{16pt}}
\newcommand*{\ReportNumber}[1]{\renewcommand*{\@ReportNumber}{#1}}
\newcommand{\@ReportNumber}{}

\AtBeginDocument{%
  \@checkReportNumberdefined{}
}




%% BibLaTeX needs to precede acknowledgements (otherwise acknowledgements can't contain citations).

% Allow MONTH/YEAR to be defined; otherwise by default the date of compilation
\newcommand*{\MONTH}[1]{\renewcommand*{\@MONTH}{#1}}
\newcommand{\@MONTH}{%
  \ifcase\the\month
  \or January% 1
  \or February% 2
  \or March% 3
  \or April% 4
  \or May% 5
  \or June% 6
  \or July% 7
  \or August% 8
  \or September% 9
  \or October% 10
  \or November% 11
  \or December% 12
  \fi}

\newcommand*{\YEAR}[1]{\renewcommand*{\@YEAR}{#1}}
\newcommand{\@YEAR}{\the\year}

\newcommand\acknowledgements[1]{\renewcommand\@acknowledgements{#1}}
\newcommand\@acknowledgements{}

\newcommand*{\ISBN}[1]{\renewcommand*{\@ISBN}{#1}}
\newcommand{\@ISBN}{}

\RequirePackage{tabularx}
\AtBeginEnvironment{tabular}{\small}
\AtBeginEnvironment{tabularx}{\small}
\newcolumntype{R}{>{\RaggedLeft\arraybackslash}X}
\newcolumntype{Q}{>{\raggedleft\arraybackslash}X}
  
\AtBeginDocument{%
  \pagestyle{empty}
  \let\mytitle\@title
  \let\mysubtitle\@subtitle
  \let\myauthor\@author
  
  % TitlePage
  \onecolumn
  \AddToShipoutPicture*{\BackgroundPic}
  \phantom{.}\hfill\titleTH
  \ClearShipoutPicture
  \clearpage
 
  % use microtype
  
  %
% Following page.
\@pagetwo
\clearpage
\pagestyle{scrheadings}
}

% Table of contents
\setcounter{tocdepth}{0}

\newlength{\currentparskip}

% Designing the overview
% ---
% We define a new environment that take as its contents the text of the 
% overview, placing it with a chapter* heading 'Overview' all on the same
% page. 
\newlength{\overviewExtra}
\setlength{\overviewExtra}{0pt}
\newenvironment{keyfindings}[1][]%
% vspace{-24.5pt} should align Overview baseline with chapter baseline
% Putting \label outside \addchap causes inconsistent chapter breaks
  {\onecolumn\vtop to 0pt\bgroup\ifstrempty{#1}{\vspace{-24.5pt}}{\vspace{#1}}\addchap{Key findings\label{chap:KeyFindings}}\addtolength{\columnsep}{\overviewExtra}}%
  {\addtolength{\columnsep}{-\overviewExtra}\vss\egroup\hfill}
  
\newenvironment{summary}%
  {\onecolumn\vtop to 0pt\bgroup\vspace{-25pt}\chapter*{Summary}\begin{multicols}{2}}%
  {\end{multicols}\vss\egroup\hfill}
  
\newenvironment{onepager}[1]%  
  {\onecolumn\vtop to 0pt\bgroup\vspace{-25pt}\chapter*{#1}\begin{multicols}{2}}%
  {\end{multicols}\vss\egroup\hfill}

\newlength{\recommendationExtra}
\setlength{\recommendationExtra}{0pt}
\newenvironment{recommendations}[1][]%
  {\onecolumn\vtop to 0pt\bgroup\ifstrempty{#1}{\vspace{-24.5pt}}{\vspace{#1}}\addchap{Recommendations}\label{chap:Recommendations}\addtolength{\columnsep}{\recommendationExtra}\begin{multicols}{2}}%
  {\end{multicols}\addtolength{\columnsep}{-\recommendationExtra}\vss\egroup\hfill}

\newcounter{reco}
\crefalias{reco}{rec}
\Crefname{reco}{Recommendation}{Recommendations}
\Crefname{rec}{Recommendation}{Recommendations}
\newcommand*{\recommendation}[1]{{\subsection*{Recommendation \refstepcounter{reco}\thereco: #1}}}

\newcounter{subreco}
\newcommand*{\subrecommendation}[1]{{\color{Blue}\refstepcounter{subreco}Rec.\hspace{4pt}\thereco\alph{subreco}: {#1}\hspace{0.4em}}}


\RequirePackage{tocloft}
\renewcommand{\cftchapfont}{\normalsize\normalfont}
\renewcommand{\cftchapdotsep}{4.5}
\renewcommand{\cftchappagefont}{\normalfont}
\renewcommand{\cftdot}{\normalfont .}
\renewcommand{\@pnumwidth}{1em}

% indent of list of figures
\renewcommand{\cftfigindent}{0em}

\AtBeginDocument{\renewcommand{\contentsname}{Table of contents}}
\newcommand{\contentspage}{%
  \onecolumn
  \begin{multicols}{2}
    \phantom{.}
    \vfill
    \columnbreak
    \begingroup
      \raggedright
      \tableofcontents
    \endgroup
  \end{multicols}
  %
}

\newcommand{\oneraggedpage}{\let\mytextbottom\@textbottom
  \let\mytexttop\@texttop
  \raggedbottom
  \afterpage{%
  \global\let\@textbottom\mytextbottom
  \global\let\@texttop\mytexttop}}


% Discretionary commands for consistent treatment of common abbreviations
\newcommand*{\eg}{e.g.}
\newcommand*{\ie}{i.e.}
\newcommand*{\etc}{etc.}

% Helpful for determining which labels are causing infinite reruns.
% https://tex.stackexchange.com/questions/154594/how-to-diagnose-a-permanent-labels-may-have-changed-warning?noredirect=1&lq=1

\def\@testdef #1#2#3{%
  \def\reserved@a{#3}\expandafter \ifx \csname #1@#2\endcsname
 \reserved@a  \else
\typeout{^^Jlabel #2 changed:^^J%
\meaning\reserved@a^^J%
\expandafter\meaning\csname #1@#2\endcsname^^J}%
\@tempswatrue \fi}

% To detect user's current version
\listfiles
