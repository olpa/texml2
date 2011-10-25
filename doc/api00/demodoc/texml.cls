\ProvidesClass{texml}[2011/10/25 TeXML support (api00)]
\LoadClass[a4paper]{letter}
\RequirePackage{xcolor}

\newenvironment{para}{\ignorespaces}{\par\ignorespaces}
\renewcommand\emph[1]{{\fontshape{it}\selectfont #1}}
