% LaTeX documentclass for studia publisher :
%xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
%xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
%%                                                                                                                                                                          %%
%%                                                                                                                                                                          %%
%%                                                      Fichier d'essais pour réglages .cls                                                      %%
%%                                                                                                                                                                          %%
%%        Cette version 2 de studia-Hermann est configurée pour les bibliographies "à la main"         %%
%%                                                                                                                                                                          %%
%xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
%xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
%
\def\PackageName{studia-Hermann}
\def\PackageVersion{1.2}
\def\firstversion{24/10/2010}
\def\filedate{24/10/2010}
\def\docdate{24/10/2010}
%
%%% Prélude %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%=== Identification Part
%
\NeedsTeXFormat{LaTeX2e}

\def\fileversion{\PackageVersion}
\ProvidesClass{\PackageName}[
     \filedate \space Version:\space \PackageVersion \space ]
\typeout{style Latex2e pour les articles ou actes Studia, Ivan Lavallée}
\ProvidesFile{\PackageName.cls}[\filedate \space Version:\space
\PackageVersion]
%--------------------------------------------------------------------------------------------------------------------------------------
%
%                                                               Initialisation du code
%
%--------------------------------------------------------------------------------------------------------------------------------------
% Définition des variables d'option et leur valeur par défaut
% Les tests if ne comportent pas de `@' pour pouvoir être utilisés dans les documents
% En particulier \iftreatise peut être utile

\newif\iffrenchlang   \frenchlangtrue    % ouvrage en français, false => in english
\newif\ifcropmarks    \cropmarksfalse    % affichage du contour de page et de corps du texte
\newif\ifemptyheaders \emptyheadersfalse % aucun entéte de page, mais numérotation hors cadre
\newif\iffirstbreak   \firstbreaktrue    % true => saut de page sur la page de titre
\newif\iffleqn        \fleqnfalse        % true => formules mathématiques à gauche, sinon centrées
%--------------------------------------------------------------------------------------------------------------------------------------
%
%                                                             Declaration des options
%
%--------------------------------------------------------------------------------------------------------------------------------------

\DeclareOption{english}{%
   \typeout{studia-Hermann.cls : ``english'' option}%
   \frenchlangfalse
}
% Pour caler les formules "au fer" à gauche
\DeclareOption{fleqn}{%
    \typeout{studia-Hermann.cls : ``fleqn'' option}%
    \PassOptionsToClass{fleqn}{article}%
    \fleqntrue%
}
% Permet de voir les limites de page et de texte
\DeclareOption{cropmarks}{%
   \typeout{studia-Hermann.cls : ``cropmarks'' option}%
   \cropmarkstrue%
}
% Pour supprimer les entêtes de page
\DeclareOption{empty}{%
   \typeout{studia-Hermann.cls : ``empty'' option}%
   \emptyheaderstrue %
}
% Permet de gérer le saut de premiére page
\DeclareOption{nofirstpagebreak}{%
   \typeout{studia-Hermann.cls : ``nofirstpagebreak'' option}%
   \firstbreakfalse %
}

\DeclareOption*{\typeout{*** Option ``\CurrentOption'' inconnue ***\\ Syntaxe :
documentclass[fleqn,english,cropmarks,empty,nofirstpagebreak]{studia-Hermann}}}
\PassOptionsToClass{12pt,twoside}{article}
%--------------------------------------------------------------------------------------------------------------------------------------
%
%                                                                Exécution des options
%
%--------------------------------------------------------------------------------------------------------------------------------------

\ExecuteOptions{12pt}         % Options par défaut
\ProcessOptions               % …évaluation des déclarations d'options
%--------------------------------------------------------------------------------------------------------------------------------------
%
%                                                              Blocs utilisés
%
%--------------------------------------------------------------------------------------------------------------------------------------

 \LoadClass[a4paper]{article}
 \RequirePackage{ifthen}
 \RequirePackage{amsmath}
 \RequirePackage{amsfonts}
 \RequirePackage{amssymb}
 \RequirePackage[dvips]{graphicx}
\RequirePackage[french,english]{babel}
%--------------------------------------------------------------------------------------------------------------------------------------
%
%%%%%%%%%%%%%%%%%%%%%%%%% Rajoutés par Ivan %%%%%%%%%%%%%%
%
%--------------------------------------------------------------------------------------------------------------------------------------
\usepackage{latexsym}
\usepackage{amsmath}
%\usepackage[applemac]{inputenc}
\usepackage{times}
\usepackage{algorithm}
\usepackage{tabularx}
\usepackage{framed}
\usepackage{ragged2e}
%                                         -------------------------------------------------------------------------------------------
%
%                                                                    Utilisation éventuelle de la couleur
%
%                                                                                            o0O0o
%
\usepackage{color}

\definecolor{rouge}{rgb}{1,0,0}
\definecolor{vert}{rgb}{0,1,0}
\definecolor{bleu}{rgb}{0,0,1}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%---------------------------------------------------------------------------------------------------------------------------
% Attention ! avec babel, tout appel à \selectlanguage active un espacement spécial pour
% les listes et redéfinit les noms de certaines entités (tablename, figurename....
% Mais on peut agir

\let\ORIselectlanguage\selectlanguage
\gdef\selectlanguage#1{\ORIselectlanguage{#1}\@mkstudia}
\def\french{\selectlanguage{frenchb}\@mkstudia}
\def\english{\selectlanguage{english}\@mkstudia}

%%% Fin du prélude

%%% Code des Commandes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                                                                                                 %
%   Dans l'ordre des sections de Consignes aux auteurs (cf. http://www.editions-studia.fr)     %
%                                                             ``Consignes aux auteurs ...''                                                  %
%                                                                                                                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------------------------------------- <2. Présentation> ---------------------------------------------------------

% Polices du corps du texte

% L'encodage recommandé est T1, notamment pour les guillemets français
% Mais certaines installations n'acceptent pas cet encodage ou nécessitent
% trop de constructions de polices pour dvips ou xdvi : je ne l'impose pas
% Il semble que les guillemets français puissent être obtenus par
% \leftguillemets \rightguillemets même sans l'encodage T1.
% En cas de problème avec certains caractères pour certaines polices
% forcer l'encodage à T1 ou OT1 OU SUPPRIMER cet encodage.
% - soit ici,
%\usepackage[T1]{fontenc}
%\usepackage[latin1]{inputenc}
%\usepackage[latin]{inputenc}

% - soit dans le prélude du document, par exemple avec \usepackage[T1]{fontenc}

% Latex/Unix: Times romain
\renewcommand{\rmdefault}{ptm}  % Postscript Times
\renewcommand{\sfdefault}{phv}  % Postscript Helvetica
\renewcommand{\ttdefault}{pcr} % Police Courier : un peu large
\renewcommand{\ttdefault}{cmtt} % Police Latex tt : plus jolie et étroite,

\newcommand{\FonteTexte}{\normalfont\fontsize{10}{12}\selectfont}

\newlength{\interligne}       % Ne change pas dans ce style: quand on parle
\setlength{\interligne}{12pt} % d'une ligne blanche, c'est ça : 4.22mm
% On peut utiliser cette commande dans le texte, par exemple
% \setlength{\baselineskip}{1\interligne} pour revenir à l'espacement normal
\newcommand{\@espace}{\hspace*{0.5em}}  
% <2.1. Format> --------------------------------------------------------------
%--------------------------------------------------------------------------------------------------------------------------------
%                          Dimensionnements
%--------------------------------------------------------------------------------------------------------------------------------
%                                     Page
%--------------------------------------------------------------------------------------------------------------------------------
% Format des pages (pour papier a4 ou letter)
% Rappel : A4 = 210mm x 297 mm USLETTER = 8.5in x 11in = 215.9mm * 279.4mm
% soit USLETTER = 6mm de plus dans la marge droite et 17.6mm de moins dans le bas de page
\setlength{\textwidth}{120mm}  % 120mm = 341pt
\setlength{\textheight}{190mm} % 190mm = 540pt (45 lignes * 12pt) = 189.78mm
%--------------------------------------------------------------------------------------------------------------------------------
%
%                                     Texte
%
%--------------------------------------------------------------------------------------------------------------------------------
% <2.1.1 Marges et Format> ---------------------------------------------------

%--- Dimensions horizontales

\setlength{\hoffset}{-1in}            % à ajuster par dvips selon imprimante
% ancien \setlength{\oddsidemargin}{45mm}      % pour que la marge de gauche
% ancien \setlength{\evensidemargin}{45mm}     % soit exactement de 45mm
 \setlength{\oddsidemargin}{40mm} 
  \setlength{\evensidemargin}{40mm} 
\setlength{\marginparsep}{0mm}     % 
  % Largeur de la marge de droite pour papier:
  %  - A4     : largeur papier = 210 mm => 210 - (120+45)  = 45  mm
  %  - Letter : 8.5in = 8.5*25.4mm = 215.9mm => 215.9-165  = 50.9mm

%--- Dimensions verticales

\setlength{\voffset}{-1in}    % à ajuster selon imprimante pour que la marge
                              % au dessus du texte (page 2) soit exactement
                              % de 53.5mm

\setlength{\topmargin}{44mm}   % distance bord sup feuille au haut de l'en-tête
\settoheight{\headheight}{2mm} %
\setlength{\headsep}{7.5mm}    %
                               % 44mm + 2mm + 7.5mm = 53.5mm (marge au dessus du texte)

%--- Tracé du cadre indiquant les bords du papier une fois l'article massicoté
% au format des revues/actes Hermann (160mm x 240mm)

% Ce tracé est obtenu par l'option \documentclass[cropmarks]{studia-Hermann}
% (par défaut pas de tracé) et est commandé à partir de l'en-tête (picture).
% L'avantage de ce système est de ne pas influencer la mise en page de Latex

%-- Commandes utilisateur
\newcommand{\CropMarksOn}{\renewcommand{\@DrawPageFrame}{\@PageFrame}}
\newcommand{\CropMarksOff}{\renewcommand{\@DrawPageFrame}{}}

%-- Primitives internes
\newcommand{\@DrawPageFrame}{}
\newcommand{\@PageFrame}{%
   \thinlines%
   \setlength{\unitlength}{1mm}%
   \begin{picture}(0,0)%
   % Les mesures sont faites à partir du point de référence qui est posé à gauche
   % de l'en-tête (7.5mm au dessus du texte)
   % Les dimensions des pages massicotées sont de 150 x 220 mm
   % Donc, en format A4, les bandes supprimées au dessus sont de (297-220)/2=38.5mm
  % => distance du coin supérieur gauche au dessus du point de référence:
   % 53.5mm -7.5mm =46mm ; 46mm-38.5mm = 7.5mm
%\put(-20,17.5){\line(1,0){160}}  % Ancien massicotage
%\put(-20,17.5){\line(0,-1){240}}   % 
%   \put(140,17.5){\line(0,-1){240}}   % 
%   \put(-20,-222.5){\line(1,0){160}}  %
  %-------------------------------------- Nouveau massicotage ----------------------
 \put(-30,7,5){\line(1,0){15}}          % Gère la ligne de massicotage supérieure
 \put(135,7.5){\line(1,0){15}}          % Gère la ligne de massicotage supérieure
 \put(-15,22,5){\line(0,-1){15}}    % Gère la ligne de massicotage gauche
 \put(-15,-212,5){\line(0,-1){15}}    % Gère la ligne de massicotage gauche
 \put(135,22.5){\line(0,-1){15}}   % Gère la ligne de massicotage droite
 \put(135,-212.5){\line(0,-1){15}}   % Gère la ligne de massicotage droite
 \put(-30,-212.5){\line(1,0){15}}  % Gère la ligne de massicotage inférieure
 \put(135,-212.5){\line(1,0){15}}  % Gère la ligne de massicotage inférieure
  
   
   
   
% \put(-20,7.5){\line(1,0){160}}          % Gère la ligne de massicotage supérieure
%   \put(-15,17.5){\line(0,-1){240}}    % Gère la ligne de massicotage gauche
%   \put(135,17.5){\line(0,-1){240}}   % Gère la ligne de massicotage droite
%   \put(-20,-212.5){\line(1,0){160}}  % Gère la ligne de massicotage inférieure
%   \put(0,-7.5){\line(1,0){120}}          % marque du bord supérieur du texte
  % \put(0,-197.5){\line(1,0){120}}      % 7.5+190 = 197.5mm: bas du texte
   %\put(0,-7.7){\line(0,-1){190}}         % côté gauche du texte
   %\put(120,-7.5){\line(0,-1){190}}    % côté droit du texte
   \end{picture}%
}%

\ifcropmarks
   \CropMarksOn
\else
   \CropMarksOff
\fi

\flushbottom
%-------------------------------------------------------------------------------------------------------------------------%
%                                                                                                                                                                 %
%                                                       Style de la première page                                                           %
%                                                                                                                                                                 %
%-------------------------------------------------------------------------------------------------------------------------%
% hauteur du texte maximum: 170mm
% pied de page pour la signature, à 194mm sous le point de référence,

\newcommand{\@FonteEntete}{\normalfont\small}

\newcommand{\ps@firstpage}{%
   % Une première page est normalement impaire, mais on définit les deux...
   \renewcommand{\@oddhead}{\parbox{\textwidth}{\@DrawPageFrame}}%
   \renewcommand{\@evenhead} { \parbox{\textwidth}{\@DrawPageFrame} }%
   % Le pied de page ne sert normnalement que pour la signature de l'article
   % dont la ligne de base est à 44mm du bord inférieur de la feuille A4
    \setlength{\footskip}{10mm} % dist. bas du texte au bas du pied de page.
                                % Ainsi le haut de l'entete de page et le bas
                                % du pied de page sont tous les deux à 44mm du
                                % bord de feuille A4 (et 26.4 mm en USLetter)
   \ifemptyheaders
      \renewcommand{\@oddfoot}{}%
      \renewcommand{\@evenfoot}{}%
   \else
      \renewcommand{\@oddfoot}{\parbox{\textwidth}{\raggedright\VAR@SignatureArticle}}%
      \renewcommand{\@evenfoot}{\parbox{\textwidth}{\raggedright\VAR@SignatureArticle}}%
   \fi
}% firstpage style

%--------------------------------------------------------------------------------------------------------
%                                  Style des pages courantes
%--------------------------------------------------------------------------------------------------------

\newcommand{\ps@otherpage}{%
   \ifemptyheaders
      % Au cas où il y aurait un pied de page, celui-ci doit sortir du cadre massicoté,
      %  lequel est à 25mm sous le bas du texte. Donc 25+15mm=40mm
      \setlength{\footskip}{40mm} % réduire à 33mm pour USLETTER
      \renewcommand{\@evenhead}{\parbox{\textwidth}{\@DrawPageFrame}}%
      \renewcommand{\@oddhead}{\parbox{\textwidth}{\@DrawPageFrame}}%
      \renewcommand{\@evenfoot}{\@FonteEntete\thepage \hspace*{\fill}}%
      \renewcommand{\@oddfoot}{\hspace*{\fill}\@FonteEntete \thepage}%
   \else
      \renewcommand{\@evenhead}{%
         \parbox{\textwidth}{%
             \@DrawPageFrame\@FonteEntete\thepage\hspace{\@ESPage}\VAR@EnTeteG}}%
      \renewcommand{\@oddhead}{%
         \parbox{\textwidth}{%
             \@DrawPageFrame\hspace*{\fill}\@FonteEntete\VAR@EnTeteD%
             \hspace{\@ESPage}\thepage}}%
      \renewcommand{\@evenfoot}{}%
      \renewcommand{\@oddfoot}{}%
   \fi
}% otherpage style

\pagestyle{otherpage}% doit être placé après les définitions précédentes

%--------------------------------------------------------------------------------------------------------
%                                  2.1.2. En-tête
%--------------------------------------------------------------------------------------------------------

\newcommand{\FonteEnTete}{\normalfont\fontsize{9}{12}\selectfont}% Police des en-têtes
\newcommand{\VAR@EnTeteG}{%
  \FonteEnTete%
  Nom de la revue ou conférence (à définir par \texttt{\@BS submitted ou \@BS toappear})}
\newcommand{\VAR@EnTeteD}{%
  \FonteEnTete%
  Titre abrégé de l'article (à définir par \texttt{\@BS title[titre abrégé]\{titre\}})}
  
%--------------------------------------------------------------------------------------------------------
%
%                                  2.2 Titres et sous-titres
%
%--------------------------------------------------------------------------------------------------------

%--------------------------------------------------------------------------------------------------------
%
%                                  2.2.1 Première page de l'article
%
%--------------------------------------------------------------------------------------------------------

\newcommand{\nofirstpagebreak}{\firstbreakfalse}
\newcommand{\firstpagebreak}{\firstbreaktrue}

\newcommand{\maketitlepage}{%
%   \@redefinitions%
   \setlength{\parskip}{0pt}%
   \setlength{\parindent}{0pt}%
   \thispagestyle{firstpage}%
   % - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
  % Block 1\iere page
  
 {
  % Sous-bloc Titre-Auteurs, adressses
  {
   \par{\FonteRubrique\hfill \VAR@rubrique \hfill}
   \par\vspace*{6pt}\@filet%Filet de tête de première page ;interlignage=épaisseur du filet
  % \par\vspace*{12pt}%
   \par\vspace*{6pt}% Distance du titre au filet
   \@Titre{\VAR@title}% + vspace de 2 lignes de 12 pts
   \@SousTitre{\VAR@subtitle}% + vspace de 2 lignes de 12 pts
   \@Auteurs{\VAR@author}%
	% \par\vspace*{1\interligne}% espace avant Adresse
	 \par\vspace*{.5\interligne}% espace avant Adresse
   \@Adresse{\VAR@address}%
    \par\vspace*{1\interligne}% espace entre adresse et filet
   \par\@filet%
   \par\vspace*{2pt}% espace entre filet et le bloc <Résumé Abstract Motscles Keywords>
   } 
   % Fin du "sous-bloc première page", c'est ce qui m'a permis de "découpler" la centration des auteurs et adresses des résumé et abstract 
   \iffrenchlang
      \@Resume{\VAR@resume}%
      \par\vspace*{3pt}%
      %\@Abstract{\VAR@abstract}%
      %\par\vspace*{3pt}%
      \@MotsCles{\VAR@motscles}%
      %\par\vspace*{3pt}%
      %\@KeyWords{\VAR@keywords}%
    \else
      \@Abstract{\VAR@abstract}%
      \par\vspace*{3pt}%
      %\@Resume{\VAR@resume}%
      %\par\vspace*{3pt}%
      \@KeyWords{\VAR@keywords}%
      %\par\vspace*{3pt}%
      %\@MotsCles{\VAR@motscles}%
   \fi
   \FonteTexte%
   \par\vspace*{4pt}%
   \@filet%
   \setcounter{section}{0}%
   \setcounter{footnote}{0}%
 }
 
 %                                       Terminaison du "Bloc de première page"
 % - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -  
   
   \iffirstbreak
      \par\vspace*{\stretch{3}}\newpage%
   \else
      \par\vspace*{1\interligne}
   \fi
   % Pour éviter les interférences avec d'autres packages, il vaut mieux définir
   % les constantes suivantes après \begin{document}, par exemple ici.
   \setlength{\parindent}{5mm} % renfoncement (ou retrait)
   \setlength{\parskip}{6pt}   % 
}%\maketitlepage

\renewcommand{\maketitle}{\maketitlepage}

\newcommand{\FonteTitreEdito}{% 18 points interligné 21.6pt (120% du corps)
   \rmfamily\upshape\fontsize{18}{21.6}\selectfont}
\newcommand{\@Coordonnateurs}{Les Coordonnateurs}

\newenvironment{editorial}[5]%
%   #1 rev
%   #2 Volume
%   #3 Numero
%   #4 Année
%   #5 Prénom \textsc{Nom} des rédacteurs de l'éditorial, séparés par des \\.
%   Exemple
%   \begin{editorial}{L'Objet}{6}{1}{2000}{Pascal \textsc{ANDRE}\\Roger \textsc{ROUSSEAU}}
%    texte de l'edito si possible sur deux pages au plus
%   \end{editorial}
{%BEGIN
   \journal{#1}{#2}{#3}{#4}{-}{-}%
   \renewcommand{\@Coordonnateurs}{#5}%
   \thispagestyle{firstpage}%
   \renewcommand{\VAR@SignatureArticle}{}%
   \@shorttitle{Editorial}%
   \setlength{\parindent}{5mm} % renfoncement (ou retrait)
   \setlength{\parskip}{6pt}   % conforme à la version du 8/9/99 des consignes
   \par\vspace*{2pt}\@filet    %interlignage=épaisseur du filet
   \par\vspace*{20pt}%
   \begin{center}\FonteTitreEdito Editorial \end{center}%
   \par\vspace*{1\interligne}%
   \FonteTexte\itshape%
}{%END
   \FonteTexte\upshape%
   \begin{flushright}
   \@Coordonnateurs
   \end{flushright}
   \newpage%
}

%-------------------------------------------------------------------------------------------------------%
%                                                                                                                                         %
%                                                    2.2.1.1. Titre et sous-titre                                         %
%                                                                                                                                         %
%-------------------------------------------------------------------------------------------------------%

\renewcommand{\title}[2][]{
%usage: \title[shorttitle]{title}
   \ifthenelse{\equal{#1}{}}{%Y
      \@shorttitle{#2}%
   }{%
      \@shorttitle{#1}%
   }
   \renewcommand{\VAR@title}{#2}
}
%-------------------------------------------------------------------------------------------------------%
%                                                                                                                                         %
%                  Titre Courant (ou abrégé) pour les en-têtes de pages impaires       %
%                                                                                                                                         %
%-------------------------------------------------------------------------------------------------------%
\newcommand{\VAR@TitreAbrege}{Titre abrégé : %
             à définir par \texttt{\@backslashchar shorttitle\{...\}}}
\newsavebox{\@shtitle}
\newcommand{\@shorttitle}[1]{%
   % #1: Titre abrégé de l'article (moins de 40 caractères, pour en-tête)
   % test de la largeur < 55mm
   \sbox{\@shtitle}{\@FonteEntete #1}%
    \ifthenelse{\lengthtest{\wd\@shtitle > 55mm}}{%
     \@latex@error{short title defined by \@backslashchar title[short]{long} too long:
       (55mm max)}\@eha
    }{}
   \renewcommand{\VAR@TitreAbrege}{#1}
}

\def\subtitle#1{\renewcommand{\VAR@subtitle}{#1}}
\def\author#1{\renewcommand{\VAR@author}{#1}}
\def\address#1{\renewcommand{\VAR@address}{#1}}
\def\abstract#1{\renewcommand{\VAR@abstract}{#1}}
\def\motscles#1{\renewcommand{\VAR@motscles}{#1}}
\def\keywords#1{\renewcommand{\VAR@keywords}{#1}}
\def\resume#1{\renewcommand{\VAR@resume}{#1}}

%-- Primitives internes
\def\@BS{\symbol{"5C}}
\def\@ARGS{\{\ldots\}}
\def\@ARG#1{\{\normalfont\textit{#1}\}}
\newcommand{\@filet}{\rule{\linewidth}{0.20pt}}

%--- Signature et en-têtes de l'article :
%    peuvent être définis par \submitted, \journal ou \proceedings

\newcommand{\FonteSignature}{\normalfont\fontsize{9pt}{10.8pt}\selectfont}
\newcommand{\FonteRubrique}{%
%   \rmfamily\mdseries\scshape\fontsize{14pt}{16.8pt}\selectfont}
   \rmfamily\mdseries\fontsize{12pt}{14.4pt}\selectfont}

\newcommand{\VAR@rubrique}{}
\newcommand{\VAR@SignatureArticle}{}
\newcommand{\VAR@NomRevue}{}
\newcommand{\VAR@VolumeRevue}{}
\newcommand{\VAR@NumeroRevue}{}
\newcommand{\VAR@AnneeRevue}{}
\newlength{\@ESPage}\setlength{\@ESPage}{4mm}% 5 espaces blancs
\newcommand{\VAR@Pages}{}
\newcommand{\VAR@NumeroSoumission}{}
\newcommand{\VAR@NomActes}{}
\newcommand{\VAR@TypeArticle}{revue}

%Ancienne commande
\iffalse \newcommand{\journal}[7][]{% pour article de revue
   % #1 [rubrique]
   % #2 {revue}
   % #3 {vol}
   % #4 {num}
   % #5 {annee}
   % #6 {1ere p.}
   % #7 {dern. page}
   \renewcommand{\VAR@TypeArticle}{revue}%
   \renewcommand{\VAR@rubrique}{\uppercase{#1}}
   \renewcommand{\VAR@NomRevue}{#2}
   \renewcommand{\VAR@VolumeRevue}{#3}
   \renewcommand{\VAR@NumeroRevue}{#4}
   \renewcommand{\VAR@AnneeRevue}{#5}
 %  \renewcommand{\VAR@Pages}{#6}
 %  \ifthenelse{\equal{\VAR@Pages}{-}}{%1re page inconnue
  %    \setcounter{page}{1}
      % on augmente l'espace après numéro page pour permettre à l'éditeur de
      % surcharger le bon numéro
  %    \setlength{\@ESPage}{6mm}
%      \renewcommand{\VAR@Pages}{}
 %  }
 %  {%1ere page donnée
  %    \setcounter{page}{#6}
 %     \renewcommand{\VAR@Pages}{, pages #6\ \`a #7}
  % }
   \ifthenelse{\equal{\VAR@NumeroRevue}{\LMO}}{%signature spéciale pour LMO
        \renewcommand{\VAR@SignatureArticle}{\FonteSignature %
            \VAR@NomRevue\ -- \VAR@VolumeRevue/\VAR@AnneeRevue. %
            \VAR@NumeroRevue'\VAR@AnneeRevue\VAR@Pages}
        \renewcommand{\VAR@EnTeteG}{\FonteEnTete\VAR@NomRevue\ -- %
            \VAR@VolumeRevue/\VAR@AnneeRevue. \VAR@NumeroRevue'\VAR@AnneeRevue}
   }{\ifthenelse{\equal{\VAR@NomRevue}{\LOBJET}}{%
     % spécial pour la revue L'objet : sans mot Volume, sans numéro
      \renewcommand{\VAR@SignatureArticle}{\FonteSignature%
          \VAR@NomRevue\ -- \VAR@VolumeRevue/\VAR@AnneeRevue\VAR@Pages}
      \renewcommand{\VAR@EnTeteG}{\FonteEnTete\VAR@NomRevue\ -- %
          \VAR@VolumeRevue/\VAR@AnneeRevue.}
   }{% Autres revues
     \ifthenelse{\equal{\VAR@NumeroRevue}{}}{%pas de numéro revue
        \renewcommand{\VAR@SignatureArticle}{\FonteSignature %
            \VAR@NomRevue -- \VAR@VolumeRevue/\VAR@AnneeRevue\VAR@Pages}
        \renewcommand{\VAR@EnTeteG}{\FonteEnTete\VAR@NomRevue. %
            Volume \VAR@VolumeRevue/\VAR@AnneeRevue}
    }{% numéro de revue présent, présentation normale, style TSI :
        \renewcommand{\VAR@SignatureArticle}{\FonteSignature %
            \VAR@NomRevue. Volume \VAR@VolumeRevue\ -- n$^\circ$ \VAR@NumeroRevue/\VAR@AnneeRevue\VAR@Pages}
        \renewcommand{\VAR@EnTeteG}{\FonteEnTete\VAR@NomRevue. %
            Volume \VAR@VolumeRevue\ -- n$^\circ$ \VAR@NumeroRevue/\VAR@AnneeRevue}
   }}}
   \renewcommand{\VAR@EnTeteD}{\FonteEnTete\VAR@TitreAbrege}
}%\journal
 #\fi

\newcommand{\journal}[3]{% pour article de revue
   % #1 {revue. Volume 19 -- n∞ 1/2005,}
   % #2 {1ere p.}
   % #3 {dern. page}
   \renewcommand{\VAR@TypeArticle}{revue}%
%   \renewcommand{\VAR@rubrique}{\uppercase{#1}}
   \renewcommand{\VAR@NomRevue}{#1}
%   \renewcommand{\VAR@VolumeRevue}{#3}
%   \renewcommand{\VAR@NumeroRevue}{#4}
%   \renewcommand{\VAR@AnneeRevue}{#5}
   \renewcommand{\VAR@Pages}{#2}
   \ifthenelse{\equal{\VAR@Pages}{-}}{%1re page inconnue
      \setcounter{page}{1}
      % on augmente l'espace après numéro page pour permettre à l'éditeur de
      % surcharger le bon numéro
      \setlength{\@ESPage}{6mm}
      \renewcommand{\VAR@Pages}{}
   }{%1ere page donnée
      \setcounter{page}{#2}
      \renewcommand{\VAR@Pages}{, pages #2\ \`a #3}
   }
      \renewcommand{\VAR@SignatureArticle}{\FonteSignature\VAR@NomRevue\VAR@Pages}
      \renewcommand{\VAR@EnTeteG}{\FonteEnTete\VAR@NomRevue}
      \renewcommand{\VAR@EnTeteD}{\FonteEnTete\VAR@TitreAbrege}
}%\journal

\newcommand{\proceedings}[2]{% pour article d'actes de conférence
% usage: {#1} = nom de la conférence
%        {#2} = numéro de première page
   \renewcommand{\VAR@TypeArticle}{conference}%
   \renewcommand{\VAR@NomActes}{#1}%
   \renewcommand{\VAR@Pages}{#2}%
   \ifthenelse{\equal{\VAR@Pages}{-}}{%1re page inconnue
      \setcounter{page}{1}
      % on augmente l'espace après numéro page pour permettre à l'éditeur de
      % surcharger le bon numéro
      \setlength{\@ESPage}{6mm}
   }{%1ere page donnée
      \setcounter{page}{#2}
   }
   \renewcommand{\VAR@SignatureArticle}{}
   \renewcommand{\VAR@EnTeteG}{\FonteEnTete\VAR@NomActes}
   \renewcommand{\VAR@EnTeteD}{\FonteEnTete\VAR@TitreAbrege}
}

\newcommand{\submitted}[3][\today]{% Commun Actes ou Revue
% usage: [#1] = date (par défaut \today)
%        {#2} = nom revue ou conférence
%        {#3} = numéro de soumission (le plus souvent de 1 à 3).
   \renewcommand{\VAR@NomRevue}{#2}%
   \renewcommand{\VAR@NumeroSoumission}{%
      \ifthenelse{\equal{#3}{1}}{1\iere~}{#3\ieme~}}%
   \renewcommand{\VAR@SignatureArticle}{%
     \FonteSignature\raggedright
        \VAR@NumeroSoumission soumission \`a \textit{\VAR@NomRevue}, le #1
   }
   \renewcommand{\VAR@EnTeteG}{\FonteEnTete\VAR@NumeroSoumission soumission \`a
                               \textit{\VAR@NomRevue}}
   \renewcommand{\VAR@EnTeteD}{\FonteEnTete\VAR@TitreAbrege}
}
%--------------------------------------------------------------------------------------------------------------------------------------
%
%                                  Noms prédéfinis de revues studia Science Publications
%
%--------------------------------------------------------------------------------------------------------------------------------------

%--------------------------------------------------------------------------------------------------------------------------------------%

\newcommand{\SCU}{Studia Complexica Universalis}
\newcommand{\SIU}{Studia Informatica Universalis}

%--------------------------------------------------------------------------------------------------------------------------------------%

%--- Titre de l'article

\newcommand{\VAR@title}{Titre de l'article%
   {\par\renewcommand{\baselinestretch}{0.66}\par\FonteTexte%@@
    \textit{A définir par la commande
    \texttt{\upshape\@BS title[} titre abrégé \texttt{\upshape]\{} titre complet \texttt{\upshape\}}
    sur une ou deux lignes coupées par \@BS\@BS.\\
    Le sous-titre éventuel est défini sur une ou deux lignes par la commande\\
    \texttt{\upshape\@BS subtitle\{} sous-titre complet \texttt{\upshape\}}.}}}

\newcommand{\FonteTitre}{% 18 points interligné 21.6pt (120% du corps)
%   \rmfamily\bfseries\upshape\fontsize{18}{21.6}\selectfont}
   \rmfamily\bfseries\upshape\fontsize{18}{22}\selectfont}

\newcommand{\@Titre}[1]{{%
   % #1: Titre complet de l'article (peut incorporer des `\\')
   %\setlength{\parskip}{0pt}\flushleft%
      \setlength{\parskip}{0pt}\center%
   \FonteTitre #1%
        \par\vspace*{10pt}% 
}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modifié %%%%%%%%%%%%%
%--- Sous-Titre de l'article

\newcommand{\VAR@subtitle}{}

\newcommand{\FonteSousTitre}{% 14pt interligné 16.8pt (120% du corps)
%   \rmfamily\bfseries\upshape\fontsize{14}{16.8}\selectfont}
   \rmfamily\bfseries\upshape\fontsize{14}{17}\selectfont}

\newcommand{\@SousTitre}[1]{{%
   % #1: Sous-titre de l'article dans une police plus petite (peut incorporer des `\\')
   \setlength{\parskip}{0pt}\center%
   \FonteSousTitre #1%
  % \par\vspace*{21.2pt}% 2 lignes = 24 pts - (16.8-14)
   \par\vspace*{2pt}%
}}

%------------------------------------------------------------------------------------------------------------
%                                                                 Auteurs
%------------------------------------------------------------------------------------------------------------

\newcommand{\FonteAuteur}{% 12pt interligné 14.4pt (120% du corps)
    \rmfamily\bfseries\upshape\fontsize{12}{14.4}\selectfont}
   
\newcommand{\VAR@author}{%
   Auteur(s)\\
   {\FonteTexte\textit{A définir par la commande \texttt{\upshape\@BS author\@ARGS}.
    Dans le cas de plusieurs auteurs, séparer chaque auteur par
    \texttt{\upshape\@BS andauthor}.
    Dans le cas d'adresses différentes des auteurs, indexer chaque auteur avec des
   astérisques comme \fup{*} ou \fup{**} obtenues par
  \texttt{\upshape\@BS fup\{*\}} ou \texttt{\upshape\@BS fup\{**\}}.
}}}

\newcommand{\@Auteurs}[1]{\center{% Là j'ai rajouté \center pour centrer auteurs et adresses mais ça                      
\FonteAuteur #1%                                   %
} }

\newcommand{\andauthor}{%
   % Tiret long maigre de séparation entre deux noms d'auteurs
   {\FonteTexte\mdseries --- }}

%--- Adresse(s) de(s) auteur(s)

\newcommand{\FonteAdresse}{\rmfamily\mdseries\itshape\fontsize{10}{12}\selectfont}

\newcommand{\VAR@address}{Adresse :\\
   {\FonteTexte\textit A définir sur plusieurs lignes par la commande
    \texttt{\upshape\@BS address\@ARGS}.\\
    Email sur la dernière ligne.\\
    Dans le cas d'adresses différentes des auteurs, indexer chaque auteur avec des
   astérisques comme \fup{*} ou \fup{**} obtenues par
  \texttt{\upshape\@BS fup\{*\}} ou \texttt{\upshape\@BS fup\{**\}}.\\
   Voir aussi <<~Consignes aux auteurs~>>
}}

\newcommand{\@Adresse}[1]{\FonteAdresse #1}

%--- Résumé et Abstract

\newcommand{\FonteResume}{\rmfamily\mdseries\itshape\fontsize{9}{11}\selectfont}

% Studia demande des petites capitales italiques postscript
% Comme cette police n'existe pas sous Unix,
% on utilise l'italique de Times en utilisant des capitales réduites de 2 points.
% Si un utilisateur dispose d'une police en petites capitales italiques adaptée
% à Times, il peut facilement faire l'adaptation.

\newcommand{\FonteTitreResume}{%
%   \rmfamily\mdseries\itshape\fontsize{7}{11}\selectfont}
    \rmfamily\mdseries\itshape\fontsize{9}{10}\selectfont}

\newcommand{\VAR@resume}{%
   {\FonteTexte\textit{A dÈfinir par la commande \texttt{\upshape\@BS resume\@ARGS}}}}

\newcommand{\@TitreResume}[2]{\FonteTitreResume \noindent #1#2\FonteResume}

\newcommand{\@Resume}[1]{{\french\@TitreResume{\bf RÈsumÈ}{.} #1}}

%--- Abstract

\newcommand{\VAR@abstract}{%
   {\FonteTexte\textit{A dÈfinir par la commande \texttt{\upshape\@BS abstract\@ARGS}}}}

\newcommand{\@Abstract}[1]{{\english\@TitreResume{\bf Abstract}{.} #1}}

%--- Mots-clés

\newcommand{\VAR@motscles}{%
   {\FonteTexte\textit{A dÈfinir par la commande \texttt{\upshape\@BS motscles\@ARGS}}}}

\newcommand{\@MotsCles}[1]{{\french\@TitreResume{\bf Mots-ClÈs}{~:} #1}}

%--- Keyswords

\newcommand{\VAR@keywords}{%
   {\FonteTexte\textit{A dÈfinir par la commande \texttt{\upshape\@BS keywords\@ARGS}}}}

\newcommand{\@KeyWords}[1]{{\english\@TitreResume{\bf Keywords}{:} #1}}

% <2.2.2 Titre des paragraphes ou titre d'´ inter ª (sections)>----------------

% cf \Latex Companion 2.3 pp 18-31
% Les définitions suivantes visent à donner la police particulière
% exigée par studia uniquement dans les titres de section et non pas dans les
% références. Ainsi, 2.2. apparaîtra en gras droit dans le titre et en
% police normale dans les références par \label ... \ref.
% Si l'on veut la même présentation dans les références de sections,
% il faut tout simplement incorporer la définition voulue dans celle
% de \thesection ... \thesubsection

% On numérote jusqu'à la section de niveau 4
\setcounter{secnumdepth}{4}

% Pour les titres de section sur plusieurs lignes, Hermann ne veut pas d'indentation
\def\@hangfrom#1{%
       \setbox\@tempboxa\hbox{{#1}}%
%       \hangindent \wd\@tempboxa\noindent\box\@tempboxa %% ORIGINAL de latex.ltx
       \hangindent \wd 0 \noindent\box\@tempboxa %% MODIF
}

% Attention : les dimensions des interlignes before et after skip
% doivent tenir compte du parskip qui est de 6pts et qui s'ajoute aux valeurs
% indiquées

%--- Niveau 1: section : gras,  beforeskip = 2 lignes, afterskip = 1 ligne

\newcommand{\FonteSectionI}{\normalsize\normalfont\bfseries\raggedright}%

\renewcommand{\thesection}{\arabic{section}}%
% Il faut enlever le parskip de 6pts aux valeurs des skips (2 lignes=24-6=18pts...)
\renewcommand{\section}{%
   \renewcommand{\@seccntformat}[1]{\thesection.\hspace{0.5em}}%
   \@startsection{section}% % nom de l'inter
   {1}%                     % niveau de l'inter
   {0pt}%                   % l'indentation du titre et du texte suivant
   {18pt plus 2pt minus 2pt}% beforeskip
   {6pt plus 1pt minus 1 pt}% afterskip
   {\FonteSectionI}%        % style
}

%--- Niveau 2: sous-section : gras+ital, beforeskip = 2 lignes, afterskip = 1 ligne

\newcommand{\FonteSectionII}{\normalsize\normalfont\bfseries\itshape\raggedright}%

\renewcommand{\thesubsection}{\thesection.\arabic{subsection}}%
\renewcommand{\subsection}{%
  \renewcommand{\@seccntformat}[1]%
               {{\normalsize\normalfont\bfseries\thesubsection.\hspace{0.5em}}}%
  \@startsection%
   {subsection}%            % nom de l'inter
   {2}%                     % niveau de l'inter
   {0pt}%                   % l'indentation du titre et du texte suivant
   {18pt plus 2 pt minus 1pt}% beforeskip
   {6pt plus 1pt minus 1pt}  %afterskip
   {\FonteSectionII}}%      % style

%--- Niveau 3: sous-sous-section : italique, beforeskip=1 lignes, afterskip=1 ligne

\newcommand{\FonteSectionIII}{\normalsize\normalfont\itshape\raggedright}%

\renewcommand{\thesubsubsection}{\thesubsection.\arabic{subsubsection}}%
\renewcommand{\subsubsection}{%
  \renewcommand{\@seccntformat}[1]{%
               {\normalsize\normalfont\thesubsubsection.\hspace{0.5em}}}%
  \@startsection%
   {subsubsection}%         % nom de l'inter
   {3}%                     % niveau de l'inter
   {0mm}%                   % l'indentation du titre et du texte suivant
   {6pt plus 1pt minus 1pt} % beforeskip
   {0.1pt}%                 % afterskip (0pt supprimerait le passage à la ligne)
   {\FonteSectionIII}}%     % style


%--- Niveau 4: paragraphes: police normale, beforeskip=1 ligne, afterskip=1 ligne

\newcommand{\FonteSectionIV}{\normalsize\normalfont\raggedright}%

\renewcommand{\theparagraph}{\thesubsubsection.\arabic{paragraph}}%
\renewcommand{\paragraph}{%
  \renewcommand{\@seccntformat}[1]{%
               {\normalsize\normalfont\theparagraph.\hspace{0.5em}}}%
  \@startsection%
   {paragraph}%             % nom de l'inter
   {4}%                     % niveau de l'inter
   {0mm}%                   % l'indentation du titre et du texte suivant
   {6pt plus 1pt minus 1 pt}% beforeskip
   {0.1pt}%                 % afterskip  (0pt supprimerait le passage à la ligne)
   {\FonteSectionIV}}%      % style

%                         2.3 Alinéas       

% Disable single lines at the start of a paragraph
\clubpenalty = 10000
% Disable single lines at the end of a paragraph
 \widowpenalty = 10000
 \displaywidowpenalty = 10000
 \sloppy
 \raggedbottom

%                           2.4 Notes de bas de page

% corps 9pt/11pt, numérotation décimale
% séparées du texte par 1 ligne blanche
% filet maigre de 2.5 cm

\newcommand{\FonteNoteBasPage}{\fontsize{9}{10.8}\selectfont\normalfont}
\renewcommand{\footnotesize}{\FonteNoteBasPage}
\addtolength{\skip\footins}{6pt} % = 12pt = 1 ligne avant le filet

\renewcommand{\footnoterule}{%
% Redéfinition du filet de notes de bas de page.
% Doit avoit une hauteur totale virtuelle nulle (cf Latex Companion p. 72)
\par\vspace*{-12.3pt}% Hauteur totale => donnera une hauteur virtuelle nulle
\noindent\rule{2.5cm}{0.25pt}\vspace*{6pt} % => hauteur totale 9.3pt
                                          % Hauteur espace sous filet = 6pt
}

\setlength{\footnotesep}{2pt} % Espace vertical avant chaque note (strut)

\newcommand{\@Myfnmark}{% Redéfinit la marque de footnote comme un indice
      % french.sty redéfinit bêtement \@makefntext => donc j'utilise une autre macro.
      \mbox{$\arabic{footnote}$. }%
}

\renewcommand{\@makefntext}[1]{%
      \noindent\@Myfnmark#1%
}%

\def\@thefnmark{\normalsize\arabic{footnote}}
%
%%%                                                                    2.6 Formules                                                                         %%%
%
%--------------------------------------------------------------------------------------------------------------------------------------%

% En mode standard, les modes equation, eqnarray et eqnarray*
% fournissent bien une ligne blanche au dessus et au dessous et la numerotation
% collée sur la marge de droite.
% La seule adaptation est la numérotation [n] au lieu de (n).
%\renewcommand{\@eqnnum}{\reset@font\rmfamily [\theequation]}
%\renewcommand{\tagform@}[1]{\reset@font\rmfamily [#1]} % pour align

%%%%%% retour au mode standard %%%%%%%%%%%%%%%%%%%%%%%



% Définition des espaces avec l'option fleqn :
% L'option fleqn (\documentclass[fleqn]...) place les formules indentées à
% gauche. Par défaut, les formules sont centrées.

\iffleqn
    %\setlength{\topsep}{1\baselineskip}  % espace supplém. au dessus/dessous des formules.
    \setlength{\mathindent}{10mm}        % indentation des formules
\fi

% Définition des espaces sans l'option fleqn :

\setlength{\jot}{0.5\baselineskip}      % espace entre les lignes de eqnarray
\setlength{\abovedisplayskip}{0mm}      % espace supplémentaire au dessus des longues formules
\setlength{\abovedisplayshortskip}{0mm} % idem, formules courtes
\setlength{\belowdisplayskip}{0mm}      % espace supplémentaire au dessous des longues formules
\setlength{\belowdisplayshortskip}{0mm} % idem, formules courtes

%--- Utilitaires facultatifs

% Utiliser la commande \eqncont pour terminer une ligne de formule
% sur plusieurs lignes : supprime la numérotation + espace vertical plus serré.
\newcommand{\eqncont}{\nonumber \\[0\baselineskip]}

%%% Retardement du pagebreak

%  Pour retarder le passage automatique à la page suivante de 1 ou 2 lignes,
%  utiliser le couple de commandes \delaynewpage{nb lignes} ... \forcenewpage
%  Attention! cette technique est du bricolage, mais cependant indispensable
%  pour une présentation optimale d'un document. (indiquée par Lamport).
%  IL NE FAUT L'EMPLOYER QUE TOUT ¿ LA FIN, JUSTE AVANT L'IMPRESSION FINALE.
%  En principe ne pas retarder de plus de 2 lignes !

\newcommand{\delaynewpage}[1]{\enlargethispage{#1\baselineskip}}
   % ¿ utiliser sur la page à allonger, à un endroit bien visible
   % avant le premier paragraphe ou la figure à faire tenir sur la page.
   % Provoque une descente des notes de bas de page s'il y en a !

\newcommand{\forcenewpage}{\pagebreak\noindent}
   % ¿ utiliser à l'endroit précis où la coupure doit être forcée.
   % on peut placer cette commande en début de ligne, avec suite du texte
   % sur la ligne suivante pour faciliter son repérage

% Listes --------------------------------------------------------------------------------------------------

% Espaces communs aux listes numérotées (enumerate) ou non (itemize)
% Attention !!! Ces espacements ne peuvent être effectifs que si Babel ne les redéfinit
% pas dans \@trivlist, comme il tente de l'imposer. Il faut ABSOLUMENT
% exécuter \bbl@nonfrenchlistspacing avant le début du document (\AtBeginDocument).

\def\mkstudialist{%
   \gdef\itemize{\studiaitemize}\gdef\enditemize{\endstudiaitemize}%
   \gdef\enumerate{\studiaenumerate}\gdef\endenumerate{\endstudiaenumerate}%
}

\newcommand{\studiaItemizeSpacing}{%
% espacements horizontaux
   \leftmargin=0pt
   \labelsep=1mm
   \ifnum \@itemdepth < 2  % niveau 0 ou 1
      \setlength{\itemindent}{\itemindenti}
      \listparindent=5mm
   \else                   % niveau 2
      \itemindent=\itemindentii
      \listparindent=10mm
   \fi
   \labelwidth=0pt
   \rightmargin=0pt
  % espacements verticaux des listes
   \itemsep=0pt
   \topsep=0pt
   \parskip=6pt
   \partopsep=0pt
   \parsep=3pt
% pénalités
   \sloppy%
   \widowpenalty4000%ligne isolée en début de page
   \clubpenalty4000%ligne isolée à la fin d'une page
}

\newcommand{\displaylistdimen}{
     itemdepth: \the\@itemdepth ;\\
     topsep: \the\topsep ;\\
     itemsep: \the\itemsep ;\\
     parskip: \the\parskip ;\\
     partopsep: \the\partopsep ;\\
     parsep: \the\parsep.
}

% Listes non numérotées (itemize) -------------------------------------------------------------------------

\newlength{\labelwidthi}      % largeur de la boîte contenant le retrait de 5mm + tiret moyen
\settowidth{\labelwidthi}{--} % environ 2mm, tiret moyen
\addtolength{\labelwidthi}{5mm} % retrait + largeur du tiret moyen, environ 7mm

\newlength{\itemindenti}      % décalage du texte de 1ère ligne depuis marge gauche
\setlength{\itemindenti}{\labelwidthi} % = labelwidthi
\addtolength{\itemindenti}{1mm}          %   + labelsep

\newlength{\labelwidthii}       % largeur de la boîte contenant le retrait de 10mm + tiret court
\settowidth{\labelwidthii}{-}   % environ 1mm, tiret court
\addtolength{\labelwidthii}{10mm} % retrait + largeur du tiret court, environ 11mm

\newlength{\itemindentii}      % décalage du texte de 1ère ligne depuis marge gauche
\setlength{\itemindentii}{\labelwidthii} % = labelwidthii
\addtolength{\itemindentii}{1mm}           %   + labelsep

\newenvironment{studiaitemize}{% liste à deux niveaux seulement
 \ifnum \@itemdepth > 1 \@toodeep % ici, \@itemdepth = niveau -1
 \else%
    \renewcommand{\labelitemi}{{\makebox[\labelwidthi][r]{--}}}%
    \renewcommand{\labelitemii}{{\makebox[\labelwidthii][r]{-}}}%
    \advance\@itemdepth \@ne%
    \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
    \begin{list}%
       {\csname\@itemitem\endcsname}%
       {\def\makelabel##1{\llap{##1}}% ##1 signifie arg 1 de list et non de studiaitemize
        % définir les espaces ici et nulle part ailleursn
        \studiaItemizeSpacing %
       }%\item \displaylistdimen
 \fi}{\end{list}}%

% Listes numérotées ---------------------------------------------------------------------------------------

\newlength{\enumwidthi}      % largeur de la boîte contenant le retrait de 5mm + n.
\settowidth{\enumwidthi}{5)} % environ 2mm, chiffre le plus large
\addtolength{\enumwidthi}{5mm} % retrait + largeur du tiret moyen, environ 7mm

\newlength{\enumindenti}      % décalage du texte de 1ère ligne depuis marge gauche
\setlength{\enumindenti}{\enumwidthi} % = enumwidthi
\addtolength{\enumindenti}{1mm}          %   + enumsep

\newlength{\enumwidthii}         % largeur de la boîte contenant le retrait de 10mm + tiret court
\settowidth{\enumwidthii}{b)}   % environ 1mm, tiret court
\addtolength{\enumwidthii}{10mm} % retrait + largeur du tiret court, environ 11mm

\newlength{\enumindentii}      % décalage du texte de 1ère ligne depuis marge gauche
\setlength{\enumindentii}{\enumwidthii} % = enumwidthii
\addtolength{\enumindentii}{1mm}           %   + enumsep

\newenvironment{studiaenumerate}{% enumération à un niveau
 \ifnum \@itemdepth >1 \@toodeep
 \else%
      \advance\@enumdepth \@ne%
      \advance\@itemdepth \@ne%
      \edef\@enumctr{enum\romannumeral\the\@enumdepth}%
      \begin{list}%
         {\csname label\@enumctr\endcsname}%
         {%
          \usecounter{\@enumctr}%
          \def\makelabel##1{\llap{##1}}% ##1 signifie arg 1 de list et non de studiaenumerate
          % définir les espaces horizontaux ici
          \studiaItemizeSpacing%
          \ifnum \@itemdepth =1
             \setlength{\itemindent}{\enumindenti}
          \else
             \setlength{\itemindent}{\enumindentii}
          \fi
          \renewcommand{\labelenumi}{{\makebox[\labelwidthi][r]{\arabic{enumi})}}}
          \renewcommand{\labelenumii}{{\makebox[\labelwidthii][r]{\alph{enumii})}}}%
         }
   \fi}{\end{list}}

%%%%%%%%%%%% OLD %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\def\ITEMIZE{% liste à 3 niveaux: labelitemi=``---''
  \ifnum \@itemdepth >3 \@toodeep%
  \else%
      \renewcommand{\labelitemi}{{---}}%
      \renewcommand{\labelitemii}{\@labelindent{--}}%
      \renewcommand{\labelitemiii}{\@labelindent\@labelindent{-}}%
      \advance\@itemdepth \@ne%
      \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
      \list%
         {\csname\@itemitem\endcsname}%
         {%
          \renewcommand{\@curlabel}{\csname\@itemitem\endcsname}%
          \def\makelabel##1{\llap{##1}}%
          \mkstudialist%
         }%
  \fi%
}
\let\endITEMIZE =\endlist

% Redéfinition de quote pour réduire l'espace vertical à 6 pts

\let\quoteORI\quote
\let\endquoteORI\endquote
\renewenvironment{quote}{%
\par\vspace{-6pt}%
\begin{quoteORI}%
}{%
\end{quoteORI}%
\par\vspace{-6pt}%
}

%<2.8 Remarques ou notes> -----------------------------------------------------

\newcommand{\remark}[1]{{\setlength{\parskip}{0pt}%
   \par\noindent\textsc{\remarkname}.\ ---\ #1\\}}
\newcommand{\note}[1]{{\setlength{\parskip}{0pt}%
    \par\noindent\textsc{\notename}.\ ---\ #1\\}}
%< 2.8 bis Remarques francisée > --------------------------------------
\newcommand{\remarque}[1]{{\setlength{\parskip}{0pt}%
    \par\noindent\textsc{\remarkname}.\ ---\ #1\\}}

%<2.9 Typographie> ------------------------------------------------------------

%<2.10 acknowledgements>  ---------------------------------------------------------
\newcommand{\acknowledgements}[1]{{
   \setlength{\parskip}{0pt}%
   \par\vspace{2\interligne}\noindent%
   \FonteTexte \acknowledgementsname%
   \nopagebreak[4]%
   \par\vspace{1\interligne}#1%
}}
%< 2.11 Remerciements > --------------------------------------------------------
% J'ai francisé le commande ci-dessus, acknowledgement étant un mot barbare à manipuler pour des % frenchies ---------------------------------------------------------------------------
\newcommand{\remerciements}[1]{{
   \setlength{\parskip}{0pt}%
   \par\vspace{2\interligne}\noindent%
   \FonteTexte \acknowledgementsname%
   \nopagebreak[4]%
   \par\vspace{1\interligne}#1%
}}
%======================= Biblio à la main =======================================
% Code importé du package Harvard.sty pour obtenir des citations bibliographiques du style (Machin, 2001)

%<2.13 Biographie> ----------------------------------------------------------------------
% Il faut 2 lignes au dessus de la première biographie et 1 ligne
% de séparation entre les biographies.

\newcommand{\@fonteBiographie}{%
   \rmfamily\mdseries\itshape\fontsize{9}{11}\selectfont}
\newcommand{\@firstbiography}{yes}

\newcommand{\biography}[2]{
  % #1 = Nom de l'auteur,
  % #2 = biographie enchaînée derrière le nom d'auteur, typiquement
  %      ´ est chercheur au laboratoire... ª
  \ifthenelse{\equal{\@firstbiography}{yes}}{% première biographie
  \renewcommand{\@firstbiography}{no}%
  \par\vspace{18pt}\noindent% + 6pt de \parskip
  }{% biographies suivantes
  \par\vspace{6pt}\noindent% + 6pt de \parskip
  }%
  {\@fonteBiographie \textbf{#1} #2
}}

% <3. Remise des articles> ---------------------------------------------------

% <3.1 Journal de bord> ------------------------------------------------------

\newcommand{\andeditor}{% mot `et' en police normale
   {{\FonteTexte et\ }}}

\newcommand{\logbook}[3]{{% pour revues seulement
 % #1: date réception
 % #2: date révision
 % #3: rédacteur responsable
 \setlength{\parskip}{0pt}%
 \par\vspace{1\interligne}%
 \begin{flushright}%
    Article reÁu le #1.\\
    Version rÈvisÈ le #2.\\
    RÈdacteur responsable~: \textsc{#3}
 \end{flushright}%
}}

% <3.2 Annexe pour le service de fabrication> ------------------------------------

\newcommand{\adressestudia}{
\begin{flushright}
{\setlength{\parsep}{0mm} \fbox{\parbox{70mm}{
\begin{flushright}
\textsc{Service \'Editorial -- Studia Universalis}\\ 41 rue Gay Lussac, 75005
Paris\\ TÈl : 01-44-10-84/83\\ mel
: contact@complexica.net\\ Serveur web : http://studia.complexica.net
\end{flushright}
}}}
\end{flushright}}

\newcommand{\VAR@titletranslation}{}
\newcommand{\VAR@Phone}{}
\newcommand{\VAR@Fax}{}
\newcommand{\VAR@Email}{}

\newcommand{\publisher}[3]{
   % Pour construire la page d'annexe pour le service de fabrication
   % usage: {#1} = traduction du titre
   %        {#2} = numero de téléphone des auteurs
   %        {#3} = fax des auteurs
   %        {#4} = Email des auteurs
   %\renewcommand{\VAR@titletranslation}{#1}
   \renewcommand{\VAR@Phone}{#1}
   \renewcommand{\VAR@Fax}{#2}
   \renewcommand{\VAR@Email}{#3}

   \let\myenddocument\enddocument
   \def\enddocument{
         \pagebreak \pagestyle{empty}\enlargethispage{80mm}
   %\renewcommand{\@enddocumenthook}{
      \parbox{\textwidth}{
       \fbox{\parbox{\textwidth}{\begin{center}
           \textbf{\large ANNEXE POUR LA FABRICATION}\\
          A FOURNIR PAR LES AUTEURS AVEC UN EXEMPLAIRE PAPIER\\
          DE LEUR ARTICLE 
        \end{center}}}\\[10mm]
        %\setcounter{section}{0}
      \ifthenelse{\equal{\VAR@TypeArticle}{revue}}{
         \textsc{1. Article pour la revue~:}
         \begin{quote}\textit{\VAR@NomRevue}\end{quote}\vspace*{3mm}
      }{
         \textsc{1. Article pour les actes~:}
         \begin{quote}\textit{\VAR@NomActes}\end{quote}\vspace*{3mm}
      }
      \textsc{2. Auteurs~:}
         \begin{quote}\textit{\VAR@author}\end{quote}\vspace*{3mm}
      \textsc{3. Titre de l'article~:}
         \begin{quote}\textit{\VAR@title}\end{quote}\vspace*{3mm}
      \textsc{4. Titre \underline{abrÈgÈ} pour le haut de page \underline{moins de 40 signes}~:}
         \begin{quote}\textit{\VAR@TitreAbrege}\end{quote}\vspace*{3mm}
       \textsc{5. Date de cette version~:}
         \begin{quote}\textit{\today}\end{quote}\vspace*{3mm}
      \textsc{6. CoordonnÈes des auteurs~:}
        \begin{itemize}
             \item adresse postale~: \begin{quote}\VAR@address\end{quote}
             \item tÈlÈphone~: \VAR@Phone
             \item tÈlÈcopie~: \VAR@Fax
             \item e-mail~: \VAR@Email
        \end{itemize}\vspace*{3mm}
      \textsc{7. Logiciel utilisÈ pour la prÈparation de cet article~:}
      \begin{quote}
        \LaTeX, avec le fichier de style \texttt{\PackageName.cls}, \\
              version~\PackageVersion\ du \filedate.
      \end{quote}
      %\vspace*{3mm}
    %\textsc{8. Formulaire de copyright~:}
      %\begin{quote}
      %  Retourner le formulaire de copyright signé par les auteurs, téléchargé
      %  sur~:
      %  \texttt{http://www.revuesonline.com}
     % \end{quote}\vspace*{3mm}
      }

    \adressestudia

     \myenddocument}%\renewcommand{\@enddocumenthook}
}%\publisher



%%%-- Macros utilisées pour le paramétrage sur le langage

%-- Macros identiques en francais et english
\def\pagename{p.~}
\def\pagesname{p.~}%@@normalement {pp.~}
\def\etalter{\ et al.}
\def\Inname{}
\def\inname{}
\def\editornames{Eds.}
\def\editorname{Ed.}
\def\Volumename{Vol.~}
\def\volumename{vol.~}
\def\notename{Note}
\def\degree{$^\circ$}%@@

% Quelques macros utiles définies dans french
\def\fup#1{\raisebox{0.55ex}{\protect\small #1\kern+.17em}}%
\def\fsc{\scshape}%
\def\@umer#1{$^{\mathrm #1}$\kern.2em\ignorespaces}%
\def\numero{n\@umer{o}}%
\def\numero{n\@umer{o}}%
\def\Numero{N\@umer{o}}%
\def\numeros{n\@umer{o}}%
\def\Numeros{N\@umer{o}}%
\def\fups#1{\raisebox{0.55ex}{{\scriptsize #1}}~}%
\def\ier{\fups{er}}%
\def\iere{\fups{re}}%
\def\ieme{\fups{e}}%
\def\iers{\fups{ers}}%
\def\ieres{\fups{res}}%
\def\iemes{\fups{es}}%

%-- Macros différentes en francais et english
% Doivent être redéfinies après chaque sélection de langage
\def\initialisation{%
  \iffrenchlang %
%   \AutoSpaceBeforeFDP %
   \def\figurename{Figure}%
   \def\figname{\bf figure}%
   \def\tablename{Tableau}%
   \def\tabname{tableau}%
   \def\programname{Programme}%
   \def\progname{programme}%
   \def\studiaRefname{Bibliographie}%
   \def\andname{, }
   \def\acknowledgementsname{Remerciements}
   \def\remarkname{Remarque}
   \def\editionname{\'edition}
   \def\chaptername{chapitre}
   \def\technicalreportname{rapport}% valeur par défaut du champ type
   \def\numbername{\numero}%{n∞~}
   \def\Numbername{\Numero}%{N∞~}
   \def\ofname{de}
   \def\guilo{´~}% C. Queinnec
   \def\guilf{~ª}% C. Queinnec
   \def\Jan{janvier}
   \def\Feb{f\'evrier}
   \def\Mar{mars}
   \def\Apr{avril}
   \def\May{mai}
   \def\Jun{juin}
   \def\Jul{juillet}
   \def\Aug{ao\^ut}
   \def\Sep{septembre}
   \def\Oct{octobre}
   \def\Nov{novembre}
   \def\Dec{d\'ecembre}
\else
   \def\figurename{Figure}%
   \def\figname{\bf figure}%
   \def\tablename{Table}%
   \def\tabname{table}%
   \def\programname{Program}%
   \def\progname{program}%
   \def\studiaRefname{References}%
   \def\andname{, }
   \def\acknowledgementsname{Acknowledgements}
   \def\remarkname{Remark}
   \def\editionname{edition}
   \def\chaptername{chapter}
   \def\technicalreportname{report }% valeur par défaut du champ type
   \def\numbername{num.~}
   \def\Numbername{Num.~}
   \def\ofname{of}
   \def\guilo{``}
   \def\guilf{''}
   \def\Jan{Jan. }
   \def\Feb{Feb. }
   \def\Mar{March }
   \def\Apr{April }
   \def\May{May }
   \def\Jun{June }
   \def\Jul{Jul. }
   \def\Aug{Aug. }
   \def\Sep{Sep. }
   \def\Oct{Oct. }
   \def\Nov{Nov. }
   \def\Dec{Dec. }
\fi
}%\initialisation

\def\@mkstudia{%
   \mkstudialist
   \initialisation % dépend du langage
}

\AtBeginDocument{%
  \iffrenchlang %
%      \selectlanguage{frenchb}%
           \selectlanguage{french}%
  \else %
      \selectlanguage{english}%
  \fi%
}

%EOF

%%%%%%%%%%%%%%%%%% Doc %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% version commune pour les articles de revue et d'actes studia
% Seule différence entre les deux types d'article :
% en version finale, utiliser \journal pour un article de revue et
%                             \proceedings pour un article de conférence.
%
% Documentation résumée %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Toutes les commandes LaTex2e s'utilisent normalement y compris \maketitle remplacée
% par \maketitlepage. Pour connaître les différentes commandes de définition de la
% première page, IL SUFFIT DE COMMENCER UN ARTICLE PAR \maketitlepage :
% les commandes à utiliser avant \maketitlepage s'afficheront par défaut.

%%% Utilisation des options:

% Langue utilisée pour l'article (français ou anglais)
%\documentclass{studia-Hermann}          % article en français avec le package babel
%\documentclass[english]{studia-Hermann} % article en anglais (avec babel)

% Numérotation des pages: par défaut, les pages sont numérotées avec les en-têtes
% construits à partir des commandes \title, \submitted, \journal ou \proceedings

% La signature de l'article apparait en bas de la 1ere page: elle est
% construite par \submitted, \journal ou \proceedings

% Option empty: ni signature, ni en-tête, ni pied de page...
% Exemple:
%\documentclass[empty]{studia-Hermann}\header{LMO'99} % remise finale art. français

% Option nofirstpagebreak: pas de saut de page sur la page de titre
% (non standard, pour certaines revues seulement)

% Option cropmarks: tracé des contours de page et de corps du texte
% (utile pour visualiser les problèmes éventuels de débordement de ligne ou page,
%  ne s'utilise qu'en mise au point finale).
% Exemple:
%\documentclass[cropmarks]{studia-Hermann}         % article en français avec contour de page

% Option fleqn: formules mathématiques cadrées à 2cm du bord gauche du corps du texte
% (par défaut les formules sont centrées) Exemple:
%\documentclass[fleqn]{studia-Hermann}    % article en français avec formules à gauche

% Mode d'emploi détaillé:
% cf les fichiers doc-studia-Hermann.ltx et doc-studia-Hermann.ps

%%% Liste des commandes utilisateur

% Commandes spécifiques au style, avec nb d'arguments entre crochets

% \title[1]
% \subtitle[1]
% \author[1]
% \andauthor
% \fup
% \address[1]
% \abstract[1]
% \motscles[1]
% \keywords[1]
% \resume[1]
% \maketitlepage
% \nofirstpagebreak
% \firstpagebreak
%----
% \submitted[2]
% \journal[8]
% \proceedings[3]
%----
% \acknowledgements[1]
% \logbook[3]
% \andeditor
% \biography[2]
%%% Utilitaires
% \remark[1]
% \note[1]
% \bibliography
% \CropMarksOn
% \CropMarksOff
% \eqncont
% \delaynewpage[1]
% \forcenewpage
% \newenvironment{editorial}[5]

% Commandes ou Environnements Latex redefinis

% \rmdefault
% \sfdefault
% \ttdefault
% \maketitle    % alias \maketitlepage
% \thesection
% \section
% \thesubsection
% \subsection
% \thesubsubsection
% \subsubsection
% \theparagraph
% \paragraph
% \footnotesize
% \footnoterule
% \newblock
% \renewenvironment{thebibliography}[1]
% \renewenvironment{itemize}
% \renewenvironment{enumerate}
% \newenvironment{ITEMIZE}
% \renewenvironment{figure}

% Compteurs entiers ou variables décimales Latex redéfinis

% section: 0
% page: 1
% secnumdepth: 4
% topnumber: 2            % nb max de fig. en haut
% bottomnumber: 1         % nb max de fig. en bas
% totalnumber: 3          % nb max de fig. par page
% \topfraction : 1        % fraction remplissage haut
% \bottomfraction : 1     % fraction remplissage bas
% \textfraction   : 0     % fraction remplissage texte / page
% \floatpagefraction: 0.8 % fraction remplissage floatpage

% Fontes utilisables (utilisation rare)

% \FonteTexte
% \FonteEnTete
% \FonteRubrique
% \FonteSignature
% \FonteTitre
% \FonteSousTitre
% \FonteAuteur
% \FonteResume
% \FonteTitreResume
% \FonteSectionI
% \FonteSectionII
% \FonteSectionIII
% \FonteSectionIV
% \FonteNoteBasPage
% \FonteTitreLegende
% \FonteCaptionName
% \FonteBibliographie

% Paramètres de listes (Normalement, ne pas modifier)
% \mkstudialist % Pour modifier les dimensions
% \labelitemi
% \labelitemii
% \labelitemiii
% \labelitemiv
% Variables utilisables
% \newlength{\interligne}
