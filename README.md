- [Description of *Roll Call Votes in the Mexican Senate* data repository](#org66d0561)
- [Francisco Cantú](#org36498f2)
- [Contents](#org6bc54fb)
- [License](#org5ed2306)
- [Citation in BibTex format](#org3dc2918)
- [Files included](#orgcffd6ec)
- [Data](#org548e35f)
- [Codebook](#orgf6712a1)
- [Acknowledgements](#orgaad60a4)

Last revision: 2025-03-31 Report inconsistencies to emagar at gmail dot com.

---

<h2> Recent news </h2>

<sup><sub>2024-03-28</sub></sup> First public commit of the data for the University of Houston [EITM](https://uh.edu/hobby/cpp/events/eitm/).

---


<a id="org66d0561"></a>

# Description of *Roll Call Votes in the Mexican Senate* data repository

-   Authors: Francisco Cantú and Eric Magar
-   Email: emagar at gmail dot com
-   GitHub: <https://github.com/emagar/senmex>

The repository contains roll call data for recent Senates of the Mexican Congress. It also includes data on the senators present in each Legislature. Data has been compiled primarily from the Senate's web page at <https://senado.gob.mx/>, with information from other sources and the press.


<a id="org36498f2"></a>

# Francisco Cantú

![img](./AIB_5906.jpg)

The repository honors the memory of my colleague and friend Francisco Cantú, who passed away on August 17, 2023. Francisco was a [leading scholar](https://www.cambridge.org/core/journals/political-science-today/article/in-memoriam-francisco-cantu/7E57FEBCB15FDFEC437654A4C95F48DE) and esteemed co-author whose prolific career was, sadly, much too short. In 2014, while preparing our analysis of [roll call votes for the Mexican Chamber of Deputies](https://github.com/emagar/dipmex), Francisco aimed at remedying the absence of research on Senate politics. The Senate has not systematically reported roll call votes in its web page. (It has since began reporting this information, but the data remains incomplete, and does not go back beyond 2007.) In absence of data of individual senator behavior, the upper chamber of Congress remained in relative obscurity. He circulated a collection of digital issues of the *Diario de los Debates*, each containing, within the transcripts of the debates and the session's floor proceedings, one or more roll call votes. We anticipated the systematization of this information to be quite challenging, and the project remained pending. This repository performs the data systematization.


<a id="org6bc54fb"></a>

# Contents

The repository contains code, raw data, and clean roll call databases. Code included replicates data downloading from the primary source (`code/getweb/`); databases preparation from raw data (`code/rcPrep`); and descriptive analysis and ideal point estimation (`code/rcAnalysis/`). Raw data is in `data/fromWeb/` directory. ****If interested in clean roll call votes only, simply copy the contents of the `data/votes-for-web/` subdirectory****. Data includes roll call votes of the 59th (1sep2003-31aug2006) and 60th (1sep2006-31aug2009) Legislatures. Data is distributed in R (<http://cran.r-project.org/>) and csv formats.


<a id="org5ed2306"></a>

# License

This repository in under the MIT License, see <http://opensource.org/licenses/MIT>. The sole condition to use the data is to cite it as follows: Francisco Cantu, Scott Desposato, and Eric Magar. 2014. "Consideraciones metodologicas para estudiantes de politica legislativa mexicana: sesgo por seleccion en votaciones nominales". Politica y Gobierno vol. 21, num. 1, pp. 25-54.


<a id="org3dc2918"></a>

# Citation in BibTex format

```<TeX>
@article{cantuDesposatoMagar2014,
        title = {Consideraciones metodol\'ogicas para estudiantes de pol\'itica legislativa mexicana: sesgo por selecci\'on en votaciones nominales},
        author = {Cant\'u, Francisco and Desposato, Scott and Magar, Eric},
        year = {2014},
        volume = {21},
        number = {1},
        journal = {Pol\'itica y Gobierno},
        pages = {25--54}
        url = {http://www.politicaygobierno.cide.edu/index.php/pyg/article/view/18/564}
}
```


<a id="orgcffd6ec"></a>

# Files included

-   `data/senadores/sen*.csv` = comma-separated vote for all senators elected to one term of office (their terms span two consecutive three-year *Legislaturas*). The numerals identify the term in question (eg. `dip58.59.csv` are members of the 58th and 59th Legislatures). **Columns include the following data**:
    -   `pila` = member's first and middle names.
    -   `patmat` = member's last names (includes patronym and matronym, as used in Mexico).
    -   `id` = senator identifier. For members elected statewide, the `id` concatenates the state abbreviation, the ranking (parties nominate ranked pairs of candidates, the top-voted list elects both its members, the runner-up list elects it top-ranked member), and whether the member is *propietario* (p) or *suplente* (s). (Eg. `bc1p`, `bc2p`, and `bc3p` indicate Baja California's three propietario senators, 1 and 2 are from the plurality/majority list, 3 is from the runner-up list.) For 32 members elected by proportional representation (PR), the state abbreviation part is replaced by `rp`.
    -   `edon` = state number. For PR members it is 0.
    -   `birth` = member's birthyear.
    -   `fm` = member's gender (\*F\*emale or \*M\*ale).
    -   `part` = member's party.
    -   `postulo` = party or coalition that nominated the member, if different from `part`.
    -   `dsmd` = dummy equal 1 for statewide elected members, 0 for members elected by PR.
    -   `dsup` = dummy equal 1 for *suplentes*, 0 for *propietarios*. Suplentes may replace propietarios who take a leave of absence (*licencia*).
    -   `yrin` `moin` `dyin` = date when member took oath.
    -   `yrout` `moout` `dyout` = date when member took a leave of absence.

-   (Under construction)


<a id="org548e35f"></a>

# Data

The objects in each R file (zipped together in text-only files) are the following: -`sendat` has individual senator information (names, gender, state and district, party, dcarta=dummy equal 1 for members filing a letter of intent with the chamber's Junta to run for office again (inapplicable before 2018 see [this](http://eleccionconsecutiva.diputados.gob.mx/contendientes)), dreran=dummy equal 1 for members renominated, dreelected=dummy equal 1 for members who reelected. -`votdat`: has vote information (favor=ayes, contra=nays, absten=abstained, quorum=present but not voting, ausen=no show, title=motion considered, leg=legislature, yr-mo-dy=vote's date). -`rc`: roll call vote information (0=was not chamber member, 1=aye, 2=nay, 3=abstained, 4=present but did not vote, 5=no show).


<a id="orgf6712a1"></a>

# Codebook

Variables usually appear as columns in a data frame. A list of variables included in the files follows.

-   `leg` = Legislature numeral.
-   `pila` = member's first name and middle names, if any.
-   `patmat` = member's last names (patronym and matronym).
-   `id` = member's id: state + district + p/s for propietario/suplente.
-   `birth` = member's birth year.
-   `gen` = member's gender, \*F\*emale or \*M\*ale.
-   `postulo` = electoral party or coalition.
-   `part` = legislative party.
-   `edo` = member's state.
-   `dsmd` = dummy equal 1 if member elected in single-member district, 0 otherwise.
-   `dsup` = dummy equal 1 if member was elected as a substitute (*suplente*), 0 otherwise (*propietario*).
-   `cabecera` = member's district administrative head.
-   `yrin1`, `moin1`, `dyin1` = year month day member first started serving in the Legislature, ie. date `doath` gets value 1.
-   `yrout1`, `moout1`, `dyout1` = year month day member first took a leave of absence (*licencia*), if any.
-   `yrin2`, `moin2`, `dyin2` = year month day member returned from first leave of absence, if any.
-   `yrout2`, `moout2`, `dyout2` = year month day member took second leave of absence (*licencia*), if any.
-   `yrin3`, `moin3`, `dyin3` = year month day member returned from second leave of absence, if any.
-   `yrout3`, `moout3`, `dyout3` = year month day member took third leave of absence (*licencia*), if any.
-   `lider` = member's leadership post, if any.
-   `prescom` = dummy equal 1 if member was a committee chair, 0 otherwise (*propietario*).
-   `repite` = member's previous Congressional experience (sequential Legislatures if prior deputy, \`sen' if prior senator).
-   `doath` = dummy equal 1 if member took the oath of office (*toma de protesta*), 0 otherwise.
-   `ptysh` = share of seats controlled by the member's party.
-   `nom` = member's name.


<a id="orgaad60a4"></a>

# Acknowledgements

Eric Magar Meurs acknowledges financial support from the Asociación Mexicana de Cultura A.C. Many thanks to [Carlos Alexis Sarabia](https://github.com/calexissarabia) for research assistance. The author is responsible for mistakes and shortcomings in the data. Please report any error to emagar at gmail dot com.
