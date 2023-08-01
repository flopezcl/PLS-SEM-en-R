# Guía de PLS-SEM en R
Los modelos de ecuaciones estructurales (SEM) se han convertido en una de las principales técnicas de análisis multivariados utilizados en ciencias sociales. Esta técnica combina el uso de variables latentes (no observadas) con datos provenientes de medidas usados como insumos para el análisis de causalidad (Williams et al., 2009).

Los métodos SEM permiten modelizar relaciones entre variables predictoras y criterios; incorporar variables latentes, medidas por medio de indicadores y testear estadísticamente teorías con datos empíricos. 
Los métodos SEM más utilizados son mediante análisis de covarianzas (CB-SEM), de Varianza (PLS-SEM) y análisis generalizado de componentes (GSCA), siendo actualmente PLS-SEM el método más utilizado (Hair et al., 2019; Hair, Hult, Ringle, Sarstedt, et al., 2017). 

Los principales softwares para desarrollar estos análisis son SmartPLS, XLSTAT and WarpPLS cuyos precios oscilan entre US$170 y US$270 anuales, siendo SmartPLS uno de los más utilizados dada su facilidad y recursos (Ghasemy et al., 2020; Memon et al., 2021). 

Existen también diversos paquetes en R que permiten su implementación en forma libre, entre ellos cSEM, semPLS y SEMinR (Becker et al., 2023), no observando en la literatura referencias a paquetes en Phyton, aun cuando para CB-SEM si existen, los que no son populares y cuentan con una limitada documentación (Kanti et al., 2022).

Respecto al uso de estas técnicas, se ha observado una baja presencia de investigadores desde Latinoamérica, mostrando los actuales análisis bibliométricos unas pocas investigaciones desde Chile, Colombia y Brasil (Ciavolino et al., 2022; Lirio-Loli & Dextre-Martínez, 2022; Mohd Ghazali et al., 2023). Lo anterior pudiera tener su origen en el costo de las licencias de estas aplicaciones o en el desconocimiento de su aplicación mediante R. Por este motivo, se ha planteado como objetivo replicar un análisis de PLS-SEM mediante R, generando un script que sea fácil de comprender y modificar, para generar nuevos análisis. 
La principal contribución de este trabajo será mostrar la facilidad y potencialidad de utilizar R en dichos estudios, así como también, el incorporar características de estos análisis que se encuentran en otras librerías y que complementan estos. 

Para ello se utilizará como referencia la investigación de Ramirez-Correa et al. (2023), en la cual se utilizó análisis PLS-SEM, con multigrupos, dejando los datos a disposición de los lectores. 

Cabe señalar que este trabajo no corresponde a un curso de PLS-SEM, de su forma de trabajo o de los reportes a informar en las investigaciones, siendo el alcance el mostrar la forma de trabajar y obtener los principales reportes de dichos modelos en R Studio, esto ya que existen múltiples documentos con guías básicas de trabajo, de reportes a ser incluidos y ejecución en distintas herramientas tales como (Becker et al., 2023; Guenther et al., 2023; Hair et al., 2019; Hair, Hult, Ringle, & Sarstedt, 2017; Latan & Noonan, 2017; Marin-Garcia & Alfalla-Luque, 2019; Ringle et al., 2020).

##	Metodología
Nuestro objetivo es replicar un análisis de PLS-SEM utilizando R Studio, generando un script que facilite la comprensión de su ejecución. Como objetivo secundario nos hemos planteado el integrar al análisis de PLS-SEM, la generación de estadísticas descriptivas y una ampliación del análisis hacia modelos Pathmox y de los efectos de variables moderadoras y mediadoras. 

Para lo anterior utilizaremos como base los datos e investigación realizada por Ramirez-Correa et al. (2023) ubicados en https://doi.org/10.1371/journal.pone.0284585.s001 (Se incluye en este repositorio como 2023.TRI_MGA.csv)

El paquete a utilizar para el análisis de ecuaciones estructurales será SeminR esto dado por la existencia del libro de Hair et al (2021), quienes crearon SmartPLS, así también, dado que, dicho paquete no utiliza la librería Lavaan consideramos que esto puede facilitar su uso inicial.  

Acionalmente al libro antes citado se utilizará como fuente de información la investigación de Ray et al. (2022), la información del siguiente repositorio https://github.com/cran/seminr y la documentación de los paquetes a utilizar. 

Este código permite:
- Generar visualización de datos (estadística descriptiva);
- Análisis de PLS-SEM y PLS predict;
- Análisis de efectos mediadores;
- Análisis de efectos moderadores;
- Comparación entre diferentes modelos
- Análisis de multigrupo;
- Análisis de constructo de segundo orden;
- Análisis Pathmox. 

## Archivos 
1. Archivo de datos:  "2023.TRI_MGA.csv"
   
2. Ejemplo de resultados en html: "PLSSEM_en_R-VF.html"
   
3. Carpeta con reportes generados en la ejecución: "PLSSEM_en_R-VF_files"
 
4. Códigos de ejecución: "Guía de PLS-SEM en R - Markdown.md"

### Incluye las siguientes secciones: 

A. Carga Datos

B. Análisis descriptivo

C. Trabajar datos faltantes

D. Convertir en categóricas y numéricas

E. Modelo de Ecuaciones Estructurales

F. Análisis de Mediación

G. Predict PLS

H. Análisis de Moderadores

I. Comparación con otros modelos

J. Análisis Multigrupo

K. Análisis Segundo Orden

L. Análisis Pathmox

## Favor citar como:
López, Felipe A., (2023). Guía de PLS-SEM en R. https://github.com/flopezcl/PLS-SEM-en-R

# Bibliografía

Becker, J.-M., Cheah, J.-H., Gholamzade, R., Ringle, C. M., & Sarstedt, M. (2023). PLS-SEM’s most wanted guidance. International Journal of Contemporary Hospitality Management, 35(1), 321–346. https://doi.org/10.1108/IJCHM-04-2022-0474

Branley-Bell, D., Gómez, Y., Coventry, L., Vila, J., & Briggs, P. (2021). Developing and Validating a Behavioural Model of Cyberinsurance Adoption. Sustainability, 13(17), 9528. https://doi.org/10.3390/su13179528
Calero Valdez, A., Kojan, L., Danks, N. P., & Ray, S. (2023). Structural Equation Modeling in HCI Research using SEMinR. Extended Abstracts of the 2023 CHI Conference on Human Factors in Computing Systems, 1–3. https://doi.org/10.1145/3544549.3574171

Cheah, J.-H., Thurasamy, R., Memon, M. A., Chuah, F., & Ting, H. (2020). Multigroup Analysis using SmartPLS: Step-by-Step Guidelines for Business Research. Asian Journal of Business Research, 10(3). https://doi.org/10.14707/ajbr.200087

Ciavolino, E., Aria, M., Cheah, J.-H., & Roldán, J. L. (2022). A tale of PLS Structural Equation Modelling: Episode I— A Bibliometrix Citation Analysis. Social Indicators Research, 164(3), 1323–1348. https://doi.org/10.1007/s11205-022-02994-7

Ghasemy, M., Teeroovengadum, V., Becker, J.-M., & Ringle, C. M. (2020). This fast car can move faster: a review of PLS-SEM application in higher education research. Higher Education, 80(6), 1121–1152. https://doi.org/10.1007/s10734-020-00534-1

Guenther, P., Guenther, M., Ringle, C. M., Zaefarian, G., & Cartwright, S. (2023). Improving PLS-SEM use for business marketing research. Industrial Marketing Management, 111, 127–142. https://doi.org/10.1016/j.indmarman.2023.03.010

Hair, J. F., Hult, G. T. M., Ringle, C. M., & Sarstedt, M. (2017). A Primer on Partial Least Squares Structural Equation Modeling (PLS-SEM). SAGE Publications Inc. https://us.sagepub.com/en-us/nam/a-primer-on-partial-least-squares-structural-equation-modeling-pls-sem/book244583

Hair, J. F., Hult, G. T. M., Ringle, C. M., Sarstedt, M., Danks, N. P., & Ray, S. (2021). Partial Least Squares Structural Equation Modeling (PLS-SEM) Using R. Springer International Publishing. https://doi.org/10.1007/978-3-030-80519-7

Hair, J. F., Hult, G. T. M., Ringle, C. M., Sarstedt, M., & Thiele, K. O. (2017). Mirror, mirror on the wall: a comparative evaluation of composite-based structural equation modeling methods. Journal of the Academy of Marketing Science, 45(5), 616–632. https://doi.org/10.1007/s11747-017-0517-x

Hair, J. F., Risher, J. J., Sarstedt, M., & Ringle, C. M. (2019). When to use and how to report the results of PLS-SEM. European Business Review, 31(1), 2–24. https://doi.org/10.1108/EBR-11-2018-0203

Kanti, S., Taneja, A., Riaz, S., & Das, S. (2022). CB-SEM IMPLEMENTATION IN R AND PYTHON: A REVIEW AND COMPARATIVE STUDY. Journal of Applied Structural Equation Modeling, 6(1), 1–22. https://doi.org/10.47263/JASEM.6(1)01

Lamberti, G., Aluja, T. B., & Sanchez, G. (2016). The Pathmox approach for PLS path modeling segmentation. Applied Stochastic Models in Business and Industry, 32(4), 453–468. https://doi.org/10.1002/asmb.2168

Lamberti, G., Banet Aluja, T., & Sanchez, G. (2017). The Pathmox approach for PLS path modeling: Discovering which constructs differentiate segments. Applied Stochastic Models in Business and Industry, 33(6), 674–689. https://doi.org/10.1002/asmb.2270

Latan, H., & Noonan, R. (Eds.). (2017). Partial Least Squares Path Modeling. Springer International Publishing. https://doi.org/10.1007/978-3-319-64069-3

Lirio-Loli, F., & Dextre-Martínez, W. (2022). Bibliometric analysis of the scientific production found in Scopus and Web of Science about business administration. https://doi.org/10.48550/arXiv.2201.02760

Madhavan, M., Sharafuddin, M. A., & Chaichana, T. (2022). Impact of Business Model Innovation on Sustainable Performance of Processed Marine Food Product SMEs in Thailand—A PLS-SEM Approach. Sustainability, 14(15), 9673. https://doi.org/10.3390/su14159673

Marin-Garcia, J. A., & Alfalla-Luque, R. (2019). Protocol: How to deal with Partial Least Squares (PLS) research in Operations Management. A guide for sending papers to academic journals. WPOM-Working Papers on Operations Management, 10(1), 29. https://doi.org/10.4995/wpom.v10i1.10802

Martínez-Filgueira, X.-M., Peón, D., & López-Iglesias, E. (2022). Determinants of innovation by agri-food firms in rural Spain: an MCA PLS-SEM analysis. International Food and Agribusiness Management Review, 25(1), 103–119. https://doi.org/10.22434/IFAMR2021.0015

Memon, M. A., T., R., Cheah, J.-H., Ting, H., Chuah, F., & Cham, T. H. (2021). PLS-SEM STATISTICAL PROGRAMS: A REVIEW. Journal of Applied Structural Equation Modeling, 5(1), i–xiv. https://doi.org/10.47263/JASEM.5(1)06

Mohd Ghazali, Z., Wan Yaacob, W. F., & Wan Omar, W. M. (2023). LGCM and PLS-SEM in Panel Survey Data: A Systematic Review and Bibliometric Analysis. Data, 8(2), 32. https://doi.org/10.3390/data8020032

Nayyar, V. (2022). Reviewing the impact of digital migration on the consumer buying journey with robust measurement of PLS‐SEM and R Studio. Systems Research and Behavioral Science, 39(3), 542–556. https://doi.org/10.1002/sres.2857

Nuñez-Maldonado, A., Flores-Romero, M. B., & Durán-Sánchez, A. (2022). The PLS Method in Tourism research: : A Bibliometric approach. Tourism and Hospitality International Journal, 18(1), 84–99.

Qazimirsaeed, A., Khosravi, H., Rafieian, M., Mirzahossein, H., & Forciniti, C. (2022). Walkability Policies in Developing Countries: What Do People Need and Prefer in Iran? Sustainability, 14(17), 10808. https://doi.org/10.3390/su141710808

Ramírez-Correa, P. E., Arenas-Gaitán, J., Rondán-Cataluña, F. J., Grandon, E. E., & Ramírez-Santana, M. (2023). Adoption of social networking sites among older adults: The role of the technology readiness and the generation to identifying segments. PLOS ONE, 18(4), e0284585. https://doi.org/10.1371/journal.pone.0284585

Ray, S., Danks, N. P., Calero Valdez, A., & Velasquez Estrada, J. M. (2022). Building and Estimating Structural Equation Models. https://cran.r-project.org/web/packages/seminr/seminr.pdf

Ringle, C. M., Sarstedt, M., Mitchell, R., & Gudergan, S. P. (2020). Partial least squares structural equation modeling in HRM research. The International Journal of Human Resource Management, 31(12), 1617–1643. https://doi.org/10.1080/09585192.2017.1416655

Russo, D., & Stol, K.-J. (2022). PLS-SEM for Software Engineering Research. ACM Computing Surveys, 54(4), 1–38. https://doi.org/10.1145/3447580

Sarstedt, M., Hair, J. F., Cheah, J.-H., Becker, J.-M., & Ringle, C. M. (2019). How to Specify, Estimate, and Validate Higher-Order Constructs in PLS-SEM. Australasian Marketing Journal, 27(3), 197–211. https://doi.org/10.1016/j.ausmj.2019.05.003

Sarstedt, M., & Moisescu, O.-I. (2023). Quantifying uncertainty in PLS-SEM-based mediation analyses. Journal of Marketing Analytics. https://doi.org/10.1057/s41270-023-00231-9

Schamberger, T. (2023). Conducting Monte Carlo simulations with PLS-PM and other variance-based estimators for structural equation models: a tutorial using the R package cSEM. Industrial Management & Data Systems, 123(6), 1789–1813. https://doi.org/10.1108/IMDS-07-2022-0418
Williams, L. J., Vandenberg, R. J., & Edwards, J. R. (2009). 12 Structural Equation Modeling in Management Research: A Guide for Improved Analysis. The Academy of Management Annals, 3(1), 543–604. https://doi.org/10.1080/19416520903065683

