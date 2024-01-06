# Etude de la croissance staturo-pondérale des enfants de la cohorte PÉLAGIE

### CONTEXTE

Plusieurs maladies chroniques survenant à l’âge adulte ont été associées au développement pré et post-natal, en particulier à la croissance du poids et de la taille. Dans ce contexte, les modèles de croissance, développés pour suivre le développement des enfants, sont étudiés et associés à des déterminants potentiels de la croissance. L’[Institut National de la Santé et de la Recherche Médicale](https://www.inserm.fr) (INSERM) et plus précisément l’[**Institut de Recherche sur la Santé, l’Environnement et le Travail**](https://www.irset.org/fr) (IRSET) travaillent activement sur ce sujet et établissent un lien entre les questions environnementales et sanitaires et les trajectoires de croissance et les maladies chroniques associées.

### OBJECTIFS

Cette étude, encadrée par l'IRSET[^1], a pour but la modélisation de la croissance staturo-pondérale des enfants de la **cohorte PÉLAGIE** (Perturbateurs Endocriniens : Étude Longitudinale sur les Anomalies de la Grossesse, l’Infertilité et l’Enfance). Le but de notre étude est de comparer la qualité d'ajustement de quatre modèles de croissance. Pour cela, les modèles ont été ajustés sur les données des enfants bretons de la cohorte PÉLAGIE entre la naissance et 13 ans. Le premier objectif consiste à **déterminer le modèle qui décrit le mieux la croissance individuelle du poids et de la taille** à l'aide d'une modélisation à effets mixtes. Le second objectif est de **déterminer plusieurs profils de croissance** afin de les mettre en relation avec les déterminants de la croissance.

[^1]: Cette étude a été réalisée en collaboration avec [Nathalie COSTET](mailto:nathalie.costet@univ-rennes.fr) et [Charline WAREMBOURG](mailto:charline.warembourg@univ-rennes.fr)

### DONNEES

Dans la cohorte PÉLAGIE, les données de croissance du poids et de la taille sont recueillies entre la naissance et 17 ans. Au cours de l'étude, les mères ont rempli plusieurs questionnaires ponctuels (2, 6 et 12 ans) afin de recueillir des informations sur la croissance de leur enfant à différents stades. Environ 3 500 enfants participaient à l'étude (3 421 mères étaient incluses à la création de la cohorte). Afin de garantir la robustesse des résultats, nous avons ensuite exclu de notre base de données les enfants ayant moins de 5 mesures et aucun suivi de croissance après l'âge de 8 ans. Au final, notre base de données comprenait 23 793 mesures de 1 460 enfants.

### METHODOLOGIE

Nous avons comparé quatre modèles présentés dans la littérature : le modèle de Jenss-Bayley et sa variante le modèle Jenss adapté ainsi que les modèles de Reed (1<sup>er</sup> et 2<sup>nd</sup> ordre).

* **Modèle Jenss-Bayley** : $f(t) = \alpha + \beta \cdot t + e^{\gamma + \delta t}$
* **Modèle Jenss adapté** : $f(t) = \alpha + \beta \cdot t + e^{\gamma + \delta t} + \omega \cdot t^2$
* **Modèle Reed du 1er ordre** : $f(t) = \alpha + \beta \cdot t + \gamma\ln(t) + \frac{\delta}{t}$
* **Modèle Reed du 2nd ordre** : $f(t) = \alpha + \beta \cdot t + \gamma\ln(t) + \frac{\delta}{t} + \frac{\omega}{t^2}$

La qualité de l'ajustement des modèles a été évaluée à l'aide de l'écart-type des résidus, du critère d'information d'Akaike, du critère d'information bayésien et du test du rapport de vraisemblance pour les modèles emboîtés. Parmi les quatre modèles, nous avons cherché à déterminer celui qui s'ajustait le mieux aux données, c'est-à-dire celui qui minimisait l'AIC et le BIC et qui présentait la plus grande vraisemblance.

Comme on peut le voir sur le graphique ci-dessous, les résidus du modèle Jenss adapté ont une plus petite variance et sont quasiment centrés en 0 comparé aux autres modèles où il y a plus de variance. Aussi, il reste de la tendance dans les résidus, ce qui semble indiauer qu'il reste de l'information dans les résidus.

![BoxplotResidusPoidsAnnees](docs/BoxplotResidusPoidsAnnees.pdf)

Si l'on représente tous les modèles sur le graphique des poids populationnels on se rend compte que seul le modèle Jenss adapté capte le rebond d'adiposité[^2].

[^2]: Le rebond d’adiposité est le point le plus bas de la courbe de l’IMC, juste avant l’ascension de la courbe. Normalement, le rebond d’adiposité se situe vers l’âge de 6 ans. S’il survient avant, on parle de rebond précoce.

![GraphiquePopulationnelPoids](docs/GraphiquePopulationnelPoids.pdf)


### CONCLUSION

Dans notre étude, sur les données de 0 à 13 ans, pour le poids et la taille, le **modèle de Jenss adapté a présenté le meilleur ajustement** parmi les quatre modèles utilisés. L'étude PÉLAGIE étant toujours en cours, un nouveau suivi à l'âge de 18 ans débutera en 2022 qui permettra de collecter de nouvelles données de croissance et ainsi d'étudier la croissance de la naissance jusqu'à l'âge adulte.

__**MOTS-CLES**__ taille $\bullet$ poids $\bullet$ modèles de croissance $\bullet$ modèles de Jenss $\bullet$ modèles de Reed $\bullet$ cohorte $\bullet$ origines développementales de la santé et des maladies adultes

---

### Auteurs

Cette étude est réalisée dans le cadre du projet statistique de deuxième année à l'[ENSAI](https://ensai.fr) (Ecole Nationale de la Statistique et de l'Analyse de l'Information).

* [Corentin DAUMONT](mailto:corentin.daumont@eleve.ensai.fr)
* [Alanna GENIN](mailto:alannagenin@gmail.com)
* [Camille PLIQUET](mailto:camille.pliquet@eleve.ensai.fr)

