# PredictorSystem
As organizations realize how data analysis helps them to harness their data and use it to identify new opportunities, the popularity of specialized programming languages like R rises. R is an open source programming language and an environment for statistical computing and graphics, whose increasing notoriety has attracted lots of new users, including everyday users that cannot take the most of the R’s capabilities due to a lack of computing resources. For these reasons, a Volunteer Computing (VC) platform for R software is currently being developed to allow public participants to, voluntarily, share their devices’ idle processing power in exchange for computing credits. These credits can then be used to request for computing power within the platform. In this work we propose a decision system for the mentioned platform that, through estimations, selects the most suitable execution site for a given R script. In order to generate such estimations we follow a history based approach, where we use previous function calls observations to create regression models. The results from this proposed system were validated using the R-Benchmark 25 script, which is globally used in the R community as an utility to measure the R’s performance under different machines.


## Important Files

: .R :

- rExecutor.R
- realArgumentMiner.R
- callTreeAnalysis.R
- predictorFunct.R
- mutualMinerFunctions.R
- drawTree.R
- historyReader.R

: .py :

- saveModule.py
- connectionModule.py
- dummyVolunteers.py
- dummyMarket.py
- offPredictor.py

Francisco Banha's Files
-
- client.R
-

## Important Installations

- Python 2.7
- R 3.4.0
- install Iperf3 on the machine
- install: `sudo apt-get install python-dev`
- install the necessary R packages
- Use `sudo apt-get install r-cran-car` in case `car` package is not installing
- install so that the banner can be printed`sudo apt-get install xclip || echo "alias clipboard='xclip -sel clip'" >> ~/.bashrc`


## Important Notes
- Código do Francisco Banha (sandbox) não está integrado. Precisa de uma versão modificada do próprio R.
- A parte de gravar o ambiente de execução para depois fornecer ao incubator, não está a funcionar. Mas não é essencial visto que o que se quer é a integração com o interpretador do R (ver issue #6)
- The price (a.k.a credits per time unit) is not being considered in the decision
- The R files have to be run from the indicated folder (ver issue #1)

## Main Commands

- Run `offPredictor.py`, `dummyVolunteers.py` and `dummyMarket.py`

In order to **_run the example file_**:

```
(...)/PredictorSystem_public/CodeAnalysisRW/Demos$  Rscript testScript.R

```

## Other Commands


In order to **_populate_** a certain **package** with CRAN example file:
```
(...)/PredictorSystem_public/CodeAnalysisRW$  Rscript populateFunction.R stats

```

In order to **_populate_** a certain **function** with CRAN example file:
```
(...)/PredictorSystem_public/CodeAnalysisRW$  Rscript populateFunction.R stats lm

```

In order to **_display the records_** a certain **function** has in the system:

```
(...)/PredictorSystem_public/CodeAnalysisRW$  Rscript historyReader.R stats lm

```

In order to **_display the records_** a certain **package** has in the system:

```
(...)/PredictorSystem_public/CodeAnalysisRW$  Rscript historyReader.R TOTAL stats

```


