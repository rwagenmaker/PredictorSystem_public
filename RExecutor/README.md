# RExecutor



::::::::::::::::::: rExecutor.R :::::::::::::::::::
-


Library Imports:

- library rPython
- library parallel


Code Imports:

- code from Francisco Banha : client.R (@RExecutor)
   - This is currently commented, but the point is to trigger the execution in a server running the RSserver - currently in localhost
   - This needs to be done with the modified code from Francisco. Currently not doing, instead we're just using regular R 
   - R version 3.4.2 (2017-09-28) -- "Short Summer"
  
- realArgumentMiner.R (@RExecutor)
   - Used to generate freshMinings, without executing any of the expressions. The profiling process may require the algorithm to perform some inner predictions, so that a final value is reached (mostly related to the size of the variables).

- callTreeAnalysis.R (@CodeAnalysisRW)
   

- predictorFunct.R (@CodeAnalysisRW)
