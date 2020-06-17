This repository serves as research archive for the Master's thesis 'Missing the Point: Non-Convergence in Iterative Imputation Algorithms' by Hanne Oberman. 


**Abstract**

Iterative imputation is a popular tool to accommodate missing data. While it is widely accepted that valid inferences can be obtained with this technique, these inferences all rely on algorithmic convergence. There is no consensus on how to evaluate the convergence properties of the method. This paper provides insight into identifying non-convergence in iterative imputation algorithms. Our simulation study found that-in the cases considered-inferential validity was achieved after five to ten iterations, much earlier than indicated by the \widehat{R} and AC diagnostics. We conclude that it never hurts to iterate longer, but such calculations hardly bring added value.


**Contents**

The thesis manuscript is named `Oberman,H.I.-MSBBSS.pdf`, and the file `EthicalApproval.pdf` gives proof of ethical clearance by the FETC (Utrecht University).


Additionally, this research archive contains all of the files necessary to reproduce the simulation study and the accompanying manuscript. Namely:

- `1.Execute.R` | The script to run the entire simulation set-up. Results are saved as `complete.Rdata` in the `Results` folder.

- `2.ReproduceExample.R` | The script to reproduce the example of pathological non-convergence by van Buuren (2018). Results are saved as `example_conv.Rdata`, `example_nonconv.Rdata`, and `example_diagnostics.Rdata` in the `Results` folder.

- `3.GenerateSupplementaryFigures.R` | The code to obtain the full set of results, whereas the thesis manuscript only displays conditions where the number of iterations is at most fifty. The resulting figures are saved as `ResultsEstimates.png` and `ResultsDiagnostics.png` in the `Results` folder.


The following folders are used to store the necessary functions, figures, layout, and results:

- `Functions` | Contains modular functions that are used in the simulation script `1.Execute.R` and the example of pathological non-convergence `2.ReproduceExample.R`.

- `Results` | Contains the results of the simulation study created using `1.Execute.R`, and the data of the pathological non-convergence example created using `2.ReproduceExample.R`.

- `Figures` | Contains the supplementary figures created using `3.GenerateSupplementaryFigures.R` and the functions that are used to create figures in the thesis manuscript.

- `Writeup` | Contains the thesis manuscript files (e.g., R Markdown, LaTeX template, literature).


**Permission and access**

The entire research archive is available through [github.com/hanneoberman/MissingThePoint](https://github.com/hanneoberman/MissingThePoint). The Github repository is public, and therefore completely 'open access'. The data will be stored for a minimal duration of ten years.

-Hanne Oberman, 11-05-2020

NB. Information about software and package versions can be found in the corresponding files e.g., `1.Execute.R`.