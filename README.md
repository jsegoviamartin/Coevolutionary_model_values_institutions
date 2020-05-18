# Coevolutionary_model_values_institutions
Co-evolutionary model of value systems, institutions and cultural conventions. It incorporates compliance, confirmation, content and frequency biases into the learning and production algorithm.  As described in Chapter 5 (thesis).

The file [run_model_COEVO.py](https://github.com/jsegoviamartin/Coevolutionary_model_values_institutions/blob/master/run_model_COEVO.py) contains a commented version of the Python code used to implement the model, as described in the thesis dissertation (Ch. 5).

In the folder [COEVO_Final] you can find all the Python code you need to run the model. It contains the scripts for running the simulations with different parameter combinations: In the labels, `Het` stands for heterogeneiry, `Hom` for homogeneity, `PR` for pseudorandom, `OTA` for one takes all. Also, `WR` stands for agents initialised with an unique varaint and `R` for agents initialised with a variant form a pool of variants with replacement. `I00` stands for institutional power 0, `I05` for 0.5 and `I10` for 1.0. In this folder you can also find the `csv` files that contain all the data collected during the simulations.

In the file [COEVO_plots] you can find some R code to replicate the plots I show in Ch.5. You just need to type the appropriate csv file name and run the script.

A preprint of this work will be available soon.

Bib tex:

```
@misc{Segovia_coevomodel,
  author = {Segovia-Martin, Jose},
  title = {Co-evolutionary model of value systems, institutions and cultural conventions},
  year = {2020},
  publisher = {GitHub},
  journal = {GitHub repository},
  howpublished = {\url{https://github.com/jsegoviamartin/Coevolutionary_model_values_institutions}}
}
