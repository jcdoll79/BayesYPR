# Modifications made to original code

1: The original code was written in JAGS. Stan code is currently under development and documentation is not complete. Please use with caution.

2: The current parametrization does not incorporate uncertainty in the assigned age as was done in the original mauscript. Attempts are being made to incorporate this back into the model but it is currenlty not coverging with this added level of uncertainty. Thus, to use the current model requires the assumption that all ages were assigned perfectly.

3: The von Bertlanffy growth model has been expanded to include modeling the correlation among the three estimated coefficients at the global level with a multivariate normal distribution (Midway et al. 2015).

Midway, S.R., T. Wagner, S.A. Arnott, P. Biondo, F. Martinez-Andrade, and T.F. Wadsworth. 2015. Spatial and temporal variability in growth of southern flounder (<i>Paralichthys lethostigma</i>). Fisheries Research 167:323-332. https://doi.org/10.1016/j.fishres.2015.03.009
