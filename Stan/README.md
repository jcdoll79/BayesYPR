# Modifications made to original code

1: The original code was written in JAGS. Stan code is currently under development and documentation is not complete. Please use with caution.

2: The current parametrization does not incorporate uncertainty in the assigned age as was done in the original mauscript. Attempts are being made to incorporate this back into the model but it is currenlty not coverging with this added level of uncertainty. Thus, to use the current model requires the assumption that all ages were assigned perfectly.

3: The von Bertlanffy growth model has been expanded to include modeling the correlation among the three estimated coefficients at the global level with a multivariant normal distribution.
