# MATH 3MB3 - Final Report on Mathematical Modelling

## Team Members
- Howard Wang
- Jordan Yang
- Yiming Xia
- Zitong Gu
- Zhexi Yin

## Introduction
This project focuses on the predictive modeling of human impacts on the carbon cycle. Our analysis concentrates on a small-scale carbon cycle environment, examining the effects of human activities like tree cutting on carbon density across different components of the ecosystem.

## Models Developed
### Base Model
Our base model simulates carbon flow in an environment excluding oceans and direct human influences. It consists of two scenarios:
- **Scenario 1:** A simple model tracking carbon through litter, assuming constant area and carbon input/output rates.
- **Scenario 2:** A more complex iteration considering the respiration rates of humus and litter, and plant growth affecting litter carbon density.

### Model Expansion
To simulate real-world scenarios, we introduced additional components like plant parts and stable humus charcoal, along with mechanisms to account for area change due to human activities and seasonal variations.

## Results
### Primary Results
The expanded model shows the impact of human activities on carbon density, indicating a general decrease in carbon density over time due to human intervention, except in the case of stable humus charcoal.

### Seasonal Effects
We explored how seasonal harvesting affects carbon density in an agricultural ecosystem, showing significant fluctuations and overall growth in carbon density due to human interference.

## Discussion
### Implications of Results
Our findings highlight the critical role of forests and biomass-rich ecosystems as carbon sinks and suggest strategies for carbon sequestration and climate mitigation.

### Limitations of Models
Our models simplify complex natural systems and might not accurately represent all physical, chemical, and biological interactions, which could affect their applicability in different real-world scenarios.

## References
1. J Goudriaan and P Ketner. "A simulation study for the global carbon cycle, including man’s impact on the biosphere." Climatic Change, 1984.
2. Thomas A Weber and Karsten Neuhoff. "Carbon markets and technological innovation." Journal of Environmental Economics and Management, 2010.
3. Hye Sun You, Jill A Marshall, and Cesar Delgado. "Assessing students’ disciplinary and interdisciplinary understanding of global carbon cycling." Journal of Research in Science Teaching, 2018.

## Code
For the R scripts and detailed implementation of our models, see the accompanying `script.R` file provided in this repository.
