# R CPC engine
- This application aim to provide clinical services utilizing the exsisting, recognized models into the real world.

## Features
1. Web-based (R-shiny)
2. Runnig nlme models for clinical pharmacokinetic consultation service, Korea
3. Needed libraries
   - shiny
   - shinydashboard
   - dashboardthemes
   - shinyWidgets
   - devtools
   - rsconnect
   - RxODE
   - nlmixr
   - reticulate
   - dplyr
   - ggplot2
   - plotly
   - rhandsontable

## Instruction
1. Data for each model is stored on the platform

2. Select the desired ID and click "Load" at the top to fetch the associated data.

![image1](https://github.com/user-attachments/assets/8b592fc2-7a57-4eb2-a968-4d5f0216b3f1)

3. Information about the loaded patient, selected model, drug administration and observed plasma concentration could be checked 

![image2](https://github.com/user-attachments/assets/94cdc767-648b-44fb-bb1b-aa693b7ee5a0)

4. Values derived from the built-in PK or PKPD models on the platform are statistically processed and displayed as graphs.

![image3](https://github.com/user-attachments/assets/3dc1a3a3-4a60-4b66-b7fe-b5bcc0920a6d)

5. Input a hypothetical dosing scenario and click "Run Model" at the top to execute the simulation.
   Click "Calculate steady state PK params" in the "Scenario-based predictions" tab to view NCA parameter results for various scenarios.

![image4](https://github.com/user-attachments/assets/f18aed3f-d6fe-4249-8abb-9982abeeae6a)

6. Select the parameters(Cpeak, Ctrough, AUC0-24, AUCtau), define the window for each parameter, and click "Calculate PTA" to compute the probability of therapeutic success.

![image5](https://github.com/user-attachments/assets/2b9984ff-449c-4293-9543-be0d3c58786c)
