Adapting CRISP-DM to Model Enteric Fermentation Emission: Farm Level Application 

This public version of the code employed to build GLEAM model using multiple CRISP-DM approach to implement modules.
The implemented model employ agriculture survey for production and area, available in public repositories for Ecuador [here](https://www.ecuadorencifras.gob.ec/informacion-de-anos-anteriores-espac/)
Four modules were implemented: 

![image link](https://github.com/PBG-Ec/AgCRISP-DM/blob/main/Sumypic.png)

# I. HERD MODULE : Typology Mean weight Tier2 param Milk average

- Farm db: cattle breeding farms 
- Mean protein produced: prot_venta_carn > prot_venta_l
- Mean Weights 
- Other Parameters Compiled with Tier 2 reference
- Average milk production per db by Canton, class & yr

# II. CROP / RATION AND INTAKE  MODULE  :  farm digestibility db 

- GAEZ Parameters, GLEAM Parameters 
- FPAT_TP FEED GROUP REFERENCE GLEAM EC GCI PARAMETERS 
- CROP DB : Select crop 
- GAEZ DATA FOR ESPAC 2000-2020 
- MERGE GZ GLR & FPAT 
- RUN GLEAM FEED RATION MODULE (chap 3) 

# III. EMISSION  MODULE  :  Apply IPCC formula build animal  db 

- MEAN Digestibility 
- COMPUTE ENERGY & EMISSION 
