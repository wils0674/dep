# this script extractS data from WEPP files in order to prepare it for WEPS input
# currently set up to read data for one WEPP site and:
# 1) update WEPS file -> 2) run WEPS -> 3) collect WEPP data for next day and -> 4) repeat

# updated: Dec 2019
# currently: not updating the wind speed/direction data - that will happen at IA State
# for testing purposes, the WEPS file has high wind speed for every day of the year

# required input files: grph_0 (WEPP daily output file - obtained from Darryl H. at IA State)
# required input files: p0 (WEPP soil properties file - obtained from Darryl H. at IA State)

args = commandArgs(trailingOnly = TRUE)

# if we are running in interactive mode
if (length(args) == 0) {
  print('length(args) == 0')  
  generic_template_in <- 'WEPStest';
  weppgraphfile <- 'grph_0.txt';
  weppsoilfile <- 'p0.sol';
  Year <- 2019;
  DayofYear <- 365;
} else {
  # get arguments from the command line
  generic_template_in=args[1];
  weppgraphfile=args[2];
  weppsoilfile=args[3];
  Year=as.numeric(args[4]);
  DayofYear=as.numeric(args[5]);
}
generic_template_in.in<-paste(generic_template_in, ".in", sep="")
print(paste('Template File In: ', generic_template_in.in, sep=""))
print(paste('WEPP graph file: ', weppgraphfile, sep=""))
print(paste('WEPP soil file: ', weppsoilfile, sep = ""))
print(paste('Year: ', Year, sep = ""))
print(paste('Day of Year: ', DayofYear, sep = ""))

require(readr)
require(dplyr)
#require(readtext)

# directory/file information here (shouldn't need to change script anywhere else)

#Set working directory 
#####*****working.dir<-"G:/SOIL/ESTIMATORS/Projects/BWSR_TillageStudy/WEPSstandalone/WorkSpace/DailyUpdate_Sept2019"
######working.dir<-"G:/SOIL/ESTIMATORS/Projects/BWSR_TillageStudy/WEPSstandalone/WorkSpace/DailyUpdate_Sept2019"
######setwd(working.dir)

######dir<- working.dir

#*******************shouldn't be any user input required down here*************
# colnames from commented portion of grph_0.txt
colnames<-c("year","Day","Precip_in","AvDet_t-a","MxDet_t-a","DetPt_ft","AvDepo_t-a","MxDepo_t-a","DepoPt_ft","SedLving_lb-ft",
            "5d_avMntmp_F","5d_avMxtmp_F","dlyMntmp_F","dlyMxtmp_F","IrrDpth_in","IrrVol_in","Runoff_in","IntRillNetLoss_t-a","CnpyHt_ft","CnpyCov",
            "LAI","IntRillCover","RillCover","AbvGrndliveBiom_t-a","LiveRootMass_t-a","LRM_0-15cm_t-a","LRM_15-30cm_t-a","LRM_30-60cm_t-a","Rtdepth_in","StndngDeadBiom_t-a",
            "Rsd_t-a","PrevRsd_t-a","OldRsd_t-a","SbmRsd_t-a","PrevSbmRsu_t-a","OldSbmRsd_t-a","DeadRoot_t-a","PrevRoot_t-a","OldRoot_t-a","Porosity%",
            "BlkDens_lbs-ft3","EffHydCond_in-h","Suct_in","ET_in","Drnge_in-day","DpthDrain_ft","EfInt_in-hr","PkRunoff_in-hr","EffRunoffDur_hr","EnrichRatio",
            "AdjKi","AdjKr","AdjTauc","RillWidth_in","PlntTransp_in","SoilEvap_in","Seepage_in","WaterStress","TempStress","TotSoilWater_in",
            "SoilWaterLyr1","SoilWaterLyr2","SoilWaterLyr3","SoilWaterLyr4","SoilWaterLyr5","SoilWaterLyr6","SoilWaterLyr7","SoilWaterLyr8","SoilWaterLyr9","SoilWaterLyr10",
            "RndRough_in","RdgHeight_in","FrstDepth_in","ThwDepth_in","SnwDepth_in","SnwMeltWater_in","SnwDensity_lb-ft3","RillCovFricFac","FricFracLivePlant","RillTotalFricFrac",
            "CompFricFactor","RillCoverRnge","LiveBasalAreaFricFac","LivePlantCanopyFricFac","DaysSinceDisturb","CropType","RsdType","PrevRsdType","OldRsdType","DeadRootType",
            "PrevDeadRootType","OldDeadRootType","SedLeavingOFE_lb-ft","EvapfromRsd","TotFrozSoilWater_in","FrzH2OLyr1","FrzH2OLyr2","FrzH2OLyr3","FrzH2OLyr4","FrzH2OLyr5",
            "FrzH2OLyr6","FrzH2OLyr7","FrzH2OLyr8","FrzH2OLyr9","FrzH2OLyr10")

# set colwidths vector to read "grph_0.txt" as a fixed width file
colwidths<-c(4,6,rep(11,83),rep(3,7),rep(11,13))
colwidths<-as.integer(colwidths) #changes from numeric to integer
WEPPoutAll<- read_fwf(
    weppgraphfile,
    comment = "#",
    skip = 121,
    col_positions = fwf_widths(colwidths, col_names = colnames),
    col_types = "iidddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddiiiiiiiddddddddddddd")
WEPPout <- dplyr::filter(WEPPoutAll, year == Year)
WEPPout$Precip_mm<-WEPPout$Precip_in*25.4
WEPPout$IrrDpth_mm<-WEPPout$IrrDpth_in*25.4
WEPPout$SnwMeltWater_mm<-WEPPout$SnwMeltWater_in*25.4

#***************WEPP soil variables
WEPPsoil<- read.table(weppsoilfile, skip = 4, nrows = 1, sep = " ")
Depthmm<-round(as.numeric(WEPPsoil[1,4]),digits=2)
Sandpct<-round(as.numeric(WEPPsoil[1,5]),digits=2)
Sandpct<-Sandpct/100 #needs to be in decimal percent for calculations

# determine very fine sand based on equation 7.2 from Foster et all., 2004 and USDA-ARS (2013) RUSLE2 documentation.
VFSandpct <- 0.74*Sandpct - 0.62*Sandpct^2

Claypct<-round(as.numeric(WEPPsoil[1,6]),digits=2)
Claypct<-Claypct/100 #needs to be in decimal percent for calculations
Organicpct<-round(as.numeric(WEPPsoil[1,7]),digits=2)
Organicpct<-Organicpct/100 #needs to be in decimal percent for calculations
CECmeq<-round(as.numeric(WEPPsoil[1,8]),digits=2)
Rockpct<-round(as.numeric(WEPPsoil[1,9]),digits=2)
Rockpct<-Rockpct/100 #don't think I need this, but including here for consistency
#compute silt by difference
Siltpct<-round((1-(Sandpct+Claypct)),digits=2)

# Compute soil CaCO3 based on pedotransfer function found in:
# Wang, Q., Y. Li, and W. Klassen. 2005. Determination of Cation Exchange Capacity on Low to Highly
# Calcareous Soils. Communications in Soil Science and Pland Anlaysis, 36: 1479-1498.
# DOI: 10.1081/CSS-200058493 
OrgCgkg<-Organicpct*1000 #to get into proper units (g/kg) for pedotransfer function
Claygkg<-Claypct*1000 #to get into proper units (g/kg) for pedotransfer function
CaCO3gkg<- (CECmeq - 1.909 - 0.0199*OrgCgkg - 0.0199*Claygkg) / 0.0076
CaCO3pct<-CaCO3gkg/1000 #to get back into proper units for WEPS calculations

#################temp CaCO3pct value for comparison to WEPS outputs###################Sept24_2019
CaCO3pct<-0.00 #based on soils data for WAUKON FSL in WEPS model for comparison here
#################remove this later####################################################Sept24_2019

# compute soil bulk desity from a pedo transfer function by Martin et al., 2017 (PTF4 from Table 2)
# pb = 1.70398 - 0.00313*silt + 0.00261*clay - 0.11245*OrgC
# Martin, M.A., M. Reyes, and F.J. Taguas. 2017. Estimating soil bulk density with information metrics of soil texture
# Geoderma 287, 66-70.
# pb<- 1.70398 - 0.00313*Siltpct + 0.00261*Claypct - 0.11245*Organicpct
# turns out I don't need this pedotransfer function, there is a bulk density value in WEPP outputs

#Extract daily data from "grph_0.txt", write WEPS.in file and run SWEEP 

#####*****old section for running the WEPPgraph file as a loop.  Should be able to delete this
for (i in 1:nrow(WEPPout)){

print(i)

WEPS<-readLines(generic_template_in, n=-1)
# substitute WEPS parameter values on a line-by-line basis

### Compute SWEEP soil crust and aggregate parameters based on equations from SWEEP technical documentation
### Comment numbers in brackets refer to [page number, equation number] from:
### WEPS technical documentation, BETA release 95-08 (downloaded pdf from WEPS website)
### OR SWEEP documentation; default is WEPS documentation. If SWEEP documentation, it'll be noted

#CUMP = cumulative depth of snowmelt, rainfall, and sprinkler irrigation (mm)
#CUMSR = cumulative depth of sprinkler irrigation and rainfall (mm)
#These cumulative values reset to zero when there has been soil disturbance WEPPout$DaysSinceDisturb < 1 ###########################Sept 27 2019

if (WEPPout$DaysSinceDisturb[i]<=2) {
  CUMP <- 0.0
  CUMSR <- 0.0
} 
  
if (i==1) {
  CUMP <- WEPPout$SnwMeltWater_mm[i]+WEPPout$Precip_mm[i]+WEPPout$IrrDpth_mm[i]
  CUMSR <- WEPPout$Precip_mm[i]+WEPPout$IrrDpth_mm[i]
} else {
  CUMP <- CUMP + WEPPout$SnwMeltWater_mm[i]+WEPPout$Precip_mm[i]+WEPPout$IrrDpth_mm[i]
  CUMSR <- CUMSR + WEPPout$Precip_mm[i]+WEPPout$IrrDpth_mm[i] 
}

#SDag = Soil Layer Aggregate Density [SWEEP document page 17]  Mg/m3
SDag <- 2.01*(0.72 + 0.00092 * Depthmm)
#adjustments for absolute range allowed in SWEEP document page 17
if (SDag < 0.6){
  SDag <- 0.6
} else if (SDag > 2.5){
  SDag <- 2.5
}

#SEag = Soil Layer Aggregate Stability [SWEEP document page 17]  ln(J/kg)
SEag <- 0.83 + 15.7*Claypct - 23.8*Claypct^2
#adjustments for absolute range allowed in SWEEP document page 17
if (SEag < 0.1){
  SEag <- 0.1
} else if (SEag > 7.0){
  SEag <- 7.0
}
  
#Slagm = Aggregate Geometric Mean Diameter [modified, see new SWEEP document page 17]
Slagm <- exp(1.343-2.235*Sandpct-1.226*Siltpct-0.0238*(Sandpct/Claypct) 
             + 33.6*Organicpct+6.85*CaCO3pct)*(1+0.006*Depthmm)
#adjustments for absolute range allowed in SWEEP document page 17
if (Slagm < 0.03){
  Slagm <- 0.03
} else if (Slagm > 30.0){
  Slagm <- 30.0
}


#S0ags = Aggregate Geometric Standard Deviation (that's a zero "0", not an "O") [SWEEP document page 17]
S0ags <- 1.0 / (0.0203 + 0.00193*Slagm + 0.074 / Slagm^0.5)
#adjustments for absolute range allowed in SWEEP document page 17
if (S0ags < 1.0){
  S0ags <- 1.0
} else if (S0ags > 20.0){
  S0ags <- 20.0
}


#SDbko = Settled bulk density at 300mm depth [S-15, 51]
SDbko <- SDag

#SDcr = Soil Crust Density [S-15, 52]
SDcr <- 0.576 + 0.603 * SDbko 

#############this equation probably needs to be updated!!!
######look at WEPS tech document page S-9 equation 23 and 21
#SFlos = Fraction Loose Material on Surface [S-8, 21]
if (CUMSR < 10) {
  SFlos <- 0 #based on equation and SWEEP manual page 18
} else if (CUMSR >= 10) {
  SFlos <- 0.14 + 0.001*CUMSR
}
# range guidance from SWEEP documentation page 18
if (SFlos<0){
  SFlos <- 0.0
} else if (SFlos > 1.0){
  SFlos <- 1.0
}

#Slagn = Soil Layer Minimum Aggregate Size [SWEEP document page 17]
Slagn <- 0.01 

#SMXlos = Maximum Loose, Erodible Mass on Crust [S-8, 19]
SMXlos <- 0.1*exp(-0.57+0.22*(Sandpct/Claypct) + 7.0*CaCO3pct - Organicpct)

#SMlos = Mass of Loose Material on Crust [S-8, 20], see also SWEEP document page 18  (kg/m2)
if (CUMSR < 10) {
  SMlos <- 3
} else if (CUMSR < 180) {
  SMlos <- SMXlos*(1 - 0.005 * CUMSR)
} else if (CUMSR >= 180){
  SMlos <- 0.0
}
# range guidance from SWEEP documentation page 18
if (SMlos < 0){
  SMlos <- 0.0
} else if (SMlos > 3.0){
  SMlos <- 3.0
}

#Slagx = Soil Layer Maximum Aggregate Size [SWEEP document page 17]
p <- 1.52 * S0ags^-0.449
Slagx <- S0ags * Slagm + 0.84^p
#adjustments for absolute range allowed in SWEEP document page 17
if (Slagx < 1.0){
  Slagx <- 1.0
} else if (Slagx > 1000.0){
  Slagx <- 1000.0
}

#SEag = Aggregate Stability Crushing Energy [S-9, 24]
SEag <- 0.83 + 15.7*Claypct - 23.8*Claypct^2

#SFcr = Soil Fraction Crust Cover [S-7, 17]
if (CUMP < 10) {
  SFcr <- 0.0
} else if (CUMP >= 10) {
  SFcr <- 0.36 + 0.0024 * CUMP
}
# range guidance from SWEEP documentation page 18
if (SFcr < 0){
  SFcr <- 0.0
} else if (SFcr > 1.0){
  SFcr <- 1.0
}

#SEcr = Crust Stability Crushing Energy [S-7, 18]
SEcr <- SEag

#SZcr = Crust Thickness [S-6, 14]
Acr <- -0.072 * 0.2*Claypct
Bcr <- 1.56 - 2.9 * Claypct
if (CUMP < 200){
  SZcr <- Acr * CUMP + Bcr * CUMP^0.5 
} else if (CUMP >= 200){
  tempCUMP <- 200
  SZcr <- Acr * tempCUMP + Bcr * tempCUMP^0.5 
}
# range guidance from SWEEP documentation page 18
if (SZcr < 0){
  SZcr <- 0.0
} else if (SZcr > 23.0){
  SZcr <- 23.0
}

# Only need to update the file when we are actually going to write something
if (i < DayofYear){
    next
}

### WEPS.IN +++ BIOMASS +++ 
WEPS[86]<-paste(" ",(round((WEPPout$CnpyHt_ft[i]*0.3048),digits=2)),sep="") #biomass height on day i; converted to m
WEPS[89]<-paste(" ",(round((WEPPout$CnpyHt_ft[i]*0.3048),digits=2)),sep="") #crop height on day i;  converted to m


#***************need to find way to estimate Stem Area Index**********
WEPS[93]<-paste(" ",0.0,"", (round(WEPPout$LAI[i], digits=2)),collapse="") #crop StemAI and LAI
#   WEPS[120]<-paste(" ",(round(WEPPout$LAI[i], digits=2)),"", (round(WEPPout$LAI[i], digits=2)),collapse="") #crop StemAI and LAI
   
#line 141 is flat biomass cover m2/m2, Need to convert residue t/ac to biomass cover (m/m)
#Percent cover estimated from residue biomass after equtions provided in Gregory, 1982
#Currently using a single cover coefficient (corn), need to find a way to update this by crop
Residuelbac<-WEPPout$`Rsd_t-a`[i]*2000 #convert t/ac to lb/acre
covercoef<-0.00038 #currently based on value for corn from Gregory 1982 (need to update for other crops)
Cover<-(1-exp(-covercoef*Residuelbac))
WEPS[114]<-Cover

### WEPS.IN +++ SOIL +++ 
WEPS[119]<-paste(" ",1,sep="")  #number of soil layers (WEPS only needs info for the top layer)
WEPS[124]<-paste(" ",Depthmm,sep="") #thickness of soil layer in mm
   
bulkdensity<-round((WEPPout$`BlkDens_lbs-ft3`[i]*16018.47/1000000),digits=2)
WEPS[127]<-paste(" ",bulkdensity,sep="")
WEPS[130]<-paste(" ",(round((Sandpct),digits=2)),sep="")
WEPS[132]<-paste(" ",(round((VFSandpct),digits=2)),sep="") 
WEPS[134]<-paste(" ",(round((Siltpct),digits=2)),sep="")
WEPS[136]<-paste(" ",(round((Claypct),digits=2)),sep="")
WEPS[139]<-paste(" ",(round((Rockpct),digits=2)),sep="")

# new in spring/summer 2019 (this note is just to help with trouble shooting)
WEPS[142]<-paste(" ",(round((SDag), digits=2)),sep="") #Soil layer aggregate density (Mg/m^3)
WEPS[144]<-paste(" ",(round((SEag), digits=2)),sep="") #Soil layer aggregate stability (ln(K/kg))
WEPS[146]<-paste(" ",(round((Slagm), digits=2)),sep="") #Soil layer geometric mean diameter (mm)
WEPS[148]<-paste(" ",(round((Slagn), digits=2)),sep="") #Soil layer minimum aggregate size (mm)
WEPS[150]<-paste(" ",(round((Slagx), digits=2)),sep="") #Soil layer maximum aggregate size (mm)
WEPS[152]<-paste(" ",(round((S0ags), digits=2)),sep="") #Soil layer geometric std deviation (mm/mm)

#next line contains: Surface Crust Fraction, Surface Crust Thickness, Fraction Loose Material on Surface,
#Mass of loose material on the crust, soil crust density, soil crust stability
WEPS[161]<-paste("",(round((SFcr), digits=2)),(round((SZcr), digits=2)),
                 (round((SFlos), digits=2)),(round((SMlos), digits=2)),
                 (round((SDcr), digits=2)),(round((SEcr), digits=2)),sep=" ") 

WEPS[166]<-paste(" ",(round((WEPPout$RndRough_in[i]*25.4),digits=2)),sep="") #Allmaras Random Roughness (convert to mm)


### WEPS.IN +++ HYDROLOGY +++   
      
WEPS[178]<-paste(" ",(round((WEPPout$SnwDepth_in[i]*25.4),digits=2)),sep="") #snow depth converted to mm

#convert soil water to proper units (Mg/Mg)
SoilWatercm<-WEPPout$SoilWaterLyr1[i]*2.54 #assume water density of 1g/cc
Soilpb<-as.numeric(bulkdensity)
SoilMass<-Depthmm/10*Soilpb
Layer1WaterContent<-round((SoilWatercm/SoilMass),digits=2)
WEPS[184]<-paste(" ",Layer1WaterContent,sep="")

SurfaceLayerWaterContent<-(c(rep(Layer1WaterContent,12))) #daily WEPP value, repeated here for hourly WEPS input
WEPS[190]<-paste(" ",SurfaceLayerWaterContent,collapse="")
WEPS[191]<-paste(" ",SurfaceLayerWaterContent,collapse="")

### WEPS.IN +++ WEATHER +++  
#this will be where wind data are input

if (i == DayofYear){
    write(WEPS, file=generic_template_in, append=FALSE)
    break
}

} # End of loop
