# ----------------------------------------
# Tentamen Tillämpad statistik, VT20
# Namn: Erik Tille
# Personnummer: 000908-7651
# ----------------------------------------



# Uppgift 1 # --------------------------------------------------------------------------------------------
rm(list = ls())
# Bestäm ditt personliga datamaterial genom att
# fylla i ditt personnummers sex första siffror istället 
# för YYMMDD i set.seed()
set.seed(000908)
library("Ecdat")
data("Wages1")
df <- Wages1[sample(nrow(Wages1), 500) , ]

# 1.1
xbar <- mean(df$school) # Tar fram stickprovsmedelvärdet för x
df$dx <- df$school - xbar # Tar fram ny variabel för differansen av x och xbar
df$dx_sq <- df$dx^2 # Tar fram ny variabel för dx i kvadrat
SSxx <- sum(df$dx_sq) # SSxx är summan av kvadrerade dx

ybar <- mean(df$wage) # Tar fram stickprovsmedelvärdet för y
df$dy <- df$wage - ybar # Tar fram ny variabel för differansen av y och ybar
df$dxdy <- df$dx * df$dy # Multiplicerar båda värdena med varandra
SSxy <- sum(df$dxdy) # SSxx är summan av dx * dy
# Svar: SSxx = 1378,352 och SSxy = 693,090

# 1.2
b1_hat <- SSxy / SSxx # Formeln för att ta fram skattade beta 1 
# Svar: Kvoten 0.5028393 är vårt skattade beta 1, alltså lutningen för koefficienten b1.
# Det innebär att timlönen ökar med i genomsnitt USD 0.5028393 per års extra utbildning.

# 1.3
ols_model1 <- lm(wage~school, data = df) # Skapar ols-modell
wage_given_school14 <- ols_model1$coefficients[1] + ols_model1$coefficients[2] * 14
# Ovanstående ekvation skattar timlönen för en person med 14 års utbildning, alltså USD 6.908795

# 1.4 
df$pred_vals_wage <- predict(ols_model1) # Skapar ny variabel för predicerade värden på wage
df$wage_error <- df$pred_vals_wage - df$wage # Residualen
df$wage_error_sq <- df$wage_error^2 # Residualen i kvadrat
SSE <- sum(df$wage_error_sq) # Formel för att ta fram SSE
MSE <- SSE / (500 - 2) # Formel för att ta fram MSE
# Svar: SSE är residualkvadratsumman, 4159.164 och MSE är medelkvadratfelet 8.351735

# 1.5 
t_alpha <- qt(0.975, (500 - 2)) # t-värdet för tvåsidigt 95% konfidensintervall
KI <- c(wage_given_school14 - t_alpha*sqrt(MSE), wage_given_school14 + t_alpha*sqrt(MSE)) 
# Konfidensintervallet ovan.
# Svar: Med 95% säkerhet är timlönen för en person med 14 års utbildning mellan 
# USD 1.230823 och USD 12.586766.


# Slut Uppgift 1 -----------------------------------------------------------------------------------------






# Uppgift 2 # --------------------------------------------------------------------------------------------
rm(list = ls())
# Bestäm ditt personliga datamaterial genom att
# fylla i ditt personnummers sex första siffror istället 
# för YYMMDD i set.seed()
set.seed(000908)
library("Ecdat")
data("Wages1")
df <- Wages1[sample(nrow(Wages1), 500) , ]

# 2.1
ols_model2 <- lm(log(wage)~school, data = df) # Ny modell med lön på logaritmisk skala
# Svar: Riktiningskoefficienten beta 1 blir nu 0.10003 vilket tyder på en procentuell ökning av lönen
# med 10,003% per extra utbildningsår.

# 2.2
df$D <- ifelse(df$sex == "female", 1, 0) # Skapar binär variabel för kön
df$D_school <- df$D * df$school # Skapar interaktionsvariabel för kön och utbildning
ols_model3 <- lm(wage ~ school + D + D_school, data = df) # Skapar OLS-modell
# Svar : Skattningen av beta 1 vilket är 0,557 tolkas som att timlönen ökar så mycket per extra 
# utbildningsår för både kvinnor och män i USD. Skattningen av beta 3 vilket är 0.002837 tolkas som 
# att kvinnor får USD 0.002837 mer i ökning av timlönen än vad män får per extra utbildningsår.

# 2.3
summary(ols_model3) # Tar fram nödvändiga värden för koefficienterna
# Svar: I "summary" kan vi se att t-värdet för koefficient beta 3 är 0,018. Det är alltså
# inte tillräckligt stor skillnad för att vi på 5% signifikansnivå ska kunna förkasta 
# nollhypotesen om att beta 3 är lika med 0. P-värdet är 0,986, alltså betydligt större än 0,05
# vilket vi använder som signifikansnivå.


# Slut Uppgift 2 -----------------------------------------------------------------------------------------






# Uppgift 3 # --------------------------------------------------------------------------------------------
rm(list = ls())
# Bestäm ditt personliga datamaterial genom att
# fylla i ditt personnummers sex första siffror istället 
# för YYMMDD i set.seed()
set.seed(000908)
library("Ecdat")
data("Doctor")
df <- Doctor[sample(nrow(Doctor), 485, replace = TRUE), ]

# 3.1
N <- 22000 # Totalt antal hushåll i distriktet
xbar <- mean(df$doctor) # Medelvärde för antal besök hos doktorn per hushåll
tau_hat <- N * xbar # Skattning av totala läkarbesöken i distriktet
# Svar: Skattning av totala läkarbesöken i distriktet är 35563 st

# 3.2
s2 <- var(df$doctor) # Tar fram variansen för x
n <- length(df$doctor) # Tar fram n, alltså stickprovsstorleken
vxbar <- (N^2) * (1 - n / N) * (s2 / n) # Formel för variansskattning till tau
t_alpha <- qt(0.975, (485 - 1)) # t-värdet för tvåsidigt 95% konfidensintervall
KI <- c(tau_hat - t_alpha*sqrt(vxbar), tau_hat + t_alpha*sqrt(vxbar)) # Konfidensintervall 95%
# Svar: Med 95% säkerhet är totala antalet läkarbesök mellan 29493.05 och 41632.72. 
# Avrunda uppåt på grund av att antal besök inte kan vara ett decimaltal. 
# Konfidensintervallet blir 29494 < 35563 =< 41633. 

# 3.3 

# Tar fram nödvändiga värden för stratum 1
N1 <- 9000
xbar1 <- mean(df$doctor[df$children == 1])
n1 <- length(df$doctor[df$children == 1])

# Tar fram nödvändiga värden för stratum 2
N2 <- 7000
xbar2 <- mean(df$doctor[df$children == 2])
n2 <- length(df$doctor[df$children == 2])

# Tar fram nödvändiga värden för stratum 3
N3 <- 6000
xbar3 <- mean(df$doctor[df$children > 2])
n3 <- length(df$doctor[df$children > 2])

# Stratum j, skapar vektorer för alla stratums värden
Nj <- c(N1, N2, N3) # N för populationen per stratum
nj <- c(n1, n2, n3) # n för stickprovsstorleken per stratum
xbarj <- c(xbar1, xbar2, xbar3) # alla stratums stickprovsmedelvärden

taust_hat <- sum((Nj)*xbarj) # Tar fram stickprovstotalen stratumviktat 
# Svar: Skattning av totala läkarbesöken i distriktet är 35569 st efter stratifierng


# 3.4
# Tar fram variansskattningarna för respektive stratum
s12 <- var(df$doctor[df$children == 1])
s22 <- var(df$doctor[df$children == 2])
s32 <- var(df$doctor[df$children > 2])
sj2 <- c(s12, s22, s32) # alla stratums variansskattningar

vxbarst <- sum((Nj^2) * (1 - nj / Nj) * (sj2 / nj)) # Formel för stratumvariansskattningen
KI_st <- c(taust_hat - t_alpha*sqrt(vxbarst), taust_hat + t_alpha*sqrt(vxbarst)) # Konfidensintervall 95%
# Svar: Med 95% säkerhet är totala antalet läkarbesök mellan 29250.03 och 41887.23 efter stratifierng.
# Avrunda uppåt på grund av att antal besök inte kan vara ett decimaltal. 
# Konfidensintervallet blir 29251 < 35569 =< 41888. 

# 3.5 
# Skillnaden mellan efterstratifiering och stratifierat urval är att efterstratifiering 
# görs efter ett stickprov redan är taget och att ett stratifierat urval är en metod för 
# att dra ett stickprov. Konsekvensera av att inte stratifiera i urvalsprocessen
# är att vissa stratum kan av slumpen få väldigt små stickprov vilken försämrar precisionen.


# Slut Uppgift 3 -----------------------------------------------------------------------------------------





# Uppgift 4 # --------------------------------------------------------------------------------------------
rm(list = ls())
# Bestäm ditt personliga datamaterial genom att
# fylla i ditt personnummers sex första siffror istället 
# för YYMMDD i set.seed()
set.seed(000908)
library("Ecdat")
data("Doctor")
df <- Doctor[sample(nrow(Doctor), 485, replace = TRUE), ]

# 4.1
hist(df$access * 100, main = "Index över tillgång till sjukvård", ylab = "Antal hushåll", 
     xlab = "Indexvärde i procent (%)", col = "lightblue", xlim = c(0, 100))
# Jag har valt ett histogram för att åskådliggöra fördelningen av indexet tillgång till sjukvård.
# Jag tycker att det visar fördelningen på ett tydligt och enkelt sätt.
# Jag har döpt axlarna och rubriken för att tydligt kunna se vad histogrammet visar.
# Jag har valt att färglägga histogrammet i ljusblått för att jag 
# tycker det är lättare att se fördelningen. x - axeln är utdragen till intervallet
# 0-100 % för att se hela möjliga intervallet. Skalan är omgjord till % för att jag tycker det är
# ett tydligare sätt att se ett index. 


# 4.2 
df$access_bin <- ifelse(df$access < 0.2, 1, 0) # Skapar binär variabel för sjukvårdstillgång
n_bad_access <- sum(df$access_bin) # Räknar ut antalet ettor i binära variabeln.
# Svar: Antalet personer med dålig tillgång till sjukvård i stickprovet är 93 st. 

# 4.3 
df$doctor_cat <- NA # Skapar en tom variabel
df$doctor_cat[df$doctor == 0] <- 0
df$doctor_cat[df$doctor == 1] <- 1
df$doctor_cat[df$doctor > 1] <- 2
# Ovan är en kategorivariabel skapad för antal läkarbesök per hushåll

table1 <- table(df$access_bin, df$doctor_cat) # Skapar tabell över binära variabeln access_bin och 
# kategorivariabeln doctor_cat

prop_table1 <- prop.table(table1) * 100 # Tabell där vi ser procent av hushållen i respektive kategori.
# På första raden visas de hushåll som har god tillgång till sjukvård där vi ser att en majoritet 
# har besökt sjukvården minst en gång och en stor del fler än två gånger. På andra raden där 
# hushåll med dålig tillgång till sjukvård visas kan vi se att majoriteten aldrig besöker
# sjukvården och att få har fler läkarbesök än 1. 


# 4.4
chisq.test(table1) # Genomför chi2-test för access_bin och doctor_cat.
# Jag väljer att genomföra ett chi2-test för att testa signifikansen i skillnaden mellan de
# olika värdena. Testet visar ett p-värde på 0.006611 vilket gör att vi inte kan förkasta nollhypotesen
# om att det inte finns någon skillnad på 5% signifikansnivå vilket är den vanligaste nivån.
# Däremot ser vi ett relativt lågt p-värde vilket antyder att det är låg sannolikhet att 
# få ett minst så lågt värde som 0.006611, närmre bestämt 6,611% sannolikhet. Värdet gör att vi kan 
# förkasta nollhypotesen på exempelvis 10% signifikansnivå men med en större osäkerhet
# i jämförelse med en lägre signifikansnivå. 


# Slut Uppgift 4 -----------------------------------------------------------------------------------------

