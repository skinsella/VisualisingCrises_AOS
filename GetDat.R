library(devtools)
library(tidyverse)
library(zoo)
library(ecb)
library(lubridate)
library(eurostat)
library(plotly)
library(viridis)
library(alluvial)
library(ggthemes)
library(BIS)
library(ggalluvial)
library(ggrepel)
library(ggthemr)
library(colorspace)
library(ggpubr)
library(geofacet)
library(countrycode)
library(corrplot)

nasq_10_f_bs<-get_eurostat("nasq_10_f_bs", time_format = "num", type = "label")
nasq_10_f_tr<-get_eurostat("nasq_10_f_tr", time_format = "num", type = "label")
nasq_10_nf_tr<-get_eurostat("nasq_10_nf_tr", time_format = "num", type = "label")
nasa_10_f_bs<-get_eurostat("nasa_10_f_bs", time_format = "num", type = "label")
nasa_10_f_tr<-get_eurostat("nasa_10_f_tr", time_format = "num", type = "label")
nasa_10_nf_tr<-get_eurostat("nasa_10_nf_tr", time_format = "num", type = "label")

save(nasq_10_f_bs,file="Data/QuartBS.RData")
save(nasq_10_f_tr,file="Data/QuartTR.RData")
save(nasq_10_nf_tr,file="Data/QuartNFTR.RData")
save(nasa_10_f_bs, file="Data/AnnualBS.RData")
save(nasa_10_f_tr,file="Data/AnnualTR.RData")
save(nasa_10_nf_tr, file="Data/AnnualNFTR.RData")

load("Data/QuartBS.RData")
load("Data/QuartTR.RData")
load("Data/QuartNFTR.RData")

load("Data/AnnualBS.RData")
load("Data/AnnualTR.RData")
load("Data/AnnualNFTR.RData")



NFTRitems<-c("Gross domestic product at market prices","Gross national income at market prices","Disposable income, gross","Saving, gross","Net lending (+) /net borrowing (-)",
             "Compensation of employee"," Taxes on production and imports","Taxes on products","Other taxes on production","Property income","Interest","Rents",
             "Current taxes on income, wealth, etc.","Social contributions and benefits","Capital transfers","Capital taxes","Gross fixed capital formation","Exports of goods and services",
             "Imports of goods and services","Total general government expenditure","Total general government revenue")

FTRItems<-c("Currency and deposits","Short-term debt securities","Long-term debt securities","Short-term - Loans","Long-term - Loans","Equity and investment fund shares","Financial derivatives and employee stock options",
            "Net acquisition of financial assets/ Net incurrence of liabilities","Net financial transactions")
BSItems<-c("Currency and deposits","Short-term debt securities","Long-term debt securities","Short-term - Loans","Long-term - Loans","Equity and investment fund shares","Financial derivatives and employee stock options",
           "Financial net worth")