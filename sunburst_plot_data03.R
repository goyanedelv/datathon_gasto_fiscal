setwd("/datathon_gasto_fiscal/")

library(sunburstR)
library(htmlwidgets)

# Preámbulo

	## Paths
	path_01="Core_Dataset_1_monthly_spend_2009-2017"
	path_02="Core_Dataset_2_Anual_revenue_and_spend_2009-2017_by_Programs_(millions)"
	path_03="Core_Dataset_3_Revenue_and_Spending_CG_Real_values"
	path_04="Additional_Dataset_4_Population_of_Chile_1990-2017"
	path_05="Additional_Dataset_5_GDP_Chilean_1990-2017_millions_chilean_pesos"
	path_06="Additional_Dataset_6_Revenue_and_Spending_CG_Nominal values"

	## Filenames
	file_01="dataset_1_monthly_spend_2009-2017.csv"
	file_02="dataset_2_Anual_revenue and spend_2009-2017 by Programs (millions).csv"
	file_03="dataset_3_Revenue_and_ pending_(Central_Government)_1990-2017 (real).csv"
	file_04="dataset_4_Population of Chile 1990 -2017.csv"
	file_05="dataset_5_GDP Chilean 1990-2017_millions chilean pesos.csv"
	file_06="dataset_6_Revenue_and_ pending_(Central_Government)_1990-2017 (nominal).csv"

	paths=c(path_01,path_02,path_03,path_04,path_05,path_06)
	files=c(file_01,file_02,file_03,file_04,file_05,file_06)

	## Función de lectura
	lector<-function(number){

		path=paths[number]
		file=files[number]

		data <- read.table(unz(paste0("00_data/",path,".zip"),
		 paste0(path,"/",file)),header=T, sep=";", encoding = "UTF-8", quote="\"",fill=TRUE)

		return(data)
	}

#Sunburst para la data 3

	data03=lector(3)

	data03$Periodo=as.factor(data03$Periodo)

	data03$Periodo=paste0("Año_",data03$Periodo)

	data03$NIVEL1=NULL

	data03=unique(data03)

	data03$Real_amount=gsub(",",".",data03$Real_amount)

	data03$Real_amount=as.numeric(data03$Real_amount)	

	data03$jerarq=paste0(	data03$Periodo,"-",
							data03$NIVEL2,"-",
							data03$NIVEL3)

	data_sb=data.frame(Jerarquia=data03$jerarq,Monto=data03$Real_amount)

	test=sunburst(data_sb,percent=FALSE,count=TRUE)

	test

	setwd("/datathon_gasto_fiscal/05_output/a_produccion")

	saveWidget(test, "wholedatabase3.html", selfcontained = FALSE)