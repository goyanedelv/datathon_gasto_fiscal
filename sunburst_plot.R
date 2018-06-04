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

# Análisis de Data02 || DESCRIPCIÓN DEL PRESUPUESTO
	data02=lector(2)

	## Jerarquización de Partidas (jerarquización manual)
	jerarq=read.csv("04_intermediate/JerarquiaPartidas.csv",fileEncoding="UTF-8")

	jerarq$Clasif[jerarq$Autonomo==1]="Organismo Autonomo"
	jerarq$Clasif[jerarq$Industria.y.Hacienda==1]="Industria y Hacienda" 
	jerarq$Clasif[jerarq$Infraestructura==1]="Infraestructura" 
	jerarq$Clasif[jerarq$Interior.y.Gobierno==1]="Interior y Gobierno"
	jerarq$Clasif[jerarq$Seguridad.Social==1]="Seguridad Social"
	jerarq$Clasif[jerarq$Educacion==1]="Educacion" 
	jerarq$Clasif[jerarq$Ciudadania==1]="Ciudadania"
	jerarq$Clasif[jerarq$Salud==1]="Salud"
	jerarq$Clasif[jerarq$Tesoreria==1]="Tesoreria"
	jerarq$Clasif[jerarq$Medio.Ambiente==1]="Medio Ambiente"

	key_sector=data.frame(Partida=levels(data02$Partida), Sector=jerarq$Clasif)

	data02_sector <- merge(data02,key_sector,by="Partida")

# Simplificando
	data02_sector$ASIGNACI.U.0094.N=NULL
	data02_sector$ITEM=NULL
	data02_sector$Ejec_Acum_Real=NULL
	#data02_sector$TIPO=NULL
	data02_sector$Programa=NULL
	data02_sector$SUBT.U.0090.TULO=NULL

	data02_sector=unique(data02_sector)
	data02_sector=subset(data02_sector,TIPO=="GASTOS")
	data02_sector$TIPO=droplevels(data02_sector$TIPO)


	#data02_sector$jerarq=paste0(	data02_sector$Cap.U.0090.tulo,"-",
	#								data02_sector$Partida,"-",
	#								data02_sector$Sector,"-",
	#								data02_sector$Periodo)

	data02_sector$jerarq=paste0(	data02_sector$Periodo,"-",
								data02_sector$Sector,"-",
								data02_sector$Partida,"-",
								data02_sector$Cap.U.0090.tulo)


	data_sb=data.frame(Jerarquia=data02_sector$jerarq,Monto=data02_sector$Ppto_inicial_Real)

	test=sunburst(data_sb)
	setwd("/datathon_gasto_fiscal/05_output/a_produccion")

	saveWidget(test, "wholedatabase2.html", selfcontained = FALSE)

# Bonus Track 2017
	data02_egr_tes=subset(data02,data02$TIPO=="GASTOS" & data02$Partida==levels(data02$Partida)[29])
	data02_egr_tes=droplevels(data02_egr_tes)
	agg_egr_tes <- aggregate(x = data02_egr_tes$Ppto_inicial_Real,
	  by = list(data02_egr_tes$ITEM,data02_egr_tes$Periodo,
	  	data02_egr_tes$Programa,data02_egr_tes$SUBT.U.0090.TULO), FUN = sum,na.rm=TRUE)
	names(agg_egr_tes)<- c("ITEM","Periodo","Programa","Subtitulo","Monto")

	jerarq_egr_tes=read.csv("04_intermediate/jerarq_egr_tes.csv")

	agg_egr_tes2=merge(agg_egr_tes,jerarq_egr_tes,by="ITEM")

	agg_egr_tes2$jerarq=paste0(agg_egr_tes2$Njerarq,"-",
							agg_egr_tes2$ITEM,"-",
							agg_egr_tes2$Subtitulo,"-",
							agg_egr_tes2$Programa)

	data_sb=data.frame(Jerarquia=agg_egr_tes2$jerarq,Monto=agg_egr_tes2$Monto)

	test=sunburst(data_sb)

	test