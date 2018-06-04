
setwd("/datathon_gasto_fiscal/")
library(ggplot2)
library(ggalt)
library(plotly)
library(htmlwidgets)

Sys.setenv("plotly_username"="*******")
Sys.setenv("plotly_api_key"="***************")

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


#Gastos e ingresos
	pibpo=read.csv("04_intermediate/as_pib.csv")
	names(pibpo)[1]="Sector"

	pibpo_agg=aggregate(x=pibpo$Monto_PIB,by=list(pibpo$Periodo,pibpo$Grupo),FUN=sum,na.rm=TRUE)
	colnames(pibpo_agg)<-c("Periodo","Grupo","Monto_PIB")

#PIB
	data05=lector(5)
	inflacion=read.csv("04_intermediate/inflation.csv",sep=",")

	#inflacion$rate=(1+inflacion$inflation/100)
	inflacion$rate=(1+inflacion$deflac/100)

	largo=nrow(inflacion)
	for(j in 1:largo){

	  inflacion$factor[j]=prod(inflacion[j:nrow(inflacion),]$rate)
	  inflacion$factor[j]=inflacion$factor[j]/inflacion$rate[j]
	}
	#inflacion$factor[10]=1
	colnames(inflacion)[1]<-"A_o"
	data05b=merge(data05,inflacion,by="A_o")
	colnames(data05b)[c(1,2)]<-c("year","PIB_Nominal_MM")

	data05b$PIB_Real_MM=data05b$PIB_Nominal_MM*data05b$factor

	data05b[,2:5]=NULL

	data05b$PIB_Real_Bi=data05b$PIB_Real_MM/(10^6)

	dict_pib=data05b$PIB_Real_Bi

	pibb=data.frame(Periodo=seq(2008,2017),Monto=dict_pib,Grupo=rep("PIB",10),
		Monto_PIB=rep(1,10))
	pibb=pibb[-c(1),]


# Chart Educ

	pibpo_educ=subset(pibpo,Sector==levels(pibpo$Sector)[4])
	pibpo_educ$Monto=NULL
	pibpo_educ$Grupo=NULL
	names(pibpo_educ)=c("Grupo","Periodo","Monto_PIB")
	pibpo_educ=pibpo_educ[-c(2),]

	pibpo_salud=subset(pibpo,Sector=="Salud")
	pibpo_salud$Monto=NULL
	pibpo_salud$Grupo=NULL
	names(pibpo_salud)=c("Grupo","Periodo","Monto_PIB")

	pibpo_segsoc=subset(pibpo,Sector=="Seguridad Social")
	pibpo_segsoc$Monto=NULL
	pibpo_segsoc$Grupo=NULL
	names(pibpo_segsoc)=c("Grupo","Periodo","Monto_PIB")

	pibpo_agg=rbind(pibpo_agg,pibpo_educ,pibpo_salud,pibpo_segsoc)

	deuda=rbind(c(2010,"Deuda Publica",8.6),c(2014,"Deuda Publica",14.9),c(2017,"Deuda Publica",23.6))
	deuda=as.data.frame(deuda)
	names(deuda)=c("Periodo","Grupo","Monto_PIB")
	pibpo_agg=rbind(pibpo_agg,deuda)

	pibpo_agg$Monto_PIB=as.numeric(pibpo_agg$Monto_PIB)
	pibpo_agg$Periodo=as.numeric(pibpo_agg$Periodo)

#Chart
	chart_pib_percent=ggplot(data=pibpo_agg, aes(x=factor(Periodo), y=Monto_PIB,group=Grupo,colour=Grupo)) +
    geom_line(size=0.5) +
    geom_point() +
    xlab("Año")+ylab("Porcentaje del PIB del año")+
	  labs(title = "Ingresos y Gastos como porcentaje del PIB")+ylim(0,30)

	setwd("/datathon_gasto_fiscal/05_output/a_produccion/")

	ppib2 <- ggplotly(chart_pib_percent)
	saveWidget(ppib2, file="chart_pib_percent.html",selfcontained=FALSE) #guarda como html

	setwd("/datathon_gasto_fiscal/")
