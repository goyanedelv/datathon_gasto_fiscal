#Main versión 2.

library(ggplot2)
library(ggalt)
library(plotly)
library(htmlwidgets)

Sys.setenv("plotly_username"="goyanedelv")
Sys.setenv("plotly_api_key"="......")

# Benchmark prensa
#### En 2017 budget fue de $42.000.000.000.000 o $42 billones o $42.000.000 millones)
#### ó USD$60.000 millones
#### 1 billón: 1.000.000.000.000
#### http://www.t13.cl/noticia/negocios/las-cifras-ley-presupuesto-2017
#### $9.489.630 

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

# Llevando Data05 a Real2017
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

	pibpo=data.frame(Periodo=seq(2008,2017),PIB=dict_pib,PIB2017=rep("red",10))
	pibpo=pibpo[-c(1),]
	#pibpo$Periodo=as.factor(pibpo$Periodo)

	chart_pib=ggplot(data=pibpo, aes(x=factor(Periodo), y=PIB,group="PIB",colour=PIB2017)) +
    geom_line(size=0.5) +
    geom_point() +
    xlab("Año")+ylab("PIB (en billones de pesos)")+
	  labs(title = "Evolución del Producto Interno Bruto", 
	    subtitle = "En pesos 2017")

	##### dict_pib[XXXX-2007]

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

## Analisis de tesoreria (Ingresos), Distribución

	data02_ing_tes=subset(data02,data02$TIPO=="INGRESOS" & data02$Partida==levels(data02$Partida)[29])
	data02_ing_tes=droplevels(data02_ing_tes)

	data02_ing_tes$Ppto_inicial_Real=data02_ing_tes$Ppto_inicial_Real/10^6

	agg_ing <- aggregate(x = data02_ing_tes$Ppto_inicial_Real,
	  by = list(data02_ing_tes$ITEM,data02_ing_tes$Periodo), FUN = sum,na.rm=TRUE)
	colnames(agg_ing)<-c("Item","Periodo","Ingreso")
	agg_ing=droplevels(agg_ing)
	clasif_ingresos=read.csv("04_intermediate/clasif_ingresos.csv")
	key_clasif=data.frame(Item=levels(agg_ing$Item), Clasif=clasif_ingresos$Clasif)
	agg_ing=merge(agg_ing,key_clasif,by="Item")

	#More beautiful names for stylish charting:
	Año=as.factor(agg_ing$Periodo)

	chart05<-ggplot(agg_ing, aes(x=Año, y=Ingreso, fill=Clasif)) + 
	  geom_bar(stat="identity")+xlab("Año")+ylab("Ingresos (en billones de pesos)")+
	  labs(title = "Evolución de Ingresos por Item", 
	    subtitle = "Ingresos a Tesorería General de la Republica")+
	  scale_fill_discrete(name = "Clasificacion")

## Analisis de tesoreria (Ingresos), Crecimiento
	agg_ing2017=agg_ing[agg_ing$Periodo==2017,]
	agg17=aggregate(x=agg_ing2017$Ingreso,by=list(agg_ing2017$Clasif),FUN=sum,na.rm=TRUE)
	colnames(agg17)<-c("Clasificacion","Ingreso")

	agg_ing2009=agg_ing[agg_ing$Periodo==2009,]
	agg09=aggregate(x=agg_ing2009$Ingreso,by=list(agg_ing2009$Clasif),FUN=sum,na.rm=TRUE)
	colnames(agg09)<-c("Clasificacion","Ingreso")

	cagr_ing=-1+(agg17$Ingreso/agg09$Ingreso)^(1/(2017-2009))

	agg09=droplevels(agg09)
	resumen_ing=data.frame(Sector=levels(agg09$Clasificacion),
	  CAGR=round(cagr_ing*100,digits=1),i2009=agg09$Ingreso,i2017=agg17$Ingreso)

	theme_set(theme_bw())
	data_chart06<-resumen_ing#[resumen_ing$Sector!="Otros",]
	#data_chart06 <- data_chart06[order(-data_chart06$CAGR),]
	data_chart06$CAGR[8]=round(data_chart06$CAGR[8],digits=0)

	CAGR_y=data_chart06$CAGR
	CAGR_y[8]=30

	chart06<-ggplot(data_chart06, aes(x=Sector, y=CAGR_y, label=CAGR)) + 
	  geom_point(stat='identity', fill="plot(black", size=10)  +
	  geom_segment(aes(y = 0, 
	                   x = Sector, 
	                   yend = CAGR_y, 
	                   xend = Sector), 
	               color = "black") +
	  geom_text(color="white", size=4) +
	  labs(title="Crecimiento Anual Compuesto de Ingresos", 
	       subtitle="2009-2017") + ylab("Crecimiento Anual Compuesto (%)")+
	  ylim(-5, 32) +#geom_hline(yintercept = 25)+geom_hline(yintercept = 25.5)+
	  geom_segment(aes(x=7.7, y=24.5, xend=8.3, yend=25.5))+
	  geom_segment(aes(x=7.7, y=24.0, xend=8.3, yend=25.0))+
	  #annotate("text", x = 0.6, y = 30, label = "345")+
	  #scale_y_discrete(name ="Dose (mg)", limits=c("0","15","30","345"))+
	  coord_flip()

## Analisis de tesoreria (Egresos)

	data02_egr_tes=subset(data02,data02$TIPO=="GASTOS" & data02$Partida==levels(data02$Partida)[29])
	data02_egr_tes=droplevels(data02_egr_tes)
	agg_egr_tes <- aggregate(x = data02_egr_tes$Ppto_inicial_Real,
	  by = list(data02_egr_tes$ITEM,data02_egr_tes$Periodo), FUN = sum,na.rm=TRUE)
	names(agg_egr_tes)<- c("ITEM","Periodo","Monto")

	jerarq_egr_tes=read.csv("04_intermediate/jerarq_egr_tes.csv")

	agg_egr_tes2=merge(agg_egr_tes,jerarq_egr_tes,by="ITEM")

	agg_egr_tes3 <- aggregate(x = agg_egr_tes2$Monto,
	  by = list(agg_egr_tes2$Njerarq,agg_egr_tes2$Periodo), FUN = sum,na.rm=TRUE)
	names(agg_egr_tes3)<- c("Sector","Periodo","Monto")

	agg_egr_tes3$Monto=agg_egr_tes3$Monto/10^6

	chart01b<-ggplot(agg_egr_tes3, aes(x=as.factor(Periodo), y=Monto, fill=Sector)) + 
	  geom_bar(stat="identity")+xlab("Año")+ylab("Presupuesto (en billones de pesos)")+
	  labs(title = "Evolución del presupuesto por sector", 
	    subtitle = "")

		### Crecimiento del Presupuesto 2009-2017

	 	agg_egr_09=subset(agg_egr_tes3,Periodo==2009)
	  	agg_egr_17=subset(agg_egr_tes3,Periodo==2017)
	  	agg_egr_17=agg_egr_17[-c(8,10),]

	  	cagr_egr=-1+(agg_egr_17$Monto/agg_egr_09$Monto)^(1/(2017-2009))
		agg_egr_09=droplevels(agg_egr_09)
		resumen_egr=data.frame(Sector=levels(agg_egr_09$Sector),
		  CAGR=round(cagr_egr*100,digits=1),i2009=agg_egr_09$Monto,i2017=agg_egr_17$Monto)

		theme_set(theme_bw())
		data_chart03<-resumen_egr
		data_chart03$CAGR[7]=round(data_chart03$CAGR[7],digits=0)

		CAGR_y2=data_chart03$CAGR
		CAGR_y2[7]=30

		chart03<-ggplot(data_chart03, aes(x=Sector, y=CAGR_y2, label=CAGR)) + 
		  geom_point(stat='identity', fill="black", size=10)  +
		  geom_segment(aes(y = 0, 
		                   x = Sector, 
		                   yend = CAGR_y2, 
		                   xend = Sector), 
		               color = "black") +
		  geom_text(color="white", size=4) +
		  labs(title="Crecimiento Anual Compuesto de Gastos", 
		       subtitle="2009-2017") + ylab("Crecimiento Anual Compuesto (%)")+
		  ylim(-5, 32) +#geom_hline(yintercept = 25)+geom_hline(yintercept = 25.5)+
		  geom_segment(aes(x=6.7, y=24.5, xend=7.3, yend=25.5))+
		  geom_segment(aes(x=6.7, y=24.0, xend=7.3, yend=25.0))+
		  #annotate("text", x = 0.6, y = 30, label = "345")+
		  #scale_y_discrete(name ="Dose (mg)", limits=c("0","15","30","345"))+
		  coord_flip()	

## Gap Ejecución vs Presupuesto (solo 2017)

	# agg_sector_anho_ejec <- aggregate(x = data02_sector$Ejec_Acum_Real, by = list(data02_sector$Sector,data02_sector$Periodo), FUN = sum,na.rm=TRUE)
	# colnames(agg_sector_anho_ejec)<-c("Sector","Periodo","Gasto")

	# gsto_2017=agg_sector_anho_ejec[agg_sector_anho_ejec$Periodo==2017,]

	# data_chart04=data.frame(Sector=gsto_2017$Sector,Gasto=gsto_2017$Gasto,Presupuesto=ppto_2017$Presupuesto)

	# theme_set(theme_classic())

	# data_chart04$Sector <- factor(data_chart04$Sector, levels=as.character(data_chart04$Sector)) 

	# l=data_chart04$Gasto
	# r=data_chart04$Presupuesto

	# chart04 <- ggplot(data_chart04, aes(x=Gasto, xend=Presupuesto, y=Sector, group=Sector)) + 
	#      geom_point(data = data_chart04, aes(x = Gasto, color = "Gasto"), size = 2.0)+
	#      geom_point(data = data_chart04, aes(x = Presupuesto, color = "Presupuesto"), size = 1.75)+
	#         geom_dumbbell(aes(x = l, xend =r ),
	#         				colour_x="#a3c4dc",
	#                       size_xend=1.75,
	#                       size_x=2.0,
	#                       colour_xend="#0e668b") + 
	#         labs(x=NULL, 
	#              y=NULL, 
	#              title="Presupuesto versus Gasto", 
	#              subtitle="Ejercicio 2017", 
	#              caption="")+

	# scale_color_manual(name = "", values = c("#a3c4dc", "#0e668b"))

### Construyendo red Ingresos --> Tesoreria --> Sectores

	flujo=read.csv("04_intermediate/flujo_tesoreria.csv")

	flujo2017=subset(flujo,flujo$Periodo==2017)
	flujo2017$Periodo=NULL

	flujo2017$From[14]="Tesoreria"
	flujo2017$Monto[14]=-1*flujo2017$Monto[14]
	flujo2017$To=as.character(flujo2017$To)
	flujo2017$To[14]="Devolucion y Credito de Impuestos"
	flujo2017$To[11]="Otros "
	flujo2017$To=as.factor(flujo2017$To)

	Sankey <- gvisSankey(flujo2017, from="From", to="To", weight="Monto",
	                     options=list(
	                       sankey="{link: {color: { fill: '#1b4687' } },
	                            node: { color: { fill: '#a61d4c' },
	                            label: { color: '#000000' } }}"))
	plot(Sankey) #Manual Save

## Exportando I
	setwd("C:/Users/Gonzalo/Dropbox (JPAL LAC)/datathon_gasto_fiscal/05_output/a_produccion/")

	#chart01
	#chart01b #re-coded usando datos de tesorería (evito doble contabilidad)
	#ggsave("05_output/chart01.pdf")
	#dev.off()
	p1 <- ggplotly(chart01b)
	saveWidget(p1, file="chart01.html",selfcontained=FALSE) #guarda como html

	#chart02
	#ggsave("05_output/chart02.pdf")
	#dev.off()

	#chart03 #versión nueva usando data de tesorería
	#ggsave("05_output/chart03.pdf")
	#dev.off()
	p3 <- ggplotly(chart03)
	saveWidget(p3, file="chart03.html",selfcontained=FALSE) #guarda como html

	#chart04
	#ggsave("05_output/chart04.pdf")
	#dev.off()
	p4 <- ggplotly(chart04)
	saveWidget(p4, file="chart04.html",selfcontained=FALSE) #guarda como html

	#chart05
	#ggsave("05_output/chart05.pdf")
	#dev.off()
	p5 <- ggplotly(chart05)
	saveWidget(p5, file="chart05.html",selfcontained=FALSE) #guarda como html

	#chart06
	#ggsave("05_output/chart06.pdf")
	#dev.off()
	p6 <- ggplotly(chart06)
	saveWidget(p6, file="chart06.html",selfcontained=FALSE) #guarda como html

	ppib1 <- ggplotly(chart_pib)
	saveWidget(ppib1, file="chart_pib.html",selfcontained=FALSE) #guarda como html


	write.csv(agg_egr_tes3,"05_output/gasos_por_sector.csv")

	rm(data02)
	setwd("C:/Users/Gonzalo/Dropbox (JPAL LAC)/datathon_gasto_fiscal/")

# Análisis de Data01 || Event studies y Fixed effects?

	data01=lector(1)

# Data Tree de Data 02 Ejercicio 2017
	# 	library(treemap)
	# 	library(networkD3)
	# 	library(data.tree)

	# 	data02_17=subset(data02,Periodo==2017)
	# 	data02_17$Ppto_inicial_Real=NULL
	# 	data02_17$Ejec_Acum_Real=NULL
	# 	data02_17$ITEM=NULL
	# 	data02_17$ASIGNACI.U.0094.N=NULL
	# 	data02_17$TIPO=NULL

	# 	data02_17$pathString=paste("Presupuesto 2017",data02_17$Partida,
	# 		data02_17$Cap.U.0090.tulo,data02_17$Programa,sep="|")

	# 	write.csv(data02_17,"lala.csv")

	# 	data02_17=read.csv("lala.csv")

	# 	data02_17$ï..Periodo=NULL
	# 	data02_17=unique(data02_17)

	# 	useRtree <- as.Node(data02_17, pathDelimiter = "|")
	# 	useRtreeList <- ToListExplicit(useRtree, unname = TRUE)
	# 	radialNetwork( useRtreeList)

	# 	#### mas corta
	# 	data02_17$pathString=paste("Presupuesto 2017",data02_17$Partida,
	# 		data02_17$Capitulo,sep="|")

	# 	write.csv(data02_17,"lala.csv")

	# 	data02_17=read.csv("lala.csv")

	# 	data02_17$ï..Periodo=NULL
	# 	data02_17=unique(data02_17)

	# 	useRtree <- as.Node(data02_17, pathDelimiter = "|")
	# 	useRtreeList <- ToListExplicit(useRtree, unname = TRUE)
	# 	radialNetwork( useRtreeList)



## Education
	data01=lector(1)

	slice_and_stack<-function(base_data,sector_name){

	  data01=base_data

	  data01_edu=subset(data01,data01$Nombre.Partida==sector_name)
	  data01_edu=droplevels(data01_edu)

	  agg_mes=aggregate(x = data01_edu$Monto_sum, by = list(data01_edu$Periodo,data01_edu$Mes),
	   FUN = sum,na.rm=TRUE)
	  colnames(agg_mes)<-c("Anho","Mes","Monto")

	  from=c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio"
	  ,"Agosto","Septiembre", "Octubre","Noviembre","Diciembre")
	  to=c("01","02","03","04","05","06","07","08","09","10","11","12")

	  map=setNames(to,from)
	  vector=agg_mes$Mes
	  vector_ls=as.matrix(vector)
	  vector_ls[]=map[vector_ls]
	  agg_mes$MesN=vector_ls

	  agg_mes$Periodo=paste0(agg_mes$Anho,agg_mes$MesN,"01")
	  agg_mes$Periodo=as.Date(agg_mes$Periodo,format="%Y%m%d")

	  agg_mes=agg_mes[order(agg_mes$Periodo),]
	  agg_mes$Monto=agg_mes$Monto/10^6

	  return(agg_mes)
	}

	#"Ministerio De Educación"
	#"Ministerio De Salud"
	#"Ministerio Del Deporte"
	#"Ministerio De Vivienda Y Urbanismo"
	#"Ministerio Del Trabajo Y Previsión Social"
	#"Tesoro Público"
	#"Ministerio De Desarrollo Social"

	agg_edu=slice_and_stack(data01,"Ministerio De Educación")
	agg_sld=slice_and_stack(data01,"Ministerio De Salud")
	agg_mds=slice_and_stack(data01,"Ministerio De Desarrollo Social")
	agg_vvd=slice_and_stack(data01,"Ministerio De Vivienda Y Urbanismo")
	agg_mtp=slice_and_stack(data01,"Ministerio Del Trabajo Y Previsión Social")

	agg_sss=agg_mtp
	agg_sss$Monto=agg_mtp$Monto+agg_vvd$Monto+agg_mds$Monto

	agg_edu$Grupo="Educacion"
	agg_sld$Grupo="Salud"
	agg_sss$Grupo="Seguridad Social"

	sectores=c("Educación","Salud","Seguridad Social")
	colores=c("blue3","red3","green3")
	custom_date=agg_edu$Periodo[1]

	key_dates=c("20100311","20140311","20180311","20120101","20160101")
	key_dates=as.Date(key_dates,format="%Y%m%d")

	chart07<-ggplot()+geom_smooth(data=agg_edu,aes(x=Periodo,y=Monto,group=Grupo,colour=Grupo), 
	  fill="blue2",size=1,span=0.04,se = FALSE)+
	geom_smooth(data=agg_sld,aes(x=Periodo,y=Monto,group=Grupo,colour=Grupo), 
	  fill="red2",size=1,span=0.04,se = FALSE)+
	geom_smooth(data=agg_sss,aes(x=Periodo,y=Monto,group=Grupo,colour=Grupo), 
	  fill="green2",size=1,span=0.04,se = FALSE)+
	labs(x="Periodo",y="Gasto (en billones de pesos)",
	  title="Evolución del Gasto",subtitle="Educación, Salud y Seguridad Social. 2009-2017",
	  caption="Team: soa_bachelet")+

	geom_segment(aes(x=key_dates[1], y=0, xend=key_dates[1], yend = 1300),linetype="dashed",  color="black")+
	geom_segment(aes(x=key_dates[2], y=0, xend=key_dates[2], yend = 1300),linetype="dashed",  color="black")+

	annotate("text", x=key_dates[4], y=1300, label= "Piñera (2010-2014)",color="black",size=4)+
	annotate("text", x=key_dates[5], y=1300, label= "Bachelet (2014-2018)",color="black",size=4)#+

	#geom_point(aes(x=custom_date,y=300, color="blue2"), show.legend=TRUE)+
	#geom_point(aes(x=custom_date,y=300, color="red2"), show.legend=TRUE)+
	#geom_point(aes(x=custom_date,y=300, color="green2"), show.legend=TRUE)+
	#scale_color_manual("", values=colores,labels=sectores)+
	#theme(legend.position = "top")

## Exportando II
	setwd("C:/Users/Gonzalo/Dropbox (JPAL LAC)/datathon_gasto_fiscal/05_output/a_produccion/")
	#chart07
	#ggsave("05_output/chart07.pdf")
	#dev.off()
	p7 <- ggplotly(chart07)
	saveWidget(p7, file="chart07.html",selfcontained=FALSE) #guarda como html
	setwd("C:/Users/Gonzalo/Dropbox (JPAL LAC)/datathon_gasto_fiscal/")


	rm(data01)
