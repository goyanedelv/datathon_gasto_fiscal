#Main versión 2.

setwd("/datathon_gasto_fiscal/")
	
	library(data.tree)
	library(networkD3)

# Clasificación de Egresos

	jerarq_egr_tes=read.csv("04_intermediate/jerarq_egr_tes.csv")
	names(jerarq_egr_tes)<-c("ITEM","Sector")

	jerarq_egr_tes$ITEM=as.character(jerarq_egr_tes$ITEM)

	jerarq_egr_tes$ITEM[3]="A Otras Entidades Publicas"
	jerarq_egr_tes$ITEM[6]="Amortizacion Deuda Externa"
	jerarq_egr_tes$ITEM[7]="Amortizacion Deuda Externa"
	jerarq_egr_tes$ITEM[8]="Compensaciones por Daños a Terceros y/o a la Propiedad "
	jerarq_egr_tes$ITEM[10]="Compra de Titulos y Valore"

	jerarq_egr_tes$ITEM=as.factor(jerarq_egr_tes$ITEM)

	jerarq_egr_tes$pathString=paste("Clasificación de Egresos",
		jerarq_egr_tes$Sector,jerarq_egr_tes$ITEM,sep="|")

	useRtree <- as.Node(jerarq_egr_tes, pathDelimiter = "|")
	useRtreeList <- ToListExplicit(useRtree, unname = TRUE)
	bleble=radialNetwork(useRtreeList,fontSize = 15)

	setwd("/datathon_gasto_fiscal/05_output/a_produccion")

	saveWidget(bleble, "Clasificacion_egresos.html", selfcontained = FALSE)
	setwd("/datathon_gasto_fiscal/")

# Clasificación de Ingresos

	clasif_ingresos=read.csv("04_intermediate/clasif_ingresos.csv")

	clasif_ingresos$Item=as.character(clasif_ingresos$Item)

	clasif_ingresos$Item[25]="Participacion de Utilidades"
	clasif_ingresos$Item[32]="Venta o Rescate de Titulos y Valores"

	clasif_ingresos$Item=as.factor(clasif_ingresos$Item)

	clasif_ingresos$pathString=paste("Clasificación de Ingresos",
		clasif_ingresos$Clasif,clasif_ingresos$Item,sep="|")

	useRtreeI <- as.Node(clasif_ingresos, pathDelimiter = "|")
	useRtreeListI <- ToListExplicit(useRtreeI, unname = TRUE)
	blabla=radialNetwork(useRtreeListI,fontSize = 15)

	setwd("/datathon_gasto_fiscal/05_output/a_produccion")

	saveWidget(blabla, "Clasificacion_igresos.html", selfcontained = FALSE)