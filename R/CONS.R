#' @title Consonance Analysis Module
#'
#' @author Nery Sofia Huerta-Pacheco, Victor Manuel Aguirre-Torres, Teresa Lopez-Alvarez
#'
#' @description Consonance Analysis is a useful numerical and graphical approach for evaluating the consistency of the measurements and the panel of people involved in sensory evaluation. It makes use of several uni and multivariate techniques either graphical or analytical. It shows the implementation of this procedure in a graphical interface.
#'
#' You can learn more about this package at:
#' http://www.uv.mx/personal/nehuerta/cons/
#'
#' @details
#'
#' CONS is a package with a graphical interface that performs Consonance Analysis. This procedure is an
#' approach useful for evaluating the consistency of the measurements performed by a panel of people
#' involved in sensory evaluation. It makes use of several uni and multivariate techniques either graphical
#' or analytical. The procedure helps evaluate the linearirty of an attribute with respect to the panel
#' performance. If there is evidence that an attribute is not linear then this information could be used to
#' improve the evaluation of such characteristic. The method also helps detect whether some members of
#' the panel deviate from the rest. A critical tool for this procedure is Principal Components Analysis.
#' Note: CONS is free software and comes with ABSOLUTELY NO WARRANTY.

#' @return CONS is a graphic interface
#' @examples \dontrun{
#' ##Install package
#' library(CONS)
#' ##Call the package
#' CONS()
#' }
#'
#' @references
#' Arnold GM, Williams AA (1985). "The Use of Generalised Procrusters Analysis in
#' Sensory Analysis". In JR Piggot (ed.) Statistical Procedures in Food Research.
#' North Holland, Amsterdam.
#'
#' Banfield CF, Harries JM (1975). " A Techniques for Comparing Judges' Performance
#' in Sensory Tests". Journal of Food Technology, 10, 1-10.
#'
#' Dijksterhuis GB, Gower JC (1991/92). "The Interpretation of Generalised Procrusters
#' Analysis and Allied Methods". Food Quality and Preference, 3, 67-87.
#'
#' Dijksterhuis GB (1995). "Assessing Panel Consonance". Food Quality and Preference,
#' 6(1):714. http://dx.doi.org/10.1016/0950-3293(94)P4207-M
#'
#' Dijksterhuis GB (2004). "Assessing Panel Consonance". Multivariate Data Analysis in
#' Sensory and Consumer Science. ISBN: 978-0-917678-41-7. DOI: 10.1002/9780470385050.ch2
#'
#' Gower JC (1975). "Generalised Procrusters Analysis." Psychometrika, 40, 33-51.
#'
#' Sebastien Le, Julie Josse, Francois Husson (2008). FactoMineR: An R Package for
#' Multivariate Analysis. Journal of Statistical Software, 25(1), 1-18.
#' 10.18637/jss.v025.i01
#'
#' van der Burg E (1988). "Nonlinear Canonical Correlation and Some Related Techniques."
#' DSWO press, Leiden.
#'
#' van der Burg E, Dijksterhuis GB (1989). "Nonlinear Canonical Correlation Analysis
#' of Multiway Data". In R Coppi, S Bolasco (eds.) Multiway Data Analysis, 245-255.
#' North-Holland, Elsevier Science Publishers B.V.
#'
#' @export CONS
#' @import graphics
#' @import grDevices
#' @import utils
#' @import tcltk
#' @import gWidgets
#' @import readxl
#' @import REdaS
#' @import FactoMineR
#' @import pander
#' @import gridExtra
#' @importFrom raster cv
#' @importFrom stats cor quantile sd var
#'
CONS<-function(){

  mi<- new.env()

  ##Library
  options("guiToolkit"="tcltk")

  ##Screen
  w<- gwindow("CONS",visible=FALSE,width = 800,height= 510)
  g<- ggroup(horizontal=FALSE, spacing=0, container = w)

  nb <- gnotebook(container=g,width = 800,height= 500)
  g1<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "CONS")
  g2<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Descriptive general")
  g3<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Descriptive per matrix")
  g4<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Consonance")

  #GLOBAL VARIABLES
  assign("gdata",NULL, envir =mi)
  assign("gdata1",NULL, envir =mi)
  assign("n",NULL, envir =mi)
  assign("m",NULL, envir =mi)
  assign("a",NULL, envir =mi)
  assign("na",NULL, envir =mi)
  assign("np",NULL, envir =mi)
  assign("nr",NULL, envir =mi)
  assign("naa",NULL, envir =mi)
  assign("npp",NULL, envir =mi)
  assign("nrr",NULL, envir =mi)
  assign("x",NULL, envir =mi)
  assign("namesmat",NULL, envir =mi)
  assign("names2",NULL,envir =mi)
  assign("anames",NULL,envir =mi)
  assign("V1",NULL, envir =mi)
  assign("V2",NULL, envir =mi)
  assign("V3",NULL, envir =mi)
  assign("V4",NULL, envir =mi)
  assign("n2",NULL, envir =mi)
  assign("m1",NULL, envir =mi)
  assign("gdata2",NULL, envir =mi)
  assign("result1",NULL,envir =mi)
  assign("namesmat",NULL,envir =mi)
  assign("namesmat1",NULL,envir =mi)
  assign("valnames",NULL, envir =mi)
  assign("MVAF2",NULL, envir =mi)
  assign("MCON1",NULL, envir =mi)
  assign("MCON2",NULL, envir =mi)

  ##MENU - OPEN
  #Open csv
  abrirc<-function(h,...){
    data<-tk_choose.files()
    data1<-read.csv(data)
    assign("gdata",data1, envir =mi)
  }

  #Open txt
  abrirt<-function(h,...){
    data<-tk_choose.files()
    data1<-read.table(data,header=TRUE)
    assign("gdata",data1, envir =mi)
  }

  #Open xlsx
  openex<-function(h,...){
    data<-tk_choose.files()
    xlsx<-read_excel(data,sheet = 1, col_names=TRUE)
    data2<-as.data.frame(xlsx)
    assign("gdata",data2, envir =mi)
  }

  ##View
  ver<-function(h,...){
    gdata<-get("gdata",envir =mi)
    fix(gdata)
  }

  ##Re-start
  inicio<-function(h,...){
    dispose(w)
    CONS()
  }

  ##Parameters
  parm<-function(h,...){
    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 350)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =mi)
    var<- colnames(gdata)
    var1<-c("Null",var)
    tbl[1,1] <- "Assessor"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))

    tbl[2,1] <- "Product"
    tbl[2,2] <- (cb2 <- gcombobox(var, container=tbl))

    tbl[3,1] <- "Replicate"
    tbl[3,2] <- (cb3 <- gcombobox(var1, container=tbl))

    tbl[5,3] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(b, handler=function(h,...) {
      na1<-svalue(cb1)
      np1<-svalue(cb2)
      nr1<-svalue(cb3)
      assign("na",na1, envir =mi)
      assign("np",np1, envir =mi)
      assign("nr",nr1, envir =mi)

      na<-get("na",envir =mi)
      np<-get("np",envir =mi)
      nr<-get("nr",envir =mi)
      n1<-ncol(gdata)

      vta<-na
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }
      assign("a",a, envir =mi)
      vtp<-np
      for(v in 1:n1){
        if(vtp==colnames(gdata[v])){
          p<-gdata[,v]
          c2<-v
        }
      }
      if(nr=="Null"){
        nr<-1
      }else{nr<-nr}

      if(nr>1){
        vtr<-nr
        for(v in 1:n1){
          if(vtr==colnames(gdata[v])){
            r<-gdata[,v]
            c3<-v
          }
        }
      }
      # Count factor a
      ta<-table(a)
      naa<-nrow(as.matrix(ta))
      anames<-rownames(as.matrix(ta))
      # Count factor p
      tp<-table(p)
      npp<-nrow(as.matrix(tp))
      # Count factor r
      if(nr>1){
        tr<-table(r)
        nrr<-nrow(as.matrix(tr))
      }else{nrr<-1}

      assign("naa",naa, envir =mi)
      assign("npp",npp, envir =mi)
      assign("nrr",nrr, envir =mi)
      assign("anames",anames, envir =mi)

      #For variable
      drops<-c(na,np,nr)
      varib<-gdata[ , !(names(gdata) %in% drops)]
      m<-nrow(varib)
      n<-ncol(varib)

      assign("n",n, envir =mi)
      assign("m",m, envir =mi)
      assign("gdata1",varib, envir =mi)
      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  ##Close
  cerrar<-function(h,...){
    dispose(w)
  }

  save<-function(h,...){
    MVAF2<-get("MVAF2", envir =mi)
    MCON1<-get("MCON1", envir =mi)
    MCON2<-get("MCON2", envir =mi)
    valnames<-get("valnames", envir =mi)

    filename <- tclvalue(tkgetSaveFile())
    nam<-paste0(filename,".pdf")
    df <- round(t(MVAF2),digits=2)
    d<-round(MCON2, digits=2)
    pdf(file=nam)
    mytheme <- gridExtra::ttheme_default(
      core = list(fg_params=list(cex = 0.6)),
      colhead = list(fg_params=list(cex = 0.6)),
      rowhead = list(fg_params=list(cex = 0.6)))

    g1 <- gridExtra::tableGrob(df, theme = mytheme)
    g2 <- gridExtra::tableGrob(d, theme = mytheme)
    haligned <- combine(g1,g2, along=1)
    grid.arrange(haligned, ncol=1)
    #grid.arrange(g1,g2, ncol=2)
    nma<-ncol(MVAF2)
    color<-c("#FFC300","darkorange","#FF5733","#C70039","#900C3F","#E91E63","#9C27B0","#673AB7","darkorchid4","darkblue","#3F51B5","#03A9F4","#00BCD4","#009688","darkgreen","#4CAF50","#CDDC39","gray","black","#FF9999","#99FF99","#99CCFF","#9999FF","#CC99FF","#FF99CC")
    matplot(MVAF2,type =c("o"),ylim=c(0,100),pch=20,col=color,xaxt="n",ylab="",cex.axis=0.45,cex.lab=0.45,cex.names=0.45,main=paste("Proportion VAF per dimension for the",nma,"PCAs"))
    axis(1,1:nrow(MVAF2),rownames(MVAF2),cex.axis=0.45)
    legend("topright",legend=paste("At",c(1:ncol(MVAF2)),"-",valnames),pch=16,col=color,cex=0.5)
    maxco<-max(MCON1)+10
    barplot(MCON1,ylim=c(0,maxco),col=color,cex.axis=0.45,cex.lab=0.45,cex.names=0.45,main="Consonance")
    dev.off()
  }

  ##Factor loading
  loadingAV<-function(h,...){
    w1<- gwindow("Loading plots",visible=FALSE,width = 400,height= 350)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =mi)
    gdata1<-get("gdata1",envir =mi)
    npp<-get("npp",envir =mi)
    nrr<-get("nrr",envir =mi)
    namesmat<-get("namesmat",envir =mi)
    var<- colnames(gdata1)

    tbl[1,1]<- "Choose the Information"

    tbl[2,1] <- "Plot 1"
    tbl[2,2] <- (cb1 <- gcombobox(var, container=tbl))

    tbl[3,1] <- "Plot 2"
    tbl[3,2] <- (cb2 <- gcombobox(var, container=tbl))

    tbl[4,1] <- "Plot 3"
    tbl[4,2] <- (cb3 <- gcombobox(var, container=tbl))

    tbl[5,1] <- "Plot 4"
    tbl[5,2] <- (cb4 <- gcombobox(var, container=tbl))

    tbl[6,3] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(b, handler=function(h,...) {
      V1<-svalue(cb1)
      V2<-svalue(cb2)
      V3<-svalue(cb3)
      V4<-svalue(cb4)

      assign("V1",V1, envir =mi)
      assign("V2",V2, envir =mi)
      assign("V3",V3, envir =mi)
      assign("V4",V4, envir =mi)

      n1<-ncol(gdata1)

      vt1<-V1
      for(v in 1:n1){
        if(vt1==colnames(gdata1[v])){
          v1<-v
        }
      }

      vt2<-V2
      for(v in 1:n1){
        if(vt2==colnames(gdata1[v])){
          v2<-v
        }
      }

      vt3<-V3
      for(v in 1:n1){
        if(vt3==colnames(gdata1[v])){
          v3<-v
        }
      }

      vt4<-V4
      for(v in 1:n1){
        if(vt4==colnames(gdata1[v])){
          v4<-v
        }
      }

      #PC --------------------------------------------------------------------------------------
      PCA <- function (X,TI,scale.unit = TRUE, ncp = 5, ind.sup = NULL, quanti.sup = NULL,
                       quali.sup = n, row.w = NULL, col.w = NULL, graph = TRUE,
                       axes = c(1, 2))
      {
        moy.ptab <- function(V, poids) {
          as.vector(crossprod(poids/sum(poids),as.matrix(V)))
        }

        ec.tab <- function(V, poids) {
          ecart.type <- sqrt(as.vector(crossprod(poids/sum(poids),as.matrix(V^2))))
          ecart.type[ecart.type <= 1e-16] <- 1
          return(ecart.type)
        }
        fct.eta2 <- function(vec,x,weights) {
          VB <- function(xx) {
            return(sum((colSums((tt*xx)*weights)^2)/ni))
          }
          tt <- tab.disjonctif(vec)
          ni <- colSums(tt*weights)
          unlist(lapply(as.data.frame(x),VB))/colSums(x^2*weights)
        }

        X <- as.data.frame(X)
        ng<-ncol(X)
        nr<-nrow(X)
        ta<-table(X[,ng])
        tp<-nrow(ta)
        tr<-nr/tp

        X <- droplevels(X)
        if (any(is.na(X))) {
          warning("Missing values are imputed by the mean of the variable: you should use the imputePCA function of the missMDA package")
          if (is.null(quali.sup))
            X[is.na(X)] = matrix(colMeans(X,na.rm=TRUE),ncol=ncol(X),nrow=nrow(X),byrow=TRUE)[is.na(X)]
          else for (j in (1:ncol(X))[-quali.sup]) X[, j] <- replace(X[, j], is.na(X[, j]), mean(X[, j], na.rm = TRUE))
        }
        Xtot <- X
        if (!is.null(quali.sup))
          X <- X[, -quali.sup,drop=FALSE]
        auxi <- colnames(X)[!sapply(X, is.numeric)]
        if (length(auxi)>0)  stop(paste("\nThe following variables are not quantitative: ", auxi))
        todelete <- c(quali.sup, quanti.sup)
        if (!is.null(todelete)) X <- Xtot[, -todelete,drop=FALSE]
        if (!is.null(ind.sup)) {
          X.ind.sup <- X[ind.sup, , drop = FALSE]
          X <- X[-ind.sup, , drop = FALSE]
        }
        ncp <- min(ncp, nrow(X) - 1, ncol(X))
        if (is.null(row.w)) row.w <- rep(1, nrow(X))
        row.w.init <- row.w
        row.w <- row.w/sum(row.w)
        if (is.null(col.w)) col.w <- rep(1, ncol(X))
        centre <- moy.ptab(X,row.w)
        data <- X
        X <- t(t(as.matrix(X))-centre)
        if (is.null(attributes(X)$row.names)) rownames(X) <- rownames(data)
        if (is.null(attributes(X)$names)) colnames(X) <- colnames(data)
        if (scale.unit) {
          ecart.type <- ec.tab(X,row.w)
          X <- t(t(X)/ecart.type)
        }
        else ecart.type <- rep(1, length(centre))
        dist2.ind <- rowSums(t(t(X^2)*col.w))
        dist2.var <- as.vector(crossprod(rep(1,nrow(X)),as.matrix(X^2*row.w)))
        res.call <- list(row.w = (row.w/sum(row.w)), col.w = col.w,
                         scale.unit = scale.unit, ncp = ncp, centre = centre,
                         ecart.type = ecart.type, X = Xtot, row.w.init = row.w.init,call=match.call())
        tmp <- svd.triplet(X, row.w = row.w, col.w = col.w,ncp=ncp)
        eig <- tmp$vs^2
        vp <- as.data.frame(matrix(NA, length(eig), 3))
        rownames(vp) <- paste("F", 1:length(eig))
        colnames(vp) <- c("Eigenvalue","Variability (%)","Cumulative %")
        vp[, "Eigenvalue"] <- eig
        vp[, "Variability (%)"] <- (eig/sum(eig)) * 100
        vp[, "Cumulative %"] <- cumsum(vp[, "Variability (%)"])
        V <- tmp$V
        U <- tmp$U
        eig <- eig[1:ncp]
        coord.ind <- t(t(as.matrix(U))*sqrt(eig))
        coord.var <- t(t(as.matrix(V))*sqrt(eig))
        contrib.var <- t(t(coord.var^2)/eig)*col.w
        dist2 <- dist2.var
        cor.var <- coord.var/sqrt(dist2)
        cos2.var <- cor.var^2
        rownames(coord.var) <- rownames(cos2.var) <- rownames(cor.var) <- rownames(contrib.var) <- colnames(X)
        colnames(coord.var) <- colnames(cos2.var) <- colnames(cor.var) <- colnames(contrib.var) <- paste("Dim",
                                                                                                         c(1:ncol(V)), sep = ".")
        res.var <- list(coord = coord.var[, 1:ncp], cor = cor.var[,
                                                                  1:ncp], cos2 = cos2.var[, 1:ncp], contrib = contrib.var[,
                                                                                                                          1:ncp] * 100)
        dist2 <- dist2.ind
        cos2.ind <- coord.ind^2/dist2
        contrib.ind <- t(t(coord.ind^2*row.w/sum(row.w))/eig)
        rownames(coord.ind) <- rownames(cos2.ind) <- rownames(contrib.ind) <- names(dist2) <- rownames(X)
        colnames(coord.ind) <- colnames(cos2.ind) <- colnames(contrib.ind) <- paste("Dim",
                                                                                    c(1:ncol(U)), sep = ".")
        res.ind <- list(coord = coord.ind[, 1:ncp,drop=FALSE], cos2 = cos2.ind[,
                                                                               1:ncp,drop=FALSE], contrib = contrib.ind[, 1:ncp,drop=FALSE] * 100, dist = sqrt(dist2))
        res <- list(eig = vp, var = res.var, ind = res.ind, svd = tmp)
        if (!is.null(ind.sup)) {
          if (is.null(ecart.type)) ecart.type <- rep(1, length(centre))
          X.ind.sup <- t(t(as.matrix(X.ind.sup))-centre)
          X.ind.sup <- t(t(X.ind.sup)/ecart.type)
          coord.ind.sup <- t(t(X.ind.sup)*col.w)
          coord.ind.sup <- crossprod(t(coord.ind.sup),tmp$V)
          dist2 <- rowSums(t(t(X.ind.sup^2)*col.w))
          cos2.ind.sup <- coord.ind.sup^2/dist2
          coord.ind.sup <- coord.ind.sup[, 1:ncp, drop = F]
          cos2.ind.sup <- cos2.ind.sup[, 1:ncp, drop = F]
          colnames(coord.ind.sup) <- colnames(cos2.ind.sup) <- paste("Dim",  c(1:ncp), sep = ".")
          rownames(coord.ind.sup) <- rownames(cos2.ind.sup) <- names(dist2) <- rownames(X.ind.sup)
          res.ind.sup <- list(coord = coord.ind.sup, cos2 = cos2.ind.sup, dist = sqrt(dist2))
          res$ind.sup = res.ind.sup
          res.call$ind.sup = ind.sup
        }
        if (!is.null(quanti.sup)) {
          X.quanti.sup <- as.data.frame(Xtot[, quanti.sup,drop=FALSE])
          if (!is.null(ind.sup)) X.quanti.sup <- as.data.frame(X.quanti.sup[-ind.sup, ,drop=FALSE])
          colnames(X.quanti.sup) <- colnames(Xtot)[quanti.sup]
          res.call$quanti.sup = X.quanti.sup
          centre.sup <- moy.ptab(X.quanti.sup,row.w)
          X.quanti.sup <- t(t(as.matrix(X.quanti.sup))-centre.sup)
          if (scale.unit) {
            ecart.type.sup <- ec.tab(X.quanti.sup, row.w)
            X.quanti.sup <- t(t(X.quanti.sup)/ecart.type.sup)
          }
          coord.vcs <- t(X.quanti.sup*row.w)
          coord.vcs <- crossprod(t(coord.vcs),tmp$U)
          col.w.vcs <- rep(1, ncol(coord.vcs))
          cor.vcs <- matrix(NA, ncol(X.quanti.sup), ncol(tmp$U))
          dist2 <- as.vector(crossprod(rep(1,nrow(X.quanti.sup)),as.matrix(X.quanti.sup^2*row.w)))
          cor.vcs <- coord.vcs/sqrt(dist2)
          cos2.vcs <- cor.vcs^2
          colnames(coord.vcs) <- colnames(cor.vcs) <- colnames(cos2.vcs) <- paste("Dim", c(1:ncol(cor.vcs)), sep = ".")
          rownames(coord.vcs) <- rownames(cor.vcs) <- rownames(cos2.vcs) <- colnames(Xtot)[quanti.sup]
          res.quanti.sup <- list(coord = coord.vcs[, 1:ncp, drop=FALSE], cor = cor.vcs[, 1:ncp, drop=FALSE], cos2 = cos2.vcs[, 1:ncp, drop=FALSE])
          res$quanti.sup = res.quanti.sup
        }
        if (!is.null(quali.sup)) {
          X.quali.sup <- as.data.frame(Xtot[, quali.sup,drop=FALSE])
          if (!is.null(ind.sup)) X.quali.sup <- as.data.frame(X.quali.sup[-ind.sup,,drop=FALSE])
          colnames(X.quali.sup) <- colnames(Xtot)[quali.sup]
          nombre <- modalite <- NULL

          if (ncp>1) eta2 <- t(sapply(X.quali.sup,fct.eta2,res$ind$coord,weights=row.w))
          else {
            eta2 <- as.matrix(sapply(X.quali.sup,fct.eta2,res$ind$coord,weights=row.w),ncol=ncp)
            colnames(eta2) = paste("Dim", 1:ncp)
            rownames(eta2) = colnames(X.quali.sup)
          }

          for (i in 1:ncol(X.quali.sup)) {
            var <- as.factor(X.quali.sup[, i])
            n.mod <- nlevels(var)
            modalite <- c(modalite, n.mod)
            bary <- matrix(NA, n.mod, ncol(X))
            for (j in 1:n.mod) {
              ind <- levels(var)[j]
              bary[j, ] <- moy.ptab(data[which(var == ind), ], row.w[which(var == ind)])

              nombre <- c(nombre, sum(row.w.init[which(var == ind)]))
            }
            colnames(bary) <- colnames(X)
            if ((levels(var)[1] %in% (1:nrow(X))) | (levels(var)[1] %in% c("y", "Y", "n", "N"))) row.names(bary) <- paste(colnames(X.quali.sup)[i], as.character(levels(var)))
            else row.names(bary) <- as.character(levels(var))
            if (i == 1)  barycentre <- bary
            else barycentre <- rbind(barycentre, bary)
          }
          bary <- t(t(barycentre)-centre)
          if (!is.null(ecart.type)) bary <- t(t(bary)/ecart.type)
          dist2 <- rowSums(t(t(bary^2)*col.w))
          coord.barycentre <- t(t(bary)*col.w)
          coord.barycentre <- crossprod(t(coord.barycentre),tmp$V)
          colnames(coord.barycentre) <- paste("Dim", 1:ncol(coord.barycentre), sep = ".")
          cos2.bary.sup <- coord.barycentre^2/dist2
          vtest <- t(t(coord.barycentre)/sqrt(eig))
          if (sum(row.w.init)>1) vtest <- vtest*sqrt(nombre/((sum(row.w.init) - nombre)/(sum(row.w.init) - 1)))
          else vtest <- vtest*sqrt(nombre)
          cos2.bary.sup <- cos2.bary.sup[, 1:ncp, drop=FALSE]
          coord.barycentre <- coord.barycentre[, 1:ncp, drop=FALSE]
          vtest <- vtest[, 1:ncp, drop=FALSE]
          dimnames(cos2.bary.sup) <- dimnames(vtest) <- dimnames(coord.barycentre)
          names(dist2) <- rownames(coord.barycentre)
          res.quali.sup <- list(coord = coord.barycentre, cos2 = cos2.bary.sup, v.test = vtest, dist = sqrt(dist2), eta2=eta2)
          call.quali.sup <- list(quali.sup = X.quali.sup, modalite = modalite, nombre = nombre, barycentre = as.data.frame(barycentre), numero = quali.sup)
          res$quali.sup = res.quali.sup
          res.call$quali.sup = call.quali.sup
        }
        res$call = res.call
        class(res) <- c("PCA", "list ")
        if (graph & (ncp>1)) {
          dev.new()
          par(mfrow=c(1,2))
          color<-c("#FFC300","darkorange","#FF5733","#C70039","#900C3F","#E91E63","#9C27B0","#673AB7","darkorchid4","darkblue","#3F51B5","#03A9F4","#00BCD4","#009688","darkgreen","#4CAF50","#CDDC39","gray","black")
          plot.PCA(res, choix = "var",ylim=c(-1.5,1.5),xlim=c(-1.5,1.5),title=paste("Attribute:",TI),yaxt="n",xaxt="n",cex=0.5,cex.lab=0.6)
          axis(1,c(-1.5:1.5),cex.axis=0.6)
          axis(2,c(-1.5:1.5),cex.axis=0.6)
          plot.PCA(res, choix = "ind", ylim=c(-5,5),xlim=c(-5,5),title="",yaxt="n",xaxt="n",cex=0.6,cex.lab=0.6,habillage=n,palette=palette(c("darkorange","#FF5733","#C70039","#900C3F","#E91E63","#9C27B0","#673AB7","darkorchid4","darkblue","#3F51B5","#03A9F4","#00BCD4","#009688","darkgreen","#4CAF50","#CDDC39","gray","black","#FFC300")))
          z<-res$ind$coord
          r=tr
          inc=r
          si=1
          for(i in 1:tp){
            lines(z[c(si:inc),1],z[c(si:inc),2],type="l",col=color[i])
            si<-si+r
            inc<-inc+r
          }
          axis(1,c(-5:5),cex.axis=0.6)
          axis(2,c(-5:5),cex.axis=0.6)
        }
      }
      #-----------------------------------------------------------------------------------------
      #PC1
      end<-npp*nrr
      cat<-as.character(gdata[c(1:end),2])
      x1<-read.table(namesmat[,v1])
      x11<-cbind(x1,cat)
      n<-ncol(x11)
      res1<-PCA(x11,V1,n)

      #PC2
      end<-npp*nrr
      x2<-read.table(namesmat[,v2])
      x21<-cbind(x2,cat)
      n<-ncol(x21)
      res2<-PCA(x21,V2,n)

      #PC3
      end<-npp*nrr
      x3<-read.table(namesmat[,v3])
      x31<-cbind(x3,cat)
      n<-ncol(x31)
      res3<-PCA(x31,V3,n)

      #PC4
      end<-npp*nrr
      x4<-read.table(namesmat[,v4])
      x41<-cbind(x4,cat)
      n<-ncol(x41)
      res4<-PCA(x41,V4,n)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  ##MENU - METHOD
  # 0-------------------------------------------------------------------------------------------
  #Descriptive general
  cd<-function(h,...){
    gdata<-get("gdata",envir =mi)
    n<-get("n",envir =mi)
    m<-get("m",envir =mi)
    gdata1<-get("gdata1",envir =mi)
    # General
    DG<-function(gdata1){
      varib<-gdata1
      min<-min(varib)
      max<-max(varib)
      mean<-sum(t(sapply(varib, mean, na.rm=TRUE))/(n))

      #Case
      nr<-matrix(,1,n)
      for(i in 1:n){
        nr[,i]<-nrow(varib[i])
      }
      d1<-nr
      d2<-t(sapply(varib, min, na.rm=TRUE))
      d3<-t(sapply(varib, max, na.rm=TRUE))
      d4<-(sapply(varib,quantile,prob = c(0.25,0.5,0.75), na.rm=TRUE))
      d5<-(sapply(varib, mean, na.rm=TRUE))
      d5t<-t(d5)
      d6<-t(sapply(varib, var, na.rm=TRUE))
      d7<-t(sapply(varib, sd, na.rm=TRUE))
      d8<-t(sapply(varib, cv, na.rm=TRUE))

      mat<-t(round(rbind(d1,d2,d3,d4,d5t,d6,d7,d8),digits = 3))
      colnames(mat)<-c("N. of observations","Minimum","Maximum","1st Quartile","Median","3rd Quartile","Mean","Variance (n-1)","Standard deviation (n-1)","Variation coefficient")
      pandoc.table(mat,plain.ascii = TRUE)
      assign("n",n, envir =mi)
      assign("m",m, envir =mi)
      assign("gdata1",varib, envir =mi)

      #Graph
      dev.new()
      boxplot(varib,data=varib,col="lightgray",ylim=c(min-1,max+1),cex.axis=0.5,cex.lab=0.5)
      points(d5,col="red",pch=18)
      abline(h=mean,col="red")
    }

    ##g2
    tbl<-glayout(container=g2)
    gseparator(horizontal=TRUE, container=g2)
    outputArea <- gtext(container=g2, expand=TRUE,width = 780,height= 480)
    out <- capture.output(DG(gdata1))
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea, out)
  }
  # 1 -------------------------------------------------------------------------------------------
  #Assessors Times Variable
  ATVD<-function(h,...){
    n<-get("n",envir =mi)
    npp<-get("npp",envir =mi)
    nrr<-get("nrr",envir =mi)
    naa<-get("naa",envir =mi)
    anames<-get("anames",envir =mi)
    gdata1<-get("gdata1",envir =mi)

    DMAV<-function(n,npp,nrr,naa,gdata1){
      varib<-gdata1
      n<-n
      nn=naa
      mm=npp*nrr
      namesmat<-matrix(,1,n)
      for(i in 1:n){
        namesmat[,i]<-paste0("Mat", i, ".txt")
      }

      #Matrix act for Variable Times Assesors
      res=n-1
      x<-matrix(,mm,nn)
      while(res>=0){
        k<-n-res
        cont=0
        nmat<-matrix(varib[,k])
        for(i in 1:nn){
          for(j in 1:mm){
            x[j,i]<-nmat[j+cont,]
          }
          cont=cont+mm
        }
        colnames(x)<-paste0("J-",anames,".","A",k)
        write.table(x,namesmat[,n-res])
        res<-res-1
      }

      for(i in 1:n) {
        mat <- paste("Mat.", i, sep = "")
        assign(mat, read.table(namesmat[,i]))
      }

      names2<-ls(pattern = "Mat..$")
      for(i in 1:n) {
        x<-read.table(namesmat[,i])
        print(paste("Data:",names2[i]),quote=F)
        pandoc.table(x,plain.ascii = TRUE)
        varib<-x
        m<-nrow(varib)
        n<-ncol(varib)
        min<-min(varib)
        max<-max(varib)
        mean<-sum(t(sapply(varib, mean, na.rm=TRUE))/(n))
        #Case
        nr<-matrix(,1,n)
        for(i in 1:n){
          nr[,i]<-nrow(varib[i])
        }

        d1<-nr
        d2<-t(sapply(varib, min, na.rm=TRUE))
        d3<-t(sapply(varib, max, na.rm=TRUE))
        d4<-(sapply(varib,quantile,prob = c(0.25,0.5,0.75), na.rm=TRUE))
        d5<-(sapply(varib, mean, na.rm=TRUE))
        d5t<-t(d5)
        d6<-t(sapply(varib, var, na.rm=TRUE))
        d7<-t(sapply(varib, sd, na.rm=TRUE))
        d8<-t(sapply(varib, cv, na.rm=TRUE))

        mat<-t(round(rbind(d1,d2,d3,d4,d5t,d6,d7,d8),digits = 3))
        colnames(mat)<-c("N. of observations","Minimum","Maximum","1st Quartile","Median","3rd Quartile","Mean","Variance (n-1)","Standard deviation (n-1)","Variation coefficient")
        mat[is.na(mat)] <- "0"
        print("Summary statistics",quote=F)
        pandoc.table(mat,plain.ascii = TRUE)
        #Correlations
        print("Correlation matrix (Pearson (n-1))",quote=F)
        COR<-round(cor(x),digits = 3)
        pandoc.table(COR,plain.ascii = TRUE)
      }
    }
    ##g3
    tbl<-glayout(container=g3)
    gseparator(horizontal=TRUE, container=g3)
    outputArea <- gtext(container=g3, expand=TRUE,width = 780,height= 200)
    gr1<- ggroup(horizontal=TRUE, spacing=0, container = g3)
    b1<-glabel("Assessors Times Variable     ",container=gr1)
    font(b1) <- list(weight="bold",size= 10,family="sans",align ="left",spacing = 5)
    out <- capture.output(DMAV(n,npp,nrr,naa,gdata1))
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea, out)
  }

  # 3 -------------------------------------------------------------------------------------------
  #Assessors Times Variable Consonance
  ATVC<-function(h,...){
    n<-get("n",envir =mi)
    npp<-get("npp",envir =mi)
    nrr<-get("nrr",envir =mi)
    naa<-get("naa",envir =mi)
    gdata1<-get("gdata1",envir =mi)
    anames<-get("anames",envir =mi)
    valnames<-colnames(gdata1)
    assign("valnames",valnames, envir =mi)

    COAV<-function(n,npp,nrr,naa,gdata1){
      varib<-gdata1
      nfac<-ncol(varib)
      n<-n
      nn=naa
      mm=npp*nrr
      namesmat<-matrix(,1,n)
      for(i in 1:n){
        namesmat[,i]<-paste0("Mat", i, ".txt")
      }
      #Matrix act for Variable Times Assesors
      res=n-1
      x<-matrix(,mm,nn)
      while(res>=0){
        k<-n-res
        cont=0
        nmat<-matrix(varib[,k])
        for(i in 1:nn){
          for(j in 1:mm){
            x[j,i]<-nmat[j+cont,]
          }
          cont=cont+mm
        }
        colnames(x)<-paste0("J-",anames,".","A",k)
        write.table(x,namesmat[,n-res])
        res<-res-1
      }

      for(i in 1:n) {
        mat <- paste("Mat.", i, sep = "")
        assign(mat, read.table(namesmat[,i]))
      }

      names2<-ls(pattern = "Mat..$")
      assign("namesmat",namesmat,envir =mi)
      MVAF<-0
      MCON<-matrix(,1,1)
      for(i in 1:n) {
        x<-read.table(namesmat[,i])
        print(paste("Data:",names2[i]),quote=F)
        pandoc.table(x,plain.ascii = TRUE)

        #PCA library ----------------------------------------------------------------------------------------
        PCA <- function (X, scale.unit = TRUE, ncp = 5, ind.sup = NULL, quanti.sup = NULL,
                         quali.sup = NULL, row.w = NULL, col.w = NULL, graph = TRUE,
                         axes = c(1, 2))
        {
          moy.ptab <- function(V, poids) {
            as.vector(crossprod(poids/sum(poids),as.matrix(V)))
          }

          ec.tab <- function(V, poids) {
            ecart.type <- sqrt(as.vector(crossprod(poids/sum(poids),as.matrix(V^2))))
            ecart.type[ecart.type <= 1e-16] <- 1
            return(ecart.type)
          }
          fct.eta2 <- function(vec,x,weights) {   ## pb avec les poids
            VB <- function(xx) {
              return(sum((colSums((tt*xx)*weights)^2)/ni))
            }
            tt <- tab.disjonctif(vec)
            ni <- colSums(tt*weights)
            unlist(lapply(as.data.frame(x),VB))/colSums(x^2*weights)
          }

          X <- as.data.frame(X)
          X <- droplevels(X)
          if (any(is.na(X))) {
            warning("Missing values are imputed by the mean of the variable: you should use the imputePCA function of the missMDA package")
            if (is.null(quali.sup))
              X[is.na(X)] = matrix(colMeans(X,na.rm=TRUE),ncol=ncol(X),nrow=nrow(X),byrow=TRUE)[is.na(X)]
            else for (j in (1:ncol(X))[-quali.sup]) X[, j] <- replace(X[, j], is.na(X[, j]), mean(X[, j], na.rm = TRUE))
          }
          Xtot <- X
          if (!is.null(quali.sup))
            X <- X[, -quali.sup,drop=FALSE]
          auxi <- colnames(X)[!sapply(X, is.numeric)]
          if (length(auxi)>0)  stop(paste("\nThe following variables are not quantitative: ", auxi))
          todelete <- c(quali.sup, quanti.sup)
          if (!is.null(todelete)) X <- Xtot[, -todelete,drop=FALSE]
          if (!is.null(ind.sup)) {
            X.ind.sup <- X[ind.sup, , drop = FALSE]
            X <- X[-ind.sup, , drop = FALSE]
          }
          ncp <- min(ncp, nrow(X) - 1, ncol(X))
          if (is.null(row.w)) row.w <- rep(1, nrow(X))
          row.w.init <- row.w
          row.w <- row.w/sum(row.w)
          if (is.null(col.w)) col.w <- rep(1, ncol(X))
          centre <- moy.ptab(X,row.w)
          data <- X
          X <- t(t(as.matrix(X))-centre)
          if (is.null(attributes(X)$row.names)) rownames(X) <- rownames(data)
          if (is.null(attributes(X)$names)) colnames(X) <- colnames(data)
          if (scale.unit) {
            ecart.type <- ec.tab(X,row.w)
            X <- t(t(X)/ecart.type)
          }
          else ecart.type <- rep(1, length(centre))
          dist2.ind <- rowSums(t(t(X^2)*col.w))
          dist2.var <- as.vector(crossprod(rep(1,nrow(X)),as.matrix(X^2*row.w)))
          res.call <- list(row.w = (row.w/sum(row.w)), col.w = col.w,
                           scale.unit = scale.unit, ncp = ncp, centre = centre,
                           ecart.type = ecart.type, X = Xtot, row.w.init = row.w.init,call=match.call())
          tmp <- svd.triplet(X, row.w = row.w, col.w = col.w,ncp=ncp)
          eig <- tmp$vs^2
          vp <- as.data.frame(matrix(NA, length(eig), 3))
          rownames(vp) <- paste("F", 1:length(eig))
          colnames(vp) <- c("Eigenvalue","Variability (%)","Cumulative %")
          vp[, "Eigenvalue"] <- eig
          vp[, "Variability (%)"] <- (eig/sum(eig)) * 100
          vp[, "Cumulative %"] <- cumsum(vp[, "Variability (%)"])
          V <- tmp$V
          U <- tmp$U
          eig <- eig[1:ncp]
          coord.ind <- t(t(as.matrix(U))*sqrt(eig))
          coord.var <- t(t(as.matrix(V))*sqrt(eig))
          contrib.var <- t(t(coord.var^2)/eig)*col.w
          dist2 <- dist2.var
          cor.var <- coord.var/sqrt(dist2)
          cos2.var <- cor.var^2
          rownames(coord.var) <- rownames(cos2.var) <- rownames(cor.var) <- rownames(contrib.var) <- colnames(X)
          colnames(coord.var) <- colnames(cos2.var) <- colnames(cor.var) <- colnames(contrib.var) <- paste("Dim",
                                                                                                           c(1:ncol(V)), sep = ".")
          res.var <- list(coord = coord.var[, 1:ncp], cor = cor.var[,
                                                                    1:ncp], cos2 = cos2.var[, 1:ncp], contrib = contrib.var[,
                                                                                                                            1:ncp] * 100)
          dist2 <- dist2.ind
          cos2.ind <- coord.ind^2/dist2
          contrib.ind <- t(t(coord.ind^2*row.w/sum(row.w))/eig)
          rownames(coord.ind) <- rownames(cos2.ind) <- rownames(contrib.ind) <- names(dist2) <- rownames(X)
          colnames(coord.ind) <- colnames(cos2.ind) <- colnames(contrib.ind) <- paste("Dim",
                                                                                      c(1:ncol(U)), sep = ".")
          res.ind <- list(coord = coord.ind[, 1:ncp,drop=FALSE], cos2 = cos2.ind[,
                                                                                 1:ncp,drop=FALSE], contrib = contrib.ind[, 1:ncp,drop=FALSE] * 100, dist = sqrt(dist2))
          res <- list(eig = vp, var = res.var, ind = res.ind, svd = tmp)
          if (!is.null(ind.sup)) {
            if (is.null(ecart.type)) ecart.type <- rep(1, length(centre))
            X.ind.sup <- t(t(as.matrix(X.ind.sup))-centre)
            X.ind.sup <- t(t(X.ind.sup)/ecart.type)
            coord.ind.sup <- t(t(X.ind.sup)*col.w)
            coord.ind.sup <- crossprod(t(coord.ind.sup),tmp$V)
            dist2 <- rowSums(t(t(X.ind.sup^2)*col.w))
            cos2.ind.sup <- coord.ind.sup^2/dist2
            coord.ind.sup <- coord.ind.sup[, 1:ncp, drop = F]
            cos2.ind.sup <- cos2.ind.sup[, 1:ncp, drop = F]
            colnames(coord.ind.sup) <- colnames(cos2.ind.sup) <- paste("Dim",  c(1:ncp), sep = ".")
            rownames(coord.ind.sup) <- rownames(cos2.ind.sup) <- names(dist2) <- rownames(X.ind.sup)
            res.ind.sup <- list(coord = coord.ind.sup, cos2 = cos2.ind.sup, dist = sqrt(dist2))
            res$ind.sup = res.ind.sup
            res.call$ind.sup = ind.sup
          }
          if (!is.null(quanti.sup)) {
            X.quanti.sup <- as.data.frame(Xtot[, quanti.sup,drop=FALSE])
            if (!is.null(ind.sup)) X.quanti.sup <- as.data.frame(X.quanti.sup[-ind.sup, ,drop=FALSE])
            colnames(X.quanti.sup) <- colnames(Xtot)[quanti.sup]
            res.call$quanti.sup = X.quanti.sup
            centre.sup <- moy.ptab(X.quanti.sup,row.w)
            X.quanti.sup <- t(t(as.matrix(X.quanti.sup))-centre.sup)
            if (scale.unit) {
              ecart.type.sup <- ec.tab(X.quanti.sup, row.w)
              X.quanti.sup <- t(t(X.quanti.sup)/ecart.type.sup)
            }
            coord.vcs <- t(X.quanti.sup*row.w)
            coord.vcs <- crossprod(t(coord.vcs),tmp$U)
            col.w.vcs <- rep(1, ncol(coord.vcs))
            cor.vcs <- matrix(NA, ncol(X.quanti.sup), ncol(tmp$U))
            dist2 <- as.vector(crossprod(rep(1,nrow(X.quanti.sup)),as.matrix(X.quanti.sup^2*row.w)))
            cor.vcs <- coord.vcs/sqrt(dist2)
            cos2.vcs <- cor.vcs^2
            colnames(coord.vcs) <- colnames(cor.vcs) <- colnames(cos2.vcs) <- paste("Dim", c(1:ncol(cor.vcs)), sep = ".")
            rownames(coord.vcs) <- rownames(cor.vcs) <- rownames(cos2.vcs) <- colnames(Xtot)[quanti.sup]
            res.quanti.sup <- list(coord = coord.vcs[, 1:ncp, drop=FALSE], cor = cor.vcs[, 1:ncp, drop=FALSE], cos2 = cos2.vcs[, 1:ncp, drop=FALSE])
            res$quanti.sup = res.quanti.sup
          }
          if (!is.null(quali.sup)) {
            X.quali.sup <- as.data.frame(Xtot[, quali.sup,drop=FALSE])
            if (!is.null(ind.sup)) X.quali.sup <- as.data.frame(X.quali.sup[-ind.sup,,drop=FALSE])
            colnames(X.quali.sup) <- colnames(Xtot)[quali.sup]
            nombre <- modalite <- NULL

            if (ncp>1) eta2 <- t(sapply(X.quali.sup,fct.eta2,res$ind$coord,weights=row.w))
            else {
              eta2 <- as.matrix(sapply(X.quali.sup,fct.eta2,res$ind$coord,weights=row.w),ncol=ncp)
              colnames(eta2) = paste("Dim", 1:ncp)
              rownames(eta2) = colnames(X.quali.sup)
            }

            for (i in 1:ncol(X.quali.sup)) {
              var <- as.factor(X.quali.sup[, i])
              n.mod <- nlevels(var)
              modalite <- c(modalite, n.mod)
              bary <- matrix(NA, n.mod, ncol(X))
              for (j in 1:n.mod) {
                ind <- levels(var)[j]
                bary[j, ] <- moy.ptab(data[which(var == ind), ], row.w[which(var == ind)])

                nombre <- c(nombre, sum(row.w.init[which(var == ind)]))
              }
              colnames(bary) <- colnames(X)
              if ((levels(var)[1] %in% (1:nrow(X))) | (levels(var)[1] %in% c("y", "Y", "n", "N"))) row.names(bary) <- paste(colnames(X.quali.sup)[i], as.character(levels(var)))
              else row.names(bary) <- as.character(levels(var))
              if (i == 1)  barycentre <- bary
              else barycentre <- rbind(barycentre, bary)
            }
            bary <- t(t(barycentre)-centre)
            if (!is.null(ecart.type)) bary <- t(t(bary)/ecart.type)
            dist2 <- rowSums(t(t(bary^2)*col.w))
            coord.barycentre <- t(t(bary)*col.w)
            coord.barycentre <- crossprod(t(coord.barycentre),tmp$V)
            colnames(coord.barycentre) <- paste("Dim", 1:ncol(coord.barycentre), sep = ".")
            cos2.bary.sup <- coord.barycentre^2/dist2
            vtest <- t(t(coord.barycentre)/sqrt(eig))
            if (sum(row.w.init)>1) vtest <- vtest*sqrt(nombre/((sum(row.w.init) - nombre)/(sum(row.w.init) - 1)))
            else vtest <- vtest*sqrt(nombre)
            cos2.bary.sup <- cos2.bary.sup[, 1:ncp, drop=FALSE]
            coord.barycentre <- coord.barycentre[, 1:ncp, drop=FALSE]
            vtest <- vtest[, 1:ncp, drop=FALSE]
            dimnames(cos2.bary.sup) <- dimnames(vtest) <- dimnames(coord.barycentre)
            names(dist2) <- rownames(coord.barycentre)
            res.quali.sup <- list(coord = coord.barycentre, cos2 = cos2.bary.sup, v.test = vtest, dist = sqrt(dist2), eta2=eta2)
            call.quali.sup <- list(quali.sup = X.quali.sup, modalite = modalite, nombre = nombre, barycentre = as.data.frame(barycentre), numero = quali.sup)
            res$quali.sup = res.quali.sup
            res.call$quali.sup = call.quali.sup
          }
          res$call = res.call
          class(res) <- c("PCA", "list ")
          if (graph & (ncp>1)) {
          }
          return(res)
        }
        #------------------------------------------------------------------------------------------------------
        result<-PCA(x)
        #Eigenvalues
        EG<-round(as.matrix(result$eig),digits = 3)
        print("Principal Component Analysis",quote=F)
        pandoc.table(EG,plain.ascii = TRUE)
        #Inertia for factors
        EV<-as.matrix(EG[,1])
        VAF<-as.matrix(EG[,2])
        print("Variability Factors (VAF)",quote=F)
        colnames(VAF)<-paste("At",i,"(V %)")
        pandoc.table(VAF,plain.ascii = TRUE)
        nx<-nrow(EG)
        EVC<-(EV[2:nx,1]^2)
        #Consonance
        CONS<-as.matrix(EV[1,1]^2)/(sum(EVC))
        rownames(CONS)<-paste("At",i)
        colnames(CONS)<-"Consonance"
        pandoc.table(CONS,plain.ascii = TRUE)
        MVAF<-cbind(MVAF,VAF)
        MCON<-rbind(MCON,CONS)
      }
      color<-c("#FFC300","darkorange","#FF5733","#C70039","#900C3F","#E91E63","#9C27B0","#673AB7","darkorchid4","darkblue","#3F51B5","#03A9F4","#00BCD4","#009688","darkgreen","#4CAF50","#CDDC39","gray","black","#FF9999","#99FF99","#99CCFF","#9999FF","#CC99FF","#FF99CC")
      print("FAV Matrix",quote=F)
      nvaf<-ncol(MVAF)
      MVAF1<-MVAF[,2:nvaf]
      nma<-ncol(MVAF1)
      pandoc.table(MVAF1,plain.ascii = TRUE)
      rest<-ncol(MVAF1)
      if(rest<=10){
        dev.new()
        par(mfrow=c(1,2))
        matplot(MVAF1,type =c("o"),ylim=c(0,100),pch=20,col=color,xaxt="n",ylab="",cex.axis=0.5,cex.lab=0.5,cex.names=0.5,main=paste("Proportion VAF per dimension for the",nma,"PCAs"))
        axis(1,1:nrow(MVAF1),rownames(MVAF1),cex.axis=0.5)
        legend("topright",legend=paste("At",c(1:ncol(MVAF1)),"-",valnames),pch=16,col=color,cex=0.6)
        print("Consonance Matrix",quote=F)
        ncon<-nrow(MCON)
        MCON1<-MCON[2:ncon,]
        maxco<-max(MCON1)+10
        pandoc.table(MCON1,plain.ascii = TRUE)
        barplot(MCON1,ylim=c(0,maxco),col=color,cex.axis=0.5,cex.lab=0.5,cex.names=0.5,main="Consonance")
      }else{
        dev.new()
        matplot(MVAF1,type =c("o"),ylim=c(0,100),pch=20,col=color,xaxt="n",ylab="",cex.axis=0.5,cex.lab=0.5,cex.names=0.5,main=paste("Proportion VAF per dimension for the",nma,"PCAs"))
        axis(1,1:nrow(MVAF1),rownames(MVAF1),cex.axis=0.5)
        legend("topright",legend=paste("At",c(1:ncol(MVAF1)),"-",valnames),pch=16,col=color,cex=0.6)
        print("Consonance Matrix",quote=F)
        ncon<-nrow(MCON)
        MCON1<-MCON[2:ncon,]
        maxco<-max(MCON1)+10
        pandoc.table(MCON1,plain.ascii = TRUE)
        dev.new()
        barplot(MCON1,ylim=c(0,maxco),col=color,cex.axis=0.5,cex.lab=0.5,cex.names=0.5,main="Consonance")
      }
      MVAF2<-as.matrix(MVAF1)
      MCON2<-as.matrix(MCON1)
      colnames(MCON2)<-"Consonance"
      assign("MCON1",MCON1, envir =mi)
      assign("MVAF2",MVAF2, envir =mi)
      assign("MCON2",MCON2, envir =mi)
    }
    ##g4
    tbl<-glayout(container=g4)
    gseparator(horizontal=TRUE, container=g4)
    outputArea <- gtext(container=g4, expand=TRUE,width = 780,height= 200)
    gr1<- ggroup(horizontal=TRUE, spacing=0, container = g4)
    tbl <- glayout(container=gr1, horizontal=FALSE)
    tbl[1,1] <- "Assessors Times Variable"
    tbl[1,2] <- (cb1 <- gbutton("Loading Plots", container=tbl,handler=loadingAV))
    tbl[1,3] <- "Consonance Analysis"
    tbl[1,4] <- (cb1 <- gbutton("Save as PDF", container=tbl,handler=save))
    out <- capture.output(COAV(n,npp,nrr,naa,gdata1))
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea, out)
  }

  #-------------------------------------------------------------------------------------------
  # MENUS
  abrir2<-list(una=gaction("csv",handler=abrirc),dos=gaction("txt",handler=abrirt),tres=gaction("xlsx",handler=openex))
  menulistaA<-list(Open=abrir2,u2=gaction("View",handler=ver),u3=gaction("Refresh",handler=inicio),u4=gaction("Close",handler=cerrar))
  imp2<-list(una=gaction("Descriptive per matrix",handler=ATVD),dos=gaction("Consonance",handler=ATVC))
  menulistaZ<-list(u0=gaction("Parameters",handler=parm),u1=gaction("Descriptive general",handler=cd),Consonance.analysis=imp2)

  ##MENU - HELP

  #Manual
  y1<- function(h,..) gmessage("http://www.uv.mx/personal/nehuerta/cons/",title="Link")

  menulistaY<-list(u0=gaction("Information",handler=y1))

  ##MENU
  mb_list <-list(File=menulistaA,Method=menulistaZ,Help=menulistaY)
  gmenu(mb_list, container=g)

  ##g1
  #Information
  tmp1 <- gframe("", container=g1, expand=TRUE,horizontal=FALSE)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                  Consonance Analysis Module                  ",container=tmp1)
  font(tg) <- list(weight="bold",size= 28,family="sans",align ="center",spacing = 5)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("            ITAM             Delphi Intelligence                 UV        ",container=tmp1)
  font(tg) <- list(weight="bold",size= 24,family="sans",align ="center",spacing = 5)
  tg<-glabel("                 Statistics Deparment                                            Sophisticated Research                    Universidad Veracruzana             ",container=tmp1)
  font(tg) <- list(weight="bold",size= 12,family="sans",align ="center",spacing = 5)
  tg<-glabel("                                                                  ",container=tmp1)
  tg<-glabel("                                                                  ",container=tmp1)
  visible(w) <- TRUE
}
