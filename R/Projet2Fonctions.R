
# FONCTIONS #

#tourne la matrice (pour l'affichage) ##########################################
rotate <- function(x) t(apply(x, 2, rev))                                      #
################################################################################

#génération de l'image #########################################################
generate <- function(hauteur,largeur,mat){                                     #
  h<-1
  l<-1
  while(h<hauteur){
    while(l<largeur){
      if(paste(sapply(list(mat[h,l],mat[h,l+1]),function(x) x),collapse='')=="01"){
        mat[h+1,l]<-1
      }else{
        mat[h+1,l]<-0
      }
      l<-l+1
      while(l<largeur){
        if(paste(sapply(list(mat[h,l-1],mat[h,l],mat[h,l+1]),function(x) x),collapse='')=="001" || paste(sapply(list(mat[h,l-1],mat[h,l],mat[h,l+1]),function(x) x),collapse='')=="100"){
          mat[h+1,l]<-1
        }else{
          mat[h+1,l]<-0
        }
        l<-l+1
      }
      if(paste(sapply(list(mat[h,l-1],mat[h,l]),function(x) x),collapse='')=="10"){
        mat[h,l]<-1
      }else{
        mat[h,l]<-0
      }
      l<-l+1
    }
    l<-1
    h<-h+1
  }
  image(rotate(mat))
  return(mat)
}                                                                              #
################################################################################

#transformation des 101 en 111 #################################################
turn101to111 <- function(hauteur,largeur,mat){                                 #
  h<-1
  l<-1
  while(h<=hauteur){
    while(l+1<largeur){
      if(paste(sapply(list(mat[h,l],mat[h,l+1],mat[h,l+2]),function(x) x),collapse='')=="101"){
        mat[h,l+1]<-1
      }
      l<-l+1
    }
    l<-1
    h<-h+1
  }
  image(rotate(mat))
  return(mat)
}                                                                              #
################################################################################

#Suppresion des "BLOC" #########################################################
supprBloc <- function(hauteur,largeur,mat){                                    #
  #Premiere ligne
  l<-1
  h<-1
  while(l+1<largeur){
    if(paste(sapply(list(mat[h,l],mat[h,l+1],mat[h,l+2],mat[h+1,l+1],mat[h+1,l+2]),function(x) x),collapse='')=="10011"){
      mat[h,l+1]<-1
      mat[h,l+2]<-1
    }
    l<-l+1
  }
  #Milieu
  l<-1
  h<-h+1
  while(h+1<hauteur){
    while(l+1<largeur){
      if(paste(sapply(list(mat[h-1,l+1],mat[h-1,l+2],mat[h,l],mat[h,l+1],mat[h,l+2],mat[h+1,l+1],mat[h+1,l+2]),function(x) x),collapse='')=="1110011"){
        mat[h,l+1]<-1
        mat[h,l+2]<-1
      }
      l<-l+1
    }
    l<-1
    h<-h+1
  }
  #Derniere ligne
  while(l+1<largeur){
    if(paste(sapply(list(mat[h-1,l+1],mat[h-1,l+2],mat[h,l],mat[h,l+1],mat[h,l+2]),function(x) x),collapse='')=="11100"){
      mat[h,l+1]<-1
      mat[h,l+2]<-1
    }
    l<-l+1
  }
  image(rotate(mat))
  return(mat)
}                                                                              #
################################################################################

#Supprime les détails sur les bords droit et gauche ############################
detail <- function(hauteur,mat){                                               #
  l<-1
  boucle<-0
  while(boucle<2){
    h<-1
    if(paste(sapply(list(mat[h,l],mat[h+1,l]),function(x) x),collapse='')=="01"){
      mat[h,l]<-1
    }
    while(h+2<hauteur){
      if(paste(sapply(list(mat[h,l],mat[h+1,l],mat[h+2,l]),function(x) x),collapse='')=="101"){
        mat[h+1,l]<-1
      }
      h<-h+1
    }
    if(paste(sapply(list(mat[h+1,l],mat[h+2,l]),function(x) x),collapse='')=="10"){
      mat[h+2,l]<-1
    }
    l<-largeur
    boucle<-boucle+1
  }
  image(rotate(mat))
  return(mat)
}                                                                              #
################################################################################

# Compte les nombres de 0 et 1 #################################################
counter <- function(alea,mat,hauteur,largeur){                                 #
  Alea0<-length(alea[alea==0])
  Alea1<-length(alea[alea==1])
  Mat0<-length(mat[mat==0])
  Mat1<-length(mat[mat==1])
  return(data.frame(Gen0=Alea0,Gen1=Alea1,End0=Mat0,End1=Mat1,GenRatio=Alea0/Alea1,EndRatio=Mat0/Mat1,nbTriangle=nbTriangle(hauteur,largeur,mat)))
}                                                                              #
################################################################################

# Compte le nombre de triangles sur la figure ##################################
nbTriangle <- function(hauteur,largeur,mat){                                   #
  compt<-0
  h<-1
  l<-1
  #1ere ligne
  while(l<largeur-1){
    if(paste(sapply(list(mat[h,l],mat[h,l+1]),function(x) x),collapse='')=="10"){
      compt<-compt+1
      mat[h,l+1]<-2
    }
    l<-l+1
  }
  #milieu
  while(h<hauteur-1){
    l<-2
    while(l<largeur){
      if(paste(sapply(list(mat[h,l-1],mat[h-1,l],mat[h,l]),function(x) x),collapse='')=="110"){
        compt<-compt+1
        mat[h,l]<-2
      }
      l<-l+1
    }
    h<-h+1
  }
  #coté droit et gauche
  l<-1
  h<-1
  while(h+1<hauteur){
    if(paste(sapply(list(mat[h,l],mat[h+1,l]),function(x) x),collapse='')=="10"){
      compt<-compt+1
      mat[h+1,l]<-2
    }
    h<-h+1
  }
  image(rotate(mat))
  return(compt)
}                                                                              #
################################################################################

# Fait évoluer le nombre de 0 dans alea ########################################
nb0 <- function(nb){                                                           #
  alea<-rep(1,100)
  where0<-sample(1:100,size=nb)
  i<-1
  while(i<=nb){
    alea[where0[i]]<-0
    i<-i+1
  }
  return(alea)
}                                                                              #
################################################################################

# Simule 1 ou plusieurs fois ###################################################
OneSimu <- function(largeur,hauteur,evolve){                                   #
  FinalTab<-head(data.frame(Gen0=NA,Gen1=NA,End0=NA,End1=NA,GenRatio=NA,EndRatio=NA,nbTriangle=NA),0)
  if(evolve){
    nbIteration<-100
  }else{
    nbIteration<-1
  }
  i<-1
  while(i<=nbIteration) {
    if(evolve){
      alea<-nb0(i)   #####Permet de calculer avec le nb de 0 que l'on veux en generation initiale
    }else{
      alea<-sample(c(0,1),size = largeur,replace=TRUE) ##### Si on veut une seule génération
    }
    co<-matrix(0,hauteur,largeur)
    co[1,]<-alea
    co<- generate(hauteur,largeur,co)
    co<-turn101to111(hauteur,largeur,co)
    co<-supprBloc(hauteur,largeur,co)
    co<-turn101to111(hauteur,largeur,co)
    co<-detail(hauteur,co)
    FinalTab<-rbind(FinalTab,counter(alea,co,hauteur,largeur))
    i<-i+1
  }
  print(FinalTab)
  if(evolve){
    #Le graphique final
    plot(x=FinalTab$Gen0,
         y=FinalTab$nbTriangle,
         main="Evolution du nombre de triangle en fonction du nombre de 0 initialement",
         xlab="Nombre de 0",
         ylab="Nombre de Triangle",
         col.main="red")
    panel.smooth(FinalTab$Gen0, FinalTab$nbTriangle)
    abline(v=33,col="red", lty=2, lwd=3)
    abline(v=66,col="red", lty=2, lwd=3)
  }
}                                                                              #
################################################################################
