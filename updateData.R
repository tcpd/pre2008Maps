dir ="./AE/Data"
for(file in list.files(dir)){
  if(file!="Madras"){
    st = fread(paste(dir,file,"derived/lokdhaba/ae_maps.csv",sep="/"),na="")
    fwrite(st,paste0("/Users/mohitkumar/github/pre2008Maps/",file,"_ae_ld_maps.csv"),na="")
  }
}

