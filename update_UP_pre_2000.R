ac.as.pc.1999 = read.csv("~/github/tcpd_data/data/GE/Data/derived/ac_wise_pc_mastersheet.csv",stringsAsFactors = F, na="") %>% subset(State_Name== "Uttar_Pradesh" & Assembly_No == 13 & Position ==1 & Poll_No == 0)
ac.as.pc.2004 = read.csv("~/github/tcpd_data/data/GE/Data/derived/ac_wise_pc_mastersheet.csv",stringsAsFactors = F, na="") %>% subset(State_Name== "Uttar_Pradesh" & Assembly_No == 14 & Position ==1 & Poll_No == 0)

ac.as.pc.2004$Old_AC = NA
unmatched = c()
for( i in 1:nrow(ac.as.pc.2004)){
  if(is.na(ac.as.pc.2004$Old_AC[i])){
    print(paste("matching AC:",ac.as.pc.2004$Constituency_No[i],ac.as.pc.2004$Constituency_Name[i],ac.as.pc.2004$PC_Name[i]))
    ac.as.pc.1999$dists = stringdist::stringdist(tolower(ac.as.pc.2004$Constituency_Name[i]),tolower(ac.as.pc.1999$Constituency_Name),method = "jw")
    matched = which(ac.as.pc.1999$dists==0)
    ac.as.pc.1999 = ac.as.pc.1999[order(ac.as.pc.1999$dists),]
    if(length(matched)==1){
      ac.as.pc.2004$Old_AC[i] = ac.as.pc.1999$Constituency_No[matched]
      print("matched")
    }else{
      
      print(ac.as.pc.1999[1:10,c("Constituency_No","Constituency_Name","dists","PC_Name")])
      to_select = readline(prompt = "select row numbers which matches the constituency:")
      sel = as.integer(strsplit(to_select," ")[[1]]) #pick number of the correct village to map
      if(length(sel)==1){
        ac.as.pc.2004$Old_AC[i] = ac.as.pc.1999$Constituency_No[sel]
      }else{
        unmatched = c(unmatched,ac.as.pc.2004$Constituency_No[i])
        print("not matched")
      }
      
    }
  }
  
}
sum(is.na(ac.as.pc.2004$Old_AC))

const.map = unique(ac.as.pc.2004[,c("Constituency_No","Old_AC")])

post.2000.UP = read_sf("~/github/pre2008Maps/Maps/Uttar_Pradesh_pre2008ac.geojson")
post.2000.UP$AC_NO = as.integer(post.2000.UP$AC_NO)

pre.2000.up = dplyr::left_join(post.2000.UP,const.map,by=c("AC_NO"="Constituency_No"))
pre.2000.up$AC_NO = pre.2000.up$Old_AC
pre.2000.up$Old_AC = NULL
names(pre.2000.up)

pre.2000.uk = read_sf("~/github/pre2008Maps/Maps/pre_2000_UK.geojson")
pre.2000.uk$AC_NO = as.integer(pre.2000.uk$AC_NO)
setnames(pre.2000.uk, old ="AC_name",new="AC_NAME")
pre.2000.uk$State = unique(pre.2000.up$State)
pre.2000.uk$ST_CODE = unique(pre.2000.up$ST_CODE)
pre.2000.uk$STcode2001 = NA
pre.2000.uk$SP_ID = NA
pre.2000.uk$AC_TYPE = NA

pre.2000.all.up = rbind(pre.2000.up,subset(pre.2000.uk,select=names(pre.2000.up)))

st = "Uttar_Pradesh"
pre_2008_ac = read_sf(paste0("~/github/pre2008Maps/Maps/",st,"_pre2008ac.geojson"))
post_2008_ac = read_sf(paste0("~/github/ld_temp/LokdhabaApi/Maps/json/",st,"_AC_json.geojson"))
setnames(post_2008_ac,old= c("ASSEMBLY","ASSEMBLY_1","State_Name","PARLIAMENT", "P_NAME"),new=c("AC_NO","AC_NAME","State","PC_NO","PC_NAME"))


st.winners = fread(paste0("~/github/tcpd_data/data/AE/Data/",st,"/derived/mastersheet.csv"),na="") %>% subset(Position ==1 & Poll_No ==0)
st.assemblies = st.winners[,list("Total_Seats"= .N),by=c("State_Name","Assembly_No","Year","DelimID")]
print(st.assemblies)


pre.2000.all.up$Min_Assembly = min(st.assemblies$Assembly_No[which(st.assemblies$DelimID==3)])
pre.2000.all.up$Max_Assembly = 13

pre_2008_ac$Min_Assembly = 14
pre_2008_ac$Max_Assembly = max(st.assemblies$Assembly_No[which(st.assemblies$DelimID==3)])

pre_2008_ac = rbind(pre_2008_ac,pre.2000.all.up)
post_2008_ac$Min_Assembly = min(st.assemblies$Assembly_No[which(st.assemblies$DelimID==4)])
post_2008_ac$Max_Assembly = max(st.assemblies$Assembly_No[which(st.assemblies$DelimID==4)])

pre_2008_ac[,names(post_2008_ac)[which(!names(post_2008_ac) %in% names(pre_2008_ac))]] = NA
post_2008_ac[,names(pre_2008_ac)[which(!names(pre_2008_ac) %in% names(post_2008_ac))]] = NA


all_ac = rbind(pre_2008_ac,subset(post_2008_ac, select = names(pre_2008_ac)))

all_ac$Election_Type = "AE"
setnames(all_ac, old="State",new="State_Name")
st_write(all_ac,paste0("~/Maps/allMaps/",st,"_all_ac.geojson"),delete_dsn = TRUE)



sort(unique(ac.as.pc$State_Name))
unique(ac.as.pc$Assembly_No)
unique(ac.as.pc$Year)
