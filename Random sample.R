install.packages("xlsx")

for(i in 1:1){
  allresultdata = list()
  u = 0
  readfile = function(){
    path <<- readline(prompt = "請輸入名單的檔案路徑(e.g. C:/Users/.../file.csv): ")
    All <<- read.csv(path, header = TRUE)
    #Datasetup
    population = list()
    Biggroupname = c()
    group = levels(All$組別)
    for(i in 1: length(group)){
      if (gsub(" ", "", group[i], fixed = TRUE) != ""){
        Biggroupname = unique(c(Biggroupname, substr(group[i], 1, 1)))
        population[[group[i]]] = All[which(All$組別 == group[i]),]
      }
    }
    Biggroup = list()
    for(i in 1: length(Biggroupname)){
      Biggroup[[Biggroupname[i]]] = population[substr(names(population), 1, 1) == Biggroupname[i]]
    }
    population <<- population
    Biggroup <<- Biggroup
    Biggroupname <<- Biggroupname
    cat("\n Data Setup Ready!!\n")
  }
  
  #report selection function
  report = function(group = "A", n = 1){
    enter = unique(unlist(strsplit(gsub(",", " ", readline(prompt = "輸入報告的組別分類(e.g. B): "), fixed = TRUE)," ")))
    enter = enter[! enter %in% ""]
    AA = Biggroup[which(Biggroupname %in% as.character(enter))]
    base = c()
    for(i in 1:length(AA)){
      base = c(base, AA[[i]])
    }
    select = function(AA){
      AA = AA[sample(1:nrow(AA), nrow(AA), replace = FALSE),]
      return(AA[sample(1:nrow(AA), 1), ])
    }
    choose = lapply(base, function(x) select(x))
    result = c()
    for(i in 1:length(choose)){
      result = rbind(result, choose[[i]])
    }
    cat("\n\n", paste(enter,collapse = ","), "大組的報告幸運兒:\n\n" )
    u <<- u + 1
    name <<- paste(paste0("#",u, collapse = ""), paste(enter,collapse = ","), "大組的報告幸運兒" )
    allresultdata[[name]] <<- result
    return(result[,c("組別","學號","姓名")])
  }
  
  
  #ask selection function
  ask = function(group = "A, B", n = 10){
    enter = unique(unlist(strsplit(gsub(",", " ", readline(prompt = "輸入問問題的組別分類(e.g. C, D): "), fixed = TRUE)," ")))
    enter = enter[! enter %in% ""]
    AA = Biggroup[which(Biggroupname %in% as.character(enter))]
    base = c()
    for(i in 1 : length(enter)){
      for(j in 1: length(AA[[i]])){
        base = rbind(base, AA[[i]][[j]])
      }
    }
    base = base[(sample(1:nrow(base), nrow(base), replace = FALSE)),]
    choose = base[sample(1:nrow(base), 10, replace = FALSE),]
    choose = choose[order(choose$組別),]
    cat("\n\n", paste(enter,collapse = ","), "大組問問題的幸運兒:\n\n")
    u <<- u + 1
    name <<- paste(paste0("#",u, collapse = ""), paste(enter,collapse = ","), "大組問問題的幸運兒" )
    allresultdata[[name]] <<- choose
    return(choose[,c("組別","學號","姓名")])
  }
  
  
  #allresults function
  
  allresults = function(){
    print(lapply(allresultdata,'[', c("組別","學號","姓名")))
    Sys.sleep(1)
    if(readline(prompt = "是否要儲存抽籤結果?[Y/N]:") == "Y"){
      if(Sys.getenv("R_ARCH") == "/x64"){
        Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_151')
        cat("Please install or update your JAVA from the following website first: \n")
        cat("http://javadl.oracle.com/webapps/download/AutoDL?BundleId=227552_e758a0de34e24606bca991d704f6dcbf\n")
        invisible(readline(prompt = "After finishing the installation, press [enter] to continue.\n"))
      }else{
        Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_151')
        cat("Please install or update your JAVA from the following website first: \n")
        cat("http://javadl.oracle.com/webapps/download/AutoDL?BundleId=227550_e758a0de34e24606bca991d704f6dcbf")
        invisible(readline(prompt = "After finishing the installation, press [enter] to continue.\n"))
      }
      
      oldw <<- getOption("warn")
      options(warn = -1)
      library(xlsx, warn.conflicts = FALSE, quietly = TRUE)
      options(warn = oldw)
      
      for(i in 1:1){
        wd <<- readline(prompt = "請輸入檔案存放的資料夾路徑(e.g. C:/Users/.../foldername):")
        filename <<- readline(prompt = "請輸入檔案名稱:")
        setwd(wd)
        for(i in 1:length(allresultdata)){
          if(i == 1){
            write.xlsx(allresultdata[[i]], file = paste0(filename,".xlsx"), 
                                sheetName = names(allresultdata)[i], row.names=FALSE)
          }else{
            write.xlsx(allresultdata[[i]], file = paste0(filename,".xlsx"), 
                       sheetName = names(allresultdata)[i], append=TRUE, row.names=FALSE)
          }
        }
        cat("\n",paste("已將結果存至:",paste0(wd, paste0("\\",filename,".xlsx"))),"\n")
      }
    }
  }
  
  cat("\n Ready!!\n")
  cat("\n 請按照下列指示以及範例格式匯入名單資料\n")
  readfile()
  cat("\n 重新匯入名單資料請直接在命令列輸入readfile()\n")
  cat(" 抽報告的人請直接在命令列輸入report()\n")
  cat(" 抽問問題的人請直接在命令列輸入ask()\n")
  cat(" 查看或儲存抽籤結果歷史紀錄請直接在命令列輸入allresults()\n")
  cat("\n 輸入指令後請按照指示以及範例繼續進行抽籤!!\n")
  cat("\n 注意:輸入的組別只能是以大組為單位e.g. A, B, C, D\n")
  cat(" 注意:輸入兩組以上的組別請以空格或逗號隔開\n")

}


