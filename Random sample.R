
for(i in 1:1){
  readfile = function(path = filepath){
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
      return(AA[sample(1:nrow(AA), 1), c("組別","學號","姓名")])
    }
    choose = lapply(base, function(x) select(x))
    result = c()
    for(i in 1:length(choose)){
      result = rbind(result, choose[[i]])
    }
    cat("\n\n", paste(enter,collapse = ","), "大組的報告幸運兒:\n\n" )
    return(result)
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
    base = base[(sample(1:nrow(base), nrow(base), replace = FALSE)), c("組別","學號","姓名")]
    choose = base[sample(1:nrow(base), 10, replace = FALSE), c("組別","學號","姓名")]
    choose = choose[order(choose$組別),]
    cat("\n\n", paste(enter,collapse = ","), "大組問問題的幸運兒:\n\n")
    return(choose)
  }
  
  cat("\n Ready!!\n")
  cat("\n 請按照下列指示以及範例格式匯入名單資料\n")
  filepath = readline(prompt = "請輸入名單的檔案路徑(e.g. C:/Users/.../file.csv): ")
  readfile()
  cat("\n Data Setup Ready!!\n")
  cat("\n 抽報告的人請直接在命令列輸入report()\n")
  cat(" 抽問問題的人請直接在命令列輸入ask()\n")
  cat("\n 輸入指令後請按照指示以及範例繼續進行抽籤!!\n")
  cat("\n 注意:輸入的組別只能是以大組為單位e.g. A, B, C, D\n")
  cat(" 注意:輸入兩組以上的組別請以空格或逗號隔開\n")

}




