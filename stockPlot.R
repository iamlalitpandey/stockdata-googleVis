
library(RMySQL)
library(quantmod)
library(googleVis)

getinfo <- function()
    {
    usr <- readline(prompt="Enter your username to connect: ")
    pwd <- readline(prompt="Enter your password : ")
    quotes <- readline(prompt="Enter your stock quote to be fetched : ")
    return (list(usr, pwd,quotes))
    
    } 


info<-getinfo()
drv <- dbDriver("MySQL")
con <- dbConnect(drv, user =info[[1]], password= info[[2]], host = "Your Host name", dbname="Your DB NAME")
quotes<-info[[3]]
getSymbols(Symbols = info[[3]], src = "yahoo" ,from=Sys.Date()-30,to=Sys.Date())
stock_data=(get(quotes))

dbWriteTable(con,paste("f513_",quotes,sep=""),data.frame(stock_data))
#dbClearResult(res1)
df=data.frame(dbReadTable(con, paste("f513_",quotes,sep="")))
initial_asset=1000000
current_asset=1000000
share_quantity=ceiling(initial_asset/df[1,2])
share_quantity
df["action"]<-NA
df["current assets"]<-NA
df["holding share"]<-NA
df

for (i in (1:nrow(df)))
{
    if((current_asset==initial_asset) ||(current_asset>initial_asset*1.02))
    {
        df[i,7]="BUY"
        share_quantity=ceiling(current_asset/df[i,1])
        df[i,9]= share_quantity
        df[i,8]= share_quantity*df[i,4] 
        current_asset=share_quantity*df[i,4]
        
    }
    else if((current_asset<(initial_asset*0.98)))
    {   
        pre_share=share_quantity
        share_quantity=ceiling(share_quantity-(share_quantity*0.1))
        df[i,9]=share_quantity
        df[i,8]=share_quantity*df[i,4]+(pre_share*0.1*df[1,4])
        current_asset=share_quantity*df[i,4]+(pre_share*0.1*df[1,4])
        df[i,7]="SELL"
    }
    else
    {
        df[i,7]="HOLD"
        df[i,9]= share_quantity
        df[i,8]= share_quantity*df[i,4]
        current_asset=share_quantity*df[i,4]
    }
}

df$Dates <- rownames(df) 
rownames(df)<-NULL
names(df)[1] <- "open_price"
names(df)[4] <- "close_price"

df
cmb <- 
    gvisComboChart(df, xvar="Dates", yvar=c("open_price","close_price","current assets"),
                   options=list(title="Daily stock trends - Open/Close price Vs Assets",
                                titleTextStyle="{color:'blue',fontName:'Courier',fontSize:16}",
                                curveType="function", 
                                pointSize=9,
                                seriesType="bars",
                                series="[{type:'bars',targetAxisIndex:0,color:'gray'},
                                        {type:'bars',targetAxisIndex:0,color:'orange'},
                                        {type:'line',targetAxisIndex:1,color:'black'}, ]",
                                vAxes="[{title:'In US dollors',titleTextStyle: {color: 'black'},textStyle:{color: 'black'},textPosition: 'out',minValue:5}, 
                                        {title:'Current assets (US dollors) ',format:'#,###',titleTextStyle: {color: 'grey'},textStyle:{color: 'grey'},textPosition: 'out'}]",
                                hAxes="[{title:'Dates',textPosition: 'out'}]",
                                width=1500, height=500
                   ), 
                   chartid="twoaxiscombochart"
    )
plot(cmb)
df
