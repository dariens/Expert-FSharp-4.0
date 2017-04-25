#r "Packages/Deedle/Deedle.dll"

open Deedle

let scriptLocation = __SOURCE_DIRECTORY__
let dataPath = scriptLocation + @"\Data"

let startDate = System.DateTime(2016, 1,1)
let endDate = System.DateTime(2016,12,31)

let joistSold =
    let joistSold = Deedle.Frame.ReadCsv(dataPath + @"\Fallon\Jobs Sold.csv")
    joistSold.ReplaceColumn("J. PO Date", joistSold.GetColumn<System.DateTime>("J. PO Date"))
    let isInDateRange = 
        joistSold.GetColumn("J. PO Date")
        |> Series.mapValues (fun date -> date >= startDate && date <= endDate)
    joistSold.AddColumn("IsInDateRange", isInDateRange)
    let joistSold = joistSold.FilterRowsBy("IsInDateRange", true)
    let joistSold = joistSold.FillMissing("BLANK")
    joistSold

let joistSoldPODateFilt =
    let joistSoldDate = joistSold.GetColumn<System.DateTime>("J. PO Date") 
    let joistSoldJobNumber = joistSold.GetColumn<string>("Job Number")   
    let frame = frame ["J. PO Date" => joistSoldDate]
    frame.AddColumn("Job Number", joistSoldJobNumber)
    frame


    




    

let quotedVsSold = Deedle.Frame.ReadCsv(dataPath + @"\Fallon\Quoted VS Sold.csv")

