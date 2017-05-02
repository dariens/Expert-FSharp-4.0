﻿namespace ReportAnalysis


module DsmReportAnalysis =
    //#r "U:/CODE/F#/WeeklyWorkbookAnalysis/packages/XPlot.Plotly.1.4.2/lib/net45/Xplot.Plotly.dll"
    //#r "Microsoft.Office.Interop.Excel.dll"
    open Microsoft.Office.Interop.Excel
    open System.IO
    open System.Runtime.InteropServices
    open XPlot.Plotly
    


    /// Get a FileInfo [] containing the reports in the 'Weekly Sales Report' folder
    
    type FeedbackLine = 
        {Tons : obj
         SellingPrice : obj
         FreightPrice : obj
         Adjustment: obj
         PricePerTonPlant : obj
         PricePerTonDel : obj
         Company: obj
         Miles: obj
         Customer: obj
         Comments: obj}

        static member Null =
            {Tons = null
             SellingPrice = null
             FreightPrice = null
             Adjustment = null
             PricePerTonPlant = null
             PricePerTonDel = null
             Company = null
             Miles = null
             Customer = null
             Comments = null}

        member x.IsNull =
            (x.Company = null &&
             x.Tons = null &&
             x.SellingPrice = null &&
             x.FreightPrice = null &&
             x.Adjustment = null &&
             x.Company = null &&
             x.Miles = null &&
             x.Customer = null &&
             x.Comments = null)
                        

    type JFeedback = 
        {Dsm : string
         WeekStart : System.DateTime
         JobNumber : string
         JobName : string
         FeedbackLines : FeedbackLine list}

    type InfoReturnTypes =
        | JoistFeedback of JFeedback list
    
    let feedbackToExcel (feedbackList : JFeedback list) =
        let app = new ApplicationClass(Visible = false)
        let workbook = app.Workbooks.Add()
        let worksheet = (workbook.Worksheets.[1] :?> Worksheet)
        let mutable row = 2
        for f in feedbackList do
            for line in f.FeedbackLines do
                let r = string row
                worksheet.Range("A" + r).Value2 <- f.JobName
                worksheet.Range("B" + r).Value2 <- f.JobNumber
                worksheet.Range("C" + r).Value2 <- (string line.Company).Trim()
                worksheet.Range("D" + r).Value2 <- line.Tons
                worksheet.Range("E" + r).Value2 <- line.SellingPrice
                worksheet.Range("F" + r).Value2 <- line.PricePerTonPlant
                worksheet.Range("G" + r).Value2 <- line.PricePerTonDel
                worksheet.Range("H" + r).Value2 <- f.Dsm
                worksheet.Range("I" + r).Value2 <- f.WeekStart.ToString("MM/dd/yyyy")
                row <- row + 1

        worksheet.Range("A1").Value2 <- "Job Name"
        worksheet.Range("B1").Value2 <- "Job Number"
        worksheet.Range("C1").Value2 <- "Company"
        worksheet.Range("D1").Value2 <- "Tons"
        worksheet.Range("E1").Value2 <- "Selling Price"
        worksheet.Range("F1").Value2 <- "$/Ton (Plant)"
        worksheet.Range("G1").Value2 <- "$/Ton (Del.)"
        worksheet.Range("H1").Value2 <- "DSM"
        worksheet.Range("I1").Value2 <- "Week Start"
        
        workbook.SaveAs(@"\\nmbsfaln-fs\sales\TOOLS\WEEKLY SALES REPORT\Reporting\Report" + "_" + System.DateTime.Now.ToString("MM_dd_yyy_HH_mm") + ".xlsx")
        Marshal.ReleaseComObject(worksheet) |> ignore
        workbook.Close()
        Marshal.ReleaseComObject(workbook) |> ignore
        app.Quit()
        Marshal.ReleaseComObject(app) |> ignore
        System.GC.Collect()

    let mutable (allFeedback : JFeedback list) = []
    let combinedFeedback (feedbackList : JFeedback list) =
        for f in feedbackList do
            allFeedback <- f :: allFeedback

    let handleInfoReturnTypes (info : InfoReturnTypes list) =
        let mutable result = []
        match info with
            | [] -> ()
            | JoistFeedback(fbList) :: tail -> combinedFeedback fbList

    


    let getInfoFromAllReports (reports : FileInfo []) (getInfo : (ApplicationClass -> string -> InfoReturnTypes list)) =
        let tempExcelApp = new Microsoft.Office.Interop.Excel.ApplicationClass(Visible = false)
        try
            for f in reports do
                if f.Name.Contains("~$") = false then
                    let reportInfo = getInfo tempExcelApp f.FullName
                    handleInfoReturnTypes reportInfo
        finally
            tempExcelApp.Quit()
            Marshal.ReleaseComObject(tempExcelApp) |> ignore
            System.GC.Collect()



    

        /// Function that pulls info from one of the reports. The 'pullInfoFunction' will determine what info is taken from the report 
    let pullInfoFromReport (pullInfoFunctions : (Workbook -> InfoReturnTypes) list) (tempExcelApp: Microsoft.Office.Interop.Excel.Application) (reportPath : string)  =
        let tempReportPath = System.IO.Path.GetTempFileName()
        File.Delete(tempReportPath)
        File.Copy(reportPath, tempReportPath)
        let workbook = tempExcelApp.Workbooks.Open(tempReportPath)
        let mutable allInfo = []
        for infoFunction in pullInfoFunctions do
            let infoPulledFromWorkbook = infoFunction(workbook)
            allInfo <- infoPulledFromWorkbook :: allInfo
        workbook.Close()
        Marshal.ReleaseComObject(workbook) |> ignore
        System.GC.Collect() |> ignore
        allInfo


    /// Get Joist FB Info
    let getJoistFeedback (worksheet : Worksheet ) =
        let dsm = string (worksheet.Range("G2").Value2)
        let weekStart = string (worksheet.Range("L2").Text)
        let fbRowStart = System.Convert.ToInt32(worksheet.Range("P4").Value2)
        let fbRowEnd = System.Convert.ToInt32(worksheet.Range("Q4").Value2)
        let feedback =
          let mutable row = fbRowStart
          [while row <= fbRowEnd do
              if worksheet.Range("A" + string row).Value2 = null then
                  row <- row + 1
              else
                let jobNumber = string (worksheet.Range("A" + string row).Value2)
                let jobName = string (worksheet.Range("B" + string row).Value2)
                let feedbackList = 
                    let mutable mutableFeedbackList = [] : FeedbackLine list
                    let mutable atNextJob = false
                    [while atNextJob = false && row <= fbRowEnd do
                        let feedbackLine = {Tons = worksheet.Range("C" + string row).Value2
                                            SellingPrice = worksheet.Range("D" + string row).Value2
                                            FreightPrice = worksheet.Range("E" + string row).Value2
                                            Adjustment = worksheet.Range("F" + string row).Value2
                                            PricePerTonPlant = worksheet.Range("G" + string row).Value2
                                            PricePerTonDel = worksheet.Range("H" + string row).Value2
                                            Company = worksheet.Range("I" + string row).Value2
                                            Miles = worksheet.Range("J" + string row).Value2
                                            Customer = worksheet.Range("K" + string row).Value2
                                            Comments = worksheet.Range("L" + string row).Value2}
                        row <- row + 1
                        if worksheet.Range("A" + string row).Value2 <> null then
                            atNextJob <- true 
                        else
                            if feedbackLine.IsNull = false then
                                yield feedbackLine ]

                let joistFeedback = {Dsm = dsm;
                                     JobNumber = jobNumber;
                                     JobName = jobName;
                                     WeekStart = System.DateTime.Parse weekStart;
                                     FeedbackLines = feedbackList}
                yield joistFeedback]
                
        feedback

    let getAllJoistFeedback (workbook : Workbook) =
        let returns =
            [ for s in workbook.Worksheets do
                let s = s :?> Worksheet
                let invalidSheets = ["Current"; "JOIST FEEDBACK"; "DECK FEEDBACK"]
                if (List.contains s.Name invalidSheets) = false then
                    yield getJoistFeedback s ]
        let jfeedbackList = List.concat returns
        JoistFeedback(jfeedbackList)   

    let pullInfoFunctions = [getAllJoistFeedback]

    let getAllInfo = pullInfoFromReport pullInfoFunctions : (ApplicationClass -> string -> InfoReturnTypes list )

    let dsmReports =
        let reportPath = @"\\nmbsfaln-fs\sales\tools\weekly sales report\"
        let reportDirectory = new DirectoryInfo(reportPath)
        let files = reportDirectory.GetFiles();
        files

    let singleDsmReport dsm =
        [| for s in dsmReports do
               if s.Name.Contains(dsm) then
                   yield s|]

    let getInfoFromBobsReport = getInfoFromAllReports (singleDsmReport "Stearns")
    let GetBobsInfo () = getInfoFromBobsReport getAllInfo
    //GetBobsInfo ()

    let getInfoFromAllDsmReports = getInfoFromAllReports dsmReports
    let GetAllInfo () = getInfoFromAllDsmReports getAllInfo
    //GetAllInfo ()

    let SendFeedbackToExcel () = feedbackToExcel allFeedback

module XPlotting =
    open XPlot.Plotly

    let MyPlot () =
        let trace1 =
            Scatter(
                x = [1; 2; 3; 4],
                y = [10; 15; 13; 17])

        let trace2 =
            Scatter(
                x = [2; 3; 4; 5],
                y = [16; 5; 11; 9])

        let plot =
            [trace1; trace2]
            |> Chart.Plot
            |> Chart.WithWidth 700
            |> Chart.WithHeight 500

        plot.Show()


module Deedleing =
    //#r "../packages/Deedle.1.2.5/lib/net40/Deedle.dll"

    open Deedle
    open System

    module Frame =
        let whereRowValuesInColMeetReq colIndex req frame =
            Frame.filterRowValues (fun (objSeries : ObjectSeries<'a>) ->
                    (req (objSeries.Get(colIndex)))) frame

        let whereRowValuesInColEqual colIndex acceptableValues frame =
            frame
            |> whereRowValuesInColMeetReq colIndex (fun obj -> Seq.contains obj acceptableValues)
            //Frame.filterRowValues (fun (objSeries : ObjectSeries<'a>) -> 
            //        (Seq.contains (objSeries.Get(colIndex)) acceptableValues)) frame

        let whereRowValuesInColDontEqual colIndex unacceptableValues frame =
            frame
            |> whereRowValuesInColMeetReq colIndex (fun obj -> (Seq.contains obj unacceptableValues) = false)

        let whereRowValuesInColAreInDateRange colIndex startDate endDate frame =
            frame
            |> whereRowValuesInColMeetReq colIndex (fun date ->
                                                        let date = System.DateTime.Parse(string date)
                                                        date >= startDate && date <= endDate)

        let removeDuplicateRows index (df : Frame<'a, 'b>) =
          let unique = Seq.distinctBy (fun (a, b) -> a) (df.GroupRowsBy(index).RowKeys)
          let nonDupKeys = 
              [for tup in unique do
                  let value, key = tup
                  yield key]
          df.Rows.[nonDupKeys]
        
        
        let merge_On (infoFrame : Frame<'c, 'b>) index missingReplacement (initialFrame : Frame<'a,'b>) =
              let newFrame = initialFrame.Clone()
              let newInfoFrame = infoFrame.Clone()
                               |> removeDuplicateRows index 
                               |> Frame.indexRowsString index
              let initialSeries = newFrame.GetColumn(index)
              for colKey in newInfoFrame.ColumnKeys do
                  let newSeries =
                      [for f in initialSeries.ValuesAll do
                            if Seq.contains f newInfoFrame.RowKeys then  
                                let key = newInfoFrame.GetRow(f)
                                let newValue = key.[colKey]
                                yield newValue
                            else
                                let newValue = box missingReplacement
                                yield newValue]
                  newFrame.AddColumn(colKey, newSeries)
              newFrame

        

        let sumColumnsBy colIndex (frame: Frame<'a, 'b>) =
                let distinctValues = frame.GetColumn(colIndex).Values |> Seq.distinct
                let mutable row = -1
                let seriesList =
                  [for distinctValue in distinctValues do
                    row <- row + 1
                    let sumOfDistinctValue = frame
                                             |> whereRowValuesInColEqual colIndex [distinctValue]
                                             |> Frame.dropCol colIndex
                                             |> Stats.sum
                                             
                    for key in sumOfDistinctValue.Keys do                   
                         yield (box row, key, sumOfDistinctValue.[key])]
                let newFrame = seriesList |> Frame.ofValues
                newFrame.AddColumn(colIndex, distinctValues)
                newFrame
                


        let sumSpecificColumnsBy byColumn (wantedColumns) (frame: Frame<'a, 'b>) =
            let allColumns = byColumn :: wantedColumns
            let newFrame = frame.Columns.[allColumns]
            let summedFrame = newFrame |> sumColumnsBy byColumn
            summedFrame

        let getSpecificColumns (columns: 'b list) (frame: Frame<'a, 'b>) =
            frame.Columns.[columns]

        let renameColumn initialName finalName (frame: Frame<'a, 'b>) =
            frame.RenameColumn(initialName, finalName)
            frame

    let createDsmReport startDate endDate dsm =
        //let startDate = System.DateTime(2017,1,1)
        //let endDate = System.DateTime.Now
        //#r "../packages/Deedle.1.2.5/lib/net40/Deedle.dll"
        //Environment.CurrentDirectory <- @"U:\CODE\F#\Expert-FSharp-4.0\WeeklyWorkbookAnalysis\WeeklyWorkbookAnalysis\bin\Debug"
        
        let quotedVsSold = 
            match dsm with
            | "" -> Frame.ReadCsv(@"Data\Quoted Vs Sold.csv")
            | dsm -> Frame.ReadCsv(@"Data\Quoted Vs Sold.csv")
                     |> Frame.whereRowValuesInColEqual "Quote DSM" [dsm]

        let jobsSold = Frame.ReadCsv(@"Data\Jobs Sold.csv")
        let jobsQuoted = Frame.ReadCsv(@"Data\Jobs Quoted.csv")               
    
        let filteredJobsQuoted = 
            jobsQuoted
            |> Frame.whereRowValuesInColAreInDateRange "Date Quoted" startDate endDate
            |> Frame.getSpecificColumns ["Job Number"; "Total Tons"]
            |> Frame.renameColumn "Total Tons" "Quoted Tons (From 'Jobs Quoted')"
             
        let mergedQuotedVsSoldWithJobsQuoted = 
            quotedVsSold
            |> Frame.merge_On filteredJobsQuoted "Job Number" 0.0
            |> Frame.getSpecificColumns ["Job Number"; "Customer"; "J.Tons(Base)";"Quoted Tons (From 'Jobs Quoted')"]
            |> Frame.renameColumn "J.Tons(Base)" "Quoted Tons (From 'Quoted Vs. Sold')"
            |> Frame.aggregateRowsBy 
                (seq ["Customer"])
                (seq ["Quoted Tons (From 'Jobs Quoted')";"Quoted Tons (From 'Quoted Vs. Sold')"])
                Stats.sum

        let soldByCustomer = 
            jobsSold
            |> Frame.whereRowValuesInColAreInDateRange "J. PO Date" startDate endDate
            |> Frame.aggregateRowsBy (seq ["Customer"]) (seq ["Total Tons"]) Stats.sum
            |> Frame.renameColumn "Total Tons" "Sold Tons"

     
        let soldJobNumbers = jobsSold.GetColumn<obj>("Job Number").ValuesAll
        
        let jobsQuotedAndSold = 
            quotedVsSold
            |> Frame.whereRowValuesInColDontEqual "Job Number" soldJobNumbers
            |> Frame.getSpecificColumns ["Customer"; "J.Tons(Base)"]
            |> Frame.renameColumn "J.Tons(Base)" "Quoted Tons That We Sold (From 'Quoted Vs. Sold')"
            |> Frame.aggregateRowsBy
                (seq ["Customer"])
                (quotedVsSold.Columns.Keys)
                Stats.sum

        let filteredJobsQuotedAndSold = 
             jobsQuoted
             |> Frame.whereRowValuesInColAreInDateRange "Date Quoted" startDate endDate
             |> Frame.whereRowValuesInColEqual "Job Number" soldJobNumbers
             |> Frame.renameColumn "Total Tons" "Quoted Tons That We Sold (From 'Jobs Quoted')"
             |> Frame.getSpecificColumns ["Job Number"; "Quoted Tons That We Sold (From 'Jobs Quoted')"]
             
         
        let mergedQuotedVsSoldWithJobsQuoted3 =
            quotedVsSold
            |> Frame.merge_On filteredJobsQuotedAndSold "Job Number" 0.0
            |> Frame.getSpecificColumns ["Job Number"; "Customer"; "J.Tons(Base)";"Quoted Tons That We Sold (From 'Jobs Quoted')"]
            |> Frame.aggregateRowsBy
                (seq ["Customer"])
                (seq ["Quoted Tons That We Sold (From 'Jobs Quoted')"])
                Stats.sum


        let customerAnalysis =
            let newFrame =
                mergedQuotedVsSoldWithJobsQuoted
                |> Frame.merge_On soldByCustomer "Customer" 0.0
                |> Frame.merge_On jobsQuotedAndSold "Customer" 0.0
                |> Frame.merge_On mergedQuotedVsSoldWithJobsQuoted3 "Customer" 0.0

            newFrame.AddColumn("Quoted Tons That We Sold To Others (From 'Jobs Quoted')",
                seq [ for v in newFrame.RowKeys do
                        let quoted = newFrame.Rows.[v].["Quoted Tons That We Sold (From 'Jobs Quoted')"]
                        let sold = newFrame.Rows.[v].["Sold Tons"]
                        yield (quoted :?> float) - (sold :?> float)])

            newFrame.AddColumn("Quoted Tons That We Sold To Others (From 'Quoted Vs. Sold')",
                seq [ for v in newFrame.RowKeys do
                        let quoted = newFrame.Rows.[v].["Quoted Tons That We Sold (From 'Quoted Vs. Sold')"]
                        let sold = newFrame.Rows.[v].["Sold Tons"]
                        yield (quoted :?> float) - (sold :?> float)])

            newFrame.AddColumn("Sold Percentage (From 'Jobs Quoted')",
                seq [ for v in newFrame.RowKeys do
                        let quoted = newFrame.Rows.[v].["Quoted Tons (From 'Jobs Quoted')"]
                        let sold = newFrame.Rows.[v].["Sold Tons"]
                        if (quoted :?> float) <> 0.0 then
                            yield (sold :?> float) / (quoted :?> float)
                        else
                            yield 0.0])

            newFrame.Columns.[["Customer";
                               "Sold Tons";
                               "Quoted Tons (From 'Jobs Quoted')";
                               //"Quoted Tons That We Sold To Others (From 'Jobs Quoted')";
                               //"Quoted Tons That We Sold (From 'Jobs Quoted')";
                               "Quoted Tons (From 'Quoted Vs. Sold')";
                               "Sold Percentage (From 'Jobs Quoted')"]]
                               //"Quoted Tons That We Sold To Others (From 'Quoted Vs. Sold')";
                               //"Quoted Tons That We Sold (From 'Quoted Vs. Sold')"]]
        System.IO.Directory.CreateDirectory(@"Output\Temp") |> ignore
        customerAnalysis.SaveCsv( @"Output\Temp\Customer Analysis_" + dsm + ".csv")

        printfn "%A" ("Finished Report For " + dsm)

    let createDsmReports startDate endDate dsms =
        for dsm in dsms do
            createDsmReport startDate endDate dsm
        printfn "%A" "All Finshed!"
 
    let createEstimatorReport () =
        //#r "../packages/Deedle.1.2.5/lib/net40/Deedle.dll"
        //System.Environment.CurrentDirectory <- @"U:\CODE\F#\Expert-FSharp-4.0\WeeklyWorkbookAnalysis\WeeklyWorkbookAnalysis\bin\Debug"
        //open Deedle
        //open System

        let jobsSold = Frame.ReadCsv(@"Data\Jobs Sold.csv")
        let jobsQuoted = Frame.ReadCsv(@"Data\Jobs Quoted.csv")
        let calander = new System.Globalization.GregorianCalendar()
        let estimatorAnalysis () =
            let quotedJobsCount =
                jobsQuoted
                |> Frame.addCol "Week"
                    (jobsQuoted.GetColumn("Date Quoted")
                     |> Series.mapValues (fun value -> 
                                             System.Globalization.GregorianCalendar().GetWeekOfYear(
                                              System.DateTime.Parse(value),
                                              System.Globalization.CalendarWeekRule.FirstDay,
                                              System.DayOfWeek.Sunday)))
                |> Frame.addCol "Year"
                    (jobsQuoted.GetColumn("Date Quoted")
                    |> Series.mapValues (fun value -> System.DateTime.Parse(value).Year))
                |> Frame.aggregateRowsBy (seq ["TakeoffPerson"; "Year"; "Week"]) (seq ["Job Number"]) Stats.count
                |> Frame.sortRows "TakeoffPerson"
                |> Frame.renameColumn "Job Number" "Values"
                |> Frame.addCol "Report Type"
                    ((seq [for row in [0..jobsQuoted.Rows.KeyCount-1] do yield "Jobs Quoted Count"])
                     |> Series.ofValues)
           
            let quotedTons =
                jobsQuoted
                |> Frame.addCol "Week"
                    (jobsQuoted.GetColumn("Date Quoted")
                     |> Series.mapValues (fun value -> 
                                             System.Globalization.GregorianCalendar().GetWeekOfYear(
                                              System.DateTime.Parse(value),
                                              System.Globalization.CalendarWeekRule.FirstDay,
                                              System.DayOfWeek.Sunday)))
                |> Frame.addCol "Year"
                    (jobsQuoted.GetColumn("Date Quoted")
                    |> Series.mapValues (fun value -> System.DateTime.Parse(value).Year))
                |> Frame.aggregateRowsBy (seq ["TakeoffPerson"; "Year"; "Week"]) (seq ["Total Tons"]) Stats.sum
                |> Frame.sortRows "TakeoffPerson"
                |> Frame.renameColumn "Total Tons" "Values"
                |> Frame.addCol "Report Type"
                    ((seq [for row in [0..jobsQuoted.Rows.KeyCount-1] do yield "Tons Quoted"])
                     |> Series.ofValues)

            let soldJobCount =
                jobsSold
                |> Frame.addCol "Week"
                    (jobsSold.GetColumn("J. PO Date")
                     |> Series.mapValues (fun value -> 
                                             System.Globalization.GregorianCalendar().GetWeekOfYear(
                                              System.DateTime.Parse(value),
                                              System.Globalization.CalendarWeekRule.FirstDay,
                                              System.DayOfWeek.Sunday)))
                |> Frame.addCol "Year"
                    (jobsSold.GetColumn("J. PO Date")
                    |> Series.mapValues (fun value -> System.DateTime.Parse(value).Year))
                |> Frame.aggregateRowsBy (seq ["takeoffperson"; "Year"; "Week"]) (seq ["Job Number"]) Stats.count
                |> Frame.sortRows "takeoffperson"
                |> Frame.renameColumn "Job Number" "Values"
                |> Frame.renameColumn "takeoffperson" "TakeoffPerson"
                |> Frame.addCol "Report Type"
                    ((seq [for row in [0..jobsSold.Rows.KeyCount-1] do yield "Jobs Sold Count"])
                     |> Series.ofValues)
           
            let soldTons =
                jobsSold
                |> Frame.addCol "Week"
                    (jobsSold.GetColumn("J. PO Date")
                     |> Series.mapValues (fun value -> 
                                             System.Globalization.GregorianCalendar().GetWeekOfYear(
                                              System.DateTime.Parse(value),
                                              System.Globalization.CalendarWeekRule.FirstDay,
                                              System.DayOfWeek.Sunday)))
                |> Frame.addCol "Year"
                    (jobsSold.GetColumn("J. PO Date")
                    |> Series.mapValues (fun value -> System.DateTime.Parse(value).Year))
                |> Frame.aggregateRowsBy (seq ["takeoffperson"; "Year"; "Week"]) (seq ["Total Tons"]) Stats.sum
                |> Frame.sortRows "takeoffperson"
                |> Frame.renameColumn "Sold Tons" "Values"
                |> Frame.renameColumn "takeoffperson" "TakeoffPerson"
                |> Frame.addCol "Report Type"
                    ((seq [for row in [0..jobsSold.Rows.KeyCount-1] do yield "Sold Tons"])
                     |> Series.ofValues)



            quotedJobsCount.SaveCsv(@"Output\Estimator Takeoff Count.csv")
            quotedTons.SaveCsv(@"Output\Estimator Total Tons.csv")
            soldJobCount.SaveCsv(@"Output\Estimator Sold Job Count.csv")
            soldTons.SaveCsv(@"Output\Estimator Sold Tons.csv")

            
            let createFileOfAllLine (files : string list) (outputPath : string) =
                let mutable allLines = ""
                for file in files do
                    use sr = new System.IO.StreamReader(file)
                    let lines = sr.ReadToEnd()
                    allLines <- allLines + lines
                use file = new System.IO.StreamWriter(outputPath)
                file.Write allLines
            
            createFileOfAllLine ([@"Output\Estimator Takeoff Count.csv";
                                   @"Output\Estimator Total Tons.csv";
                                   @"Output\Estimator Sold Job Count.csv";
                                   @"Output\Estimator Sold Tons.csv"]) @"Output\Estimator Analysis.csv"            
        
        estimatorAnalysis ()
        printfn "%s" "All Finished"


    let test () =
        let data =
                [(0, "Name", box "Darien");
                 (0, "Col 2", box 30);
                 (0, "Col 3", box 40);
                 (1, "Name", box "Niko");
                 (1, "Col 2", box 6);
                 (1, "Col 3", box 7);
                 (2, "Name", box "Nedelee");
                 (2, "Col 2", box 150);
                 (2, "Col 3", box 160);
                 (3, "Name", box "Darien");
                 (3, "Col 2", box 30);
                 (3, "Col 3", box 40);
                 (4, "Name", box "Niko");
                 (4, "Col 2", box 6);
                 (4, "Col 3", box 7);
                 (5, "Name", box "Nedelee");
                 (5, "Col 2", box 150);
                 (5, "Col 3", box 160)] |> Frame.ofValues |> Frame.sumSpecificColumnsBy "Name" ["Col 2"] 
        
        printfn "%A" (data.Format())

         