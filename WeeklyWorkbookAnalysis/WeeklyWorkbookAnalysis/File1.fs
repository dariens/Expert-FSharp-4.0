namespace ReportAnalysis


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

    module Frame =
        let whereRowValuesInColMeetReq colIndex req frame =
            Frame.filterRowValues (fun (objSeries : ObjectSeries<'a>) ->
                    (req (objSeries.Get(colIndex)))) frame

        let whereRowValuesInColEqual colIndex acceptableValues frame =
            frame
            |> whereRowValuesInColMeetReq colIndex (fun obj -> Seq.contains obj acceptableValues)
            //Frame.filterRowValues (fun (objSeries : ObjectSeries<'a>) -> 
            //        (Seq.contains (objSeries.Get(colIndex)) acceptableValues)) frame

        let removeDuplicateRows index (df : Frame<'a, 'b>) =
          let unique = Seq.distinctBy (fun (a, b) -> a) (df.GroupRowsBy(index).RowKeys)
          let nonDupKeys = 
              [for tup in unique do
                  let value, key = tup
                  yield key]
          df.Rows.[nonDupKeys]

        let merge_On (initialFrame : Frame<'a,'b>) (infoFrame : Frame<'c, 'b>) index =
              let newFrame = initialFrame.Clone()
              let newInfoFrame = infoFrame.Clone()
                               |> removeDuplicateRows index 
                               |> Frame.indexRowsString index
              let initialSeries = newFrame.GetColumn(index)
              for colKey in newInfoFrame.ColumnKeys do
                  let newSeries =
                      [for f in initialSeries.ValuesAll do
                            let key = newInfoFrame.GetRow(f)
                            let newValue = key.[colKey]
                            yield newValue]
                  newFrame.AddColumn(colKey, newSeries)
              newFrame 
 

    //let myFrame = Frame.ReadCsv("U:\CODE\F#\Expert-FSharp-4.0\SalesByCustomer\Data\Fallon\Jobs Sold.csv")
    let myFrame = Frame.ReadCsv("C:\Users\user\Documents\CODE\F#\Expert F# 4.0\SalesByCustomer\Data\Fallon\Jobs Sold.csv")

    let newFrame = myFrame
                       |> Frame.whereRowValuesInColEqual "takeoffperson" ["Darien Shannon"]
                      // |> Frame.whereRowValuesInColEqual "salesperson" ["Chris Cline"]
                      // |> Frame.whereRowValuesInColMeetReq "Total Tons" (fun x -> System.Convert.ToDouble(x) > 40.0)
    let newFrame2 = myFrame.GroupRowsBy("takeoffperson") :Frame<(string*_), _>
    newFrame2.Format()
    newFrame2.RowKeys

    newFrame.Columns.[["Job Name";"Total Tons";"Profit"]].Sum().Format()
    newFrame.Format()





