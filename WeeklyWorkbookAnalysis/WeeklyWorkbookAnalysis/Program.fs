﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open ReportAnalysis.DsmReportAnalysis
open ReportAnalysis.Deedleing
open System.IO

[<EntryPoint>]
let main argv = 
    //GetAllInfo ()
    //SendFeedbackToExcel ()
    //MyPlot ()
    //printfn "%s" "COMPLETE!"
    createReport (System.DateTime(2017, 1, 1)) System.DateTime.Now
    let s = System.Console.ReadLine()
    0 // return an integer exit code
