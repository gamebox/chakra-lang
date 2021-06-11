module RunProcess

open System
open System.Diagnostics

let runProc filename args = 
    let timer = Stopwatch.StartNew()
    let procStartInfo = 
        ProcessStartInfo(
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            FileName = filename,
            Arguments = args,
            WorkingDirectory = System.IO.Directory.GetCurrentDirectory ()
        )

    let outputs = Collections.Generic.List<string>()
    let errors = Collections.Generic.List<string>()
    let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) =
        f args.Data

    let p = new Process(StartInfo = procStartInfo)
    p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler outputs.Add))
    p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler errors.Add))
    let started = 
        try
            printfn "[%s]: %s %s" p.StartInfo.WorkingDirectory p.StartInfo.FileName p.StartInfo.Arguments
            p.Start()
        with | ex ->
            ex.Data.Add("filename", filename)
            reraise()
    if not started then
        failwithf "Failed to start process %s" filename
    printfn "Started %s with pid %i" p.ProcessName p.Id
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()
    timer.Stop()
    let errors =
        if p.ExitCode <> 0 then
            Seq.filter (fun o -> String.IsNullOrEmpty o |> not) errors
        else Seq.empty

    timer.ElapsedMilliseconds, outputs, errors