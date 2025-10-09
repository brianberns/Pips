namespace Pips

open BenchmarkDotNet.Running

module Program =

    [<EntryPoint>]
    let main _ =
        BenchmarkSwitcher.FromAssembly(
            typeof<Benchmarks>.Assembly).Run()
            |> ignore
        0
