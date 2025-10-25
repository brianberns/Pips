open Microsoft.Extensions.Hosting
open Microsoft.Azure.Functions.Worker

module Program =
    [<EntryPoint>]
    let main args =
        let host =
            HostBuilder()
                .ConfigureFunctionsWorkerDefaults()
                .Build()
        host.Run()
        0
