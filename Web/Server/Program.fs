namespace Pips.Web

open Microsoft.Extensions.Hosting

module Program =
    [<EntryPoint>]
    let main args =
        let host =
            HostBuilder()
                .ConfigureFunctionsWorkerDefaults()
                .Build()
        host.Run()
        0
