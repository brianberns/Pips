namespace Pips

open System

open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc
open Microsoft.Azure.WebJobs
open Microsoft.Azure.WebJobs.Extensions.Http

module Daily =

    [<FunctionName("Daily")>]
    let run (
        [<HttpTrigger(AuthorizationLevel.Function, "get")>]
        req : HttpRequest) =
        task {
            let date = DateOnly.Parse(req.Query["date"][0])
            let dateStr = date.ToString("yyyy-MM-dd")
            let uri = $"https://www.nytimes.com/svc/pips/v1/{dateStr}.json"
            let! puzzleMap = Daily.loadHttpAsync uri
            return OkObjectResult(puzzleMap) :> IActionResult
        }
