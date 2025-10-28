namespace Pips

open System
open System.Net
open System.Net.Http

open Microsoft.Azure.Functions.Worker
open Microsoft.Azure.Functions.Worker.Http

module Daily =

    let private client = new HttpClient()

    [<Function("Daily")>]
    let run ([<HttpTrigger(AuthorizationLevel.Anonymous, "get")>] req : HttpRequestData) =
        task {
            let dateValue = req.Query["date"]
            let date = DateOnly.Parse(dateValue)
            let dateStr = date.ToString("yyyy-MM-dd")
            let uri = $"https://www.nytimes.com/svc/pips/v1/{dateStr}.json"
            let! text = client.GetStringAsync(uri)

            let response = req.CreateResponse(HttpStatusCode.OK)
            response.Headers.Add("Content-Type", "application/json; charset=utf-8")
            do! response.WriteStringAsync(text)
            return response
        }
