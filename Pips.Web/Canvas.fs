namespace Pips.Web

open Browser
open Browser.Types

open Fable.Core

type Context = CanvasRenderingContext2D

module Canvas =

    /// Clears the given canvas.
    let clear (ctx : Context) =
        ctx.clearRect(
            0.0, 0.0,
            ctx.canvas.width, ctx.canvas.height)

    /// Current animation frame ID.
    let mutable private animationId = 0.0

    /// Cancels the current animation. 
    let cancelAnimation () =
        window.cancelAnimationFrame(animationId)
        animationId <- 0.0

    /// Calls the given function in a loop to animate frames.
    let animate fps callback =

        cancelAnimation ()

        let interval = 1000.0 / fps
        let mutable lastTime = 0.0

        let rec loop iFrame timestamp =
            let delta = timestamp - lastTime
            if delta >= interval then
                requestFrame (iFrame + 1)
                lastTime <- timestamp - (delta % interval)
                callback iFrame
            else
                requestFrame iFrame

        and requestFrame iFrame =
            animationId <-
                window.requestAnimationFrame(loop iFrame)

        requestFrame 0

[<AutoOpen>]
module CanvasExtensions =

    type CanvasRenderingContext2D with

        /// Round rectangle.
        [<Emit("($0).roundRect($1, $2, $3, $4, $5)")>]
        member _.roundRect(
            x : float, y : float,
            width : float, height: float,
            radius : float) : unit =
            failwith "JS interop"
