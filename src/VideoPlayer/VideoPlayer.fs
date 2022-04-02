namespace VideoPlayer

open System
open LibVLCSharp.Shared
open Avalonia.FuncUI
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout

module VideoPlayer =
    let view =
        Component.create("VideoPlayer",fun ctx ->
            let libVLC = new LibVLC(true)
            let mp = new MediaPlayer(libVLC)
            let media = new Media(libVLC, new Uri("http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4"))

            ctx.trackDisposable libVLC
            ctx.trackDisposable mp
            ctx.trackDisposable media

            DockPanel.create [
                DockPanel.verticalAlignment VerticalAlignment.Center
                DockPanel.horizontalAlignment HorizontalAlignment.Center
                DockPanel.children [
                    Button.create [
                        Button.width 64
                        Button.horizontalAlignment HorizontalAlignment.Center
                        Button.horizontalContentAlignment HorizontalAlignment.Center
                        Button.content "Play"
                        Button.onClick (fun _ -> mp.Play(media) |> ignore)
                        Button.dock Dock.Bottom
                    ]
                ]
            ]
        )