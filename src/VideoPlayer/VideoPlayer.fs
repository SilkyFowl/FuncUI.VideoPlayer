namespace VideoPlayer

open System
open LibVLCSharp.Shared

open System.Reactive.Linq
open System.Reactive.Subjects
open System.Reactive.Disposables

open Avalonia
open Avalonia.Data
open Avalonia.Platform

open Avalonia.FuncUI
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.Types


[<AutoOpen>]
module Helper =
    let inline tap f x =
        f x
        x

module LibVLCSharp =
    let libVLC = lazy new LibVLC(true)

module MediaPlayer =
    open LibVLCSharp
    let create () = new MediaPlayer(libVLC.Value)

    let inline attachHandle (mediaPlayer: MediaPlayer) (platformHandle: IPlatformHandle) =
        match Environment.OSVersion.Platform with
        | PlatformID.Win32NT -> mediaPlayer.Hwnd <- platformHandle.Handle
        | PlatformID.MacOSX -> mediaPlayer.XWindow <- uint platformHandle.Handle
        | PlatformID.Unix -> mediaPlayer.NsObject <- platformHandle.Handle
        | _ -> ()

    let inline detachHandle (mediaPlayer: MediaPlayer) =
        match Environment.OSVersion.Platform with
        | PlatformID.Win32NT -> mediaPlayer.Hwnd <- IntPtr.Zero
        | PlatformID.MacOSX -> mediaPlayer.XWindow <- 0u
        | PlatformID.Unix -> mediaPlayer.NsObject <- IntPtr.Zero
        | _ -> ()

module Media =
    open LibVLCSharp
    let create uri = new Media(libVLC.Value, uri = uri)

type VideoView() =
    inherit NativeControlHost()

    let mediaPlayerSub = new BehaviorSubject<MediaPlayer option>(None)
    let platformHandleSub = new BehaviorSubject<IPlatformHandle option>(None)

    let attacher =
        mediaPlayerSub.CombineLatest platformHandleSub
        |> Observable.subscribe (function
            | Some mp, Some p -> MediaPlayer.attachHandle mp p
            | _ -> ())


    member x.MediaPlayer
        with get () =
            if mediaPlayerSub.IsDisposed then
                None
            else
                mediaPlayerSub.Value
        and set value = mediaPlayerSub.OnNext value

    static member MediaPlayerProperty =
        AvaloniaProperty.RegisterDirect<VideoView, MediaPlayer option>(
            nameof MediaPlayer,
            (fun o -> o.MediaPlayer),
            (fun o v -> o.MediaPlayer <- v),
            defaultBindingMode = BindingMode.TwoWay
        )

    override _.CreateNativeControlCore(parent) =
        base.CreateNativeControlCore parent
        |> tap (Some >> platformHandleSub.OnNext)

    override _.DestroyNativeControlCore(control) =
        attacher.Dispose()

        Option.iter MediaPlayer.detachHandle mediaPlayerSub.Value
        platformHandleSub.OnNext None

        base.DestroyNativeControlCore control

module VideoView =
    let create (attrs: IAttr<VideoView> list) : IView<VideoView> = ViewBuilder.Create<VideoView>(attrs)

type VideoView with
    static member mediaPlayer<'t when 't :> VideoView> value =
        AttrBuilder<'t>
            .CreateProperty<_>(VideoView.MediaPlayerProperty, value, ValueNone)


module VideoPlayer =
    let viewByVideoView =
        Component.create (
            "VideoPlayer-VideoView",
            fun ctx ->

                let mp = MediaPlayer.create () |> tap ctx.trackDisposable

                let media =
                    Uri "http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4"
                    |> Media.create
                    |> tap ctx.trackDisposable

                DockPanel.create [
                    DockPanel.verticalAlignment VerticalAlignment.Stretch
                    DockPanel.horizontalAlignment HorizontalAlignment.Stretch
                    DockPanel.children [
                        Button.create [
                            Button.width 64
                            Button.horizontalAlignment HorizontalAlignment.Center
                            Button.horizontalContentAlignment HorizontalAlignment.Center
                            Button.content "Play"
                            Button.onClick (fun _ -> mp.Play(media) |> ignore)
                            Button.dock Dock.Bottom
                        ]

                        TextBlock.create [
                            TextBlock.dock Dock.Bottom
                            TextBlock.text $"Hwnd: {mp.Hwnd}"
                        ]

                        VideoView.create [
                            VideoView.verticalAlignment VerticalAlignment.Stretch
                            VideoView.horizontalAlignment HorizontalAlignment.Stretch
                            VideoView.mediaPlayer (Some mp)
                        ]
                    ]
                ]
        )