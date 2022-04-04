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

module Observable =
    let mergeSeq (sources: IObservable<_> seq) = Observable.Merge sources

module LibVLCSharp =
    let libVLC = lazy new LibVLC(true)

module MediaPlayer =
    open LibVLCSharp
    let create () = new MediaPlayer(libVLC.Value)

    let inline attachHandle (platformHandle: IPlatformHandle) (mediaPlayer: MediaPlayer) =
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
        platformHandleSub.CombineLatest mediaPlayerSub
        |> Observable.subscribe (function
            | Some p, Some mp -> MediaPlayer.attachHandle p mp
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

module LibVLCSharpComponent =

    let seekBar id (player: MediaPlayer) writablePosition onPositionChanged attrs =
        Component.create (
            id,
            fun ctx ->
                let minValue = 0.0
                let maxValue = 1.0

                let outlet = ctx.useState (Unchecked.defaultof<Slider>, false)
                let isPressed = ctx.useState (false, false)
                let position = ctx.usePassed writablePosition

                let handler _ : unit = position.Current |> onPositionChanged

                ctx.useEffect (handler, [ EffectTrigger.AfterChange position ])

                ctx.useEffect (
                    (fun _ ->
                        [ player.Opening
                          |> Observable.merge player.Playing
                          |> Observable.merge player.Paused
                          |> Observable.merge player.EndReached
                          |> Observable.merge player.Stopped
                          |> Observable.map (fun _ -> float player.Position)

                          player.PositionChanged
                          |> Observable.map (fun e ->
                              if player.State = VLCState.Ended then
                                  maxValue
                              else
                                  float e.Position |> max minValue) ]
                        |> Observable.mergeSeq
                        |> Observable.subscribe position.Set),
                    [ EffectTrigger.AfterInit ]
                )

                ctx.attrs attrs

                View.createWithOutlet
                    outlet.Set
                    Slider.create
                    [ Slider.minimum minValue
                      Slider.maximum maxValue

                      if player.IsSeekable then
                          double position.Current
                      else
                          minValue
                      |> Slider.value

                      Slider.onPointerPressed (fun _ -> isPressed.Set true)
                      Slider.onPointerReleased (fun _ ->
                          if player.IsSeekable then
                              player.Position <- float32 outlet.Current.Value
                              onPositionChanged outlet.Current.Value
                          else
                              outlet.Current.Value <- minValue

                          isPressed.Set false) ]
        )


module VideoPlayer =
    let viewByVideoView =
        Component.create (
            "VideoPlayer-VideoView",
            fun ctx ->

                let mp = MediaPlayer.create () |> tap ctx.trackDisposable

                let position = ctx.useState 0.0

                let path =
                    ctx.useState (
                        "http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4",
                        false
                    )

                let errors = ctx.useState ""

                let validate text =
                    backgroundTask {
                        if String.IsNullOrEmpty path.Current then
                            return Error "NullOrEmpty"
                        else
                            let media = Uri text |> Media.create


                            let opt =
                                MediaParseOptions.ParseLocal
                                ||| MediaParseOptions.ParseNetwork

                            match! media.Parse(opt, timeout = 3000) with
                            | MediaParsedStatus.Done -> return Ok media
                            | other -> return Error $"Parse {other}"
                    }

                DockPanel.create [
                    DockPanel.verticalAlignment VerticalAlignment.Stretch
                    DockPanel.horizontalAlignment HorizontalAlignment.Stretch
                    DockPanel.children [
                        Grid.create [
                            Grid.dock Dock.Bottom
                            Grid.columnDefinitions "Auto,*"
                            Grid.rowDefinitions "Auto,32"
                            Grid.children [
                                Button.create [
                                    Button.width 64
                                    Button.column 0
                                    Button.row 0
                                    Button.horizontalAlignment HorizontalAlignment.Center
                                    Button.horizontalContentAlignment HorizontalAlignment.Center
                                    Button.content "Play"
                                    Button.onClick (fun _ ->
                                        task {
                                            match! validate path.Current with
                                            | Ok media ->
                                                errors.Set ""
                                                mp.Play media |> ignore
                                            | Error ex -> errors.Set ex
                                        }
                                        |> ignore)
                                    Button.dock Dock.Bottom
                                ]
                                TextBox.create [
                                    TextBox.row 0
                                    TextBox.rowSpan 2
                                    TextBox.column 1
                                    TextBox.verticalAlignment VerticalAlignment.Top
                                    TextBox.text path.Current
                                    if not <| String.IsNullOrEmpty errors.Current then
                                        TextBox.errors [ errors.Current ]
                                    TextBox.onTextChanged path.Set
                                ]
                            ]
                        ]

                        LibVLCSharpComponent.seekBar "player" mp position ignore [ Component.dock Dock.Bottom ]

                        VideoView.create [
                            VideoView.verticalAlignment VerticalAlignment.Stretch
                            VideoView.horizontalAlignment HorizontalAlignment.Stretch
                            VideoView.mediaPlayer (Some mp)
                        ]
                    ]
                ]
        )