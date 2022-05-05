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

open LibVLCSharp.Avalonia.FuncUI


module LibVLCSharp =
    let libVLC =
        lazy
            (let instance = new LibVLC(true)
             new State<_>(instance) :> IReadable<_>)

module MediaPlayer =
    open LibVLCSharp
    let create () = new MediaPlayer(libVLC.Value.Current)

module Media =
    open LibVLCSharp

    let ofUri uri =
        new Media(libVLC.Value.Current, uri = uri)

    let ofPath text =
        backgroundTask {
            if String.IsNullOrEmpty text then
                return Error "NullOrEmpty"
            else
                let media = Uri text |> ofUri


                let opt =
                    MediaParseOptions.ParseLocal
                    ||| MediaParseOptions.ParseNetwork

                match! media.Parse(opt, timeout = 3000) with
                | MediaParsedStatus.Done -> return Ok media
                | other -> return Error $"Parse {other}"
        }

module SeekBar =

    let create id (player: MediaPlayer) writablePosition onPositionChanged attrs =
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
                        |> List.reduce Observable.merge
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


module VideoPlayerComponent =
    open Avalonia.Media

    let player = lazy (new State<_>(MediaPlayer.create ()))

    let opacityMask: IBrush =
        LinearGradientBrush(
            StartPoint = RelativePoint(Point(0.0, 1.0), RelativeUnit.Relative),
            EndPoint = RelativePoint(Point(0.0, 0.0), RelativeUnit.Relative),
            GradientStops =
                (GradientStops()
                 |> tap (fun s ->
                     [ GradientStop(Color.Parse "Black", 0.0)
                       GradientStop(Color.Parse "Gray", 0.8)
                       GradientStop(Color.Parse "Transparent", 1.0) ]
                     |> s.AddRange))
        )

    let view =
        Component.create (
            "VideoPlayer-VideoView",
            fun ctx ->

                let mp = ctx.usePassedRead player.Value

                let path =
                    ctx.useState (
                        "http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4",
                        false
                    )

                let videoViewVisible = ctx.useState false
                let position = ctx.useState (0.0, false)

                ctx.useEffect (
                    (fun _ ->
                        CompositeDisposable.ofSeq [
                            mp.Current.Playing
                            |> Observable.subscribe (fun _ -> videoViewVisible.Set true)

                            mp.Current.Stopped
                            |> Observable.subscribe (fun _ -> videoViewVisible.Set false)
                        ]
                        :> IDisposable),
                    [ EffectTrigger.AfterInit ]
                )

                let errors = ctx.useState ""

                VideoView.create [
                    VideoView.isVideoVisible videoViewVisible.Current

                    VideoView.mediaPlayer (Some mp.Current)
                    VideoView.content (
                        Grid.create [
                            Grid.classes [ "videoview-content" ]
                            Grid.rowDefinitions "*,Auto,Auto"
                            Grid.children [
                                Canvas.create [
                                    Canvas.row 1
                                    Canvas.rowSpan 2
                                    Canvas.column 0
                                    Canvas.columnSpan 3
                                    Canvas.background Brushes.Black
                                    Canvas.opacity 0.5
                                    Canvas.opacityMask opacityMask
                                ]
                                SeekBar.create
                                    "videoplayer-seekbar"
                                    mp.Current
                                    position
                                    ignore
                                    [ Component.row 1 ]
                                Grid.create [
                                    Grid.row 2
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
                                                    match! Media.ofPath path.Current with
                                                    | Ok media ->
                                                        errors.Set ""
                                                        mp.Current.Play media |> ignore
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
                            ]
                        ]
                    )
                ]
        )

module VideoPlayerElmish =
    open global.Elmish

    type State =
        { player: MediaPlayer
          playerState: Deferred<Result<unit, string>>
          path: string }

    type Msg =
        | Play of AsyncOperationStatus<unit, Result<unit, string>>
        | SetPath of string

    let update msg state =
        match msg with
        | Play (Started _) when state.playerState = InProgress -> state, Cmd.none
        | Play (Started _) ->
            { state with playerState = InProgress },
            task {
                match! Media.ofPath state.path with
                | Ok media ->
                    if state.player.Play media then
                        return Ok() |> Finished |> Play
                    else
                        return Error "Play Failed" |> Finished |> Play
                | Error ex -> return Error ex |> Finished |> Play
            }
            |> Cmd.OfTask.result
        | Play (Finished result) -> { state with playerState = Resolved result }, Cmd.none
        | SetPath value -> { state with path = value }, Cmd.none

    let init =
        lazy
            { player = MediaPlayer.create ()
              playerState = HasNotStartedYet
              path = "http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4" }

    let view =
        Component.create (
            "player-elmish",
            (fun ctx ->

                let state, dispatch = ctx.useElmish (init.Value, update)
                let _, player = ctx.useMapRead state (fun s -> s.player)

                let videoViewVisible = ctx.useState false

                ctx.useEffect (
                    (fun _ ->
                        CompositeDisposable.ofSeq [
                            player.Playing
                            |> Observable.subscribe (fun _ -> videoViewVisible.Set true)

                            player.Stopped
                            |> Observable.subscribe (fun _ -> videoViewVisible.Set false)
                        ]
                        :> IDisposable),
                    [ EffectTrigger.AfterInit ]
                )

                VideoView.create [
                    VideoView.isVideoVisible videoViewVisible.Current
                    VideoView.mediaPlayer (Some player)
                    VideoView.content (
                        DockPanel.create [
                            DockPanel.row 1
                            DockPanel.verticalAlignment VerticalAlignment.Bottom
                            DockPanel.margin (0,0,0,8)
                            DockPanel.children [
                                Button.create [
                                    Button.width 64
                                    Button.horizontalAlignment HorizontalAlignment.Center
                                    Button.horizontalContentAlignment HorizontalAlignment.Center
                                    Button.content "Play"
                                    Button.onClick (fun _ -> Started() |> Play |> dispatch)
                                    Button.dock Dock.Left
                                ]
                                TextBox.create [
                                    TextBox.verticalAlignment VerticalAlignment.Stretch
                                    TextBox.text state.Current.path
                                    match state.Current.playerState with
                                    | Resolved (Error error) -> TextBox.errors [ error ]
                                    | _ -> ()
                                    TextBox.onTextChanged (SetPath >> dispatch)
                                ]
                            ]
                        ]
                    )
                ])
        )