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

module LibVLCSharpComponent =

    let seekBar id (player: IReadable<MediaPlayer>) writablePosition onPositionChanged attrs =
        Component.create (
            id,
            fun ctx ->
                let minValue = 0.0
                let maxValue = 1.0

                let player = ctx.usePassedRead player
                let player = player.Current
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

                View.createWithOutlet
                    outlet.Set
                    Slider.create
                    [ yield! attrs

                      Slider.minimum minValue
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
    let player = lazy (new State<_>(MediaPlayer.create ()))

    let view =
        Component.create (
            "VideoPlayer-VideoView",
            fun ctx ->

                let mp = ctx.usePassedRead player.Value

                let position = ctx.useState 0.0

                let path =
                    ctx.useState (
                        "http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4",
                        false
                    )

                let errors = ctx.useState ""

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




                        VideoView.create [
                            VideoView.isVideoVisible mp.Current.IsPlaying

                            VideoView.mediaPlayer (Some mp.Current)
                            VideoView.content (
                                DockPanel.create [
                                    DockPanel.children [
                                        LibVLCSharpComponent.seekBar
                                            "player"
                                            mp
                                            position
                                            ignore
                                            [ Slider.verticalAlignment VerticalAlignment.Bottom
                                              Slider.dock Dock.Bottom ]
                                    ]
                                ]
                            )
                        ]
                    ]
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

    let cmp () =
        Component.create (
            "player-elmish",
            (fun ctx ->

                let state, dispatch = ctx.useElmish (init.Value, update)
                let _, player = ctx.useMapRead state (fun s -> s.player)

                let videoViewVisible = ctx.useState false

                ctx.useEffect (
                    (fun _ ->
                        player.Playing
                        |> Observable.subscribe (fun _ -> videoViewVisible.Set true)),
                    [ EffectTrigger.AfterInit ]
                )

                ctx.useEffect (
                    (fun _ ->
                        player.Stopped
                        |> Observable.subscribe (fun _ -> videoViewVisible.Set false)),
                    [ EffectTrigger.AfterInit ]
                )

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
                                    Button.onClick (fun _ -> Started() |> Play |> dispatch)
                                    Button.dock Dock.Bottom
                                ]
                                TextBox.create [
                                    TextBox.row 0
                                    TextBox.rowSpan 2
                                    TextBox.column 1
                                    TextBox.verticalAlignment VerticalAlignment.Top
                                    TextBox.text state.Current.path
                                    match state.Current.playerState with
                                    | Resolved (Error error) -> TextBox.errors [ error ]
                                    | _ -> ()
                                    TextBox.onTextChanged (SetPath >> dispatch)
                                ]
                            ]
                        ]

                        VideoView.create [
                            VideoView.isVideoVisible videoViewVisible.Current
                            VideoView.mediaPlayer (Some player)
                        ]
                    ]
                ])
        )