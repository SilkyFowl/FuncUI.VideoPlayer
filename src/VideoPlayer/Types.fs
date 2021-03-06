namespace VideoPlayer

open Avalonia.FuncUI
open global.Elmish

[<AutoOpen>]
module Helper =
    let inline tap f x =
        f x
        x

type ElmishState<'Model, 'Msg>(writableModel: IWritable<'Model>, update) =
    member _.Model = writableModel.Current
    member _.WritableModel = writableModel
    member _.Update: 'Msg -> 'Model -> 'Model * Cmd<'Msg> = update

    member this.Dispatch(msg: 'Msg) =
        let model, cmd = this.Update msg this.Model

        for sub in cmd do
            sub this.Dispatch

        writableModel.Set model

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type AsyncOperationStatus<'args, 't> =
    | Started of 'args
    | Finished of 't

[<AutoOpen>]
module InputElement  =
    open Avalonia
    open Avalonia.Controls
    open Avalonia.Input
    open Avalonia.Input.TextInput
    open Avalonia.Interactivity
    open Avalonia.Media
    open Avalonia.Media.Immutable
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.Types

    type InputElement with
        static member onTextInputMethodClientRequested<'t when 't :> InputElement>(func: TextInputMethodClientRequestedEventArgs -> unit, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription<TextInputMethodClientRequestedEventArgs>(InputElement.TextInputMethodClientRequestedEvent, func, ?subPatchOptions = subPatchOptions)

[<AutoOpen>]
module ComponentExtension =

    type IComponentContext with
        // TODO: useLazyを作ろう

        member inline this.useMap (x: IReadable<'t>) mapping =
            let x = this.usePassedRead (x, false)
            let y = (mapping >> this.useState) x.Current

            this.useEffect (
                (fun _ ->
                    let state' = mapping x.Current

                    if y.Current <> state' then y.Set state'),
                [ EffectTrigger.AfterChange x ]
            )

            y, y.Current

        member inline this.useMapRead (x: IReadable<'t>) mapping =
            let y, current = this.useMap x mapping
            y :> IReadable<_>, current

        member this.useElmish<'Model, 'Msg>(init: 'Model, update, ?renderOnChange) =
            let writableModel = this.useState (init, defaultArg renderOnChange true)
            let state = ElmishState<'Model, 'Msg>(writableModel, update)

            writableModel :> IReadable<'Model>, state.Dispatch