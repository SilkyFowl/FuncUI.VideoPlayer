namespace LibVLCSharp.Avalonia.FuncUI

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Interactivity
open Avalonia.Media
open Avalonia.Layout
open Avalonia.VisualTree

open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Builder

open System

type FloatingWindow() =
    inherit Window
        (
            SystemDecorations = SystemDecorations.None,
            TransparencyLevelHint = WindowTransparencyLevel.Transparent,
            Background = Brushes.Transparent,
            TransparencyBackgroundFallback = Brushes.Black,
            SizeToContent = SizeToContent.WidthAndHeight,
            ShowInTaskbar = false
        )

    let getVisualRoot (visual: IVisual) = visual.VisualRoot :?> WindowBase

    member val Owner = Option<IVisual>.None with get, set

    member x.RaizeOwnerEvent e =
        match x.Owner with
        | Some (:? IInteractive as i) -> i.RaiseEvent e
        | _ -> ()

    override x.OnInitialized() =
        let callback e =
            match x.Content with
            | :? IControl as c when not c.IsPointerOver && x.IsPointerOver -> x.RaizeOwnerEvent e
            | _ -> ()

        x.PointerPressed |> Observable.add callback

        x.PointerReleased |> Observable.add callback

        x.GetPropertyChangedObservable WindowBase.ContentProperty
        |> Observable.add (fun e ->
            match e.NewValue with
            | :? IView as v -> x.Content <- VirtualDom.VirtualDom.create v
            | _ -> ())

#if DEBUG
        x.AttachDevTools()
#endif

type FloatingOwnerHost() =
    inherit ContentControl()

    let hostDisposables = CompositeDisposable.create ()
    let floatingDisposables = CompositeDisposable.create ()

    let floatingWindowSub = FloatingWindow() |> Subject.behavior
    let isAttachedSub = Subject.behavior false


    member inline private x.UpdateFloatingCore (manager: VisualLayerManager) (hostBounds: Rect) =
        let getLeft () =
            match manager.HorizontalAlignment with
            | HorizontalAlignment.Right -> hostBounds.Width - manager.Bounds.Width
            | HorizontalAlignment.Center -> (hostBounds.Width - manager.Bounds.Width) / 2.0
            | _ -> 0.0

        let getTop () =
            match manager.VerticalAlignment with
            | VerticalAlignment.Bottom -> hostBounds.Height - manager.Bounds.Height
            | VerticalAlignment.Center -> (hostBounds.Height - manager.Bounds.Height) / 2.0
            | _ -> 0.0

        let newSizeToContent, newWidth, newHeight, newPoint =
            match manager.HorizontalAlignment, manager.VerticalAlignment with
            | (HorizontalAlignment.Stretch, VerticalAlignment.Stretch) ->
                SizeToContent.Manual, hostBounds.Width, hostBounds.Height, Point(0.0, 0.0)
            | (HorizontalAlignment.Stretch, _) ->
                SizeToContent.Width, hostBounds.Width, Double.NaN, Point(0.0, getLeft ())
            | (_, VerticalAlignment.Stretch) ->
                SizeToContent.Height, Double.NaN, hostBounds.Height, Point(getTop (), 0.0)
            | (_, _) -> SizeToContent.Manual, Double.NaN, Double.NaN, Point(getTop (), getLeft ())

        manager.MaxWidth <- hostBounds.Width
        manager.MaxHeight <- hostBounds.Height

        floatingWindowSub.Value.SizeToContent <- newSizeToContent
        floatingWindowSub.Value.Width <- newWidth
        floatingWindowSub.Value.Height <- newHeight

        match x.PointToScreen newPoint with
        | newPosition when newPosition <> floatingWindowSub.Value.Position ->
            floatingWindowSub.Value.Position <- newPosition
        | _ -> ()

    member inline private x.UpdateFloating() =
        floatingWindowSub.Value.GetVisualDescendants()
        |> Seq.tryPick (function
            | :? VisualLayerManager as m -> Some m
            | _ -> None)
        |> Option.iter (fun manager -> x.UpdateFloatingCore manager x.Bounds)

    member inline private x.InitFloatingWindow(floatingWindow: FloatingWindow) =
        floatingWindow.Bind(FloatingWindow.ContentProperty, x.GetObservable FloatingOwnerHost.ContentProperty)
        |> floatingDisposables.Add

        let root = x.GetVisualRoot() :?> WindowBase

        (x.GetObservable FloatingOwnerHost.ContentProperty,
         x.GetObservable FloatingOwnerHost.BoundsProperty,
         root.PositionChanged,
         root.GetObservable Window.WindowStateProperty)
        |> Observable.combineLatest4With (fun content hostBounds _ _ ->
            let manager =
                match content with
                | null -> None
                | _ ->
                    floatingWindow.GetVisualDescendants()
                    |> Seq.tryPick (function
                        | :? VisualLayerManager as m -> Some m
                        | _ -> None)

            manager, hostBounds)
        |> Observable.subscribe (function
            | None, _ -> ()
            | Some manager, hostBounds -> x.UpdateFloatingCore manager hostBounds)
        |> floatingDisposables.Add

    override x.OnInitialized() =
        base.OnInitialized()

        x.GetObservable ContentControl.IsVisibleProperty
        |> Observable.combineLatest3 floatingWindowSub isAttachedSub
        |> Observable.subscribe (fun (floating, isAttached, isVisible) ->

            if floating.IsVisible && isAttached = isVisible then
                ()
            elif isVisible && isAttached then
                x.InitFloatingWindow floatingWindowSub.Value

                task {
                    do! Task.delayMilliseconds 1
                    x.UpdateFloating()
                }
                |> ignore

                x.GetVisualRoot() :?> Window |> floating.Show

            else
                floatingDisposables.Clear()
                floating.Hide())
        |> hostDisposables.Add

    override x.OnAttachedToVisualTree e =
        isAttachedSub.OnNext true
        base.OnAttachedToVisualTree e

    override x.OnDetachedFromVisualTree e =
        isAttachedSub.OnNext false
        base.OnDetachedFromVisualTree e

    member x.FloatingWindow
        with get () = floatingWindowSub.Value
        and set (value: FloatingWindow) =
            if not <| refEquals floatingWindowSub.Value value then
                floatingDisposables.Clear()

                floatingWindowSub.Value.Close()
                value.Content <- floatingWindowSub.Value.Content
                floatingWindowSub.OnNext value

    static member FloatingWindowProperty =
        AvaloniaProperty.RegisterDirect<FloatingOwnerHost, _>(
            nameof
                Unchecked.defaultof<FloatingOwnerHost>
                    .FloatingWindow,
            (fun o -> o.FloatingWindow),
            (fun o v -> o.FloatingWindow <- v)
        )

module FloatingOwnerHost =
    let create (attrs) =
        ViewBuilder.Create<FloatingOwnerHost> attrs

    let floatingWindow<'t when 't :> FloatingOwnerHost> (floatingWindow: FloatingWindow) : IAttr<'t> =
        AttrBuilder<'t>
            .CreateProperty<FloatingWindow>(FloatingOwnerHost.FloatingWindowProperty, floatingWindow, ValueNone)