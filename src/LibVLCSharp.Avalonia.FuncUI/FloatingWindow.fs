namespace LibVLCSharp.Avalonia.FuncUI

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Interactivity
open Avalonia.Media
open Avalonia.Platform
open Avalonia.Win32
open Avalonia.Layout
open Avalonia.VisualTree

open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Builder

open System

open NativeModule

/// Windows環境でタイトルバーのアクティブ状態を制御するFloatingWindow側のクラス
type FloatingWindowImpl() =
    inherit WindowImpl()

    static let floatingHostList = MailboxProcessor.createAgent Map.empty<nativeint, IVisual>

    let tryGetFloatingHostRoot (x: FloatingWindowImpl) =
        Map.tryFind x.Handle.Handle
        |> MailboxProcessor.postAndReply floatingHostList
        |> Option.map (fun v -> v.GetVisualRoot())

    let (|FloatingHosteHandle|_|) (x: FloatingWindowImpl) =
        match tryGetFloatingHostRoot x with
        | Some (:? WindowBase as w) -> WindowBase.getHandle w |> Some
        | _ -> None

    static member Register (floatingWindow: WindowBase) (owner: IVisual) =
        Map.add floatingWindow.PlatformImpl.Handle.Handle owner
        |> MailboxProcessor.post floatingHostList

    static member UnRegister(floatingWindow: WindowBase) =
        Map.remove floatingWindow.PlatformImpl.Handle.Handle
        |> MailboxProcessor.post floatingHostList

    override x.WndProc(hWnd, msg, wParam, lParam) =
        match msg, wParam, x with
        | WM_NCACTIVATE, Active, FloatingHosteHandle hostRoot ->
            // 自身がアクティブになるときに親もアクティブにする
            PostMessage(hostRoot, msg, nativeBool true, 0)
            |> ignore

            ``base``.WndProc(hWnd, msg, wParam, lParam)

        | WM_NCACTIVATE, Deactive, FloatingHosteHandle (NotEq lParam hostRoot) ->

            // 次にアクティブになるウィンドウが親以外の場合、
            // 親を非アクティブ化する。
            PostMessage(hostRoot, msg, wParam, lParam)
            |> ignore

            ``base``.WndProc(hWnd, msg, wParam, lParam)
        | _ -> ``base``.WndProc(hWnd, msg, wParam, lParam)

module FloatingWindowImpl =
    let tryGet () =
        if Environment.OSVersion.Platform = PlatformID.Win32NT then
            new FloatingWindowImpl() :> IWindowImpl |> Some
        else
            None


type FloatingWindow() =
    inherit WindowWrapper
        (
            FloatingWindowImpl.tryGet (),
            SystemDecorations = SystemDecorations.None,
            TransparencyLevelHint = WindowTransparencyLevel.Transparent,
            Background = Brushes.Transparent,
            TransparencyBackgroundFallback = Brushes.Black,
            SizeToContent = SizeToContent.WidthAndHeight,
            ShowInTaskbar = false
        )

    let visualLayerManagerSub = Subject.behavior None

    let getVisualRoot (visual: IVisual) = visual.VisualRoot :?> WindowBase

    let mutable floatingHost = Option<IVisual>.None

    member x.FloatingHost
        with get (): Option<IVisual> = floatingHost
        and set (value: Option<IVisual>) =
            if Environment.OSVersion.Platform = PlatformID.Win32NT then
                FloatingWindowImpl.UnRegister x
                Option.iter (FloatingWindowImpl.Register x) value

            floatingHost <- value

    member x.VisualLayerManager =
        match visualLayerManagerSub.Value with
        | Some _ as vm -> vm
        | None ->
            x.GetVisualDescendants()
            |> Seq.tryPick (function
                | :? VisualLayerManager as m ->
                    Some m |> visualLayerManagerSub.OnNext
                    visualLayerManagerSub.Value
                | _ -> None)

    member x.VisualLayerManagerObservable: IObservable<_> = visualLayerManagerSub

    member x.RaizeOwnerEvent e =
        match x.FloatingHost with
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

/// Windows環境でタイトルバーのアクティブ状態を制御する、FloatingWindowHostを使用するWindow側のクラス
type FloatingWindowHostRootImpl() =
    inherit WindowImpl()

    let getFloatingHostHandle (f: FloatingWindow) =
        match f.FloatingHost with
        | Some o ->
            o.VisualRoot :?> WindowBase
            |> WindowBase.getHandle
        | None -> IntPtr.Zero

    let isToClientFloating (window: WindowBase) handle (floatingHostImpl: WindowImpl) =
        match window with
        | :? FloatingWindow as f ->
            WindowBase.getHandle f = handle
            && getFloatingHostHandle f = floatingHostImpl.Handle.Handle
        | _ -> false

    let (|ToClientFloating|_|) (floatingHostImpl: WindowImpl, handle) =
        getCurrentWindows ()
        |> Seq.tryPick (function
            | f when isToClientFloating f handle floatingHostImpl -> Some ToClientFloating
            | _ -> None)

    override x.WndProc(hWnd, msg, wParam, lParam) =

        match msg, wParam, (x, lParam) with
        // 遷移先が自身の子であるFloatingWindowならタイトルバーの表示をアクティブなまま非アクティブ化する。
        | WM_NCACTIVATE, Deactive, ToClientFloating -> ``base``.WndProc(hWnd, msg, nativeBool true, 0)
        | _ -> ``base``.WndProc(hWnd, msg, wParam, lParam)

module FloatingWindowHostRootImpl =
    let tryGet () =
        if Environment.OSVersion.Platform = PlatformID.Win32NT then
            new FloatingWindowHostRootImpl() :> IWindowImpl
            |> Some
        else
            None

type FloatingWindowHost() as x =
    inherit ContentControl()

    let hostDisposables = CompositeDisposable.create ()
    let floatingDisposables = CompositeDisposable.create ()

    let floatingWindowSub =
        FloatingWindow(FloatingHost = Some x)
        |> Subject.behavior

    let isAttachedSub = Subject.behavior false

    let initNewSizeToContent =
        function
        | (HorizontalAlignment.Stretch, VerticalAlignment.Stretch) -> SizeToContent.Manual
        | (HorizontalAlignment.Stretch, _) -> SizeToContent.Width
        | (_, VerticalAlignment.Stretch) -> SizeToContent.Height
        | (_, _) -> SizeToContent.Manual

    let initGetNewWidth (horizontalAlignment: HorizontalAlignment) =
        if horizontalAlignment = HorizontalAlignment.Stretch then
            fun (host: FloatingWindowHost) -> host.Bounds.Width
        else
            fun _ -> Double.NaN

    let initGetNewHeight (verticalAlignment: VerticalAlignment) =
        if verticalAlignment = VerticalAlignment.Stretch then
            fun (host: FloatingWindowHost) -> host.Bounds.Height
        else
            fun _ -> Double.NaN

    let initGetLeft =
        function
        | HorizontalAlignment.Right ->
            fun (host: FloatingWindowHost) (manager: VisualLayerManager) -> host.Bounds.Width - manager.Bounds.Width
        | HorizontalAlignment.Center -> fun host manager -> (host.Bounds.Width - manager.Bounds.Width) / 2.0
        | _ -> fun _ _ -> 0.0

    let initGetTop =
        function
        | VerticalAlignment.Bottom ->
            fun (host: FloatingWindowHost) (manager: VisualLayerManager) -> host.Bounds.Height - manager.Bounds.Height
        | VerticalAlignment.Center -> fun host manager -> (host.Bounds.Height - manager.Bounds.Height) / 2.0
        | _ -> fun _ _ -> 0.0

    /// FloatingWindowのサイズと場所を更新する関数値。
    let mutable updateFloatingCore = ignore

    /// HorizontalAlignment, VerticalAlignmentでupdateFloatingCoreを更新する。
    let initUpdateFloatingCore (verticalAlignment, horizontalAlignment) =
        let newSizeToContent = initNewSizeToContent (horizontalAlignment, verticalAlignment)
        let getNewWidth = initGetNewWidth horizontalAlignment
        let getNewHeight = initGetNewHeight verticalAlignment
        let getNewLeft = initGetLeft horizontalAlignment
        let getNewTop = initGetTop verticalAlignment

        updateFloatingCore <-
            fun (manager: VisualLayerManager) ->
                manager.MaxWidth <- x.Bounds.Width
                manager.MaxHeight <- x.Bounds.Height

                floatingWindowSub.Value.SizeToContent <- newSizeToContent
                floatingWindowSub.Value.Width <- getNewWidth x
                floatingWindowSub.Value.Height <- getNewHeight x

                let newPosition =
                    Point(getNewTop x manager, getNewLeft x manager)
                    |> x.PointToScreen

                if newPosition <> floatingWindowSub.Value.Position then
                    floatingWindowSub.Value.Position <- newPosition

    member inline private x.UpdateFloating() =
        match floatingWindowSub.Value.VisualLayerManager with
        | Some manager -> updateFloatingCore manager
        | None -> ()

    /// FloatingWindowを初期化する。
    member inline private x.InitFloatingWindow(floatingWindow: FloatingWindow) =
        // Contentをバインドする
        floatingWindow[!FloatingWindow.ContentProperty] <- x[!FloatingWindowHost.ContentProperty]

        // floatingWindowと連動してupdateFloatingCoreが更新されるようにする
        floatingWindow.VisualLayerManagerObservable
        |> Observable.subscribe (function
            | Some vm ->
                vm.GetObservable VisualLayerManager.HorizontalAlignmentProperty
                |> Observable.combineLatest2 (vm.GetObservable VisualLayerManager.VerticalAlignmentProperty)
                |> Observable.subscribe initUpdateFloatingCore
                |> floatingDisposables.Add
            | _ -> ())
        |> floatingDisposables.Add

        let root = x.GetVisualRoot() :?> WindowBase

        // updateFloatingCoreを実行するトリガーを設定する
        Observable.ignore root.PositionChanged
        |> Observable.mergeIgnore (root.GetObservable Window.WindowStateProperty)
        |> Observable.mergeIgnore (x.GetObservable FloatingWindowHost.ContentProperty)
        |> Observable.mergeIgnore (x.GetObservable FloatingWindowHost.BoundsProperty)
        |> Observable.subscribe (fun _ ->
            match floatingWindow.VisualLayerManager with
            | Some manager -> updateFloatingCore manager
            | None -> ())
        |> floatingDisposables.Add

    override x.OnInitialized() =
        base.OnInitialized()

        // floatingWindowの表示方法の設定
        x.GetObservable ContentControl.IsVisibleProperty
        |> Observable.combineLatest3 floatingWindowSub isAttachedSub
        |> Observable.subscribe (fun (floating, isAttached, isVisible) ->

            if floating.IsVisible && isAttached = isVisible then
                ()
            elif isVisible && isAttached then
                x.InitFloatingWindow floatingWindowSub.Value

                task {
                    // 最初のfloatingWindowサイズ、位置の更新
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
                floatingWindowSub.Value.FloatingHost <- None
                value.Content <- floatingWindowSub.Value.Content
                value.FloatingHost <- Some x
                floatingWindowSub.OnNext value

    static member FloatingWindowProperty =
        AvaloniaProperty.RegisterDirect<FloatingWindowHost, _>(
            nameof
                Unchecked.defaultof<FloatingWindowHost>
                    .FloatingWindow,
            (fun o -> o.FloatingWindow),
            (fun o v -> o.FloatingWindow <- v)
        )

module FloatingWindowHost =
    let create (attrs) =
        ViewBuilder.Create<FloatingWindowHost> attrs

    let floatingWindow<'t when 't :> FloatingWindowHost> (floatingWindow: FloatingWindow) : IAttr<'t> =
        AttrBuilder<'t>
            .CreateProperty<FloatingWindow>(FloatingWindowHost.FloatingWindowProperty, floatingWindow, ValueNone)