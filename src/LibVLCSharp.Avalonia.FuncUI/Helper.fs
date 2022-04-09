namespace LibVLCSharp.Avalonia.FuncUI

[<AutoOpen>]
module Helper =
    let inline tap f x =
        f x
        x

    let inline notFun f x = f x |> not

    let inline (|NotEq|_|) x target =
        if target <> x then
            Some target
        else
            None

    let inline refEquals a b = obj.ReferenceEquals(a, b)

module Task =
    open System.Threading.Tasks
    let delayMilliseconds time = Task.Delay(millisecondsDelay = time)

module MailboxProcessor =
    [<RequireQualifiedAccess>]
    type MailboxProcessorMsg<'state, 'u, 'r> =
        | Post of ('state -> 'state)
        | Reply of ('u -> 'r) * AsyncReplyChannel<'r>

    let inline createAgent initialState =
        MailboxProcessor.Start (fun inbox ->
            let rec loop oldState =
                async {
                    match! inbox.Receive() with
                    | MailboxProcessorMsg.Post postFunc ->
                        let newState = postFunc oldState
                        return! loop newState
                    | MailboxProcessorMsg.Reply (replyFunc, ch) ->
                        replyFunc oldState |> ch.Reply
                        return! loop oldState
                }

            loop initialState)

    let inline post (agent: MailboxProcessor<MailboxProcessorMsg<'p, 'u, 'r>>) postFunc =
        MailboxProcessorMsg.Post postFunc |> agent.Post

    let inline postAndReply (agent: MailboxProcessor<MailboxProcessorMsg<'p, 'u, 'r>>) replyFunc =
        agent.PostAndReply(fun reply -> MailboxProcessorMsg.Reply(replyFunc, reply))

module NativeModule =
    open System.Runtime.InteropServices

    let CW_USEDEFAULT = ((int) 0x80000000)

    let GWL_STYLE = -16
    let GWL_EXSTYLE = -20

    let WS_EX_NOACTIVATE = 0x08000000un

    let WS_EX_TOOLWINDOW = 0x00000080un

    [<Literal>]
    let WM_ACTIVATE = 0x0006u

    [<Literal>]
    let WM_NCACTIVATE = 0x0086u

    [<Literal>]
    let SW_SHOWNOACTIVATE = 4u

    [<Literal>]
    let MA_NOACTIVATE = 3

    let WA_INACTIVE = 0

    let nativeBool (b: bool) = if b then 1n else 0n

    let (|Active|Deactive|) =
        function
        | 0n -> Deactive
        | _ -> Active

    [<DllImport("user32.dll", SetLastError = true)>]
    extern unativeint GetWindowLongPtr(nativeint hWnd, int nIndex)

    [<DllImport("user32.dll", SetLastError = true)>]
    extern nativeint SetWindowLongPtr(nativeint hWnd, int nIndex, unativeint dwLong)

    [<DllImport("user32.dll")>]
    extern int ShowWindow(nativeint hWnd, uint nCmdShow)

    [<DllImport("user32.dll", SetLastError = true)>]
    extern nativeint CreateWindowEx(unativeint dwExStyle, uint16 lpClassName, string lpWindowName, unativeint dwStyle, int x, int y, int nWidth, int nHeight, nativeint hWndParent, nativeint hMenu, nativeint hInstance, nativeint lpParam)

    [<return: MarshalAs(UnmanagedType.Bool)>]
    [<DllImport("user32.dll", SetLastError = true, CharSet = CharSet.Auto)>]
    extern bool PostMessage(nativeint hWnd, uint msg, nativeint wParam, nativeint lParam)


module Observable =
    open System
    open System.Reactive.Linq

    let inline ignore source = Observable.map ignore source

    let inline mergeIgnore a = (ignore >> Observable.merge) a

    let inline combineLatest2 a b =
        Observable.CombineLatest(a, b, (fun a b -> a, b))

    let inline combineLatest3 a b c =
        Observable.CombineLatest(a, b, c, (fun a b c -> a, b, c))

module Subject =
    open System.Reactive.Subjects

    let behavior sources = new BehaviorSubject<_>(sources)

module CompositeDisposable =
    open System.Reactive.Disposables

    let create () = new CompositeDisposable()


[<AutoOpen>]
module AvaloniaExtention =
    open Avalonia

    let inline (!) property =
        AvaloniaProperty.op_OnesComplement property


module WindowBase =
    open Avalonia.Controls
    let getHandle (w: WindowBase) = w.PlatformImpl.Handle.Handle