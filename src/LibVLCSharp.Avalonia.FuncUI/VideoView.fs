namespace LibVLCSharp.Avalonia.FuncUI

open Avalonia
open Avalonia.Controls
open Avalonia.Data
open Avalonia.Platform

open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Builder

open LibVLCSharp.Shared

open System


module MediaPlayer =
    let isAttached (platformHandle: IPlatformHandle) (mediaPlayer: MediaPlayer) =
        match Environment.OSVersion.Platform with
        | PlatformID.Win32NT -> mediaPlayer.Hwnd = platformHandle.Handle
        | PlatformID.MacOSX -> mediaPlayer.XWindow = uint platformHandle.Handle
        | PlatformID.Unix -> mediaPlayer.NsObject = platformHandle.Handle
        | _ -> false

    let attachHandle (platformHandle: IPlatformHandle) (mediaPlayer: MediaPlayer) =
        match Environment.OSVersion.Platform with
        | PlatformID.Win32NT -> mediaPlayer.Hwnd <- platformHandle.Handle
        | PlatformID.MacOSX -> mediaPlayer.XWindow <- uint platformHandle.Handle
        | PlatformID.Unix -> mediaPlayer.NsObject <- platformHandle.Handle
        | _ -> ()

    let detachHandle (mediaPlayer: MediaPlayer) =
        mediaPlayer.Stop()

        match Environment.OSVersion.Platform with
        | PlatformID.Win32NT -> mediaPlayer.Hwnd <- IntPtr.Zero
        | PlatformID.MacOSX -> mediaPlayer.XWindow <- 0u
        | PlatformID.Unix -> mediaPlayer.NsObject <- IntPtr.Zero
        | _ -> ()

type VideoView() =
    inherit FloatingWindowHost()

    let mutable mediaPlayer = Option<MediaPlayer>.None
    let mutable platformHandle = Option<IPlatformHandle>.None

    let onUpdateHandleOrMediaPlayer () =
        match mediaPlayer, platformHandle with
        | Some mp, Some p when MediaPlayer.isAttached p mp -> ()
        | Some mp, Some p -> MediaPlayer.attachHandle p mp
        | Some mp, None -> MediaPlayer.detachHandle mp
        | _ -> ()

    let nativePresenter =
        { new NativeControlHost(Name = "VideoView-NativePresenter") with
            override _.CreateNativeControlCore(parent) =
                base.CreateNativeControlCore parent
                |> tap (fun handle ->
                    platformHandle <- Some handle
                    onUpdateHandleOrMediaPlayer ())

            override _.DestroyNativeControlCore(control) =
                platformHandle <- None
                onUpdateHandleOrMediaPlayer ()
                base.DestroyNativeControlCore control }

    override x.OnInitialized() =
        x.VisualChildren.Add nativePresenter
        base.OnInitialized()

    member x.MediaPlayer
        with get () = mediaPlayer
        and set value =
            if obj.ReferenceEquals(mediaPlayer, value) then
                ()
            else
                mediaPlayer <- value
                onUpdateHandleOrMediaPlayer ()

    static member MediaPlayerProperty =
        AvaloniaProperty.RegisterDirect<VideoView, MediaPlayer option>(
            nameof MediaPlayer,
            (fun o -> o.MediaPlayer),
            (fun o v -> o.MediaPlayer <- v),
            defaultBindingMode = BindingMode.TwoWay
        )

    member x.IsVideoVisible
        with get () = nativePresenter.IsVisible
        and set value = nativePresenter.IsVisible <- value

    static member IsVideoVisibleProperty =
        AvaloniaProperty.RegisterDirect(
            nameof Unchecked.defaultof<VideoView>.IsVideoVisible,
            (fun (o: VideoView) -> o.IsVideoVisible),
            (fun (o: VideoView) v -> o.IsVideoVisible <- v)
        )

module VideoView =

    let create (attrs: IAttr<VideoView> list) : IView<VideoView> = ViewBuilder.Create<VideoView>(attrs)

    let mediaPlayer<'t when 't :> VideoView> (player: MediaPlayer option) : IAttr<'t> =
        AttrBuilder<'t>
            .CreateProperty<MediaPlayer option>(VideoView.MediaPlayerProperty, player, ValueSome obj.ReferenceEquals)

    let isVideoVisible<'t when 't :> VideoView> (isVideoVisible: bool) : IAttr<'t> =
        AttrBuilder<'t>
            .CreateProperty<bool>(VideoView.IsVideoVisibleProperty, isVideoVisible, ValueNone)