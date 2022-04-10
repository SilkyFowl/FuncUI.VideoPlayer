namespace VideoPlayer

module Shell =
    open Avalonia
    open Avalonia.Input
    open Avalonia.Controls
    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Hosts
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Builder

    let view =
        Component (fun _ ->
            DockPanel.create [

                DockPanel.children [
                    TabControl.create [
                        TabControl.tabStripPlacement Dock.Top
                        TabControl.viewItems [
                            TabItem.create [
                                TabItem.header "Counter Sample"
                                TabItem.content Counter.view
                            ]
                            TabItem.create [
                                TabItem.header "About"
                                TabItem.content About.view
                            ]
                            TabItem.create [
                                TabItem.header "VideoPlayer-Component"
                                TabItem.content VideoPlayerComponent.view
                            ]
                            TabItem.create [
                                TabItem.header "VideoPlayer-UseElmish"
                                TabItem.content (VideoPlayerElmish.view)
                            ]
                        ]
                    ]
                ]
            ])


    open LibVLCSharp.Avalonia.FuncUI
    /// This is the main window of your application
    /// you can do all sort of useful things here like setting heights and widths
    /// as well as attaching your dev tools that can be super useful when developing with
    /// Avalonia
    type MainWindow() as this =
        inherit WindowWrapper(FloatingWindowOwnerImpl.tryGet ())

        do
            base.Title <- "Full App"
            base.Width <- 800.0
            base.Height <- 600.0
            base.MinWidth <- 800.0
            base.MinHeight <- 600.0
            this.Content <- view

            this.AttachDevTools()

//this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
//this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true