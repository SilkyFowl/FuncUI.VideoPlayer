namespace VideoPlayer

module Counter =
    open Avalonia.FuncUI
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    
    let view =
        Component.create("Counter",fun ctx ->
            let state = ctx.useState 0
            DockPanel.create [
                DockPanel.verticalAlignment VerticalAlignment.Center
                DockPanel.horizontalAlignment HorizontalAlignment.Center
                DockPanel.children [
                    Button.create [
                        Button.width 64
                        Button.horizontalAlignment HorizontalAlignment.Center
                        Button.horizontalContentAlignment HorizontalAlignment.Center
                        Button.content "Reset"
                        Button.onClick (fun _ -> state.Set 0)
                        Button.dock Dock.Bottom
                    ]
                    Button.create [
                        Button.width 64
                        Button.horizontalAlignment HorizontalAlignment.Center
                        Button.horizontalContentAlignment HorizontalAlignment.Center
                        Button.content "-"
                        Button.onClick (fun _ -> state.Current - 1 |> state.Set)
                        Button.dock Dock.Bottom
                    ]
                    Button.create [
                        Button.width 64
                        Button.horizontalAlignment HorizontalAlignment.Center
                        Button.horizontalContentAlignment HorizontalAlignment.Center
                        Button.content "+"
                        Button.onClick (fun _ -> state.Current + 1 |> state.Set)
                        Button.dock Dock.Bottom
                    ]
                    TextBlock.create [
                        TextBlock.dock Dock.Top
                        TextBlock.fontSize 48.0
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        TextBlock.text (string state.Current)
                    ]
                ]
            ]
        )