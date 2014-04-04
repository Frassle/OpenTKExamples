open OpenTK
open OpenTK.Graphics.OpenGL4

type Window(width, height, mode, title, options) as this =
    inherit GameWindow(width, height, mode, title, options)

    // Function for initialization.
    do
        this.Context.MakeCurrent(this.WindowInfo)
        // The background will just cleared with blue color.
        GL.ClearColor(0.0f, 0.0f, 1.0f, 0.0f)

    // Function is called before first update and every time when the window is resized.
    override this.OnResize(e) =
        GL.Viewport(0, 0, this.Width, this.Height)
        base.OnResize(e)

    // Function to render and display content. 
    override this.OnRenderFrame(e) =
        // Now, the background is painted blue.
        GL.Clear(ClearBufferMask.ColorBufferBit)
        // Swap the front and back buffers
        this.Context.SwapBuffers()
        base.OnRenderFrame(e)

[<EntryPoint>]
let main argv = 
    use window = new Window(640, 480, Graphics.GraphicsMode.Default, "Example Window", GameWindowFlags.Default)
    window.Run()
    0
