open OpenTK
open OpenTK.Graphics.OpenGL4

type Vertex = 
    struct
        val Position : Vector4
        val Color : Vector4

        new(position : Vector4, color : Vector4) = { Position = position; Color = color }
    end 

type Window(width, height, mode, title, options) as this =
    inherit GameWindow(width, height, mode, title, options)

    do
        this.Context.MakeCurrent(this.WindowInfo)

    let points = 
        [|
            Vertex(Vector4(-0.5f, 0.0f, 1.0f, 1.0f), Vector4(1.0f, 0.0f, 0.0f, 1.0f))
            Vertex(Vector4(-0.5f, 0.0f, 1.0f, 1.0f), Vector4(1.0f, 0.0f, 0.0f, 1.0f))
            Vertex(Vector4(-0.5f, 0.0f, 1.0f, 1.0f), Vector4(1.0f, 0.0f, 0.0f, 1.0f))
        |]

    let vertexSource = """
        #version 330
        
        layout (location = 0) in vec4 position;
        layout (location = 1) in vec4 color;

        smooth out vec4 blendColor;

        void main(void)
        {
            gl_Position = position;
	        blendColor = color;
        }
        """

    let fragmentSource = """
        #version 330

        smooth in vec4 blendColor;

        out vec4 outputColor;

        void main(void)
        {
	        outputColor = blendColor;
        }
        """

    let createShader typ source = 
        let shader = GL.CreateShader(typ)
        GL.ShaderSource(shader, source)
        GL.CompileShader(shader)
        shader

    let vertexShader = createShader ShaderType.VertexShader vertexSource

    let fragmentShader = createShader ShaderType.VertexShader vertexSource

    let program =
        let program = GL.CreateProgram()
        GL.AttachShader(program, vertexShader)
        GL.AttachShader(program, fragmentShader)
        GL.LinkProgram(program)
        program
        

    let verticesVbo = 
        let buffer = GL.GenBuffer()
        GL.BindBuffer(BufferTarget.ArrayBuffer, buffer)
        buffer
    
    let vao = 
        let array = GL.GenVertexArray()
        GL.BindVertexArray(array)
        array

    do     
        // Transfer the vertices from CPU to GPU.
        GL.BufferData(BufferTarget.ArrayBuffer, nativeint(3 * 8 * sizeof<float32>), points, BufferUsageHint.StaticDraw)
        GL.BindBuffer(BufferTarget.ArrayBuffer, 0)

        // Use the program.
        GL.UseProgram(program)
        let vertexPosition = GL.GetAttribLocation(program, "position");
        let vertexColor = GL.GetAttribLocation(program, "color");
    
        GL.BindBuffer(BufferTarget.ArrayBuffer, verticesVbo)

        GL.VertexAttribPointer(vertexPosition, 4, VertexAttribPointerType.Float, false, sizeof<float32> * 8, 0)
        GL.EnableVertexAttribArray(vertexPosition)

        GL.VertexAttribPointer(vertexColor, 4, VertexAttribPointerType.Float, false, sizeof<float32> * 8, sizeof<float32> * 4)
        GL.EnableVertexAttribArray(vertexColor)

    do
        GL.ClearColor(0.0f, 0.0f, 1.0f, 0.0f)

    override this.OnResize(e) =
        GL.Viewport(0, 0, this.Width, this.Height)
        base.OnResize(e)

    override this.OnRenderFrame(e) =
        GL.Clear(ClearBufferMask.ColorBufferBit)

        // This draws the triangle, having three points.
        GL.DrawArrays(PrimitiveType.Triangles, 0, 3);

        this.Context.SwapBuffers()
        base.OnRenderFrame(e)

[<EntryPoint>]
let main argv = 
    use window = new Window(640, 480, Graphics.GraphicsMode.Default, "Example Window", GameWindowFlags.Default)
    window.Run()
    0
