open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL4

type Vertex = 
    struct
        val Position : Vector4
        val ColorA : Color4
        val ColorB : Color4

        new(position : Vector4, colorA : Color4, colorB : Color4) = 
            { Position = position; ColorA = colorA; ColorB = colorB }
    end 

type Window(width, height, mode, title, options) as this =
    inherit GameWindow(width, height, mode, title, options)

    do
        this.Context.MakeCurrent(this.WindowInfo)

    let points = 
        [|
            Vertex(Vector4(-0.5f, 0.0f, 0.0f, 1.0f), OpenTK.Graphics.Color4.Red, OpenTK.Graphics.Color4.Green)
            Vertex(Vector4(0.5f, 0.0f, 0.0f, 1.0f), OpenTK.Graphics.Color4.Green, OpenTK.Graphics.Color4.Blue)
            Vertex(Vector4(0.f, 0.5f, 0.0f, 1.0f), OpenTK.Graphics.Color4.Blue, OpenTK.Graphics.Color4.Red)
        |]

    let vertexSource = """
        #version 330        

        uniform float time;
        uniform float loop;

        layout (location = 0) in vec4 position;
        layout (location = 1) in vec4 colorA;
        layout (location = 2) in vec4 colorB;
        
        smooth out vec4 fragColorA;
        smooth out vec4 fragColorB;

        void main(void)
        {
            float timeScale = 3.14159f * 2.0f / loop;
            float t = mod(time, loop);
            vec4 offset = vec4(
                cos(t * timeScale) * 0.5f,
                sin(t * timeScale) * 0.5f,
                0.0f,
                0.0f);

            gl_Position = position + offset;
	        fragColorA = colorA;
	        fragColorB = colorB;
        }
        """

    let fragmentSource = """
        #version 330

        uniform float time;
        uniform float loop;
        
        smooth in vec4 fragColorA;
        smooth in vec4 fragColorB;

        out vec4 outputColor;

        void main(void)
        {
            float t = mod(time, loop) / loop;
            t = max(1 - abs((t * 2) - 1), 0);
	        outputColor = mix(fragColorA, fragColorB, t);
        }
        """

    let createShader typ source = 
        let shader = GL.CreateShader(typ)
        GL.ShaderSource(shader, source)
        GL.CompileShader(shader)
        let status = GL.GetShader(shader, ShaderParameter.CompileStatus)
        if status <> 0 then
            shader
        else
            failwith (GL.GetShaderInfoLog(shader))

    let vertexShader = createShader ShaderType.VertexShader vertexSource

    let fragmentShader = createShader ShaderType.FragmentShader fragmentSource

    let program =
        let program = GL.CreateProgram()
        GL.AttachShader(program, vertexShader)
        GL.AttachShader(program, fragmentShader)
        GL.LinkProgram(program)
        let status = GL.GetProgram(program, GetProgramParameterName.LinkStatus)
        if status <> 0 then
            program
        else
            failwith (GL.GetProgramInfoLog(program))        

    let verticesVbo = 
        let buffer = GL.GenBuffer()
        GL.BindBuffer(BufferTarget.ArrayBuffer, buffer)
        buffer
    
    let vao = 
        let array = GL.GenVertexArray()
        GL.BindVertexArray(array)
        array
    
    // variable to keep track of total time    
    let mutable time = 0.0f

    // Get the uniform locations        
    let timeLocation = GL.GetUniformLocation(program, "time")
    let loopLocation = GL.GetUniformLocation(program, "loop")

    do     
        // Transfer the vertices from CPU to GPU.
        GL.BufferData(BufferTarget.ArrayBuffer, nativeint(3 * sizeof<Vertex>), points, BufferUsageHint.StaticDraw)
        GL.BindBuffer(BufferTarget.ArrayBuffer, 0)

        // Use the program.
        GL.UseProgram(program)
        let vertexPosition = GL.GetAttribLocation(program, "position");
        let vertexColorA = GL.GetAttribLocation(program, "colorA");
        let vertexColorB = GL.GetAttribLocation(program, "colorB");
    
        GL.BindBuffer(BufferTarget.ArrayBuffer, verticesVbo)

        GL.VertexAttribPointer(vertexPosition, 4, VertexAttribPointerType.Float, false, sizeof<Vertex>, 0)
        GL.EnableVertexAttribArray(vertexPosition)

        GL.VertexAttribPointer(vertexColorA, 4, VertexAttribPointerType.Float, false, sizeof<Vertex>, sizeof<Color4>)
        GL.EnableVertexAttribArray(vertexColorA)

        GL.VertexAttribPointer(vertexColorB, 4, VertexAttribPointerType.Float, false, sizeof<Vertex>, sizeof<Vector4> + sizeof<Color4>)
        GL.EnableVertexAttribArray(vertexColorB)


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

    override this.OnUpdateFrame(e) =
        time <- time + float32(e.Time)
        GL.Uniform1(timeLocation, time)
        GL.Uniform1(loopLocation, 4.0f)

        base.OnUpdateFrame(e)

[<EntryPoint>]
let main argv = 
    use window = new Window(640, 480, Graphics.GraphicsMode.Default, "Example Window", GameWindowFlags.Default)
    window.Run()
    0
