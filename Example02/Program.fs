open OpenTK
open OpenTK.Graphics.OpenGL4

type Window(width, height, mode, title, options) =
    inherit GameWindow(width, height, mode, title, options)
    
    let vertexSource = """
        #version 330

        layout (location = 0) in vec4 position;

        void main(void)
        {
            gl_Position = position;
        }
        """

    let fragmentSource = """
        #version 330

        out vec4 outputColor;

        void main(void)
        {
	        outputColor = vec4(1.0, 0.0, 0.0, 1.0);
        }
        """
        
    // Points of a triangle in normalized device coordinates.
    let points = [| -0.5f; 0.0f; 0.0f; 1.0f; 0.5f; 0.0f; 0.0f; 1.0f; 0.0f; 0.5f; 0.0f; 1.0f |]
    
    let mutable vertexShader = 0
    let mutable fragmentShader = 0
    let mutable program = 0

    let mutable verticesVbo = 0
    let mutable vao = 0

    override this.OnLoad(e) =    
        // Load the source of the vertex and fragment shader.
        vertexShader <-
            let shader = GL.CreateShader(ShaderType.VertexShader)
            GL.ShaderSource(shader, vertexSource)
            GL.CompileShader(shader)
            shader
        
        fragmentShader <-
            let shader = GL.CreateShader(ShaderType.FragmentShader)
            GL.ShaderSource(shader, fragmentSource)
            GL.CompileShader(shader)
            shader
    
        // Build the program.
        program <- 
            let program = GL.CreateProgram()
            GL.AttachShader(program, vertexShader)
            GL.AttachShader(program, fragmentShader)
            GL.LinkProgram(program)
            program

        // Create and bind the VBO for the vertices.
        verticesVbo <-
            let buffer = GL.GenBuffer()
            GL.BindBuffer(BufferTarget.ArrayBuffer, buffer)
            buffer
    
        // Create the VAO for the program.
        vao <-
            let array = GL.GenVertexArray()
            GL.BindVertexArray(array)
            array

        // Transfer the vertices from CPU to GPU.
        GL.BufferData(BufferTarget.ArrayBuffer, nativeint(3 * 4 * sizeof<float32>), points, BufferUsageHint.StaticDraw)
        GL.BindBuffer(BufferTarget.ArrayBuffer, 0)

        // Use the program.
        GL.UseProgram(program)

        // Retrieve the vertex location in the program.
        let vertexPosition = GL.GetAttribLocation(program, "position")

        // Bind the only used VBO in this example.
        GL.BindBuffer(BufferTarget.ArrayBuffer, verticesVbo)
        GL.VertexAttribPointer(vertexPosition, 4, VertexAttribPointerType.Float, false, 0, 0)
        GL.EnableVertexAttribArray(vertexPosition)

        GL.ClearColor(0.0f, 0.0f, 1.0f, 0.0f)
        base.OnLoad(e)

    // Function to delete resources
    override this.OnUnload(e) = 
        GL.BindBuffer(BufferTarget.ArrayBuffer, 0)
        GL.DeleteBuffer(verticesVbo)
        GL.DeleteVertexArray(vao)

        GL.UseProgram(0)
        GL.DeleteProgram(program)
        GL.DeleteShader(vertexShader)
        GL.DeleteShader(fragmentShader)
        base.OnUnload(e)

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
