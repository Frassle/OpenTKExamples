open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL4

type Vertex = 
    struct
        val Position : Vector4
        val Color : Color4

        new(position : Vector4, color : Color4) = 
            { Position = position; Color = color }
    end 
    
type Window(width, height, mode, title, options) =
    inherit GameWindow(width, height, mode, title, options)

    let quad color p0 p1 p2 p3 = 
        [| p0; p1; p2; p2; p1; p3 |]
        |> Array.map (fun (p : Vector3) -> Vertex(Vector4(p.X, p.Y, p.Z, 1.0f), color))

    let smallquad color p0 = 
        quad color
            <| p0
            <| p0 + Vector3(0.1f, 0.0f, 0.0f)
            <| p0 + Vector3(0.0f, 0.1f, 0.0f)
            <| p0 + Vector3(0.1f, 0.1f, 0.0f)
//
//                Vector3[] positionVboData = new Vector3[]{
//            new Vector3(-1.0f, -1.0f,  1.0f),
//            new Vector3( 1.0f, -1.0f,  1.0f),
//            new Vector3( 1.0f,  1.0f,  1.0f),
//            new Vector3(-1.0f,  1.0f,  1.0f),
//            new Vector3(-1.0f, -1.0f, -1.0f),
//            new Vector3( 1.0f, -1.0f, -1.0f), 
//            new Vector3( 1.0f,  1.0f, -1.0f),
//            new Vector3(-1.0f,  1.0f, -1.0f) };
//
//        int[] indicesVboData = new int[]{
//             // front face
//                0, 1, 2, 2, 3, 0,
//                // top face
//                3, 2, 6, 6, 7, 3,
//                // back face
//                7, 6, 5, 5, 4, 7,
//                // left face
//                4, 0, 3, 3, 7, 4,
//                // bottom face
//                0, 1, 5, 5, 4, 0,
//                // right face
//                1, 5, 6, 6, 2, 1, };

    let points =
        [|
            // Front quad
            quad Color4.Red 
                <| Vector3(-1.0f, -1.0f, -1.0f)
                <| Vector3(-1.0f,  1.0f, -1.0f)
                <| Vector3( 1.0f, -1.0f, -1.0f)
                <| Vector3( 1.0f,  1.0f, -1.0f)

            // Right quad
            quad Color4.Green 
                <| Vector3(1.0f, -1.0f, -1.0f)
                <| Vector3(1.0f,  1.0f, -1.0f)
                <| Vector3(1.0f, -1.0f,  1.0f)
                <| Vector3(1.0f,  1.0f,  1.0f)
                
            // Left quad
            quad Color4.Blue
                <| Vector3(-1.0f, -1.0f, -1.0f)
                <| Vector3(-1.0f, -1.0f,  1.0f)
                <| Vector3(-1.0f,  1.0f, -1.0f)
                <| Vector3(-1.0f,  1.0f,  1.0f)

            // Back quad
            quad Color4.White
                <| Vector3(-1.0f, -1.0f, 1.0f)
                <| Vector3( 1.0f, -1.0f, 1.0f)
                <| Vector3(-1.0f,  1.0f, 1.0f)
                <| Vector3( 1.0f,  1.0f, 1.0f)

            // Top quad
            quad Color4.Pink
                <| Vector3(-1.0f, 1.0f, 1.0f)
                <| Vector3( 1.0f, 1.0f, 1.0f)
                <| Vector3(-1.0f, 1.0f, -1.0f)
                <| Vector3( 1.0f, 1.0f, -1.0f)

            // Bottom quad
            quad Color4.Purple
                <| Vector3(-1.0f, -1.0f, 1.0f)
                <| Vector3(-1.0f, -1.0f, -1.0f)
                <| Vector3( 1.0f, -1.0f, 1.0f)
                <| Vector3( 1.0f, -1.0f, -1.0f)
        |] |> Array.concat

    let vertexSource = """
        #version 330        

        uniform mat4 projection;

        layout (location = 0) in vec4 position;
        layout (location = 1) in vec4 color;
        
        smooth out vec4 fragColor;

        void main(void)
        {
            gl_Position = projection * position;
	        fragColor = color;
        }
        """

    let fragmentSource = """
        #version 330
                
        smooth in vec4 fragColor;

        out vec4 outputColor;

        void main(void)
        {
	        outputColor = fragColor;
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
            
    let mutable vertexShader = 0
    let mutable fragmentShader = 0
    let mutable program = 0

    let mutable projectionLocation = 0
    
    let mutable verticesVbo = 0
    let mutable vao = 0

    let mutable rotation = 0.0f
    let mutable yTranslate = 0.0f
    let mutable yBounce = true

    override this.OnLoad(e) =
        vertexShader <- createShader ShaderType.VertexShader vertexSource

        fragmentShader <- createShader ShaderType.FragmentShader fragmentSource

        program <-
            let program = GL.CreateProgram()
            GL.AttachShader(program, vertexShader)
            GL.AttachShader(program, fragmentShader)
            GL.LinkProgram(program)
            let status = GL.GetProgram(program, GetProgramParameterName.LinkStatus)
            if status <> 0 then
                program
            else
                failwith (GL.GetProgramInfoLog(program))        

        verticesVbo <-
            let buffer = GL.GenBuffer()
            GL.BindBuffer(BufferTarget.ArrayBuffer, buffer)
            buffer
    
        vao <-
            let array = GL.GenVertexArray()
            GL.BindVertexArray(array)
            array

        // Get the uniform locations        
        projectionLocation <- GL.GetUniformLocation(program, "projection")
 
        // Transfer the vertices from CPU to GPU.
        GL.BufferData(BufferTarget.ArrayBuffer, nativeint(points.Length * sizeof<Vertex>), points, BufferUsageHint.StaticDraw)
        GL.BindBuffer(BufferTarget.ArrayBuffer, 0)

        // Use the program.
        GL.UseProgram(program)
        let vertexPosition = GL.GetAttribLocation(program, "position");
        let vertexColor = GL.GetAttribLocation(program, "color");
    
        GL.BindBuffer(BufferTarget.ArrayBuffer, verticesVbo)

        GL.VertexAttribPointer(vertexPosition, 4, VertexAttribPointerType.Float, false, sizeof<Vertex>, 0)
        GL.EnableVertexAttribArray(vertexPosition)

        GL.VertexAttribPointer(vertexColor, 4, VertexAttribPointerType.Float, false, sizeof<Vertex>, sizeof<Color4>)
        GL.EnableVertexAttribArray(vertexColor)


        // Enable face culling
        GL.Enable(EnableCap.CullFace)
        GL.CullFace(CullFaceMode.Back)
        GL.FrontFace(FrontFaceDirection.Ccw)

        GL.ClearColor(0.0f, 0.0f, 0.0f, 0.0f)
        
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

        rotation <- if rotation >= 360.0f then rotation - 360.0f else rotation + (10.0f * float32(e.Time))
        yBounce <- if yBounce then yTranslate >= -4.0f else yTranslate >= 4.0f
        yTranslate <- yTranslate + (if yBounce then -1.0f else +1.0f) * float32(e.Time)

        // Set projection matrix
        let projectionMatrix = 
            Matrix4.CreateRotationY(MathHelper.DegreesToRadians(rotation)) *
            Matrix4.CreateTranslation(0.0f, yTranslate, -4.0f) *
            Matrix4.CreatePerspectiveFieldOfView(MathHelper.DegreesToRadians(90.0f), float32(this.ClientSize.Width) / float32(this.ClientSize.Height), 0.1f, 10.f)

        GL.UniformMatrix4(projectionLocation, false, ref projectionMatrix)

        GL.DrawArrays(PrimitiveType.Triangles, 0, points.Length)

        this.Context.SwapBuffers()
        base.OnRenderFrame(e)

[<EntryPoint>]
let main argv = 
    use window = new Window(640, 480, Graphics.GraphicsMode.Default, "Example Window", GameWindowFlags.Default)
    window.Run()
    0
