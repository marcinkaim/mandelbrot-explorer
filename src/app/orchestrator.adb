with Ada.Text_IO;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with SDL2_Thin;   use SDL2_Thin;
with OpenGL_Thin; use OpenGL_Thin;
with Render_Interface;

package body Orchestrator is

   -------------------------------------------------------------------------
   -- Job_Queue Implementation (Ring Buffer)
   -------------------------------------------------------------------------
   protected body Job_Queue is
      procedure Push (Job : Render_Interface.Tile_Description) is
      begin
         if Count = Jobs'Length then
            Head := (Head mod Jobs'Length) + 1; -- Drop oldest
         else
            Count := Count + 1;
         end if;
         Tail := (Tail mod Jobs'Length) + 1;
         Jobs (Tail) := Job;
      end Push;

      entry Pop (Job : out Render_Interface.Tile_Description)
        when Count > 0 is
      begin
         Head := (Head mod Jobs'Length) + 1;
         Job := Jobs (Head);
         Count := Count - 1;
      end Pop;

      function Length return Natural is
      begin
         return Count;
      end Length;
   end Job_Queue;

   -------------------------------------------------------------------------
   -- Compute_Worker Implementation
   -------------------------------------------------------------------------
   task body Compute_Worker is
      My_Queue    : Job_Queue_Access;
      My_Context  : Context_Access;
      Current_Job : Render_Interface.Tile_Description;
      Should_Run  : Boolean := True;
   begin
      accept Start (Queue   : Job_Queue_Access;
                    Context : Context_Access) do
         My_Queue   := Queue;
         My_Context := Context;
      end Start;

      Ada.Text_IO.Put_Line ("[Worker] Thread Started.");

      while Should_Run loop
         select
            accept Stop do
               Should_Run := False;
            end Stop;
         else
            select
               My_Queue.Pop (Current_Job);
               -- [SIMULATION]
               Ada.Text_IO.Put_Line ("[Worker] Processed Tile: Zoom=" & 
                                     Long_Float'Image(Current_Job.Zoom));
               delay 0.01; 
            or
               delay 0.1;
            end select;
         end select;
      end loop;

      Ada.Text_IO.Put_Line ("[Worker] Thread Stopped.");
   end Compute_Worker;

   -------------------------------------------------------------------------
   -- Private Helpers (Shaders & Geometry)
   -------------------------------------------------------------------------
   
   -- Vertex Shader Source (GLSL 4.10 Core)
   VS_Source : constant String :=
     "#version 410 core" & ASCII.LF &
     "layout (location = 0) in vec2 aPos;" & ASCII.LF &
     "layout (location = 1) in vec2 aTexCoord;" & ASCII.LF &
     "out vec2 TexCoord;" & ASCII.LF &
     "void main() {" & ASCII.LF &
     "   gl_Position = vec4(aPos, 0.0, 1.0);" & ASCII.LF &
     "   TexCoord = aTexCoord;" & ASCII.LF &
     "}" & ASCII.NUL;

   -- Fragment Shader Source
   FS_Source : constant String :=
     "#version 410 core" & ASCII.LF &
     "out vec4 FragColor;" & ASCII.LF &
     "in vec2 TexCoord;" & ASCII.LF &
     "uniform sampler2D screenTexture;" & ASCII.LF &
     "void main() {" & ASCII.LF &
     "   FragColor = texture(screenTexture, TexCoord);" & ASCII.LF &
     "}" & ASCII.NUL;

   procedure Check_Shader_Error (Shader : GLuint; Pname : GLenum; Msg : String) is
      Success : aliased GLint;
      Info_Log : aliased array (1 .. 512) of char;
      Dummy   : aliased GLsizei;
   begin
      if Pname = GL_COMPILE_STATUS then
         glGetShaderiv (Shader, Pname, Success'Address);
      else
         glGetProgramiv (Shader, Pname, Success'Address);
      end if;

      if Success = 0 then
         if Pname = GL_COMPILE_STATUS then
            glGetShaderInfoLog (Shader, 512, Dummy'Access, Info_Log'Address);
         else
            glGetProgramInfoLog (Shader, 512, Dummy'Access, Info_Log'Address);
         end if;
         Ada.Text_IO.Put_Line ("[GL ERROR] " & Msg);
         raise Program_Error with "Shader Compilation/Linking Failed";
      end if;
   end Check_Shader_Error;

   function Compile_Shaders return GLuint is
      Vertex, Fragment : GLuint;
      Program : GLuint;
      VS_Ptr, FS_Ptr : chars_ptr;
      Str_Arr : aliased array (0 .. 0) of chars_ptr;
   begin
      -- 1. Vertex Shader
      Vertex := glCreateShader (GL_VERTEX_SHADER);
      VS_Ptr := New_String (VS_Source);
      Str_Arr(0) := VS_Ptr;
      glShaderSource (Vertex, 1, Str_Arr'Address, System.Null_Address);
      glCompileShader (Vertex);
      Check_Shader_Error (Vertex, GL_COMPILE_STATUS, "Vertex Shader");
      Free (VS_Ptr);

      -- 2. Fragment Shader
      Fragment := glCreateShader (GL_FRAGMENT_SHADER);
      FS_Ptr := New_String (FS_Source);
      Str_Arr(0) := FS_Ptr;
      glShaderSource (Fragment, 1, Str_Arr'Address, System.Null_Address);
      glCompileShader (Fragment);
      Check_Shader_Error (Fragment, GL_COMPILE_STATUS, "Fragment Shader");
      Free (FS_Ptr);

      -- 3. Link Program
      Program := glCreateProgram;
      glAttachShader (Program, Vertex);
      glAttachShader (Program, Fragment);
      glLinkProgram (Program);
      Check_Shader_Error (Program, GL_LINK_STATUS, "Program Linking");

      glDeleteShader (Vertex);
      glDeleteShader (Fragment);
      return Program;
   end Compile_Shaders;

   -------------------------------------------------------------------------
   -- Orchestrator Private Logic
   -------------------------------------------------------------------------

   procedure Handle_Resize (Self : in out Controller; New_W, New_H : int) is
      PBO_Ptr : GLuint_Ptr := Self.PBO_Handle'Unchecked_Access;
   begin
      if New_W <= 0 or New_H <= 0 then
         return;
      end if;

      Ada.Text_IO.Put_Line ("[Orchestrator] Window Resized: " & 
                            int'Image(New_W) & "x" & int'Image(New_H));

      -- 1. Update State
      Self.Width  := New_W;
      Self.Height := New_H;

      -- 2. Update Viewport
      glViewport (0, 0, GLsizei(Self.Width), GLsizei(Self.Height));

      -- 3. Reallocate Texture
      -- Note: glDeleteTextures takes address of the handle
      glDeleteTextures (1, Self.Texture_Handle'Address);
      
      glGenTextures (1, Self.Texture_Handle'Address);
      glBindTexture (GL_TEXTURE_2D, Self.Texture_Handle);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GLint(GL_NEAREST));
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GLint(GL_NEAREST));
      
      -- Allocate new texture storage
      glTexImage2D 
        (GL_TEXTURE_2D, 0, GLint(GL_RGBA8), 
         GLsizei(Self.Width), GLsizei(Self.Height), 
         0, GL_RGBA, GL_UNSIGNED_BYTE, System.Null_Address);

      -- 4. Reallocate PBO
      -- Note: glDeleteBuffers takes a pointer to GLuint
      glDeleteBuffers (1, PBO_Ptr);
      
      glGenBuffers (1, Self.PBO_Handle'Address);
      glBindBuffer (GL_PIXEL_UNPACK_BUFFER, Self.PBO_Handle);
      
      -- Reallocate PBO storage
      glBufferData 
        (GL_PIXEL_UNPACK_BUFFER, 
         Long_Integer(Self.Width * Self.Height * 4), 
         System.Null_Address, 
         GL_STREAM_DRAW);
         
      glBindBuffer (GL_PIXEL_UNPACK_BUFFER, 0);

      -- 5. Force Redraw
      Self.Queue.Push ((0, 0, Self.Width, Self.Height, 0.0, 0.0, Self.Zoom_Level));
   end Handle_Resize;

   -------------------------------------------------------------------------
   -- Orchestrator Lifecycle
   -------------------------------------------------------------------------

   procedure Initialize (Self : in out Controller) is
      Res : SDL_Result;
      GL_Attr_Res : int;
      
      -- Quad Vertices (Pos: X,Y, Tex: U,V)
      type Vertex_Data is array (1 .. 24) of GLfloat;
      Vertices : aliased Vertex_Data :=
        (
         -1.0,  1.0,  0.0, 1.0, -- Top Left
         -1.0, -1.0,  0.0, 0.0, -- Bot Left
          1.0, -1.0,  1.0, 0.0, -- Bot Right
         
         -1.0,  1.0,  0.0, 1.0, -- Top Left
          1.0, -1.0,  1.0, 0.0, -- Bot Right
          1.0,  1.0,  1.0, 1.0  -- Top Right
        );
      Queue_Ptr : Job_Queue_Access;
      Ctx_Ptr   : Context_Access;
   begin
      -- 1. SDL Init
      Res := SDL_Init (SDL_INIT_VIDEO);
      if Res /= SDL_SUCCESS then
         raise Program_Error with "SDL_Init failed";
      end if;

      -- 2. GL Attributes (Core 4.1)
      GL_Attr_Res := SDL_GL_SetAttribute (SDL_GL_CONTEXT_MAJOR_VERSION, 4);
      GL_Attr_Res := SDL_GL_SetAttribute (SDL_GL_CONTEXT_MINOR_VERSION, 1);
      GL_Attr_Res := SDL_GL_SetAttribute (SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
      GL_Attr_Res := SDL_GL_SetAttribute (SDL_GL_DOUBLEBUFFER, 1);

      -- 3. Create Window
      declare
         Title_C : chars_ptr := New_String ("Mandelbrot Explorer (Ada/CUDA)");
      begin
         Self.Window_Handle := SDL_CreateWindow 
           (Title_C, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 
            Self.Width, Self.Height, 
            SDL_WINDOW_OPENGL or SDL_WINDOW_SHOWN or SDL_WINDOW_RESIZABLE);
         Free (Title_C);
      end;
      
      if Self.Window_Handle = null then
         raise Program_Error with "SDL_CreateWindow failed: " & Value (SDL_GetError);
      end if;

      -- 4. Create Context
      Self.GL_Context := SDL_GL_CreateContext (Self.Window_Handle);
      if Self.GL_Context = SDL_GLContext (System.Null_Address) then
         raise Program_Error with "SDL_GL_CreateContext failed";
      end if;

      Ada.Text_IO.Put_Line ("[Orchestrator] GL Context Initialized.");

      -- 5. Compile Shaders
      Self.Program_Handle := Compile_Shaders;
      glUseProgram (Self.Program_Handle);
      
      declare
         Name_C : aliased char_array := To_C ("screenTexture");
      begin
         glUniform1i (glGetUniformLocation (Self.Program_Handle, Name_C'Address), 0);
      end;

      -- 6. Setup Geometry (VAO/VBO)
      glGenVertexArrays (1, Self.VAO_Handle'Address);
      glGenBuffers (1, Self.VBO_Handle'Address);
      
      glBindVertexArray (Self.VAO_Handle);
      glBindBuffer (GL_ARRAY_BUFFER, Self.VBO_Handle);
      glBufferData (GL_ARRAY_BUFFER, Vertices'Size / 8, Vertices'Address, GL_STATIC_DRAW);
      
      -- Pos Attribute
      glVertexAttribPointer (0, 2, GL_FLOAT, GL_FALSE, 4 * 4, System.Null_Address);
      glEnableVertexAttribArray (0);
      
      -- Tex Attribute (Offset = 2 * sizeof(float) = 8)
      glVertexAttribPointer (1, 2, GL_FLOAT, GL_FALSE, 4 * 4, To_Address(8));
      glEnableVertexAttribArray (1);

      -- 7. Setup Texture & PBO
      -- Texture
      glGenTextures (1, Self.Texture_Handle'Address);
      glBindTexture (GL_TEXTURE_2D, Self.Texture_Handle);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GLint(GL_NEAREST));
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GLint(GL_NEAREST));
      glTexImage2D (GL_TEXTURE_2D, 0, GLint(GL_RGBA8), Self.Width, Self.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, System.Null_Address);
      
      -- PBO
      glGenBuffers (1, Self.PBO_Handle'Address);
      glBindBuffer (GL_PIXEL_UNPACK_BUFFER, Self.PBO_Handle);
      glBufferData (GL_PIXEL_UNPACK_BUFFER, Long_Integer(Self.Width * Self.Height * 4), System.Null_Address, GL_STREAM_DRAW);
      glBindBuffer (GL_PIXEL_UNPACK_BUFFER, 0); -- Unbind

      Ada.Text_IO.Put_Line ("[Orchestrator] Graphics Resources Allocated.");

      -- 8. Start Worker
      -- FIX CRIT-02: Removing Unchecked_Conversion for pointers.
      -- Assuming Queue and Context are aliased in spec and accessible directly.
      -- However, since the types are distinct, Unchecked_Access is sometimes valid for passing strictly typed pointers
      -- if the receiving type is defined as 'access all'.
      Queue_Ptr := Self.Queue'Unchecked_Access;
      Ctx_Ptr   := Self.Context'Unchecked_Access;
      
      Self.Worker.Start (Queue_Ptr, Ctx_Ptr);
      Self.Is_Running := True;
   end Initialize;

   procedure Run_UI_Loop (Self : in out Controller) is
      Event : aliased SDL_Event;
   begin
      while Self.Is_Running loop
         -- 1. Poll Events
         while SDL_PollEvent (Event'Access) /= 0 loop
            if Event.Common.type_field = SDL_QUIT_EVENT then
               Self.Is_Running := False;
            
            -- Handle Window Resize
            elsif Event.Common.type_field = SDL_WINDOWEVENT then
               if Event.Window.event = SDL_WINDOWEVENT_RESIZED then
                  Handle_Resize (Self, Event.Window.data1, Event.Window.data2);
               end if;

            elsif Event.Common.type_field = SDL_KEYDOWN then
               -- Simple Navigation Logic
               if Event.Key.keysym.sym = SDLK_ESCAPE then
                  Self.Is_Running := False;
               elsif Event.Key.keysym.sym = SDLK_w then
                  Self.Zoom_Level := Self.Zoom_Level * 1.1;
                  Ada.Text_IO.Put_Line ("Zoom In: " & Long_Float'Image(Self.Zoom_Level));
                  Self.Queue.Push ((0,0, Self.Width, Self.Height, 0.0, 0.0, Self.Zoom_Level));
               elsif Event.Key.keysym.sym = SDLK_s then
                  Self.Zoom_Level := Self.Zoom_Level / 1.1;
                  Ada.Text_IO.Put_Line ("Zoom Out: " & Long_Float'Image(Self.Zoom_Level));
                  Self.Queue.Push ((0,0, Self.Width, Self.Height, 0.0, 0.0, Self.Zoom_Level));
               end if;
            end if;
         end loop;

         -- 2. Render
         glClearColor (0.2, 0.3, 0.3, 1.0);
         glClear (GL_COLOR_BUFFER_BIT);

         glActiveTexture (GL_TEXTURE0);
         glBindTexture (GL_TEXTURE_2D, Self.Texture_Handle);

         -- Update texture from PBO
         glBindBuffer (GL_PIXEL_UNPACK_BUFFER, Self.PBO_Handle);
         glTexSubImage2D (GL_TEXTURE_2D, 0, 0, 0, Self.Width, Self.Height, GL_RGBA, GL_UNSIGNED_BYTE, System.Null_Address);
         glBindBuffer (GL_PIXEL_UNPACK_BUFFER, 0);

         -- Draw Quad
         glBindVertexArray (Self.VAO_Handle);
         glDrawArrays (GL_TRIANGLES, 0, 6);
         glBindVertexArray (0);

         SDL_GL_SwapWindow (Self.Window_Handle);
      end loop;
   end Run_UI_Loop;

   procedure Shutdown (Self : in out Controller) is
      VBO_Ptr : GLuint_Ptr := Self.VBO_Handle'Unchecked_Access;
      PBO_Ptr : GLuint_Ptr := Self.PBO_Handle'Unchecked_Access;
   begin
      Self.Worker.Stop;
      
      Ada.Text_IO.Put_Line ("[Orchestrator] Cleaning up GL resources...");

      -- 1. Buffers (VBO, PBO)
      if Self.VBO_Handle /= 0 then
         glDeleteBuffers (1, VBO_Ptr);
      end if;
      if Self.PBO_Handle /= 0 then
         glDeleteBuffers (1, PBO_Ptr);
      end if;

      -- 2. Vertex Array
      if Self.VAO_Handle /= 0 then
         glDeleteVertexArrays (1, Self.VAO_Handle'Address);
      end if;

      -- 3. Textures
      if Self.Texture_Handle /= 0 then
         glDeleteTextures (1, Self.Texture_Handle'Address);
      end if;

      -- 4. Shader Program
      if Self.Program_Handle /= 0 then
         glDeleteProgram (Self.Program_Handle);
      end if;

      -- 5. Context & Window
      SDL_GL_DeleteContext (Self.GL_Context);
      SDL_DestroyWindow (Self.Window_Handle);
      SDL_Quit;
      
      Ada.Text_IO.Put_Line ("[Orchestrator] Shutdown complete.");
   end Shutdown;

end Orchestrator;