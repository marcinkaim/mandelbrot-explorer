with Interfaces.C; use Interfaces.C;
with System;

package OpenGL_Thin is
   pragma Preelaborate;

   ---------------------------------------------------------------------------
   -- Basic Types (compatible with <GL/gl.h> i <GL/glext.h>)
   ---------------------------------------------------------------------------
   subtype GLenum is unsigned;
   subtype GLboolean is unsigned_char;
   subtype GLbitfield is unsigned;
   subtype GLvoid is System.Address;
   subtype GLbyte is signed_char;
   subtype GLshort is short;
   subtype GLint is int;
   subtype GLubyte is unsigned_char;
   subtype GLushort is unsigned_short;
   subtype GLuint is unsigned;
   subtype GLsizei is int;
   subtype GLfloat is C_float;
   subtype GLdouble is double;
   subtype GLchar is char;
   
   type GLint_Ptr is access all GLint;
   type GLuint_Ptr is access all GLuint;
   type GLchar_Ptr is access all GLchar;
   type GLvoid_Ptr is new System.Address;

   GL_FALSE : constant GLboolean := 0;
   GL_TRUE  : constant GLboolean := 1;

   ---------------------------------------------------------------------------
   -- OpenGL Constants (Core Profile)
   ---------------------------------------------------------------------------
   
   GL_ARRAY_BUFFER         : constant GLenum := 16#8892#; -- VBO
   GL_PIXEL_UNPACK_BUFFER  : constant GLenum := 16#88EC#; -- PBO (Important for CUDA!)
   GL_STATIC_DRAW          : constant GLenum := 16#88E4#;
   GL_STREAM_DRAW          : constant GLenum := 16#88E0#;
   GL_DYNAMIC_DRAW         : constant GLenum := 16#88E8#;

   GL_BYTE           : constant GLenum := 16#1400#;
   GL_UNSIGNED_BYTE  : constant GLenum := 16#1401#;
   GL_FLOAT          : constant GLenum := 16#1406#;

   GL_TEXTURE_2D     : constant GLenum := 16#0DE1#;
   GL_TEXTURE0       : constant GLenum := 16#84C0#;
   GL_RGBA           : constant GLenum := 16#1908#;
   GL_RGBA8          : constant GLenum := 16#8058#;

   GL_TEXTURE_MAG_FILTER : constant GLenum := 16#2800#;
   GL_TEXTURE_MIN_FILTER : constant GLenum := 16#2801#;
   GL_NEAREST        : constant GLenum := 16#2600#;
   GL_LINEAR         : constant GLenum := 16#2601#;

   GL_TRIANGLES      : constant GLenum := 16#0004#;
   GL_TRIANGLE_STRIP : constant GLenum := 16#0005#;

   GL_FRAGMENT_SHADER : constant GLenum := 16#8B30#;
   GL_VERTEX_SHADER   : constant GLenum := 16#8B31#;
   
   GL_COMPILE_STATUS  : constant GLenum := 16#8B81#;
   GL_LINK_STATUS     : constant GLenum := 16#8B82#;
   GL_INFO_LOG_LENGTH : constant GLenum := 16#8B84#;

   GL_COLOR_BUFFER_BIT : constant GLbitfield := 16#00004000#;

   ---------------------------------------------------------------------------
   -- API Functions (Import)
   ---------------------------------------------------------------------------

   procedure glViewport (x, y : GLint; width, height : GLsizei);
   pragma Import (C, glViewport, "glViewport");

   procedure glClearColor (red, green, blue, alpha : GLfloat);
   pragma Import (C, glClearColor, "glClearColor");

   procedure glClear (mask : GLbitfield);
   pragma Import (C, glClear, "glClear");

   procedure glGenBuffers (n : GLsizei; buffers : System.Address);
   pragma Import (C, glGenBuffers, "glGenBuffers");

   procedure glBindBuffer (target : GLenum; buffer : GLuint);
   pragma Import (C, glBindBuffer, "glBindBuffer");

   procedure glBufferData (target : GLenum; size : Long_Integer; data : System.Address; usage : GLenum);
   pragma Import (C, glBufferData, "glBufferData");

   procedure glDeleteBuffers (n : GLsizei; buffers : GLuint_Ptr);
   pragma Import (C, glDeleteBuffers, "glDeleteBuffers");

   procedure glGenVertexArrays (n : GLsizei; arrays : System.Address);
   pragma Import (C, glGenVertexArrays, "glGenVertexArrays");

   procedure glBindVertexArray (array_obj : GLuint);
   pragma Import (C, glBindVertexArray, "glBindVertexArray");

   procedure glEnableVertexAttribArray (index : GLuint);
   pragma Import (C, glEnableVertexAttribArray, "glEnableVertexAttribArray");

   procedure glVertexAttribPointer 
     (index      : GLuint; 
      size       : GLint; 
      c_type     : GLenum; 
      normalized : GLboolean; 
      stride     : GLsizei; 
      pointer    : System.Address); -- Offset in VBO
   pragma Import (C, glVertexAttribPointer, "glVertexAttribPointer");

   procedure glDeleteVertexArrays (n : GLsizei; arrays : System.Address);
   pragma Import (C, glDeleteVertexArrays, "glDeleteVertexArrays");

   procedure glGenTextures (n : GLsizei; textures : System.Address);
   pragma Import (C, glGenTextures, "glGenTextures");
   
   procedure glDeleteTextures (n : GLsizei; textures : System.Address);
   pragma Import (C, glDeleteTextures, "glDeleteTextures");

   procedure glActiveTexture (texture : GLenum);
   pragma Import (C, glActiveTexture, "glActiveTexture");

   procedure glBindTexture (target : GLenum; texture : GLuint);
   pragma Import (C, glBindTexture, "glBindTexture");

   procedure glTexParameteri (target : GLenum; pname : GLenum; param : GLint);
   pragma Import (C, glTexParameteri, "glTexParameteri");

   -- Texture allocation (Mutable storage)
   procedure glTexImage2D 
     (target         : GLenum; 
      level          : GLint; 
      internalformat : GLint; 
      width          : GLsizei; 
      height         : GLsizei; 
      border         : GLint; 
      format         : GLenum; 
      c_type         : GLenum; 
      pixels         : System.Address);
   pragma Import (C, glTexImage2D, "glTexImage2D");

   -- Texture update (e.g. from PBO)
   procedure glTexSubImage2D
     (target  : GLenum;
      level   : GLint;
      xoffset : GLint;
      yoffset : GLint;
      width   : GLsizei;
      height  : GLsizei;
      format  : GLenum;
      c_type  : GLenum;
      pixels  : System.Address);
   pragma Import (C, glTexSubImage2D, "glTexSubImage2D");

   function glCreateShader (shaderType : GLenum) return GLuint;
   pragma Import (C, glCreateShader, "glCreateShader");

   procedure glShaderSource 
     (shader : GLuint; 
      count  : GLsizei; 
      string : System.Address; -- chars_ptr_array
      length : System.Address);
   pragma Import (C, glShaderSource, "glShaderSource");

   procedure glCompileShader (shader : GLuint);
   pragma Import (C, glCompileShader, "glCompileShader");

   -- Getting compilation/linking status (iv = integer vector)
   procedure glGetShaderiv (shader : GLuint; pname : GLenum; params : System.Address);
   pragma Import (C, glGetShaderiv, "glGetShaderiv");

   procedure glGetShaderInfoLog 
     (shader  : GLuint; 
      bufSize : GLsizei; 
      length  : access GLsizei; 
      infoLog : System.Address);
   pragma Import (C, glGetShaderInfoLog, "glGetShaderInfoLog");

   function glCreateProgram return GLuint;
   pragma Import (C, glCreateProgram, "glCreateProgram");

   procedure glAttachShader (program : GLuint; shader : GLuint);
   pragma Import (C, glAttachShader, "glAttachShader");

   procedure glLinkProgram (program : GLuint);
   pragma Import (C, glLinkProgram, "glLinkProgram");

   procedure glGetProgramiv (program : GLuint; pname : GLenum; params : System.Address);
   pragma Import (C, glGetProgramiv, "glGetProgramiv");

   procedure glGetProgramInfoLog 
     (program : GLuint; 
      bufSize : GLsizei; 
      length  : access GLsizei; 
      infoLog : System.Address);
   pragma Import (C, glGetProgramInfoLog, "glGetProgramInfoLog");

   procedure glUseProgram (program : GLuint);
   pragma Import (C, glUseProgram, "glUseProgram");

   function glGetUniformLocation (program : GLuint; name : System.Address) return GLint;
   pragma Import (C, glGetUniformLocation, "glGetUniformLocation");

   procedure glUniform1i (location : GLint; v0 : GLint);
   pragma Import (C, glUniform1i, "glUniform1i");

   procedure glDeleteShader (shader : GLuint);
   pragma Import (C, glDeleteShader, "glDeleteShader");

   procedure glDeleteProgram (program : GLuint);
   pragma Import (C, glDeleteProgram, "glDeleteProgram");

   procedure glDrawArrays (mode : GLenum; first : GLint; count : GLsizei);
   pragma Import (C, glDrawArrays, "glDrawArrays");

end OpenGL_Thin;