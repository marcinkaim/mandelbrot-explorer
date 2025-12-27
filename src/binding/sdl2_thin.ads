with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package SDL2_Thin is
   pragma Preelaborate;

   ---------------------------------------------------------------------------
   -- Basic Types and Constants
   ---------------------------------------------------------------------------
   
   subtype SDL_Result is int;
   SDL_SUCCESS : constant SDL_Result := 0;

   SDL_INIT_VIDEO : constant unsigned := 16#0000_0020#;
   SDL_WINDOW_OPENGL     : constant unsigned := 16#0000_0002#;
   SDL_WINDOW_SHOWN      : constant unsigned := 16#0000_0004#;
   SDL_WINDOW_RESIZABLE  : constant unsigned := 16#0000_0020#;
   SDL_WINDOWPOS_CENTERED : constant int := 16#2FFF_0000#;

   type SDL_GLattr is
     (SDL_GL_RED_SIZE,
      SDL_GL_GREEN_SIZE,
      SDL_GL_BLUE_SIZE,
      SDL_GL_ALPHA_SIZE,
      SDL_GL_BUFFER_SIZE,
      SDL_GL_DOUBLEBUFFER,
      SDL_GL_DEPTH_SIZE,
      SDL_GL_STENCIL_SIZE,
      SDL_GL_ACCUM_RED_SIZE,
      SDL_GL_ACCUM_GREEN_SIZE,
      SDL_GL_ACCUM_BLUE_SIZE,
      SDL_GL_ACCUM_ALPHA_SIZE,
      SDL_GL_STEREO,
      SDL_GL_MULTISAMPLEBUFFERS,
      SDL_GL_MULTISAMPLESAMPLES,
      SDL_GL_ACCELERATED_VISUAL,
      SDL_GL_RETAINED_BACKING,
      SDL_GL_CONTEXT_MAJOR_VERSION,
      SDL_GL_CONTEXT_MINOR_VERSION,
      SDL_GL_CONTEXT_EGL,
      SDL_GL_CONTEXT_FLAGS,
      SDL_GL_CONTEXT_PROFILE_MASK,
      SDL_GL_SHARE_WITH_CURRENT_CONTEXT,
      SDL_GL_FRAMEBUFFER_SRGB_CAPABLE,
      SDL_GL_CONTEXT_RELEASE_BEHAVIOR);

   for SDL_GLattr use
     (SDL_GL_RED_SIZE                   => 0,
      SDL_GL_GREEN_SIZE                 => 1,
      SDL_GL_BLUE_SIZE                  => 2,
      SDL_GL_ALPHA_SIZE                 => 3,
      SDL_GL_BUFFER_SIZE                => 4,
      SDL_GL_DOUBLEBUFFER               => 5,
      SDL_GL_DEPTH_SIZE                 => 6,
      SDL_GL_STENCIL_SIZE               => 7,
      SDL_GL_ACCUM_RED_SIZE             => 8,
      SDL_GL_ACCUM_GREEN_SIZE           => 9,
      SDL_GL_ACCUM_BLUE_SIZE            => 10,
      SDL_GL_ACCUM_ALPHA_SIZE           => 11,
      SDL_GL_STEREO                     => 12,
      SDL_GL_MULTISAMPLEBUFFERS         => 13,
      SDL_GL_MULTISAMPLESAMPLES         => 14,
      SDL_GL_ACCELERATED_VISUAL         => 15,
      SDL_GL_RETAINED_BACKING           => 16,
      SDL_GL_CONTEXT_MAJOR_VERSION      => 17,
      SDL_GL_CONTEXT_MINOR_VERSION      => 18,
      SDL_GL_CONTEXT_EGL                => 19,
      SDL_GL_CONTEXT_FLAGS              => 20,
      SDL_GL_CONTEXT_PROFILE_MASK       => 21,
      SDL_GL_SHARE_WITH_CURRENT_CONTEXT => 22,
      SDL_GL_FRAMEBUFFER_SRGB_CAPABLE   => 23,
      SDL_GL_CONTEXT_RELEASE_BEHAVIOR   => 24);

   SDL_GL_CONTEXT_PROFILE_CORE : constant int := 16#0001#;

   ---------------------------------------------------------------------------
   -- Opaque Handles (Safe Types)
   ---------------------------------------------------------------------------
   
   type SDL_Window is null record;
   type SDL_Window_Ptr is access all SDL_Window;
   pragma Convention (C, SDL_Window_Ptr);

   type SDL_GLContext is new System.Address;

   ---------------------------------------------------------------------------
   -- Events - C Union Mapping
   ---------------------------------------------------------------------------

   type SDL_EventType is new unsigned;
   SDL_QUIT_EVENT  : constant SDL_EventType := 16#100#;
   SDL_WINDOWEVENT : constant SDL_EventType := 16#200#; 
   SDL_KEYDOWN     : constant SDL_EventType := 16#300#;
   SDL_KEYUP       : constant SDL_EventType := 16#301#;

   SDL_WINDOWEVENT_RESIZED : constant unsigned_char := 5;

   type SDL_Keycode is new int;
   SDLK_ESCAPE : constant SDL_Keycode := 27;
   SDLK_w      : constant SDL_Keycode := 119;
   SDLK_a      : constant SDL_Keycode := 97;
   SDLK_s      : constant SDL_Keycode := 115;
   SDLK_d      : constant SDL_Keycode := 100;

   type SDL_Keysym is record
      scancode : int;
      sym      : SDL_Keycode;
      mod_flags: unsigned_short;
      unused   : unsigned;
   end record;
   pragma Convention (C, SDL_Keysym);

   type SDL_KeyboardEvent is record
      type_field : SDL_EventType;
      timestamp  : unsigned;
      windowID   : unsigned;
      state      : unsigned_char;
      repeat     : unsigned_char;
      padding2   : unsigned_char;
      padding3   : unsigned_char;
      keysym     : SDL_Keysym;
   end record;
   pragma Convention (C, SDL_KeyboardEvent);

   type SDL_Window_Event is record
      type_field : SDL_EventType;
      timestamp  : unsigned;
      windowID   : unsigned;
      event      : unsigned_char;
      padding1   : unsigned_char;
      padding2   : unsigned_char;
      padding3   : unsigned_char;
      data1      : int;           -- Width
      data2      : int;           -- Height
   end record;
   pragma Convention (C, SDL_Window_Event);

   type SDL_QuitEvent is record
      type_field : SDL_EventType;
      timestamp  : unsigned;
   end record;
   pragma Convention (C, SDL_QuitEvent);

   type SDL_CommonEvent is record
      type_field : SDL_EventType;
      timestamp  : unsigned;
   end record;
   pragma Convention (C, SDL_CommonEvent);

   type Event_Padding_Array is array (1 .. 56) of unsigned_char;

   type SDL_Event (Event_Type : SDL_EventType := 0) is record
      case Event_Type is
         when SDL_QUIT_EVENT =>
            Quit : SDL_QuitEvent;
         when SDL_WINDOWEVENT =>
            Window : SDL_Window_Event;
         when SDL_KEYDOWN | SDL_KEYUP =>
            Key  : SDL_KeyboardEvent;
         when others =>
            Common : SDL_CommonEvent;
            Padding : Event_Padding_Array; 
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Event);
   pragma Unchecked_Union (SDL_Event);

   ---------------------------------------------------------------------------
   -- API Functions (Import)
   ---------------------------------------------------------------------------

   -- System
   function SDL_Init (flags : unsigned) return SDL_Result;
   pragma Import (C, SDL_Init, "SDL_Init");

   procedure SDL_Quit;
   pragma Import (C, SDL_Quit, "SDL_Quit");

   function SDL_GetError return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, SDL_GetError, "SDL_GetError");

   -- Window
   function SDL_CreateWindow 
     (title    : Interfaces.C.Strings.chars_ptr;
      x, y     : int;
      w, h     : int;
      flags    : unsigned) return SDL_Window_Ptr;
   pragma Import (C, SDL_CreateWindow, "SDL_CreateWindow");

   procedure SDL_DestroyWindow (window : SDL_Window_Ptr);
   pragma Import (C, SDL_DestroyWindow, "SDL_DestroyWindow");

   -- OpenGL Context
   function SDL_GL_SetAttribute (attr : SDL_GLattr; value : int) return int;
   pragma Import (C, SDL_GL_SetAttribute, "SDL_GL_SetAttribute");

   function SDL_GL_CreateContext (window : SDL_Window_Ptr) return SDL_GLContext;
   pragma Import (C, SDL_GL_CreateContext, "SDL_GL_CreateContext");
   
   procedure SDL_GL_DeleteContext (context : SDL_GLContext);
   pragma Import (C, SDL_GL_DeleteContext, "SDL_GL_DeleteContext");

   procedure SDL_GL_SwapWindow (window : SDL_Window_Ptr);
   pragma Import (C, SDL_GL_SwapWindow, "SDL_GL_SwapWindow");

   -- Events
   function SDL_PollEvent (event : access SDL_Event) return int;
   pragma Import (C, SDL_PollEvent, "SDL_PollEvent");

end SDL2_Thin;