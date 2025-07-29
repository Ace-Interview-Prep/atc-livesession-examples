If you run in this OpenGL issue:

```sh 
[nix-shell:~/git/atc/pr/atc-livesession-examples/2025-07-12-pong]$ cabal run
OpenGLContext (OpenGLConfig {glColorPrecision = V4 8 8 8 0, glDepthPrecision = 24, glStencilPrecision = 8, glMultisampleSamples = 1, glProfile = Core Normal 3 3})
Window created successfully
pingpong: user error (OpenGL context creation failed: SDLCallFailed {sdlExceptionCaller = "SDL.Video.glCreateContext", sdlFunction = "SDL_GL_CreateContext", sdlExceptionError = "Invalid window"})
```

On NixOS (or when using Nix shells), the environment is very isolated, and the usual system OpenGL libraries/drivers might not be automatically available or correctly linked inside your shell or build environment.

In particular, the OpenGL driver bindings and native graphics libraries (like NVIDIA’s proprietary drivers or Mesa for Intel/AMD) aren’t always injected by default, causing your app’s OpenGL context creation to fail.

## [NixGL](https://github.com/nix-community/nixGL) is a helper project that bridges this gap.

- It wraps your app execution with the proper OpenGL drivers and environment variables.
- It ensures the correct GPU driver libraries are loaded inside the Nix shell.
- It detects your GPU type (NVIDIA, Intel, AMD) and loads the matching driver transparently.
- It makes sure the OpenGL context can be created successfully.

```sh
cd ..
git clone https://github.com/nix-community/nixGL.git
cd nixGL
nix build .#nixGLIntel
cd ../2025-07-12-pong/
nix-shell
../nixGL/result/bin/nixGLIntel cabal run
```

Effectively ran your app with the right OpenGL driver environment injected, enabling SDL to create a valid OpenGL context.
