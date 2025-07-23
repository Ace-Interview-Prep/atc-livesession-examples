If you run in this OpenGL issue:

```sh 
[nix-shell:~/git/atc/pr/atc-livesession-examples/2025-07-12-pong]$ cabal run
OpenGLContext (OpenGLConfig {glColorPrecision = V4 8 8 8 0, glDepthPrecision = 24, glStencilPrecision = 8, glMultisampleSamples = 1, glProfile = Core Normal 3 3})
Window created successfully
pingpong: user error (OpenGL context creation failed: SDLCallFailed {sdlExceptionCaller = "SDL.Video.glCreateContext", sdlFunction = "SDL_GL_CreateContext", sdlExceptionError = "Invalid window"})
```

You need [NixGL](https://github.com/nix-community/nixGL)

```sh
cd ..
git clone https://github.com/nix-community/nixGL.git
cd nixGL
nix build .#nixGLIntel
cd ../2025-07-12-pong/
nix-shell
../nixGL/result/bin/nixGLIntel cabal run
```

