;;; Minimal SDL2 bindings via Chez's FFI. Only what the platform layer uses.
(library (doom i_sdl)
  (export SDL_INIT_VIDEO SDL_INIT_AUDIO SDL_INIT_EVENTS
          SDL_WINDOWPOS_CENTERED SDL_WINDOW_RESIZABLE
          SDL_RENDERER_PRESENTVSYNC SDL_PIXELFORMAT_ARGB8888
          SDL_TEXTUREACCESS_STREAMING
          SDL_QUIT SDL_KEYDOWN SDL_KEYUP SDL_MOUSEMOTION
          SDL_MOUSEBUTTONDOWN SDL_MOUSEBUTTONUP
          sdl-init sdl-quit sdl-get-error sdl-get-ticks sdl-delay
          sdl-create-window sdl-destroy-window sdl-set-hint
          sdl-create-renderer sdl-render-set-logical-size sdl-render-clear
          sdl-render-copy sdl-render-present sdl-render-read-pixels
          sdl-get-renderer-output-size
          sdl-create-texture sdl-update-texture
          make-sdl-event sdl-poll-event sdl-event-type sdl-event-key-sym
          sdl-event-mouse-xrel sdl-event-mouse-yrel sdl-event-mouse-button)
  (import (chezscheme))

  (define lib (load-shared-object "libSDL2-2.0.so.0"))

  (define SDL_INIT_VIDEO  #x20)
  (define SDL_INIT_AUDIO  #x10)
  (define SDL_INIT_EVENTS #x4000)
  (define SDL_WINDOWPOS_CENTERED #x2FFF0000)
  (define SDL_WINDOW_RESIZABLE #x20)
  (define SDL_RENDERER_PRESENTVSYNC #x4)
  (define SDL_PIXELFORMAT_ARGB8888 #x16362004)
  (define SDL_TEXTUREACCESS_STREAMING 1)
  (define SDL_QUIT #x100)
  (define SDL_KEYDOWN #x300)
  (define SDL_KEYUP #x301)
  (define SDL_MOUSEMOTION #x400)
  (define SDL_MOUSEBUTTONDOWN #x401)
  (define SDL_MOUSEBUTTONUP #x402)

  (define sdl-init      (foreign-procedure "SDL_Init" (unsigned-32) int))
  (define sdl-quit      (foreign-procedure "SDL_Quit" () void))
  (define sdl-get-error (foreign-procedure "SDL_GetError" () string))
  (define sdl-get-ticks (foreign-procedure "SDL_GetTicks" () unsigned-32))
  (define sdl-delay     (foreign-procedure "SDL_Delay" (unsigned-32) void))
  (define sdl-set-hint  (foreign-procedure "SDL_SetHint" (string string) boolean))

  (define sdl-create-window
    (foreign-procedure "SDL_CreateWindow" (string int int int int unsigned-32) uptr))
  (define sdl-destroy-window (foreign-procedure "SDL_DestroyWindow" (uptr) void))
  (define sdl-create-renderer
    (foreign-procedure "SDL_CreateRenderer" (uptr int unsigned-32) uptr))
  (define sdl-render-set-logical-size
    (foreign-procedure "SDL_RenderSetLogicalSize" (uptr int int) int))
  (define sdl-render-clear (foreign-procedure "SDL_RenderClear" (uptr) int))
  ;; Rects are always NULL (0): whole texture to whole logical target.
  (define sdl-render-copy (foreign-procedure "SDL_RenderCopy" (uptr uptr uptr uptr) int))
  (define sdl-render-present (foreign-procedure "SDL_RenderPresent" (uptr) void))
  (define sdl-render-read-pixels
    (foreign-procedure "SDL_RenderReadPixels" (uptr uptr unsigned-32 u8* int) int))
  (define %get-output-size
    (foreign-procedure "SDL_GetRendererOutputSize" (uptr u8* u8*) int))
  ;; Returns (values width height) in physical pixels.
  (define (sdl-get-renderer-output-size r)
    (let ([w (make-bytevector 4 0)] [h (make-bytevector 4 0)])
      (%get-output-size r w h)
      (values (bytevector-s32-native-ref w 0) (bytevector-s32-native-ref h 0))))
  (define sdl-create-texture
    (foreign-procedure "SDL_CreateTexture" (uptr unsigned-32 int int int) uptr))
  (define sdl-update-texture
    (foreign-procedure "SDL_UpdateTexture" (uptr uptr u8* int) int))

  ;; SDL_Event is a 56-byte union; we read the few fields we need by offset.
  (define (make-sdl-event) (make-bytevector 56 0))
  (define sdl-poll-event (foreign-procedure "SDL_PollEvent" (u8*) int))
  (define (sdl-event-type ev) (bytevector-u32-native-ref ev 0))
  (define (sdl-event-key-sym ev) (bytevector-s32-native-ref ev 20))     ; key.keysym.sym
  (define (sdl-event-mouse-xrel ev) (bytevector-s32-native-ref ev 28))  ; motion.xrel
  (define (sdl-event-mouse-yrel ev) (bytevector-s32-native-ref ev 32))  ; motion.yrel
  (define (sdl-event-mouse-button ev) (bytevector-u8-ref ev 16)))       ; button.button
