#include <stdio.h>
#include <SDL/SDL.h>

#define WIDTH	320
#define HEIGHT	200

void init(void);
void display(int frame);

unsigned char *fb;
unsigned int msec;

static SDL_Surface *surf;

static unsigned int prev_fps_update;
static int nframes;

int main(void)
{
	unsigned int start_time;
	unsigned char *src, *dest;

	SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER);

	if(!(surf = SDL_SetVideoMode(WIDTH, HEIGHT, 16, SDL_SWSURFACE))) {
		fprintf(stderr, "failed to set video mode\n");
		return 1;
	}
	SDL_WM_SetCaption("tinyray", "tinyray");

	fb = malloc(WIDTH * HEIGHT * 2);

	start_time = prev_fps_update = SDL_GetTicks();
	for(;;) {
		SDL_Event ev;
		while(SDL_PollEvent(&ev)) {
			switch(ev.type) {
			case SDL_QUIT:
			case SDL_KEYDOWN:
				goto done;
			default:
				break;
			}
		}

		msec = SDL_GetTicks() - start_time;
		display(msec / 70);

		if(SDL_MUSTLOCK(surf)) {
			SDL_LockSurface(surf);
		}

		src = fb;
		dest = surf->pixels;
		memcpy(dest, src, WIDTH * HEIGHT * 2);

		if(SDL_MUSTLOCK(surf)) {
			SDL_UnlockSurface(surf);
		}

		SDL_Flip(surf);
		++nframes;
		{
			unsigned int fps_time = msec - prev_fps_update;
			if(fps_time >= 1500) {
				char buf[64];
				float fps = (float)nframes * 1000.0 / (float)fps_time;
				nframes = 0;
				prev_fps_update = msec;
				sprintf(buf, "tinyray [fps: %.1f]\n", fps);
				SDL_WM_SetCaption(buf, buf);
			}
		}
	}

done:
	SDL_Quit();
	return 0;
}
