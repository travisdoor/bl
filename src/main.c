#include <stdio.h>
#include <string.h>

enum delimiter
{
  NONE      = 0,
  SPACE     = ' ',
  LB        = '{',
  RB        = '}',
  LBF       = '(',
  RBF       = ')',
  COMMA     = ',',
  SEMICOLON = ';'
};

int main(int argc, char *argv[])
{
  char src[] = " int   main( ) { return 0; }";
  char *curr = &src[0];
  char *prev = &src[0];
  char buf[256];
  enum delimiter dcurr = NONE;

  printf("src: '%s'\n", src);
  while (*curr != '\0')
  {
    switch (*curr) {
      case SPACE:
        /*puts("> space"); */
        dcurr = SPACE;
        break;
      case LB:
        /*puts("> {"); */
        dcurr = LB;
        break;
      case RB:
        /*puts("> }"); */
        dcurr = RB;
        break;
      case LBF:
        /*puts("> ("); */
        dcurr = LBF;
        break;
      case RBF:
        /*puts("> )"); */
        dcurr = RBF;
        break;
      case COMMA:
        /*puts("> ,"); */
        dcurr = COMMA;
        break;
      case SEMICOLON:
        /*puts("> ;");*/
        dcurr = SEMICOLON;
        break;
      default:
        /*puts("none");*/
        dcurr = NONE;
        break; 
    }

    if (dcurr != NONE) {
      size_t l = curr - prev;
      if (l > 0) {
        memcpy(&buf[0], prev, l);
        buf[l] = '\0';
        puts(buf);
      }
      prev = curr + 1;
    }

    curr++;
  }
  printf ("\n");

  return 0;
}
