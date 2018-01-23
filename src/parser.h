#define BL_MAX_TOKEN_SIZE 255

typedef enum _delimiter
{
  SPACE = 0,
  COMPARE,
  COMMENT,
  COMMENT_BEGIN,
  COMMENT_END,
  NEW_LINE,
  LB,
  RB,
  BEGIN,
  END,
  SET,
  COLON,
  SEMICOLON,
  CMP_TIME,
  COMMA,
  AL,
  AR,
  LESS,
  GREATER,
  STAR,
  QUOTE,
  PERC,
  DOT,
  COUNT
} delimiter_e;

typedef enum _data_type
{
  DELIMITER,
  TOKEN
} data_type_e;

typedef union _data
{
  delimiter_e delimiter;
  char token[256];
} data_u;

const char *
delimiter_to_str(delimiter_e d);

size_t
parse(char        **src,
      data_type_e  *type,
      data_u       *data);
