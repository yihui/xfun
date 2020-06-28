#include <stdlib.h>
#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <Rversion.h>

const char base64_table[65] = {
  'A','B','C','D','E','F','G','H',
  'I','J','K','L','M','N','O','P',
  'Q','R','S','T','U','V','W','X',
  'Y','Z','a','b','c','d','e','f',
  'g','h','i','j','k','l','m','n',
  'o','p','q','r','s','t','u','v',
  'w','x','y','z','0','1','2','3',
  '4','5','6','7','8','9','+','/',
  '%'
};

const char padding = '=';
const unsigned char upadding = '=';

void base64_encode_impl(
  const unsigned char* input,
  const R_xlen_t input_len,
  char* output,
  const R_xlen_t output_len
) {
  R_xlen_t input_len_left = input_len;
  R_xlen_t i1 = 0;
  R_xlen_t i2 = 0;
  while(input_len_left > 2) {
    output[i2++] = base64_table[input[i1] / 4];
    output[i2++] = base64_table[16 * (input[i1] % 4) + input[ i1 + 1 ] / 16];
    output[i2++] = base64_table[4 * (input [ i1 + 1] % 16) + input[i1 + 2] / 64];
    output[i2++] = base64_table[input[i1 + 2] % 64];
    i1 += 3;
    input_len_left -= 3;
  }
  if (input_len_left) {
    output[i2++] = base64_table[input[i1] / 4];
    if (input_len_left > 1) {
      output[i2++] = base64_table[16 * (input[i1] % 4) + input[ i1 + 1 ] / 16];
      output[i2++] = base64_table[4 * (input [i1 + 1] % 16)];
      output[i2++] = padding;
    } else {
      output[i2++] = base64_table[16 * (input[i1] % 4)];
      output[i2++] = padding;
      output[i2++] = padding;

    }
  }
}

static const short base64_reverse_table[256] = { -2, -2, -2, -2, -2, -2, -2, -2,
		-2, -1, -1, -2, -2, -1, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
		-2, -2, -2, -2, -2, -2, -1, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, 62,
		-2, -2, -2, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -2, -2, -2, -2,
		-2, -2, -2, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
		17, 18, 19, 20, 21, 22, 23, 24, 25, -2, -2, -2, -2, -2, -2, 26, 27, 28,
		29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,
		47, 48, 49, 50, 51, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
		-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
		-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
		-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
		-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
		-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
		-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
		-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2 };

int base64_decode_impl(
  const unsigned char* input,
  R_xlen_t input_len,
  unsigned char* output,
  const R_xlen_t output_len
) {
  for (R_xlen_t i = 0;i < output_len;i++) {
    output[i] = 0;
  }
  int ch = 0;
  R_xlen_t i = 0, j = 0, k = 0;
  while(input_len-- > 0) {
    ch = *input++;
    if (ch == padding) {
      if (*input != upadding && (i % 4) == 1) {
        return 1;
      }
      continue;
    }
    ch = base64_reverse_table[ch];
    if (ch == -1) {
      continue;
    }
    else if (ch == -2) {
      return 1;
    }
    switch (i % 4) {
      case 0 : {
        output[j] = ch << 2;
        break;
      }
      case 1 : {
        output[j++] |= ch >> 4;
        output[j] = (ch & 0x0f) << 4;
        break;
      }
      case 2 : {
        output[j++] |= ch >> 2;
        output[j] = (ch & 0x03) << 6;
        break;
      }
      case 3 : {
        output[j++] |= ch;
        break;
      }
    }
    i++;
  }
  k = j;
  if (ch == padding ) {
    switch (i % 4) {
      case 1 : {
        return 1;
      }
      case 2 : k++;
      case 3 : output[k] = 0;
    }
  }
  if (j != output_len) return 1;
  return 0;
}

SEXP base64_enc(SEXP input) {
  int rv = 0;
  // get input
  R_xlen_t input_len;
#if defined(R_VERSION) && R_VERSION >= R_Version(3,0,0)
  input_len = XLENGTH(input);
#else
  input_len = LENGTH(input);
#endif
  R_xlen_t output_len = input_len / 3 * 4;
  if (input_len % 3) {
    output_len += 4;
  }
  unsigned char *input_content = (unsigned char*) RAW(input);
  // declare output
  // allocate memory
  SEXP result = R_NilValue;
  result = PROTECT(NEW_CHARACTER(1));
  if (result) { // check if memory is allocated successfully
    char* result_content = (char*) malloc(output_len + 1);
    if (result_content) { // check if memory is allocated successfully
      base64_encode_impl(input_content, input_len, result_content, output_len);
      result_content[output_len] = 0;
      SET_STRING_ELT(result, 0, mkChar(result_content));
      free(result_content);
    } else {
      rv = 2;
    }
  } else {
    rv = 1;
    result = R_NilValue;
  }
  UNPROTECT(1);
  switch (rv) {
  case 1 : {
    error("Failed to allocate memory for result");
    break;
  }
  case 2 : {
    error("Failed to allocate memory for result_content");
    break;
  }
  default : { }
  }
  return result;
}

SEXP base64_dec(SEXP input) {
  int rv = 0;
  R_xlen_t input_len;
  SEXP result = R_NilValue;
#if defined(R_VERSION) && R_VERSION >= R_Version(3,0,0)
  input_len = XLENGTH(input);
#else
  input_len = LENGTH(input);
#endif
  if (input_len != 1 || TYPEOF(input) != STRSXP) {
    rv = 1;
  } else {
    SEXP input_char = STRING_ELT(input, 0);
    const unsigned char* input_p = (unsigned char*) CHAR(input_char);
    const R_xlen_t input_str_size = strlen((const char*) input_p);
    if (input_str_size % 4 != 0) {
      rv = 2;
    } else {
      R_xlen_t output_len = input_str_size / 4 * 3;
      if (input_str_size > 0 && input_p[input_str_size - 1] == upadding) {
        output_len -= 1;
        if (input_p[input_str_size - 2] == upadding) output_len -= 1;
      }
      result = PROTECT(NEW_RAW(output_len));
      if (result) {
        unsigned char* result_content = RAW_POINTER(result);
        if (base64_decode_impl(
          input_p,
          input_str_size,
          result_content,
          output_len
        ) != 0) {
          rv = 2;
          UNPROTECT(1);
          result = R_NilValue;
        } else {
          UNPROTECT(1);
        }
      } else {
        rv = 3;
        result = R_NilValue;
      }
    }
  }
  switch (rv) {
    case 1 : {
      error("The input should be a character vector with length 1");
      break;
    }
    case 2 : {
      error("The input string is not a valid base64 encoded string");
      break;
    }
    case 3 : {
      error("Failed to allocate memory for result");
      break;
    }
  }
  return result;
}
