#include <stdlib.h>
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
  SEXP result = PROTECT(NEW_CHARACTER(1));
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

