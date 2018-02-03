#include <stdio.h>
#include <stdint.h>
#include <time.h>

uint16_t carp[5][0x8000] = {0};

const uint32_t MAX = 2147483648 - 1;
uint16_t stack[2147483648];

uint32_t sc = 0;  // stack counter

uint16_t ack(uint16_t a, uint16_t b, uint16_t c) {
  uint16_t tmp, tmp2;
  
  if (a == 0) {
    return (b+1) & 0x7fff;
  }

  if (b == 0) {
    if (carp[a-1][c] != 0) {
      return carp[a-1][c];
    }else {
      tmp = ack(a-1, c, c);
      carp[a-1][c] = tmp;
      return tmp;
    }
  }

  //inner recursive call "b"
  if (carp[a][b-1] != 0) {
    tmp = carp[a][b-1];
  }else {
    tmp = ack(a, b-1, c);
    carp[a][b-1] = tmp;
  }

  //outer recursive call
  if(carp[a-1][tmp] !=0) {
    tmp2 = carp[a-1][tmp];
  }else {
    tmp2 = ack(a-1, tmp, c);
    carp[a-1][tmp] = tmp2;
  }
  //return ack(a-1, tmp, c);
  return tmp2;
}

/*********************************************************************************/

void push (uint16_t a) {
  if (sc == MAX) {
    printf("Full Stack\n");
  }else {
    stack[sc] = a;
    sc += 1;
  }}

uint16_t pop () {
  uint16_t r;
  
  if (sc == 0) {
    printf("Stack Empty\n");
    return 0;
  }else {
    sc -= 1;
    r = stack[sc];
    //could clear stack value here
    
    return r;
  }}

uint16_t ackstack(uint16_t a, uint16_t b, uint16_t c) {
  uint16_t tmp;
  
  for (;;) {
    if (a == 0) {
      //a = (b+1) & 0x7fff;
      return (b+1) & 0x7fff;
    }
    if (b == 0) {
      a -= 1;
      b = c;
      continue;
    }
    push(a);
    b -= 1;

    if (carp[a][b] != 0) {
      b = carp[a][b];
    }else {
      tmp = ackstack(a,b,c);
      carp[a][b] = tmp;
      b = tmp;
    }
    a = pop();
    a -= 1;
  }}

void clear_carp() {
  for (int i=0;i<6;i++) {
    for (int j=0;j<=0x8000;j++) {
      carp[i][j] = 0;
    }}}


/*********************************************************************************/

int main() {
  uint16_t val;
  time_t start_time, end_time;

  //FIRST - recursive
  start_time = time(NULL);
  
  for (int i=1; i<0x7fff; i++) {
    //val = ackstack(4,1,i);
    val = ack(4,1,i);
    if (val == 6)
      printf("%d :: %d \n", i, val);
    clear_carp();
  }

  end_time = time(NULL);
  printf("ack: run time: %.0f seconds\n", difftime(end_time, start_time));

  //SECOND - stack
  start_time = time(NULL);
  
  for (int i=1; i<0x7fff; i++) {
    val = ackstack(4,1,i);
    if (val == 6)
      printf("%d :: %d \n", i, val);
    clear_carp();
  }

  end_time = time(NULL);
  printf("ackstack: run time: %.0f seconds\n", difftime(end_time, start_time));


  return 0;
}

