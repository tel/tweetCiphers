#include <stdint.h> 
#include <stdio.h> 
#define LOOP(n) for(i=0;i<n;++i)
#define W(v,n) ((uint64_t*)v)[n]
#define R(v,n)(((v)<<(64-n))|((v)>>n)) 

/* This forms the core of the random permutation function of the
   sponge construction. I think it's supposed to be Salsa20. It looks
   like it but uses a very different internal state. In any case,
   `ROUNDS` is called every time the permutation is to be performed in
   the sponge construction. */
#define AXR(a,b,c,r) x[a]+=x[b];x[c]=R(x[c]^x[a],r); 
#define G(a,b,c,d) {AXR(a,b,d,32) AXR(c,d,b,25) AXR(a,b,d,16) AXR(c,d,b,11)} 

/* This loop is quite annoying, @(r--)@ decrements @r@ and returns its
   previous value. If that value is zero then the loop
   terminates. This means that the loop executes 6 times and observes
   @r@ in @[5,4,3,2,1,0]@. */
#define ROUNDS {for(r=6;r--;){LOOP(4) G(i,i+4,i+8,i+12) \
                              LOOP(4) G(i,(i+1)%4+4,(i+2)%4+8,(i+3)%4+12)}}
 

int main(int _,char**v){

  /* Less than classy initialization here. In particular it's cleverly
     defining `f = true` if it's in encrypt mode and false
     otherwise. */
  uint64_t x[16],i,c,r,f='e'==*v[1]; 

  /* This initializes the sponge state to 16 multiples of the 8-byte
     string "twithasi". According to the Keccak/Sponge site it's
     naturally intiailized with just zeros, so this may not be
     necessary. */
  LOOP(16)
    x[i]=i*0x7477697468617369ULL; 

  /* Inserts (key || nonce) into the sponge state as the first 48
     bytes and permutes. */
  LOOP(4) x[i]=W(v[2],i);
  LOOP(2) x[i+4]=W(v[3],i);
  ROUNDS;

  /* The duplex steps! For each byte read in encode or decode it by
     XORing with the first byte of the first uint64 of the sponge
     state. */
  while((c=getchar())!=EOF){
    /* If we're decrypting and we see a newline we're... done?
       Really? */
    if(!f&&10==(x[0]^c)%256)return 0;
    /* x[0] is a uint64 so (x[0] ^ c) upcasts the input character to
       be a uint64. Then putchar truncates it. Endianness here is
       problematic. Or does it matter or long as it's consistent? (At
       the very least I'd think that a client with a different
       endianness couldn't decrypt with this code). */
    putchar(x[0]^c);

    /* This is a bizarre line---IF we're decrypting then zero out the
       first byte of x[0]. Otherwise just leave it. How does that
       work? */
    x[0]=c^(f?x[0]:x[0]&~255ULL);

    /* Permute again at the end of each round. */
    ROUNDS;
  }

  /* Truly no idea why this happens. Should it be a 10? It feels like
     it's terminating the message on a 1. It may also just be the
     definition of the authenticated tag. Why use this final XOR then?
     Without it you could authenticate empty messages? */
  x[0]^=1;
  ROUNDS;

  /* And this just prints out (x[4] XOR x[5]) and (x[6] XOR x[7]) as
     an authenticated tag. */
  LOOP(8) putchar(255&((x[4]^x[5])>>8*i)); 
  LOOP(8) putchar(255&((x[6]^x[7])>>8*i)); 
  return 0;
}
