/* Header files */

#include <stdio.h>
#include <stdlib.h>

#include "hashcons-i.h"
#include "warehouse-i.h"


/* Definition of variables */

/* number of entries in the heap */
tagHC entriesHC = 1;
/* pointer to the next free record of the heap */
tagHC freeHC = 1;
/* number of entries triggering HC GC */
tagHC HCGCLIMIT;
/* hash table */
struct bucketHC * tableHC;
/* heap for list Hash Consing */
struct nodeHC * heapHC;
#ifdef STATS
/* count total hits */
static unsigned int totalHits = 0;
/* count total misses */
static unsigned int totalMisses = 0;
/* count garbage collection cycles */
static unsigned int totalCycles = 0;
/* count total collected */
static unsigned int totalCollected = 0;
/* maximum visited chain length in HC hash table */
static unsigned int maxChainLength = 0;
/* sum of visited chain lengths in HC hash table */
static unsigned long int sumChainLength = 0;
/* number of visited chain lengths in HC hash table */
static unsigned int countChainLength = 0;
#endif 

/* Hash functions */

#ifndef HASH
#define HASH HASH_DJB_S
#endif

#define HASH_J      1
#define HASH_ROT    2
#define HASH_SIM    3
#define HASH_UC     4
#define HASH_UNI    5
#define HASH_ZOB    6
#define HASH_CRC    7
#define HASH_FNV    8
#define HASH_ONE    9
#define HASH_WANG  10
#define HASH_JJ    11
#define HASH_DJB   12
#define HASH_DJB_S 13

#if HASH == HASH_J
static tagHC hashHC (tag h, list l)
{
  register tagHC a, b, c;
    
  /* the internal state */
  a = 0x9e3779b9 + l;
  b = 0x9e3779b9 + (tagHC) h;
  c = 0;
	
  /* handling the key */
  a -= b; a -= c; a ^= (c>>13);
  b -= c; b -= a; b ^= (a<<8); 
  c -= a; c -= b; c ^= (b>>13);
  a -= b; a -= c; a ^= (c>>12);
  b -= c; b -= a; b ^= (a<<16);
  c -= a; c -= b; c ^= (b>>5); 
  a -= b; a -= c; a ^= (c>>3); 
  b -= c; b -= a; b ^= (a<<10);
  c -= a; c -= b; c ^= (b>>15); 

  return c & HCMASK;
}
#endif

#if HASH == HASH_ROT
static tagHC hashHC (tag h, list l)
{
  register tagHC hash = sizeof(tag) + sizeof(list);

  for (register int i=0; i<sizeof(list); ++i)
    hash = (hash<<4) ^ (hash>>28) ^ *((unsigned char *) &l + i);
  for (register int i=0; i<sizeof(tag); ++i)
    hash = (hash<<4) ^ (hash>>28) ^ *((unsigned char *) &h + i);

  return (hash ^ (hash>>10) ^ (hash>>20)) & HCMASK;
}
#endif

#if HASH == HASH_SIM
/* where is this? */
static tagHC hashHC (tag h, list l)
{
  register tagHC hash = 0x9e3779b9; /* an arbitrary value */

  hash += ((tagHC) h << 9); 
  hash ^= (l >> 8); 
  hash += (hash << 15); 
  
  return hash & HCMASK;
}
#endif

#if HASH == HASH_UC
/* where is this? */
static tagHC hashHC (tag h, list l)
{
  register tagHC hash = (tagHC) h << 8; 

  hash ^= (hash << 13) ^ (l >> 6);
 
  return hash & HCMASK;
}
#endif

#if HASH == HASH_UNI
/* auxiliary hashing table for HASH_UNI */
static const tagHC tab[32] = {
   0x10396c58, 0x8f8c3bab, 0x19598ef0, 0x86640504,
   0x4593cbef, 0x55ac8cd1, 0x42755320, 0xf7444e25,
   0x38407305, 0x8471a161, 0xa16ccafe, 0x3d196b1a,
   0x399b548a, 0x5385d9b2, 0xf903febe, 0xfe2443e0,
   0xe71ad9e2, 0xaed994e8, 0xce0211a0, 0xe7a7a0b6,
   0x8270a748, 0x2605beb3, 0x4df8c084, 0xcbb642be,
   0x32539b9d, 0x19e491e4, 0x9bcc2388, 0xf867c672,
   0x25f41711, 0x47806b89, 0xaa94eaef, 0xa2bcf2a5
};

static tagHC hashHC (tag h, list l)
{
  tagHC hash = sizeof(tag) + sizeof(list);

  for (register int i=0; i<(sizeof(list) << 3); i+=8) {
    register unsigned char k = *((unsigned char *) &l + (i>>3));
    
    if (k&0x01) hash ^= tab[i+0];
    if (k&0x02) hash ^= tab[i+1];
    if (k&0x04) hash ^= tab[i+2];
    if (k&0x08) hash ^= tab[i+3];
    if (k&0x10) hash ^= tab[i+4];
    if (k&0x20) hash ^= tab[i+5];
    if (k&0x40) hash ^= tab[i+6];
    if (k&0x80) hash ^= tab[i+7];
  }
  for (register int i=0; i<(sizeof(tag) << 3); i+=8) {
    register unsigned char k = *((unsigned char *) &h + (i>>3));
    
    if (k&0x01) hash ^= tab[i+0];
    if (k&0x02) hash ^= tab[i+1];
    if (k&0x04) hash ^= tab[i+2];
    if (k&0x08) hash ^= tab[i+3];
    if (k&0x10) hash ^= tab[i+4];
    if (k&0x20) hash ^= tab[i+5];
    if (k&0x40) hash ^= tab[i+6];
    if (k&0x80) hash ^= tab[i+7];
  }

  return hash & HCMASK;
}
#endif

#if HASH == HASH_ZOB || HASH == HASH_CRC
/* auxiliary hashing table for HASH_ZOB and HASH_CRC */
static const tagHC tab[][256] = {
  0x0206efb4, 0x591d64d2, 0xfafea3fc, 0x74efaf17, 
  0x3f139e7c, 0x56c28105, 0x7c47a965, 0xe65dffe6, 
  0xb8a92902, 0xbbe650b5, 0xc73f84ed, 0xda8f060a, 
  0x53c05daf, 0xa60466df, 0x7b2990bd, 0xa0d1c663, 
  0xbb790b81, 0x69c98a07, 0x4dd1ff39, 0xeaa83f59, 
  0xcc861d76, 0xdb1cd1b9, 0xcce2990c, 0xa6a30c5e, 
  0xaeb3da2d, 0x3433706a, 0x9d17c2ac, 0x7b2cb57a, 
  0xf6263e3a, 0x05131969, 0x08b526c6, 0xee242b00, 
  0x139f19df, 0x1e2abb2c, 0x1e34412a, 0x7ba600df, 
  0x38c52235, 0xa192783f, 0x22696f32, 0x5a7185c9, 
  0xbd2fb2d8, 0xa4fac306, 0x4d4f8d00, 0x1c982aad, 
  0x31ed6b47, 0x3f065a06, 0x7d042af1, 0x04bdd8c7, 
  0x37bcf3a1, 0xe49b7b3f, 0x57e83ce0, 0x60b00564, 
  0xc7bc24b3, 0x579d592b, 0x059f257c, 0x705b2eee, 
  0x2889c709, 0x55d777ff, 0x973aff4f, 0x321aae91, 
  0xd2d334e8, 0xc14bf6ee, 0x17b2fcc5, 0x8b351f09, 
  0x14e12b26, 0xfb143a7e, 0xa2330abe, 0xa1a0e799, 
  0x636e6cdf, 0x6149344b, 0x1b404ff5, 0x02052749, 
  0x07243785, 0xac767222, 0xbf925119, 0xa9b5fbf1, 
  0xff6a1aa3, 0x40feef11, 0x02472ae2, 0xa757a0a8, 
  0xdfaedc62, 0xbe1173e7, 0xb7a41cf5, 0x39eee3ee, 
  0x4cca9791, 0x9300ad40, 0xdc143d71, 0x06e82fe0, 
  0x65c04e24, 0x4ef8f535, 0x8018edf2, 0xc03a249c, 
  0xdbe406b8, 0x09456b6c, 0xe691aad8, 0x3ee145f0, 
  0x99183ac5, 0x8808da58, 0x01bac879, 0x00f002f0, 
  0x341ef408, 0x35cc7c05, 0xdca9c387, 0x1a54ac8f, 
  0xc2b0fda0, 0x1233dc9e, 0x7d2d3c3a, 0x2a82fd69, 
  0x9acbb7aa, 0xa373445c, 0xd569dc3b, 0xd9a8d6ae, 
  0x5c443280, 0xc1c1b957, 0x3bd20a4c, 0xb9d99cc5, 
  0x900fbf62, 0xaf32b821, 0x73128909, 0xb955a4c1, 
  0x92f6a017, 0x6e69b389, 0x13fd33eb, 0xc246c670, 
  0x99e1b62f, 0x6152bff9, 0xd0805439, 0xe46655ea, 
  0xc635aa49, 0xccf33d2e, 0x41d1014d, 0x8cc74e3c, 
  0xb6fc1b6f, 0xd1cc73cb, 0xcaa68536, 0x5806e9ed, 
  0x1e0312e5, 0xcb109a7a, 0xceb3ab2b, 0x46f76fd5, 
  0x01523b20, 0xe27cc994, 0xf916d091, 0x46c08589, 
  0x5bb9a629, 0x96667bd3, 0x0b81b91f, 0x6dbe5734, 
  0xda07f3e8, 0xb88a1c50, 0xd87e275b, 0xb28a6a5a, 
  0x61f83385, 0xf95d70be, 0x5b25bc0d, 0x4f89f51d, 
  0xe90c5286, 0x0f21a75e, 0x62277e08, 0xcc1864b8, 
  0x572ee2c9, 0xaa3ecc06, 0x159d3d30, 0x7dea26e5, 
  0xccf8caeb, 0xaa1d8810, 0x02b2e9ef, 0x523b063a, 
  0xb6dab11d, 0xcb67b032, 0x09e21615, 0x5d84ff0d, 
  0x47698166, 0x4d524d1e, 0x9e1ff30e, 0x32ff45ad, 
  0x3e31004c, 0x321e10d7, 0x42f8fd42, 0x06d18f3d, 
  0xcfbfabd1, 0x7a6e2972, 0xaf73251d, 0x376e2350, 
  0x38c6501e, 0x57ef61a9, 0x0cc51aff, 0xfb7d16a1, 
  0x517a57e3, 0xe1395ffb, 0x6da09ab4, 0xd4c17c41, 
  0xb821d397, 0xb7872c79, 0x4b0713c1, 0x3fbc85c6, 
  0xee8b7de4, 0xf61a540c, 0x57cc34cd, 0xe3edd379, 
  0x8786ab03, 0x9cceba60, 0x5caec2ad, 0x4af127d1, 
  0xfeebe647, 0xa199e62d, 0x4849b063, 0xbf90505b, 
  0xeff08847, 0xb705e830, 0xf48a9985, 0x87baf150, 
  0x2d0146b1, 0x2daa87fe, 0xba55cb7d, 0xded03ac7, 
  0x035c4470, 0x3c5d0eef, 0x70813e46, 0x275ca9e7, 
  0x3fedb33b, 0xc797fd32, 0xd0ad70ff, 0xe4889c77, 
  0xd624b80c, 0xd492b4a5, 0x62dba537, 0x66b45e0b, 
  0xf4c43a7e, 0x9b34af75, 0x498731ee, 0x615a4ea9, 
  0xd2ec4639, 0xdc935206, 0x440521e4, 0x14ebd670, 
  0xa3f995aa, 0x800e8cca, 0x0ea54551, 0x5c1a5fe7, 
  0xda909a3f, 0x990e8909, 0x4563eb91, 0x620ba56a, 
  0x55abc673, 0xf3b9ee59, 0x88f79e05, 0xb5cebc4d, 
  0xdab89b1f, 0x7c185c42, 0x79e3b884, 0xa97d61c0, 
  0x040518a8, 0xf92d3937, 0xb91c5a15, 0x96701fdf, 
  0x710256fe, 0xab4599a1, 0xb4cf48d6, 0x3c26e54c, 
  0xbdb236d9, 0x86073787, 0x0690d62b, 0x9c058d48, 
  0x7c834645, 0x0461f296, 0xe0a9b30c, 0xcb8918f2, 
  0x58ac3f62, 0xc290bbc0, 0x3b5bbf0a, 0x44e97efb, 
  0x1f969771, 0x5622e6b0, 0x2d384f4a, 0x71b32082, 
  0x461d1b68, 0x32df9f1a, 0xa4237569, 0xabbabc76, 
  0x291a26ea, 0xdc58aa8a, 0x8cc70ce4, 0x2933e0f0, 
  0xadab61e9, 0xc38537a6, 0xeb21e712, 0xbeb8468d, 
  0x63cc0c5d, 0xa61d437e, 0x93ce6d99, 0x52a0e81d, 
  0xbb3ec0e4, 0x8751a8bf, 0xb80f5f05, 0xf366860a, 
  0x245d8d80, 0xd25ba6b1, 0x9db313da, 0xe7c20911, 
  0x45a1f523, 0xd7cb011e, 0xa7be4cff, 0x2ea06a1f, 
  0x3b44f10a, 0xd511a549, 0x7cccf566, 0x573d8ce3, 
  0x02c5a55c, 0x95e18e96, 0x5284e0af, 0x478aafbc, 
  0xe058a866, 0x006aab82, 0xdf78e121, 0x532215b8, 
  0xfa60c3dc, 0xd5eff437, 0xa27b2328, 0x94be4661, 
  0x6fa59665, 0x63cc861a, 0x3f8d788c, 0xf6799b38, 
  0x3635d90a, 0x181e9be8, 0x8c290b54, 0x8077043c, 
  0xe9beff1a, 0x71e3dbf1, 0xb3f457f8, 0x9a746f9c, 
  0xcb27d5fd, 0xcbbb142b, 0x8b63b9a7, 0xa9fd43c5, 
  0x63b30cdd, 0x67c2c7fc, 0x73eb162b, 0x06004e41, 
  0x30848603, 0xeb12c8bb, 0x245884b6, 0x206d374f, 
  0x04bf8c17, 0x696e22c0, 0xd3164d2f, 0x4d67b81c, 
  0xf6c1ab43, 0x49bdf764, 0xb84fce06, 0x84ae6f82, 
  0x1014cfab, 0x50a772de, 0x09365498, 0x1db8b365, 
  0x282ab20d, 0x88b9682b, 0x8048d519, 0x1590a42b, 
  0x63dafb35, 0xe12806ef, 0x9b52a287, 0x4e9e438b, 
  0x6c7587e8, 0x5ec18776, 0xb00914e9, 0xd493f9cc, 
  0xefa7d63b, 0xdeec1ea5, 0x2feb8aa8, 0x32b6d86c, 
  0x8682af6d, 0xbe26db86, 0x19550c78, 0x69c79020, 
  0xeba09a36, 0x69f4e18b, 0x9b08b040, 0x553f91ac, 
  0x3e59bf65, 0x43456432, 0x0a0562a1, 0x42ae8965, 
  0xc7bf347e, 0xf8b8df9d, 0x055c5acd, 0xb6cde4ed, 
  0x20b1a699, 0x7889de12, 0x54792ff2, 0x490634d4, 
  0x1b701b0c, 0x55f7e5d2, 0x0fe5e202, 0x156ed095, 
  0x6438e81d, 0xfbeda171, 0x1b78ed2c, 0xfeffb652, 
  0xa3f4cf97, 0x172ff6fb, 0xbe182363, 0x4a929885, 
  0x4f006ef1, 0x96fd5a22, 0xef849718, 0x8787e362, 
  0xe708210a, 0x3086f642, 0xd094c131, 0x6ddf0374, 
  0x4d8728ab, 0xd7871538, 0x9a43e6e4, 0x2c9fe0d7, 
  0x8d3e5ef6, 0x14c25a55, 0xc49df54e, 0xa102efba, 
  0xdd55976c, 0x391a6a2c, 0x1f6dce4f, 0x5a6c0f11, 
  0xbd2287d3, 0xab450050, 0xf70c58e5, 0x81182fed, 
  0x29cad303, 0xd848a86a, 0x571fbfc7, 0xacd61841, 
  0x51688ad9, 0x1228cb7e, 0x7d732779, 0x1b467da1, 
  0xe8162851, 0x8b2016df, 0xaabd8fa5, 0x84be6790, 
  0xaefb1476, 0x5b6bf3ad, 0x87533dc8, 0x177cfb6a, 
  0x0acd089a, 0xbd0c89c2, 0x6f0d0dcc, 0x37b827ee, 
  0x530e481c, 0xf48a807b, 0x811f7fa1, 0xbc54a8d3, 
  0x3329e56e, 0x39332e4c, 0xf402747b, 0xf39e90e9, 
  0x218e3d44, 0x85b16e4b, 0x0e21026a, 0xe6fdaad0, 
  0x3300d004, 0xdc8591a4, 0x6813847a, 0xf89a3685, 
  0x533b60d5, 0xd4c3a6b8, 0x7e2794db, 0x2825ab93, 
  0xd1281f83, 0xf0a18eb4, 0xd429b179, 0x09d75b3b, 
  0xdc521e13, 0xc3f7e1a4, 0xb1eb4e32, 0x5b79b8a4, 
  0xbbaee838, 0xb410dbd3, 0xe5a78e48, 0xc5509fcf, 
  0xaf2b4d0e, 0x045abfb9, 0x92fda38a, 0xcdc164ef, 
  0x78718b80, 0x84d7dd14, 0x44ec3e7b, 0x369e53ca, 
  0x97cf3a2a, 0x71e8fc0b, 0x357f4373, 0x17e2516a, 
  0x2242dded, 0xa978aba8, 0xa9b670b8, 0x793f710a, 
  0xc56fdaed, 0xd3c9f126, 0x960f0b6e, 0x66052067, 
  0xbf58bfdd, 0x2bf4d9b6, 0xa3d5d656, 0x5410499f, 
  0x97cc678e, 0x7a197161, 0xa369cfe9, 0x35cf845a, 
  0xa4a199ea, 0x3f088842, 0xc4479ee9, 0x4bcc5ebb, 
  0xe4f06914, 0x9d074825, 0xabeba416, 0x6ea81d1f, 
  0x1ea62c59, 0x697f09af, 0xd8ea0a13, 0x31cc6c8b, 
  0xa3e3ea6c, 0x6d5c6b55, 0x0d64a546, 0xa75b1078, 
  0x94fef8ec, 0x3217e1ad, 0x12cbd2b8, 0xc93da653, 
  0xbbe62851, 0x8e39baff, 0x26d199ee, 0x8843f8c8, 
  0xfaaa589b, 0x8142e103, 0x90887b91, 0x9513f06c, 
  0x26bae21a, 0x552eb954, 0xfa30e7c7, 0x59401565, 
  0x8281541a, 0x5ac6746b, 0x69bfe993, 0xfbd786a2, 
  0x85361e2c, 0x97d27326, 0x30641413, 0x48e0c372, 
  0xa209e95a, 0xd6dea9fd, 0x8737c30d, 0xc3b29ea7, 
  0x9598b128, 0xd7ab528e, 0xa4b54f37, 0x30cd4bce, 
  0xa59e1dcc, 0x416bcefe, 0x43705f42, 0x5ada3235, 
  0x192b680e, 0x66ed1257, 0x250f6747, 0x4984e59d, 
  0xfd1fee04, 0xb54d91e2, 0x9f59dafa, 0xbe098c16, 
  0xafd4cfee, 0x4a40f618, 0x392d6d58, 0xd0d8ed8d, 
  0xb6539f24, 0xffdf9660, 0x2e447760, 0x20c29d8c, 
  0xe326a5fd, 0xfb4f4b71, 0xd2cb8c0f, 0xae44344c, 
  0xd6cf1b15, 0x2810aae9, 0x2cda8caf, 0x98a110ab, 
  0x674c2644, 0xd67b1065, 0xeff0aa07, 0xecaa122f, 
  0x6043562b, 0x82189a32, 0xa3a13573, 0x9f76f7b9, 
  0xdb6f98e5, 0xc3b5c48d, 0x36af1ecb, 0x7c363a25, 
  0x1278e24d, 0x3e4f7177, 0xbd55803b, 0x84f6452e, 
  0x8b3f0561, 0x81c57037, 0x18ed7b4a, 0x10df0120, 
  0x7e07a15a, 0x61b7f176, 0x33e76374, 0xa52ed240, 
  0x32eba60a, 0xdbfbb017, 0xeae55440, 0x888a6eb4, 
  0x3f276b62, 0x3d74546d, 0xcfc46a7e, 0x670aa262, 
  0xdc674bcc, 0x55c8a7ac, 0xa18df3ca, 0x73e6baec, 
  0x9c623bb5, 0xb00a5729, 0x3ef3dd06, 0xe6e542cc, 
  0x275fab35, 0x2588d8ec, 0xf9c9f887, 0x93e34a3a, 
  0x5e8e548a, 0x9be6a2f3, 0xcc6316c7, 0x71259f25, 
  0x487635e3, 0x4b45e229, 0xa0e6b642, 0x01f326b0, 
  0xac3bf937, 0x1605d60a, 0xcf5df469, 0xde38ed56, 
  0x58d94666, 0x1bdf4442, 0x6bc3df12, 0x7dad3190, 
  0x6c694298, 0xa79a49c2, 0x776c1d66, 0x6f93fddc, 
  0x81bfe541, 0xbeda45c9, 0x3c119689, 0x85584169, 
  0xc6538923, 0xe0a4aa29, 0xc566f38d, 0x00e99375, 
  0xe27e2264, 0xf9b5f711, 0xd12a93de, 0x46a20812, 
  0xe5bf7896, 0x5ae2e42a, 0xfc961014, 0x831a5a63, 
  0xbcb2a4be, 0x8dc05f9d, 0x90efc307, 0x2b0c50b4, 
  0xc6c1c166, 0xbbc6f852, 0x645f6817, 0xf4fef3e1, 
  0xa6760773, 0xbe53641e, 0xc192207a, 0xeca0bc92, 
  0x4bb3668b, 0x10fef81b, 0x18cad890, 0x1f1852ce, 
  0x7a26fea6, 0x64a2ea82, 0x2c9114ed, 0xc317631c, 
  0x1bd87d32, 0xfb11c35f, 0xc89f0031, 0x429fe6dc, 
  0xedd28f45, 0x71b24294, 0x97bc41cd, 0xce7fe169, 
  0x2731a130, 0x66dcff46, 0x1abb3d3e, 0x3f04df1d, 
  0xd140f9e9, 0x3ae32333, 0xed6fcf90, 0xb7ac10c6, 
  0x2d6ade50, 0xbff7cfa9, 0x4e19f175, 0x7e63f7e0, 
  0xd6fe73cf, 0xc0f56a50, 0xe2756ece, 0xfd888b62, 
  0xb4cbd415, 0x19362204, 0x6a31db08, 0x3f3a7b1c, 
  0xa35b582a, 0x65555ce4, 0x7ebac590, 0xddeb2338, 
  0x89e8abae, 0xde2b1a34, 0xd83d2950, 0xe4daca7b, 
  0x51331404, 0x65bb7ed7, 0x606d2260, 0x145f507e, 
  0xd60770d6, 0xc78dc23e, 0x8477df12, 0x1d8fab68, 
  0x4a635e12, 0x711f61db, 0x11a47994, 0xc4261a97, 
  0x347bd823, 0xe369cbbf, 0x5101b214, 0xf9e532db, 
  0xcf832ef1, 0x06e9bbba, 0xfdec4497, 0xe996fd81, 
  0x6dafc718, 0xe757876c, 0x25e4b67d, 0xf2a5d4ad, 
  0xb1cc003d, 0xd7fa9689, 0x8ace2a08, 0xc680c571, 
  0x8c97c55e, 0x07044ace, 0x1c3a4dfa, 0x4e85f3c8, 
  0x0f966e9f, 0xc3586f8d, 0x4b22791e, 0xc281c424, 
  0x09697798, 0xb303e2e4, 0x3f491cf0, 0x6cc0d23a, 
  0xe235f982, 0xa0478a8f, 0x7c0a753b, 0xb79ed914, 
  0x63898d35, 0xb6e8843e, 0xfe90e847, 0x1b99047b, 
  0xa3f83f8c, 0xa5a7986c, 0x8d06760d, 0x479b4759, 
  0xe6fc30fb, 0x2fa34707, 0x0b6591a3, 0x564c49b9, 
  0x24d77b3f, 0x3556aa58, 0x4c4f1e00, 0x6eb1821d, 
  0xf14277dc, 0xacdb5718, 0x3f415aa8, 0x8107503f, 
  0x1cd5773e, 0xf0b61451, 0xd2da01e4, 0x5e551766, 
  0x16e4c573, 0x4cb1fa0a, 0x3842ea14, 0x671ab71e, 
  0xaa6dfb9b, 0x03871653, 0x8a2d40c9, 0x71b965e4, 
  0x45c795a7, 0x513f73da, 0x76096d2e, 0x295c0e19, 
  0x50f0da72, 0x3390006a, 0x5233f1ab, 0xc1452571, 
  0xb21e9cc4, 0xebdbc66f, 0xbaad0370, 0xaa7712e2, 
  0x0cc0cbc1, 0xeade4c14, 0x92fa5412, 0x48ad60c3, 
  0x294a8ba0, 0x9e003f51, 0xa708945c, 0x432beacc, 
  0x240c1ad3, 0x0fbf8f66, 0x5ca66213, 0x760dc4b6, 
  0xad34746b, 0xaa8b12c5, 0xbe3c7683, 0x43d9cfe0, 
  0xcd9223ac, 0x18e0f625, 0x97f50442, 0xcf89df67, 
  0xaaa3c047, 0xa7ec2aa3, 0xe26ca9cc, 0x19cda7c7, 
  0xacb56665, 0xa1564a71, 0x3c411a9f, 0x222b43aa, 
  0xfdbfa446, 0x31c64ce3, 0x7577f02c, 0xfc8f4780, 
  0x468602a8, 0x0e80e236, 0xd31998b2, 0x6815112c, 
  0xf75f5a21, 0x1d6c84e7, 0x54fa7bec, 0x7ad711ad, 
  0xa86efb8c, 0xcdcb8880, 0x1bffe332, 0xbdb5f617, 
  0x045ef9cd, 0xe29a3e4b, 0x77373d43, 0x6d5595e4, 
  0xb334723b, 0xa752350b, 0xaea9b248, 0xad389186, 
  0xbd1d523d, 0x4af60433, 0xb892a94c, 0xe8acfa21, 
  0xc0640398, 0xd1966478, 0x372b0570, 0xeabb01f1, 
  0xea60ce33, 0xe403dd96, 0x9b4d333c, 0x64022f92, 
  0xa732ec9c, 0x80043509, 0xf34855ec, 0x35816557, 
  0x41a3b394, 0x745dd61f, 0xbdba696f, 0xd61c2a2c, 
  0x3e764300, 0x0c01c5bb, 0x0c8e8ea0, 0xa42dee94, 
  0x5b24e144, 0x5f643394, 0xd117b8fd, 0x253c2cf4, 
  0x77b2182e, 0xe7efabf0, 0x9873b5f8, 0xd80d2f19, 
  0xe8e3b73e, 0x3107a3be, 0xac4653ec, 0x0d0f0b9a, 
  0x7a92a1bf, 0xebfd0af2, 0x4b922d05, 0xcf789c55, 
  0x5f9d5d8a, 0x454c3db2, 0x9b09eb24, 0x88e31222, 
  0x483506df, 0x07b825d8, 0xc28c4e53, 0x564bb58c, 
  0xcfb3bbea, 0x71fecc8b, 0xfeab74fc, 0xbe8b8c01, 
  0xf52b14a3, 0x23ef8d3c, 0x5d22d2dc, 0x46934fd8, 
  0x5fa88cf9, 0x45feb7ff, 0x33a9fd56, 0xe622b7b7, 
  0x429fcd14, 0x068c7e91, 0xc9bf3864, 0x60611cd5, 
  0x15a25876, 0x7d38f0e9, 0x417b6141, 0x51e13c49, 
  0x23faf5aa, 0x97a9f530, 0xbc8dd317, 0x2e88213b, 
  0x42183229, 0x7a7218e7, 0x3bcb69e9, 0x695a3324, 
  0x14c1a108, 0xeda56cd3, 0xb9eeb364, 0xdadaf0c9, 
  0xe398295b, 0xbb8d7475, 0x25aa3dd0, 0x8ac0240b, 
  0x15da5b02, 0x1444cef4, 0x8421e50c, 0xa91af969, 
  0xce3e02f9, 0x769939b0, 0x3d69d4c6, 0x0cd68f5d, 
  0x2f5fc93d, 0x9458128d, 0x9b1c2d15, 0x0100c707, 
  0xe59b67e4, 0x8e1566af, 0x48105dbe, 0xdcd4e7b5, 
  0x0994bf14, 0x1e26c3cc, 0xd0975338, 0x38966267, 
  0xf50f7a3f, 0x0f0448ae, 0x300581ff, 0x4cf57f3c, 
  0x5bdfc82e, 0x7ac192dd, 0x20008485, 0xeb40dbf0, 
  0x7cf71e7f, 0xd75130c1, 0x3d172356, 0xaf69603c, 
  0xf79e7fc3, 0xf8e2418e, 0xa66885b0, 0xc0e7abd9, 
  0xfa0675b0, 0x33862fbe, 0xcecbf35b, 0xd1cd6332, 
  0xf26a5efc, 0x93ae2c73, 0xb8b16017, 0xb0a50d7a, 
  0x2939f187, 0xb235042e, 0x573bd2de, 0xd269a20a, 
  0x4408e9d9, 0xcb7d9495, 0x6f851746, 0x21e5b7ee, 
  0x7c4fce9c, 0x6e9fde9a, 0x4c9f3507, 0x1052f8bd, 
  0x4ffe2755, 0xc120e6c6, 0x76ba2295, 0xc24efa6f, 
  0xd7151cf8, 0xc71857be, 0x14417e0f, 0x4e2579b6, 
  0x23a0e91f, 0xe02886e2, 0x4b10984b, 0xe1905952, 
  0xae44639b, 0x3a706974, 0xc79d49be, 0xa86293a6, 
  0x72121cc0, 0x2b94ee03, 0x6c79d553, 0x131f2be3, 
  0x432ec4a4, 0xb9160725, 0xdfbdd116, 0x4761b218, 
  0xb0cf5fb4, 0xaa18e65e, 0xa2ef7f2e, 0xfdc97cdc, 
  0x261e9727, 0x69f67b8c, 0x477deb54, 0x6b83b071, 
  0xe4ea403d, 0xdf1fd45d, 0x27f738b9, 0x3d943c09, 
  0xd9275e16, 0xf3049e77, 0x63d7ab21, 0xf47dc4d6, 
  0x9cd4c9a9, 0x9dd119c4, 0x68e9beb5, 0xdc2e5d7a, 
  0x9c7ee07b, 0x2782f868, 0x2db5450d, 0xc71d11dd, 
  0x0bc315d5, 0x39376333, 0x56b66722, 0x92f33f69, 
  0xf8ab4e39, 0xed13ce26, 0xc27e89f0, 0xc3eea8ec, 
  0xa1b447c8, 0x9a365a44, 0x44ba3a7b, 0x5ba4742b, 
  0xd76bacd6, 0x2d894f42, 0x2a533594, 0x641ec157, 
  0x19a1c748, 0xdc846d59, 0x5262ae66, 0xf9e6d2a7, 
  0x5819b436, 0xf4808d66, 0xa8485951, 0x9501a1cb, 
  0x1d4169ba, 0xd565b500, 0x17ec9436, 0xbfaf7396, 
  0xbcc91e13, 0x823b8300, 0x36b6e37e, 0x75918cb2, 
  0x9ff2ad8e, 0xabfdf84e, 0xd7ef74ce, 0x9a11828e, 
  0xa5fe1e2c, 0x5ed794e0, 0x40503735, 0xa84891b2, 
  0xef5d5de6, 0xdc2252eb, 0x1fbff904, 0x4ae02019, 
  0x34e3cefe, 0x07430c1b, 0xce56e274, 0xde7c5547, 
  0xf64062b4, 0xead94bf2, 0xfe696942, 0x79cd3391, 
  0xb2a8d0c6, 0x52a2cced, 0xe47b7280, 0x2928ecf0, 
  0x5e19e1c7, 0x1ebd16b4, 0x0a4ad880, 0x0410369a, 
  0x23ef828b, 0xae571231, 0x9b41f396, 0x1f6369e6, 
  0x62deef5d, 0x83614353, 0x34d7cd1f, 0x0c39e79f, 
  0x266cce53, 0x525df945, 0x726796dd, 0xa4f64c1b, 
  0xbfd251fe, 0xe0b4943f, 0x2f7c2c1b, 0x1e6a02f5, 
  0x790f7f86, 0x600ed568, 0x3f260d3b, 0xd4bcdf2e, 
  0x13b85be0, 0x5c784f13, 0x9b79915c, 0x30d89c7b, 
  0x9a95949d, 0xbf03c306, 0x576e9b41, 0xdc19b5ee, 
  0x9616a670, 0x299cdbdd, 0x78c8cc01, 0xb9178785, 
  0x198b9f1e, 0xc6a67576, 0xcbd4190f, 0x2e4c64bb, 
  0x8dded307, 0xccb2f998, 0xb9b5f3da, 0x8d790acd, 
  0x1a18207b, 0x816e4a0d, 0xaf835420, 0x6eb0f196, 
  0xb45aa46d, 0x049bed64, 0xf2753ce4, 0xe40bc1b4, 
  0xd93afc01, 0xc740e934, 0x66ca5c05, 0xb700cf06, 
  0x087bb95c, 0xaf96e541, 0x0165f9b9, 0x2ca641fe, 
  0x9b921cca, 0x20ecbf14, 0x3c849f42, 0xefd62ee6, 
  0x3423b4ed, 0xb4ff14a1, 0x9c96ed60, 0x5e235637, 
  0xaff1b372, 0x5a47d7be, 0x67ee2752, 0xab6123cf, 
  0x148fd023, 0x620cb492, 0x1ea6d027, 0x2e474a99, 
  0x85dac644, 0x2e122c5d, 0xdaa7192d, 0x6ad1fdde, 
  0xc90ef8db, 0x2bd580ad, 0xc7cbb2d4, 0x57fa1bbd, 
  0x0dd85260, 0x91eb2104, 0xb93b5665, 0x7772f2a9, 
  0xa6116afd, 0xc1c16356, 0x63e39ac3, 0x2ae74925, 
  0xb94eda3a, 0x8fc60c60, 0x10a08e7e, 0x277e121f, 
  0x6ba9c263, 0x6fb143a4, 0x2edd7d51, 0x70672adf, 
  0xed83a740, 0x0c431229, 0xe43afb53, 0x6c72cdc4, 
  0x57779036, 0xea95957e, 0x8dd2ee33, 0x20586433, 
  0x1fe6793d, 0x2a5415b6, 0x5f16357b, 0xea8aa0d2, 
  0xb23a3d52, 0x19a0fbe5, 0x208f4c43, 0x865a4819, 
  0xcabd4e46, 0x0eec2c48, 0xceb356ba, 0xaf400c3e, 
  0x7c95c819, 0x098a2776, 0x3430e52d, 0x872ef158, 
  0x26931eb7, 0x332c9f45, 0x5a3e9563, 0x4ad3cb02, 
  0x6024db4d, 0xd79936bc, 0x058d72d2, 0xaaf7ff49, 
  0x50bae51b, 0x0f12ab95, 0xa53fbf68, 0xd37a7e21, 
  0x9a4c9cb8, 0xae9dbbf4, 0xa810739f, 0xb31b24a0, 
  0xa52e56b2, 0xb01c3819, 0xad56fc08, 0xc75fae5c, 
  0xf717cdb6, 0x10cc6cf7, 0xa3697fa2, 0x1acd94b6, 
  0x7dbbdd1e, 0x5d8f6cdd, 0x033c3a4b, 0x476498c3, 
  0xb2f9511e, 0x9d5a6fcd, 0x277bcfe9, 0xcdde5074, 
  0xee3a6962, 0x0ccd1fc5, 0x96fdd87c, 0x73c6d8c3, 
  0xecf45fc2, 0x28eff314, 0x24feb4dd, 0xdb1b7dd6, 
  0xbfd662e4, 0xb39fb30b, 0xbb035e74, 0xff2c3729, 
  0xe45e7d1c, 0xa2fecc8e, 0x59e18deb, 0x4fc1dbc4, 
  0xd93dac7b, 0xda19b907, 0xd05003cd, 0xfbc9faab
};
#endif

#if HASH == HASH_ZOB
static tagHC hashHC (tag h, list l)
{
  register tagHC hash = sizeof(tag) + sizeof(list);
  
  for (register int i=0; i<sizeof(list); ++i)
    hash ^= tab[i][*((unsigned char *) &l + i)];
  for (register int i=0; i<sizeof(tag); ++i)
    hash ^= tab[i][*((unsigned char *) &h + i)];

  return hash & HCMASK;
}
#endif

#if HASH == HASH_CRC
static tagHC hashHC (tag h, list l)
{
  register tagHC hash = sizeof(tag) + sizeof(list);

  for (register int i=0; i<sizeof(list); ++i)
    hash = (hash<<8) ^ tab[0][(hash>>24) ^ *((unsigned char *) &l + i)];
  for (register int i=0; i<sizeof(tag); ++i)
    hash = (hash<<8) ^ tab[0][(hash>>24) ^ *((unsigned char *) &h + i)];

  return hash & HCMASK;
}
#endif

#if HASH == HASH_FNV
static tagHC hashHC (tag h, list l)
{
  register tagHC hash = 0x811c9dc5;

  for (register int i=0; i<sizeof(list); ++i) {
     /* multiply by the 32 bit FNV magic prime mod 2^32 */
#if defined(NO_FNV_GCC_OPTIMIZATION)
     hash *= 0x01000193;
#else
     hash += (hash<<1) + (hash<<4) + (hash<<7) + (hash<<8) + (hash<<24);
#endif
     /* xor the bottom with the current octet */
     hash ^= *((unsigned char *) &l + i);
  }
  for (register int i=0; i<sizeof(tag); ++i) {
     /* multiply by the 32 bit FNV magic prime mod 2^32 */
#if defined(NO_FNV_GCC_OPTIMIZATION)
     hash *= 0x01000193;
#else
     hash += (hash<<1) + (hash<<4) + (hash<<7) + (hash<<8) + (hash<<24);
#endif
     /* xor the bottom with the current octet */
     hash ^= *((unsigned char *) &h + i);
  }

  return hash & HCMASK;
}
#endif

#if HASH == HASH_ONE
static tagHC hashHC (tag h, list l)
{
  register tagHC hash = 0;
 
  for (register int i=0; i<sizeof(list); ++i) {
    hash += *((unsigned char *) &l + i);
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  for (register int i=0; i<sizeof(tag); ++i) {
    hash += *((unsigned char *) &h + i);
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);

  return hash & HCMASK;
}
#endif

#if HASH == HASH_WANG
/* what do we do with this? */
static tagHC hashHC (tag h, list l)
{
  register tagHC hash = l;
 
  hash += ~(hash << 15);
  hash ^=  (hash >> 10);
  hash +=  (hash << 3);
  hash ^=  (hash >> 6);
  hash += ~(hash << 11);
  hash ^=  (hash >> 16);

  hash ^= h;
  
  return hash & HCMASK;
}
#endif

#if HASH == HASH_JJ
/* what do we do with this? */
static tagHC hashHC (tag h, list l)
{
  register tagHC hash = l;

  hash += (hash << 12);
  hash ^= (hash >> 22);
  hash += (hash << 4);
  hash ^= (hash >> 9);
  hash += (hash << 10);
  hash ^= (hash >> 2);
  hash += (hash << 7);
  hash ^= (hash >> 12);

  hash ^= h;
  
  return hash & HCMASK;
}
#endif

#if HASH == HASH_DJB
static tagHC hashHC (tag h, list l)
{
  register tagHC hash = 5381;

  for (register int i=0; i<sizeof(list); ++i)
    hash = ((hash << 5) + hash) + *((unsigned char *) &l + i);
  for (register int i=0; i<sizeof(tag); ++i)
    hash = ((hash << 5) + hash) + *((unsigned char *) &h + i);

  return hash & HCMASK;
}
#endif

#if HASH == HASH_DJB_S
static tagHC hashHC (tag h, list l)
{
  register tagHC hash = 5381;

  hash = ((hash << 5) + hash) + (((tagHC) l)^((tagHC) h<<13));

  return hash & HCMASK;
}
#endif


/* Initialization */

void initializeHC ()
{
  /* allocating the hash table */
  if (tableHC != NULL)
    free(tableHC);
  tableHC = (struct bucketHC *) malloc(HCTABLESIZE * sizeof(struct bucketHC));
  if (tableHC == NULL) {
    fprintf(stderr, "Out of memory: allocating the table for hash consing\n");
    exit(1);
  }

  /* initializing the hash table */
  for (tagHC i = 0; i < HCTABLESIZE; i++)
    tableHC[i].ptr = HCNUL;

  /* allocating the heap */
  if (heapHC != NULL)
    free(heapHC);
  heapHC = (struct nodeHC *) malloc(HCHEAPSIZE * sizeof(struct nodeHC));
  if (heapHC == NULL) {
    fprintf(stderr, "Out of memory: allocating the heap for hash consing\n");
    exit(1);
  }

  /* the first heap record: the empty list */
  heapHC[0].head = 0;           /* we need to clear the active flag */
  heapHC[0].tail = HCNUL;	/* the empty list doesn't have a tail */
  heapHC[0].ptr = HCNUL;	/* null */

  /* the rest of heap records */
  for (tagHC i = 1; i < HCHEAPSIZE-1; i++)
    heapHC[i].ptr = i+1;        /* add it to the list of free nodes */
			
  /* the last heap record */
  heapHC[HCHEAPSIZE-1].ptr = HCNUL;   /* null */

  /* initializing the state */
  entriesHC = 1;
  HCGCLIMIT = HCHEAPSIZE * 0.98 - MAXDIM;
  freeHC = 1;
#ifdef STATS
  totalHits = 0;
  totalMisses = 0;
  totalCollected = 0;
  totalCycles = 0;
#endif   

  printf("HC memory: %u bytes\n", HCTABLESIZE * sizeof(struct bucketHC)
	 + HCHEAPSIZE * sizeof(struct nodeHC));
}


/* Hash consing */

static list hashCons (tag h, list l)
{
#ifdef DEBUG
#ifdef TRACE
  printf("hashCons: %u", h);
  for (list ll = l; ll != 0; ll = heapHC[ll].tail)
    printf(": %u", heapHC[ll].head);
  printf("\n");
  fflush(stdout);
#endif
#endif

  tagHC hashval = hashHC(h, l);
  tagHC ptr;
#ifdef STATS  
  unsigned chainLength = 0;
#endif   

  for (ptr = tableHC[hashval].ptr; ptr != HCNUL; ptr = heapHC[ptr].ptr)
    if (heapHC[ptr].head == h && heapHC[ptr].tail == l) {
#ifdef STATS        
      totalHits++;
#endif       
      return ptr;
    }
    else {
#ifdef STATS    
      chainLength++;
#endif       
      ;
    }

#ifdef STATS
  if (chainLength > maxChainLength)
    maxChainLength = chainLength;
  sumChainLength += chainLength;
  countChainLength++;
#endif 

  ptr = freeHC;
  freeHC = heapHC[freeHC].ptr;
  entriesHC++;

  heapHC[ptr].ptr = tableHC[hashval].ptr;
  heapHC[ptr].head = h;
  heapHC[ptr].tail = l;
  tableHC[hashval].ptr = ptr;

#ifdef STATS
  totalMisses++;
#endif   
  return ptr;
}


/* Garbage collection */

void gcollectHC (World *w)
{
#ifdef STATS        
  totalCycles++;
#endif   

#ifdef DEBUG
  printf("BEGIN:\tgarbage collecting the heap, entries=%u\n", entriesHC);
  fflush(stdout);
#endif

  /* MARKing phase */
	
#ifdef DEBUG
  printf("\tmarking\n");
  fflush(stdout);
#endif

  /* go through the stack of active worlds */
  for (; w != NULL; w = w->prev)
    for (dimension d = 0; d < MAXDIM; d++)
      for (list l = w->l[d];
	   l != 0 && !(heapHC[l].head & ACTIVEMASK);
	   l = heapHC[l].tail) {
	heapHC[l].head |= ACTIVEMASK;
#ifdef DEBUG
#ifdef SANITY
	printf("SANITY:\tmarking HC place=%u\n", l);
	fflush(stdout);
#endif
#endif
      }
  
#ifdef DEBUG
  printf("\ttraversing the warehouse\n");
  fflush(stdout);
#endif

#ifdef GC_KEEP_WAREHOUSE
  /* go through the warehouse and make more lists active */
  for (tagWH i = 0; i < WHTABLESIZE; i++)
    for (tagWH ptr = tableWH[i].ptr; ptr != WHNUL; ptr = heapWH(ptr)->ptr)
      for (dimension d = 0; d < heapWH(ptr)->depDim; d++)             /* careful! */
	for (list l = heapWH(ptr)->l[d];
	     l != 0 && !(heapHC[l].head & ACTIVEMASK);
	     l = heapHC[l].tail) {
	  heapHC[l].head |= ACTIVEMASK;

#ifdef DEBUG
#ifdef SANITY
	  printf("SANITY:\tmarking HC place=%u\n", l);
	  fflush(stdout);
#endif
#endif
	}
#else
  /* go through the warehouse and collect entries with inactive lists */
  for (tagWH i = 0; i < WHTABLESIZE; i++)
    for (tagWH * ptr = &(tableWH[i].ptr); *ptr != WHNUL;) {
      if (heapWH(*ptr)->val != WAITINGVALUE)
        for (dimension d = 0; d < heapWH(*ptr)->depDim; d++) {        /* careful! */
          list l = heapWH(*ptr)->l[d];

          if (l != 0 && !(heapHC[l].head & ACTIVEMASK)) {
            tagWH prev = *ptr;

#ifdef DEBUG
#ifdef SANITY
            printf("SANITY:\tcollecting WH place=%u\n", prev);
            fflush(stdout);
#endif
#endif
            *ptr = heapWH(*ptr)->ptr;
            heapWH(prev)->ptr = freeWH;
            freeWH = prev;
            entriesWH--;
#ifdef STATS            
            totalCollected++;
#endif             
            /* must update ages in the warehouse and the global age */
            goto skipForwardingPtr;  /* I hate this, wish it were a continue */
          }
        }
      ptr = &(heapWH(*ptr)->ptr);

skipForwardingPtr:
	return;

    }
#endif

  /* SWEEPing phase */

#ifdef DEBUG
  printf("\tsweeping\n");
  fflush(stdout);
#endif

  for (tagHC i = 0; i < HCTABLESIZE; i++)
    for (tagHC * ptr = &(tableHC[i].ptr); *ptr != HCNUL; )
      if (heapHC[*ptr].head & ACTIVEMASK) {
#ifdef DEBUG
#ifdef SANITY
	printf("SANITY:\tresetting HC place=%u\n", *ptr);
	fflush(stdout);
#endif
#endif
	heapHC[*ptr].head &= ~ACTIVEMASK;
	ptr = &(heapHC[*ptr].ptr);
      }
      else {
	tagHC prev = *ptr;

#ifdef DEBUG
#ifdef SANITY
	printf("SANITY:\tcollecting HC place=%u\n", prev);
	fflush(stdout);
#endif
#endif
	*ptr = heapHC[*ptr].ptr;
	heapHC[prev].ptr = freeHC;
	freeHC = prev;
	entriesHC--;
#ifdef STATS	
	totalCollected++;
#endif 	
      }

#ifdef DEBUG
  printf("END:\tgarbage collecting the heap, entries=%u\n", entriesHC);
  fflush(stdout);
#endif

#ifdef GC_KEEP_WAREHOUSE
  if (entriesHC >= HCGCLIMIT) {
    gcollectWH();
    gcollectHC(w);
#endif
    if (entriesHC >= HCHEAPSIZE - MAXDIM) {
      fprintf(stderr, "FATAL ERROR: Out of heap space for hash consing\n");
      exit(1);
    }
#ifdef GC_KEEP_WAREHOUSE
  }
#endif
}


/* Intensional operators */

list call (tag i, list l)
{
  return hashCons(i, l);
}

list actuals (tag i, list l)
{
  if (heapHC[l].head == i)
    return heapHC[l].tail;

  fprintf(stderr, "Internal error: wrong actuals (%u instead of %u)\n",
	  heapHC[l].head, i);
  exit(1);
}

tag cases (list l)
{
  return heapHC[l].head;
}


/* Statistics */

void statsHC ()
{
  printf("\n\nHC heapsize:\t%u\nHC freerecs:\t%.2lf%%\n\n",
	 HCHEAPSIZE, ((double) (HCHEAPSIZE - entriesHC) / HCHEAPSIZE) * 100);
#ifdef STATS	 
  printf("HC total seeks:\t%u\n"
	 "        cycles:\t%u\n"
	 "     collected:\t%u\n"
         "          hits:\t%u (%.2lf%%)\n"
         "        misses:\t%u (%.2lf%%)\n"
	 "   max v chain:\t%u\n"
	 "  mean v chain:\t%.3lf\n\n",
         totalHits + totalMisses, totalCycles, totalCollected,
         totalHits, (double) totalHits / (totalHits + totalMisses) * 100,
         totalMisses, (double) totalMisses / (totalHits + totalMisses) * 100,
	 maxChainLength, (double) sumChainLength / countChainLength);
#endif

#ifdef HCDUMP
  for (tagHC i = 0; i < HCTABLESIZE; i++)
    printf("tableHC[%d]: %u\n", i, tableHC[i].ptr);
  for (tagHC i = 0; i < HCHEAPSIZE; i++)
    printf("heapHC[%d]: %u %u\n", i, heapHC[i].head, heapHC[i].tail);
  printf("entriesHC: %u\n", entriesHC);
  printf("freeHC: %u\n", freeHC);
#endif
}


/* Printing lists */

void printList (list l)
{
   printf("[");
	while (l != 0) {
      printf("%d", heapHC[l].head);
      l = heapHC[l].tail;
      if (l != 0)
         printf(",");
   }
	printf("]");
}
