#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>
#include <pthread.h>
//#define NDEBUG 1
#include <assert.h>

#include "StringTable_cbits.h"

static pthread_mutex_t mutex_hash = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mutex_string = PTHREAD_MUTEX_INITIALIZER;

// 23 bits of chunk space to leave one bit for 'valid' flag.
// valid flag must be set to 1 for it to be a valid atom

static void dieif(bool,char *);
static uint32_t hash2(uint32_t salt,unsigned char *key, int key_len);
static uint32_t hash3(uint32_t salt,unsigned char *key, int key_len);

// string allocation stuff

#define NUM_CHUNKS 256
#define CHUNK_SIZE 32768



#define ATOM_LEN(c)     (((atom_t)(c)) & ATOM_LEN_MASK)
#define CHUNK_INDEX(c)  (((atom_t)(c) >> 8)&0xFF)
#define CHUNK_OFFSET(c) (((atom_t)(c) & ~VALID_BITMASK) >> 16 )

#define MAKE_ATOM(ci,co,len) ((((len) & 0xFF) | ((((unsigned)ci) & 0xff) << 8) | (((unsigned)co) << 16))|VALID_BITMASK)

#define ATOM_PTR(c) (&(stringtable_chunks[CHUNK_INDEX(c)][CHUNK_OFFSET(c)]))

#define ATOM_VALID(a) ((a) & VALID_BITMASK)



static unsigned char first_chunk[CHUNK_SIZE];
static unsigned char *stringtable_chunks[NUM_CHUNKS] = { first_chunk };

static uint16_t current_chunk = 0;
static uint16_t next_free_offset = 0;

static atom_t
add_string(unsigned char *cs, int len)
{
    pthread_mutex_lock(&mutex_string);
        //printf("add_string(%c,%c,%i)\n",cs[0],cs[1],len);
    assert(len >= 0);
    assert(len < MAX_ENTRY_SIZE);
    assert(next_free_offset < CHUNK_SIZE);
    if(next_free_offset + 1 > CHUNK_SIZE - MAX_ENTRY_SIZE) {
        dieif(current_chunk >= NUM_CHUNKS - 1, "No more chunks");
        current_chunk++;
        assert(!stringtable_chunks[current_chunk]);
        stringtable_chunks[current_chunk] = malloc(CHUNK_SIZE);
        dieif(!stringtable_chunks[current_chunk], "error alocating memory");
        next_free_offset = 0;
    }
    memcpy(stringtable_chunks[current_chunk] + next_free_offset, cs, len);
    atom_t r = MAKE_ATOM(current_chunk, next_free_offset, len);
    assert(CHUNK_INDEX(r) == current_chunk);
    assert(CHUNK_OFFSET(r) == next_free_offset);
    assert(ATOM_PTR(r) == stringtable_chunks[current_chunk] + next_free_offset);
    assert(ATOM_LEN(r) == len);
    next_free_offset += len;
    assert(next_free_offset < CHUNK_SIZE);
    assert(current_chunk < NUM_CHUNKS);
    pthread_mutex_unlock(&mutex_string);
    return r;
}

// hashtable stuff

#define KEEP_HASH 2

#define CUCKOO_HASHES 2U
#define CUCKOO_BUCKETS 2U

typedef uint32_t hash_t;

struct hentry {
#if KEEP_HASH
        hash_t hashes[CUCKOO_HASHES];
#endif
        atom_t atom;
};

#define INIT_SIZE 2
static uint32_t hsize = INIT_SIZE;
static struct hentry init_htable[(1 << INIT_SIZE) * CUCKOO_HASHES];
static struct hentry *htable = init_htable;

#define HASHSIZE  (1 << hsize)
#define HASHMASK (HASHSIZE - 1)

#define INDEX_HASH(i)  ((i) / HASHSIZE)
#define HASH_INDEX(h,x) ((h * HASHSIZE) + (HASHMASK & ((uint32_t)x)))

#define HASH_BUCKET(x,b) ((((x) + (b)) % HASHSIZE) + (INDEX_HASH(x)*HASHSIZE))

#define DEPTH_LIMIT 512

static void hash_insert(struct hentry x);

static void
fast_insert(int t, int tb, struct hentry hb) {
        hash_insert(hb);
}

bool
atom_exists(atom_t a) {
        for(int i = 0; i < HASHSIZE*CUCKOO_HASHES; i++) {
                if(a == htable[i].atom) return true;
        }
        return false;
}
bool
item_exists(char *cs, int len) {
        for(int i = 0; i < HASHSIZE*CUCKOO_HASHES; i++) {
                atom_t a = htable[i].atom;
                if(ATOM_VALID(a)) {
                    if(len == ATOM_LEN(a) && !memcmp(ATOM_PTR(a),cs,len))
                        return true;
                }
        }
        return false;
}

void
dump_table(void) {
        for(int i = 0; i < HASHSIZE*CUCKOO_HASHES; i++) {
                atom_t a = htable[i].atom;
                if(ATOM_VALID(a)) {
                        printf("%p %u: ",ATOM_PTR(a),ATOM_LEN(a));
                        fwrite(ATOM_PTR(a),1,ATOM_LEN(a),stdout);
                        fwrite("\n",1,1,stdout);
                }
        }
}


static void
grow_table(void) {
    fprintf(stderr,"grow_table[[[\n");
        uint32_t os = (1 << hsize++) * CUCKOO_HASHES;
        struct hentry *ot = htable;
        htable = calloc(sizeof(struct hentry),CUCKOO_HASHES * (1 << hsize));

        for(int i = 0; i < os; i++) {
                if(ATOM_VALID(ot[i].atom))
                        fast_insert(0,0,ot[i]);
        }
        if(ot != init_htable) free(ot);
    fprintf(stderr,"]]]\n");
}

#if KEEP_HASH
#define FHASH(x,i) ((x).hashes[i])
#else
#define FHASH(x,i) (hash2(i,ATOM_PTR((x).atom),ATOM_LEN((x).atom)))
#endif

static void
hash_insert(struct hentry x) {
        assert(ATOM_VALID(x.atom));
         fprintf(stderr,"hash_insert(%x,%p:%i,%x,%x,[%x,%x]", x.atom, ATOM_PTR(x.atom), ATOM_LEN(x.atom), x.hashes[0], x.hashes[1],HASH_INDEX(0,x.hashes[0]),HASH_INDEX(1,x.hashes[1]));
        assert(!atom_exists(x.atom));
        if(item_exists(ATOM_PTR(x.atom),ATOM_LEN(x.atom))) 
            dump_table();
        assert(!item_exists(ATOM_PTR(x.atom),ATOM_LEN(x.atom)));
        atom_t start = x.atom;
        for(int loop = 0; loop < DEPTH_LIMIT;loop++) {
                for(int i = 0; i < CUCKOO_HASHES; i++) {
//                        int e = HASH_INDEX(i,FHASH(x,i));
                        for(int j = 0; j < CUCKOO_BUCKETS; j++) {
                                //#struct hentry *b = &(htable[(e + j) & HASHJMASK ]);
                                //struct hentry *b = &htable[HASH_BUCKET(e,j)];
                                struct hentry *b = &htable[HASH_INDEX(i,FHASH(x,i) + j)];
                                if(!ATOM_VALID(b->atom)) {
                                        *b = x;
                                        fprintf(stderr,")\n");
                                        return;
                                }
                                struct hentry tb = x;
                                x = *b;
                                *b = tb;
                        }
                        // struct hentry *b = &(htable[e]);
                }
                if(x.atom == start) {
                        break;
                }
        }
        grow_table();
        fprintf(stderr,"R");
        return hash_insert(x);
}



atom_t
stringtable_lookup(unsigned char *cs, int len)
{
        fprintf(stderr,"stringtable_lookup(%c,%c,%*s,%i)\n",cs[0],cs[1],len,cs,len);
        pthread_mutex_lock(&mutex_hash);
        assert(len >= 0);
        assert(len < MAX_ENTRY_SIZE);
        hash_t h[CUCKOO_HASHES];
        for(uint32_t i = 0; i < CUCKOO_HASHES; i++) {
                h[i] = hash2(i,cs,len);
                //int e = HASH_INDEX(i,h[i]);
                for(int j = 0; j < CUCKOO_BUCKETS; j++) {
                        //struct hentry *b = &htable[(e + i) & HASHJMASK ];
                        //struct hentry *b = &htable[HASH_BUCKET(e,j)];
                        struct hentry *b = &htable[HASH_INDEX(i,h[i] + j)];
#if KEEP_HASH
                        if (ATOM_VALID(b->atom) && h[i] == b->hashes[i] && len == ATOM_LEN(b->atom) &&  !memcmp(ATOM_PTR(b->atom),cs,len)) {
                            pthread_mutex_unlock(&mutex_hash);
                            return b->atom;
                        }
#else
                        if (ATOM_VALID(b->atom) && len == ATOM_LEN(b->atom) && !memcmp(ATOM_PTR(b->atom),cs,len)) {
                            pthread_mutex_unlock(&mutex_hash);
                            return b->atom;
                        }
#endif
                }
        }
        atom_t na = add_string(cs,len);
        struct hentry hb;
        hb.atom = na;
#if KEEP_HASH
        memcpy(hb.hashes,h,sizeof hb.hashes);
#endif
        hash_insert(hb);
        pthread_mutex_unlock(&mutex_hash);
        return na;
}



int
lexigraphic_compare(atom_t x, atom_t y)
{
    int xl = ATOM_LEN(x);
    int yl = ATOM_LEN(y);
    return memcmp(ATOM_PTR(x),ATOM_PTR(y),xl < yl ? xl : yl) || xl - yl;
}


atom_t
atom_append(atom_t x,atom_t y)
{
    unsigned char *xs,*ys;
    int xl,yl;

    xl = stringtable_find(x,&xs);
    yl = stringtable_find(y,&ys);

    unsigned char buf[MAX_ENTRY_SIZE];

    memcpy(buf,xs,xl);
    memcpy(buf + xl,ys,yl);

    return stringtable_lookup(buf,xl + yl);
}

char *
stringtable_ptr(atom_t cl)
{
        assert(ATOM_VALID(cl));
        return ATOM_PTR(cl);
}

int
stringtable_get(atom_t cl, char buf[MAX_ENTRY_SIZE])
{
        assert(ATOM_VALID(cl));
        memcpy(buf,ATOM_PTR(cl),ATOM_LEN(cl));
        return ATOM_LEN(cl);
}

int
stringtable_find(atom_t cl, unsigned char **res)
{
        assert(ATOM_VALID(cl));
        *res = ATOM_PTR(cl);
        return ATOM_LEN(cl);
}

void
stringtable_stats(void)
{
    unsigned static_memory = sizeof(stringtable_chunks);
    printf("Static Memory: %u\n", static_memory);
    unsigned dynamic_memory = (current_chunk + 1) * CHUNK_SIZE;
    unsigned data_memory = current_chunk*CHUNK_SIZE + next_free_offset;;
    printf("Used Chunks: %u/%u - %u bytes\n", current_chunk + 1, NUM_CHUNKS, data_memory);
    unsigned num_entries = 0;
    unsigned hash_types[CUCKOO_HASHES];
    memset(hash_types,0,sizeof hash_types);
    unsigned num_total = 0;
    for(int i = 0; i < HASHSIZE * CUCKOO_HASHES; i++) {
            num_total++;
            dynamic_memory += sizeof(struct hentry);
            if(ATOM_VALID(htable[i].atom)) {
                    num_entries++;
                    hash_types[i / HASHSIZE]++;
            }
    }

    for(int i = 0; i < CUCKOO_HASHES; i++)
            printf("Hash Table  %i: %u\n", i, hash_types[i]);

    printf("Usage: %u/%u %.3f%%\n", num_entries, num_total, (double)num_entries * 100.0 / num_total);

    printf("Dynamic Memory: %u\n", dynamic_memory);
    printf("Storage Efficiency: %.3f%%\n", (double)data_memory * 100.0/ dynamic_memory);

}




static void
dieif(bool w,char *str)
{
    if(w) {
        fprintf(stderr, "stringlib: %s\n", str);
        exit(1);
    }
}

// hash functions


static uint32_t
hash2(uint32_t salt, unsigned char *key, int key_len)
{
        uint32_t hash = salt;
        for (int i = 0; i < key_len; i++) {
                hash += key[i];
                hash += (hash << 10);
                hash ^= (hash >> 6);
        }
        hash += (hash << 3);
        hash ^= (hash >> 11);
        hash += (hash << 15);
        return hash;
}

/*
int
main(int argc, char *argv[])
{
    unsigned char buf[MAX_ENTRY_SIZE];
    while(fgets((char *)buf,MAX_ENTRY_SIZE,stdin)) {
        buf[MAX_ENTRY_SIZE - 1] = '\0';
        unsigned len = strlen((char *)buf);
        if(!len) continue;
        if(buf[len - 1] == '\n') buf[--len] = '\0';
        stringtable_lookup(buf,len);
        //printf("%x: %s\n", a, buf);

    }

    dump_table();
    stringtable_stats();

    return 0;
}
static hash_t
hash3(uint32_t salt, unsigned char* str, size_t len)
{
        const uint32_t fnv_prime = 0x811C9DC5;
        unsigned int hash      = salt;
        for(int i = 0; i < len; i++) {
                hash *= fnv_prime;
                hash ^= str[i];
                hash ^= salt;
        }
        return hash;
}
*/
