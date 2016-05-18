#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>


int *_initArray(int size, int init)
{int i;
 int *a = (int *)malloc(sizeof(int) + size*sizeof(int));
 a[0]=size;
 for(i=1;i<=size;i++) a[i]=init;
 return a+1;
}

void _checkIndexArray(int *a, int i)
{
	if(i<0 || i>a[-1]) {
		fprintf(stderr, "indice %d invalido en array..\n", i);
		exit(-1);
	}
}

void _checkNil(int *a) {
	if (a == 0) 
		exit (-1);
}

int *_allocRecord(int size, ...)
{int i;
 int *p, *a;
 va_list va; 
 p = a = (int *)malloc(size*sizeof(int));
 va_start(va, size);
 for(i=0;i<size;i++) *p++ = va_arg(va, int);
 return a;
}

struct string {int length; unsigned char chars[1];};

int _stringcmp(struct string *s, struct string *t)
{
 if (s==t) return 0;
 int i;
 int bound = (s->length < t->length) ? s->length : t->length;
 for(i=0;i<bound;i++) if (s->chars[i]!=t->chars[i]) return s->chars[i] - t->chars[i];
 return (s->length - t->length);
}

void print(struct string *s)
{int i; unsigned char *p=s->chars;
 for(i=0;i<s->length;i++,p++) putchar(*p);
}

void printInteger(int number) {
	printf("%d", number); 
}

void flush()
{
 fflush(stdout);
}

struct string consts[256];
struct string empty={0,""};

int main()
{int i;
 for(i=0;i<256;i++)
   {consts[i].length=1;
    consts[i].chars[0]=i;
   }
 tigermain(0 /* static link!? */);
 return 0;
}

int ord(struct string *s)
{
 if (s->length==0) return -1;
 else return s->chars[0];
}

struct string *chr(int i)
{
 if (i<0 || i>=256) 
   {printf("chr(%d) out of range\n",i); exit(1);}
 return consts+i;
}

int size(struct string *s)
{ 
 return s->length;
}

struct string *substring(struct string *s, int first, int n)
{
 if (first<0 || first+n>s->length)
   {printf("substring([%d],%d,%d) out of range\n",s->length,first,n);
    exit(1);}
 if (n==1) return consts+s->chars[first];
 {struct string *t = (struct string *)malloc(sizeof(int)+n);
  int i;
  t->length=n;
  for(i=0;i<n;i++) t->chars[i]=s->chars[first+i];
  return t;
 }
}

struct string *concat(struct string *a, struct string *b)
{
 if (a->length==0) return b;
 else if (b->length==0) return a;
 else {int i, n=a->length+b->length;
       struct string *t = (struct string *)malloc(sizeof(int)+n);
       t->length=n;
       for (i=0;i<a->length;i++)
	 t->chars[i]=a->chars[i];
       for(i=0;i<b->length;i++)
	 t->chars[i+a->length]=b->chars[i];
       return t;
     }
}

int not(int i)
{ return !i;
}

void _exit(int i) 
{
	exit(i); 
}

struct string *getstr()
{int i=getc(stdin);
 if (i==EOF) return &empty;
 else return consts+i;
}
