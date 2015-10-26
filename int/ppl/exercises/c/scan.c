#include <stdio.h>
#include <math.h>    /* pow */
#include <string.h>  /* strlen */
#include <ctype.h>   /* tolower */

#define BASE36_BUFLEN  7

static char *_base36_set = "0123456789abcdefghijklmnopqrstuvwxyz";
static int is_base36(signed char *text)
{
	signed char *p;  /* on non EBCDIC machines char is signed */

	for (p = text; *p; p++) {
		if (  ('0' <= *p && '9' >= *p)
		   || ('a' <= *p && 'z' >= *p)
		   || ('A' <= *p && 'Z' >= *p)
		   )
			continue;
		return 0;
	}

	return 1;
}

static int base362ulong(unsigned long *val, signed char *b36)
{
	size_t len = 0;
	unsigned long total = 0;
	unsigned long pval = 0;
	signed char *p;
	signed char *t;

	len = strlen(b36);
	if (BASE36_BUFLEN - 1 < len || 1 > len)
		return -1;

	len--;
	for (p = b36; *p; p++, len--) {
		t = strchr(_base36_set, tolower(*p));
		if (!t)
			return -1;
		pval = (unsigned long)t - (unsigned long)_base36_set;
		pval = pval * (unsigned long)pow(36, len);
		total += pval;
	}

	*val = total;
	return 0;
}

static char *numstrrev(signed char *to, signed char *from)
{
	int len = 0;
	signed char *p;
	signed char *q;

	len = strlen(from);
	while (len > 1 && '0' == from[len - 1]) {
		from[len - 1] = '\0';
		len = strlen(from);
	}

	for (p = from + len - 1, q = to; p >= from; p--, q++)
		*q = *p;

	return to;
}

static int ulong2base36(signed char *b36, unsigned int len, unsigned long val)
{
	int i = 1;
	unsigned long pval = 0;
	signed char reverse[BASE36_BUFLEN] = {0}; /* HERE */
	signed char *p;

	pval = val % (unsigned long)pow(36, 1) / (unsigned long)pow(36, 0);
	if (1 >= len)
		return -1;
	if (!pval)
		reverse[0] = '0';
	len = len > BASE36_BUFLEN ? BASE36_BUFLEN : len;  /* HERE */
	for (i = 2, len = len - 1, p = reverse; len; i++, p++, len--) {
		*p = _base36_set[pval];
		pval = val % (unsigned long)pow(36, i)
		       / (unsigned long)pow(36, i - 1);
	}

	(void)numstrrev(b36, reverse);
	b36[strlen(reverse)] = '\0';
	return 0;
}

static int test1(void)
{
	int ret = 0;
	int var = 0;
	unsigned long l = 0;
	signed char b36[BASE36_BUFLEN] = {0};
	signed char blb[BASE36_BUFLEN + 12] = {0};  /* HERE */
	ret = sscanf("Z051", "%i", &var);
	printf("var = [%i], ret = [%i], b36 size [%i]\n", /* HERE */
	       var, ret, sizeof(b36));

	ret = is_base36("1293");                       /* true */
	printf("is_base36 [%i] (true/false)\n", ret);
	ret = is_base36("3^T91");                      /* false */
	printf("is_base36 [%i] (true/false)\n", ret);
	ret = is_base36("tf573");                      /* false */
	printf("is_base36 [%i] (true/false)\n", ret);

	ret = base362ulong(&l, "");                    /* fail */
	printf("base362ulong [%lu] [%i]\n", l, ret);
	ret = base362ulong(&l, "E3bCT91");             /* fail */
	printf("base362ulong [%lu] [%i]\n", l, ret);
	ret = base362ulong(&l, "3^T91");               /* fail */
	printf("base362ulong [%lu] [%i]\n", l, ret);
	ret = base362ulong(&l, "M");                   /* 22 */
	printf("base362ulong [%lu] [%i]\n", l, ret);
	ret = base362ulong(&l, "MF");                  /* 807 */
	printf("base362ulong [%lu] [%i]\n", l, ret);
	ret = base362ulong(&l, "1293");                /* 49 575 */
	printf("base362ulong [%lu] [%i]\n", l, ret);
	ret = base362ulong(&l, "tf573");               /* 49 415 439 */
	printf("base362ulong [%lu] [%i]\n", l, ret);
	ret = base362ulong(&l, "tAf573");              /* 1 771 021 839 */
	printf("base362ulong [%lu] [%i]\n", l, ret);

	l = 0;
	ret = ulong2base36(b36, sizeof(b36), l);       /* 0 */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	l = 22;
	ret = ulong2base36(b36, sizeof(b36), l);       /* m */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	l = 807;
	ret = ulong2base36(b36, sizeof(b36), l);       /* mf */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	l = 49575;
	ret = ulong2base36(b36, sizeof(b36), l);       /* 1293 */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	l = 49415439;
	ret = ulong2base36(b36, sizeof(b36), l);       /* tf573 */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	l = 1771021839;
	ret = ulong2base36(b36, sizeof(b36), l);       /* taf573 */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);

	ret = ulong2base36(b36, 0, l);                 /* fail */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	ret = ulong2base36(b36, 9, l);                 /* af573 */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	ret = ulong2base36(blb, 9, l);                 /* af573 HERE */
	printf("ulong2base36 [%s] [%i]\n", blb, ret);
	ret = ulong2base36(b36, 8, l);                 /* af573 */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	ret = ulong2base36(blb, 8, l);                 /* af573 HERE */
	printf("ulong2base36 [%s] [%i]\n", blb, ret);
	ret = ulong2base36(b36, 7, l);                 /* af573 */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	ret = ulong2base36(blb, 7, l);                 /* af573 HERE */
	printf("ulong2base36 [%s] [%i]\n", blb, ret);
	ret = ulong2base36(b36, 6, l);                 /* af573 */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	ret = ulong2base36(blb, 6, l);                 /* af573 HERE */
	printf("ulong2base36 [%s] [%i]\n", blb, ret);
	ret = ulong2base36(b36, 5, l);                 /* f573 */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	ret = ulong2base36(b36, 4, l);                 /* 573 */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	ret = ulong2base36(b36, 3, l);                 /* 73 */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	ret = ulong2base36(b36, 2, l);                 /* 3 */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);
	ret = ulong2base36(b36, 1, l);                 /* fail HERE */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);

	l = 807;
	ret = ulong2base36(b36, 4, l);                 /* mf */
	printf("ulong2base36 [%s] [%i]\n", b36, ret);

	return 0;
}

int main(int argc, char **argv)
{
	unsigned long l = 0;
	signed char b36[BASE36_BUFLEN] = {0};
	static char *usage = "%s [-b|-n|-t] number\n"
			     "\t where -b <22> converts decimal -> base36\n"
			     "\t       -n <3t> converts base36  -> decimal\n"
			     "\t       -t <1>  runs a throughout test\n";

	if (3 != argc) {
		printf(usage, argv[0]);
		return 1;
	}

	if (!strcmp("-b", argv[1])) {
		if (!is_base36(argv[2])) {
			printf("Not a base36 number [%s]\n", argv[2]);
		}
		else {
			base362ulong(&l, argv[2]);
			printf("%lu\n", l);
		}
	}
	else if (!strcmp("-n", argv[1])) {
		l = strtoul(argv[2], NULL, 10);
		ulong2base36(b36, sizeof(b36), l);
		printf("%s\n", b36);
	}
	else if (!strcmp("-t", argv[1])) {
		if (!strcmp("1", argv[2]))
			test1();
		else
			printf("no test number [%s]\n", argv[2]);
	}
	else {
		printf(usage, argv[0]);
		return 1;
	}

	return 0;
}

