#include <stdio.h>
#include <stdlib.h>

int main (int argc, char * argv[])
{
	int i=0, c, bytes=4, group=4;

	if (argc >= 2)
		bytes = atoi(argv[1]);
	if (argc >= 3)
		group = atoi(argv[2]);

	while ((c = getchar()) != EOF) {
		if (i % bytes == 0) printf("0x");
		printf("%02x", c); i++;
		if (i % bytes == 0) printf(", ");
		if (i % (group * bytes) == 0) printf("\n");
	}
	printf("\n");
	return 0;
}
