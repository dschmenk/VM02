#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

unsigned char buf[256];

int main(int argc, char **argv)
{
    int fd, offset;

    if (argc < 2)
    {
        printf("Usage: %s <binfile>\n", argv[0]);
        return (1);
    }
	if ((fd = open(argv[1], O_RDONLY, 0)) > 0)
	{
		offset = 0;
		while (read(fd, buf, 1) == 1)
		{
			if (offset++ & 0x0F)
				printf(",$%02X", buf[0]);
			else
				printf("\n\t.BYTE\t$%02X", buf[0]);
		}
		printf("\n");
	}
	close(fd);
}