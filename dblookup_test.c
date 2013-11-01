// Title
// Description
// by Nicolas Avrutin
// 2013

#include <stdlib.h>
#include <stdio.h>
#include "dblookup.h"

int main(){
    char buf[256];
    db_init(256, buf);
    POSITION pos;
    pos.bm = 320;
    pos.bk = 1073741824;
    pos.wm = 163840;
    pos.wk = 128;
    pos.color = 1;
    int i = dblookup(&pos, 0);
    /* DEBUG */
    fprintf(stderr, "i: %d\n", i);
    /* DEBUG */
    
    return 0;
}
