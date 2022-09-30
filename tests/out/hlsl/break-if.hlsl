
void breakIfEmpty()
{
    bool loop_init = true;
    while(true) {
        if (!loop_init) {
            if (true) {
                break;
            }
        }
        loop_init = false;
    }
    return;
}

void breakIfEmptyBody(bool a)
{
    bool b = (bool)0;
    bool c = (bool)0;

    bool _expr9 = c;
    bool unnamed = (a == _expr9);
    bool loop_init_1 = true;
    while(true) {
        if (!loop_init_1) {
            b = a;
            bool _expr4 = b;
            bool c_1 = (a != _expr4);
            c = c_1;
            if (unnamed) {
                break;
            }
        }
        loop_init_1 = false;
    }
    return;
}

void breakIf(bool a_1)
{
    bool d = (bool)0;
    bool e = (bool)0;

    bool _expr9 = e;
    bool unnamed_1 = (a_1 == _expr9);
    bool loop_init_2 = true;
    while(true) {
        if (!loop_init_2) {
            if (unnamed_1) {
                break;
            }
        }
        loop_init_2 = false;
        d = a_1;
        bool _expr4 = d;
        bool e_1 = (a_1 != _expr4);
        e = e_1;
    }
    return;
}

[numthreads(1, 1, 1)]
void main()
{
    return;
}
