
void fb1_(inout bool cond)
{
    bool loop_init = true;
    while(true) {
        if (!loop_init) {
            bool _expr6 = cond;
            if (!(_expr6)) {
                break;
            }
        }
        loop_init = false;
        continue;
    }
    return;
}

void main_1()
{
    bool param = (bool)0;

    param = false;
    fb1_(param);
    return;
}

void main()
{
    main_1();
}
