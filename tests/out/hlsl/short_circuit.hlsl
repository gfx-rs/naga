
bool a()
{
    return true;
}

bool b()
{
    return true;
}

bool c()
{
    return true;
}

bool d()
{
    return true;
}

[numthreads(1, 1, 1)]
void main()
{
    bool local = (bool)0;
    bool local_1 = (bool)0;
    bool local_2 = (bool)0;
    bool local_3 = (bool)0;
    bool local_4 = (bool)0;
    bool local_5 = (bool)0;
    bool local_6 = (bool)0;

    const bool _e0 = a();
    if (_e0) {
        local = true;
    } else {
        const bool _e1 = b();
        local = _e1;
    }
    bool _expr4 = local;
    if (_expr4) {
        local_1 = true;
    } else {
        const bool _e5 = c();
        local_1 = _e5;
    }
    bool unnamed = local_1;
    const bool _e9 = a();
    if (_e9) {
        const bool _e10 = b();
        local_2 = _e10;
    } else {
        local_2 = false;
    }
    bool _expr13 = local_2;
    if (_expr13) {
        const bool _e14 = c();
        local_3 = _e14;
    } else {
        local_3 = false;
    }
    bool unnamed_1 = local_3;
    const bool _e18 = a();
    if (_e18) {
        local_4 = true;
    } else {
        const bool _e19 = b();
        local_4 = _e19;
    }
    bool _expr22 = local_4;
    if (_expr22) {
        const bool _e23 = c();
        if (_e23) {
            local_5 = true;
        } else {
            const bool _e24 = d();
            local_5 = _e24;
        }
        bool _expr27 = local_5;
        local_6 = _expr27;
    } else {
        local_6 = false;
    }
    bool unnamed_2 = local_6;
}
