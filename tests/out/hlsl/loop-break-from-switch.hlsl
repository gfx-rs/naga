
static int global = (int)0;
static int global_1 = (int)0;

struct FragmentInput_main {
    nointerpolation int member : LOC0;
};

void function()
{
    bool local = false;

    int _expr8 = global;
    while(true) {
        switch(_expr8) {
            case 0: {
                global_1 = 0;
                local = true;
                break;
            }
            default: {
                break;
            }
        }
        bool _expr11 = local;
        if (_expr11) {
            break;
        }
        global_1 = -9;
        break;
    }
    return;
}

int main(FragmentInput_main fragmentinput_main) : SV_Target0
{
    int param = fragmentinput_main.member;
    global = param;
    function();
    int _expr3 = global_1;
    return _expr3;
}
