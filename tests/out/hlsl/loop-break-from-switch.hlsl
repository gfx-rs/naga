
static int global = (int)0;
static int global_1 = (int)0;

struct FragmentInput_main {
    nointerpolation int member : LOC0;
};

void function()
{
    int _expr8 = global;
    while(true) {
        switch(_expr8) {
            case 0: {
                global_1 = 0;
                break;
            }
            default: {
                break;
            }
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
