#version 460 core

bool a() { return true; }
bool b() { return true; }
bool c() { return true; }
bool d() { return true; }

void main() {
    bool out1 = a() || b() || c();
    bool out2 = a() && b() && c();
    bool out3 = (a() || b()) && (c() || d());
}
