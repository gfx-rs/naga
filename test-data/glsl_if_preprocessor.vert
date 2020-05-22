#version 460 core

#define TEST 3

#if TEST == 0
#error 0
#elif TEST == 1
#error 1
#elif TEST == 2
#error 2
#else
#error You shouldn't do that
#endif
