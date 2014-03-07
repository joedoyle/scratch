
namespace Bfc { namespace FastPathFill2 {
int f2(int param);
// {
//     return param;
// }

struct T
{
    void f5();
};

}}



namespace Bfc
{
namespace FastPathFill { namespace f3 {

int f(int s);

}}


// namespace FastPathFill2
// {

// void f2()
// {
// }

// }

}


int Bfc::FastPathFill::f3::f(int s)
{
    Bfc::FastPathFill2::T *p;
    p->f5();
    Bfc::FastPathFill2::f2(s);
    return s;
}
