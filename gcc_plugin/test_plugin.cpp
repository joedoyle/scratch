
namespace Bfc { namespace FastPathFill2 {
int f2(int);
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
    s = Bfc::FastPathFill2::f2(s);
    return s;
}
