Some quotes from StackOverflow regarding reflection in C++:

*"Inspection by iterating over members of a type, enumerating its methods and so on. This is not possible with C++."*

*"You can't iterate over the member functions of a class for example."*

You can. Sort of.

Well, there's obviously more to it than that.

# Compile-time reflection

Introspective is a header file that makes good use of template capabilities and allows
new classes to selectively open up some or all of their members to reflection, regardless of
whether the inspected member is a constant, a static variable or a instance member function. It records
selected members and their declared names in a list, infers their types and provides the caller with the
addresses of recorded members, when needed. All of this happens during compilation, and a
decent compiler should find it easy to optimize all intermediate functions and generated names away.

Let's take a tour.

```c++
#include <string>
#include <introspective.h>

using namespace introspective;

struct Reflective: Introspective<Reflective>
{

    // Declaring and defining functions with the supplied macros might seem
    // a little daunting at first.
    RuntimeIntrospectiveStaticFn(add) (int x, int y) -> int { return x + y; }
    
    // It does not look a lot like C++, I agree.
    ConstexprIntrospectiveValue(Pie) = 3.14;
    
    // What the macro needs is the name of the declaration, nothing else.
    // Type inference can do the rest for us.
    RuntimeIntrospectiveStaticFn(sub) (double x, double y) -> double { return x - y; }
    
    // Declare it, but define it somewhere else. It can wait.
    RuntimeIntrospectiveStaticFn(div) (double x, double y) -> double;
    
    // Say we had a object variable that we do not want recorded.
    // Just leave out the macro then.
    double value;
    
    // Instance member functions are just another declaration in the
    // eyes of reflection. Return types are deduced from context.
    RuntimeIntrospectiveObjectFn(mul) (double y) { return value * y; }
    
    // We might record a instance variable just as easily.
    // This style is certainly not ideal, since it looks like declaring
    // a member function. You may remap the macros of course if you
    // feel like it.
    std::string RuntimeIntrospectiveValue(strung);
    
};

// The definition of the function already declared requires
// no reflection magic; these two are separate.
double Reflective::div(double x, double y) { return x / y; }

```

Although it tries to offload the burden of reflection to template metaprogramming as much as possible,
the fact remains that templates are not known for being concise. That is why Introspective employs macros
to dress the reflected members like some version of C++ one could reason with just by looking at it,
while staying faithful to C++ idioms.

How bad can the interface to this be, one might wonder.

```c++
#include <iostream>

int main()
{
    // Get the address of the first member that has been indexed
    // in the definition of the Reflective struct. It is a
    // static function taking two integers.
    // No casting of any kind necessary.
    int (* addAddress)(int, int) = Reflective::GetMemberByIndex<0>().Stencilled();

    // Prints 17.
    std::cout << addAddress(9, 8) << std::endl;

    // Just underneath the definition of the add function there is pie.
    const double* pieAddress = Reflective::GetMemberByIndex<1>().Stencilled();

    // Get the address of div, the function split into declaration and definition.
    double (* divAddress)(double, double) = Reflective::GetMemberByIndex<3>().Stencilled();
    std::cout << divAddress(1.0, *pieAddress) << std::endl;
}
```

Handles object member functions and variables just as well.

```c++
int main()
{
    Reflective t{ .value = 2.71, .strung = "Lorem ipsum" };

    std::string Reflective::* strungAddress =
        t.GetMemberByIndex<Reflective::GetReflectiveMemberCount() - 1>()
         .Stencilled();
    double (Reflective::* mulAddress)(double) = t.GetMemberByIndex<4>().Stencilled();

    std::cout << (t.*mulAddress)(Reflective::Pie) << std::endl;
    std::cout << t.*strungAddress << std::endl;
}
```

Gets members even by name, although admittedly with much clunkier syntax than I could
have ever anticipated. If someone has a way to make this easier without
macros, please feel free to contribute!

```c++
int main()
{
    using namespace introspective;

    // String literals may not under any circumstances be used as arguments
    // to templates, directly or indirectly. One can get around this by
    // storing a constexpr static character array somewhere else.
    constexpr static char queryName[] = "sub";

    auto subRef = Reflective::GetMemberByName<tcstr<Intern(queryName)>()>()
                             .Stencilled();
    std::cout << subRef(5, 8) << std::endl;
}
```

Reflective template members are also supported, under the condition that all
template parameters are declared as non-type template parameters.
Template parameters have space in the back as varargs to the macro invocation.
The syntax might very well also support variadic non-type template arguments,
but this has not been tested yet.

```c++
struct Templatte: Introspective<Templatte>
{
    RuntimeIntrospectiveStaticFn(Lattemp, int x, int y, int z) (double a)
    {
        return x - y + z * a;
    }
};

int main()
{
    // Observe where the template arguments went.
    double (* generic)(double) = Templatte::GetMemberByIndex<0>()
                                           .Stencilled<5, 6, 7>();
    std::cout << generic(3.14) << std::endl;
}
```

As a bonus, the header also provides some short macros for
detecting a specific member in an unspecified generic class.
I'll mention them here briefly.

* `HasMember(InType, member, ...) -> bool`. Indicates true if member `member`
   can be found in type `InType`, regardless of whether the member is a function
   or a variable. Actually returns either `std::true_type` or `std::false_type`,
   but these are implicitly convertible to `bool` in any context. The varargs
   must be filled with matching parameter types if the subject of the search
   is a function with specific parameters.
* `GetStaticMember(InType, member, MemberType) -> MemberType*`. Returns a pointer
   to the static variable `InType::member`, if one such exists, otherwise
   `nullptr`. `MemberType` may also be a function type, as in `int(std::string,
   double)` without pointer notation. Actually returns either a valid value
   of `MemberType*` or a `std::nullptr_t`, depending on the existence of
   the member.
* `GetStaticConstant(InType, member, MemberType) -> const MemberType*`. Same as
   `GetStaticMember`, but enforces const-ness of the member. If the member
   is not const-qualified, returns `nullptr`.
* `GetObjectMember(InType, member, MemberType) -> MemberType InType::*`. Same as
  the static version, but returns a pointer-to-member. Does not support object
  member functions at the moment.

These macros may be used inside a function and may be treated as such, they only
hide two lines of template boilerplate code. As a consequence of template
metaprogramming, these macros also support template type parameters as their
argument.

## Requirements

Fairly thin; the header file only depends on the standard library. However,
it is written for C++20 and uses some features that have been introduced
with that or the previous revision:

* `__VA_OPT__`
* Structural types as non-type template parameters
* Lambda literals in unevaluated contexts
* Default-constructible lambda types where their closure is equal to
  itself.
* `consteval` for making sure none of the reflection algorithms leak
  over into the runtime.
* Fold expressions for variadic template arguments (might have been
  already introduced with C++17, mentioned for the sake of
  completeness)

GCC happens to have implemented all of these.
Clang 12 is lacking some C++20 features and cannot compile the header
at the time of this writing. Other compilers may or may not work.

## Motivation

Although this header does not provide universal, but rather selective
reflection, it is still helping me personally to reduce code duplication. One might take
the task of registering Lua functions from C++ to the Lua runtime as
an example, where a lot of functions might need to be fit to
the C calling scheme: arguments need to be extracted from the Lua stack,
the actual function needs to be called with the correctly typed arguments
in the right order and the return value needs to be made available to Lua
again. By throwing in the semantics of C++ object member functions to
the mix, one stands in front of a dauting procedure of translating
Lua stack code to C++ and back again, if support for communication between
Lua objects and C++ objects is to be realized. By declaring member functions
as reflectable, the compiler is able to generate a compile-time list of all
functions that one wants the Lua runtime to have direct access to, with 
function types inferred by the compiler. One only needs to apply some template
transformations to each function in that list to obtain freshly ~~baked~~
generated static Lua functions that can be directly fed to the Lua runtime,
without having to go through the hassle of listing a name for the Lua function,
referencing C++ functions and translating Lua types to C++ types manually.

Until C++ implements some real universal reflection, this header ought to do it
for the time being.

Any feedback or contribution is greatly appreciated!

