/*

    Introspective, a Reflection Library for C++, limited in scope by the C++20 revision.
    Copyright (C) 2021  Josip Palavra

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

*/

#include <utility>
#include <algorithm>
#include <type_traits>

namespace introspective
{

struct ArbitraryDetection {

    template <typename U, typename T>
    using TypePredicate = std::enable_if_t<
        std::is_invocable_v<U, T>,
        std::invoke_result_t<U, T>>;

    template <typename T, typename... Args, typename U> constexpr static auto GetPtr(U f) ->
        std::enable_if_t<
            std::is_pointer_v<
                TypePredicate<U, T*>>
            || std::is_member_pointer_v<
                TypePredicate<U, T*>>,
            TypePredicate<U, T*>>
    {
        return f(static_cast<T*>(nullptr));
    }

    template <typename T, typename... Args, typename U> constexpr static auto GetPtr(U f) ->
        std::enable_if_t<
            std::is_invocable_v<
                TypePredicate<U, T*>,
                Args...>
            && not std::is_pointer_v<
                TypePredicate<U, T*>>
            && not std::is_member_pointer_v<
                TypePredicate<U, T*>>,
            std::invoke_result_t<TypePredicate<U, T*>, Args...>(*)(Args...)>
    {
        return static_cast<
            std::invoke_result_t<TypePredicate<U, T*>, Args...>(*)(Args...)
        >(f(static_cast<T*>(nullptr)));
    }

    template <typename, typename..., typename = void> constexpr static auto GetPtr(...) ->
        std::nullptr_t
    { return nullptr; }

    template <typename T, typename... Args, typename U> constexpr static auto CheckInstantiable(U f) ->
        std::enable_if_t<
            not std::is_null_pointer_v<
                decltype(ArbitraryDetection::GetPtr<T, Args...>(f))
            >,
            std::true_type
        >
    { return std::true_type(); }

    template <typename T, typename... Args, typename U> constexpr static auto CheckInstantiable(U f) ->
        std::enable_if_t<
            std::is_null_pointer_v<decltype(ArbitraryDetection::GetPtr<T, Args...>(f))>,
            std::false_type>
    { return std::false_type(); }

    template <typename T, typename... Args, typename U> constexpr static auto ConservedInstance(U f)
    {
        return []() { return ArbitraryDetection::GetPtr<T, Args...>(U{}); };
    }

    template <typename T, typename V, typename... Args, typename U> constexpr static auto GetInstance(U _1) ->
        std::enable_if_t<
            std::is_same_v<decltype(ArbitraryDetection::GetPtr<T, Args...>(_1)), V>
            || (std::is_convertible_v<decltype(ArbitraryDetection::GetPtr<T, Args...>(_1)), V>
                && not std::is_null_pointer_v<decltype(ArbitraryDetection::GetPtr<T, Args...>(_1))>),
            V>
    {

        return static_cast<V>(ArbitraryDetection::GetPtr<T>(_1));
    }

    template <typename T, typename V, typename... Args, typename U> constexpr static auto GetInstance(U _1) ->
        std::enable_if_t<
            std::is_convertible_v<
                std::invoke_result_t<decltype(ArbitraryDetection::GetPtr<T, Args...>(_1)), Args...>,
                std::invoke_result_t<V, Args...>>,
            V>
    {
        // Lambda is necessary to cast the return type of the inner lambda in _1 to the desired type,
        // while making the compiler happy about the fact that we are not asking it to convert
        // function pointer types into one another, which has its own quirks.
        return [](Args... args) -> std::invoke_result_t<V, Args...>
        {
            // This expression here is one reason this template file depends on C++20:
            // The C++20 revision allows lambda types to be default-constructible, provided that they do not
            // capture anything from its environment, neither by reference nor by value.
            // In prior versions of C++, lambda types were not default-constructible (not even explicitly
            // constructible by the user except writing down its lambda literal).
            // Note that, when compiled with 'clang++ -std=c++17' using Apple's clang compiler,
            // this expression compiles regardless.
            return static_cast<std::invoke_result_t<V, Args...>>(ArbitraryDetection::GetPtr<T, Args...>(U{})(args...));
        };
    }

    template <typename, typename V, typename...> constexpr static auto GetInstance(...) ->
        std::enable_if_t<
            std::is_pointer_v<V>
            || std::is_member_pointer_v<V>,
            std::nullptr_t>
    {
        return nullptr;
    }
};

template <typename T, typename... Args, std::enable_if_t<std::is_constructible_v<T, Args...>, bool> = true>
auto GenericConstructor(Args... args)
{
    return new T{std::move(args)...};
}

#define SemanticsLocalType(select, ...) ([](auto type) -> \
        decltype(&GenericConstructor< \
            typename std::remove_pointer_t<decltype(type)>::select \
            ,##__VA_ARGS__ \
        >) \
    { \
        return &GenericConstructor<typename std::remove_pointer_t<decltype(type)>::select ,##__VA_ARGS__ >; \
    })

#define SemanticsMember(select) ([](auto arg) -> decltype(&std::remove_pointer_t<decltype(arg)>::select) \
    { return &std::remove_pointer_t<decltype(arg)>::select; })

// Returns a constant bool if given type contains some named member variable or member function.
// Works with template types and arguments, too.
// Varargs are the parameter types of the function, if the member in question happens to be a function. For functions
// with arity 0 one need not provide any varargs.
#define HasMember(InType, member, ...) (ArbitraryDetection::CheckInstantiable<InType, ##__VA_ARGS__>(SemanticsMember(member)))

// Returns stateless lambda yielding a pointer to given member of given type, or nullptr if no such member exists.
// Varargs are the parameter types of the function, if the member in question happens to be a function. For functions
// with arity 0 one need not provide any varargs.
#define ConservedMember(InType, member, ...) (ArbitraryDetection::ConservedInstance<InType, ##__VA_ARGS__>(SemanticsMember(member)))

// Returns a stateless function acting as middleman between the caller and the constructor of a local type 'Inner'
// inside type 'Outer'. The local type needs to inherit from a known type 'AsType', which - upon invocation of the
// returned lambda - will be converted to the known type.
// The new object is placed on the heap with operator new.
// Varargs are the parameter types of the constructor in question.
#define GetLocalTypeConstructor(Outer, Inner, AsType, ...) (ArbitraryDetection::GetInstance<Outer, std::add_pointer_t<std::add_pointer_t<AsType>(__VA_ARGS__)> ,##__VA_ARGS__ >(SemanticsLocalType(Inner ,##__VA_ARGS__)))

// Returns a pointer to a static member variable of some given type or a function pointer to a static member
// function of some given type, or nullptr if no such member exists.
// The member needs to be convertible to the given type, or - in the case of a function - the argument and
// the return types must match insofar as to be convertible to each respective other.
#define GetStaticMember(InType, member, MemberType, ...) (ArbitraryDetection::GetInstance<InType, std::add_pointer_t<MemberType>>(SemanticsMember(member)))

// Same as macro GetStaticMember, except that it enforces const on the given type.
#define GetStaticConstant(InType, member, MemberType) (ArbitraryDetection::GetInstance<InType, std::add_pointer_t<std::add_const_t<MemberType>>>(SemanticsMember(member)))

// Returns a pointer to member to a member variable.
// This one is a little tricky; play around with the types a little bit. I have not seen
// a lot of pointer-to-member types in the wild, but it's there for those who need it.
// Some advice: take note and observe how pointer-to-member types interact with classes from the
// standard library - say, std::function.
#define GetObjectMember(InType, member, MemberType) (ArbitraryDetection::GetInstance<InType, MemberType InType::*>(SemanticsMember(member)))

struct CompileTimeString {
    char const * const str;
    const std::size_t s;
    template <std::size_t n>
    consteval CompileTimeString(const char (&a)[n]) : str(a), s(n) {}
    consteval CompileTimeString(char* const a, std::size_t n) : str(a), s(n) {}

    consteval char operator[](std::size_t n) const
    {
        return n < s ? str[n] : '\0';
    }
    consteval std::size_t size() const { return s; }
};

// Records a string of characters as a type.
// Cannot accept string literals as template argument, neither directly
// nor indirectly through functions.
//
// Name is shorthand for 'Compile Time String'
template <char... cs> struct ctstr
{
    constexpr static inline char String[sizeof...(cs)] = {cs...};
    constexpr static inline std::size_t Length = sizeof...(cs);
    consteval ctstr() = default;
};

// Maaaaybe file a bug report for this in g++, this contraption caused
// the compiler to crash.
// The tcstr_s template and its specializations are functionally equivalent
// to the consteval function tcstr() defined just below.
template <CompileTimeString str, int i, typename = void, char... coll>
struct tcstr_s
{
    using type = ctstr<str[i], coll...>;
};

template <CompileTimeString str, int i, char... coll>
struct tcstr_s<str, i, std::enable_if_t<std::less(i, 0)>, coll...>
{
    using type = typename tcstr_s<str, str.size() - 1, void>::type;
};

template <CompileTimeString str, int i, char... coll>
struct tcstr_s<str, i, std::enable_if_t<std::greater(i, 0)>, coll...>
{
    using type = typename tcstr_s<str, i - 1, void, str[i], coll...>::type;
};

template <CompileTimeString str>
using tcstr_t = typename tcstr_s<str, -1>::type;

// Name is shorthand for 'to compiled string'.
template <CompileTimeString str, int i = -1, char... coll>
consteval auto tcstr()
{
    if constexpr(str.size() == 0) return ctstr<>();
    else if constexpr(i < 0) return tcstr<str, str.size() - 1>();
    else if constexpr(i > 0) return tcstr<str, i - 1, str[i], coll...>();
    else return ctstr<str[i], coll...>();
}

template<unsigned... digits>
constexpr char IntToChars[] = {('0' + digits)..., '\0'};

template <unsigned remaining, unsigned... digits>
consteval auto& CompiledIntString()
{
    if constexpr(remaining == 0) return IntToChars<digits...>;
    else return CompiledIntString<remaining / 10, remaining % 10, digits...>();
}

template <std::size_t len> consteval CompileTimeString Intern(const char (&str)[len])
{
    return CompileTimeString(str);
}

// Settings pertaining to looking up the reflective members of the type 'Where'.
// Second template parameter leaves space for private specializations based on the first argument, if needed.
template <typename Where, typename AlwaysVoid = void>
struct IntrospectiveSettings
{
    // How far the compiler will look for another reflective member in each direction,
    // typically measured in lines.
    // If you happen to experience a template recursion stack overflow at compile time,
    // try and reduce this number.
    // Note that - when measured in lines - this means that you might have to put the reflective
    // declarations inside the reflective type closer to each other, so that the recursion
    // can recognize all reflective members.
    constexpr static inline int MaximumSeekReach = 35;
    // How many reflective members the type wants to be able to enumerate.
    // Members with indices equal to or exceeding this limit might not be enumerated
    // at all.
    constexpr static inline std::size_t MemberLimit = 60;
};

// Implementation of template algorithms. Not part of public interface.
// Short for 'template implementation' and misslepping of 'template'.
namespace timpl
{

template <template <typename...> typename TypenamesTemplate, typename Default, typename AlwaysVoid = void, typename... Args>
struct IsTemplateInstantiable_f: std::false_type
{
    using Type = Default;
};

template <template <typename...> typename TypenamesTemplate, typename Default, typename... Args>
struct IsTemplateInstantiable_f<TypenamesTemplate, Default, std::void_t<TypenamesTemplate<Args...>>, Args...>: std::true_type
{
    using Type = TypenamesTemplate<Args...>;
};

template <auto x>
consteval auto CtAbs()
{
    if constexpr(x >= 0) { return x; }
    else { return -x; }
}
struct ChainEnd { using Type = void; };

template <template <int, int> typename Count, int i, typename _Default, typename = void>
struct IsCountInstantiable_f: std::false_type { using Type = _Default; };

template <template <int, int> typename Count, int i, typename _Default>
struct IsCountInstantiable_f<Count, i, _Default, std::void_t<typename Count<i, 0>::Head>>
{
    using Type = Count<i, 0>;
};

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, int loc, typename _Default, typename = void>
struct IsChainInstantiable_f: std::false_type {
    using Type = typename _Default::Type;
};

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, typename Default, int loc>
struct IsChainInstantiable_f<Chain, TypeDisambiguate, loc, Default, std::void_t<typename Chain<TypeDisambiguate, loc, 0>::TailAbove>>:
    std::true_type
{
    using Type = Chain<TypeDisambiguate, loc, 0>;
};

template <template <template <typename, int, int> typename, typename, int, int, int> typename Recurseek, 
          template <typename, int, int> typename Chain, 
          typename TypeDisambiguate, int seek, int inc, int from>
struct DelayedRecursionTemplate
{
    using Type = typename Recurseek<Chain, TypeDisambiguate, seek, inc, from>::Type;
};

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, int seek, int inc, int from>
struct RecursiveSeek_impl
{
    using Type = typename std::conditional_t<std::less<int>{}(CtAbs<seek - from>(), IsTemplateInstantiable_f<IntrospectiveSettings, void, void, TypeDisambiguate>::Type::MaximumSeekReach),
          IsChainInstantiable_f<Chain, TypeDisambiguate, seek, DelayedRecursionTemplate<RecursiveSeek_impl, Chain, 
          TypeDisambiguate, seek + inc, inc, from>>,
                                  ChainEnd>::Type;
};

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, int seek, int inc, int from, typename = void>
struct RecursiveSeek_f: std::false_type
{
    using Type = void;
};

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, int seek, int inc, int from>
struct RecursiveSeek_f<Chain, TypeDisambiguate, seek, inc, from, 
    std::enable_if_t<not std::is_void_v<typename RecursiveSeek_impl<Chain, TypeDisambiguate, seek, inc, from>::Type>>>:
    std::true_type
{
    using Type = typename RecursiveSeek_impl<Chain, TypeDisambiguate, seek, inc, from>::Type;
};

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, int seek, int inc, int from>
using RecursiveSeek_t = typename RecursiveSeek_f<Chain, TypeDisambiguate, seek, inc, from>::Type;

template <template <typename, int, int> typename Chain, typename TypeDisambiguate, int seek, int inc, int from>
constexpr inline bool IsRecursionSuccessful = RecursiveSeek_f<Chain, TypeDisambiguate, seek, inc, from>::value;

template <typename Check, typename = void>
struct MilesBelowCheck { constexpr static int MilesBelow = 0; };

template <typename Check>
struct MilesBelowCheck<Check, std::enable_if_t<not std::is_void_v<typename Check::TailAbove>>>
{
    enum { MilesBelow = 1 + MilesBelowCheck<typename Check::TailAbove>::MilesBelow };
};

}

// Enables the inheriting class to allow introspection into its members that have been
// declared with introspection macros. Records their types and names, does not dis-
// criminiate between member variables or functions.
// The template parameter should be the inheriting class itself. This struct does not add
// any virtual functions or instance member variables.
template <typename _Self>
struct Introspective
{
    // Alias for macros, for not having to name the introspected-upon type directly in them.
    // Requires having to name it as a template parameter to this type though.
    using IntrospectiveSelf = _Self;

    constexpr Introspective() = default;

    // Arguments to these functions are supplied by template instances to ensure evaluation at compile time.
    template <std::size_t index, std::size_t memberLimit = IntrospectiveSettings<IntrospectiveSelf, void>::MemberLimit>
    consteval static auto GetMemberByIndex()
    {
        return Introspective<_Self>::FindIntrospect<[](auto introspectiveMeta)
            -> bool { return index == decltype(introspectiveMeta)::MilesBelow; }>(
            std::make_index_sequence<memberLimit>{});
    }

    // Predicate is a non-type template argument with a constexpr operator() member function yielding bool.
    template <auto predicate, std::size_t memberLimit = IntrospectiveSettings<IntrospectiveSelf, void>::MemberLimit>
    consteval static auto GetMemberByPredicate()
    {
        return Introspective<_Self>::FindIntrospect<predicate>(std::make_index_sequence<memberLimit>{});
    }

    // Name type parameter has to be a instance of template 'ctstr', with appropriate stringization.
    template <auto name, std::size_t memberLimit = IntrospectiveSettings<IntrospectiveSelf, void>::MemberLimit>
    consteval static auto GetMemberByName()
    {
        return Introspective<_Self>::FindIntrospect<[](auto meta) -> bool
            { if constexpr(name.Length != decltype(meta)::Head::Length) { return false; }
              else
              {
                  for(std::size_t i = 0; i < name.Length; ++i)
                  {
                      if(name.String[i] != decltype(meta)::Head::String[i]) { return false; }
                  }
                  return true;
              }
            }>(std::make_index_sequence<memberLimit>{});
    }

    template <std::size_t memberLimit = IntrospectiveSettings<IntrospectiveSelf, void>::MemberLimit>
    consteval static std::size_t GetReflectiveMemberCount()
    {
        // A lambda literal with its closure equal to itself is as of C++20 of a structural type and may decay
        // into a bare function pointer at compile time.
        return Introspective<_Self>::First<[](auto) -> bool { return false; }>(std::make_index_sequence<memberLimit>{});
    }

// Consider the type names defined here as part of the private interface, subject to change.
// This macro was written to pollute the surrounding type namespace as little as possible with each introspection,
// occupying only two names in the entire surrounding type namespace.
//
// Second macro argument enables usage of other stateful compile time counters, such as the non-standard __COUNTER__ macro
// or other hacks you might find. This header restricts itself to using the __LINE__ macro for portability.
//
// Third argument plays a key role when reflecting over template members: it controls whether the member
// only accepts type parameters or non-type parameters; the only two possible values for the flavor
// are 'typename' for type parameters and 'auto' for non-type parameters respectively.
#define PlainIntrospectiveRawBoilerplate(name, StatefulCompiledCounter, templateFlavor, ...) \
    template <typename DistinctIntrospectiveSelf, int loc, int dummy> \
    struct IntrospectivesChain; \
    template <int dummy> \
    struct IntrospectivesChain<IntrospectiveSelf, StatefulCompiledCounter, dummy> \
    { \
        using TailAbove = timpl::RecursiveSeek_t<IntrospectiveSelf::IntrospectivesChain, IntrospectiveSelf, StatefulCompiledCounter - 1, -1, StatefulCompiledCounter>; \
        constexpr static int MilesBelow = timpl::MilesBelowCheck<IntrospectivesChain<IntrospectiveSelf, StatefulCompiledCounter, dummy>>::MilesBelow; \
        constexpr static bool IsFirstIntrospect = MilesBelow == 0; \
        constexpr static char Name[] = #name; \
        using Head = decltype(tcstr<Intern(Name)>()); \
    }; \
    template <int i, int specializationDelay> struct IntrospectivesCount; \
    template <int specializationDelay> struct IntrospectivesCount<IntrospectivesChain<IntrospectiveSelf, StatefulCompiledCounter, 0>::MilesBelow, specializationDelay>: \
        IntrospectivesChain<IntrospectiveSelf, StatefulCompiledCounter, 0> \
    { \
        friend struct Introspective<IntrospectiveSelf>; \
        struct ObtainPtr { __VA_OPT__(template <templateFlavor... targs>) consteval static auto Stencilled() { \
            return &IntrospectiveSelf::name __VA_OPT__(<targs...>); }; consteval ObtainPtr() = default; }; \
        consteval static auto CollectMember() { return ObtainPtr{}; } \
    };

// Enables any class member thusly named to be reflected upon, no matter the type of the member.
#define IntrospectiveBoilerplate(name, ...) PlainIntrospectiveRawBoilerplate(name, __LINE__, auto, __VA_ARGS__)

#define FlavouredIntrospectiveBoilerplate(name, templateFlavour, ...) PlainIntrospectiveRawBoilerplate(name, __LINE__)

// Shorthand for declaring a reflective object or instance member function.
#define RuntimeIntrospectiveObjectFn(name, ...) IntrospectiveBoilerplate(name, __VA_ARGS__); __VA_OPT__(template <) __VA_ARGS__ __VA_OPT__(>) auto name

// Shorthand for declaring a reflective static member function.
#define RuntimeIntrospectiveStaticFn(name, ...) IntrospectiveBoilerplate(name, __VA_ARGS__); __VA_OPT__(template <) __VA_ARGS__ __VA_OPT__(>) static auto name

// Shorthand for declaring a member of any kind, except types.
#define TypedIntrospectiveMember(types, name, ...) IntrospectiveBoilerplate(name, __VA_ARGS__); __VA_OPT__(template <) __VA_ARGS__ __VA_OPT__(>) types name

// Shorthand for declaring a constexpr constant.
#define ConstexprIntrospectiveValue(name, ...) IntrospectiveBoilerplate(name, __VA_ARGS__); \
    __VA_OPT__(template <) __VA_ARGS__ __VA_OPT__(>) constexpr static inline auto name

// Shorthand for declaring a constinit constant.
#define ConstinitIntrospectiveValue(name, ...) IntrospectiveBoilerplate(name, __VA_ARGS__); \
    __VA_OPT__(template <) __VA_ARGS__ __VA_OPT__(>) constinit static inline auto name

// Shorthand for declaring a reflective member variable of any kind.
#define RuntimeIntrospectiveValue(name) name; IntrospectiveBoilerplate(name);

private:

    template <std::size_t i, auto predicate>
    struct FoldGenericFind
    {
        constexpr static inline auto Index = i;

        using This   = FoldGenericFind<i, predicate>;
        using Result = typename timpl::IsCountInstantiable_f<IntrospectiveSelf::template IntrospectivesCount, i, std::nullptr_t>::Type;

        // Used in place of ObtainPtr when no such member can be found
        struct MemberFail
        {
            consteval MemberFail() = default;
            // This could potentially come back to bite me:
            // When reflecting upon a non-existent template member, if
            // any of the template's arguments were type template arguments,
            // this lambda literal would fail to compile, and the compiler
            // could not tell what the actual problem is: that there was no
            // such template member in the first place.
            // < should a template parameter pack be here? >
            // I'll put it in, we'll see how this shakes out
            template <auto...> consteval static std::nullptr_t Stencilled() { return nullptr; }
        };

        template <std::size_t j>
        consteval auto operator ,(const FoldGenericFind<j, predicate>& next)
        {
            if constexpr(i < j && std::is_null_pointer_v<typename This::Result>) { return *this; }
            else if constexpr(i < j && predicate(typename This::Result{})) { return *this; }
            else { return next; }
        }
        consteval auto GetMember()
        {
            if constexpr(std::is_null_pointer_v<typename This::Result>) { return MemberFail{}; }
            else { return Result::CollectMember(); }
        }
    };

    template <auto predicate, std::size_t... haystack>
    consteval static auto FindIntrospect(std::index_sequence<haystack...>)
    {
        return (FoldGenericFind<haystack, predicate>{}, ...).GetMember();
    }

    template <auto predicate, std::size_t... haystack>
    consteval static std::size_t First(std::index_sequence<haystack...>)
    {
        return decltype((FoldGenericFind<haystack, predicate>{}, ...))::Index;
    }

};

}

