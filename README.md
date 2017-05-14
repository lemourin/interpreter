lang language
=============

This language's syntax is based on `C`. Things which make this language  
a little bit different are:
- no traditional function definition (`void func_name(int arg1, ...)`),  
  functions are treated like any other values, can be created by using  
  the `C++11` lambda syntax (`[](int arg1, int arg2) -> void`)
- pseudo-polymorphic types: `array[type]`,  
  `function[type1, type2 -> return_type]`, `tuple[t1, t2, ...]`
- no regular `C` for: `for (int i = 0; i < n; i++)`, for-each instead:  
  `for i in container`
- `auto` keyword which can contain variable of any type
- no `main` function, code is executed line by line
- `print expr` - prints the value of `expr` to `stdout`

To build the interpreter, type `make`.
