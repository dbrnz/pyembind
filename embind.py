from symbol_table import SymbolTable


class EmbindGenerator(object):
    def __init__(self, symbol_dir, classes):
        self.symbol_table = SymbolTable()
        self.symbol_table.LoadSymbolsFromDirectory(symbol_dir)

        self.classes = classes

        self.known_symbols = ['', # Typeless 'types', e.g. return value of ctor is parsed to an empty string.
                              'void',
                              'bool',
                              'char',
                              'signed char',
                              'unsigned char',
                              'short',
                              'signed short',
                              'unsigned short',
                              'int',
                              'signed int',
                              'unsigned int',
                              'long',
                              'signed long',
                              'unsigned long',
                              'float',
                              'double',
                              'unsigned int',
                              'std::string',
                              'emscripten::val',
                             ]

        self.generated_function_selectors = []

        # Lists the types for which we need to generate type identifier functions for.
        self.require_type_function = []


    def output(self, output):
        self.cpp_out = open('{}.cpp'.format(output), 'w')
        self.js_out = open('{}.js'.format(output), 'w')

        self.cpp_out.write('''#ifdef EMSCRIPTEN

#include <emscripten/bind.h>
using namespace emscripten

#include "embind_prologue.h" # Implement this file and all required code and header files to compile this file here."
''')

        self.js_out.write('function RegisterFunctionSelectors() {\n')

        for c in self.classes:
            self.GenerateCtorFunctions(c)

        # TODO:: if we create a separate bindings file for each, generate a unique name here.
        self.cpp_out.write('EMSCRIPTEN_BINDINGS(bindings) {\n\n')

        for c in self.classes:
            self.WriteForwardDeclaration(c)

        for c in self.classes:
            self.GenerateEmbindFile(c, self.known_symbols)

        self.cpp_out.write("\n}\n\n")
        self.cpp_out.write("#endif\n")
        self.cpp_out.close()

        print("Writing embind_symbols.cpp done.")

        self.GenerateTypeIdAssignments()

        self.js_out.write("}\n")
        self.js_out.write("window['RegisterFunctionSelectors'] = RegisterFunctionSelectors;\n")

        self.GenerateIsOfTypeFunctions()

        self.js_out.close()

        print("Writing embind_symbols.js done.")


    def WriteForwardDeclaration(self, class_name):
        pass
#            string t = "auto " + class_name + "_class = class_<" + class_name + ">(\"" + class_name + "\");\n\n"
#            self.cpp_out.Write(t)


    def FetchFunctionOverloads(self, function):
        functionOverloads = [s for s in function.parent.children
                                   if s.name == function.name and self.IsGoodSymbol(s)]
        functionOverloads.sort(lambda left, right: len(right.parameters) - len(left.parameters))
        return functionOverloads


    def GenerateTypeIdAssignments(self):
        typeIdCounter = 1
        for n, _type in enumerate(self.require_type_function):
            if _type in ["float", "int"]:
                continue
            self.js_out.write('    Module.{0}.prototype.TypeId = {1}; /* Magic automatically generated TypeId number for {0} */\n'
                              .format(_type, n+1))


    def GenerateIsOfTypeFunctions(self):
        self.js_out.write('''function isNumber(value) {
    if ((undefined === value) || (null === value)) {
        return false;
    }
    if (typeof value == 'number') {
        return true;
    }
    return !isNaN(value - 0);
}
''')
        for n, _type in enumerate(self.require_type_function):
            if _type in ['float', 'int']:
                self.js_out.write('function IsOfType_{}(obj) {{ return isNumber(obj); }}\n'.format(_type))
            else:
                self.js_out.write('''/* Magic automatically generated TypeId number for {0} */
function IsOfType_{0}(obj) {{ return obj != undefined && obj != null && obj.TypeId == {1}; }}\n'''.format(_type, n))


    def GenerateFunctionSelector(self, functionOverloads):
        functionOverloads.sort(lambda left, right: len(right.parameters) - len(left.parameters))
        function = functionOverloads[0]
        if (function.parent.name + "::" + function.name) in self.generated_function_selectors:
            return
        self.generated_function_selectors.append(function.parent.name + "::" + function.name)
        if len(function.parameters) == 0:
            return

        # TODO: Add support for REAL ctors.
        isCtor = (function.name == function.parent.name)
        if isCtor:
            prototype = ['{}_ = function('.format(function.name)]
        else:
            prototype = ['{}.prototype.{} = function('.format(function.parent.name, function.name)]
        prototype.append(', '.join(['arg{}'.format(i+1) for i in len(function.parameters)]))
        prototype.append(') {\n')
        self.js_out.write(''.join(prototype))

        thisFunc = functionOverloads[0]
        for i in xrange(len(functionOverloads)):
            nextFunc = functionOverloads[i + 1] if i < (len(functionOverloads) - 1) else None
            self.js_out.write("    ")
            if i: self.js_out.write("else ")
            if nextFunc:
                if len(thisFunc.parameters) != len(nextFunc.parameters):
                    self.js_out.write("if (arg{} != undefined)".format(len(thisFunc.parameters)))
                else:
                    self.js_out.write("if (")
                    for j in xrange(len(thisFunc.parameters)):
                        _type = thisFunc.parameters[j].BasicType()
                        if j: self.js_out.write(" && ")
                        self.js_out.write("IsOfType_{}(arg{})".format(_type, j+1))
                        if _type not in self.require_type_function:
                            self.require_type_function.append(_type)
                    self.js_out.write(")")
            self.js_out.write("\n")
            self.js_out.Write('    {}'.format('return ' if thisFunc.type != 'void' else ''))
            self.js_out.Write('{}.{}'.format('Module' if isCtor else 'this', function.name))
            for p in thisFunc.parameters:
                self.js_out.write("_" + p.BasicType().replace(':', '_').replace('<', '_').replace('>', '_'))
            paramList = ['arg{}'.format(j+1) for j in xrange(len(thisFunc.parameters))]
            self.js_out.write('({});\n'.format(', '.join(paramList)))
            thisFunc = nextFunc

        self.js_out.write('}\n')

    def IsGoodSymbol(s):
        if ("noembind" in s.attributes
         or s.type not in self.known_symbols):
            return False
        for p in s.parameters:
            if p.BasicType() not in self.known_symbols:
                return False
        return True


    def GenerateEmbindFile(class_name):
        if not self.symbol_table.contains(class_name):
            print("Error: Cannot generate bindings for class '{}', XML for that class doesn't exist!".format(class_name))
            return

        code = ['##include <emscripten/bind.h>',
                '#using namespace emscripten;',
                '',
                '##include "embind_prologue.h" # Implement this file and all required code and header files to compile this file here.',
                '',
                '#EMSCRIPTEN_BINDINGS({0}) {'.format(class_name),
                'class_<{0}>("{0}")'.format(class_name)
               ]

        hasCtorExposed = False  # Embind only supports exposing one ctor, so pick the first one.

#            t += class_name + "_class\n"
        s = self.symbol_table.symbol(class_name)
        for f in s.children:
            if f.visibilityLevel != VisibilityLevel.Public:
                continue  # Only public functions and members are exported.

            functionOverloads = self.FetchFunctionOverloads(f)

            good_symbol = "noembind" not in f.attributes # If True, this symbol is exposed. If False, this symbol is not enabled for JS.
            reason = ['' if good_symbol else '(ignoring since [noembind] specified)']

            if f.kind == "function" and not f.name.StartsWith("operator"):
                isCtor = (f.name == class_name)
                if good_symbol and f.type not in self.known_symbols:
                    good_symbol = False
                    reason.append('({} is not known to embind)'.format(f.type))

                hasOverloads = len(functionOverloads) > 1
                targetFunctionName = [f.name] # The JS side name with which this function will be exposed.
                funcPtrType = ['{}({}*)('.format(f.type,
                                                 '' if f.isStatic else '{}::'.format(class_name))]
                paramList = []
                for p in f.parameters:
                    paramList.append(p.type)
                    if good_symbol and p.BasicType() not in self.known_symbols:
                        good_symbol = False
                        reason.append('{} is not known to embind)'.format(p.BasicType()))
                    if hasOverloads:
                        targetFunctionName.append('_{}'.format(p.BasicType().replace(':', '_').replace('<', '_').replace('>', '_')))

                funcPtrType.append('{})'.format(','.join(paramList)))
                if f.isConst: funcPtrType.append(' const')

                # TODO: Remove this line once multiple ctors is supported!
                if (good_symbol and f.name == class_name and hasCtorExposed):
                    good_symbol = False
                    reason = "(Multiple constructors not yet supported by embind!)"

                if not good_symbol:
                    code.append('# /*{}*/'.format(reason))

                if isCtor:
                    code.append('    .constructor<{}>()\n'.format(','.join(parmList)))
                    if good_symbol: hasCtorExposed = True
                else:
                    if f.isStatic: code.append('    .class_function(')
                    else:          code.append('    .function(')
                    code.append('"{}", ({})&{}::{})'.format(targetFunctionName, funcPtrType, class_name, f.name))

                if hasOverloads and good_symbol:
                    self.GenerateFunctionSelector(functionOverloads)

            elif f.kind == "variable" and f.visibilityLevel == VisibilityLevel.Public:
                if f.type not in self.known_symbols:
                    code.append('# /* {} is not known to embind. */'.format(f.type))
                elif f.IsArray():
                    code.append('# /* Exposing array types as fields are not supported by embind. */')
                elif f.isStatic:
                    code.append('# /* Exposing static class variables not yet implemented (are they supported?) */')
                code.append('    .property("{0}", &{1}::{0})'.format(f.name, class_name))


        code.append("    ;")
        code.append("#}")

        self.RegisterCtorFunctions(class_name)

        self.cpp_out.Write('\n'.join(code))


    def GenerateCtorFunctions(self, class_name):
        s = self.symbol_table.symbol(class_name)
        ctors = []
        for f in s.children:
            if f.name == s.name and len(f.parameters): # 0-parameter ctors are created with 'new type();'
                good_ctor = True
                for p in f.parameters:
                    if p.BasicType() not in self.known_symbols:
                        good_ctor = False
                        break
                if good_ctor:
                    ctors.append(f)
                    self.cpp_out.write('{} {}'.format(class_name,
                                                      '_'.join(class_name + [p.BasicType() for p in f.parameters])))
                    self.cpp_out.write('{} {{ return {}{}; }}'.format(f.ArgStringWithTypes(),
                                                                      class_name,
                                                                      f.ArgStringWithoutTypes()))
        self.cpp_out.write('\n')
#            self.js_out.WriteLine(class_name + " = Module." + class_name +";")
        self.js_out.write('window["{0}"] = Module.{0};\n'.format(class_name))
        if len(ctors) > 1:
            self.GenerateFunctionSelector(ctors)
            self.js_out.write('window["{0}_"] = Module.{0}_;\n'.format(class_name))


    def RegisterCtorFunctions(self, class_name):
        s = self.symbol_table.symbol(class_name)
        for f in s.children:
            if f.name == s.name and len(f.parameters): # 0-parameter ctors are created with 'new type();'
                good_ctor = True
                for p in f.parameters:
                    if p.BasicType() not in self.known_symbols:
                        good_ctor = False
                        break
                if good_ctor:
                    t = '_'.join(class_name + [p.BasicType() for p in f.parameters])
                    self.cpp_out.write('function("{0}", &{0});\n'.format(t))


if __name__ == '__main__':
    import sys

    if len(sys.argv) < 2:
        sys.exit("Usage: EmbindGenerator <directory_to_doxygen_xml_docs> [class1 [class2 [class3 ... [classN]]]]")

    generator = EmbindGenerator(sys.argv[1], sys.argv[2:])
    generator.output('embind_symbols')
