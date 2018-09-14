#==============================================================================

#==============================================================================

import glob
import os
import re

from lxml import etree

#==============================================================================

# From https://www.pythoncentral.io/how-to-implement-an-enum-in-python/

def enum(*args):
    enums = dict(zip(args, range(len(args))))
    return type('Enum', (), enums)

#==============================================================================

VisibilityLevel = enum(
    'Public',
    'Protected',
    'Private')

def ParseVisibilityLevel(s):
    if s == "protected":
        return VisibilityLevel.Protected
    elif s == "private":
        return VisibilityLevel.Private
    return VisibilityLevel.Public

#==============================================================================

Virtualness = enum(
    'None',
    'Virtual',
    'Pure')

def ParseVirtualness(s):
    if s == "virtual":
        return Virtualness.Virtual
    elif s == "pure" or s == "pure-virtual":
        return Virtualness.Pure
    return Virtualness.None

#==============================================================================

def EscapeFilename(s):
    return (s.replace(".html", "_")
             .replace(".", "_")
             .replace("*", "__mul__")
             .replace("/", "__div__")
             .replace("+", "__plus__")
             .replace("-", "__minus__")
             .replace("::", "__scope__")
             .replace(":", "__dc__")
             .replace("<=", "__leq__")
             .replace("<", "__lt__")
             .replace(">=", "__geq__")
             .replace(">", "__gt__")
             .replace("==", "__eqa__")
             .replace("=", "__eq__")
             .strip())

#==============================================================================

class EnumValue(object):
    def __init__(self):
        self.previousEnum = None
        self.name = None
        self.initializer = ''

    def value(self):
        if len(self.initializer) > 0:
            m = re.match("\\s*=\\s*(.+)", self.initializer)
            if m:
                val = m.group(1)
                return int(val.trim())
        if self.previousEnum != None:
            return self.previousEnum.value() + 1
        else:
            return 0

#==============================================================================

class Parameter(object):
    def __init__(self):
        # int, or const char *, etc..
        self.type = None
        # The name of the symbol.
        self.name = None
        # The default value for this parameter in the function call.
        self.defaultValue = None
        # The @param documentation associated with this parameter.
        self.comment = ''

    def IsAPointer(self):
        return self.type.strip().endswith("*")

    # If type "const float3 &" -> BasicType() returns "float3".
    # If type "const float3 &" -> BasicType() returns "float3".
    # If type "float3 * const" -> BasicType() returns "float3*".
    # If type "const float3 * const" -> BasicType() returns "const float3*".
    def BasicType(self):
        t = self.type.strip()
        if t.endswith("&"):
            t = t.substr(0, len(t) - 1).strip()
            if t.startswith("const"):
                t = t.substr(5).strip()
        if t.endswith("const"):
            t = t.substr(0, len(t) - 5).strip()
        return t

    # Returns the basic type as a sanitated identifier.
    # If type "const float3 &" -> BasicType() returns "float3".
    # If type "const float3 &" -> BasicType() returns "float3".
    # If type "float3 * const" -> BasicType() returns "float3_ptr".
    # If type "const float3 * const" -> BasicType() returns "float3_ptr".
    def BasicTypeId(self):
        t = self.type.strip()
        if t.endswith("&"):
            t = t.substr(0, len(t) - 1).strip()
        if t.startswith("const"):
            t = t.substr(5).strip()
        if t.endswith("const"):
            t = t.substr(0, len(t) - 5).strip()
        if t.endswith("*"):
            t = t.substr(0, len(t) - 1).strip()
            if t.endswith("const"):
                t = t.substr(0, len(t) - 5).strip()
            t = t + "_ptr"
        return t

#==============================================================================

class Symbol(object):
    def __init__(self, id, kind, parent=None):
        self.id = id        # The string that was used as the key to add to the global symbol map.
        self.kind = kind    # "file", "function", "class", "variable", "struct", "enum", "enumvalue", "typedef"
        self.parent = None  #  The symbol this symbol is contained in.

        # <summary>
        # If this is a function, specifies the return value.
        # If this is a member variable, specifies the variable type.
        # If this is a class or a struct, this field is empty.
        # </summary>
        self.type = None
        self.visibilityLevel = None
        self.virtualness = None

        # <summary>
        # The name of the symbol this structure represents.
        # </summary>
        self.name = None
        self.fullDefinition = None
        self.path = None
        self.filename = None
        self.argList = None

        # <summary>
        # If kind == "define", then this symbol is a #define macro, and this string contains the body of the macro.
        # </summary>
        self.macroBody = ''

        # <summary>
        # Specifies under which name this symbol will be present in the class member index.
        # If empty, this member will be hidden from the index.
        # </summary>
        self.classMemberIndexTitle = ''
        self.isStatic = None
        self.isMutable = None
        self.isExplicit = None
        self.isConst = None

        # <summary>
        # The code file where this symbol is defined.
        # </summary>
        self.sourceFilename = None
        self.sourceFileStartLine = None
        self.sourceFileEndLine = None

        # <summary>
        # If this Symbol is "enum", this contains a list of all the different values this enum can take.
        # </summary>
        self.enumValues = []

        # <summary>
        # If this Symbol is "function", this contains a list of all the parameters to the function.
        # </summary>
        self.parameters = []
        self.notes = []
        self.todos = []
        self.bugs = []
#        public Dictionary<string, Symbol> children = new Dictionary<string,Symbol>()
        self.children = []

        # <summary>
        # If this symbol is a class or a struct, this refers to the symbols that are the base classes of this class, or an empty list if no inheritance is used.
        # \todo This has not been implemented yet. Doxygen has <base>BaseclassName</base> elements, implement this!
        # </summary>
        self.baseClasses = []

        # <summary>
        # If non-None, this member specifies an overload of this function that is shown in the
        # doc webpage instead of this symbol. (This is to make the docs look shorter).
        # </summary>
        self.similarOverload = None

        # <summary>
        # This list specifies which other symbols use this symbol as the master doc page.
        # </summary>
        self.otherOverloads = []

        # If kind == "file", this specifies all files this file includes.
        self.includes = []
 #        self.comments = []

        # <summary>
        # If True, the generated documentation page should group the C++ syntax block with the other overloads of this function.
        # </summary>
        self.groupSyntax = False

        # <summary>
        # Specifies the documentation for the @return field.
        # </summary>
        self.returnComment = ''

        # <summary>
        # Lists all the [docGeneratorAttribute] sections in code comments for this symbol, without the '[' and ']' characters.
        # </summary>
        self.attributes = []

        self.documentationCategory = ''


    # For 'typedef boost::uint8_t u8' returns 'boost::uint8_t'
    def TypedefRealType(self):
        m = re.match("typedef\\s+(.+)\\s+\\S+", self.fullDefinition)
        if m:
            return m.group(1)
        else:
            return ''

    def HasOverloadsWithSameFunctionName(self):
        for s in self.parent.children:
            if s != self and s.name == self.name:
                return True
        return False

    # <summary>
    # If this Symbol is a class or a struct, returns the child symbol that is considered to be the ctor of this class.
    # </summary>
    # <returns></returns>
    def ClassCtor(self):
        if self.kind != "class" and self.kind != "struct":
            return None
        for s in self.children:
            if s.NameWithoutNamespace() == self.name and s.kind == "function":
                if s.similarOverload != None:
                    return s.similarOverload
                else:
                    return s
        return None # children.First()

    def AllClassCtors(self):
        if self.kind != "class" and self.kind != "struct":
            return []
        ctors = []
        for s in self.children:
            if s.NameWithoutNamespace() == self.name and s.kind == "function":
                ctors.append(s)
        return ctors

    def IsClassAbstract(self):  # Returns True if class contains pure virtual functions.
        for s in self.children:
            if s.kind == "function" and s.virtualness == Virtualness.Pure:
                Console.WriteLine("Class " + self.name + " is abstract because of " + s.name)
                return True
        return False

    def ClassCopyCtor(self):
        if self.kind != "class" and self.kind != "struct":
            return None
        for s in self.children:
            if s.NameWithoutNamespace() == self.name and s.kind == "function":
                if len(s.parameters) == 1 and s.parameters[0].type == ("const " + self.NameWithoutNamespace() + " &"):
                    return s
        return None # children.First()

    def ClassAssignmentOperator(self):
        if self.kind != "class" and self.kind != "struct":
            return None
        for s in self.children:
            if s.NameWithoutNamespace() == "operator=":
                return s
        return None # children.First()

    def ClassDtor(self):
        if self.kind != "class" and self.kind != "struct":
            return None
        for s in self.children:
            if s.NameWithoutNamespace() == "~" + self.name and s.kind == "function":
                if s.similarOverload != None:
                    return s.similarOverload
                else:
                    return s
        return None # children.First()

    # <summary>
    # If this symbol is a function, returns the parameter list without identifier names. E.g. int foo(int a, int b, float *c) returns "int, int, float *"
    # </summary>
    # <returns></returns>
    def FunctionParameterListWithoutNames(self):
        ','.join([p.type for p in self.parameters])

    def Namespace(self):
        i = self.name.LastIndexOf("::")
        if i != -1:
            return self.name.substr(0, i)
        return ''

    def ArgStringWithoutTypes(self):
        return '(' + ','.join([p.name for p in self.parameters]) + ')'

    def ArgStringWithTypes(self):
        return '(' + ','.join([p.type + ' ' + p.name for p in self.parameters]) + ')'

    def FindChildByName(self, name):
        for s in self.children:
            if s.name == name:
                return s
        return None

    def NameWithoutNamespace(self):
        ns = self.Namespace()
        nameWithoutNamespace = self.name.substr(len(ns))
        if nameWithoutNamespace.startswith("::"):
            return nameWithoutNamespace.substr(2)
        else:
            return nameWithoutNamespace

    def IsArray(self):
        return self.argList.startswith("[") and self.argList.endswith("]")

    def ArrayLength(self):
        try:
            return int(self.argList[1: -1])
        except ValueError:
            return 0

    def IsConst(self):
        return (self.isConst
             or kind == "function" and self.argList.endswith("const")
             or kind == "variable" and 'const' in self.type)

    def FindParamByName(self, paramName):
        for p in self.parameters:
            if p.name == paramName:
                return p
        return None

    def Category(self):
        return self.documentationCategory

    def ScopeName(self):
        s = ''
        p = self.parent
        while p != None and (p.kind == "class" or p.kind == "struct"):
            s = p.name + "::" + s
            p = p.parent
        return s

    def ScopeNameWithoutNamespace(self):
        s = ''
        p = self.parent
        while p != None and (p.kind == "class" or p.kind == "struct"):
            s = p.NameWithoutNamespace() + "::" + s
            p = p.parent
        return s

    def FullQualifiedSymbolName(self):
        if self.kind == "typedef":
            return self.name; # todo!
        s = self.type + " "
        s += self.ScopeName()
        s += self.name
        s += self.argList
        return s.strip()

    def FullQualifiedSymbolNameWithoutNamespace(self):
        if self.kind == "define":
            return "#define " + self.NameWithoutNamespace() + self.argList
        else:
            s = self.type + " "
            s += ScopeNameWithoutNamespace()
            s += NameWithoutNamespace()
            s += argList
            return s

    # pageSuffix = "php"

    def ClassIndexDocFilename(self):
        return "index_" + EscapeFilename(self.name) + ".php"

    def MemberDocumentationFilename(self):
        return MemberDocumentationFilenameWithoutFileSuffix() + ".php"

    def MemberDocumentationFilenameWithoutFileSuffix(self):
        if kind == "class" or kind == "struct":
            return EscapeFilename(self.name) + "_summary"
        if self.similarOverload != None:
            return self.similarOverload.MemberDocumentationFilenameWithoutFileSuffix()
        if self.parent != None:
            return EscapeFilename(self.parent.name) + "_" + EscapeFilename(self.name)
        else:
            return EscapeFilename(self.name)

    def IsPODType(self, _type):
        return _type in ["bool", "int", "float"]  # TODO: Add more basic types here.

    def FetchFunctionOverloads(self, knownSymbolNames):
        functionOverloads = [s for s in self.parent.children if s.name == self.name]
                          # && IsGoodSymbol(s, knownSymbolNames)
        functionOverloads.sort(lambda left, right: len(right.parameters) - len(left.parameters))
        return functionOverloads

#==============================================================================

class VariableListEntry(object):
    def __init__(self):
        self.item = None
        self.value = None

#==============================================================================

class CodeFile(object):
    def __init__(self, filename):
        self.filename = filename
        self.lines = []

#==============================================================================

class SymbolTable(object):
    def __init__(self):
        self.codeFiles = {}
        self.symbols = {}
        self.symbolsByName = {}
        self.todos = []
        self.bugs = []

    def contains(self, name):
        return name in self.symbolsByName

    def symbol(self, name):
        return self.symbolsByName.get(name)

    def LoadSymbolsFromDirectory(self, dir):
        for filename in glob.glob(os.path.join(dir, '*.xml')):
            self.LoadSymbolsFromFile(filename)
        self.GroupSimilarOverloads()

    def LoadSymbolsFromFile(self, filename):
        doc = etree.parse(filename)
        root = doc.getroot()
        for e in root:
            if e.tag == "compounddef":
                if e.get("kind") != "page":
                    if e.get("kind") == "file":
                        self.ParseCodeFileList(e)
                    self.ParseCompoundDefElement(e)


    # Goes through all symbols recursively and groups similar functions
    # together, setting the similarOverload attribute.
    def GroupSimilarOverloads(self):
        for s in self.symbols.itervalues():
            if s.kind == "class" or s.kind == "struct":
                self.GroupSimilarSymbolOverloads(s)

    @staticmethod
    def GroupSimilarSymbolOverloads(s):
        for i, master in enumerate(s.children):
            for j in xrange(i+1, xrange(len(s.children.Count))):
                child = s.children[j]
                if master.similarOverload == None and child.similarOverload == None and child.otherOverloads.Count == 0:
                    if master.name == child.name:
                        if len(child.Comments()) == 0 or child.groupSyntax:
                            child.similarOverload = master
                            master.otherOverloads.append(child)
                        else:
                            break

    def contains(self, name):
        return name in self._symbols

    def symbol(self, name):
        return self._symbols.get(name)


    def ParseSectionDefElement(self, parent, e):
        for child in e:
            if child.tag == "memberdef":
                member = Symbol(child.get("id"), child.get("kind"), parent)
                member.visibilityLevel = ParseVisibilityLevel(child.get("prot"))
                member.isStatic = (child.get("static") == "yes")
                member.isConst = (child.get("const") == "yes")
                member.isMutable = (child.get("mutable") == "yes")
                member.isExplicit = (child.get("explicit") == "yes")
                member.virtualness = ParseVirtualness(child.get("virt"))
                member.type = GetXmlElementChildNodeValue(child, "type", True)
                if (member.kind == "variable" and member.type.startswith("const ")
                 and not (member.type.endswith("*") or member.type.endswith("&"))):
                    member.type = member.type.substr(6).strip()
                    member.isConst = True
                member.fullDefinition = GetXmlElementChildNodeValue(child, "definition", True)
                member.argList = GetXmlElementChildNodeValue(child, "argsstring")

                member.name = GetXmlElementChildNodeValue(child, "name", True)
                member.classMemberIndexTitle = member.name
                if member.name.startswith("@"):
                    continue;  # Doxygen creates items with names starting with '@' at least for unnamed unions, ignore those altogether.
                symbols[member.id] = member
                if member.kind == "typedef" or member.kind == "enum":
                    symbolsByName[member.FullQualifiedSymbolName()] = member
                if member.name == "EntityPtr":
                    print(member.name + " == " + member.FullQualifiedSymbolName() + ", parent: " + parent.name)
                parent.children.append(member)

                 # Function parameters.
                for param in child:
                    if param.tag == "param":
                        p = Parameter()
                        if member.kind == "define":  # This is a #define macro.
                            p.type = ''
                            p.name = GetXmlElementChildNodeValue(param, "defname")
                        else:  # This is a real function
                            p.type = GetXmlElementChildNodeValue(param, "type", True)
                            if p.type.startswith("constScene"):
                                Console.WriteLine(p.type)
                            p.name = GetXmlElementChildNodeValue(param, "declname")
                        member.parameters.append(p)
                    elif param.tag == "enumvalue":
                        ev = EnumValue()
                        ev.name = GetXmlElementChildNodeValue(param, "name", True)
                        ev.initializer = GetXmlElementChildNodeValue(param, "initializer", True)
                        if member.enumValues.Count > 0:
                            ev.previousEnum = member.enumValues.Last()
                        member.enumValues.append(ev)

                 # If this is a #define macro, get the macro body code.
                if member.kind == "define":
                    member.macroBody = GetXmlElementChildNodeValue(child, "initializer")
                     # Create the argList from scratch, because it is not present in the xml in same form than for functions.
                    member.argList = member.ArgStringWithoutTypes()

                 # Function parameter comments.
                parameters = child.AllGrandChildElementsOfTypeAndAttribute("parameterlist", "kind", "param")
                if parameters != None and len(parameters):
                    for paramNode in parameters:
                        for param in paramNode:
                            if param.tag == "parameteritem":
                                paramName = param.FirstChildElementOfType("parameternamelist").FirstChildElementOfType("parametername").InnerText.strip()
                                p = member.FindParamByName(paramName)
                                if p != None:
                                    p.comment = param.FirstChildElementOfType("parameterdescription").InnerText
                         # Remove the parameterlist from the detailed description node so that it won't appear in the 'detailedDescription' documentation string.
                         # The detailed description is created manually.
                        paramNode.ParentNode.RemoveChild(paramNode)

                 # Function return value.
                retVal = child.FirstGrandChildElementOfTypeAndAttribute("simplesect", "kind", "return")
                if retVal != None:
                    member.returnComment = retVal.InnerText
                    retVal.ParentNode.RemoveChild(retVal)

                # TODO: Add location.

    def ParseCodeFileList(self, e):
        codeFile = CodeFile(''.join(e.find('compoundname').itertext()))
        l = FirstGrandChildElementOfType(e, "programlisting")
        if l == None:
            return
        prevLine = 0
        for c in l:
            if c.tag == "codeline":
                curLine = int(c.get("lineno"))
                for i in xrange(curLine - 1 - prevLine):
                    codeFile.lines.append('')
                codeFile.lines.append(c.InnerXml)
                prevLine = curLine
        codeFiles[FirstChildElementOfType(e, "location").get("file")] = codeFile


    def ParseCompoundDefElement(self, e):
        s = Symbol(e.get("id"), e.get("kind"))
        s.visibilityLevel = ParseVisibilityLevel(e.get("prot"))
        s.name = GetXmlElementChildNodeValue(e, "compoundname")
        s.classMemberIndexTitle = s.name
        self.symbols[s.id] = s
        self.symbolsByName[s.name] = s
        for child in e:
            if child.tag == "sectiondef":
                self.ParseSectionDefElement(s, child)
         # Also has the following members:
         # <collaborationgraph>
         # <location>
         # <listofallmembers>
         # <includes>

#==============================================================================

def FirstChildElementOfType(e, tag):
    return e.find(tag)

def FirstChildElementOfTypeAndAttribute(e, tag, attr, value):
    for child in e.findall(tag):
        if child.get(attr) == value:
            return child
    return None

def AllChildElementsOfType(e, tag):
    return e.findall(tag)

def AllChildElementsOfTypeAndAttribute(e, tag, attr, value):
    return [child for child in e.findall(tag) if child.get(attr) == value]

def FirstGrandChildElementOfType(e, tag):
    t = e.find(tag)
    if t != None: return t
    for child in e:
        t = FirstGrandChildElementOfType(child, tag)
        if t != None: return t
    return None

def FirstGrandChildElementOfTypeAndAttribute(e, tag, attr, value):
    t = FirstChildElementOfTypeAndAttribute(e, tag, attr, value)
    if t != None: return t
    for child in e:
        t = FirstGrandChildElementOfTypeAndAttribute(child, tag, attr, value)
        if t != None: return t
    return None

def AllGrandChildElementsOfType(e, tag):
    l2 = AllChildElementsOfType(e, tag)
    if len(l2): return l2
    l = []
    for child in e:
        l2 = AllGrandChildElementsOfType(child, tag)
        if l2 != None: l.extend(l2)
    return l

def AllGrandChildElementsOfTypeAndAttribute(e, tag, attr, value):
    l2 = AllChildElementsOfTypeAndAttribute(e, tag, attr, value)
    if len(l2): return l2
    l = []
    for child in e:
        l2 = AllGrandChildElementsOfTypeAndAttribute(child, tag, attr, value)
        l.extend(l2)
    return l

# <summary>
# Compiled regular expression for performance.
# </summary>
_htmlRegex = re.compile("<.*?>")

# <summary>
# Remove HTML from string with compiled Regex.
# </summary>
def StripTagsRegexCompiled(source):
    return _htmlRegex.sub(source, " ")


def GetXmlElementChildNodeValue(element, childNode, asText=False):
    for e in element:
        if e.tag == childNode:
            xml = ''
            if asText:
                xml = StripTagsRegexCompiled(e.InnerXml).StripHtmlCharacters().replace("  ", " ")
            else:
                xml = e.InnerXml
            xml = xml.strip()
            if xml.startswith("<p>") and xml.endswith("</p>"):
                xml = xml.substr(3, len(xml) - 7).strip()
            if xml == "<p />":
                return ''
            return xml
    return ''

def GetChildElementsByName(node, tag):
    return e.findall(tag)

#==============================================================================

# <summary>
# Outputs endIdx = -1 if not found.
# </summary>
# <param name="dataString"></param>
# <param name="startIdx"></param>
# <param name="start"></param>
# <param name="end"></param>
# <param name="endIdx"></param>
# <returns></returns>
def FindStringInBetween(dataString, startIdx, start, end):
    endIdx = -1
    patternStart = dataString.find(start, startIdx)
    if patternStart == -1:
        return ('', -1)
    patternStart += len(start)
    patternEnd = dataString.find(end, patternStart)
    if patternEnd == -1:
        return ('', -1)
    text = dataString.substr(patternStart, patternEnd - patternStart)
    endIdx = patternEnd + len(end)
    return (text, endIdx)

def StripHtmlCharacters(s):
    return System.Net.WebUtility.HtmlDecode(str.replace("&nbsp;", " ").replace("&#160;", " ").replace("&amp;", "&")).replace("&lt;", "<").replace("&gt;", ">").strip()

def StripHtmlLinks(s):
    while True:  # TODO: regex.
        firstA = s.find("<a")
        firstCloseA = s.find("</a")
        if firstA == -1:
            firstA = len(s) + 1
        if firstCloseA == -1:
            firstCloseA = len(s) + 1
        start = min(firstA, firstCloseA)
        if start == (len(s) + 1):
            return s
        end = s.find(">", start)
        if end == -1:
            return s
        s = s.substr(0, start) + s.substr(end + 1)

#==============================================================================
