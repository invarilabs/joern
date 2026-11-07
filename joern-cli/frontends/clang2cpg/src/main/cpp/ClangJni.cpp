// Explicit in-process Clang-to-JVM prototype. No serialized AST transport.
#include <jni.h>
#include <algorithm>
#include <map>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/TypeTraits.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Lex/Lexer.h"
#include "clang/Lex/MacroArgs.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/SHA256.h"
#include "llvm/Support/VirtualFileSystem.h"
#include "llvm/Support/Program.h"

namespace {
using namespace clang;
[[noreturn]] void fail(const std::string &Message) { throw std::runtime_error("Clang JNI: " + Message); }
std::string identifier(const void *Pointer) {
  if (!Pointer) fail("required compiler identity is null");
  return "0x" + llvm::utohexstr(reinterpret_cast<uintptr_t>(Pointer), true);
}
std::string declarationUsr(const NamedDecl *Declaration) {
  llvm::SmallString<128> Buffer;
  if (index::generateUSRForDecl(Declaration->getCanonicalDecl(), Buffer)) fail("declaration lacks a compiler USR");
  return Buffer.str().str();
}
struct LocalFrame {
  JNIEnv *Env;
  explicit LocalFrame(JNIEnv *Value) : Env(Value) { if (Env->PushLocalFrame(128) < 0) fail("cannot allocate JNI local frame"); }
  ~LocalFrame() { if (Env) Env->PopLocalFrame(nullptr); }
  jobject keep(jobject Value) { jobject Result = Env->PopLocalFrame(Value); Env = nullptr; return Result; }
};
class JavaValues {
  JNIEnv *Env;
  jclass MapClass, ListClass, LongClass, DoubleClass, BooleanClass;
  jmethodID MapConstructor, Put, ListConstructor, Add, LongValue, DoubleValue, BooleanValue;
public:
  explicit JavaValues(JNIEnv *Value) : Env(Value) {
    MapClass = Env->FindClass("java/util/LinkedHashMap"); ListClass = Env->FindClass("java/util/ArrayList");
    LongClass = Env->FindClass("java/lang/Long"); DoubleClass = Env->FindClass("java/lang/Double"); BooleanClass = Env->FindClass("java/lang/Boolean");
    check();
    MapConstructor = Env->GetMethodID(MapClass, "<init>", "()V"); Put = Env->GetMethodID(MapClass, "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
    ListConstructor = Env->GetMethodID(ListClass, "<init>", "()V"); Add = Env->GetMethodID(ListClass, "add", "(Ljava/lang/Object;)Z");
    LongValue = Env->GetStaticMethodID(LongClass, "valueOf", "(J)Ljava/lang/Long;");
    DoubleValue = Env->GetStaticMethodID(DoubleClass, "valueOf", "(D)Ljava/lang/Double;");
    BooleanValue = Env->GetStaticMethodID(BooleanClass, "valueOf", "(Z)Ljava/lang/Boolean;"); check();
  }
  JNIEnv *environment() const { return Env; }
  void check() const { if (Env->ExceptionCheck()) fail("JVM rejected native map construction"); }
  jobject object() { jobject Result = Env->NewObject(MapClass, MapConstructor); check(); return Result; }
  jobject list() { jobject Result = Env->NewObject(ListClass, ListConstructor); check(); return Result; }
  jstring string(llvm::StringRef Text) {
    llvm::SmallVector<llvm::UTF16, 128> Units;
    if (!llvm::convertUTF8ToUTF16String(Text, Units)) fail("compiler text is not valid UTF-8");
    jstring Result = Env->NewString(reinterpret_cast<const jchar *>(Units.data()), Units.size()); check(); return Result;
  }
  jobject number(int64_t Value) { jobject Result = Env->CallStaticObjectMethod(LongClass, LongValue, static_cast<jlong>(Value)); check(); return Result; }
  jobject real(double Value) { jobject Result = Env->CallStaticObjectMethod(DoubleClass, DoubleValue, static_cast<jdouble>(Value)); check(); return Result; }
  jobject boolean(bool Value) { jobject Result = Env->CallStaticObjectMethod(BooleanClass, BooleanValue, static_cast<jboolean>(Value)); check(); return Result; }
  void put(jobject Object, llvm::StringRef Name, jobject Value) {
    if (!Value) fail("null structured compiler value for " + Name.str());
    jstring Key = string(Name); jobject Previous = Env->CallObjectMethod(Object, Put, Key, Value); check();
    if (Previous) Env->DeleteLocalRef(Previous);
    Env->DeleteLocalRef(Key); Env->DeleteLocalRef(Value);
  }
  void add(jobject Array, jobject Value) {
    if (!Value) fail("null structured compiler list value");
    Env->CallBooleanMethod(Array, Add, Value); check(); Env->DeleteLocalRef(Value);
  }
  void text(jobject Object, llvm::StringRef Name, llvm::StringRef Value) { put(Object, Name, string(Value)); }
  void integer(jobject Object, llvm::StringRef Name, int64_t Value) { put(Object, Name, number(Value)); }
  void flag(jobject Object, llvm::StringRef Name, bool Value) { put(Object, Name, boolean(Value)); }
};

class CompilerMaps {
  JavaValues &Java;
  ASTContext &Context;
  SourceManager &Manager;
  std::map<const EnumDecl *, std::string> EnumIdentities;
  std::set<const Decl *> ActiveDeclarations;
  std::set<const Decl *> WrittenDeclarations;
  std::set<const Stmt *> WrittenStatements;
public:
  jobject Nodes;
  struct EnumInventory : RecursiveASTVisitor<EnumInventory> {
    std::vector<const EnumDecl *> Ordered; std::set<const EnumDecl *> Seen;
    bool VisitEnumDecl(EnumDecl *Declaration) {
      const auto *Canonical = Declaration->getCanonicalDecl();
      if (Seen.insert(Canonical).second) Ordered.push_back(Canonical);
      return true;
    }
  };
  CompilerMaps(JavaValues &Values, ASTContext &Value, jobject Metadata) : Java(Values), Context(Value), Manager(Context.getSourceManager()), Nodes(Metadata) {
    EnumInventory Inventory; Inventory.TraverseDecl(Context.getTranslationUnitDecl());
    std::map<std::string, std::vector<const EnumDecl *>> Groups;
    for (const auto *Declaration : Inventory.Ordered) Groups[declarationUsr(Declaration)].push_back(Declaration);
    for (const auto &Group : Groups)
      for (size_t Index = 0; Index < Group.second.size(); ++Index)
        EnumIdentities[Group.second[Index]] = Group.first + (Group.second.size() == 1 ? "" : "#enum:" + std::to_string(Index));
  }
  bool main(SourceLocation Location) const { return Location.isValid() && Manager.isWrittenInMainFile(Manager.getExpansionLoc(Location)); }
  std::string enumIdentity(const EnumDecl *Declaration) const {
    auto Found = EnumIdentities.find(Declaration->getCanonicalDecl());
    if (Found == EnumIdentities.end()) fail("enum declaration lacks compiler inventory identity");
    return Found->second;
  }
  std::string typeName(QualType Type) {
    if (Type.isNull()) fail("required compiler type is null");
    QualType Canonical = Context.getCanonicalType(Type); const clang::Type *Shape = Canonical.getTypePtr();
    std::string Prefix = Canonical.isConstQualified() ? "const " : "";
    if (Canonical.isVolatileQualified()) Prefix += "volatile ";
    if (Canonical.isRestrictQualified()) Prefix += "restrict ";
    if (auto Record = dyn_cast<RecordType>(Shape)) return Prefix + "clang:" + declarationUsr(Record->getDecl());
    if (auto Enum = dyn_cast<EnumType>(Shape)) return Prefix + "clang:" + enumIdentity(Enum->getDecl());
    if (auto Pointer = dyn_cast<PointerType>(Shape)) return typeName(Pointer->getPointeeType()) + "*" + (Prefix.empty() ? "" : " " + Prefix);
    if (auto Array = dyn_cast<ArrayType>(Shape)) {
      std::string Extent;
      if (auto Constant = dyn_cast<ConstantArrayType>(Array)) { llvm::SmallString<32> Digits; Constant->getSize().toString(Digits, 10, false); Extent = Digits.str().str(); }
      else if (!isa<VariableArrayType>(Array) && !isa<IncompleteArrayType>(Array)) fail("unsupported compiler array extent");
      return typeName(Array->getElementType()) + "[" + Extent + "]";
    }
    if (auto Function = dyn_cast<FunctionType>(Shape)) {
      std::string Result = typeName(Function->getReturnType()) + "(";
      if (auto Prototype = dyn_cast<FunctionProtoType>(Function)) {
        bool First = true; for (QualType Parameter : Prototype->param_types()) { if (!First) Result += ","; First = false; Result += typeName(Parameter); }
        if (Prototype->isVariadic()) Result += First ? "..." : ",...";
      }
      return Result + ")";
    }
    if (isa<BuiltinType>(Shape)) return Canonical.getAsString(Context.getPrintingPolicy());
    if (auto Atomic = dyn_cast<AtomicType>(Shape)) return "_Atomic(" + typeName(Atomic->getValueType()) + ")";
    if (auto Complex = dyn_cast<ComplexType>(Shape)) return "_Complex " + typeName(Complex->getElementType());
    if (auto Vector = dyn_cast<VectorType>(Shape)) return "vector[" + std::to_string(Vector->getNumElements()) + "](" + typeName(Vector->getElementType()) + ")";
    fail(std::string("unsupported compiler type constructor ") + Shape->getTypeClassName());
  }
  jobject semantic(QualType Type) {
    LocalFrame Frame(Java.environment());
    if (Type.isNull()) fail("semantic type is null");
    QualType Canonical = Context.getCanonicalType(Type); const clang::Type *Shape = Canonical.getTypePtr(); jobject Result = Java.object();
    Java.text(Result, "canonical_identity", typeName(Canonical)); Java.text(Result, "value_spelling", Canonical.getAsString(Context.getPrintingPolicy()));
    jobject Qualifiers = Java.list();
    if (Canonical.isConstQualified()) Java.add(Qualifiers, Java.string("const"));
    if (Canonical.isVolatileQualified()) Java.add(Qualifiers, Java.string("volatile"));
    if (Canonical.isRestrictQualified()) Java.add(Qualifiers, Java.string("restrict"));
    Java.put(Result, "qualifiers", Qualifiers);
    if (auto Pointer = dyn_cast<PointerType>(Shape)) {
      Java.text(Result, "kind", "Pointer"); Java.put(Result, "pointee", semantic(Pointer->getPointeeType()));
    } else if (auto Function = dyn_cast<FunctionType>(Shape)) {
      Java.put(Result, "result", semantic(Function->getReturnType())); jobject Arguments = Java.list();
      if (auto Prototype = dyn_cast<FunctionProtoType>(Function)) {
        Java.text(Result, "kind", "FunctionProto"); Java.flag(Result, "variadic", Prototype->isVariadic());
        for (QualType Argument : Prototype->param_types()) Java.add(Arguments, semantic(Argument));
      } else { Java.text(Result, "kind", "FunctionNoProto"); Java.flag(Result, "variadic", false); }
      Java.put(Result, "arguments", Arguments);
    } else if (auto Record = dyn_cast<RecordType>(Shape)) {
      const auto *Declaration = Record->getDecl(); std::string Name = Declaration->getNameAsString();
      if (Name.empty() && Declaration->getTypedefNameForAnonDecl()) Name = Declaration->getTypedefNameForAnonDecl()->getNameAsString();
      Java.text(Result, "kind", "Record"); Java.text(Result, "declaration_usr", declarationUsr(Declaration)); Java.text(Result, "declaration_name", Name);
      Java.text(Result, "tag", Declaration->isUnion() ? "union" : "struct"); Java.flag(Result, "complete", Declaration->getDefinition() != nullptr);
    } else if (auto Enum = dyn_cast<EnumType>(Shape)) {
      Java.text(Result, "kind", "Enum"); Java.text(Result, "declaration_usr", declarationUsr(Enum->getDecl())); Java.text(Result, "declaration_name", Enum->getDecl()->getNameAsString());
      const auto Stable = enumIdentity(Enum->getDecl()); if (Stable != declarationUsr(Enum->getDecl())) Java.text(Result, "declaration_identity", Stable);
    } else if (auto Array = dyn_cast<ArrayType>(Shape)) {
      Java.put(Result, "element", semantic(Array->getElementType()));
      if (auto Constant = dyn_cast<ConstantArrayType>(Array)) {
        if (Constant->getSize().getActiveBits() > 63) fail("compiler array extent exceeds signed 64-bit range");
        Java.text(Result, "kind", "ConstantArray"); Java.integer(Result, "extent", Constant->getSize().getZExtValue());
      } else if (isa<IncompleteArrayType>(Array)) Java.text(Result, "kind", "IncompleteArray");
      else if (isa<VariableArrayType>(Array)) Java.text(Result, "kind", "VariableArray");
      else fail("unsupported compiler array semantic constructor");
    } else if (auto Atomic = dyn_cast<AtomicType>(Shape)) {
      Java.text(Result, "kind", "Atomic"); Java.put(Result, "value", semantic(Atomic->getValueType()));
    } else if (auto Complex = dyn_cast<ComplexType>(Shape)) {
      Java.text(Result, "kind", "Complex"); Java.put(Result, "element", semantic(Complex->getElementType()));
    } else if (auto Vector = dyn_cast<VectorType>(Shape)) {
      Java.text(Result, "kind", "Vector"); Java.put(Result, "element", semantic(Vector->getElementType())); Java.integer(Result, "elements", Vector->getNumElements());
    } else if (auto Builtin = dyn_cast<BuiltinType>(Shape)) {
      std::string Kind;
      switch (Builtin->getKind()) {
#define IMAGE_TYPE(ImgType, Id, SingletonId, Access, Suffix) case BuiltinType::Id: Kind = #Id; break;
#include "clang/Basic/OpenCLImageTypes.def"
#define EXT_OPAQUE_TYPE(ExtType, Id, Ext) case BuiltinType::Id: Kind = #Id; break;
#include "clang/Basic/OpenCLExtensionTypes.def"
#define SVE_TYPE(Name, Id, SingletonId) case BuiltinType::Id: Kind = #Id; break;
#include "clang/Basic/AArch64SVEACLETypes.def"
#define PPC_VECTOR_TYPE(Name, Id, Size) case BuiltinType::Id: Kind = #Id; break;
#include "clang/Basic/PPCTypes.def"
#define RVV_TYPE(Name, Id, SingletonId) case BuiltinType::Id: Kind = #Id; break;
#include "clang/Basic/RISCVVTypes.def"
#define WASM_TYPE(Name, Id, SingletonId) case BuiltinType::Id: Kind = #Id; break;
#include "clang/Basic/WebAssemblyReferenceTypes.def"
#define AMDGPU_TYPE(Name, Id, SingletonId, Width, Align) case BuiltinType::Id: Kind = #Id; break;
#include "clang/Basic/AMDGPUTypes.def"
#define HLSL_INTANGIBLE_TYPE(Name, Id, SingletonId) case BuiltinType::Id: Kind = #Id; break;
#include "clang/Basic/HLSLIntangibleTypes.def"
#define BUILTIN_TYPE(Id, SingletonId) case BuiltinType::Id: Kind = #Id; break;
#include "clang/AST/BuiltinTypes.def"
#undef BUILTIN_TYPE
      default: fail("unsupported target-specific builtin type");
      }
      if (Kind.empty()) fail("missing compiler builtin type name"); Java.text(Result, "kind", Kind);
    } else fail(std::string("unsupported semantic type ") + Shape->getTypeClassName());
    return Frame.keep(Result);
  }
  jobject location(SourceRange Range, bool IncludeCode = true) {
    LocalFrame Frame(Java.environment()); jobject Result = Java.object();
    if (Range.isInvalid()) { Java.flag(Result, "generated", true); return Frame.keep(Result); }
    SourceLocation Begin = Manager.getExpansionLoc(Range.getBegin()), Last = Manager.getExpansionLoc(Range.getEnd());
    if (!Begin.isFileID() || !Last.isFileID() || Manager.getFileID(Begin) != Manager.getFileID(Last)) fail("source range crosses physical files");
    unsigned Start = Manager.getFileOffset(Begin), End = Manager.getFileOffset(Lexer::getLocForEndOfToken(Last, 0, Manager, Context.getLangOpts()));
    llvm::StringRef Buffer = Manager.getBufferData(Manager.getFileID(Begin));
    if (End < Start || End > Buffer.size()) fail("invalid compiler source range");
    Java.text(Result, "file", Manager.getFilename(Begin)); Java.integer(Result, "offset", Start); Java.integer(Result, "end", End);
    Java.integer(Result, "line", Manager.getSpellingLineNumber(Begin)); Java.integer(Result, "column", Manager.getSpellingColumnNumber(Begin));
    if (IncludeCode) Java.text(Result, "code", Buffer.slice(Start, End));
    Java.flag(Result, "macro", Range.getBegin().isMacroID() || Range.getEnd().isMacroID());
    SourceLocation Spelling = Manager.getSpellingLoc(Range.getBegin()), SpellingLast = Manager.getSpellingLoc(Range.getEnd());
    Java.text(Result, "spelling_file", Manager.getFilename(Spelling)); Java.integer(Result, "spelling_offset", Manager.getFileOffset(Spelling));
    if (Spelling.isFileID() && SpellingLast.isFileID() && Manager.getFileID(Spelling) == Manager.getFileID(SpellingLast)) {
      unsigned First = Manager.getFileOffset(Spelling), Finish = Manager.getFileOffset(Lexer::getLocForEndOfToken(SpellingLast, 0, Manager, Context.getLangOpts()));
      llvm::StringRef Physical = Manager.getBufferData(Manager.getFileID(Spelling));
      if (IncludeCode && First <= Finish && Finish <= Physical.size()) Java.text(Result, "spelling_code", Physical.slice(First, Finish));
    }
    return Frame.keep(Result);
  }
  void sourceType(jobject Ast, jobject Info, QualType Type) {
    jobject Spelling = Java.object(); Java.text(Spelling, "qualType", Type.getAsString(Context.getPrintingPolicy())); Java.put(Ast, "type", Spelling);
    Java.text(Info, "type", typeName(Type)); Java.put(Info, "semantic_type", semantic(Type));
  }
  jobject reference(const ValueDecl *Declaration) {
    LocalFrame Frame(Java.environment()); jobject Result = Java.object();
    Java.text(Result, "id", identifier(Declaration)); Java.text(Result, "kind", std::string(Declaration->getDeclKindName()) + "Decl");
    Java.text(Result, "name", Declaration->getNameAsString());
    jobject Type = Java.object(); Java.text(Type, "qualType", Declaration->getType().getAsString(Context.getPrintingPolicy())); Java.put(Result, "type", Type);
    return Frame.keep(Result);
  }
  jobject declaration(const Decl *Declaration) {
    LocalFrame Frame(Java.environment());
    if (!ActiveDeclarations.insert(Declaration).second) fail("recursive compiler declaration syntax");
    jobject Result = Java.object(); Java.text(Result, "id", identifier(Declaration));
    Java.text(Result, "kind", std::string(Declaration->getDeclKindName()) + "Decl");
    SourceRange Range = Declaration->getSourceRange();
    if (const auto *Variable = dyn_cast<VarDecl>(Declaration)) {
      if (Variable->isFileVarDecl() && Variable->getLocation().isValid() && !main(Variable->getLocation()))
        Range = SourceRange(Variable->getLocation(), Range.getEnd());
    }
    jobject Info = location(Range); Java.text(Info, "canonical", identifier(Declaration->getCanonicalDecl()));
    if (const auto *Named = dyn_cast<NamedDecl>(Declaration)) { Java.text(Info, "name", Named->getNameAsString()); Java.text(Result, "name", Named->getNameAsString()); }
    if (Declaration->isImplicit()) Java.flag(Result, "isImplicit", true);
    if (const auto *Value = dyn_cast<ValueDecl>(Declaration)) sourceType(Result, Info, Value->getType());
    jobject Children = Java.list();
    if (const auto *Variable = dyn_cast<VarDecl>(Declaration)) {
      Java.flag(Info, "file_scope", Variable->isFileVarDecl()); Java.flag(Info, "has_initializer", Variable->hasInit());
      Java.text(Info, "storage_duration", Variable->getTLSKind() != VarDecl::TLS_None ? "thread" : Variable->hasGlobalStorage() ? "static" : "automatic");
      Java.text(Info, "initializer_kind", Variable->hasInit() ? "explicit" : Variable->hasGlobalStorage() && Variable->isThisDeclarationADefinition() != VarDecl::DeclarationOnly ? "implicit_zero" : Variable->isFileVarDecl() ? "external" : "uninitialized");
      if (!isa<ParmVarDecl>(Variable) && (Variable->isFileVarDecl() || main(Variable->getLocation()))) Java.text(Info, "usr", declarationUsr(Variable));
      if (const auto *Parameter = dyn_cast<ParmVarDecl>(Variable)) Java.text(Info, "original_type", typeName(Parameter->getOriginalType()));
      if (Variable->hasInit() && main(Variable->getLocation())) Java.add(Children, statement(Variable->getInit()));
    } else if (const auto *Function = dyn_cast<FunctionDecl>(Declaration)) {
      Java.text(Info, "usr", declarationUsr(Function)); Java.flag(Info, "externally_visible", Function->isExternallyVisible());
      Java.text(Info, "result_type", typeName(Function->getReturnType())); Java.text(Info, "result_spelling", Function->getReturnType().getAsString(Context.getPrintingPolicy()));
      Java.flag(Info, "variadic", Function->isVariadic()); Java.flag(Info, "has_body", Function->doesThisDeclarationHaveABody());
      for (const ParmVarDecl *Parameter : Function->parameters()) Java.add(Children, declaration(Parameter));
      if (Function->doesThisDeclarationHaveABody() && main(Function->getLocation())) Java.add(Children, statement(Function->getBody()));
    } else if (const auto *Alias = dyn_cast<TypedefNameDecl>(Declaration)) sourceType(Result, Info, Alias->getUnderlyingType());
    else if (const auto *Record = dyn_cast<RecordDecl>(Declaration)) {
      sourceType(Result, Info, Context.getRecordType(Record)); Java.flag(Result, "completeDefinition", Record->isCompleteDefinition());
      if (Record->getName().empty() && Record->getTypedefNameForAnonDecl()) Java.text(Info, "name", Record->getTypedefNameForAnonDecl()->getNameAsString());
      for (const Decl *Child : Record->decls()) Java.add(Children, declaration(Child));
    } else if (const auto *Enum = dyn_cast<EnumDecl>(Declaration)) {
      sourceType(Result, Info, Context.getEnumType(Enum)); for (const auto *Enumerator : Enum->enumerators()) Java.add(Children, declaration(Enumerator));
    } else if (const auto *Enumerator = dyn_cast<EnumConstantDecl>(Declaration)) {
      Java.text(Info, "usr", declarationUsr(Enumerator)); llvm::SmallString<64> Digits; Enumerator->getInitVal().toString(Digits, 10); Java.text(Info, "constant_value", Digits.str());
      if (Enumerator->getInitExpr() && main(Enumerator->getLocation())) Java.add(Children, statement(Enumerator->getInitExpr()));
    } else if (const auto *Assertion = dyn_cast<StaticAssertDecl>(Declaration)) {
      Java.flag(Info, "assertion_failed", Assertion->isFailed());
      Java.flag(Info, "assertion_dependent", Assertion->getAssertExpr()->isValueDependent() || Assertion->getAssertExpr()->isTypeDependent());
    } else if (!isa<FieldDecl>(Declaration) && !isa<IndirectFieldDecl>(Declaration) && !isa<EmptyDecl>(Declaration) && !isa<LabelDecl>(Declaration))
      fail(std::string("unsupported compiler declaration ") + Declaration->getDeclKindName());
    Java.put(Result, "inner", Children); Java.put(Nodes, identifier(Declaration), Info);
    ActiveDeclarations.erase(Declaration); WrittenDeclarations.insert(Declaration);
    return Frame.keep(Result);
  }
  jobject statement(const Stmt *Statement) {
    LocalFrame Frame(Java.environment());
    if (!Statement) fail("required statement is null");
    jobject Result = Java.object(); Java.text(Result, "id", identifier(Statement)); Java.text(Result, "kind", Statement->getStmtClassName());
    jobject Info = location(Statement->getSourceRange()); jobject Children = Java.list();
    if (const auto *Expression = dyn_cast<Expr>(Statement)) sourceType(Result, Info, Expression->getType());
    if (const auto *Reference = dyn_cast<DeclRefExpr>(Statement)) Java.put(Result, "referencedDecl", reference(Reference->getDecl()));
    if (const auto *Member = dyn_cast<MemberExpr>(Statement)) {
      Java.text(Result, "referencedMemberDecl", identifier(Member->getMemberDecl())); Java.text(Result, "name", Member->getMemberNameInfo().getAsString()); Java.flag(Result, "isArrow", Member->isArrow());
    }
    if (const auto *Binary = dyn_cast<BinaryOperator>(Statement)) Java.text(Result, "opcode", Binary->getOpcodeStr());
    if (const auto *Unary = dyn_cast<UnaryOperator>(Statement)) { Java.text(Result, "opcode", UnaryOperator::getOpcodeStr(Unary->getOpcode())); Java.flag(Result, "isPostfix", Unary->isPostfix()); }
    if (const auto *Cast = dyn_cast<CastExpr>(Statement)) Java.text(Result, "castKind", Cast->getCastKindName());
    if (const auto *Integer = dyn_cast<IntegerLiteral>(Statement)) {
      llvm::SmallString<64> Digits; Integer->getValue().toString(Digits, 10, Integer->getType()->isSignedIntegerType()); Java.text(Result, "value", Digits.str());
    }
    if (const auto *Character = dyn_cast<CharacterLiteral>(Statement)) Java.integer(Result, "value", Character->getValue());
    if (const auto *Floating = dyn_cast<FloatingLiteral>(Statement)) {
      llvm::SmallString<64> Digits; Floating->getValue().toString(Digits); Java.text(Result, "value", Digits.str());
    }
    if (const auto *Literal = dyn_cast<StringLiteral>(Statement)) {
      std::string Spelling; llvm::raw_string_ostream Stream(Spelling); Literal->printPretty(Stream, nullptr, Context.getPrintingPolicy()); Stream.flush();
      Java.text(Info, "compiler_literal_spelling", Spelling); Java.text(Result, "value", Spelling);
      jobject String = Java.object(), Units = Java.list(); Java.integer(String, "unit_width", Literal->getCharByteWidth());
      for (unsigned Index = 0; Index < Literal->getLength(); ++Index) Java.add(Units, Java.number(Literal->getCodeUnit(Index)));
      Java.put(String, "units", Units); Java.put(Info, "string_literal", String);
    }
    if (const auto *Call = dyn_cast<CallExpr>(Statement)) if (const auto *Callee = Call->getDirectCallee()) Java.text(Info, "direct_callee_usr", declarationUsr(Callee));
    if (const auto *Jump = dyn_cast<GotoStmt>(Statement)) Java.text(Result, "targetLabelDeclId", identifier(Jump->getLabel()));
    if (const auto *Label = dyn_cast<LabelStmt>(Statement)) { Java.text(Result, "declId", identifier(Label->getDecl())); Java.text(Result, "name", Label->getName()); }
    if (const auto *Trait = dyn_cast<UnaryExprOrTypeTraitExpr>(Statement)) {
      Java.text(Result, "name", getTraitSpelling(Trait->getKind()));
      if (Trait->isArgumentType()) { jobject Type = Java.object(); Java.text(Type, "qualType", Trait->getArgumentType().getAsString(Context.getPrintingPolicy())); Java.put(Result, "argType", Type); Java.text(Info, "operand_type", typeName(Trait->getArgumentType())); }
    }
    if (const auto *Declarations = dyn_cast<DeclStmt>(Statement)) {
      for (const Decl *Declaration : Declarations->decls()) Java.add(Children, declaration(Declaration));
    } else {
      switch (Statement->getStmtClass()) {
      case Stmt::CompoundStmtClass: case Stmt::ReturnStmtClass: case Stmt::IfStmtClass: case Stmt::GotoStmtClass: case Stmt::LabelStmtClass:
      case Stmt::NullStmtClass: case Stmt::DeclRefExprClass: case Stmt::MemberExprClass: case Stmt::CallExprClass:
      case Stmt::BinaryOperatorClass: case Stmt::CompoundAssignOperatorClass: case Stmt::UnaryOperatorClass:
      case Stmt::ImplicitCastExprClass: case Stmt::CStyleCastExprClass: case Stmt::IntegerLiteralClass: case Stmt::FloatingLiteralClass:
      case Stmt::CharacterLiteralClass: case Stmt::StringLiteralClass: case Stmt::ParenExprClass: case Stmt::ArraySubscriptExprClass:
      case Stmt::ConditionalOperatorClass: case Stmt::UnaryExprOrTypeTraitExprClass:
        for (const Stmt *Child : Statement->children()) if (Child) Java.add(Children, statement(Child));
        break;
      default: fail(std::string("unsupported executable compiler syntax ") + Statement->getStmtClassName());
      }
    }
    Java.put(Result, "inner", Children); Java.put(Nodes, identifier(Statement), Info); WrittenStatements.insert(Statement);
    return Frame.keep(Result);
  }
  jobject translationUnit() {
    LocalFrame Frame(Java.environment()); jobject Root = Java.object(), Children = Java.list();
    Java.text(Root, "id", identifier(Context.getTranslationUnitDecl())); Java.text(Root, "kind", "TranslationUnitDecl");
    Java.put(Nodes, identifier(Context.getTranslationUnitDecl()), Java.object());
    for (const Decl *Declaration : Context.getTranslationUnitDecl()->decls()) Java.add(Children, declaration(Declaration));
    Java.put(Root, "inner", Children); return Frame.keep(Root);
  }
  jobject inputs() {
    LocalFrame Frame(Java.environment()); jobject Result = Java.object();
    for (auto Iterator = Manager.fileinfo_begin(); Iterator != Manager.fileinfo_end(); ++Iterator) {
      const FileEntry *File = Iterator->first; FileID Id = Manager.translateFile(File);
      if (Id.isInvalid()) fail("loaded source/header lacks compiler buffer");
      llvm::StringRef Contents = Manager.getBufferData(Id); auto Reference = Manager.getFileEntryRefForID(Id);
      if (!Reference) fail("source buffer lacks physical file entry");
      llvm::SmallString<256> Physical; auto Error = llvm::sys::fs::real_path(Reference->getName(), Physical);
      if (Error) fail("cannot canonicalize source input: " + Error.message());
      auto Hash = llvm::SHA256::hash(llvm::ArrayRef<uint8_t>(reinterpret_cast<const uint8_t *>(Contents.data()), Contents.size()));
      Java.text(Result, Physical, llvm::toHex(Hash, true));
    }
    return Frame.keep(Result);
  }
};

struct ParseState {
  JavaValues &Java; jobject Result = nullptr, Macros, Directives;
  explicit ParseState(JavaValues &Values) : Java(Values), Macros(Values.list()), Directives(Values.list()) {}
  ~ParseState() { if (Result) Java.environment()->DeleteGlobalRef(Result); }
};
class MacroMaps : public PPCallbacks {
  Preprocessor &Processor; ParseState &State;
  jobject range(SourceRange Range) {
    JavaValues &Java = State.Java; SourceManager &Manager = Processor.getSourceManager(); jobject Result = Java.object();
    if (Range.isInvalid()) fail("macro source range is invalid");
    SourceLocation Begin = Manager.getExpansionLoc(Range.getBegin()), Last = Manager.getExpansionLoc(Range.getEnd());
    if (!Begin.isFileID() || !Last.isFileID() || Manager.getFileID(Begin) != Manager.getFileID(Last)) fail("macro range crosses source files");
    unsigned Start = Manager.getFileOffset(Begin), End = Manager.getFileOffset(Lexer::getLocForEndOfToken(Last, 0, Manager, Processor.getLangOpts()));
    llvm::StringRef Buffer = Manager.getBufferData(Manager.getFileID(Begin));
    if (End < Start || End > Buffer.size()) fail("macro source range is invalid");
    Java.text(Result, "file", Manager.getFilename(Begin)); Java.integer(Result, "offset", Start); Java.integer(Result, "end", End);
    Java.integer(Result, "line", Manager.getSpellingLineNumber(Begin)); Java.integer(Result, "column", Manager.getSpellingColumnNumber(Begin));
    Java.text(Result, "code", Buffer.slice(Start, End)); Java.flag(Result, "macro", Range.getBegin().isMacroID() || Range.getEnd().isMacroID());
    SourceLocation Spelling = Manager.getSpellingLoc(Range.getBegin()); Java.text(Result, "spelling_file", Manager.getFilename(Spelling)); Java.integer(Result, "spelling_offset", Manager.getFileOffset(Spelling));
    return Result;
  }
public:
  MacroMaps(Preprocessor &Value, ParseState &Output) : Processor(Value), State(Output) {}
  void MacroExpands(const Token &Name, const MacroDefinition &Definition, SourceRange Range, const MacroArgs *) override {
    LocalFrame Frame(State.Java.environment()); auto &Java = State.Java; const MacroInfo *Macro = Definition.getMacroInfo();
    if (!Macro) fail("macro invocation lacks compiler definition");
    jobject Entry = range(Range); Java.text(Entry, "name", Processor.getSpelling(Name)); Java.flag(Entry, "function_like", Macro->isFunctionLike());
    jobject Replacement = Java.list(); for (const Token &Token : Macro->tokens()) Java.add(Replacement, Java.string(Processor.getSpelling(Token)));
    Java.put(Entry, "replacement", Replacement);
    SourceManager &Manager = Processor.getSourceManager();
    if (Macro->getDefinitionLoc().isValid() && Macro->getDefinitionEndLoc().isValid()) {
      jobject Definition = range(SourceRange(Macro->getDefinitionLoc(), Macro->getDefinitionEndLoc()));
      SourceLocation Begin = Manager.getSpellingLoc(Macro->getDefinitionLoc()); llvm::StringRef Buffer = Manager.getBufferData(Manager.getFileID(Begin));
      unsigned Offset = Manager.getFileOffset(Begin); auto Newline = Buffer.rfind('\n', Offset); unsigned Start = Newline == llvm::StringRef::npos ? 0 : Newline + 1;
      SourceLocation Last = Manager.getExpansionLoc(Macro->getDefinitionEndLoc()); unsigned End = Manager.getFileOffset(Lexer::getLocForEndOfToken(Last, 0, Manager, Processor.getLangOpts()));
      Java.text(Definition, "code", Buffer.slice(Start, End)); Java.put(Entry, "definition", Definition);
    }
    Java.add(State.Macros, Entry);
  }
  void directive(const Token &Name, llvm::StringRef Kind) {
    auto &Manager = Processor.getSourceManager(); if (!Name.getLocation().isValid() || !Manager.isWrittenInMainFile(Manager.getExpansionLoc(Name.getLocation()))) return;
    LocalFrame Frame(State.Java.environment()); jobject Entry = range(SourceRange(Name.getLocation()));
    State.Java.text(Entry, "kind", Kind); State.Java.text(Entry, "name", Processor.getSpelling(Name)); State.Java.add(State.Directives, Entry);
  }
  void MacroDefined(const Token &Name, const MacroDirective *) override { directive(Name, "define"); }
  void MacroUndefined(const Token &Name, const MacroDefinition &, const MacroDirective *) override { directive(Name, "undef"); }
};
class NativeConsumer : public ASTConsumer {
  ParseState &State;
public:
  explicit NativeConsumer(ParseState &Output) : State(Output) {}
  void HandleTranslationUnit(ASTContext &Context) override {
    if (Context.getDiagnostics().hasErrorOccurred()) return;
    LocalFrame Frame(State.Java.environment()); auto &Java = State.Java; jobject Document = Java.object(), Nodes = Java.object();
    CompilerMaps Maps(Java, Context, Nodes); jobject Ast = Maps.translationUnit();
    Java.integer(Document, "schema_version", 1); Java.flag(Document, "direct_cpg_only", true); Java.put(Document, "ast", Ast); Java.put(Document, "nodes", Nodes);
    Java.put(Document, "macros", Java.environment()->NewLocalRef(State.Macros)); Java.put(Document, "macro_directives", Java.environment()->NewLocalRef(State.Directives));
    Java.put(Document, "inputs", Maps.inputs());
    State.Result = Java.environment()->NewGlobalRef(Document); Java.check();
  }
};
class NativeAction : public ASTFrontendAction {
  ParseState &State;
public:
  explicit NativeAction(ParseState &Output) : State(Output) {}
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &Compiler, llvm::StringRef) override {
    if (Compiler.getLangOpts().CPlusPlus) fail("in-process prototype currently supports C; C++ requires a complete native mapping");
    Compiler.getPreprocessor().addPPCallbacks(std::make_unique<MacroMaps>(Compiler.getPreprocessor(), State));
    return std::make_unique<NativeConsumer>(State);
  }
};
class NativeDiagnostics : public DiagnosticConsumer {
public:
  std::string Messages;
  void HandleDiagnostic(DiagnosticsEngine::Level Level, const Diagnostic &Information) override {
    DiagnosticConsumer::HandleDiagnostic(Level, Information); llvm::SmallString<256> Text; Information.FormatDiagnostic(Text);
    if (Level >= DiagnosticsEngine::Error) Messages += Text.str().str() + "\n";
  }
};
std::string javaString(JNIEnv *Env, jstring Value) {
  if (!Value) fail("required Java string is null");
  const jchar *Characters = Env->GetStringChars(Value, nullptr); if (!Characters) fail("cannot read Java string");
  jsize Length = Env->GetStringLength(Value); const llvm::UTF16 *Begin = reinterpret_cast<const llvm::UTF16 *>(Characters), *End = Begin + Length;
  std::string Result(static_cast<size_t>(Length) * 4, '\0'); auto *Write = reinterpret_cast<llvm::UTF8 *>(Result.data()); const auto *Start = Write;
  auto Conversion = llvm::ConvertUTF16toUTF8(&Begin, End, &Write, Write + Result.size(), llvm::strictConversion);
  Env->ReleaseStringChars(Value, Characters); if (Conversion != llvm::conversionOK) fail("Java string is not valid Unicode");
  Result.resize(Write - Start); return Result;
}
}
extern "C" JNIEXPORT jobject JNICALL Java_io_joern_clang2cpg_parser_memory_ClangNative_parse0(JNIEnv *Env, jobject, jobjectArray Arguments, jstring Directory) {
  try {
    if (!Arguments || Env->GetArrayLength(Arguments) < 2) fail("compiler argv must contain compiler and source");
    JavaValues Java(Env); ParseState State(Java); std::vector<std::string> Command;
    for (jsize Index = 0; Index < Env->GetArrayLength(Arguments); ++Index) {
      auto Item = static_cast<jstring>(Env->GetObjectArrayElement(Arguments, Index)); Command.push_back(javaString(Env, Item)); Env->DeleteLocalRef(Item);
    }
    const auto WorkingDirectory = javaString(Env, Directory);
    auto SelectedCompiler = llvm::sys::findProgramByName(Command.front());
    if (!SelectedCompiler) fail("selected compiler executable is unavailable: " + Command.front());
    llvm::SmallString<256> CompilerPath;
    if (auto Error = llvm::sys::fs::real_path(*SelectedCompiler, CompilerPath))
      fail("cannot resolve selected compiler installation: " + Error.message());
    // Driver resource lookup must use the actual compiler installation, as the executable does.
    Command.front() = CompilerPath.str().str();
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem(llvm::vfs::createPhysicalFileSystem().release());
    if (auto Error = FileSystem->setCurrentWorkingDirectory(WorkingDirectory)) fail("invalid compiler working directory: " + Error.message());
    FileSystemOptions Options; Options.WorkingDir = WorkingDirectory;
    llvm::IntrusiveRefCntPtr<FileManager> Files(new FileManager(Options, FileSystem));
    NativeDiagnostics Diagnostics;
    tooling::ToolInvocation Invocation(Command, std::make_unique<NativeAction>(State), Files.get()); Invocation.setDiagnosticConsumer(&Diagnostics);
    if (!Invocation.run() || !State.Result) fail("compiler invocation failed: " + Diagnostics.Messages);
    return Env->NewLocalRef(State.Result);
  } catch (const std::exception &Error) {
    if (!Env->ExceptionCheck()) { jclass Failure = Env->FindClass("java/lang/IllegalArgumentException"); Env->ThrowNew(Failure, Error.what()); }
    return nullptr;
  }
}
