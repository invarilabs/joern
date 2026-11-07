// Experimental native compiler evidence bridge; no TLA abstractions live here.
#include <algorithm>
#include <map>
#include <set>
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/ExprCXX.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Lex/Lexer.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/SHA256.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/raw_ostream.h"
using namespace clang;
namespace {
std::string identity(const void *Pointer) { return "0x" + llvm::utohexstr(reinterpret_cast<uintptr_t>(Pointer), true); }
std::string usr(const NamedDecl *Declaration) {
  llvm::SmallString<128> Buffer;
  if (index::generateUSRForDecl(Declaration->getCanonicalDecl(), Buffer))
    llvm::report_fatal_error("Clang CPG: required declaration lacks a USR");
  return Buffer.str().str();
}
std::string typeName(QualType Type, ASTContext &Context) {
  if (Type.isNull()) llvm::report_fatal_error("Clang CPG: required type is null");
  QualType Canonical = Context.getCanonicalType(Type);
  const clang::Type *Shape = Canonical.getTypePtr();
  std::string Prefix = Canonical.isConstQualified() ? "const " : "";
  if (Canonical.isVolatileQualified()) Prefix += "volatile ";
  if (Canonical.isRestrictQualified()) Prefix += "restrict ";
  if (auto Record = dyn_cast<RecordType>(Shape)) return Prefix + "clang:" + usr(Record->getDecl());
  if (auto Enum = dyn_cast<EnumType>(Shape)) return Prefix + "clang:" + usr(Enum->getDecl());
  if (auto Reference = dyn_cast<ReferenceType>(Shape))
    return typeName(Reference->getPointeeType(), Context) + (isa<LValueReferenceType>(Shape) ? "&" : "&&");
  if (auto Member = dyn_cast<MemberPointerType>(Shape))
    return typeName(Member->getPointeeType(), Context) + " " + typeName(QualType(Member->getClass(), 0), Context) + "::*";
  if (auto Pointer = dyn_cast<PointerType>(Shape))
    return typeName(Pointer->getPointeeType(), Context) + "*" + (Prefix.empty() ? "" : " " + Prefix);
  if (auto Array = dyn_cast<ArrayType>(Shape)) {
    std::string Extent;
    if (auto Constant = dyn_cast<ConstantArrayType>(Array)) {
      llvm::SmallString<32> Digits; Constant->getSize().toString(Digits, 10, false); Extent = Digits.str().str();
    } else if (!isa<VariableArrayType>(Array) && !isa<IncompleteArrayType>(Array))
      llvm::report_fatal_error("Clang CPG: unsupported array extent constructor");
    return typeName(Array->getElementType(), Context) + "[" + Extent + "]";
  }
  if (auto Function = dyn_cast<FunctionType>(Shape)) {
    std::string Result = typeName(Function->getReturnType(), Context) + "(";
    if (auto Prototype = dyn_cast<FunctionProtoType>(Function)) {
      bool First = true;
      for (QualType Parameter : Prototype->param_types()) {
        if (!First) Result += ","; First = false; Result += typeName(Parameter, Context);
      }
      if (Prototype->isVariadic()) Result += First ? "..." : ",...";
    }
    return Result + ")";
  }
  if (isa<BuiltinType>(Shape)) return Canonical.getAsString(Context.getPrintingPolicy());
  if (auto Atomic = dyn_cast<AtomicType>(Shape)) return "_Atomic(" + typeName(Atomic->getValueType(), Context) + ")";
  if (auto Complex = dyn_cast<ComplexType>(Shape)) return "_Complex " + typeName(Complex->getElementType(), Context);
  if (auto Vector = dyn_cast<VectorType>(Shape)) return "vector[" + std::to_string(Vector->getNumElements()) + "](" + typeName(Vector->getElementType(), Context) + ")";
  llvm::report_fatal_error(llvm::Twine("Clang CPG: unsupported canonical type ") + Shape->getTypeClassName());
}
// Semantic types belong to the compiler frontend, not a consumer type parser.
llvm::json::Object semanticType(QualType Type, ASTContext &Context) {
  if (Type.isNull()) llvm::report_fatal_error("Clang CPG: semantic type is missing");
  QualType Canonical = Context.getCanonicalType(Type);
  const clang::Type *Shape = Canonical.getTypePtr();
  llvm::json::Object Result;
  Result["canonical_identity"] = typeName(Canonical, Context);
  Result["value_spelling"] = Canonical.getAsString(Context.getPrintingPolicy());
  llvm::json::Array Qualifiers;
  if (Canonical.isConstQualified()) Qualifiers.push_back("const");
  if (Canonical.isVolatileQualified()) Qualifiers.push_back("volatile");
  if (Canonical.isRestrictQualified()) Qualifiers.push_back("restrict");
  Result["qualifiers"] = std::move(Qualifiers);
  if (auto Reference = dyn_cast<ReferenceType>(Shape)) {
    Result["kind"] = isa<LValueReferenceType>(Shape) ? "LValueReference" : "RValueReference";
    Result["pointee"] = semanticType(Reference->getPointeeType(), Context);
  } else if (auto Member = dyn_cast<MemberPointerType>(Shape)) {
    Result["kind"] = "MemberPointer";
    Result["pointee"] = semanticType(Member->getPointeeType(), Context);
    Result["owner"] = semanticType(QualType(Member->getClass(), 0), Context);
  } else if (auto Pointer = dyn_cast<PointerType>(Shape)) {
    Result["kind"] = "Pointer";
    Result["pointee"] = semanticType(Pointer->getPointeeType(), Context);
  } else if (auto Function = dyn_cast<FunctionType>(Shape)) {
    Result["result"] = semanticType(Function->getReturnType(), Context);
    llvm::json::Array Arguments;
    if (auto Prototype = dyn_cast<FunctionProtoType>(Function)) {
      Result["kind"] = "FunctionProto"; Result["variadic"] = Prototype->isVariadic();
      for (auto Argument : Prototype->param_types()) Arguments.push_back(semanticType(Argument, Context));
    } else { Result["kind"] = "FunctionNoProto"; Result["variadic"] = false; }
    Result["arguments"] = std::move(Arguments);
  } else if (auto Record = dyn_cast<RecordType>(Shape)) {
    auto Declaration = Record->getDecl();
    Result["kind"] = "Record"; Result["declaration_usr"] = usr(Declaration);
    auto DisplayName = Declaration->getNameAsString();
    if (DisplayName.empty() && Declaration->getTypedefNameForAnonDecl())
      DisplayName = Declaration->getTypedefNameForAnonDecl()->getNameAsString();
    Result["declaration_name"] = DisplayName;
    Result["tag"] = Declaration->isUnion() ? "union" : "struct";
    Result["complete"] = Declaration->getDefinition() != nullptr;
  } else if (auto Enum = dyn_cast<EnumType>(Shape)) {
    Result["kind"] = "Enum"; Result["declaration_usr"] = usr(Enum->getDecl());
    Result["declaration_name"] = Enum->getDecl()->getNameAsString();
  } else if (auto Array = dyn_cast<ArrayType>(Shape)) {
    Result["element"] = semanticType(Array->getElementType(), Context);
    if (auto Constant = dyn_cast<ConstantArrayType>(Array)) {
      if (Constant->getSize().getActiveBits() > 63) llvm::report_fatal_error("Clang CPG: semantic array extent exceeds integer range");
      Result["kind"] = "ConstantArray"; Result["extent"] = static_cast<int64_t>(Constant->getSize().getZExtValue());
    } else if (isa<IncompleteArrayType>(Array)) Result["kind"] = "IncompleteArray";
    else if (isa<VariableArrayType>(Array)) Result["kind"] = "VariableArray";
    else llvm::report_fatal_error("Clang CPG: unsupported semantic array constructor");
  } else if (auto Atomic = dyn_cast<AtomicType>(Shape)) {
    Result["kind"] = "Atomic"; Result["value"] = semanticType(Atomic->getValueType(), Context);
  } else if (auto Complex = dyn_cast<ComplexType>(Shape)) {
    Result["kind"] = "Complex"; Result["element"] = semanticType(Complex->getElementType(), Context);
  } else if (auto Vector = dyn_cast<VectorType>(Shape)) {
    Result["kind"] = "Vector"; Result["element"] = semanticType(Vector->getElementType(), Context);
    Result["elements"] = Vector->getNumElements();
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
      default: llvm::report_fatal_error(llvm::Twine("Clang CPG: unsupported target-specific builtin semantic type ") +
                                        std::to_string(static_cast<unsigned>(Builtin->getKind())) + " (" +
                                        Canonical.getAsString(Context.getPrintingPolicy()) + ")");
    }
    if (Kind.empty()) llvm::report_fatal_error("Clang CPG: unknown native builtin type kind");
    Result["kind"] = Kind;
  } else llvm::report_fatal_error(llvm::Twine("Clang CPG: missing semantic type constructor ") + Shape->getTypeClassName());
  return Result;
}
struct Evidence {
  llvm::json::Object Nodes;
  llvm::json::Array Macros, Tokens, Directives, ConditionalDirectives, Comments;
  bool IncludeComments = false, LogPreprocessor = false;
  std::string TokenDump;
};
llvm::json::Object location(SourceRange Range, SourceManager &Manager, const LangOptions &Language) {
  llvm::json::Object Result;
  if (Range.isInvalid()) { Result["generated"] = true; return Result; }
  SourceLocation Begin = Manager.getExpansionLoc(Range.getBegin());
  SourceLocation Last = Manager.getExpansionLoc(Range.getEnd());
  if (!Begin.isFileID() || !Last.isFileID() || Manager.getFileID(Begin) != Manager.getFileID(Last))
    llvm::report_fatal_error("Clang CPG: source range crosses physical expansion files");
  unsigned Start = Manager.getFileOffset(Begin);
  unsigned End = Manager.getFileOffset(Lexer::getLocForEndOfToken(Last, 0, Manager, Language));
  llvm::StringRef Buffer = Manager.getBufferData(Manager.getFileID(Begin));
  if (End < Start || End > Buffer.size()) llvm::report_fatal_error("Clang CPG: invalid source range");
  Result["file"] = Manager.getFilename(Begin).str();
  Result["offset"] = Start; Result["end"] = End;
  Result["line"] = Manager.getSpellingLineNumber(Begin); Result["column"] = Manager.getSpellingColumnNumber(Begin);
  Result["code"] = Buffer.slice(Start, End).str();
  Result["macro"] = Range.getBegin().isMacroID() || Range.getEnd().isMacroID();
  SourceLocation Spelling = Manager.getSpellingLoc(Range.getBegin());
  Result["spelling_file"] = Manager.getFilename(Spelling).str();
  Result["spelling_offset"] = Manager.getFileOffset(Spelling);
  SourceLocation SpellingLast = Manager.getSpellingLoc(Range.getEnd());
  if (Spelling.isFileID() && SpellingLast.isFileID() && Manager.getFileID(Spelling) == Manager.getFileID(SpellingLast)) {
    unsigned First = Manager.getFileOffset(Spelling);
    unsigned Finish = Manager.getFileOffset(Lexer::getLocForEndOfToken(SpellingLast, 0, Manager, Language));
    llvm::StringRef Physical = Manager.getBufferData(Manager.getFileID(Spelling));
    if (First <= Finish && Finish <= Physical.size()) Result["spelling_code"] = Physical.slice(First, Finish).str();
  }
  return Result;
}
#include "ClangEvidenceGraph.h"
class MacroEvidence : public PPCallbacks {
  Preprocessor &PreprocessorRef; std::shared_ptr<Evidence> Output;
public:
  MacroEvidence(Preprocessor &Value, std::shared_ptr<Evidence> Destination) : PreprocessorRef(Value), Output(std::move(Destination)) {}
  void directive(const Token &Name, llvm::StringRef Kind) {
    auto &Manager = PreprocessorRef.getSourceManager();
    if (Name.getLocation().isInvalid() || !Manager.isWrittenInMainFile(Manager.getExpansionLoc(Name.getLocation()))) return;
    auto Entry = location(SourceRange(Name.getLocation()), Manager, PreprocessorRef.getLangOpts());
    Entry["kind"] = Kind.str(); Entry["name"] = PreprocessorRef.getSpelling(Name);
    if (Output->LogPreprocessor)
      llvm::errs() << *Entry.getString("file") << ":" << *Entry.getInteger("line")
                   << ": #" << Kind << " " << *Entry.getString("name") << "\n";
    Output->Directives.push_back(std::move(Entry));
  }
  void conditional(SourceLocation Position, SourceRange Condition, llvm::StringRef Kind) {
    auto &Manager = PreprocessorRef.getSourceManager();
    if (Position.isInvalid() || !Manager.isWrittenInMainFile(Manager.getExpansionLoc(Position))) return;
    auto Entry = location(Condition, Manager, PreprocessorRef.getLangOpts());
    Entry["kind"] = Kind.str();
    const std::string ConditionText = Entry.getString("code")->str();
    Entry["condition"] = ConditionText;
    Entry["line"] = Manager.getExpansionLineNumber(Position);
    if (Output->LogPreprocessor)
      llvm::errs() << *Entry.getString("file") << ":" << *Entry.getInteger("line")
                   << ": #" << Kind << " " << *Entry.getString("condition") << "\n";
    Output->ConditionalDirectives.push_back(std::move(Entry));
  }
  void If(SourceLocation Position, SourceRange Condition, ConditionValueKind) override {
    conditional(Position, Condition, "if");
  }
  void Elif(SourceLocation Position, SourceRange Condition, ConditionValueKind, SourceLocation) override {
    conditional(Position, Condition, "elif");
  }
  void Ifdef(SourceLocation Position, const Token &Name, const MacroDefinition &) override {
    conditional(Position, SourceRange(Name.getLocation()), "ifdef");
  }
  void Ifndef(SourceLocation Position, const Token &Name, const MacroDefinition &) override {
    conditional(Position, SourceRange(Name.getLocation()), "ifndef");
  }
  void MacroDefined(const Token &Name, const MacroDirective *) override { directive(Name, "define"); }
  void MacroUndefined(const Token &Name, const MacroDefinition &, const MacroDirective *) override { directive(Name, "undef"); }
  void MacroExpands(const Token &Name, const MacroDefinition &Definition, SourceRange Range, const MacroArgs *) override {
    auto Entry = location(Range, PreprocessorRef.getSourceManager(), PreprocessorRef.getLangOpts());
    Entry["name"] = PreprocessorRef.getSpelling(Name);
    const auto *Macro = Definition.getMacroInfo();
    if (!Macro) llvm::report_fatal_error("Clang CPG: macro expansion lacks its compiler definition");
    Entry["function_like"] = Macro->isFunctionLike();
    llvm::json::Array Replacement;
    if (const MacroInfo *Information = Definition.getMacroInfo()) {
      for (const Token &Part : Information->tokens()) Replacement.push_back(PreprocessorRef.getSpelling(Part));
      SourceManager &Manager = PreprocessorRef.getSourceManager();
      if (Information->getDefinitionLoc().isValid() && !Manager.isInSystemMacro(Information->getDefinitionLoc())) {
        auto Declaration = location(SourceRange(Information->getDefinitionLoc(), Information->getDefinitionEndLoc()), Manager, PreprocessorRef.getLangOpts());
        SourceLocation Begin = Manager.getSpellingLoc(Information->getDefinitionLoc());
        llvm::StringRef Buffer = Manager.getBufferData(Manager.getFileID(Begin));
        unsigned Offset = Manager.getFileOffset(Begin);
        auto Newline = Buffer.rfind('\n', Offset);
        unsigned LineStart = Newline == llvm::StringRef::npos ? 0 : Newline + 1;
        Declaration["code"] = Buffer.slice(LineStart, *Declaration.getInteger("end")).str();
        Entry["definition"] = std::move(Declaration);
      }
    }
    Entry["replacement"] = std::move(Replacement); Output->Macros.push_back(std::move(Entry));
  }
};
class CommentEvidence : public CommentHandler {
  std::shared_ptr<Evidence> Output;
public:
  explicit CommentEvidence(std::shared_ptr<Evidence> Destination) : Output(std::move(Destination)) {}
  bool HandleComment(Preprocessor &Processor, SourceRange Range) override {
    auto &Manager = Processor.getSourceManager();
    if (!Manager.isWrittenInMainFile(Range.getBegin())) return false;
    unsigned Begin = Manager.getFileOffset(Range.getBegin());
    unsigned End = Manager.getFileOffset(Range.getEnd());
    auto Buffer = Manager.getBufferData(Manager.getFileID(Range.getBegin()));
    if (End < Begin || End > Buffer.size()) llvm::report_fatal_error("Clang CPG: invalid comment range");
    Output->Comments.push_back(llvm::json::Object{
      {"file", Manager.getFilename(Range.getBegin()).str()}, {"offset", Begin}, {"end", End},
      {"line", Manager.getSpellingLineNumber(Range.getBegin())},
      {"column", Manager.getSpellingColumnNumber(Range.getBegin())},
      {"code", Buffer.slice(Begin, End).str()}});
    return false;
  }
};
class Visitor : public RecursiveASTVisitor<Visitor> {
  ASTContext &Context; Evidence &Output;
public:
  Visitor(ASTContext &Value, Evidence &Destination) : Context(Value), Output(Destination) {}
  bool shouldVisitImplicitCode() const { return true; }
  bool VisitDecl(Decl *Declaration) {
    auto Entry = location(Declaration->getSourceRange(), Context.getSourceManager(), Context.getLangOpts());
    Entry["canonical"] = identity(Declaration->getCanonicalDecl());
    if (auto Named = dyn_cast<NamedDecl>(Declaration)) Entry["name"] = Named->getNameAsString();
    if (auto Value = dyn_cast<ValueDecl>(Declaration)) {
      Entry["type"] = typeName(Value->getType(), Context);
      Entry["semantic_type"] = semanticType(Value->getType(), Context);
    }
    if (auto Variable = dyn_cast<VarDecl>(Declaration)) {
      Entry["file_scope"] = Variable->isFileVarDecl();
      Entry["has_initializer"] = Variable->hasInit();
      Entry["storage_duration"] = Variable->getTLSKind() != VarDecl::TLS_None ? "thread" : Variable->hasGlobalStorage() ? "static" : "automatic";
      Entry["initializer_kind"] = Variable->hasInit() ? "explicit" :
        Variable->hasGlobalStorage() && Variable->isThisDeclarationADefinition() != VarDecl::DeclarationOnly ? "implicit_zero" :
        Variable->isFileVarDecl() ? "external" : "uninitialized";
      if (!isa<ParmVarDecl>(Variable) && (Variable->isFileVarDecl() ||
          Context.getSourceManager().isWrittenInMainFile(Context.getSourceManager().getExpansionLoc(Variable->getLocation()))))
        Entry["usr"] = usr(Variable);
    }
    if (auto Enumerator = dyn_cast<EnumConstantDecl>(Declaration)) {
      Entry["usr"] = usr(Enumerator);
      llvm::SmallString<64> Digits; Enumerator->getInitVal().toString(Digits, 10);
      Entry["constant_value"] = Digits.str().str();
    }
    if (auto Alias = dyn_cast<TypedefNameDecl>(Declaration)) {
      Entry["type"] = typeName(Alias->getUnderlyingType(), Context);
      Entry["semantic_type"] = semanticType(Alias->getUnderlyingType(), Context);
    }
    if (auto Record = dyn_cast<RecordDecl>(Declaration)) {
      Entry["type"] = "clang:" + usr(Record);
      Entry["semantic_type"] = semanticType(Context.getRecordType(Record), Context);
      if (Record->getName().empty() && Record->getTypedefNameForAnonDecl())
        Entry["name"] = Record->getTypedefNameForAnonDecl()->getNameAsString();
    }
    if (auto Enum = dyn_cast<EnumDecl>(Declaration)) {
      Entry["type"] = "clang:" + usr(Enum);
      Entry["semantic_type"] = semanticType(Context.getEnumType(Enum), Context);
    }
    if (auto Function = dyn_cast<FunctionDecl>(Declaration)) {
      Entry["usr"] = usr(Function);
      Entry["result_type"] = typeName(Function->getReturnType(), Context);
      Entry["result_spelling"] = Function->getReturnType().getAsString(Context.getPrintingPolicy());
      Entry["variadic"] = Function->isVariadic(); Entry["has_body"] = Function->doesThisDeclarationHaveABody();
    }
    if (auto Method = dyn_cast<CXXMethodDecl>(Declaration)) {
      Entry["owner_usr"] = usr(Method->getParent());
      Entry["owner_type"] = semanticType(Context.getRecordType(Method->getParent()), Context);
      Entry["qualified_name"] = Method->getQualifiedNameAsString();
      Entry["static_method"] = Method->isStatic();
      Entry["const_method"] = Method->isConst();
      Entry["virtual_method"] = Method->isVirtual();
      if (!Method->isStatic()) {
        Entry["this_type"] = semanticType(Method->getThisType(), Context);
        Entry["this_spelling"] = Method->getThisType().getAsString(Context.getPrintingPolicy());
      }
    }
    if (auto Parameter = dyn_cast<ParmVarDecl>(Declaration)) Entry["original_type"] = typeName(Parameter->getOriginalType(), Context);
    Output.Nodes[identity(Declaration)] = std::move(Entry); return true;
  }
  bool VisitFunctionDecl(FunctionDecl *Function) {
    // Parameters created from a typeof function type have no written TypeLoc.
    for (ParmVarDecl *Parameter : Function->parameters()) VisitDecl(Parameter);
    return true;
  }
  bool VisitInitListExpr(InitListExpr *Initializer) {
    if (!Context.getSourceManager().isWrittenInMainFile(Context.getSourceManager().getExpansionLoc(Initializer->getBeginLoc()))) return true;
    if (auto *Filler = Initializer->getArrayFiller()) {
      auto Entry = location(Filler->getSourceRange(), Context.getSourceManager(), Context.getLangOpts());
      Entry["type"] = typeName(Filler->getType(), Context);
      Entry["semantic_type"] = semanticType(Filler->getType(), Context);
      Output.Nodes[identity(Filler)] = std::move(Entry);
    }
    return true;
  }
  bool VisitStmt(Stmt *Statement) {
    if (!isa<ImplicitValueInitExpr>(Statement) && !Context.getSourceManager().isWrittenInMainFile(Context.getSourceManager().getExpansionLoc(Statement->getBeginLoc()))) return true;
    auto Entry = location(Statement->getSourceRange(), Context.getSourceManager(), Context.getLangOpts());
    if (auto Expression = dyn_cast<Expr>(Statement)) {
      Entry["type"] = typeName(Expression->getType(), Context);
      Entry["semantic_type"] = semanticType(Expression->getType(), Context);
    }
    if (auto Call = dyn_cast<CXXMemberCallExpr>(Statement)) {
      const auto *Method = Call->getMethodDecl();
      if (!Method) llvm::report_fatal_error("Clang CPG: member call lacks a resolved method declaration");
      Entry["direct_callee_usr"] = usr(Method);
    }
    if (auto Construction = dyn_cast<CXXConstructExpr>(Statement)) {
      const auto *Constructor = Construction->getConstructor();
      if (!Constructor) llvm::report_fatal_error("Clang CPG: construction lacks a resolved constructor declaration");
      Entry["constructor_usr"] = usr(Constructor);
    }
    if (auto Literal = dyn_cast<StringLiteral>(Statement)) {
      llvm::json::Array Units;
      for (unsigned Index = 0; Index < Literal->getLength(); ++Index)
        Units.push_back(static_cast<int64_t>(Literal->getCodeUnit(Index)));
      Entry["string_literal"] = llvm::json::Object{{"unit_width", Literal->getCharByteWidth()}, {"units", std::move(Units)}};
    }
    if (auto Trait = dyn_cast<UnaryExprOrTypeTraitExpr>(Statement))
      if (Trait->isArgumentType()) Entry["operand_type"] = typeName(Trait->getArgumentType(), Context);
    Output.Nodes[identity(Statement)] = std::move(Entry); return true;
  }
};
class Consumer : public ASTConsumer {
  std::shared_ptr<Evidence> Output; std::string Destination; Preprocessor &Processor;
  std::unique_ptr<CommentEvidence> Comments;
public:
  Consumer(std::shared_ptr<Evidence> Data, std::string Path, Preprocessor &Input)
      : Output(std::move(Data)), Destination(std::move(Path)), Processor(Input) {
    if (Output->IncludeComments) {
      Comments = std::make_unique<CommentEvidence>(Output);
      Processor.addCommentHandler(Comments.get());
    }
  }
  ~Consumer() override {
    if (Comments) Processor.removeCommentHandler(Comments.get());
  }
  void HandleTranslationUnit(ASTContext &Context) override {
    if (Context.getDiagnostics().hasErrorOccurred()) return;
    Visitor Collector(Context, *Output); Collector.TraverseDecl(Context.getTranslationUnitDecl());
    llvm::json::Object Inputs;
    SourceManager &Manager = Context.getSourceManager();
    for (auto Iterator = Manager.fileinfo_begin(); Iterator != Manager.fileinfo_end(); ++Iterator) {
      const FileEntry *File = Iterator->first; FileID Identifier = Manager.translateFile(File);
      if (Identifier.isInvalid()) llvm::report_fatal_error("Clang CPG: loaded header has no source buffer");
      llvm::StringRef Contents = Manager.getBufferData(Identifier);
      auto Hash = llvm::SHA256::hash(llvm::ArrayRef<uint8_t>(reinterpret_cast<const uint8_t *>(Contents.data()), Contents.size()));
      auto Reference = Manager.getFileEntryRefForID(Identifier);
      if (!Reference) llvm::report_fatal_error("Clang CPG: file buffer lacks its physical file entry");
      llvm::SmallString<256> PhysicalPath;
      auto PathError = llvm::sys::fs::real_path(Reference->getName(), PhysicalPath);
      if (PathError) llvm::report_fatal_error(llvm::Twine("Clang CPG: cannot canonicalize source input ") + Reference->getName() + ": " + PathError.message());
      auto Digest = llvm::toHex(Hash, true);
      if (auto Previous = Inputs.getString(PhysicalPath))
        if (*Previous != Digest) llvm::report_fatal_error("Clang CPG: canonical source input has conflicting buffer hashes");
      Inputs[PhysicalPath.str().str()] = Digest;
    }
    std::vector<std::string> Names;
    for (const auto &Entry : Processor.macros())
      if (Processor.getMacroInfo(Entry.first)) Names.push_back(Entry.first->getName().str());
    std::sort(Names.begin(), Names.end());
    llvm::json::Array MacroNames;
    for (const auto &Name : Names) MacroNames.push_back(Name);
    std::error_code TokenError; llvm::raw_fd_ostream TokenStream(Destination + ".tokens", TokenError);
    if (TokenError) llvm::report_fatal_error("Clang CPG: cannot write compiler token evidence");
    TokenStream << Output->TokenDump;
    std::error_code Error; llvm::raw_fd_ostream Stream(Destination, Error);
    if (Error) llvm::report_fatal_error(llvm::Twine("Clang CPG: cannot write evidence: ") + Error.message());
    Stream << "{\"schema_version\":1,\"ast\":";
    Context.getTranslationUnitDecl()->dump(Stream, false, ADOF_JSON);
    Stream << ",\"compiler_graph\":" << llvm::formatv("{0}", llvm::json::Value(NativeEvidenceGraph(Context).capture()));
    Stream << ",\"nodes\":" << llvm::formatv("{0}", llvm::json::Value(std::move(Output->Nodes)));
    Stream << ",\"macro_directives\":" << llvm::formatv("{0}", llvm::json::Value(std::move(Output->Directives)));
    Stream << ",\"conditional_directives\":" << llvm::formatv("{0}", llvm::json::Value(std::move(Output->ConditionalDirectives)));
    Stream << ",\"comments\":" << llvm::formatv("{0}", llvm::json::Value(std::move(Output->Comments)));
    Stream << ",\"macros\":" << llvm::formatv("{0}", llvm::json::Value(std::move(Output->Macros)));
    Stream << ",\"tokens\":" << llvm::formatv("{0}", llvm::json::Value(std::move(Output->Tokens)));
    Stream << ",\"predefines\":" << llvm::formatv("{0}", llvm::json::Value(Processor.getPredefines()));
    Stream << ",\"macro_names\":" << llvm::formatv("{0}", llvm::json::Value(std::move(MacroNames)));
    Stream << ",\"inputs\":" << llvm::formatv("{0}", llvm::json::Value(std::move(Inputs))) << "}\n";
  }
};
class Action : public PluginASTAction {
  std::shared_ptr<Evidence> Output = std::make_shared<Evidence>(); std::string Destination;
  bool ParseArgs(const CompilerInstance &, const std::vector<std::string> &Arguments) override {
    if (Arguments.empty() || Arguments[0].empty()) return false;
    Destination = Arguments[0];
    for (size_t Index = 1; Index < Arguments.size(); ++Index) {
      if (Arguments[Index] == "include-comments") Output->IncludeComments = true;
      else if (Arguments[Index] == "log-preprocessor") Output->LogPreprocessor = true;
      else { llvm::errs() << "Clang CPG: unsupported plugin argument: " << Arguments[Index] << "\n"; return false; }
    }
    return true;
  }
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &Compiler, llvm::StringRef) override {
    Preprocessor &Processor = Compiler.getPreprocessor();
    Processor.addPPCallbacks(std::make_unique<MacroEvidence>(Processor, Output));
    Processor.setTokenWatcher([Data = Output, &Processor](const Token &Part) {
      if (Part.isAnnotation()) return;
      // Use Clang's token spelling/location APIs for the existing strict token
      // transport. This shares the parser's token stream, not another parse.
      llvm::raw_string_ostream Dump(Data->TokenDump);
      Dump << Part.getName() << " '" << Processor.getSpelling(Part) << "'\t";
      if (Part.isAtStartOfLine()) Dump << " [StartOfLine]";
      if (Part.hasLeadingSpace()) Dump << " [LeadingSpace]";
      if (Part.isExpandDisabled()) Dump << " [ExpandDisabled]";
      if (Part.needsCleaning()) Dump << " [UnClean='" << llvm::StringRef(Processor.getSourceManager().getCharacterData(Part.getLocation()), Part.getLength()) << "']";
      Dump << "\tLoc=<"; Part.getLocation().print(Dump, Processor.getSourceManager()); Dump << ">\n";
      if (Part.is(tok::eof)) { Data->Tokens.push_back(llvm::json::Object{{"kind", "eof"}}); return; }
      auto Entry = location(SourceRange(Part.getLocation()), Processor.getSourceManager(), Processor.getLangOpts());
      Entry["kind"] = Part.getName(); Entry["text"] = Processor.getSpelling(Part);
      Data->Tokens.push_back(std::move(Entry));
    });
    return std::make_unique<Consumer>(Output, Destination, Processor);
  }
  ActionType getActionType() override { return AddBeforeMainAction; }
};
}
static FrontendPluginRegistry::Add<Action> Registration("clang-cpg", "native Clang-to-CPG compiler evidence");
