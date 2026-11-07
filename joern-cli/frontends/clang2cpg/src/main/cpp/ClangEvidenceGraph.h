// Native AST facts captured during the frontend parse. No CIndex or second parse.
class NativeEvidenceGraph : public RecursiveASTVisitor<NativeEvidenceGraph> {
  ASTContext &Context;
  llvm::json::Object Nodes, Types;
  std::vector<std::string> Parents;
  std::vector<const Decl *> Owners;
  std::map<std::string, std::vector<std::string>> Children;
  std::set<std::string> SeenNodes, SeenTypes;

  std::string typeId(QualType Type) {
    if (Type.isNull()) return "";
    std::string Key = identity(Type.getAsOpaquePtr());
    if (!SeenTypes.insert(Key).second) return Key;
    const clang::Type *Shape = Type.getTypePtr();
    llvm::json::Object Entry;
    Entry["spelling"] = Type.getAsString(Context.getPrintingPolicy());
    Entry["canonical"] = typeId(Context.getCanonicalType(Type));
    Entry["unqualified"] = typeId(Type.getLocalUnqualifiedType());
    llvm::json::Array Qualifiers;
    if (Type.isConstQualified()) Qualifiers.push_back("const");
    if (Type.isVolatileQualified()) Qualifiers.push_back("volatile");
    if (Type.isRestrictQualified()) Qualifiers.push_back("restrict");
    Entry["qualifiers"] = std::move(Qualifiers);
    if (auto Parenthesized = dyn_cast<ParenType>(Shape)) {
      Entry["kind"] = "Paren";
      Entry["underlying"] = typeId(Context.getQualifiedType(Parenthesized->getInnerType(), Type.getLocalQualifiers()));
    } else if (auto Alias = dyn_cast<TypedefType>(Shape)) {
      Entry["kind"] = "Typedef"; Entry["declaration"] = identity(Alias->getDecl());
    } else if (auto Elaborated = dyn_cast<ElaboratedType>(Shape)) {
      Entry["kind"] = "Elaborated"; Entry["named"] = typeId(Elaborated->getNamedType());
    } else if (auto Pointer = dyn_cast<PointerType>(Shape)) {
      Entry["kind"] = "Pointer"; Entry["pointee"] = typeId(Pointer->getPointeeType());
    } else if (auto Array = dyn_cast<ArrayType>(Shape)) {
      Entry["element"] = typeId(Array->getElementType());
      if (auto Constant = dyn_cast<ConstantArrayType>(Array)) {
        Entry["kind"] = "ConstantArray";
        if (Constant->getSize().getActiveBits() > 63) llvm::report_fatal_error("Clang CPG: native array extent exceeds evidence integer range");
        Entry["extent"] = static_cast<int64_t>(Constant->getSize().getZExtValue());
      } else if (isa<VariableArrayType>(Array)) { Entry["kind"] = "VariableArray"; Entry["extent"] = -1; }
      else if (isa<IncompleteArrayType>(Array)) { Entry["kind"] = "IncompleteArray"; Entry["extent"] = -1; }
      else Entry["kind"] = std::string("Unsupported:") + Shape->getTypeClassName();
    } else if (auto Function = dyn_cast<FunctionType>(Shape)) {
      Entry["result"] = typeId(Function->getReturnType());
      llvm::json::Array Arguments;
      if (auto Prototype = dyn_cast<FunctionProtoType>(Function)) {
        Entry["kind"] = "FunctionProto"; Entry["variadic"] = Prototype->isVariadic();
        for (QualType Parameter : Prototype->param_types()) Arguments.push_back(typeId(Parameter));
      } else { Entry["kind"] = "FunctionNoProto"; Entry["variadic"] = false; }
      Entry["arguments"] = std::move(Arguments);
    } else if (auto Record = dyn_cast<RecordType>(Shape)) {
      Entry["kind"] = "Record"; Entry["declaration"] = identity(Record->getDecl());
    } else if (auto Enum = dyn_cast<EnumType>(Shape)) {
      Entry["kind"] = "Enum"; Entry["declaration"] = identity(Enum->getDecl());
    } else if (auto Builtin = dyn_cast<BuiltinType>(Shape)) {
      // These are Clang's native builtin constructors, not parsed type spellings.
      std::string Kind = Builtin->getName(Context.getPrintingPolicy()).str();
      switch (Builtin->getKind()) {
#define NATIVE_BUILTIN(NAME) case BuiltinType::NAME: Kind = #NAME; break
        NATIVE_BUILTIN(Void); NATIVE_BUILTIN(Bool); NATIVE_BUILTIN(Char_S); NATIVE_BUILTIN(Char_U);
        NATIVE_BUILTIN(SChar); NATIVE_BUILTIN(UChar); NATIVE_BUILTIN(Short); NATIVE_BUILTIN(UShort);
        NATIVE_BUILTIN(Int); NATIVE_BUILTIN(UInt); NATIVE_BUILTIN(Long); NATIVE_BUILTIN(ULong);
        NATIVE_BUILTIN(LongLong); NATIVE_BUILTIN(ULongLong); NATIVE_BUILTIN(Int128); NATIVE_BUILTIN(UInt128);
        NATIVE_BUILTIN(Float); NATIVE_BUILTIN(Double); NATIVE_BUILTIN(LongDouble);
#undef NATIVE_BUILTIN
        default: Kind = std::string("Unsupported:Builtin:") + Kind; break;
      }
      Entry["kind"] = Kind;
    } else {
      // Preserve an explicit constructor; consumers must request support rather
      // than receiving a guessed builtin/pointer shape.
      Entry["kind"] = std::string("Unsupported:") + Shape->getTypeClassName();
    }
    Types[Key] = std::move(Entry); return Key;
  }
  llvm::json::Value point(SourceLocation Location) {
    if (Location.isInvalid()) return nullptr;
    SourceManager &Manager = Context.getSourceManager();
    SourceLocation Expansion = Manager.getExpansionLoc(Location), Spelling = Manager.getSpellingLoc(Location);
    if (!Expansion.isFileID() || !Spelling.isFileID()) llvm::report_fatal_error("Clang CPG: native point is not a file location");
    return llvm::json::Object{{"file", Manager.getFilename(Expansion).str()}, {"offset", Manager.getFileOffset(Expansion)},
      {"line", Manager.getExpansionLineNumber(Location)}, {"column", Manager.getExpansionColumnNumber(Location)},
      {"spelling_line", Manager.getSpellingLineNumber(Spelling)}, {"spelling_column", Manager.getSpellingColumnNumber(Spelling)},
      {"spelling_file", Manager.getFilename(Spelling).str()}, {"spelling_offset", Manager.getFileOffset(Spelling)}};
  }
  llvm::json::Object base(std::string Kind, SourceLocation Location, SourceRange Range) {
    llvm::json::Object Entry{{"kind", Kind}, {"location", point(Location)}};
    SourceManager &Manager = Context.getSourceManager();
    Entry["begin"] = point(Range.getBegin());
    Entry["end"] = point(Range.isValid() ? Lexer::getLocForEndOfToken(Range.getEnd(), 0, Manager, Context.getLangOpts()) : SourceLocation());
    return Entry;
  }
  void link(std::string Key) {
    if (!Parents.empty()) {
      auto &List = Children[Parents.back()];
      if (std::find(List.begin(), List.end(), Key) == List.end()) List.push_back(Key);
    }
  }
public:
  explicit NativeEvidenceGraph(ASTContext &Input) : Context(Input) {}
  bool shouldVisitImplicitCode() const { return true; }
  bool TraverseDecl(Decl *Declaration) {
    if (!Declaration) return true;
    std::string Key = identity(Declaration); link(Key);
    if (!SeenNodes.insert(Key).second) return true;
    std::string Kind = Declaration->getDeclKindName();
    Kind += "Decl";
    if (isa<TranslationUnitDecl>(Declaration)) Kind = "TranslationUnit";
    if (isa<ParmVarDecl>(Declaration)) Kind = "ParmDecl";
    if (auto Record = dyn_cast<RecordDecl>(Declaration)) Kind = Record->isUnion() ? "UnionDecl" : "StructDecl";
    auto Entry = base(Kind, Declaration->getLocation(), Declaration->getSourceRange());
    Entry["canonical"] = identity(Declaration->getCanonicalDecl());
    Entry["semantic_parent"] = Declaration->getDeclContext() ? llvm::json::Value(identity(Decl::castFromDeclContext(Declaration->getDeclContext()))) : llvm::json::Value(nullptr);
    Entry["spelling"] = ""; Entry["usr"] = ""; Entry["type"] = nullptr; Entry["is_expression"] = false;
    if (auto Named = dyn_cast<NamedDecl>(Declaration)) {
      Entry["spelling"] = Named->getNameAsString();
      llvm::SmallString<128> Buffer;
      if (!index::generateUSRForDecl(Named->getCanonicalDecl(), Buffer)) Entry["usr"] = Buffer.str().str();
      else Entry["usr_unavailable"] = true;
    }
    if (auto Value = dyn_cast<ValueDecl>(Declaration)) Entry["type"] = typeId(Value->getType());
    if (auto Alias = dyn_cast<TypedefNameDecl>(Declaration)) {
      Entry["type"] = typeId(Context.getTypedefType(Alias)); Entry["underlying"] = typeId(Alias->getUnderlyingType());
    }
    if (auto Record = dyn_cast<RecordDecl>(Declaration)) {
      Entry["type"] = typeId(Context.getRecordType(Record)); Entry["anonymous"] = Record->isAnonymousStructOrUnion();
      Entry["definition"] = Record->getDefinition() ? llvm::json::Value(identity(Record->getDefinition())) : llvm::json::Value(nullptr);
    }
    if (auto Enum = dyn_cast<EnumDecl>(Declaration)) Entry["type"] = typeId(Context.getEnumType(Enum));
    if (auto Function = dyn_cast<FunctionDecl>(Declaration)) {
      llvm::json::Array Arguments;
      for (auto Parameter : Function->parameters()) Arguments.push_back(identity(Parameter));
      Entry["arguments"] = std::move(Arguments);
    }
    Nodes[Key] = std::move(Entry);
    Parents.push_back(Key); Owners.push_back(Declaration);
    bool Result = RecursiveASTVisitor<NativeEvidenceGraph>::TraverseDecl(Declaration);
    if (auto Function = dyn_cast<FunctionDecl>(Declaration))
      for (auto Parameter : Function->parameters()) Result = TraverseDecl(Parameter) && Result;
    Owners.pop_back(); Parents.pop_back(); return Result;
  }
  bool TraverseStmt(Stmt *Statement) {
    if (!Statement) return true;
    std::string Key = identity(Statement); link(Key);
    if (!SeenNodes.insert(Key).second) return true;
    std::string Kind = Statement->getStmtClassName();
    if (isa<ImplicitCastExpr>(Statement) || isa<ConstantExpr>(Statement)) Kind = "UnexposedExpr";
    if (isa<UnaryExprOrTypeTraitExpr>(Statement)) Kind = "UnaryExpr";
    if (isa<MemberExpr>(Statement)) Kind = "MemberRefExpr";
    auto Entry = base(Kind, Statement->getBeginLoc(), Statement->getSourceRange());
    Entry["spelling"] = ""; Entry["usr"] = ""; Entry["type"] = nullptr;
    Entry["canonical"] = Key;
    Entry["semantic_parent"] = Owners.empty() ? llvm::json::Value(nullptr) : llvm::json::Value(identity(Owners.back()));
    Entry["is_expression"] = isa<Expr>(Statement);
    if (auto Expression = dyn_cast<Expr>(Statement)) {
      Entry["type"] = typeId(Expression->getType());
      Expr::EvalResult Result;
      if (Expression->getType()->isIntegralOrEnumerationType() && Expression->EvaluateAsInt(Result, Context)) {
        const auto &Value = Result.Val.getInt();
        llvm::SmallString<64> Digits; Value.toString(Digits, 10);
        Entry["evaluation"] = llvm::json::Object{{"kind", 1}, {"unsigned", Value.isUnsigned()}, {"value", Digits.str().str()}};
      } else Entry["evaluation"] = nullptr;
    }
    if (auto Reference = dyn_cast<DeclRefExpr>(Statement)) {
      Entry["referenced"] = identity(Reference->getDecl()); Entry["spelling"] = Reference->getDecl()->getNameAsString();
    }
    if (auto Member = dyn_cast<MemberExpr>(Statement)) {
      Entry["referenced"] = identity(Member->getMemberDecl()); Entry["spelling"] = Member->getMemberDecl()->getNameAsString();
    }
    if (auto Unary = dyn_cast<UnaryOperator>(Statement)) Entry["unary_opcode"] = UnaryOperator::getOpcodeStr(Unary->getOpcode()).str();
    if (auto Binary = dyn_cast<BinaryOperator>(Statement)) Entry["binary_opcode"] = BinaryOperator::getOpcodeStr(Binary->getOpcode()).str();
    Nodes[Key] = std::move(Entry); Parents.push_back(Key);
    bool Result = RecursiveASTVisitor<NativeEvidenceGraph>::TraverseStmt(Statement);
    Parents.pop_back(); return Result;
  }
  llvm::json::Object capture() {
    if (!TraverseDecl(Context.getTranslationUnitDecl())) llvm::report_fatal_error("Clang CPG: incomplete native compiler graph traversal");
    for (auto &Pair : Nodes) {
      llvm::json::Array List;
      for (const auto &Child : Children[Pair.first.str()]) List.push_back(Child);
      Pair.second.getAsObject()->operator[]("children") = std::move(List);
    }
    return llvm::json::Object{{"schema_version", 1}, {"root", identity(Context.getTranslationUnitDecl())},
      {"nodes", std::move(Nodes)}, {"types", std::move(Types)}};
  }
};
