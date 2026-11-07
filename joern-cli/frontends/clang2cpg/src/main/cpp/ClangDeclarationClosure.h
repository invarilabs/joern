// Compiler declaration selection mirrors c2cpg's exclusion of unrelated included AST nodes.
class ClangDeclarationClosure : public RecursiveASTVisitor<ClangDeclarationClosure> {
  ASTContext &Context;
  SourceManager &Manager;
  std::set<const Decl *> Required;
  std::set<const void *> Types;
  std::vector<const Decl *> Pending;

  bool main(const Decl *Declaration) const {
    return Declaration->getLocation().isValid() &&
      Manager.isWrittenInMainFile(Manager.getExpansionLoc(Declaration->getLocation()));
  }
  void mark(const Decl *Declaration) {
    if (!Declaration || !Required.insert(Declaration).second) return;
    Pending.push_back(Declaration);
    mark(Declaration->getCanonicalDecl());
    const DeclContext *Owner = Declaration->getDeclContext();
    if (Owner && !Owner->isTranslationUnit()) mark(Decl::castFromDeclContext(Owner));
  }
  void type(QualType Value) {
    if (Value.isNull()) fail("required compiler dependency type is null");
    if (!Types.insert(Value.getAsOpaquePtr()).second) return;
    const Type *Shape = Value.getTypePtr();
    if (const auto *Alias = dyn_cast<TypedefType>(Shape)) {
      mark(Alias->getDecl()); type(Alias->getDecl()->getUnderlyingType());
    } else if (const auto *Elaborated = dyn_cast<ElaboratedType>(Shape)) type(Elaborated->getNamedType());
    else if (const auto *Parenthesized = dyn_cast<ParenType>(Shape)) type(Parenthesized->getInnerType());
    else if (const auto *Attributed = dyn_cast<AttributedType>(Shape)) type(Attributed->getModifiedType());
    type(Context.getCanonicalType(Value));
    if (const auto *Record = dyn_cast<RecordType>(Shape)) {
      mark(Record->getDecl()); mark(Record->getDecl()->getDefinition());
    } else if (const auto *Enumeration = dyn_cast<EnumType>(Shape)) {
      mark(Enumeration->getDecl()); mark(Enumeration->getDecl()->getDefinition());
    } else if (const auto *Pointer = dyn_cast<PointerType>(Shape)) type(Pointer->getPointeeType());
    else if (const auto *Array = dyn_cast<ArrayType>(Shape)) type(Array->getElementType());
    else if (const auto *Function = dyn_cast<FunctionType>(Shape)) {
      type(Function->getReturnType());
      if (const auto *Prototype = dyn_cast<FunctionProtoType>(Function))
        for (QualType Parameter : Prototype->param_types()) type(Parameter);
    } else if (const auto *Atomic = dyn_cast<AtomicType>(Shape)) type(Atomic->getValueType());
    else if (const auto *Complex = dyn_cast<ComplexType>(Shape)) type(Complex->getElementType());
    else if (const auto *Vector = dyn_cast<VectorType>(Shape)) type(Vector->getElementType());
  }
public:
  explicit ClangDeclarationClosure(ASTContext &Value) : Context(Value), Manager(Value.getSourceManager()) {
    for (const Decl *Declaration : Context.getTranslationUnitDecl()->decls())
      if (main(Declaration)) mark(Declaration);
    for (size_t Index = 0; Index < Pending.size(); ++Index) {
      const Decl *Declaration = Pending[Index];
      if (const auto *Value = dyn_cast<ValueDecl>(Declaration)) {
        if (Value->getType().isNull()) fail("dependency ValueDecl has no type: " + std::string(Value->getDeclKindName()));
        type(Value->getType());
      }
      if (const auto *Alias = dyn_cast<TypedefNameDecl>(Declaration)) type(Alias->getUnderlyingType());
      if (const auto *Record = dyn_cast<RecordDecl>(Declaration)) {
        mark(Record->getDefinition());
        for (const Decl *Child : Record->decls()) mark(Child);
      }
      if (const auto *Enumeration = dyn_cast<EnumDecl>(Declaration)) {
        mark(Enumeration->getDefinition());
        for (const Decl *Child : Enumeration->decls()) mark(Child);
      }
      if (const auto *Function = dyn_cast<FunctionDecl>(Declaration))
        for (const ParmVarDecl *Parameter : Function->parameters()) mark(Parameter);
      // Header function bodies remain external contracts, as in the existing source graph.
      if (main(Declaration)) TraverseDecl(const_cast<Decl *>(Declaration));
    }
  }
  bool TraverseStaticAssertDecl(StaticAssertDecl *Assertion) {
    mark(Assertion);
    // The compiler's diagnostic string has no value type or declaration dependency.
    return TraverseStmt(Assertion->getAssertExpr());
  }
  bool VisitDecl(Decl *Declaration) { mark(Declaration); return true; }
  bool VisitExpr(Expr *Expression) {
    if (Expression->getType().isNull()) fail("dependency Expr has no type: " + std::string(Expression->getStmtClassName()) +
      " at " + Expression->getBeginLoc().printToString(Manager));
    type(Expression->getType()); return true;
  }
  bool VisitDeclRefExpr(DeclRefExpr *Reference) { mark(Reference->getDecl()); return true; }
  bool VisitMemberExpr(MemberExpr *Member) { mark(Member->getMemberDecl()); return true; }
  bool VisitTypeLoc(TypeLoc Location) {
    if (Location.getType().isNull()) fail("dependency TypeLoc has no type at " + Location.getBeginLoc().printToString(Manager));
    type(Location.getType()); return true;
  }
  bool contains(const Decl *Declaration) const { return Required.count(Declaration) != 0; }
  const std::vector<const Decl *> &ordered() const { return Pending; }
};
