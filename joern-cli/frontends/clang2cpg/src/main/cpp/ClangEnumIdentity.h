// Compiler declaration identities, preserving unique Index USRs exactly.
class EnumIdentityCatalog : public RecursiveASTVisitor<EnumIdentityCatalog> {
  ASTContext &Context;
  std::vector<const EnumDecl *> EncounterOrder;
  std::set<const EnumDecl *> SeenDeclarations;
  std::set<const clang::Type *> SeenTypes;
  std::set<const Expr *> AssertionMessages;
  std::map<const EnumDecl *, std::string> Identities;

  void collect(const EnumDecl *Declaration) {
    const auto *Canonical = Declaration->getCanonicalDecl();
    if (SeenDeclarations.insert(Canonical).second) EncounterOrder.push_back(Canonical);
  }
  void collectType(QualType Type) {
    if (Type.isNull()) llvm::report_fatal_error("Clang CPG: enum type inventory received a null value type");
    const clang::Type *Shape = Context.getCanonicalType(Type).getTypePtr();
    if (!SeenTypes.insert(Shape).second) return;
    if (auto Enum = dyn_cast<EnumType>(Shape)) collect(Enum->getDecl());
    else if (auto Pointer = dyn_cast<PointerType>(Shape)) collectType(Pointer->getPointeeType());
    else if (auto Reference = dyn_cast<ReferenceType>(Shape)) collectType(Reference->getPointeeType());
    else if (auto Member = dyn_cast<MemberPointerType>(Shape)) {
      collectType(Member->getPointeeType()); collectType(QualType(Member->getClass(), 0));
    } else if (auto Array = dyn_cast<ArrayType>(Shape)) collectType(Array->getElementType());
    else if (auto Function = dyn_cast<FunctionType>(Shape)) {
      collectType(Function->getReturnType());
      if (auto Prototype = dyn_cast<FunctionProtoType>(Function))
        for (QualType Parameter : Prototype->param_types()) collectType(Parameter);
    } else if (auto Atomic = dyn_cast<AtomicType>(Shape)) collectType(Atomic->getValueType());
    else if (auto Complex = dyn_cast<ComplexType>(Shape)) collectType(Complex->getElementType());
    else if (auto Vector = dyn_cast<VectorType>(Shape)) collectType(Vector->getElementType());
  }
public:
  explicit EnumIdentityCatalog(ASTContext &Value) : Context(Value) {
    if (!TraverseDecl(Context.getTranslationUnitDecl()))
      llvm::report_fatal_error("Clang CPG: incomplete enum declaration identity inventory");
    std::map<std::string, std::vector<const EnumDecl *>> Groups;
    for (const auto *Declaration : EncounterOrder) Groups[usr(Declaration)].push_back(Declaration);
    for (const auto &Group : Groups)
      for (size_t Ordinal = 0; Ordinal < Group.second.size(); ++Ordinal)
        Identities[Group.second[Ordinal]] = Group.first +
          (Group.second.size() == 1 ? "" : "#enum:" + std::to_string(Ordinal));
  }
  bool shouldVisitImplicitCode() const { return true; }
  bool VisitEnumDecl(EnumDecl *Declaration) { collect(Declaration); return true; }
  bool VisitEnumType(EnumType *Type) { collect(Type->getDecl()); return true; }
  bool VisitValueDecl(ValueDecl *Declaration) { collectType(Declaration->getType()); return true; }
  bool VisitTypedefNameDecl(TypedefNameDecl *Declaration) { collectType(Declaration->getUnderlyingType()); return true; }
  bool VisitStaticAssertDecl(StaticAssertDecl *Declaration) {
    if (const auto *Message = Declaration->getMessage()) AssertionMessages.insert(Message);
    return true;
  }
  bool VisitExpr(Expr *Expression) {
    if (Expression->getType().isNull() && AssertionMessages.count(Expression) && isa<StringLiteral>(Expression)) return true;
    collectType(Expression->getType()); return true;
  }
  bool VisitTypeTraitExpr(TypeTraitExpr *Trait) {
    for (const auto *Argument : Trait->getArgs()) collectType(Argument->getType());
    return true;
  }
  std::string identityFor(const EnumDecl *Declaration, const ASTContext &Value) const {
    if (&Value != &Context) llvm::report_fatal_error("Clang CPG: enum identity inventory belongs to another AST context");
    auto Found = Identities.find(Declaration->getCanonicalDecl());
    if (Found == Identities.end()) llvm::report_fatal_error("Clang CPG: enum declaration missing from identity inventory");
    return Found->second;
  }
};
thread_local const EnumIdentityCatalog *ActiveEnumIdentities = nullptr;
class ScopedEnumIdentities {
  EnumIdentityCatalog Catalog;
public:
  explicit ScopedEnumIdentities(ASTContext &Context) : Catalog(Context) {
    if (ActiveEnumIdentities) llvm::report_fatal_error("Clang CPG: nested enum identity inventory");
    ActiveEnumIdentities = &Catalog;
  }
  ~ScopedEnumIdentities() { ActiveEnumIdentities = nullptr; }
};
std::string enumIdentity(const EnumDecl *Declaration, const ASTContext &Context) {
  if (!ActiveEnumIdentities) llvm::report_fatal_error("Clang CPG: enum identity inventory is unavailable");
  return ActiveEnumIdentities->identityFor(Declaration, Context);
}
