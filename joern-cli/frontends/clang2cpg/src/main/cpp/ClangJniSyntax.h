// Compiler-owned executable syntax capture; included inside CompilerMaps.
  jobject atomicFacts(const AtomicExpr *Atomic) {
    LocalFrame Frame(Java.environment());
  enum class Form { Init, Load, LoadCopy, Value, Exchange, C11Compare, GNUCompare };
  Form Shape;
  switch (Atomic->getOp()) {
  case AtomicExpr::AO__c11_atomic_init:
  case AtomicExpr::AO__opencl_atomic_init:
    Shape = Form::Init; break;
  case AtomicExpr::AO__c11_atomic_load:
  case AtomicExpr::AO__opencl_atomic_load:
  case AtomicExpr::AO__hip_atomic_load:
  case AtomicExpr::AO__atomic_load_n:
  case AtomicExpr::AO__scoped_atomic_load_n:
  case AtomicExpr::AO__atomic_test_and_set:
  case AtomicExpr::AO__atomic_clear:
    Shape = Form::Load; break;
  case AtomicExpr::AO__atomic_load:
  case AtomicExpr::AO__scoped_atomic_load:
    Shape = Form::LoadCopy; break;
  case AtomicExpr::AO__c11_atomic_store:
  case AtomicExpr::AO__opencl_atomic_store:
  case AtomicExpr::AO__hip_atomic_store:
  case AtomicExpr::AO__atomic_store:
  case AtomicExpr::AO__atomic_store_n:
  case AtomicExpr::AO__scoped_atomic_store:
  case AtomicExpr::AO__scoped_atomic_store_n:
  case AtomicExpr::AO__c11_atomic_exchange:
  case AtomicExpr::AO__hip_atomic_exchange:
  case AtomicExpr::AO__opencl_atomic_exchange:
  case AtomicExpr::AO__atomic_exchange_n:
  case AtomicExpr::AO__scoped_atomic_exchange_n:
  case AtomicExpr::AO__atomic_fetch_add:
  case AtomicExpr::AO__atomic_fetch_and:
  case AtomicExpr::AO__atomic_fetch_max:
  case AtomicExpr::AO__atomic_fetch_min:
  case AtomicExpr::AO__atomic_fetch_nand:
  case AtomicExpr::AO__atomic_fetch_or:
  case AtomicExpr::AO__atomic_fetch_sub:
  case AtomicExpr::AO__atomic_fetch_xor:
  case AtomicExpr::AO__atomic_add_fetch:
  case AtomicExpr::AO__atomic_and_fetch:
  case AtomicExpr::AO__atomic_max_fetch:
  case AtomicExpr::AO__atomic_min_fetch:
  case AtomicExpr::AO__atomic_nand_fetch:
  case AtomicExpr::AO__atomic_or_fetch:
  case AtomicExpr::AO__atomic_sub_fetch:
  case AtomicExpr::AO__atomic_xor_fetch:
  case AtomicExpr::AO__scoped_atomic_fetch_add:
  case AtomicExpr::AO__scoped_atomic_fetch_and:
  case AtomicExpr::AO__scoped_atomic_fetch_max:
  case AtomicExpr::AO__scoped_atomic_fetch_min:
  case AtomicExpr::AO__scoped_atomic_fetch_nand:
  case AtomicExpr::AO__scoped_atomic_fetch_or:
  case AtomicExpr::AO__scoped_atomic_fetch_sub:
  case AtomicExpr::AO__scoped_atomic_fetch_xor:
  case AtomicExpr::AO__scoped_atomic_add_fetch:
  case AtomicExpr::AO__scoped_atomic_and_fetch:
  case AtomicExpr::AO__scoped_atomic_max_fetch:
  case AtomicExpr::AO__scoped_atomic_min_fetch:
  case AtomicExpr::AO__scoped_atomic_nand_fetch:
  case AtomicExpr::AO__scoped_atomic_or_fetch:
  case AtomicExpr::AO__scoped_atomic_sub_fetch:
  case AtomicExpr::AO__scoped_atomic_xor_fetch:
  case AtomicExpr::AO__c11_atomic_fetch_add:
  case AtomicExpr::AO__c11_atomic_fetch_and:
  case AtomicExpr::AO__c11_atomic_fetch_max:
  case AtomicExpr::AO__c11_atomic_fetch_min:
  case AtomicExpr::AO__c11_atomic_fetch_or:
  case AtomicExpr::AO__c11_atomic_fetch_sub:
  case AtomicExpr::AO__c11_atomic_fetch_xor:
  case AtomicExpr::AO__opencl_atomic_fetch_add:
  case AtomicExpr::AO__opencl_atomic_fetch_and:
  case AtomicExpr::AO__opencl_atomic_fetch_max:
  case AtomicExpr::AO__opencl_atomic_fetch_min:
  case AtomicExpr::AO__opencl_atomic_fetch_or:
  case AtomicExpr::AO__opencl_atomic_fetch_sub:
  case AtomicExpr::AO__opencl_atomic_fetch_xor:
  case AtomicExpr::AO__hip_atomic_fetch_add:
  case AtomicExpr::AO__hip_atomic_fetch_and:
  case AtomicExpr::AO__hip_atomic_fetch_max:
  case AtomicExpr::AO__hip_atomic_fetch_min:
  case AtomicExpr::AO__hip_atomic_fetch_or:
  case AtomicExpr::AO__hip_atomic_fetch_sub:
  case AtomicExpr::AO__hip_atomic_fetch_xor:
  case AtomicExpr::AO__c11_atomic_fetch_nand:
    Shape = Form::Value; break;
  case AtomicExpr::AO__atomic_exchange:
  case AtomicExpr::AO__scoped_atomic_exchange:
    Shape = Form::Exchange; break;
  case AtomicExpr::AO__c11_atomic_compare_exchange_strong:
  case AtomicExpr::AO__c11_atomic_compare_exchange_weak:
  case AtomicExpr::AO__hip_atomic_compare_exchange_strong:
  case AtomicExpr::AO__hip_atomic_compare_exchange_weak:
  case AtomicExpr::AO__opencl_atomic_compare_exchange_strong:
  case AtomicExpr::AO__opencl_atomic_compare_exchange_weak:
    Shape = Form::C11Compare; break;
  case AtomicExpr::AO__atomic_compare_exchange:
  case AtomicExpr::AO__atomic_compare_exchange_n:
  case AtomicExpr::AO__scoped_atomic_compare_exchange:
  case AtomicExpr::AO__scoped_atomic_compare_exchange_n:
    Shape = Form::GNUCompare; break;
  default:
    fail("unsupported compiler atomic operand form: " + Atomic->getOpAsString().str());
  }
    jobject Operands = Java.list(); std::set<const Expr *> Captured;
    auto Add = [&](llvm::StringRef Role, const Expr *Expression) {
      if (!Expression) fail("compiler atomic operand is missing");
      if (!Captured.insert(Expression).second) fail("compiler atomic operands share an ambiguous AST identity");
      jobject Operand = Java.object(); Java.text(Operand, "role", Role);
      Java.text(Operand, "expression", identifier(Expression)); Java.add(Operands, Operand);
    };
    Add("pointer", Atomic->getPtr());
    switch (Shape) {
    case Form::Init: Add("value", Atomic->getVal1()); break;
    case Form::Load: Add("order", Atomic->getOrder()); break;
    case Form::LoadCopy: Add("result", Atomic->getVal1()); Add("order", Atomic->getOrder()); break;
    case Form::Value: Add("value", Atomic->getVal1()); Add("order", Atomic->getOrder()); break;
    case Form::Exchange: Add("value", Atomic->getVal1()); Add("result", Atomic->getVal2()); Add("order", Atomic->getOrder()); break;
    case Form::C11Compare: case Form::GNUCompare:
      Add("expected", Atomic->getVal1()); Add("desired", Atomic->getVal2());
      if (Shape == Form::GNUCompare) Add("weak", Atomic->getWeak());
      Add("order", Atomic->getOrder()); Add("failure_order", Atomic->getOrderFail()); break;
    }
    if (Atomic->getScopeModel()) Add("scope", Atomic->getScope());
    if (Captured.size() != Atomic->getNumSubExprs()) fail("compiler atomic operand coverage differs from its source layout");
    jobject Result = Java.object(); Java.integer(Result, "op_code", static_cast<int64_t>(Atomic->getOp()));
    Java.text(Result, "name", Atomic->getOpAsString()); Java.put(Result, "operands", Operands);
    return Frame.keep(Result);
  }
  jobject assemblyFacts(const GCCAsmStmt *Assembly) {
    LocalFrame Frame(Java.environment());
    const auto *Template = Assembly->getAsmString();
    if (!Template || Template->getCharByteWidth() != 1) fail("compiler GCC assembly lacks a narrow-string template");
    jobject Result = Java.object(), Outputs = Java.list(), Inputs = Java.list(), Clobbers = Java.list(), Labels = Java.list();
    auto Operand = [&](llvm::StringRef Name, llvm::StringRef Constraint, const Expr *Expression, bool Output, bool ReadWrite) {
      if (!Expression || Expression->getType().isNull()) fail("compiler GCC assembly lacks a typed source operand");
      jobject Value = Java.object(); Java.text(Value, "name", Name); Java.text(Value, "constraint", Constraint);
      Java.text(Value, "expression", identifier(Expression)); Java.put(Value, "semantic_type", semantic(Expression->getType()));
      if (Output) Java.flag(Value, "read_write", ReadWrite);
      return Value;
    };
    for (unsigned Index = 0; Index < Assembly->getNumOutputs(); ++Index)
      Java.add(Outputs, Operand(Assembly->getOutputName(Index), Assembly->getOutputConstraint(Index), Assembly->getOutputExpr(Index), true, Assembly->isOutputPlusConstraint(Index)));
    for (unsigned Index = 0; Index < Assembly->getNumInputs(); ++Index)
      Java.add(Inputs, Operand(Assembly->getInputName(Index), Assembly->getInputConstraint(Index), Assembly->getInputExpr(Index), false, false));
    for (unsigned Index = 0; Index < Assembly->getNumClobbers(); ++Index) Java.add(Clobbers, Java.string(Assembly->getClobber(Index)));
    for (unsigned Index = 0; Index < Assembly->getNumLabels(); ++Index) {
      const auto *Expression = Assembly->getLabelExpr(Index);
      if (!Expression || !Expression->getLabel() || Expression->getType().isNull()) fail("compiler assembly goto lacks a typed label expression");
      jobject Label = Java.object(); Java.text(Label, "name", Assembly->getLabelName(Index));
      Java.text(Label, "expression", identifier(Expression)); Java.text(Label, "declaration", identifier(Expression->getLabel())); Java.add(Labels, Label);
      jobject Info = location(Expression->getSourceRange()); Java.text(Info, "type", typeName(Expression->getType()));
      Java.put(Info, "semantic_type", semantic(Expression->getType())); Java.text(Info, "label_declaration", identifier(Expression->getLabel()));
      Java.put(Nodes, identifier(Expression), Info);
    }
    Java.text(Result, "result_kind", "statement"); Java.put(Result, "result_type", semantic(Context.VoidTy));
    Java.text(Result, "dialect", "gcc"); Java.text(Result, "template", Template->getString());
    Java.flag(Result, "simple", Assembly->isSimple()); Java.flag(Result, "volatile", Assembly->isVolatile()); Java.flag(Result, "goto", Assembly->isAsmGoto());
    Java.put(Result, "outputs", Outputs); Java.put(Result, "inputs", Inputs); Java.put(Result, "clobbers", Clobbers); Java.put(Result, "labels", Labels);
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
      Java.put(Info, "operand_semantic_type", semantic(Trait->getTypeOfArgument()));
      if (Trait->isArgumentType()) { jobject Type = Java.object(); Java.text(Type, "qualType", Trait->getArgumentType().getAsString(Context.getPrintingPolicy())); Java.put(Result, "argType", Type); Java.text(Info, "operand_type", typeName(Trait->getArgumentType())); }
    }
    if (const auto *Constant = dyn_cast<ConstantExpr>(Statement)) {
      if (Constant->hasAPValueResult() && Constant->getResultAPValueKind() == APValue::Int) {
        llvm::SmallString<64> Digits; Constant->getResultAsAPSInt().toString(Digits, 10); Java.text(Result, "value", Digits.str());
      }
    }
    if (const auto *Atomic = dyn_cast<AtomicExpr>(Statement)) {
      Java.text(Result, "name", Atomic->getOpAsString()); Java.put(Info, "atomic", atomicFacts(Atomic));
    }
    if (const auto *Assembly = dyn_cast<GCCAsmStmt>(Statement)) Java.put(Info, "gcc_asm", assemblyFacts(Assembly));
    if (const auto *Trait = dyn_cast<TypeTraitExpr>(Statement)) {
      if (Trait->isTypeDependent() || Trait->isValueDependent()) fail("compiler type trait result is dependent");
      jobject Value = Java.object(), Arguments = Java.list();
      for (const TypeSourceInfo *Argument : Trait->getArgs()) {
        if (!Argument || Argument->getType().isNull()) fail("compiler type trait lacks operand type evidence");
        jobject Operand = Java.object(); Java.text(Operand, "spelling", Argument->getType().getAsString(Context.getPrintingPolicy()));
        Java.put(Operand, "semantic_type", semantic(Argument->getType())); Java.add(Arguments, Operand);
      }
      Java.integer(Value, "kind", static_cast<int64_t>(Trait->getTrait())); Java.text(Value, "name", getTraitName(Trait->getTrait()));
      Java.text(Value, "spelling", getTraitSpelling(Trait->getTrait())); Java.flag(Value, "value", Trait->getValue());
      Java.flag(Value, "operands_unevaluated", true); Java.put(Value, "arguments", Arguments); Java.put(Info, "type_trait", Value);
    }
    if (const auto *Predefined = dyn_cast<PredefinedExpr>(Statement)) {
      const auto *Literal = Predefined->getFunctionName();
      if (!Literal) fail("compiler predefined identifier lacks its string literal");
      std::string Spelling; llvm::raw_string_ostream Stream(Spelling); Literal->printPretty(Stream, nullptr, Context.getPrintingPolicy()); Stream.flush();
      Java.text(Info, "predefined_name", PredefinedExpr::getIdentKindName(Predefined->getIdentKind()));
      Java.text(Info, "predefined_literal_spelling", Spelling);
      jobject String = Java.object(), Units = Java.list(); Java.integer(String, "unit_width", Literal->getCharByteWidth());
      for (unsigned Index = 0; Index < Literal->getLength(); ++Index) Java.add(Units, Java.number(Literal->getCodeUnit(Index)));
      Java.put(String, "units", Units); Java.put(Info, "string_literal", String);
    }
    if (const auto *Selection = dyn_cast<GenericSelectionExpr>(Statement)) {
      if (Selection->isResultDependent()) fail("compiler generic selection result is dependent");
      jobject Value = Java.object(); Java.text(Value, "selected_expression", identifier(Selection->getResultExpr()));
      Java.integer(Value, "selected_association", Selection->getResultIndex()); Java.flag(Value, "controlling_expression_unevaluated", true);
      if (Selection->isExprPredicate()) {
        const Expr *Controller = Selection->getControllingExpr();
        Java.text(Value, "controlling_expression", identifier(Controller)); Java.add(Children, statement(Controller));
      } else Java.put(Value, "controlling_type", semantic(Selection->getControllingType()->getType()));
      for (auto Association : Selection->associations()) {
        jobject Entry = Java.object(), Body = Java.list();
        Java.text(Entry, "associationKind", Association.getTypeSourceInfo() ? "case" : "default");
        Java.flag(Entry, "selected", Association.isSelected());
        if (Association.getTypeSourceInfo()) Java.put(Entry, "association_type", semantic(Association.getType()));
        Java.add(Body, statement(Association.getAssociationExpr())); Java.put(Entry, "inner", Body); Java.add(Children, Entry);
      }
      Java.put(Info, "generic_selection", Value);
    } else if (const auto *Initializer = dyn_cast<InitListExpr>(Statement)) {
      const InitListExpr *Semantic = Initializer->isSemanticForm() ? Initializer : Initializer->getSemanticForm();
      if (!Semantic) fail("initializer lacks its compiler semantic form");
      if (const Expr *Filler = Semantic->getArrayFiller()) {
        jobject Values = Java.list(); Java.add(Values, statement(Filler));
        for (const Expr *Element : Semantic->inits()) {
          if (!Element) fail("compiler initializer contains a missing semantic element");
          Java.add(Values, statement(Element));
        }
        Java.put(Result, "array_filler", Values);
      } else {
        for (const Expr *Element : Semantic->inits()) {
          if (!Element) fail("compiler initializer contains a missing semantic element");
          Java.add(Children, statement(Element));
        }
      }
    } else if (const auto *Loop = dyn_cast<ForStmt>(Statement)) {
      const Stmt *Slots[] = {Loop->getInit(), Loop->getConditionVariableDeclStmt(), Loop->getCond(), Loop->getInc(), Loop->getBody()};
      for (const Stmt *Slot : Slots) Java.add(Children, Slot ? statement(Slot) : Java.object());
    } else if (const auto *Assembly = dyn_cast<GCCAsmStmt>(Statement)) {
      for (unsigned Index = 0; Index < Assembly->getNumOutputs(); ++Index) Java.add(Children, statement(Assembly->getOutputExpr(Index)));
      for (unsigned Index = 0; Index < Assembly->getNumInputs(); ++Index) Java.add(Children, statement(Assembly->getInputExpr(Index)));
    } else if (const auto *Declarations = dyn_cast<DeclStmt>(Statement)) {
      for (const Decl *Declaration : Declarations->decls()) Java.add(Children, declaration(Declaration));
    } else {
      switch (Statement->getStmtClass()) {
      case Stmt::CompoundStmtClass: case Stmt::ReturnStmtClass: case Stmt::IfStmtClass: case Stmt::GotoStmtClass: case Stmt::LabelStmtClass:
      case Stmt::NullStmtClass: case Stmt::DeclRefExprClass: case Stmt::MemberExprClass: case Stmt::CallExprClass:
      case Stmt::BinaryOperatorClass: case Stmt::CompoundAssignOperatorClass: case Stmt::UnaryOperatorClass:
      case Stmt::ImplicitCastExprClass: case Stmt::CStyleCastExprClass: case Stmt::IntegerLiteralClass: case Stmt::FloatingLiteralClass:
      case Stmt::CharacterLiteralClass: case Stmt::StringLiteralClass: case Stmt::ParenExprClass: case Stmt::ArraySubscriptExprClass:
      case Stmt::ConditionalOperatorClass: case Stmt::UnaryExprOrTypeTraitExprClass:
      case Stmt::WhileStmtClass: case Stmt::DoStmtClass: case Stmt::SwitchStmtClass:
      case Stmt::CaseStmtClass: case Stmt::DefaultStmtClass: case Stmt::BreakStmtClass: case Stmt::ContinueStmtClass:
      case Stmt::ImplicitValueInitExprClass: case Stmt::CompoundLiteralExprClass: case Stmt::ConstantExprClass:
      case Stmt::StmtExprClass: case Stmt::VAArgExprClass: case Stmt::PredefinedExprClass: case Stmt::TypeTraitExprClass:
      case Stmt::AtomicExprClass:
        for (const Stmt *Child : Statement->children()) if (Child) Java.add(Children, statement(Child));
        break;
      default: fail(std::string("unsupported executable compiler syntax ") + Statement->getStmtClassName());
      }
    }
    Java.put(Result, "inner", Children); Java.put(Nodes, identifier(Statement), Info); WrittenStatements.insert(Statement);
    return Frame.keep(Result);
  }
