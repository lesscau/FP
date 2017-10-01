data Operator = Plus | Minus | Mult deriving (Show, Eq)

data Term	=	IntConstant	{intValue :: Int}
			|	Variable	{varName::String}
			|	BinaryTerm	{lhv :: Term, op :: Operator, rhv :: Term}
			|	UnaryTerm	{val::Term, op::Operator}
				deriving (Show,Eq)

(<+>) :: Term -> Term -> Term
infixl 6 <+>,<->
lhv <+> rhv = BinaryTerm lhv Plus rhv
lhv <-> rhv = BinaryTerm lhv Minus rhv
infixl 7 <*>
lhv <*> rhv = BinaryTerm lhv Mult rhv

replaceVar :: Term -> String -> Term -> Term
replaceVar (IntConstant intValue) _ _ = IntConstant intValue
replaceVar (Variable varName) var term = if varName == var then term else Variable varName
replaceVar (BinaryTerm lhv op rhv) var term =
  BinaryTerm (replaceVar lhv var term) op (replaceVar rhv var term)
replaceVar (UnaryTerm trm op) var term = UnaryTerm (replaceVar trm var term) op
