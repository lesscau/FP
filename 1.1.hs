data Operator = Plus | Minus | Mult deriving (Show, Eq)

data Term = IntConstant {intValue :: Int}
          | Variable    {varName::String}
          | BinaryTerm  {lhv :: Term, op :: Operator, rhv :: Term}
          | UnaryTerm   {val::Term, op::Operator}
            deriving (Show,Eq)

(<+>) :: Term -> Term -> Term
(IntConstant lhv) <+> (IntConstant rhv) = IntConstant (lhv + rhv)
lhv <+> rhv = BinaryTerm lhv Plus rhv

(<->) :: Term -> Term -> Term
(IntConstant lhv) <-> (IntConstant rhv) = IntConstant (lhv - rhv)
lhv <-> rhv = BinaryTerm lhv Minus rhv

(<*>) :: Term -> Term -> Term
(IntConstant lhv) <*> (IntConstant rhv) = IntConstant (lhv * rhv)
lhv <*> rhv = BinaryTerm lhv Mult rhv

infixl 6 <+>,<->
infixl 7 <*>

replaceVar :: Term -> String -> Term -> Term
replaceVar (IntConstant intValue) _ _ = IntConstant intValue
replaceVar (Variable varName) var term | (varName == var) = term 
                                       | otherwise = Variable varName
replaceVar (BinaryTerm lhv op rhv) var term =
  BinaryTerm (replaceVar lhv var term) op (replaceVar rhv var term)
replaceVar (UnaryTerm trm op) var term = UnaryTerm (replaceVar trm var term) op
