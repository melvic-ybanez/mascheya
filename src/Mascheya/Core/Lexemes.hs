module Mascheya.Core.Lexemes where

plus :: Char
plus = '+'

minus :: Char
minus = '-'

times :: Char
times = '*'

divide :: Char
divide = '/'

modulo :: Char
modulo = '%'

lessThan :: Char
lessThan = '<'

greaterThan :: Char
greaterThan = '>'

rightArrow :: String
rightArrow = "->"

lambdaSymbol :: Char
lambdaSymbol = '\\'

ifLexeme :: String
ifLexeme = "if"

openSquareBracket :: Char
openSquareBracket = '['

closeSquareBracket :: Char
closeSquareBracket = ']'

cons :: String
cons = "::"

int :: String
int = "Int"

float :: String
float = "Float"

double :: String
double = "Double"

colon :: Char
colon = ':'

comma :: Char
comma = ','

dot :: Char
dot = '.'

floatSuffix :: Char
floatSuffix = 'f'

equals :: Char
equals = '='

leftParen :: Char
leftParen = '('

rightParen :: Char
rightParen = ')'

space :: Char
space = ' '

escapePrefix :: Char
escapePrefix = '\\'

octalPrefix :: String
octalPrefix = "\\o"

hexPrefix :: String
hexPrefix = "\\x"

singleQuote :: Char
singleQuote = '\''

doubleQuote :: Char
doubleQuote = '\"'

true :: String
true = "True"

false :: String
false = "False"

underscore :: Char
underscore = '_'

exclamation :: Char
exclamation = '!'

equalsEquals :: String
equalsEquals = [equals, equals]

notEquals :: String
notEquals = [exclamation, equals]

lessThanEquals :: String
lessThanEquals = [lessThan, equals]

greaterThanEquals :: String
greaterThanEquals = [greaterThan, equals]