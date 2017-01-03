// Arithmetics -----------------------------------------------------------------

IDENTITY       = x => x
SUCCESSOR      = n => f => x => f(n(f)(x))
PREDECESSOR    = n => f => x => n(g => h => h(g(f)))(_ => x)(u => u)
ADDITION       = m => n => n(SUCCESSOR)(m)
SUBTRACTION    = m => n => n(PREDECESSOR)(m)
MULTIPLICATION = m => n => m(ADDITION(n))(f => IDENTITY)
POWER          = x => y => y(x)
ABS_DIFFERENCE = x => y => ADDITION(SUBTRACTION(x)(y))(SUBTRACTION(y)(x))

// Logic -----------------------------------------------------------------------

TRUE  = t => f => t
FALSE = t => f => f
AND   = p => q => p(q)(p)
OR    = p => q => p(p)(q)
XOR   = p => q => p(NOT(q))(q)
NOT   = c => c(FALSE)(TRUE)
IF    = c => t => f => c(t)(f)

// Comparison ------------------------------------------------------------------

IS_ZERO               = n => n(_ => FALSE)(TRUE)
IS_LESS_THAN          = m => n => NOT(IS_LESS_THAN_EQUAL(n)(m))
IS_LESS_THAN_EQUAL    = m => n => IS_ZERO(SUBTRACTION(m)(n))
IS_EQUAL              = m => n => AND(IS_LESS_THAN_EQUAL(m)(n))(IS_LESS_THAN_EQUAL(n)(m))
IS_NOT_EQUAL          = m => n => OR(NOT(IS_LESS_THAN_EQUAL(m)(n)))(NOT(IS_LESS_THAN_EQUAL(n)(m)))
IS_GREATER_THAN_EQUAL = m => n => IS_LESS_THAN_EQUAL(n)(m)
IS_GREATER_THAN       = m => n => NOT(IS_LESS_THAN_EQUAL(m)(n))
IS_NULL               = p => p(x => y => FALSE)
NIL                   = x => TRUE

// Test "Framework" ------------------------------------------------------------

ASSERT = truth => IF(truth)
  (description => `[\x1b[32m✓\x1b[0m] ${description}`)
  (description => `[\x1b[31m✗\x1b[0m] ${description}`)

REFUTE = truth => ASSERT(NOT(truth))

TEST   = description => assertion => console.log(assertion(description))

// Church Numerals -------------------------------------------------------------

$zero  = f => IDENTITY
$one   = SUCCESSOR($zero)
$two   = SUCCESSOR($one)
$three = SUCCESSOR($two)
$four  = MULTIPLICATION($two)($two)
$five  = SUCCESSOR($four)
$eight = MULTIPLICATION($two)($four)
$nine  = SUCCESSOR($eight)
$ten   = MULTIPLICATION($two)($five)

// Tests -----------------------------------------------------------------------

TEST('TRUE')
  (ASSERT(TRUE))

TEST('FALSE')
  (REFUTE(FALSE))

TEST('AND')
  (ASSERT(AND(TRUE)(TRUE)))

TEST('OR')(ASSERT(AND
  (AND(OR(TRUE)(FALSE))(OR(FALSE)(TRUE)))
  (NOT(OR(FALSE)(FALSE)))))

TEST('XOR')(ASSERT(AND
  (AND(XOR(TRUE)(FALSE))(XOR(FALSE)(TRUE)))
  (NOT(XOR(TRUE)(TRUE)))))

TEST('NOT')
  (REFUTE(NOT(TRUE)))

TEST('IF')(ASSERT(AND
  (IF(TRUE)(TRUE)(FALSE))
  (NOT(IF(FALSE)(TRUE)(FALSE)))))

TEST('IDENTITY')
  (ASSERT(IS_EQUAL(IDENTITY)(x => x)))

TEST('SUCCESSOR')
  (ASSERT(IS_EQUAL(SUCCESSOR($zero))($one)))

TEST('PREDECESSOR')
  (ASSERT(IS_EQUAL($zero)(PREDECESSOR($one))))

TEST('ADDITION')
  (ASSERT(IS_EQUAL(SUCCESSOR($one))(ADDITION($one)($one))))

TEST('SUBTRACTION')
  (ASSERT(IS_EQUAL($zero)(SUBTRACTION($one)($one))))

TEST('MULTIPLICATION')
  (ASSERT(IS_EQUAL($four)(MULTIPLICATION($two)($two))))

TEST('POWER')(ASSERT(AND
  (IS_EQUAL($nine)(POWER($three)($two)))
  (IS_EQUAL($eight)(POWER($two)($three)))))

TEST('ABS_DIFFERENCE')(ASSERT(AND
  (IS_EQUAL($one)(ABS_DIFFERENCE($three)($two)))
  (IS_EQUAL($one)(ABS_DIFFERENCE($two)($three)))))

TEST('IS_ZERO')
  (ASSERT(IS_ZERO($zero)))

TEST('IS_LESS_THAN')
  (ASSERT(IS_LESS_THAN($zero)($one)))

TEST('IS_LESS_THAN_EQUAL')(ASSERT(AND
  (IS_LESS_THAN_EQUAL($one)($one))
  (IS_LESS_THAN_EQUAL($zero)($one))))

TEST('IS_EQUAL')(ASSERT(AND
  (IS_EQUAL($zero)($zero))
  (IS_EQUAL($one)($one))))

TEST('IS_NOT_EQUAL')
  (ASSERT(IS_NOT_EQUAL($zero)($one)))

TEST('IS_GREATER_THAN_EQUAL')(ASSERT(AND
  (IS_GREATER_THAN_EQUAL($one)($one))
  (IS_GREATER_THAN_EQUAL($one)($zero))))

TEST('IS_GREATER_THAN')
  (ASSERT(IS_GREATER_THAN($one)($zero)))

TEST('IS_NULL')
  (ASSERT(IS_NULL(NIL)))

// Examples --------------------------------------------------------------------

console.log('\n--- Examples ---\n')

FACTORIAL = n => IF(IS_ZERO(n))
  (_ => SUCCESSOR(n))
  (_ => MULTIPLICATION(n)(FACTORIAL(PREDECESSOR(n))))
()

FIBONACCI = n => IF(IS_LESS_THAN_EQUAL(n)(SUCCESSOR(f => IDENTITY)))
  (_ => n)
  (_ => ADDITION
    (FIBONACCI(PREDECESSOR(n)))
    (FIBONACCI(PREDECESSOR(PREDECESSOR(n)))))
()

TEST('FACTORIAL: 5! = 120')(ASSERT(IS_EQUAL
  (FACTORIAL($five))
  (ADDITION(MULTIPLICATION($ten)($ten))(ADDITION($ten)($ten)))))

TEST('FIBONACCI: 10 = 55')(ASSERT(IS_EQUAL
  (FIBONACCI($ten))
  (ADDITION(MULTIPLICATION($five)($ten))($five))))
