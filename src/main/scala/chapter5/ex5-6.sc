/**
 * Implement headOption using foldRight
 */

def headOption: Option[A] =
  foldRight(None: Option[A])((a, b) => Some(a))