package scalite

class Match
  def apply(): String =
    1 match
      case 1 =>
        println(1)
        "1"
      case 2 => "2"


