package scalite.simple

class IfElseWhile
    def apply() =
        var x = 0
        var y = 0

        while (x < 10)
            if (x % 2 == 0)
                x = x + 1
                y += x
            else
                x = x + 2
                y += x

        do
            println("plus")
            y += 1
        while (y % 10 != 0)

        y

