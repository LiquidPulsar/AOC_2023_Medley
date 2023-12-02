import java.util.regex.Pattern

File file = new File(
    (args.size()==2 ? args[1] : "input")
    +".txt"
    )

if (args.size()==0) throw new Exception("No part specified")

if (args[0]=='1') {
    def line, matcher, tot = 0
    file.withReader { reader ->
        while ((line = reader.readLine()) != null) {
            matcher = line =~ ~/\d/
            tot += (matcher[0]+matcher[-1]).toInteger()
        }
        println tot
    }
} else if (args[0]=='2') {
    nums = "zero one two three four five six seven eight nine".split()
    map = [:]
    nums.eachWithIndex {x,y -> map[x] = "$y"}
    println map

    def line, matcher, tot = 0
    file.withReader { reader ->
        while ((line = reader.readLine()) != null) {
            matcher = line =~ ~(/(?=(\d|/ + nums.join("|") + /))/)
            tot += (
                map.getOrDefault(matcher[0][1],matcher[0][1]) +
                map.getOrDefault(matcher[-1][1],matcher[-1][1])
            ).toInteger()
        }
        println tot
    }
} else {
    throw new Exception("Invalid part ${args[0]}")
}
