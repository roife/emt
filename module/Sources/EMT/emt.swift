import EmacsSwiftModule
import NaturalLanguage

class EMT: Module {
    typealias WordBound = ConsCell<Int, Int>

    let isGPLCompatible = true
    let tokenizer = NLTokenizer(unit: .word)

    func funcName(_ name: String) -> String {
        let funcNamePrefix = "emt--"
        let funcNameSuffix = "-helper"
        return funcNamePrefix + name + funcNameSuffix
    }

    func doSplitHelper(text: String) throws -> [WordBound] {
        self.tokenizer.string = text
        let WordBounds = self.tokenizer.tokens(for: text.startIndex..<text.endIndex);

        var bounds: [WordBound] = []
        var lastIndex = text.startIndex
        var lastPos = 0
        for range in WordBounds {
            let beg = lastPos + text.distance(from: lastIndex, to: range.lowerBound)
            let end = beg + text.distance(from: range.lowerBound, to: range.upperBound)
            lastIndex = range.upperBound
            lastPos = end
            bounds.append(ConsCell(car: beg, cdr: end))
        }

        return bounds
    }

    func wordAtPointOrForwardHelper(text: String, curPos: Int) throws -> WordBound {
        let curIdx = text.index(text.startIndex, offsetBy: curPos)

        self.tokenizer.string = text
        let WordBound = self.tokenizer.tokenRange(at: curIdx)

        let start = text.distance(from: text.startIndex, to: WordBound.lowerBound)
        let end = start + text.distance(from: WordBound.lowerBound, to: WordBound.upperBound)

        return ConsCell(car: start, cdr: end)
    }

    func Init(_ env: Environment) throws {
        try env.defun(self.funcName("do-split"),
                      with: """
                            Split ARG1 into a list of words.

                            Return an array of bounds.
                            A bound is a cons with the starting position and the ending position of a word.
                            """,
                      function: self.doSplitHelper(text:))

        try env.defun(self.funcName("word-at-point-or-forward"),
                      with: """
                            Return the range of the word at ARG2 in ARG1.

                            If ARG2 is at bound of two words, return the range of the word forward.

                            This function does not tokenize the whole ARG1, so it is faster in some cases.
                            """,
                      function: self.wordAtPointOrForwardHelper(text:curPos:))
    }
}

func createModule() -> Module {
    EMT()
}
