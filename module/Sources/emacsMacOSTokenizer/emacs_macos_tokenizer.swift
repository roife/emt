import EmacsSwiftModule
import NaturalLanguage

class emacsMacOSTokenizer: Module {
    typealias WordBound = ConsCell<Int, Int>
    typealias WordAndBound = ConsCell<String, ConsCell<Int, Int>>

    let isGPLCompatible = true
    let tokenizer = NLTokenizer(unit: .word)

    func funcName(_ name: String) -> String {
        let funcNamePrefix = "emacs-macos-tokenizer--"
        let funcNameSuffix = "-helper"
        return funcNamePrefix + name + funcNameSuffix
    }

    func splitHelper(text: String) throws -> [WordAndBound] {
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

        return zip(WordBounds, bounds).map { ConsCell(car: String(text[$0]), cdr: $1) }
    }

    func splitWithoutBoundsHelper(text: String) throws -> [String] {
        self.tokenizer.string = text
        return self.tokenizer.tokens(for: text.startIndex..<text.endIndex).map { String(text[$0]) }
    }

    func wordAtPointOrForwardHelper(text: String, curPos: Int) throws -> WordAndBound {
        let curIdx = text.index(text.startIndex, offsetBy: curPos)

        self.tokenizer.string = text
        let WordBound = self.tokenizer.tokenRange(at: curIdx)

        let start = text.distance(from: text.startIndex, to: WordBound.lowerBound)
        let end = start + text.distance(from: WordBound.lowerBound, to: WordBound.upperBound)
        let range = ConsCell(car: start, cdr: end)

        return ConsCell(car: String(text[WordBound]), cdr: range)
    }

    func Init(_ env: Environment) throws {
        try env.defun(self.funcName("do-split"),
                      with: """
                            Split ARG1 into a list of words.

                            Return a list of cons, each of which has a word and its bound.
                            Bound is a cons of the start and end position of the word.
                            """,
                      function: self.splitHelper(text:))

        try env.defun(self.funcName("do-split-without-bounds"),
                      with: """
                            Split ARG1 into a list of words.
                            """,
                      function: self.splitWithoutBoundsHelper(text:))

        try env.defun(self.funcName("word-at-point-or-forward"),
                      with: """
                            Return the range of word forward at ARG2 in ARG1.

                            If current point is at bound of a word, return the one forward.
                            """,
                      function: self.wordAtPointOrForwardHelper(text:curPos:))
    }
}

func createModule() -> Module {
    emacsMacOSTokenizer()
}
