1. Nothing but the Truth
true

2. Simple Math
4

3. Intro to Strings
"HELLO WORLD"

4. Intro to List
:a :b :c

5. List: conj
'(1 2 3 4)

6. Intro to Vectors
:a :b :c

7. Vectors: conj
[1 2 3 4]

8. Intro to Sets
#{:a :b :c :d}

9. Sets: conj
2

10. Intro to Maps
20

11. Maps: conj
[:b 2]

12. Intro to Sequences

3

13. Sequences: rest
[20 30 40]

14. Intro to Functions
8

15. Double Down
*2

16. Hello World
#(str "Hello, " % \!)

17. Sequences: map
'(6 7 8)
18. Sequences: filter

'(6 7)

19. Last Element
#(nth %1 (- (count %1) 1))

20. Penultimate Element
#(nth % (- (count %) 2))

21. Nth Element
#(first (drop %2 %1))

22. Count a Sequence
#(reduce (fn [i j](inc i)) 0 %)

23. Reverese a Sequence
reduce conj ()

24. Sum It All Up
reduce +

25. Find the odd numbers
filter odd?

26. Fibonacci Sequence
#(take % (map second (iterate (fn [[fst snd]] [snd (+ fst snd)]) [0 1])))

27. Palindrome Detector
#(= (seq %) (reverse %))

28. Flatten a Sequence
#(filter (complement sequential?) (tree-seq sequential? identity %))

29. Get the Caps
#(apply str (re-seq #"[A-Z]" %))

30. Compress a Sequence
#(reverse (reduce (fn [a b] (if (= (first a) b) a (conj a b))) nil %))

31. Pack a Sequence
partition-by identity

32. Duplicate a Sequence
reduce #(conj  %1 %2 %2) []

33. Replicate a Sequence
#(mapcat (partial repeat %2) %1)

34. Implement range
#(take (- %2 %1) (iterate inc %1))

35. Local bindings
7

36. Let it Be
[z 1, y 3, x 7]

37. Regular Expressions
"ABC"

38. Maximum value
(fn [& arg] (reduce #(if (> %1 %2) %1 %2) arg))

39. Interleave Two Seqs
mapcat list

40. Interpose a Seq
#(vec (drop-last (mapcat (partial conj (list %1)) %2)))

41. Drop Every Nth Item
#(vec (apply concat (partition-all (dec %2) %2 %1)))

42. Factorial Fun
#(apply * (range 1 (inc %)))

43. Reverse Interleave
#(apply map list (partition-all %2 %1))

44. Rotate Sequence
#(drop (mod %1 (count %2)) (take (+ (mod %1 (count %2)) (count %2)) (cycle %2)))

45. Intro to Iterate
'(1 4 7 10 13)

46. Flipping out
(fn [f]#(f %2 %1))

47. Contain Yourself
4

48. Intro to some
6

49. Split a sequence
#(list (take %1 %2) (drop %1 %2))

50. Split by Type
#(map val (group-by class %))

51. Advanced Destructuring
[1 2 3 4 5]

52. Intro to Destructuring
(vector c e)

53. Longest Increasing Sub-Seq
!!!!!!

54. Partition a Sequence
#(letfn
     [(partition'[n s] 
        (lazy-seq 
         (when 
             (<= n (count s)) 
           (cons 
            (take n s) 
            (partition' n (drop n s))))))] 
   (partition' %1 %2))

56. Find Distinct Items
(fn [xs]
  (loop [xs' xs result []]
    (if (empty? xs') result
        (recur
         (remove #(= (first xs') %) xs') (conj result (first xs'))))))

57. Simple Recursion
'(5 4 3 2 1)

58. Function Composition
(fn [& fs](letfn [(comp' 
                    ([] identity)
                    ([f] f)
                    ([f g] (fn 
                             ([] (f (g)))
                             ([& x] (f (apply g x)))))
                    ([f g & h] (reduce comp' f (list* g h))))]
            (apply comp' fs)))

59. Juxtaposition
(fn [& f] (fn [& x] (map #(apply %1 x) f)))

60. Sequence Reductions
(letfn [(reductions'
  ([f init [x & xs]] 
    (lazy-seq 
      (let [redu ((fnil f 0 0) init x)] 
        (if (nil? x) 
          (list init) 
          (cons init (reductions' f redu xs)))))))]
  (fn 
    ([f [x & xs]] (reductions' f x xs))
    ([f init x] (reductions' f init x))))
    

61. Map Construction
(comp (partial apply hash-map) (partial mapcat #(list %1 %2)))

62. Re-implement Iterate
(fn [f n](letfn [(itr [x] (lazy-seq (cons x (itr (f x)))))] (itr n)))

63. Group a Sequence
(fn [f ns] (apply merge
                  (map #(hash-map (f (first %1)) (vec %1))
                             (partition-by f (sort-by f ns)))))

64. Intro to Reduce
+

65. Black Box Testing
(fn [x]
  (condp #(= %1 (empty %2)) x
    '{} ':map
    '#{} ':set
    (if (reversible? x) ':vector :list)))


66. Greatest Common Divisor
(fn [& ns](apply min
                 (apply concat
                        (take-while
                         #(not (zero? (first %)))
                         (iterate (fn[[a b]] (list (rem b a) a)) (sort ns))))))

67. Prime Numbers
(fn [c](take c
             (filter (fn [n](not-any?  #(zero?  (mod n %1)) (range 2 n)))
                       (iterate inc 2))))
b
68. Recurring Theme
'(7 6 5 4 3)

69. Merge with a Function
(fn [f x & xs] 
  (letfn [(merge-with-sub [init y] 
      (update-in init (vector (first y)) (fn [k] (let [v (second y)] (if (nil? k) v (f k v))))))] 
        (reduce #(if (seq? %2) (merge-with-sub %1 %2) (reduce merge-with-sub %1 %2)) x xs)))


70. Word Sorting
#(sort-by clojure.string/lower-case  (re-seq #"\w+" %))

71. Rearranging Code: ->
last

72. Rearranging Code: ->>
reduce +

73. Analyze a Tic-Tac-Toe Board
!!!!!!

74. Filter Perfect Squares
(fn [x] 
  (apply str
    (interpose \,
      (filter #(= % (int (Math/pow (int (Math/sqrt %)) 2))) 
      (map #(Integer/parseInt %)
        (re-seq #"[0-9]+" x))))))


75. Euler's Totient Function
(fn [x]
  (letfn[(gcd [x y] (let[r (rem y x)] (if (zero? r) x (gcd r x))))]
    (count 
      (filter #(= 1 (gcd % x))
        (range 1 (inc x))))))

76. Intro to Trampoline
[1 3 5 7 9 11]

77. Anagram Finder
(fn [l] 
  (set 
    (map #(set (fnext %))  
      (filter #(<= 2 (count (second %))) 
        (group-by #(set %) l)))))

78. Reimplement Trampoline
(fn [f & xs] 
  (loop[tr (apply f xs)] 
    (if-not (fn? tr) tr (recur (tr)))))

79. Triangle Minimal Path
!!!!!!  

80. Perfect Numbers
(fn [x]
  (= x 
    (apply + 
      (filter #(zero? (mod x %)) (range 1 x)))))

81. Set Intersection
(fn [fst snd] (set (filter #(contains? fst %) snd)))

82. Word Chains
!!!!!!

83. A Half-Truth
(fn [& bools] (not (nil? (and (some true? bools) (some false? bools)))))

84. Transitive Closure
!!!!!!

85. Power Set
(fn [x] (set 
  (reduce 
    (fn [ps x] (into ps (map #(conj % x) ps))) 
      (list (set nil)) (apply list x))))

86. Happy numbers
(fn [x]
  (letfn [(happy-number 
    ([l n] (cond 
      (= n 1) true 
      (not (apply distinct? l)) false 
      :else 
        (happy-number 
          (cons n l) 
            (int (apply + (map #(Math/pow % 2) (map #(int (- (int %) (int \0))) (str n)))))))))] 
    (happy-number (list 0) x)))



87. nill 

88. Symmetric Difference
#(set (concat (apply disj %1 %2) (apply disj %2 %1)))

89. Graph Tour
!!!!!!

90. Cartesian Product
#(set (for [x (vec %1) y (vec %2)] [x y]))

91. Graph Connectivity
!!!!!!

92. Read Roman numerals
!!!!!!

93. Partially Flatten a Sequence
!!!!!!

94. Game of Life
!!!!!!

95. To Tree, or not to Tree
#(letfn [(tree-detect [[a b c :as xs]]
           (and (= 3 (count xs))
                (not (sequential? a))
                (if (nil? b) true (if (sequential? b) (tree-detect b) false))
                (if (nil? c) true (if (sequential? c) (tree-detect c) false))))]
   (tree-detect  %))

96. Beauty is Symmetry 
#(letfn [(re-reverse [[root l-node r-node]] 
           (list root
                 (if (sequential? r-node) (re-reverse r-node) r-node) 
                 (if (sequential? l-node) (re-reverse l-node) l-node)))] 
   (= (second %) (second (re-reverse %))))

97. Pascal's Triangle
(fn [n] (nth
         (iterate #(mapv (partial apply +) (partition-all 2 1 (cons 0 %))) [1])
         (dec n)))

98. Equivalence Classes
(fn [f x] (set 
  (map #(set (second %)) (group-by f (apply list x))))) 

99. Product Digits
#(mapv (fn [n] (Integer/parseInt (str n))) (str (* %1 %2)))

100. Least Common Multiple
(fn [& ns]
  (reduce
   #(/ (* %1 %2)
       ((fn [x y]
          (loop[x' (max x y) y' (min x y)]
            (let [rem-num (mod x' y')]
              (if (zero? rem-num) y'
                  (recur y' rem-num))))) %1 %2)) ns))

101. Levenshtein Distance
!!!!!!

102. intoCamelCase
(fn [xs] 
  (apply str 
    (let [[w & ws] (re-seq #"[a-zA-Z]+" xs)]
      (cons w
        (mapcat (fn [[y & ys]] (cons (Character/toUpperCase y) ys)) ws)))))

103. Generating k-combinations
!!!!!!

104. Write Roman Numerals
!!!!!!

105. Identify keys and values
(fn [x] (apply array-map 
  (mapcat #(if (keyword (first %)) (interpose [] %) (list (apply vector %))) 
    (partition-by keyword? x))))


106. Number Maze
!!!!!!

107. Simple closures
(fn [x] (fn [y] (int (Math/pow y x))))

108. Lazy Searching
!!!!!!

109. nil

110. Sequence of pronunciations
!!!!!!

111. Crossword puzzle
!!!!!!

112. Sequs Horribilis
!!!!!!

113. Making Data Dance
!!!!!!

114. Global take-while
!!!!!!

115. The Balance of N
(fn [x] 
  (let[il (map #(- (int  %) (int \0)) (str x)) 
       sizediv2 (quot (count il) 2)]  
    (= 
      (apply + (take sizediv2 il)) 
      (apply + (take-last sizediv2 il)))))

116. Prime Sandwich
!!!!!!

117. For Science!
!!!!!!

118. Re-implement Map
#(letfn[
        (map' [x]
          (lazy-seq
           (cons
            (%1 (first x))
            (let[xs (rest x)]
              (when-not (empty? xs) (map' xs))))))]
   (map' %2))

119. Win at Tic-Tac-Toe
!!!!!!

120. Sum of square of digits
#(reduce
  (fn [x y]
    (if
        (neg?
         (reduce
          (fn [a b]
            (int (- a
                    (Math/pow (Integer/parseInt (str b)) 2))))
          (Integer/parseInt y) y))
      (inc x) x)) 0 (map str %))

121. Universal Computation Engine
!!!!!!

122. Read a binary number
(fn [ns]
  (apply +
         (map #(* %1 %2)
              (map
               (fn [ms]
                 (Integer/parseInt (str ms)))
               (apply list (reverse ns)))
              (iterate #(* 2 %) 1))))

123. nil

124. Analyze Reversi
!!!!!!

125. Gus' Quinundrum
!!!!!!

126. Through the Looking Class
Class

127. Love Triangle
!!!!!!

128. Recognize Playing Cards
(fn [[s r]]
  (array-map :suit
             ({:D :diamond, :H :heart, :S :speade, :C :club}
              (keyword (str s)))
             :rank
             (condp = r \A 12 \T 8 \J 9 \Q 10 \K 11
                    (- (Integer/parseInt (str r)) 2))))

129. nil

130. Tree reparenting
!!!!!!

131. Sum Some Set Subsets
!!!!!!

132. Insert between two items
!!!!!!

133. nil

134. A nil key
#(and
  (contains? %2 %1)
  (nil? (%2 %1)))

135. Infix Calculator
(fn [& ls]
  (letfn [
          (calc ([x] x)
            ([x y & z]
             (y (apply calc z) x)))]
    (apply calc (reverse ls))))

136. nil

137. Digits and bases
!!!!!!

138. Squares Squared
!!!!!!

139. nil

140. Veitch, Please!
!!!!!!

141. Tricky card games
!!!!!!

142. nil

143. dot product
(fn [xs ys]
  (reduce +
          (map #(* %1 %2) xs ys)))

144. Oscilrate
!!!!!!

145. For the win
'(1 5 9 13 17 21 25 29 33 37)

146. Trees into tables
(fn [ns]
    (into (array-map) 
        (for [[x y]ns 
            [y' z] y]
             (vec [(vector x y') z]))))

147. Pascal's Trapezoid
#(letfn [
         (p-trapezoid [ns]
           (lazy-seq
            (cons ns
                  (p-trapezoid
                   (map
                    (partial apply +')
                    (partition-all 2 1 (cons 0 ns)))))))]
   (p-trapezoid %))

148. The Big Divide
!!!!!!

149. nil

150. Palindromic Numbers
!!!!!!

151. nil

152. Latin Square Slicing

153. Pairwise Disjoint Sets
#(=
  (count (apply concat %))
  (count (set (apply concat %))))

154. nil

155. nil

156. Map Defaults
#(zipmap %2 (repeat %1))

157. Indexing Sequences
#(mapv
  (comp vec reverse)
  (map-indexed vector %))

158. Decurry
!!!!!!

159. nil

160. nil

161. Subset and Superset
#{1 2}

162. Logical falsity and truth
1

163. nil

164. Language of a DFA
!!!!!!

165. nil

166. Comparisons
(fn [x y z] (cond (x y z) :lt (x z y) :gt :else :eq))

167. nil

168. Infinite Matrix
!!!!!!

169. nil

170. nil

171. Intervals
!!!!!!

172. nil

173. Intro to Destructuring 2
x y

174. nil

175 nil

176 nil

177. Balancing Brackets
!!!!!!

178. Best Hand
!!!!!!

