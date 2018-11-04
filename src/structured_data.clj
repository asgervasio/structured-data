(ns structured-data)

(defn do-a-thing [x]
  (let [double (+ x x)]
    (Math/pow double double)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[v1 v2 v3]]
    (+ v1 v3))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
    (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
    (if (= (height rectangle) (width rectangle))
      true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
   (let [[[x1 y1] [x2 y2]] rectangle
         [px py] point]
     (if (<= x1 px x2) (if (<= y1 py y2) true false) false)))

(defn contains-rectangle? [outer inner]
      (let [[px py] inner]
        (if (contains-point? outer px) (if (contains-point? outer py) true false) false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if ( > (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [authors (get book :authors)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (if (= (get author :death-year) nil) true false))

(defn element-lengths [collection]
  (map (fn [v] (count v)) collection))

(defn second-elements [collection]
  (map (fn [v] (second v)) collection))

(defn titles [books]
  (map (fn [book] (get book :title)) books))

(defn monotonic? [a-seq]
  (if (apply <= a-seq) true 
    (if (apply >= a-seq) true false)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)
  ))

(defn contains-duplicates? [a-seq]
  (if ( = (count a-seq) (count (distinct a-seq))) false true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
       birth (:birth-year author)
       death (:death-year author)]
    (cond birth (str name " (" birth " - " death ")")
      :else (str name))))

(defn authors->string [authors]
(apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str
     (:title book)
     ", written by "
(authors->string (:authors book))))

(defn books->string [books]
  (let [book-count
          (count books)
        book-count-s
          (cond
             (= book-count 0) "No books"
             (= book-count 1) "1 book. "
             :else (str book-count " books. "))
        book-list
          (apply str (interpose ". " (map book->string books)))
        ]
    (str book-count-s book-list ".")))


(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter
          (fn [a] (= (:name a) name))
authors)))

(defn living-authors [authors]
 (filter alive? authors))

(defn has-a-living-author? [book]
 (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
 (filter has-a-living-author? books))

; %________%
