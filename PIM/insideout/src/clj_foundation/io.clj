(ns clj-foundation.io
  "io address three primary concerns:
  * Smoothing necessary Java interop for IO operations, particularly between strings and streams/writers
  * Extending clojure.java.io to allow specifying an environment variable to use as the input source
  * (de)Serializing Clojure data structures"
  (:require [clj-foundation.errors :as err]
            [clj-foundation.templates :as template]
            [clj-foundation.patterns :as p]
            [clj-foundation.data :as data]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn])

  (:import [java.io InputStream File FileInputStream PrintWriter ByteArrayInputStream ByteArrayOutputStream]
           [java.net URI URL Socket]))


;; File path stuff

(defn expand-path [p]
  (let [home (System/getProperty "user.home")]
    (if (str/includes? p "~")
      (str/replace p "~" home)
      p)))

(defn full-path [^String relative-path] (.getCanonicalPath (io/file relative-path)))

(defn file-details [fileOrName]
  (letfn [(extension [f]
            (let [name (.getName f)
                  last-dot (.lastIndexOf name ".")]
              (if (and (not= last-dot -1) (not= last-dot -))
                (.substring name (+ last-dot 1))
                "")))]

    (let [f (if (string? fileOrName) (io/file fileOrName) fileOrName)]
      {:full-name (.getCanonicalPath f)
       :short-name (.getName f)
       :extension (extension f)
       :directory (.isDirectory f)
       :hidden (.isHidden f)
       :last-modified-millis (.lastModified f)})))

(comment
  (println (.list (File. ".")))
  (clojure.pprint/pprint (file-details "tests.edn"))
  ,)

;; From the deprecated clojure.contrib.io library
;; https://github.com/clojure/clojure-contrib/blob/b6c6f0fa165975c416c7d135d1a844353527493b/src/main/clojure/clojure/contrib/io.clj#L352
(defmacro with-out-writer
  "Opens a writer on f, binds it to *out*, and evaluates body.
  Anything printed within body will be written to f."
  [f & body]
  `(with-open [stream# (clojure.java.io/writer ~f)]
     (binding [*out* stream#]
       ~@body)))


(defn string-output-writer
  "Create a java.io.PrintWriter whose .toString contains the data printed to it."
  []
  (let [byte-array-stream (ByteArrayOutputStream.)]
    (proxy [PrintWriter] [byte-array-stream]
      (toString []
        (.flush this)
        (str byte-array-stream)))))


(defmacro with-err-str
  "Anything printed within body to *err* will be returned as a string."
  [& body]
  `(let [stream# (string-output-writer)]
     (binding [*err* stream#]
       ~@body
       (str stream#))))


(defmacro print-writer
  "Print thing to writer"
  [stream thing]
  `(.print ~stream ~thing))


(defmacro println-writer
  "Println thing to writer"
  [stream thing]
  `(.println ~stream ~thing))


(defn string-input-stream
  "Return a java.io.InputStream containing the input string"
  [input]
  (ByteArrayInputStream. (.getBytes input)))


(defn serialize
  "Print a data structure to a file so that we may read it in later."
  [#^String filename data-structure]
  (with-out-writer
    (java.io.File. filename)
    (binding [*print-dup* true] (prn data-structure))))


(defn deserialize
  "Note that this uses `read` and will execute code!  Use clojure.edn (or `edn-seq` below) for untrusted data."
  [filename]
  (with-open [r (java.io.PushbackReader. (java.io.FileReader. filename))]
    (read r)))


;; Note: Records and such must define a serialVersionUID field for this to support migrations
(defn bin-serialize [filename o]
    (with-open [outp (-> (java.io.File. filename) java.io.FileOutputStream. java.io.ObjectOutputStream.)]
        (.writeObject outp o)))


(defn bin-deserialize [filename]
    (with-open [inp (-> (java.io.File. filename) java.io.FileInputStream. java.io.ObjectInputStream.)]
        (.readObject inp)))


(defn swallow-eof
  "Ignore an EOF exception raised when consuming seq."
  [seq]
  (-> (try
       (cons (first seq) (swallow-eof (rest seq)))
       (catch java.lang.RuntimeException e
         (when-not (= (.getMessage e) "EOF while reading")
           (throw e))))
     lazy-seq))


(defn close-on-eof
  "Closes stream on EOF exception raised when consuming seq."
  [stream seq]
  (-> (try
       (cons (first seq) (close-on-eof stream (rest seq)))
       (catch java.lang.RuntimeException e
         (when-not (= (.getMessage e) "EOF while reading")
           (throw e))
         (.close stream)
         ::EOF))
     lazy-seq))


(defn edn-seq
  "Returns the objects from file/stream as a lazy sequence.

  The nullary form reads from *in*.

  Otherwise, if `in` is a `File` or a `String`, opens a `FileInputStream`
  on it, returns `::EOF` as the final element, and automatically closes
  the stream it opened on EOF.

  If `in` is `*in*` or another already an opened stream, end-of-file handling
  is as specified by `opts` as defined by `clojure.edn/read`."
  ([]
   (edn-seq *in*))

  ([in]
   (edn-seq {} in))

  ([opts in]
   (cond
     (instance? InputStream in)  (lazy-seq (cons (clojure.edn/read opts in)
                                                 (edn-seq opts in)))

     (or (string? in)
         (instance? File in))    (let [stream (FileInputStream. in)]
         (close-on-eof stream (edn-seq opts stream)))

     :else                       (throw (IllegalArgumentException.
                                         (str "Can't read seq from " (type in)))))))


(comment
  (with-open [stream (java.io.PushbackReader. (clojure.java.io/reader "foo.txt"))]
    (dorun (map println (swallow-eof (edn-seq stream)))))
  ,)


;; Schema stubs - Leaving these in because they make good documentation

(defn one [& xs]          [:schema/one xs])
(defn optional-key [& xs] [:schema/optional-key (first xs)])
(defn cond-pre [& xs]     [:schema/cond-pre xs])
(defn explain [schema]    (.toString schema))

;; Data types / schemas specifying the kinds of types from which we can stream data
;; Used to be `defschema' but I've decided to be schema/specsX/whatever-agnostic for now

(def ByteArray (Class/forName "[B"))
(def clojure-file-inputs
  "The data types allowed as input to io/input-stream"
  [String InputStream File URI URL Socket ByteArray])

(def ResourcePath
  "A path to a JAR file resource on the classapth."
  String)

(def EnvironmentVariable
  "The name of a java system variable or O/S environment variable (in that order) that may override
  a ResourcePath."
  String)

(def ResourceOverrideVector
  "Read the resource at ResourcePath unless a Java system variable or an O/S EnvironmentVariable
   is present with the specified name.  In that case, use the value referenced by EnvironmentVariable
   as a file system path to the actual file."
  [(one EnvironmentVariable "Java system or O/S environment variable that may override the recource")
   (one ResourcePath "JAR resource on the classpath")])

(def ResourceOverrideMap
  "Read the file specified by :file or the resource specified by :resource.  If both entries are defined,
  the :file entry takes precedence over the :resource entry."
  {(optional-key :file) String
   (optional-key :resource) String})

(def ClojureFileInputs
  "The set of types Clojure allows for opening files/streams"
  (apply cond-pre clojure-file-inputs))

(def ExtendedFileInputs
  "Clojure's file inputs plus extensions for reading from either Jar resources or Files."
  (apply cond-pre ResourceOverrideVector ResourceOverrideMap clojure-file-inputs))


(defn- resolve-envar-override
  "If environment-variable is defined as a Java system variable or O/S variable (in that order), retrieve
  its value, and return that value, coerced to java.io.File.  Otherwise, return the original resource-path
  as a URL."
  [environment-variable resource-path]
  (let [config-envar-name (str "${" environment-variable "}")
        substitution-map {(keyword environment-variable) resource-path}
        file-location (template/subst<- config-envar-name substitution-map)]

    (if (= resource-path file-location)
      (io/resource file-location)
      (io/as-file file-location))))


(defn- resolve-override-map
  [{:keys [file resource]}]
  (cond
    file     (io/file file)
    resource (io/resource resource)
    :else    (throw (IllegalArgumentException. (str "Don't know how to open a file from: " map)))))


;; File reading...

(defn normalize-file-input
  "If the file-input is a string and can be converted to a resource URL, return that.
  If the file-input is a FileLocation, translate the :resource string to a URL and return that,
  otherwise return a File object wrapping the :file entry.  Otherwise, return the original file-input."
  [file-input]

  (let [result
        (cond
          (vector? file-input) (apply resolve-envar-override file-input)
          (map? file-input)    (resolve-override-map file-input)
          :else file-input)]

    (try
      (if (string? result)
        (data/replace-nil (io/resource result) result)
        result)
      (catch Throwable e
        (throw (IllegalArgumentException.
                (str "Expected result to be one of " (explain ClojureFileInputs)
                     " but found " result)
                e))))))



(defn input-stream
  "Read and return a text file as an InputStream.  Supported sources are the same as io/input-stream, with
  the following additions: ResourceOverrideMap and ResourceOverrideVector.  See ExtendedFileInputs for details."
  [file-input]
  (io/input-stream (normalize-file-input file-input)))


(defn read-file
  "Read and return a text file as a String.  Supported sources are the same as io/input-stream, with
  the following additions: ResourceOverrideMap and ResourceOverrideVector.    See ExtendedFileInputs for details."
  [file-input]
  (with-open [input (input-stream file-input)]
    (slurp input)))


(defn resource-as-string
  "1-arg variant: Reads the specified classpath resource and returns its contents as a string.
   2-arg variant: [\"ENVAR\" \"resource-file.txt\"] - Allows resource-file to be overridden as-in read-file."
  [& resource-spec]
  (let [argc (count resource-spec)]
    (cond
      (= argc 1) (read-file {:resource (first resource-spec)})
      (= argc 2) (read-file [(first resource-spec) (second resource-spec)])
      :else (throw (IllegalArgumentException.
                    (str "resource-as-string: Illegal arg list: " resource-spec))))))


(defn- parse-extended-file-location [input subs]
  (if (odd? (count subs))
    [[input (first subs)] (rest subs)]
    [input subs]))


(defn read-template
  "Read a file from file-location, applying any variable substitutions specified
  in the file using the keyword/value pairs in substitutions.

  Supported sources are the same as io/input-stream, with the following additions:
  ResourceOverrideMap and ResourceOverrideVector."

  [file-location & ex-substitutions]

  (let [[extended-file-location substitutions] (parse-extended-file-location file-location ex-substitutions)
        symbol-table (template/subst-map<- substitutions)
        file-contents-with-vars (read-file extended-file-location)
        resolved-file-contents (template/subst<- file-contents-with-vars symbol-table)]
    resolved-file-contents))
