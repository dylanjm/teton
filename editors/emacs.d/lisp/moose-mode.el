;;; moose-mode.el --- Major mode for editing MOOSE input files
;;;
;;; Usage:
;;;
;;; Commentary:
;;;
;;; License:
;;;
;;; Code:
(require 'cl-lib)

(defvar moose-mode-hook nil)

(defgroup moose ()
  "Major mode editing MOOSE input files"
  :group 'languages
  :prefix "moose-")

;;; autoload
(add-to-list 'auto-mode-alist '("\\.i\\'" . moose-mode))

(defconst moose-block-identifiers-regex
  (regexp-opt '("Adaptivity" "AuxKernels" "Bounds" "AuxScalarKernels" "AuxVariables"
                "BCs" "Constraints" "Controls" "CoupledProblems" "DGKernels" "Dampers"
                "Debug" "DeprecatedBlock" "DiracKernels" "Executioner" "Functions"
                "GlobalParams" "ICs" "InterfaceKernels" "Kernels" "Materials" "Mesh"
                "MeshGenerators" "MeshModifiers" "Modules" "MultiApps" "NodalKernels"
                "NodalNormals" "Outputs" "Postprocessors" "Preconditioning" "Problem"
                "ScalarKernels" "Transfers" "UserObjects" "Variables" "VectorPostprocessors")
              'symbols))

(defconst moose-obj-identifiers-regex
  (regexp-opt '("Adaptivity" "Indicators" "InitialCondition" "Markers"
                "MortarInterfaces" "Periodic" "Predictor" "Quadrature"
                "TimePeriods" "TimeStepper")
              'symbols))

(defconst moose-support-func-regex
  (regexp-opt '("abs" "acos" "acosh" "arg" "asin" "atan" "atan2" "atanh" "cbrt"
                "ceil" "conj" "cos" "cosh" "cot" "csc" "exp" "exp2" "floor" "hypot"
                "if" "imag" "int" "log" "log10" "log2" "max" "min" "polar" "pow"
                "real" "sec" "sin" "sinh" "sqrt" "tan" "tanh" "trunc" "plog")
              'symbols))

(defconst moose-end-sub-block-regex
  (rx "[" (group (+ ".")) "]"))

(defconst moose-end-major-block-regex
  (rx "[" "]"))



(defconst moose-font-lock-keywords
  (list
   (cons moose-support-func-regex 'font-lock-keyword-face)
   (cons moose-block-identifiers-regex 'font-lock-keyword-face)))

(define-derived-mode moose-mode prog-mode "Moose"
  "Major mode for editing Moose input files"
  (set (make-local-variable 'font-lock-defaults) '(moose-font-lock-keywords)))
