; -------- Test: `exhaustive_handlers.ml` --------; The PPX-dependent executable under test
(executable
 (name exhaustive_handlers)
 (modules exhaustive_handlers)
 (preprocess (pps ppx_effects)))

; Run the PPX on the `.ml` file
(rule
 (targets exhaustive_handlers.actual)
 (deps
  (:pp pp.exe)
  (:input exhaustive_handlers.ml))
 (action
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff exhaustive_handlers.expected exhaustive_handlers.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (run ./exhaustive_handlers.exe)))

; -------- Test: `let_effect.ml` --------; The PPX-dependent executable under test
(executable
 (name let_effect)
 (modules let_effect)
 (preprocess (pps ppx_effects)))

; Run the PPX on the `.ml` file
(rule
 (targets let_effect.actual)
 (deps
  (:pp pp.exe)
  (:input let_effect.ml))
 (action
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff let_effect.expected let_effect.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (run ./let_effect.exe)))

; -------- Test: `main.ml` --------; The PPX-dependent executable under test
(executable
 (name main)
 (modules main)
 (preprocess (pps ppx_effects)))

; Run the PPX on the `.ml` file
(rule
 (targets main.actual)
 (deps
  (:pp pp.exe)
  (:input main.ml))
 (action
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff main.expected main.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (run ./main.exe)))

; -------- Test: `rebind.ml` --------; The PPX-dependent executable under test
(executable
 (name rebind)
 (modules rebind)
 (preprocess (pps ppx_effects)))

; Run the PPX on the `.ml` file
(rule
 (targets rebind.actual)
 (deps
  (:pp pp.exe)
  (:input rebind.ml))
 (action
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff rebind.expected rebind.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (run ./rebind.exe)))

; -------- Test: `scrutinee.ml` --------; The PPX-dependent executable under test
(executable
 (name scrutinee)
 (modules scrutinee)
 (preprocess (pps ppx_effects)))

; Run the PPX on the `.ml` file
(rule
 (targets scrutinee.actual)
 (deps
  (:pp pp.exe)
  (:input scrutinee.ml))
 (action
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff scrutinee.expected scrutinee.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (run ./scrutinee.exe)))

; -------- Test: `shadowed_raise.ml` --------; The PPX-dependent executable under test
(executable
 (name shadowed_raise)
 (modules shadowed_raise)
 (preprocess (pps ppx_effects)))

; Run the PPX on the `.ml` file
(rule
 (targets shadowed_raise.actual)
 (deps
  (:pp pp.exe)
  (:input shadowed_raise.ml))
 (action
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff shadowed_raise.expected shadowed_raise.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (run ./shadowed_raise.exe)))

; -------- Test: `shallow.ml` --------; The PPX-dependent executable under test
(executable
 (name shallow)
 (modules shallow)
 (preprocess (pps ppx_effects)))

; Run the PPX on the `.ml` file
(rule
 (targets shallow.actual)
 (deps
  (:pp pp.exe)
  (:input shallow.ml))
 (action
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff shallow.expected shallow.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (run ./shallow.exe)))

; -------- Test: `when_guards.ml` --------; The PPX-dependent executable under test
(executable
 (name when_guards)
 (modules when_guards)
 (preprocess (pps ppx_effects)))

; Run the PPX on the `.ml` file
(rule
 (targets when_guards.actual)
 (deps
  (:pp pp.exe)
  (:input when_guards.ml))
 (action
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff when_guards.expected when_guards.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (run ./when_guards.exe)))


