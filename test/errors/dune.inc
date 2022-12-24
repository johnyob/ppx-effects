; -------- Test: `continue_without_value_case.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets continue_without_value_case.actual)
 (deps
  (:pp pp.exe)
  (:input continue_without_value_case.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff continue_without_value_case.expected
    continue_without_value_case.actual)))

; -------- Test: `dangling.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets dangling.actual)
 (deps
  (:pp pp.exe)
  (:input dangling.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff dangling.expected dangling.actual)))

; -------- Test: `discontinue_without_value_case.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets discontinue_without_value_case.actual)
 (deps
  (:pp pp.exe)
  (:input discontinue_without_value_case.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff discontinue_without_value_case.expected
    discontinue_without_value_case.actual)))

; -------- Test: `invalid_payload_type_in_continue.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets invalid_payload_type_in_continue.actual)
 (deps
  (:pp pp.exe)
  (:input invalid_payload_type_in_continue.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff invalid_payload_type_in_continue.expected
    invalid_payload_type_in_continue.actual)))

; -------- Test: `invalid_payload_type_in_discontinue.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets invalid_payload_type_in_discontinue.actual)
 (deps
  (:pp pp.exe)
  (:input invalid_payload_type_in_discontinue.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff invalid_payload_type_in_discontinue.expected
    invalid_payload_type_in_discontinue.actual)))

; -------- Test: `invalid_payload_type_in_match.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets invalid_payload_type_in_match.actual)
 (deps
  (:pp pp.exe)
  (:input invalid_payload_type_in_match.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff invalid_payload_type_in_match.expected
    invalid_payload_type_in_match.actual)))

; -------- Test: `invalid_payload_type_in_try.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets invalid_payload_type_in_try.actual)
 (deps
  (:pp pp.exe)
  (:input invalid_payload_type_in_try.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff invalid_payload_type_in_try.expected
    invalid_payload_type_in_try.actual)))

; -------- Test: `invalid_scrutinee_in_discontinue.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets invalid_scrutinee_in_discontinue.actual)
 (deps
  (:pp pp.exe)
  (:input invalid_scrutinee_in_discontinue.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff invalid_scrutinee_in_discontinue.expected
    invalid_scrutinee_in_discontinue.actual)))

; -------- Test: `malformed_payload.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets malformed_payload.actual)
 (deps
  (:pp pp.exe)
  (:input malformed_payload.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff malformed_payload.expected malformed_payload.actual)))

; -------- Test: `malformed_payload_hint_missing_comma.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets malformed_payload_hint_missing_comma.actual)
 (deps
  (:pp pp.exe)
  (:input malformed_payload_hint_missing_comma.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff malformed_payload_hint_missing_comma.expected
    malformed_payload_hint_missing_comma.actual)))

; -------- Test: `match_without_value_case.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets match_without_value_case.actual)
 (deps
  (:pp pp.exe)
  (:input match_without_value_case.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff match_without_value_case.expected match_without_value_case.actual)))

; -------- Test: `when_inside_effect.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets when_inside_effect.actual)
 (deps
  (:pp pp.exe)
  (:input when_inside_effect.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff when_inside_effect.expected when_inside_effect.actual)))


