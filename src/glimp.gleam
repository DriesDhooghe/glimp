//// The intent of this module called `Glimp`, short for `Gleam Imperative` is
//// to provide close alternatives to the traditional C features `while`,
//// `do while`, `for` and `switch` but in a functional language. This will
//// allow you to quickly port complex algorithms implemented in C or other
//// imperative languages to Gleam. Of course, it would be best to reimplement
//// these as functional algorithms. However, this is typically hard,
//// error-prone and very time consuming. With this module, an initial
//// `make-it-work` port becomes much easier. Once you get this initial port
//// working, you can focus your energy on implementing this in a more functional way.
////
//// For examples of how to use this library see glimp_test.gleam in the test
//// folder of the project.

import gleam/bool
import gleam/list
import gleam/option.{type Option}

/// C has a keyword `Break` that is used in switch statements and in `while`,
/// `do while` and `for` loops. It means: stop executing the code in the code
/// block at the point where Break is called and exit the case branch (switch)
/// or the loop immediately and completely.
/// It also has a `Continue` keyword that is only used in loops (`while`,
/// `do while` and `for`). Continue means: stop executing the code in the current
/// code block at the point where Continue is called and start a new iteration
/// of the loop.
/// I did not implement Continue since it is rather trivial to exit a current loop
/// by using one big if statement.
/// To implement Break, I created a custom type SwitchState (for Switch) and
/// LoopState (for loops) and expect the developer to set break in SwitchState
/// or LoopState in the code blocks of these types.
///
/// 1. IMPLEMENTING SWITCH
/// In C-syntax, the `switch` statement looks like this:
///
/// switch (expression) {
///   case x:
///     // code block for case x
///     break; // exit the switch statement after executing the code for case x
///   case y:
///     // code block for case y
///     // no break here, so if case y matches and its code block has been executed
///     // then execution will continue with checking and potentially executing case z
///     // if it also matches.
///   case z:
///     // code block for case z
///     break; // exit the switch statement after executing the code for case z
///   default: // Optional. Can be omitted in C.
///     // code block for default case (executed if no case matches)
/// }
///
/// We represent this as follows in our Gleam switch function
/// - an expression returning an generic type `a`
/// - a list of CaseBlocks. Each Caseblock consists of a match value (of type a)
///   (x, y, z in the example above) and a function (a -> #(b,Bool)) where b is
///   the result of executing the code block and the boolean indicates if a break
///   was called or not.
/// - a default function (a -> b) representing the optional default code block.
///   Note that this default function is not optional in the Gleam implementation
///   like it is in C.
///   If the C-code you are converting does not have a default branch then the
///   corresponding default function should simply return `a` (identity function).
//
//****** IMPORTANT TYPES AND FUNCTIONS ******
pub type CaseBlock(a, b) {
  CaseBlock(match: a, code: fn(a) -> #(b, Bool))
}

pub type LoopState(a) {
  LoopState(state: a, break: Bool)
  //a is the state the developer is managing
}

pub fn switch(
  expression expression: a,
  cases cases: List(CaseBlock(a, b)),
  default default: fn(a) -> b,
) -> Result(b, Nil) {
  //initial SwitchState for the case loop in the switch_helper function
  let switch_state =
    SwitchState(expression: expression, case_return: option.None, break: False)

  //transformations for the switch_helper function
  let transformed_cases = list.map(cases, transform_case)
  let transformed_default_function = transform_default(default)

  //loop through the cases by recursively calling switch_helper
  let result =
    switch_helper(switch_state, transformed_cases, transformed_default_function).case_return

  //get the result
  case result {
    option.Some(val) -> Ok(val)
    option.None -> Error(Nil)
  }
}

fn switch_helper(
  switch_state switch_state: SwitchState(a, b),
  transformed_cases transformed_cases: List(TransformedCaseBlock(a, b)),
  transformed_default_function transformed_default_function: fn(a) ->
    SwitchState(a, b),
) -> SwitchState(a, b) {
  let current_state = switch_state.expression
  let break = switch_state.break
  case break {
    True -> switch_state
    False -> {
      case transformed_cases {
        [] -> transformed_default_function(current_state)
        //run default codeblock and stop
        [first, ..rest] -> {
          let comparison = first.match == current_state
          case comparison {
            True -> {
              let new_state = first.transformed_code(current_state)
              //execute code block
              switch_helper(
                switch_state: new_state,
                transformed_cases: rest,
                transformed_default_function: transformed_default_function,
              )
            }
            False -> {
              switch_helper(
                switch_state: switch_state,
                transformed_cases: rest,
                transformed_default_function: transformed_default_function,
              )
            }
          }
        }
      }
    }
  }
}

///2. IMPLEMENTING WHILE WITH BREAK
/// In C syntax, `while` is defined as:
///
/// while (condition) {
///  // code block to be executed
///}
///
/// The `while` function in Gleam is stack-safe and checks the pre_run_condition
/// based on the communicated state at the beginning of an iteration of the while loop.
/// If the condition is true then the code in the code block handed to the `while`
/// function is run and generates a new LoopState in preparation for the next iteration.
/// The code in the code block can throw a Break just like in C.
pub fn while(
  // LoopState manages both the actual state (a) as well as whether or not Break was called.
  state state: a,
  pre_run_condition pre_run_condition: fn(a) -> Bool,
  code_to_run code_to_run: fn(a) -> LoopState(a),
) -> a {
  let transformed_state = LoopState(state: state, break: False)
  let transformed_code_to_run = transform_code_to_run(code_to_run)

  let result =
    while_helper(
      state: transformed_state,
      pre_run_condition: pre_run_condition,
      transformed_code_to_run: transformed_code_to_run,
    )
  case result {
    LoopState(a, _) -> a
  }
}

fn while_helper(
  state state: LoopState(a),
  pre_run_condition pre_run_condition: fn(a) -> Bool,
  transformed_code_to_run transformed_code_to_run: fn(LoopState(a)) ->
    LoopState(a),
) -> LoopState(a) {
  case state.break {
    True -> state
    //Break was called in the previous iteration of the loop, return previous state and stop looping
    False ->
      case pre_run_condition(state.state) {
        False -> state
        //return previous state and stop looping
        True -> {
          //calculate the new state
          let new_state = transformed_code_to_run(state)
          while_helper(new_state, pre_run_condition, transformed_code_to_run)
        }
      }
  }
}

///3. IMPLEMENTING DO WHILE WITH BREAK
/// In C, the syntax for `do while` is:
///
/// do {
///  // code block to be executed
/// }
/// while (condition);
///
/// This corresponding `do while` function in Gleam is stack-safe and checks the
/// condition based on the communicated state at the end of an iteration of the
/// while loop. It therefore runs the code in the `do while` function AT LEAST ONCE.
/// If the condition is true then the new state generated by the code is handed to
/// the next iteration of the `do while` loop and used with the next run of the code.
/// The code in the code block can throw a Break just like in C. If the condition is
/// false or break is true then the new state is returned and the loop stops.
/// Note that ending the loop returns the new state. This may or may not be desired.
/// The alternative is returning the old state. Let me know if there is a desire for
/// that feature.
pub fn do_while(
  state state: a,
  code_to_run code_to_run: fn(a) -> LoopState(a),
  post_run_condition post_run_condition: fn(a) -> Bool,
) -> a {
  let transformed_state = LoopState(state: state, break: False)
  let transformed_code_to_run = transform_code_to_run(code_to_run)

  let result =
    do_while_helper(
      state: transformed_state,
      transformed_code_to_run: transformed_code_to_run,
      post_run_condition: post_run_condition,
    )
  case result {
    LoopState(a, _) -> a
  }
}

fn do_while_helper(
  state state: LoopState(a),
  transformed_code_to_run transformed_code_to_run: fn(LoopState(a)) ->
    LoopState(a),
  post_run_condition post_run_condition: fn(a) -> Bool,
) -> LoopState(a) {
  let new_state = transformed_code_to_run(state)
  // check both post run condition and break at the same time
  let extended_post_run_condition_check =
    post_run_condition(new_state.state) && bool.negate(new_state.break)

  case extended_post_run_condition_check {
    False -> new_state
    True ->
      do_while_helper(new_state, transformed_code_to_run, post_run_condition)
  }
}

///4. IMPLEMENTING FOR WITH BREAK
/// In C, a `for`loop is defined as:
///
/// for (initialization; condition; increment) {
/// // code block to be executed
/// }
/// The initialization is executed (one time) before the execution of the code block.
/// The condition defines the condition for executing the code block.
/// And the increment code is executed (every time) after the code block has run.
///
/// In Gleam, the `for` function checks the condition using the communicated state at the
/// beginning of an iteration.
/// If the condition is true then the code block handed to the `for` function is run.
/// At the end of a run, a new state is generated that is used for the next
/// iteration.
/// If the condition is false or break is true then the code returns the final state and stops.
/// Just as the while functions, this `for` function is stack-safe.
///
pub fn for(
  //initialization
  state state: a,
  //condition
  pre_run_condition pre_run_condition: fn(a) -> Bool,
  code_to_run code_to_run: fn(a) -> LoopState(a),
  //increment
  increment_code post_run_code: fn(a) -> LoopState(a),
) -> a {
  let transformed_state = LoopState(state: state, break: False)
  let transformed_pre_run_condition =
    transform_pre_run_condition(pre_run_condition)
  let transformed_code_to_run = transform_code_to_run(code_to_run)
  let transformed_increment_code = transform_code_to_run(post_run_code)

  let result =
    for_helper(
      state: transformed_state,
      transformed_pre_run_condition: transformed_pre_run_condition,
      transformed_code_to_run: transformed_code_to_run,
      transformed_increment_code: transformed_increment_code,
    )
  case result {
    LoopState(a, _) -> a
  }
}

fn for_helper(
  state state: LoopState(a),
  transformed_pre_run_condition transformed_pre_run_condition: fn(LoopState(a)) ->
    Bool,
  transformed_code_to_run transformed_code_to_run: fn(LoopState(a)) ->
    LoopState(a),
  transformed_increment_code transformed_increment_code: fn(LoopState(a)) ->
    LoopState(a),
) -> LoopState(a) {
  case transformed_pre_run_condition(state) {
    False -> state
    True -> {
      let intermediate_state = transformed_code_to_run(state)
      let new_state = transformed_increment_code(intermediate_state)
      for_helper(
        state: new_state,
        transformed_pre_run_condition: transformed_pre_run_condition,
        transformed_code_to_run: transformed_code_to_run,
        transformed_increment_code: transformed_increment_code,
      )
    }
  }
}

///****** HELPER FUNCTIONS AND TYPES ******
type TransformedCaseBlock(a, b) {
  TransformedCaseBlock(match: a, transformed_code: fn(a) -> SwitchState(a, b))
}

//SwitchState allows us to take the return (case_return and break) of one iteration
//of the case loop into the next iteration in the Switch_helper function to both
//handle break = True and break = False in the previous iteration of the case loop.
type SwitchState(a, b) {
  SwitchState(expression: a, case_return: Option(b), break: Bool)
}

// Transforms a case into a more complex case structure for the switch_helper function.
fn transform_case(case_block: CaseBlock(a, b)) -> TransformedCaseBlock(a, b) {
  let value = case_block.match
  let original_fn = case_block.code
  let new_fn = fn(val) {
    //b
    let result = original_fn(val).0
    //break
    let flag = original_fn(val).1
    SwitchState(
      expression: value,
      case_return: option.Some(result),
      break: flag,
    )
  }
  TransformedCaseBlock(value, new_fn)
}

/// Transforms the default function in the switch statement into a more complex
/// structure for the switch_helper function.
fn transform_default(original_fn: fn(a) -> b) -> fn(a) -> SwitchState(a, b) {
  let new_fn = fn(val) {
    let result = original_fn(val)
    SwitchState(val, option.Some(result), True)
  }
  new_fn
  //return value of the function transform_default
}

// Transforms a code block into a code block that can handle breaks for the
// helper functions.
fn transform_code_to_run(
  original_fn: fn(a) -> LoopState(a),
) -> fn(LoopState(a)) -> LoopState(a) {
  fn(state: LoopState(a)) -> LoopState(a) {
    case state {
      LoopState(state: value, break: flag) -> {
        let result = original_fn(value)
        case result {
          LoopState(state: new_value, break: new_flag) ->
            LoopState(state: new_value, break: flag || new_flag)
        }
      }
    }
  }
}

// Transforms the pre-run condition into a more complex structure to handle break.
fn transform_pre_run_condition(
  original_fn: fn(a) -> Bool,
) -> fn(LoopState(a)) -> Bool {
  fn(state: LoopState(a)) -> Bool {
    case state {
      LoopState(state: value, break: flag) ->
        original_fn(value) && bool.negate(flag)
    }
  }
}
