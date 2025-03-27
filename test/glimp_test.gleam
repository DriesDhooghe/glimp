//// To test and show how to use the C constructs `switch`, `while`, `do while`,
//// and `for`
//// I have added 4 examples.
//// 1. A simple switch statement identifying a day of the week based on a day
////number.
//// 2. The traditional implementation of the factorial function using `while`.
//// See factorial_using_while_with_break
//// 3. The factorial function using 'do while`.
//// See factorial_using_do_while_with_break
//// 4. The Factorial function but this time using a for loop.
//// See factorial_using_for_with_break

import glimp.{type LoopState, CaseBlock}

pub type ErrorCheck {
  NegativeValue
  ZeroValue
}

pub type FactorialState {
  FactorialState(
    accumulator: Int,
    i: Int,
    //termination_status: TerminationStatus,
  )
}

/// The state in these functions is represented by the variables accumulator and i.
/// In functional programming languages, variables can't be changed so we create a
/// FactorialState type that contains both accumulator and i and create a new state
/// for each iteration of the loop.
pub fn factorial_using_while_with_break(
  n: Int,
) -> Result(FactorialState, ErrorCheck) {
  //Error checking, don't run if intial value is negative
  let n_is_neg = n < 0
  let n_is_0 = n == 0
  let initial_state =
    //FactorialState(accumulator: 1, i: n, termination_status: None)
    FactorialState(accumulator: 1, i: n)
  case n_is_neg {
    True -> Error(NegativeValue)
    False ->
      case n_is_0 {
        True -> Ok(initial_state)
        False ->
          Ok(
            glimp.while(
              state: initial_state,
              pre_run_condition: fn(state: FactorialState) -> Bool {
                state.i != 1
              },
              code_to_run: fn(state: FactorialState) -> LoopState(
                FactorialState,
              ) {
                let accumulator = state.accumulator
                let i = state.i
                case i {
                  // for the example, we break when i = 5 and don't calculate a new state.
                  // remove this line and we don't break at all.
                  5 -> glimp.LoopState(state: state, break: True)
                  _ ->
                    glimp.LoopState(
                      //Note how in a while loop, the counter is updated within the code block.
                      //This is in contrast to the for loop below.
                      state: FactorialState(accumulator * i, i - 1),
                      break: False,
                    )
                }
              },
            ),
          )
      }
  }
}

pub fn factorial_using_do_while_with_break(
  n: Int,
) -> Result(FactorialState, ErrorCheck) {
  //Error checking, don't run if intial value is negative
  let n_is_neg = n < 0
  let n_is_0 = n == 0
  let initial_state = FactorialState(accumulator: 1, i: n)
  case n_is_neg {
    True -> Error(NegativeValue)
    False ->
      case n_is_0 {
        True -> Ok(initial_state)
        False ->
          Ok(glimp.do_while(
            state: initial_state,
            code_to_run: fn(state: FactorialState) -> LoopState(FactorialState) {
              let accumulator = state.accumulator
              let i = state.i
              case i {
                // for the example, we break when i = 5 and don't calculate a new state.
                // remove this line and we don't break at all.
                5 -> glimp.LoopState(state: state, break: True)
                // the 2 next cases are equivalent to if (n > 1) { acc = acc * n; n = n - 1; }
                1 -> glimp.LoopState(state: state, break: True)
                _ ->
                  glimp.LoopState(
                    //Note how in a while loop, the counter is updated within the code block.
                    //This is in contrast to the for loop below.
                    state: FactorialState(accumulator * i, i - 1),
                    break: False,
                  )
              }
            },
            post_run_condition: fn(state: FactorialState) -> Bool {
              state.i != 1
            },
          ))
      }
  }
}

pub fn factorial_using_for_with_break(
  n: Int,
) -> Result(FactorialState, ErrorCheck) {
  //Error checking, don't run if intial value is negative
  let n_is_neg = n < 0
  let n_is_0 = n == 0
  let initial_state = FactorialState(accumulator: 1, i: 1)
  case n_is_neg {
    True -> Error(NegativeValue)
    False ->
      case n_is_0 {
        True -> Ok(initial_state)
        False ->
          Ok(glimp.for(
            state: initial_state,
            pre_run_condition: fn(state: FactorialState) -> Bool {
              state.i <= n
            },
            code_to_run: fn(state: FactorialState) -> LoopState(FactorialState) {
              let accumulator = state.accumulator
              let i = state.i
              case i {
                // for the example, we break when i = 5 and don't calculate a new state.
                // remove this line and we don't break at all.
                5 -> glimp.LoopState(state: state, break: True)
                _ ->
                  glimp.LoopState(
                    //Note how in a for loop, the counter is NOT updated in the code block
                    //like it is in the while function although we easily could have.
                    //This follows how the for loop works in C.
                    FactorialState(accumulator: accumulator * i, i: i),
                    break: False,
                  )
              }
            },
            increment_code: fn(state: FactorialState) -> LoopState(
              FactorialState,
            ) {
              let accumulator = state.accumulator
              let i = state.i
              case i {
                // We could insert a break here too. See the commented line.
                //5 -> glimp.LoopState(state: state, break: True)
                _ ->
                  glimp.LoopState(
                    //Note how in a for loop, the counter is NOT updated in the code block
                    //like it is in the while function although we easily could have.
                    //This follows how the for loop works in C.
                    FactorialState(accumulator: accumulator, i: i + 1),
                    break: False,
                  )
              }
            },
          ))
      }
  }
}

pub fn main() {
  //Example 1: SWITCH
  //Based on the number of a day in the week, retrieve the name of the day.
  //In the example, 1 represents Monday,2 Tuesday, etc.
  //If we give it a number greater than 7 then the function will give us an error message.
  //The C-equivalent would be:
  //switch (day) {
  //  case 1:
  //    printf("Monday");
  //    break;
  //  case 2:
  //    printf("Tuesday");
  //    break;
  //  case 3:
  //    printf("Wednesday");
  //    break;
  //  case 4:
  //    printf("Thursday");
  //    break;
  //  case 5:
  //    printf("Friday");
  //    break;
  //  case 6:
  //    printf("Saturday");
  //    break;
  //  case 7:
  //    printf("Sunday");
  //    break;
  // default:
  //    printf("Match value out of bounds");
  //}

  let cases = [
    CaseBlock(match: 1, code: fn(_) { #("Monday", True) }),
    CaseBlock(match: 2, code: fn(_) { #("Tuesday", True) }),
    CaseBlock(match: 3, code: fn(_) { #("Wednesday", True) }),
    CaseBlock(match: 4, code: fn(_) { #("Thursday", True) }),
    CaseBlock(match: 5, code: fn(_) { #("Friday", True) }),
    CaseBlock(match: 6, code: fn(_) { #("Saturday", True) }),
    CaseBlock(match: 7, code: fn(_) { #("Sunday", True) }),
  ]
  let default = fn(_) { "Match value out of bounds" }

  echo "******************"
  echo "SWITCH TEST CASES"
  echo "******************"

  echo glimp.switch(expression: 1, cases: cases, default: default)
  echo glimp.switch(expression: 2, cases: cases, default: default)
  echo glimp.switch(expression: 3, cases: cases, default: default)
  echo glimp.switch(expression: 4, cases: cases, default: default)
  echo glimp.switch(expression: 5, cases: cases, default: default)
  echo glimp.switch(expression: 6, cases: cases, default: default)
  echo glimp.switch(expression: 7, cases: cases, default: default)

  //out of bounds
  echo glimp.switch(expression: 8, cases: cases, default: default)

  // EXAMPLE 2: WHILE WITH BREAK
  // This function calculates the factorial using iteration and a while loop
  // (using an iterator i and an accumulator to hold the calculated value in
  // between iterations of the while loop).
  // In an imperative language like C, the function factiorial using a while loop
  // would be implemented as follows:
  //
  // int factorial(int n) {
  //    if (n < 0) { return -1; } //error check, return -1 if there is an error
  //    int acc = 1;
  //    while (n > 1) { acc = acc * n; n = n - 1; }
  //    return acc;
  // }
  //
  // n > 1 is the condition to check prior to running the code block. If the
  // condition is false then the loop stops and the function returns the final acc.
  // { acc = acc * n; n = n - 1 } is the code to execute if the condition is true
  // (in this case multiply the value of acc with n; store the result in acc;
  // decrement n and start the next iteration of the loop.
  // NOTE: in a while loop, the counter n is updated in the code block in C.
  // This is in contrast to the for loop (see further).
  //
  // The state in this snippet is the variable acc and the parameter n. In
  // functional programming languages, variables can't be changed so we create
  // a FactorialState type (see above) that contains both acc and n and we create
  // a new state for each iteration of the while loop.
  // The equivalent factorial function to the C code above using glimp.while is defined in
  // factorial_using_while_with_break:
  // NOTE: I added an easily removed break in the function to show how to use break
  // in the middle of a loop.

  echo "****************"
  echo "WHILE TEST CASES"
  echo "****************"

  echo factorial_using_while_with_break(4)
  //notice how in the next 2 lines, the function returns 5 as the last i, indicating a break at i = 5
  echo factorial_using_while_with_break(5)
  echo factorial_using_while_with_break(100)
  echo factorial_using_while_with_break(-1)
  echo factorial_using_while_with_break(0)
  echo factorial_using_while_with_break(1)

  //Example 3: DO WHILE WITH BREAK
  // This calculates the factorial using a do while loop. It means the loop will execute at least once
  // before checking the condition.
  // In C this could be implemented as follows:
  //
  // int factorial(int n) {
  //    if (n < 0) { return -1; } // error check: return -1 if there is an error
  //    int acc = 1;
  //   // Use a do-while loop to calculate the factorial
  //    do {
  //        if (n > 1) { acc = acc * n; n = n - 1; }
  //    } while (n > 1);  // Continue looping while n is greater than 1
  //    return acc;
  // }
  //
  // The equivalent factorial function to the C code above glimp.do_while is defined in
  // factorial_using_do_while_with_break:
  // NOTE: I added an easily removed break in the function to show how to use break
  // in the middle of a loop.
  //
  echo "*******************"
  echo "DO WHILE TEST CASES"
  echo "*******************"

  echo factorial_using_do_while_with_break(4)
  //notice how in the next 2 lines, the function returns 5 as the last i, indicating a break at i = 5
  echo factorial_using_do_while_with_break(5)
  echo factorial_using_do_while_with_break(100)
  echo factorial_using_do_while_with_break(-1)
  echo factorial_using_do_while_with_break(0)
  echo factorial_using_do_while_with_break(1)

  //Example 4: FOR WITH BREAK
  // This example alculates the factorial using a for loop.
  // In C this could be implemented as follows:
  //
  //int factorial(int n) {
  //    // Error check: return -1 if n is negative
  //    if (n < 0) { return -1;}
  //    int acc = 1;
  //    // Use a for loop to calculate the factorial
  //    for (int i = 1; i <= n; i++) { acc = acc * i; }
  //    return acc;
  //
  // The equivalent factorial function to the C code above using glimp.for is defined in
  // factorial_using_do_while_with_break:
  // NOTE: I added an easily removed break in the function to show how to use break
  // in the middle of a loop.

  echo "**************"
  echo "FOR TEST CASES"
  echo "**************"

  echo factorial_using_for_with_break(4)
  //notice how in the next 2 lines, the function returns 6 as the last i, indicating a break at i = 5
  echo factorial_using_for_with_break(5)
  echo factorial_using_for_with_break(100)
  echo factorial_using_for_with_break(-1)
  echo factorial_using_for_with_break(0)
  echo factorial_using_for_with_break(1)
}
