let rec slowFibonacci n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | x -> slowFibonacci(x-1) + slowFibonacci(x-2)

/// Define an asynchronouse workflow
let slowFibonacciAsync n =
    async {return (slowFibonacci n)}
/// val slowFibonacciAsync: n:int -> Async<int>

/// Start the workflow and don't wait for it to complete.
let startedWorklow = Async.Start(slowFibonacciAsync 36)
/// val startedWorfklow: unit

/// Start the workflow and wait on the result.
let result = Async.RunSynchronously (slowFibonacciAsync 36)
/// val result: int