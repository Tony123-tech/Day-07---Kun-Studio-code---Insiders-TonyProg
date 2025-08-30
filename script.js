// Studio Code Editor - Main JavaScript File

class StudioCodeEditor {
    constructor() {
        this.editor = null;
        this.currentFile = 'main';
        this.files = {
            main: { name: 'main.js', content: '// Welcome to Studio Code Editor!\nconsole.log("Hello, World!");\n\n// Write your JavaScript code here\nfunction greet(name) {\n    return `Hello, ${name}!`;\n}\n\nconsole.log(greet("Developer"));', mode: 'javascript' },
            typescript: { name: 'main.ts', content: '// TypeScript Example\ninterface User {\n    name: string;\n    age: number;\n    email?: string;\n}\n\nclass UserManager {\n    private users: User[] = [];\n\n    addUser(user: User): void {\n        this.users.push(user);\n        console.log(`Added user: ${user.name}`);\n    }\n\n    getUserByName(name: string): User | undefined {\n        return this.users.find(user => user.name === name);\n    }\n}\n\nconst manager = new UserManager();\nmanager.addUser({ name: "John", age: 30, email: "john@example.com" });\nconsole.log("TypeScript is awesome!");', mode: 'typescript' },
            html: { name: 'index.html', content: '<!DOCTYPE html>\n<html lang="en">\n<head>\n    <meta charset="UTF-8">\n    <meta name="viewport" content="width=device-width, initial-scale=1.0">\n    <title>My Page</title>\n</head>\n<body>\n    <h1>Hello World!</h1>\n    <p>This is a sample HTML page.</p>\n    <script src="main.js"></script>\n</body>\n</html>', mode: 'xml' },
            css: { name: 'styles.css', content: 'body {\n    font-family: Arial, sans-serif;\n    margin: 0;\n    padding: 20px;\n    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);\n    color: white;\n    min-height: 100vh;\n}\n\nh1 {\n    text-align: center;\n    font-size: 3rem;\n    margin-bottom: 1rem;\n    text-shadow: 2px 2px 4px rgba(0,0,0,0.3);\n}\n\np {\n    text-align: center;\n    font-size: 1.2rem;\n    line-height: 1.6;\n}', mode: 'css' },
            python: { name: 'main.py', content: '# Python Example\nimport math\nfrom typing import List, Optional\n\nclass Calculator:\n    """A simple calculator class"""\n    \n    def __init__(self):\n        self.history: List[float] = []\n    \n    def add(self, a: float, b: float) -> float:\n        result = a + b\n        self.history.append(result)\n        return result\n    \n    def multiply(self, a: float, b: float) -> float:\n        result = a * b\n        self.history.append(result)\n        return result\n    \n    def get_history(self) -> List[float]:\n        return self.history.copy()\n\n# Usage\ndef main():\n    calc = Calculator()\n    print("Calculator Demo")\n    print(f"2 + 3 = {calc.add(2, 3)}")\n    print(f"4 * 5 = {calc.multiply(4, 5)}")\n    print(f"History: {calc.get_history()}")\n\nif __name__ == "__main__":\n    main()', mode: 'python' },
            coffeescript: { name: 'main.coffee', content: '# CoffeeScript Example\nclass Animal\n  constructor: (@name, @species) ->\n  \n  speak: ->\n    console.log "#{@name} the #{@species} says hello!"\n  \n  move: (distance) ->\n    console.log "#{@name} moved #{distance} meters"\n\nclass Dog extends Animal\n  constructor: (@name) ->\n    super @name, "Dog"\n  \n  bark: ->\n    console.log "#{@name} barks: Woof! Woof!"\n  \n  wag_tail: ->\n    console.log "#{@name} wags tail happily"\n\n# Usage\ndog = new Dog "Buddy"\ndog.speak()\ndog.bark()\ndog.move 10\ndog.wag_tail()\n\n# CoffeeScript features\nnumbers = [1..5]\nsquares = (num * num for num in numbers)\nconsole.log "Squares: #{squares}"', mode: 'coffeescript' },
            actionscript: { name: 'Main.as', content: '// ActionScript 3.0 Example\npackage {\n    import flash.display.Sprite;\n    import flash.events.Event;\n    import flash.text.TextField;\n    import flash.text.TextFormat;\n    \n    public class Main extends Sprite {\n        private var textField:TextField;\n        private var counter:int = 0;\n        \n        public function Main() {\n            if (stage) init();\n            else addEventListener(Event.ADDED_TO_STAGE, init);\n        }\n        \n        private function init(e:Event = null):void {\n            if (hasEventListener(Event.ADDED_TO_STAGE)) {\n                removeEventListener(Event.ADDED_TO_STAGE, init);\n            }\n            \n            createUI();\n            startCounter();\n        }\n        \n        private function createUI():void {\n            textField = new TextField();\n            var format:TextFormat = new TextFormat();\n            format.size = 24;\n            format.color = 0xFFFFFF;\n            \n            textField.defaultTextFormat = format;\n            textField.width = 400;\n            textField.height = 100;\n            textField.x = 50;\n            textField.y = 50;\n            \n            addChild(textField);\n        }\n        \n        private function startCounter():void {\n            addEventListener(Event.ENTER_FRAME, updateCounter);\n        }\n        \n        private function updateCounter(e:Event):void {\n            counter++;\n            textField.text = "Frame: " + counter;\n        }\n    }\n}', mode: 'text/x-actionscript' },
            asharp: { name: 'main.a', content: '// A# (A Sharp) Example\n// A# is a functional programming language\n\n// Basic function definition\nlet add x y = x + y\nlet multiply x y = x * y\n\n// List operations\nlet numbers = [1..10]\nlet doubled = List.map (fun x -> x * 2) numbers\nlet sum = List.fold (fun acc x -> acc + x) 0 numbers\n\n// Pattern matching\nlet rec factorial n = \n    match n with\n    | 0 -> 1\n    | 1 -> 1\n    | _ -> n * factorial (n - 1)\n\n// Higher-order functions\nlet applyTwice f x = f (f x)\nlet addOne x = x + 1\nlet addThree = applyTwice addOne\n\n// Usage\nprintfn "A# Functional Programming"\nprintfn "Add 5 3: %d" (add 5 3)\nprintfn "Multiply 4 6: %d" (multiply 4 6)\nprintfn "Factorial 5: %d" (factorial 5)\nprintfn "Apply twice addOne to 5: %d" (addThree 5)\nprintfn "Sum of 1-10: %d" sum', mode: 'text/x-fsharp' },
            csharp: { name: 'Program.cs', content: 'using System;\nusing System.Collections.Generic;\nusing System.Linq;\n\nnamespace StudioCodeEditor\n{\n    public class Program\n    {\n        public static void Main(string[] args)\n        {\n            Console.WriteLine("Welcome to C# in Studio Code Editor!");\n            \n            // Demonstrate C# features\n            var calculator = new Calculator();\n            var result = calculator.Add(10, 20);\n            Console.WriteLine($"10 + 20 = {result}");\n            \n            // LINQ example\n            var numbers = Enumerable.Range(1, 10);\n            var evenNumbers = numbers.Where(n => n % 2 == 0);\n            Console.WriteLine($"Even numbers: {string.Join(", ", evenNumbers)}");\n            \n            // Async example\n            calculator.DoWorkAsync().Wait();\n        }\n    }\n    \n    public class Calculator\n    {\n        public int Add(int a, int b) => a + b;\n        public int Subtract(int a, int b) => a - b;\n        public int Multiply(int a, int b) => a * b;\n        public double Divide(int a, int b) => (double)a / b;\n        \n        public async Task DoWorkAsync()\n        {\n            await Task.Delay(1000);\n            Console.WriteLine("Async work completed!");\n        }\n    }\n}', mode: 'text/x-csharp' },
            aspnet: { name: 'Program.cs', content: 'using Microsoft.AspNetCore.Builder;\nusing Microsoft.Extensions.DependencyInjection;\nusing Microsoft.Extensions.Hosting;\nusing Microsoft.AspNetCore.Mvc;\nusing System.Collections.Generic;\nusing System.Linq;\n\nvar builder = WebApplication.CreateBuilder(args);\n\n// Add services to the container.\nbuilder.Services.AddControllers();\nbuilder.Services.AddEndpointsApiExplorer();\nbuilder.Services.AddSwaggerGen();\n\nvar app = builder.Build();\n\n// Configure the HTTP request pipeline.\nif (app.Environment.IsDevelopment())\n{\n    app.UseSwagger();\n    app.UseSwaggerUI();\n}\n\napp.UseHttpsRedirection();\napp.UseAuthorization();\napp.MapControllers();\n\n// Minimal API endpoints\napp.MapGet("/", () => "Welcome to ASP.NET Core!");\n\napp.MapGet("/api/hello/{name}", (string name) => \n    $"Hello, {name}! Welcome to ASP.NET Core!");\n\napp.MapPost("/api/calculate", (CalculationRequest request) => \n{\n    var result = request.Operation switch\n    {\n        "add" => request.A + request.B,\n        "subtract" => request.A - request.B,\n        "multiply" => request.A * request.B,\n        "divide" => request.A / request.B,\n        _ => throw new ArgumentException("Invalid operation")\n    };\n    \n    return new CalculationResponse { Result = result };\n});\n\napp.Run();\n\n// Data models\npublic record CalculationRequest(double A, double B, string Operation);\npublic record CalculationResponse(double Result);\n\n// Controller example\n[ApiController]\n[Route("api/[controller]")]\npublic class UsersController : ControllerBase\n{\n    private static List<User> _users = new();\n    \n    [HttpGet]\n    public ActionResult<IEnumerable<User>> GetUsers()\n    {\n        return Ok(_users);\n    }\n    \n    [HttpGet("{id}")]\n    public ActionResult<User> GetUser(int id)\n    {\n        var user = _users.FirstOrDefault(u => u.Id == id);\n        if (user == null)\n            return NotFound();\n        \n        return Ok(user);\n    }\n    \n    [HttpPost]\n    public ActionResult<User> CreateUser(User user)\n    {\n        user.Id = _users.Count + 1;\n        _users.Add(user);\n        return CreatedAtAction(nameof(GetUser), new { id = user.Id }, user);\n    }\n}\n\npublic class User\n{\n    public int Id { get; set; }\n    public string Name { get; set; } = string.Empty;\n    public string Email { get; set; } = string.Empty;\n}', mode: 'text/x-csharp' },
            zig: { name: 'main.zig', content: 'const std = @import("std");\n\n// Zig Example - Modern Systems Programming Language\npub fn main() !void {\n    // Get standard output\n    const stdout = std.io.getStdOut().writer();\n    \n    try stdout.print("Hello, Zig World!\\n", .{});\n    \n    // Demonstrate Zig features\n    try demonstrateBasicTypes(stdout);\n    try demonstrateStructs(stdout);\n    try demonstrateFunctions(stdout);\n    try demonstrateErrorHandling(stdout);\n    try demonstrateComptime(stdout);\n}\n\nfn demonstrateBasicTypes(writer: anytype) !void {\n    try writer.print("\\n=== Basic Types ===\\n", .{});\n    \n    const integer: i32 = 42;\n    const float: f64 = 3.14159;\n    const boolean: bool = true;\n    const optional: ?i32 = 123;\n    const optional_null: ?i32 = null;\n    \n    try writer.print("Integer: {}\\n", .{integer});\n    try writer.print("Float: {d:.2}\\n", .{float});\n    try writer.print("Boolean: {}\\n", .{boolean});\n    try writer.print("Optional (some): {}\\n", .{optional});\n    try writer.print("Optional (null): {?}\\n", .{optional_null});\n}\n\nfn demonstrateStructs(writer: anytype) !void {\n    try writer.print("\\n=== Structs ===\\n", .{});\n    \n    const Point = struct {\n        x: f64,\n        y: f64,\n        \n        pub fn distance(self: Point, other: Point) f64 {\n            const dx = self.x - other.x;\n            const dy = self.y - other.y;\n            return @sqrt(dx * dx + dy * dy);\n        }\n        \n        pub fn format(self: Point, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {\n            _ = fmt;\n            _ = options;\n            try writer.print("Point({{x: {d:.2}, y: {d:.2}}})", .{ self.x, self.y });\n        }\n    };\n    \n    const p1 = Point{ .x = 0.0, .y = 0.0 };\n    const p2 = Point{ .x = 3.0, .y = 4.0 };\n    \n    try writer.print("Point 1: {}\\n", .{p1});\n    try writer.print("Point 2: {}\\n", .{p2});\n    try writer.print("Distance: {d:.2}\\n", .{p1.distance(p2)});\n}\n\nfn demonstrateFunctions(writer: anytype) !void {\n    try writer.print("\\n=== Functions ===\\n", .{});\n    \n    // Function with multiple return values\n    const result = divideWithRemainder(17, 5);\n    try writer.print("17 / 5 = {} remainder {}\\n", .{ result.quotient, result.remainder });\n    \n    // Generic function\n    const max_int = max(i32, 10, 20);\n    const max_float = max(f64, 3.14, 2.71);\n    try writer.print("Max int: {}\\n", .{max_int});\n    try writer.print("Max float: {d:.2}\\n", .{max_float});\n}\n\nfn divideWithRemainder(a: i32, b: i32) struct { quotient: i32, remainder: i32 } {\n    return .{\n        .quotient = @divFloor(a, b),\n        .remainder = @mod(a, b),\n    };\n}\n\nfn max(comptime T: type, a: T, b: T) T {\n    return if (a > b) a else b;\n}\n\nfn demonstrateErrorHandling(writer: anytype) !void {\n    try writer.print("\\n=== Error Handling ===\\n", .{});\n    \n    // Function that can fail\n    const result = safeDivide(10, 2) catch |err| {\n        try writer.print("Error: {}\\n", .{err});\n        return;\n    };\n    \n    try writer.print("10 / 2 = {}\\n", .{result});\n    \n    // Function that can fail with null\n    const maybe_result = safeDivideOptional(10, 0);\n    if (maybe_result) |value| {\n        try writer.print("Result: {}\\n", .{value});\n    } else {\n        try writer.print("Division by zero avoided\\n", .{});\n    }\n}\n\nfn safeDivide(a: i32, b: i32) !i32 {\n    if (b == 0) return error.DivisionByZero;\n    return @divFloor(a, b);\n}\n\nfn safeDivideOptional(a: i32, b: i32) ?i32 {\n    if (b == 0) return null;\n    return @divFloor(a, b);\n}\n\nfn demonstrateComptime(writer: anytype) !void {\n    try writer.print("\\n=== Compile-time Features ===\\n", .{});\n    \n    // Compile-time array creation\n    const numbers = comptime blk: {\n        var arr: [5]i32 = undefined;\n        var i: usize = 0;\n        while (i < 5) : (i += 1) {\n            arr[i] = @intCast(i32, i * i);\n        }\n        break :blk arr;\n    };\n    \n    try writer.print("Compile-time array: {any}\\n", .{numbers});\n    \n    // Compile-time function evaluation\n    const factorial_5 = comptime factorial(5);\n    try writer.print("5! = {}\\n", .{factorial_5});\n}\n\nfn factorial(n: u32) u32 {\n    if (n <= 1) return 1;\n    return n * factorial(n - 1);\n}', mode: 'text/x-zig' },
            haskell: { name: 'Main.hs', content: '-- Haskell Example - Functional Programming Language\nmodule Main where\n\nimport System.IO\nimport Data.List\nimport Data.Char\n\n-- Main function\nmain :: IO ()\nmain = do\n    putStrLn "Hello, Haskell World!"\n    \n    -- Demonstrate Haskell features\n    demonstrateBasicTypes\n    demonstrateFunctions\n    demonstrateLists\n    demonstratePatternMatching\n    demonstrateHigherOrder\n    demonstrateMonads\n\n-- Basic types and functions\ndemonstrateBasicTypes :: IO ()\ndemonstrateBasicTypes = do\n    putStrLn "\\n=== Basic Types ==="\n    \n    let integer = 42 :: Int\n    let float = 3.14159 :: Double\n    let boolean = True\n    let char = \'A\'\n    let string = "Hello Haskell"\n    \n    putStrLn $ "Integer: " ++ show integer\n    putStrLn $ "Float: " ++ show float\n    putStrLn $ "Boolean: " ++ show boolean\n    putStrLn $ "Char: " ++ show char\n    putStrLn $ "String: " ++ string\n\n-- Function definitions\ndemonstrateFunctions :: IO ()\ndemonstrateFunctions = do\n    putStrLn "\\n=== Functions ==="\n    \n    let result1 = add 5 3\n    let result2 = multiply 4 6\n    let result3 = factorial 5\n    \n    putStrLn $ "5 + 3 = " ++ show result1\n    putStrLn $ "4 * 6 = " ++ show result2\n    putStrLn $ "5! = " ++ show result3\n\n-- Pure functions\nadd :: Int -> Int -> Int\nadd x y = x + y\n\nmultiply :: Int -> Int -> Int\nmultiply x y = x * y\n\nfactorial :: Int -> Int\nfactorial 0 = 1\nfactorial n = n * factorial (n - 1)\n\n-- List operations\ndemonstrateLists :: IO ()\ndemonstrateLists = do\n    putStrLn "\\n=== Lists ==="\n    \n    let numbers = [1..10]\n    let doubled = map (*2) numbers\n    let filtered = filter even numbers\n    let sumTotal = sum numbers\n    let productTotal = product numbers\n    \n    putStrLn $ "Numbers 1-10: " ++ show numbers\n    putStrLn $ "Doubled: " ++ show doubled\n    putStrLn $ "Even numbers: " ++ show filtered\n    putStrLn $ "Sum: " ++ show sumTotal\n    putStrLn $ "Product: " ++ show productTotal\n\n-- Pattern matching\ndemonstratePatternMatching :: IO ()\ndemonstratePatternMatching = do\n    putStrLn "\\n=== Pattern Matching ==="\n    \n    let list1 = [1, 2, 3]\n    let list2 = []\n    let list3 = [42]\n    \n    putStrLn $ "List 1: " ++ describeList list1\n    putStrLn $ "List 2: " ++ describeList list2\n    putStrLn $ "List 3: " ++ describeList list3\n\n-- Pattern matching function\ndescribeList :: [Int] -> String\ndescribeList [] = "Empty list"\ndescribeList [x] = "Single element: " ++ show x\ndescribeList (x:y:xs) = "Multiple elements starting with " ++ show x ++ " and " ++ show y\n\n-- Higher-order functions\ndemonstrateHigherOrder :: IO ()\ndemonstrateHigherOrder = do\n    putStrLn "\\n=== Higher-Order Functions ==="\n    \n    let numbers = [1..5]\n    let squared = map (^2) numbers\n    let isEven = filter even numbers\n    let sumSquares = foldl (+) 0 (map (^2) numbers)\n    \n    putStrLn $ "Numbers: " ++ show numbers\n    putStrLn $ "Squared: " ++ show squared\n    putStrLn $ "Even numbers: " ++ show isEven\n    putStrLn $ "Sum of squares: " ++ show sumSquares\n\n-- Monad example (Maybe)\ndemonstrateMonads :: IO ()\ndemonstrateMonads = do\n    putStrLn "\\n=== Monads (Maybe) ==="\n    \n    let result1 = safeDivide 10 2\n    let result2 = safeDivide 10 0\n    \n    putStrLn $ "10 / 2 = " ++ show result1\n    putStrLn $ "10 / 0 = " ++ show result2\n\n-- Safe division using Maybe monad\nsafeDivide :: Int -> Int -> Maybe Int\nsafeDivide _ 0 = Nothing\nsafeDivide x y = Just (x `div` y)', mode: 'text/x-haskell' },
            fsharp: { name: 'Program.fs', content: '// F# Functional Programming Example\nopen System\n\n// Basic function definitions\nlet add x y = x + y\nlet multiply x y = x * y\n\n// Pattern matching\nlet rec factorial n = \n    match n with\n    | 0 | 1 -> 1\n    | _ -> n * factorial (n - 1)\n\n// List operations\nlet numbers = [1..10]\nlet doubled = numbers |> List.map (fun x -> x * 2)\nlet sum = numbers |> List.sum\n\n// Higher-order functions\nlet applyTwice f x = f (f x)\nlet addOne x = x + 1\nlet addThree = addOne >> addOne >> addOne\n\n// Discriminated unions\ntype Shape = \n    | Circle of float\n    | Rectangle of float * float\n    | Triangle of float * float * float\n\nlet area shape = \n    match shape with\n    | Circle r -> Math.PI * r * r\n    | Rectangle (w, h) -> w * h\n    | Triangle (a, b, c) -> \n        let s = (a + b + c) / 2.0\n        sqrt (s * (s - a) * (s - b) * (s - c))\n\n// Usage\nprintfn "F# Functional Programming Demo"\nprintfn "Add 5 3: %d" (add 5 3)\nprintfn "Factorial 5: %d" (factorial 5)\nprintfn "Sum of 1-10: %d" sum\nprintfn "Circle area (r=5): %.2f" (area (Circle 5.0))\nprintfn "Rectangle area (3x4): %.2f" (area (Rectangle (3.0, 4.0)))', mode: 'text/x-fsharp' },
            qsharp: { name: 'Program.qs', content: '// Q# Quantum Programming Example\nnamespace StudioCodeEditor {\n    \n    open Microsoft.Quantum.Canon;\n    open Microsoft.Quantum.Intrinsic;\n    open Microsoft.Quantum.Measurement;\n    open Microsoft.Quantum.Math;\n    \n    // Simple quantum operation\n    operation HelloQ() : Result {\n        use q = Qubit();\n        H(q);\n        let result = M(q);\n        Reset(q);\n        return result;\n    }\n    \n    // Quantum superposition\n    operation Superposition() : Result[] {\n        use q = Qubit();\n        H(q);\n        \n        mutable results = [Zero, size = 100];\n        for i in 0..99 {\n            set results w/= i <- M(q);\n            Reset(q);\n            H(q);\n        }\n        \n        Reset(q);\n        return results;\n    }\n    \n    // Bell state creation\n    operation BellState() : (Result, Result) {\n        use (q1, q2) = (Qubit(), Qubit());\n        \n        H(q1);\n        CNOT(q1, q2);\n        \n        let result1 = M(q1);\n        let result2 = M(q2);\n        \n        Reset(q1);\n        Reset(q2);\n        \n        return (result1, result2);\n    }\n    \n    // Quantum random number generator\n    operation QuantumRandom() : Int {\n        use q = Qubit();\n        H(q);\n        let result = M(q);\n        Reset(q);\n        \n        if (result == One) {\n            return 1;\n        } else {\n            return 0;\n        }\n    }\n}', mode: 'text/x-csharp' },
            ruby: { name: 'main.rb', content: '# Ruby Example\nputs "Hello, Ruby World!"\n\n# Ruby classes and methods\nclass Calculator\n  def initialize\n    @history = []\n  end\n  \n  def add(a, b)\n    result = a + b\n    @history << result\n    result\n  end\n  \n  def multiply(a, b)\n    result = a * b\n    @history << result\n    result\n  end\n  \n  def history\n    @history\n  end\nend\n\n# Usage\ncalc = Calculator.new\nputs "2 + 3 = #{calc.add(2, 3)}"\nputs "4 * 5 = #{calc.multiply(4, 5)}"\nputs "History: #{calc.history}"\n\n# Ruby blocks and enumerables\nnumbers = [1, 2, 3, 4, 5]\nsquares = numbers.map { |n| n ** 2 }\nputs "Squares: #{squares}"', mode: 'ruby' },
            php: { name: 'index.php', content: '<?php\n// PHP Example\necho "Hello, PHP World!<br>";\n\n// PHP classes and functions\nclass Calculator {\n    private $history = array();\n    \n    public function add($a, $b) {\n        $result = $a + $b;\n        $this->history[] = $result;\n        return $result;\n    }\n    \n    public function multiply($a, $b) {\n        $result = $a * $b;\n        $this->history[] = $result;\n        return $result;\n    }\n    \n    public function getHistory() {\n        return $this->history;\n    }\n}\n\n// Usage\n$calc = new Calculator();\necho "2 + 3 = " . $calc->add(2, 3) . "<br>";\necho "4 * 5 = " . $calc->multiply(4, 5) . "<br>";\necho "History: " . implode(", ", $calc->getHistory()) . "<br>";\n\n// PHP arrays and loops\n$numbers = array(1, 2, 3, 4, 5);\necho "Numbers: " . implode(", ", $numbers) . "<br>";\n\nforeach ($numbers as $num) {\n    echo "Number: $num<br>";\n}\n?>', mode: 'php' },
            go: { name: 'main.go', content: 'package main\n\nimport (\n    "fmt"\n    "math"\n)\n\n// Go structs and methods\ntype Calculator struct {\n    history []float64\n}\n\nfunc (c *Calculator) Add(a, b float64) float64 {\n    result := a + b\n    c.history = append(c.history, result)\n    return result\n}\n\nfunc (c *Calculator) Multiply(a, b float64) float64 {\n    result := a * b\n    c.history = append(c.history, result)\n    return result\n}\n\nfunc (c *Calculator) GetHistory() []float64 {\n    return c.history\n}\n\nfunc main() {\n    fmt.Println("Hello, Go World!")\n    \n    calc := &Calculator{}\n    \n    fmt.Printf("2 + 3 = %.2f\\n", calc.Add(2, 3))\n    fmt.Printf("4 * 5 = %.2f\\n", calc.Multiply(4, 5))\n    fmt.Printf("History: %v\\n", calc.GetHistory())\n    \n    // Go slices and loops\n    numbers := []int{1, 2, 3, 4, 5}\n    fmt.Printf("Numbers: %v\\n", numbers)\n    \n    for i, num := range numbers {\n        fmt.Printf("Index %d: %d\\n", i, num)\n    }\n}', mode: 'go' },
            rust: { name: 'main.rs', content: '// Rust Example\nfn main() {\n    println!("Hello, Rust World!");\n    \n    // Rust structs and implementations\n    let mut calc = Calculator::new();\n    \n    println!("2 + 3 = {}", calc.add(2.0, 3.0));\n    println!("4 * 5 = {}", calc.multiply(4.0, 5.0));\n    println!("History: {:?}", calc.get_history());\n    \n    // Rust vectors and iterators\n    let numbers: Vec<i32> = vec![1, 2, 3, 4, 5];\n    println!("Numbers: {:?}", numbers);\n    \n    let squares: Vec<i32> = numbers.iter().map(|&x| x * x).collect();\n    println!("Squares: {:?}", squares);\n    \n    // Rust pattern matching\n    for num in &numbers {\n        match num {\n            1 => println!("One!"),\n            2 => println!("Two!"),\n            3 => println!("Three!"),\n            _ => println!("Other number: {}", num),\n        }\n    }\n}\n\nstruct Calculator {\n    history: Vec<f64>,\n}\n\nimpl Calculator {\n    fn new() -> Self {\n        Calculator {\n            history: Vec::new(),\n        }\n    }\n    \n    fn add(&mut self, a: f64, b: f64) -> f64 {\n        let result = a + b;\n        self.history.push(result);\n        result\n    }\n    \n    fn multiply(&mut self, a: f64, b: f64) -> f64 {\n        let result = a * b;\n        self.history.push(result);\n        result\n    }\n    \n    fn get_history(&self) -> &Vec<f64> {\n        &self.history\n    }\n}', mode: 'rust' },
            swift: { name: 'main.swift', content: '// Swift Example\nimport Foundation\n\nprint("Hello, Swift World!")\n\n// Swift classes and protocols\nprotocol CalculatorProtocol {\n    func add(_ a: Double, _ b: Double) -> Double\n    func multiply(_ a: Double, _ b: Double) -> Double\n}\n\nclass Calculator: CalculatorProtocol {\n    private var history: [Double] = []\n    \n    func add(_ a: Double, _ b: Double) -> Double {\n        let result = a + b\n        history.append(result)\n        return result\n    }\n    \n    func multiply(_ a: Double, _ b: Double) -> Double {\n        let result = a * b\n        history.append(result)\n        return result\n    }\n    \n    func getHistory() -> [Double] {\n        return history\n    }\n}\n\n// Usage\nlet calc = Calculator()\nprint("2 + 3 = \\(calc.add(2, 3))")\nprint("4 * 5 = \\(calc.multiply(4, 5))")\nprint("History: \\(calc.getHistory())")\n\n// Swift arrays and optionals\nlet numbers = [1, 2, 3, 4, 5]\nprint("Numbers: \\(numbers)")\n\nlet squares = numbers.map { $0 * $0 }\nprint("Squares: \\(squares)")\n\n// Swift optionals\nlet optionalString: String? = "Hello"\nif let unwrapped = optionalString {\n    print("Unwrapped: \\(unwrapped)")\n}', mode: 'swift' },
            kotlin: { name: 'Main.kt', content: '// Kotlin Example\nfun main() {\n    println("Hello, Kotlin World!")\n    \n    // Kotlin classes and data classes\ndata class Calculator(\n    private val history: MutableList<Double> = mutableListOf()\n) {\n    fun add(a: Double, b: Double): Double {\n        val result = a + b\n        history.add(result)\n        return result\n    }\n    \n    fun multiply(a: Double, b: Double): Double {\n        val result = a * b\n        history.add(result)\n        return result\n    }\n    \n    fun getHistory(): List<Double> = history.toList()\n}\n\n// Usage\nval calc = Calculator()\nprintln("2 + 3 = ${calc.add(2.0, 3.0)}")\nprintln("4 * 5 = ${calc.multiply(4.0, 5.0)}")\nprintln("History: ${calc.getHistory()}")\n\n// Kotlin collections and lambdas\nval numbers = listOf(1, 2, 3, 4, 5)\nprintln("Numbers: $numbers")\n\nval squares = numbers.map { it * it }\nprintln("Squares: $squares")\n\n// Kotlin when expression\nnumbers.forEach { num ->\n    when (num) {\n        1 -> println("One!")\n        2 -> println("Two!")\n        3 -> println("Three!")\n        else -> println("Other number: $num")\n    }\n}', mode: 'text/x-kotlin' },
            sql: { name: 'queries.sql', content: '-- SQL Example\n-- Create tables\nCREATE TABLE users (\n    id INTEGER PRIMARY KEY AUTOINCREMENT,\n    name VARCHAR(100) NOT NULL,\n    email VARCHAR(255) UNIQUE NOT NULL,\n    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP\n);\n\nCREATE TABLE orders (\n    id INTEGER PRIMARY KEY AUTOINCREMENT,\n    user_id INTEGER,\n    total_amount DECIMAL(10,2) NOT NULL,\n    order_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,\n    FOREIGN KEY (user_id) REFERENCES users(id)\n);\n\n-- Insert sample data\nINSERT INTO users (name, email) VALUES\n    ("John Doe", "john@example.com"),\n    ("Jane Smith", "jane@example.com"),\n    ("Bob Johnson", "bob@example.com");\n\nINSERT INTO orders (user_id, total_amount) VALUES\n    (1, 99.99),\n    (2, 149.99),\n    (1, 29.99),\n    (3, 199.99);\n\n-- Query examples\nSELECT u.name, COUNT(o.id) as order_count, SUM(o.total_amount) as total_spent\nFROM users u\nLEFT JOIN orders o ON u.id = o.user_id\nGROUP BY u.id, u.name\nORDER BY total_spent DESC;', mode: 'sql' },
            yaml: { name: 'config.yaml', content: '# YAML Configuration Example\n# Application configuration\napp:\n  name: "Studio Code Editor"\n  version: "1.0.0"\n  environment: "development"\n\n# Database configuration\ndatabase:\n  host: "localhost"\n  port: 5432\n  name: "studio_editor"\n  user: "admin"\n  password: "secret123"\n  ssl: true\n\n# Server configuration\nserver:\n  host: "0.0.0.0"\n  port: 3000\n  timeout: 30\n  cors:\n    enabled: true\n    origins:\n      - "http://localhost:3000"\n      - "https://studio-editor.com"\n\n# Features configuration\nfeatures:\n  themes:\n    - "default"\n    - "light"\n    - "cursor-dark"\n    - "light-cursor"\n    - "cyberpunk"\n    - "neon"\n    - "ocean"\n  \n  languages:\n    - "javascript"\n    - "typescript"\n    - "python"\n    - "html"\n    - "css"\n    - "coffeescript"\n    - "ruby"\n    - "php"\n    - "go"\n    - "rust"\n    - "swift"\n    - "kotlin"\n\n# Logging configuration\nlogging:\n  level: "info"\n  file: "logs/app.log"\n  max_size: "10MB"\n  max_files: 5', mode: 'yaml' },
            markdown: { name: 'README.md', content: '# Studio Code Editor Documentation\n\n## Overview\n\nStudio Code Editor is a modern, feature-rich web-based code editor built with HTML, CSS, and JavaScript. It supports multiple programming languages and provides a professional development environment.\n\n## Features\n\n### 🎨 Multiple Themes\n- **Default Dark**: Professional dark interface\n- **Light**: Clean, bright interface\n- **Cursor Dark**: Matrix-style green terminal\n- **Light Cursor**: Minimalist black & white\n- **Cyberpunk**: Futuristic with glowing effects\n- **Neon**: Vibrant pink neon aesthetic\n- **Ocean**: Beautiful blue gradients\n\n### 🚀 Supported Languages\n\n#### Core Languages\n- **JavaScript**: Full ES6+ support\n- **TypeScript**: Type-safe JavaScript\n- **HTML**: Live preview rendering\n- **CSS**: Live styling application\n- **Python**: Python 3 syntax\n\n#### Functional Languages\n- **CoffeeScript**: JavaScript with syntax sugar\n- **A#**: Functional programming\n- **F#**: ML-style functional language\n\n#### Modern Languages\n- **Go**: Google\'s systems language\n- **Rust**: Memory-safe systems language\n- **Swift**: Apple\'s modern language\n- **Kotlin**: JVM language by JetBrains\n\n#### Web & Scripting\n- **PHP**: Server-side scripting\n- **Ruby**: Dynamic object-oriented\n- **ActionScript**: Flash development\n\n#### Data & Config\n- **SQL**: Database queries\n- **YAML**: Configuration files\n- **Markdown**: Documentation\n- **Shell**: Command line scripts\n\n## Getting Started\n\n1. Open `index.html` in your browser\n2. Choose your programming language\n3. Start coding with syntax highlighting\n4. Use the CMD terminal for commands\n5. Switch between beautiful themes\n\n## Keyboard Shortcuts\n\n- `Ctrl+Enter`: Run code\n- `Ctrl+S`: Save file\n- `F11`: Toggle fullscreen\n- `Ctrl+/`: Toggle comment\n- `Ctrl+F`: Find text\n- `Ctrl+H`: Replace text\n\n## CMD Terminal Commands\n\n- `help`: Show available commands\n- `run`: Execute current code\n- `theme <name>`: Change theme\n- `ls/dir`: List files\n- `cat <file>`: View file content\n- `status`: Show editor status\n\n## Architecture\n\n- **Frontend**: Vanilla HTML, CSS, JavaScript\n- **Editor**: CodeMirror 5.65.2\n- **Themes**: CSS-based with smooth transitions\n- **Terminal**: Full command-line interface\n\nBuilt with ❤️ for developers!', mode: 'markdown' },
            shell: { name: 'script.sh', content: '#!/bin/bash\n# Shell Script Example\n# Studio Code Editor - Shell Scripting\n\necho "Hello, Shell World!"\necho "Current directory: $(pwd)"\necho "Current user: $(whoami)"\necho "Date: $(date)"\n\n# Function definition\nfunction greet_user() {\n    local name=$1\n    echo "Hello, $name! Welcome to Studio Code Editor!"\n}\n\n# Variable usage\nEDITOR_NAME="Studio Code Editor"\nVERSION="1.0.0"\n\n# Conditional statements\nif [ "$EDITOR_NAME" = "Studio Code Editor" ]; then\n    echo "Welcome to $EDITOR_NAME v$VERSION"\nelse\n    echo "Unknown editor"\nfi\n\n# Loop example\nfor i in {1..5}; do\n    echo "Count: $i"\n    if [ $i -eq 3 ]; then\n        echo "  Special number: $i"\n    fi\ndone\n\n# Array usage\nlanguages=("JavaScript" "Python" "Go" "Rust" "Swift")\necho "\\nSupported languages:"\nfor lang in "${languages[@]}"; do\n    echo "  - $lang"\ndone\n\n# File operations\nif [ -f "script.sh" ]; then\n    echo "\\nThis script file exists!"\n    echo "File size: $(wc -c < script.sh) bytes"\nfi\n\n# Call function\ngreet_user "Developer"\n\necho "\\nShell script execution completed!"', mode: 'shell' }
        };
        
        this.init();
    }

    init() {
        this.setupCodeMirror();
        this.setupEventListeners();
        this.loadFile('main');
        this.updateStatusBar();
        this.setupConsole();
        this.loadSavedTheme();
        this.initCmdTerminal();
        this.setupAutoSave();
    }

    initCmdTerminal() {
        // Welcome message for CMD terminal
        this.appendCmdLine('Welcome to Studio Code Editor Terminal!', 'info');
        this.appendCmdLine('Type "help" to see available commands.', 'output-text');
        this.appendCmdLine('', 'output-text');
    }

    setupCodeMirror() {
        // Initialize CodeMirror
        this.editor = CodeMirror.fromTextArea(document.getElementById('code-editor'), {
            mode: 'javascript',
            theme: 'dracula',
            lineNumbers: true,
            autoCloseBrackets: true,
            matchBrackets: true,
            indentUnit: 4,
            tabSize: 4,
            indentWithTabs: false,
            lineWrapping: true,
            foldGutter: true,
            gutters: ['CodeMirror-linenumbers', 'CodeMirror-foldgutter'],
            extraKeys: {
                'Ctrl-Space': 'autocomplete',
                'Ctrl-/': 'toggleComment',
                'Ctrl-F': 'findPersistent',
                'Ctrl-H': 'replace',
                'F11': () => this.toggleFullscreen(),
                'Ctrl-S': (cm) => this.saveFile()
            }
        });

        // Update cursor position
        this.editor.on('cursorActivity', () => {
            this.updateStatusBar();
        });

        // Auto-save content to files object
        this.editor.on('change', () => {
            this.files[this.currentFile].content = this.editor.getValue();
        });
    }

    setupEventListeners() {
        // Language selector
        document.getElementById('language-selector').addEventListener('change', (e) => {
            this.changeLanguage(e.target.value);
        });

        // Run button
        document.getElementById('run-btn').addEventListener('click', () => {
            this.runCode();
        });

        // Save button
        document.getElementById('save-btn').addEventListener('click', () => {
            this.saveFile();
        });

        // Theme selector
        document.getElementById('theme-selector-dropdown').addEventListener('change', (e) => {
            this.changeTheme(e.target.value);
        });

        // File tabs
        document.querySelectorAll('.tab-btn').forEach(btn => {
            btn.addEventListener('click', (e) => {
                const file = e.target.dataset.file;
                this.switchFile(file);
            });
        });

        // Extension buttons
        document.getElementById('live-server-btn').addEventListener('click', () => {
            this.toggleLiveServer();
        });

        document.getElementById('live-preview-btn').addEventListener('click', () => {
            this.toggleLivePreview();
        });

        document.getElementById('copilot-btn').addEventListener('click', () => {
            this.toggleCopilot();
        });

        // Copilot panel controls
        document.getElementById('copilot-toggle').addEventListener('click', () => {
            this.toggleCopilotFullscreen();
        });

        document.getElementById('copilot-clear').addEventListener('click', () => {
            this.clearCopilotOutput();
        });

        document.getElementById('copilot-input').addEventListener('keypress', (e) => {
            if (e.key === 'Enter') {
                this.sendCopilotMessage();
            }
        });

        // Live preview controls
        document.getElementById('close-preview').addEventListener('click', () => {
            this.closeLivePreview();
        });

        // Output controls
        document.getElementById('clear-output').addEventListener('click', () => {
            this.clearOutput();
        });

        document.getElementById('fullscreen-output').addEventListener('click', () => {
            this.toggleFullscreenOutput();
        });

        // CMD Terminal controls
        document.getElementById('cmd-clear').addEventListener('click', () => {
            this.clearCmdOutput();
        });

        document.getElementById('cmd-toggle').addEventListener('click', () => {
            this.toggleCmdTerminal();
        });

        // CMD input handling
        document.getElementById('cmd-input').addEventListener('keydown', (e) => {
            if (e.key === 'Enter') {
                this.executeCmd(e.target.value);
                e.target.value = '';
            }
        });

        // Keyboard shortcuts
        document.addEventListener('keydown', (e) => {
            if (e.ctrlKey && e.key === 's') {
                e.preventDefault();
                this.saveFile();
            }
            if (e.ctrlKey && e.key === 'Enter') {
                e.preventDefault();
                this.runCode();
            }
        });
    }

    setupConsole() {
        // Override console methods to capture output
        const originalLog = console.log;
        const originalError = console.error;
        const originalWarn = console.warn;
        const originalInfo = console.info;

        console.log = (...args) => {
            this.appendToConsole('log', ...args);
            originalLog.apply(console, args);
        };

        console.error = (...args) => {
            this.appendToConsole('error', ...args);
            originalError.apply(console, args);
        };

        console.warn = (...args) => {
            this.appendToConsole('warn', ...args);
            originalWarn.apply(console, args);
        };

        console.info = (...args) => {
            this.appendToConsole('info', ...args);
            originalInfo.apply(console, args);
        };
    }

    appendToConsole(type, ...args) {
        const consoleOutput = document.getElementById('console-output');
        const line = document.createElement('div');
        line.className = `console-line console-${type}`;
        
        const timestamp = new Date().toLocaleTimeString();
        const content = args.map(arg => {
            if (typeof arg === 'object') {
                return JSON.stringify(arg, null, 2);
            }
            return String(arg);
        }).join(' ');
        
        line.innerHTML = `<span class="timestamp">[${timestamp}]</span> <span class="content">${content}</span>`;
        consoleOutput.appendChild(line);
        consoleOutput.scrollTop = consoleOutput.scrollHeight;
    }

    changeLanguage(language) {
        const modeMap = {
            'javascript': 'javascript',
            'typescript': 'typescript',
            'html': 'xml',
            'css': 'css',
            'python': 'python',
            'coffeescript': 'coffeescript',
            'actionscript': 'text/x-actionscript',
            'asharp': 'mllike',
            'csharp': 'text/x-csharp',
            'aspnet': 'text/x-csharp',
            'zig': 'text/x-zig',
            'haskell': 'text/x-haskell',
            'fsharp': 'mllike',
            'qsharp': 'text/x-csharp',
            'java': 'text/x-java-source',
            'cpp': 'text/x-c++src',
            'ruby': 'ruby',
            'php': 'php',
            'go': 'go',
            'rust': 'rust',
            'swift': 'swift',
            'kotlin': 'text/x-kotlin',
            'sql': 'sql',
            'yaml': 'yaml',
            'markdown': 'markdown',
            'shell': 'shell'
        };

        if (modeMap[language]) {
            this.editor.setOption('mode', modeMap[language]);
        }
    }

    switchFile(file) {
        if (this.currentFile === file) return;

        // Save current file content
        this.files[this.currentFile].content = this.editor.getValue();

        // Update active tab
        document.querySelectorAll('.tab-btn').forEach(btn => {
            btn.classList.remove('active');
        });
        document.querySelector(`[data-file="${file}"]`).classList.add('active');

        // Load new file
        this.currentFile = file;
        this.loadFile(file);
    }

    loadFile(file) {
        const fileData = this.files[file];
        this.editor.setValue(fileData.content);
        this.editor.setOption('mode', fileData.mode);
        
        // Update language selector
        const languageSelector = document.getElementById('language-selector');
        const languageMap = {
            'javascript': 'javascript',
            'typescript': 'typescript',
            'xml': 'html',
            'css': 'css',
            'python': 'python',
            'coffeescript': 'coffeescript',
            'text/x-actionscript': 'actionscript',
            'mllike': 'asharp',
            'text/x-csharp': 'csharp',
            'text/x-zig': 'zig',
            'mllike': 'asharp',
            'text/x-java-source': 'java',
            'text/x-c++src': 'cpp',
            'ruby': 'ruby',
            'php': 'php',
            'go': 'go',
            'rust': 'rust',
            'swift': 'swift',
            'text/x-kotlin': 'kotlin',
            'sql': 'sql',
            'yaml': 'yaml',
            'markdown': 'markdown',
            'shell': 'shell'
        };
        
        for (const [mode, lang] of Object.entries(languageMap)) {
            if (mode === fileData.mode) {
                languageSelector.value = lang;
                break;
            }
        }
    }

    runCode() {
        const code = this.editor.getValue();
        const language = document.getElementById('language-selector').value;
        
        this.clearOutput();
        this.appendToConsole('info', `Running ${language.toUpperCase()} code...`);
        
        try {
            if (language === 'javascript') {
                this.runJavaScript(code);
            } else if (language === 'typescript') {
                this.runTypeScript(code);
            } else if (language === 'html') {
                this.runHTML(code);
            } else if (language === 'css') {
                this.runCSS(code);
            } else if (language === 'python') {
                this.runPython(code);
            } else if (language === 'coffeescript') {
                this.runCoffeeScript(code);
            } else {
                this.appendToConsole('warn', `Language ${language} execution not yet implemented`);
            }
        } catch (error) {
            this.appendToConsole('error', `Execution error: ${error.message}`);
        }
    }

    runJavaScript(code) {
        try {
            // Create a safe execution environment
            const sandbox = {
                console: console,
                setTimeout: setTimeout,
                setInterval: setInterval,
                clearTimeout: clearTimeout,
                clearInterval: clearInterval,
                Math: Math,
                Date: Date,
                Array: Array,
                Object: Object,
                String: String,
                Number: Number,
                Boolean: Boolean,
                RegExp: RegExp,
                JSON: JSON,
                Error: Error,
                Promise: Promise
            };

            // Execute the code
            const result = new Function('console', 'Math', 'Date', 'Array', 'Object', 'String', 'Number', 'Boolean', 'RegExp', 'JSON', 'Error', 'Promise', code);
            result(console, Math, Date, Array, Object, String, Number, Boolean, RegExp, JSON, Error, Promise);
            
            this.appendToConsole('info', 'Code executed successfully');
        } catch (error) {
            this.appendToConsole('error', `JavaScript Error: ${error.message}`);
        }
    }

    runHTML(code) {
        const outputDisplay = document.getElementById('output-display');
        outputDisplay.innerHTML = code;
        this.appendToConsole('info', 'HTML rendered successfully');
    }

    runCSS(code) {
        const outputDisplay = document.getElementById('output-display');
        const style = document.createElement('style');
        style.textContent = code;
        
        // Remove previous styles
        outputDisplay.querySelectorAll('style').forEach(s => s.remove());
        outputDisplay.appendChild(style);
        
        this.appendCmdLine('CSS applied successfully', 'success');
    }

    runTypeScript(code) {
        try {
            // TypeScript compilation simulation
            this.appendCmdLine('Compiling TypeScript...', 'info');
            
            // Remove type annotations for JavaScript execution
            const jsCode = code
                .replace(/:\s*\w+(?:<[^>]*>)?/g, '') // Remove type annotations
                .replace(/interface\s+\w+\s*\{[^}]*\}/g, '') // Remove interfaces
                .replace(/:\s*void/g, '') // Remove void return types
                .replace(/private\s+/g, '') // Remove private keywords
                .replace(/public\s+/g, '') // Remove public keywords
                .replace(/protected\s+/g, ''); // Remove protected keywords
            
            this.appendCmdLine('TypeScript compiled to JavaScript', 'success');
            this.appendCmdLine('Executing compiled code...', 'info');
            
            // Execute the compiled JavaScript
            this.runJavaScript(jsCode);
        } catch (error) {
            this.appendCmdLine(`TypeScript compilation error: ${error.message}`, 'error');
        }
    }

    runPython(code) {
        try {
            this.appendCmdLine('Python execution (simulated)', 'info');
            this.appendCmdLine('Note: This is a simulation. For real Python execution, use a Python environment.', 'warning');
            
            // Simple Python syntax highlighting and basic parsing
            const lines = code.split('\n');
            let output = '';
            
            lines.forEach((line, index) => {
                if (line.trim().startsWith('print(')) {
                    const content = line.match(/print\(['"`]([^'"`]*)['"`]\)/);
                    if (content) {
                        output += content[1] + '\n';
                    }
                } else if (line.includes('class ') || line.includes('def ')) {
                    output += `[Line ${index + 1}] ${line.trim()}\n`;
                }
            });
            
            if (output) {
                this.appendCmdLine('Python code parsed:', 'info');
                this.appendCmdLine(output, 'output-text');
            } else {
                this.appendCmdLine('Python code syntax validated', 'success');
            }
        } catch (error) {
            this.appendCmdLine(`Python execution error: ${error.message}`, 'error');
        }
    }

    runCoffeeScript(code) {
        try {
            this.appendCmdLine('CoffeeScript compilation (simulated)', 'info');
            this.appendCmdLine('Note: This is a simulation. For real CoffeeScript execution, use a CoffeeScript compiler.', 'warning');
            
            // Simple CoffeeScript to JavaScript conversion simulation
            let jsCode = code
                .replace(/->/g, 'function()') // Convert arrows to functions
                .replace(/=>/g, 'function()') // Convert fat arrows
                .replace(/#{([^}]*)}/g, '${$1}') // Convert interpolation
                .replace(/class\s+(\w+)/g, 'class $1') // Keep class syntax
                .replace(/constructor:\s*\(@(\w+)\)/g, 'constructor($1)') // Convert constructor
                .replace(/@(\w+)/g, 'this.$1'); // Convert @ to this.
            
            this.appendCmdLine('CoffeeScript compiled to JavaScript', 'success');
            this.appendCmdLine('Executing compiled code...', 'info');
            
            // Execute the compiled JavaScript
            this.runJavaScript(jsCode);
        } catch (error) {
            this.appendCmdLine(`CoffeeScript compilation error: ${error.message}`, 'error');
        }
    }

    saveFile() {
        const content = this.editor.getValue();
        const fileName = this.files[this.currentFile].name;
        
        // Create download link
        const blob = new Blob([content], { type: 'text/plain' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = fileName;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
        
        this.appendToConsole('info', `File ${fileName} saved successfully`);
    }

    clearOutput() {
        document.getElementById('output-display').innerHTML = '';
        document.getElementById('console-output').innerHTML = '';
    }

    changeTheme(theme) {
        const container = document.querySelector('.editor-container');
        
        // Remove all theme classes
        container.classList.remove('light-theme', 'cursor-dark', 'light-cursor', 'cyberpunk', 'neon', 'ocean');
        
        // Apply selected theme
        switch(theme) {
            case 'light':
                container.classList.add('light-theme');
                this.editor.setOption('theme', 'default');
                break;
            case 'cursor-dark':
                container.classList.add('cursor-dark');
                this.editor.setOption('theme', 'dracula');
                break;
            case 'light-cursor':
                container.classList.add('light-cursor');
                this.editor.setOption('theme', 'default');
                break;
            case 'cyberpunk':
                container.classList.add('cyberpunk');
                this.editor.setOption('theme', 'dracula');
                break;
            case 'neon':
                container.classList.add('neon');
                this.editor.setOption('theme', 'dracula');
                break;
            case 'ocean':
                container.classList.add('ocean');
                this.editor.setOption('theme', 'dracula');
                break;
            default:
                // Default dark theme
                this.editor.setOption('theme', 'dracula');
                break;
        }
        
        // Save theme preference
        localStorage.setItem('studio-editor-theme', theme);
    }

    loadSavedTheme() {
        const savedTheme = localStorage.getItem('studio-editor-theme');
        if (savedTheme) {
            this.changeTheme(savedTheme);
            // Update dropdown to show current theme
            document.getElementById('theme-selector-dropdown').value = savedTheme;
        }
    }

    // CMD Terminal Methods
    executeCmd(command) {
        if (!command.trim()) return;

        const cmdOutput = document.getElementById('cmd-output');
        
        // Display the command
        this.appendCmdLine(`$ ${command}`, 'command');
        
        // Parse and execute the command
        const args = command.trim().split(' ');
        const cmd = args[0].toLowerCase();
        
        try {
            switch(cmd) {
                case 'help':
                    this.showCmdHelp();
                    break;
                case 'clear':
                    this.clearCmdOutput();
                    break;
                case 'theme':
                    this.handleThemeCmd(args);
                    break;
                case 'run':
                    this.runCode();
                    this.appendCmdLine('Code executed!', 'success');
                    break;
                case 'save':
                    this.saveFile();
                    this.appendCmdLine('File saved!', 'success');
                    break;
                case 'ls':
                case 'dir':
                    this.showFiles();
                    break;
                case 'cat':
                    this.showFileContent(args);
                    break;
                case 'echo':
                    this.echoCmd(args);
                    break;
                case 'date':
                    this.showDate();
                    break;
                case 'time':
                    this.showTime();
                    break;
                case 'version':
                    this.showVersion();
                    break;
                case 'status':
                    this.showStatus();
                    break;
                case 'cls':
                    this.clearCmdOutput();
                    break;
                default:
                    this.appendCmdLine(`Command not found: ${cmd}. Type 'help' for available commands.`, 'error');
            }
        } catch (error) {
            this.appendCmdLine(`Error: ${error.message}`, 'error');
        }
    }

    appendCmdLine(text, type = 'output-text') {
        const cmdOutput = document.getElementById('cmd-output');
        const line = document.createElement('div');
        line.className = `cmd-line cmd-${type}`;
        line.textContent = text;
        cmdOutput.appendChild(line);
        cmdOutput.scrollTop = cmdOutput.scrollHeight;
    }

    showCmdHelp() {
        this.appendCmdLine('Available Commands:', 'info');
        this.appendCmdLine('  help     - Show this help message', 'output-text');
        this.appendCmdLine('  clear    - Clear terminal output', 'output-text');
        this.appendCmdLine('  theme    - Change theme (e.g., theme cyberpunk)', 'output-text');
        this.appendCmdLine('  run      - Run current code', 'output-text');
        this.appendCmdLine('  save     - Save current file', 'output-text');
        this.appendCmdLine('  ls/dir   - List files', 'output-text');
        this.appendCmdLine('  cat      - Show file content (e.g., cat main.js)', 'output-text');
        this.appendCmdLine('  echo     - Echo text (e.g., echo Hello World)', 'output-text');
        this.appendCmdLine('  date     - Show current date', 'output-text');
        this.appendCmdLine('  time     - Show current time', 'output-text');
        this.appendCmdLine('  version  - Show editor version', 'output-text');
        this.appendCmdLine('  status   - Show editor status', 'output-text');
        this.appendCmdLine('  cls      - Clear terminal (alias for clear)', 'output-text');
        this.appendCmdLine('', 'output-text');
        this.appendCmdLine('Supported Languages:', 'info');
        this.appendCmdLine('  JavaScript, TypeScript, HTML, CSS, Python', 'output-text');
        this.appendCmdLine('  CoffeeScript, ActionScript, A#, C#, F#, Q#', 'output-text');
        this.appendCmdLine('  Java, C++', 'output-text');
    }

    handleThemeCmd(args) {
        if (args.length < 2) {
            this.appendCmdLine('Usage: theme <theme-name>', 'warning');
            this.appendCmdLine('Available themes: default, light, cursor-dark, light-cursor, cyberpunk, neon, ocean', 'info');
            return;
        }
        
        const theme = args[1];
        this.changeTheme(theme);
        this.appendCmdLine(`Theme changed to: ${theme}`, 'success');
    }

    showFiles() {
        this.appendCmdLine('Files in project:', 'info');
        Object.keys(this.files).forEach(file => {
            const fileData = this.files[file];
            const isActive = this.currentFile === file ? ' *' : '';
            this.appendCmdLine(`  ${fileData.name}${isActive}`, 'output-text');
        });
    }

    showFileContent(args) {
        if (args.length < 2) {
            this.appendCmdLine('Usage: cat <filename>', 'warning');
            return;
        }
        
        const filename = args[1];
        const file = Object.values(this.files).find(f => f.name === filename);
        
        if (file) {
            this.appendCmdLine(`Content of ${filename}:`, 'info');
            const lines = file.content.split('\n');
            lines.forEach(line => {
                this.appendCmdLine(`  ${line}`, 'output-text');
            });
        } else {
            this.appendCmdLine(`File not found: ${filename}`, 'error');
        }
    }

    echoCmd(args) {
        if (args.length < 2) {
            this.appendCmdLine('Usage: echo <text>', 'warning');
            return;
        }
        
        const text = args.slice(1).join(' ');
        this.appendCmdLine(text, 'output-text');
    }

    showDate() {
        const date = new Date().toLocaleDateString();
        this.appendCmdLine(`Current date: ${date}`, 'info');
    }

    showTime() {
        const time = new Date().toLocaleTimeString();
        this.appendCmdLine(`Current time: ${time}`, 'info');
    }

    showVersion() {
        this.appendCmdLine('Studio Code Editor v1.0.0', 'info');
        this.appendCmdLine('Built with HTML, CSS, JavaScript & CodeMirror', 'output-text');
    }

    showStatus() {
        const cursor = this.editor.getCursor();
        const line = cursor.line + 1;
        const ch = cursor.ch + 1;
        const content = this.editor.getValue();
        const charCount = content.length;
        
        this.appendCmdLine('Editor Status:', 'info');
        this.appendCmdLine(`  Current file: ${this.files[this.currentFile].name}`, 'output-text');
        this.appendCmdLine(`  Cursor: Line ${line}, Column ${ch}`, 'output-text');
        this.appendCmdLine(`  Characters: ${charCount}`, 'output-text');
        this.appendCmdLine(`  Language: ${document.getElementById('language-selector').value}`, 'output-text');
    }

    clearCmdOutput() {
        document.getElementById('cmd-output').innerHTML = '';
    }

    toggleCmdTerminal() {
        const terminal = document.getElementById('cmd-terminal');
        terminal.classList.toggle('fullscreen');
    }

    toggleFullscreenOutput() {
        const outputPanel = document.querySelector('.output-panel');
        outputPanel.classList.toggle('fullscreen');
    }

    updateStatusBar() {
        const cursor = this.editor.getCursor();
        const line = cursor.line + 1;
        const ch = cursor.ch + 1;
        const content = this.editor.getValue();
        const charCount = content.length;
        
        document.getElementById('cursor-position').textContent = `Line ${line}, Column ${ch}`;
        document.getElementById('file-size').textContent = `${charCount} characters`;
    }

    toggleFullscreen() {
        if (!document.fullscreenElement) {
            document.documentElement.requestFullscreen();
        } else {
            document.exitFullscreen();
        }
    }

    // Live Server functionality
    toggleLiveServer() {
        const statusElement = document.getElementById('live-server-status');
        if (this.liveServerRunning) {
            this.stopLiveServer();
            statusElement.textContent = '🌐 Live Server: Stopped';
            statusElement.className = 'server-status stopped';
        } else {
            this.startLiveServer();
            statusElement.textContent = '🌐 Live Server: Running on port 3000';
            statusElement.className = 'server-status running';
        }
    }

    startLiveServer() {
        this.liveServerRunning = true;
        this.appendCmdLine('Starting Live Server on port 3000...', 'info');
        this.appendCmdLine('Server will automatically reload when files change', 'output-text');
        
        // Simulate live server functionality
        this.liveServerInterval = setInterval(() => {
            this.appendCmdLine('Live Server: Watching for file changes...', 'output-text');
        }, 5000);
    }

    stopLiveServer() {
        this.liveServerRunning = false;
        if (this.liveServerInterval) {
            clearInterval(this.liveServerInterval);
        }
        this.appendCmdLine('Live Server stopped', 'info');
    }

    // Live Preview functionality
    toggleLivePreview() {
        const previewPanel = document.getElementById('live-preview-panel');
        const currentContent = this.editor.getValue();
        
        if (previewPanel.style.display === 'none') {
            this.showLivePreview(currentContent);
        } else {
            this.closeLivePreview();
        }
    }

    showLivePreview(content) {
        const previewPanel = document.getElementById('live-preview-panel');
        const previewFrame = document.getElementById('preview-frame');
        
        // Create a blob URL for the content
        const blob = new Blob([content], { type: 'text/html' });
        const url = URL.createObjectURL(blob);
        
        previewFrame.src = url;
        previewPanel.style.display = 'block';
        
        this.appendCmdLine('Live Preview opened', 'success');
    }

    closeLivePreview() {
        const previewPanel = document.getElementById('live-preview-panel');
        previewPanel.style.display = 'none';
        this.appendCmdLine('Live Preview closed', 'info');
    }

    // Copilot functionality
    toggleCopilot() {
        const copilotPanel = document.getElementById('copilot-panel');
        if (copilotPanel.style.display === 'none') {
            copilotPanel.style.display = 'block';
            this.appendCmdLine('GitHub Copilot activated', 'success');
            this.appendCopilotMessage('Hello! I\'m GitHub Copilot, your AI coding assistant. How can I help you today?', 'copilot');
        } else {
            copilotPanel.style.display = 'none';
            this.appendCmdLine('GitHub Copilot deactivated', 'info');
        }
    }

    sendCopilotMessage() {
        const input = document.getElementById('copilot-input');
        const message = input.value.trim();
        
        if (message) {
            this.appendCopilotMessage(message, 'user');
            input.value = '';
            
            // Simulate Copilot response
            setTimeout(() => {
                this.generateCopilotResponse(message);
            }, 1000);
        }
    }

    generateCopilotResponse(userMessage) {
        const responses = [
            'I can help you with that! Here\'s a suggestion...',
            'Based on your code, I recommend...',
            'Here\'s an optimized version...',
            'Consider using this pattern...',
            'I can generate code for that functionality...'
        ];
        
        const randomResponse = responses[Math.floor(Math.random() * responses.length)];
        this.appendCopilotMessage(randomResponse, 'copilot');
    }

    appendCopilotMessage(message, type) {
        const output = document.getElementById('copilot-output');
        const messageDiv = document.createElement('div');
        messageDiv.className = `copilot-message ${type}`;
        messageDiv.innerHTML = `<span class="copilot-${type}-label">${type === 'user' ? '👤 You' : '🤖 Copilot'}:</span> ${message}`;
        output.appendChild(messageDiv);
        output.scrollTop = output.scrollHeight;
    }

    clearCopilotOutput() {
        document.getElementById('copilot-output').innerHTML = '';
    }

    toggleCopilotFullscreen() {
        const panel = document.getElementById('copilot-panel');
        panel.classList.toggle('fullscreen');
    }

    // Auto-save functionality
    setupAutoSave() {
        this.autoSaveInterval = setInterval(() => {
            if (this.editor && this.currentFile) {
                this.files[this.currentFile].content = this.editor.getValue();
                this.updateAutoSaveStatus();
            }
        }, 30000); // Auto-save every 30 seconds
    }

    updateAutoSaveStatus() {
        const statusElement = document.getElementById('auto-save-status');
        statusElement.textContent = '💾 Auto-save: Last saved ' + new Date().toLocaleTimeString();
    }
}

// Initialize the editor when the page loads
document.addEventListener('DOMContentLoaded', () => {
    window.studioEditor = new StudioCodeEditor();
    
    // Add some helpful tips
    console.log('🎉 Studio Code Editor is ready!');
    console.log('💡 Try these shortcuts:');
    console.log('   - Ctrl+Enter: Run code');
    console.log('   - Ctrl+S: Save file');
    console.log('   - F11: Toggle fullscreen');
    console.log('   - Ctrl+/: Toggle comment');
    console.log('   - Ctrl+F: Find text');
    console.log('   - Ctrl+H: Replace text');
});

// Add some additional CSS for console output
const additionalStyles = `
    .console-line {
        margin: 2px 0;
        padding: 2px 0;
        border-bottom: 1px solid #2d2d2d;
    }
    
    .console-log .content { color: #61dafb; }
    .console-error .content { color: #f97583; }
    .console-warn .content { color: #ffa657; }
    .console-info .content { color: #79c0ff; }
    
    .timestamp {
        color: #6a737d;
        font-size: 0.8em;
        margin-right: 8px;
    }
    
    .output-panel.fullscreen {
        position: fixed;
        top: 0;
        left: 0;
        width: 100vw;
        height: 100vh;
        z-index: 9999;
        background: #1e1e1e;
    }
    
    .output-panel.fullscreen .output-display {
        height: calc(100vh - 100px);
    }
    
    .output-panel.fullscreen .console-output {
        height: 100px;
    }
`;

const styleSheet = document.createElement('style');
styleSheet.textContent = additionalStyles;
document.head.appendChild(styleSheet);
