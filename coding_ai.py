#!/usr/bin/env python3
"""
Studio Code Editor - Coding AI Assistant
A Python-based AI that helps with code generation, analysis, and suggestions
"""

import re
import json
import random
from typing import Dict, List, Optional, Tuple
from datetime import datetime

class CodingAI:
    def __init__(self):
        """Initialize the Coding AI with knowledge base and patterns"""
        self.name = "Studio Code AI"
        self.version = "1.0.0"
        self.languages = {
            'python': 'Python',
            'javascript': 'JavaScript',
            'typescript': 'TypeScript',
            'html': 'HTML',
            'css': 'CSS',
            'java': 'Java',
            'cpp': 'C++',
            'csharp': 'C#',
            'aspnet': 'ASP.NET',
            'zig': 'Zig',
            'haskell': 'Haskell',
            'go': 'Go',
            'rust': 'Rust',
            'swift': 'Swift',
            'kotlin': 'Kotlin',
            'ruby': 'Ruby',
            'php': 'PHP',
            'sql': 'SQL'
        }
        
        # Code patterns and templates
        self.code_patterns = {
            'python': {
                'class': 'class {name}:\n    def __init__(self):\n        pass\n    \n    def {method}(self):\n        pass',
                'function': 'def {name}({params}):\n    """{docstring}"""\n    {body}\n    return {return_value}',
                'loop': 'for {item} in {iterable}:\n    {body}',
                'condition': 'if {condition}:\n    {body}\nelif {elif_condition}:\n    {elif_body}\nelse:\n    {else_body}'
            },
            'javascript': {
                'class': 'class {name} {{\n    constructor() {{\n        \n    }}\n    \n    {method}() {{\n        \n    }}\n}}',
                'function': 'function {name}({params}) {{\n    // {comment}\n    {body}\n    return {return_value};\n}}',
                'arrow': 'const {name} = ({params}) => {{\n    {body}\n    return {return_value};\n}}',
                'loop': 'for (let {item} of {iterable}) {{\n    {body}\n}}'
            },
            'html': {
                'basic': '<!DOCTYPE html>\n<html lang="en">\n<head>\n    <meta charset="UTF-8">\n    <meta name="viewport" content="width=device-width, initial-scale=1.0">\n    <title>{title}</title>\n</head>\n<body>\n    {content}\n</body>\n</html>',
                'form': '<form action="{action}" method="{method}">\n    <label for="{id}">{label}:</label>\n    <input type="{type}" id="{id}" name="{name}" required>\n    <button type="submit">{button_text}</button>\n</form>'
            },
            'css': {
                'flexbox': '.{class_name} {{\n    display: flex;\n    justify-content: {justify};\n    align-items: {align};\n    flex-direction: {direction};\n}}',
                'grid': '.{class_name} {{\n    display: grid;\n    grid-template-columns: {columns};\n    grid-gap: {gap};\n}}',
                'animation': '@keyframes {name} {{\n    from {{ {from_props} }}\n    to {{ {to_props} }}\n}}'
            },
            'aspnet': {
                'controller': '[ApiController]\n[Route("api/[controller]")]\npublic class {name}Controller : ControllerBase\n{{\n    [HttpGet]\n    public ActionResult<IEnumerable<{model}>> Get()\n    {{\n        // Implementation\n        return Ok(new List<{model}>());\n    }}\n    \n    [HttpGet("{{id}}")]\n    public ActionResult<{model}> Get(int id)\n    {{\n        // Implementation\n        return Ok(new {model}());\n    }}\n    \n    [HttpPost]\n    public ActionResult<{model}> Create({model} model)\n    {{\n        // Implementation\n        return CreatedAtAction(nameof(Get), new {{ id = model.Id }}, model);\n    }}\n}}',
                'minimal_api': 'var builder = WebApplication.CreateBuilder(args);\n\n// Add services\nbuilder.Services.AddControllers();\n\nvar app = builder.Build();\n\n// Configure pipeline\napp.UseHttpsRedirection();\napp.UseAuthorization();\napp.MapControllers();\n\n// Minimal API endpoints\napp.MapGet("/", () => "Hello ASP.NET Core!");\napp.MapGet("/api/hello/{{name}}", (string name) => $"Hello, {{name}}!");\n\napp.Run();',
                'model': 'public class {name}\n{{\n    public int Id {{ get; set; }}\n    public string Name {{ get; set; }} = string.Empty;\n    public string Email {{ get; set; }} = string.Empty;\n    public DateTime CreatedAt {{ get; set; }} = DateTime.UtcNow;\n}}',
                'service': 'public interface I{name}Service\n{{\n    Task<IEnumerable<{model}>> GetAllAsync();\n    Task<{model}> GetByIdAsync(int id);\n    Task<{model}> CreateAsync({model} model);\n    Task UpdateAsync({model} model);\n    Task DeleteAsync(int id);\n}}\n\npublic class {name}Service : I{name}Service\n{{\n    private readonly ApplicationDbContext _context;\n    \n    public {name}Service(ApplicationDbContext context)\n    {{\n        _context = context;\n    }}\n    \n    public async Task<IEnumerable<{model}>> GetAllAsync()\n    {{\n        return await _context.{model}s.ToListAsync();\n    }}\n    \n    public async Task<{model}> GetByIdAsync(int id)\n    {{\n        return await _context.{model}s.FindAsync(id);\n    }}\n    \n    public async Task<{model}> CreateAsync({model} model)\n    {{\n        _context.{model}s.Add(model);\n        await _context.SaveChangesAsync();\n        return model;\n    }}\n    \n    public async Task UpdateAsync({model} model)\n    {{\n        _context.{model}s.Update(model);\n        await _context.SaveChangesAsync();\n    }}\n    \n    public async Task DeleteAsync(int id)\n    {{\n        var model = await _context.{model}s.FindAsync(id);\n        if (model != null)\n        {{\n            _context.{model}s.Remove(model);\n            await _context.SaveChangesAsync();\n        }}\n    }}\n}}'
            },
            'zig': {
                'main': 'const std = @import("std");\n\npub fn main() !void {{\n    const stdout = std.io.getStdOut().writer();\n    try stdout.print("Hello, Zig World!\\n", .{{}});\n}}',
                'struct': 'const {name} = struct {{\n    {fields}\n    \n    pub fn {method}(self: {name}) {return_type} {{\n        // Implementation\n        {body}\n    }}\n}};',
                'function': 'fn {name}({params}) {return_type} {{\n    // Function implementation\n    {body}\n}}',
                'error_handling': 'fn {name}({params}) !{return_type} {{\n    if ({condition}) {{\n        return error.{error_type};\n    }}\n    \n    {body}\n    return {return_value};\n}}',
                'comptime': 'fn {name}(comptime T: type, value: T) T {{\n    // Compile-time function\n    {body}\n}}'
            },
            'haskell': {
                'main': 'module Main where\n\nmain :: IO ()\nmain = do\n    putStrLn "Hello, Haskell World!"',
                'function': '{name} :: {type_signature}\n{name} {params} = {body}',
                'list_ops': 'let {list_name} = [{start}..{end}]\nlet {result} = map (\\x -> {operation}) {list_name}',
                'pattern_matching': '{name} :: {type_signature}\n{name} {pattern1} = {result1}\n{name} {pattern2} = {result2}',
                'monad': 'do\n    {action1}\n    {action2}\n    return {result}'
            }
        }
        
        # Common code suggestions
        self.suggestions = {
            'python': [
                'Use type hints for better code documentation',
                'Consider using dataclasses for simple data structures',
                'Use list comprehensions for cleaner code',
                'Implement proper error handling with try-except',
                'Follow PEP 8 style guidelines'
            ],
            'javascript': [
                'Use const and let instead of var',
                'Implement proper error handling with try-catch',
                'Use template literals for string interpolation',
                'Consider using async/await for asynchronous operations',
                'Follow consistent naming conventions'
            ],
            'general': [
                'Add meaningful comments to complex logic',
                'Use descriptive variable and function names',
                'Implement proper error handling',
                'Write unit tests for critical functions',
                'Follow language-specific style guides'
            ],
            'aspnet': [
                'Use dependency injection for services',
                'Implement proper exception handling with try-catch',
                'Use async/await for database operations',
                'Follow REST API conventions',
                'Implement proper validation using Data Annotations',
                'Use Entity Framework Core for data access',
                'Implement logging using ILogger',
                'Use configuration patterns for app settings'
            ],
            'zig': [
                'Use error sets for proper error handling',
                'Leverage comptime for compile-time features',
                'Use optional types (?T) for nullable values',
                'Implement proper memory management',
                'Use structs for data organization',
                'Follow Zig naming conventions',
                'Use built-in functions (@sqrt, @mod, etc.)',
                'Implement proper error propagation with try/catch'
            ],
            'haskell': [
                'Use type signatures for better code documentation',
                'Leverage pattern matching for cleaner code',
                'Use higher-order functions like map, filter, fold',
                'Implement proper error handling with Maybe/Either',
                'Use list comprehensions for data transformation',
                'Follow functional programming principles',
                'Use monads for sequential operations',
                'Implement pure functions when possible'
            ]
        }
        
        # Code analysis patterns
        self.analysis_patterns = {
            'complexity': r'(for\s+\w+\s+in|while\s+\w+|if\s+\w+:|def\s+\w+)',
            'naming': r'([a-z_][a-z0-9_]*)\s*[=:]',
            'comments': r'#.*$|//.*$|/\*.*?\*/',
            'functions': r'def\s+(\w+)|function\s+(\w+)|(\w+)\s*\([^)]*\)\s*=>'
        }
    
    def analyze_code(self, code: str, language: str) -> Dict:
        """Analyze code and provide insights"""
        analysis = {
            'language': language,
            'lines': len(code.split('\n')),
            'characters': len(code),
            'complexity': 0,
            'suggestions': [],
            'issues': [],
            'quality_score': 0
        }
        
        # Count complexity indicators
        complexity_matches = re.findall(self.analysis_patterns['complexity'], code, re.MULTILINE)
        analysis['complexity'] = len(complexity_matches)
        
        # Check for potential issues
        if language == 'python':
            if 'print(' in code and 'logging' not in code:
                analysis['suggestions'].append('Consider using logging instead of print for production code')
            
            if 'except:' in code:
                analysis['issues'].append('Bare except clause - specify exception types')
                
        elif language == 'javascript':
            if 'var ' in code:
                analysis['issues'].append('Consider using const/let instead of var')
            
            if '===' not in code and '==' in code:
                analysis['suggestions'].append('Use strict equality (===) when possible')
        
        # Calculate quality score
        analysis['quality_score'] = max(0, 100 - analysis['complexity'] * 5 - len(analysis['issues']) * 10)
        
        # Add language-specific suggestions
        if language in self.suggestions:
            analysis['suggestions'].extend(random.sample(self.suggestions[language], 2))
        
        return analysis
    
    def generate_code(self, language: str, pattern: str, **kwargs) -> str:
        """Generate code based on patterns and parameters"""
        if language not in self.code_patterns or pattern not in self.code_patterns[language]:
            return f"// Code generation not supported for {language} - {pattern}"
        
        template = self.code_patterns[language][pattern]
        
        # Fill in default values if not provided
        defaults = {
            'name': 'MyClass',
            'method': 'myMethod',
            'params': 'param1, param2',
            'docstring': 'Function description',
            'body': 'pass',
            'return_value': 'None',
            'item': 'item',
            'iterable': 'items',
            'condition': 'condition',
            'body': 'pass',
            'elif_condition': 'other_condition',
            'elif_body': 'pass',
            'else_body': 'pass',
            'comment': 'Function description',
            'title': 'My Page',
            'content': '<h1>Hello World</h1>',
            'action': '/submit',
            'method': 'POST',
            'id': 'input1',
            'label': 'Input Label',
            'type': 'text',
            'name': 'input1',
            'button_text': 'Submit',
            'class_name': 'container',
            'justify': 'center',
            'align': 'center',
            'direction': 'row',
            'columns': '1fr 1fr 1fr',
            'gap': '1rem',
            'name': 'fadeIn',
            'from_props': 'opacity: 0',
            'to_props': 'opacity: 1'
        }
        
        # Update defaults with provided kwargs
        defaults.update(kwargs)
        
        # Fill template
        result = template
        for key, value in defaults.items():
            result = result.replace(f'{{{key}}}', str(value))
        
        return result
    
    def suggest_improvements(self, code: str, language: str) -> List[str]:
        """Suggest code improvements"""
        suggestions = []
        
        if language == 'python':
            if 'print(' in code:
                suggestions.append('Replace print() with logging for better debugging control')
            
            if 'except:' in code:
                suggestions.append('Specify exception types in except clauses')
            
            if 'range(len(' in code:
                suggestions.append('Use enumerate() instead of range(len()) for cleaner code')
                
        elif language == 'javascript':
            if 'var ' in code:
                suggestions.append('Replace var with const or let for better scoping')
            
            if 'function(' in code:
                suggestions.append('Consider using arrow functions for shorter syntax')
            
            if 'callback(' in code:
                suggestions.append('Use Promises or async/await instead of callbacks')
        
        # General suggestions
        if len(code.split('\n')) > 50:
            suggestions.append('Consider breaking large functions into smaller, focused functions')
        
        if code.count('if') > 10:
            suggestions.append('Consider using switch statements or lookup tables to reduce if-else chains')
        
        return suggestions
    
    def explain_code(self, code: str, language: str) -> str:
        """Explain what the code does"""
        explanation = f"Code Analysis for {language.upper()}:\n\n"
        
        lines = code.split('\n')
        explanation += f"Total lines: {len(lines)}\n"
        explanation += f"Total characters: {len(code)}\n\n"
        
        # Analyze structure
        functions = re.findall(r'def\s+(\w+)|function\s+(\w+)|(\w+)\s*\([^)]*\)\s*=>', code)
        if functions:
            explanation += "Functions/Methods found:\n"
            for func in functions:
                if func[0]: explanation += f"- {func[0]}\n"
                elif func[1]: explanation += f"- {func[1]}\n"
                elif func[2]: explanation += f"- {func[2]}\n"
            explanation += "\n"
        
        # Identify patterns
        if 'class ' in code:
            explanation += "This code defines a class structure.\n"
        
        if 'import ' in code or 'from ' in code:
            explanation += "This code imports external modules/libraries.\n"
        
        if 'try:' in code or 'except:' in code:
            explanation += "This code includes error handling.\n"
        
        if 'for ' in code or 'while ' in code:
            explanation += "This code contains loops for iteration.\n"
        
        return explanation
    
    def get_help(self) -> str:
        """Get help information about the AI"""
        help_text = f"""
{self.name} v{self.version} - Coding Assistant

Available Commands:
- analyze <code> <language>  : Analyze code quality and provide insights
- generate <language> <pattern> [params] : Generate code templates
- suggest <code> <language>  : Suggest code improvements
- explain <code> <language>  : Explain what code does
- help                       : Show this help message
- languages                  : List supported languages
- patterns <language>        : Show available code patterns

Supported Languages: {', '.join(self.languages.keys())}

Examples:
- generate python class name=Calculator
- analyze "def hello(): print('world')" python
- suggest "var x = 1" javascript
        """
        return help_text.strip()
    
    def process_command(self, command: str, *args) -> str:
        """Process AI commands"""
        try:
            if command == 'help':
                return self.get_help()
            
            elif command == 'languages':
                return f"Supported languages: {', '.join(self.languages.keys())}"
            
            elif command == 'patterns':
                if not args:
                    return "Usage: patterns <language>"
                language = args[0]
                if language in self.code_patterns:
                    patterns = list(self.code_patterns[language].keys())
                    return f"Available patterns for {language}: {', '.join(patterns)}"
                else:
                    return f"Language '{language}' not supported"
            
            elif command == 'analyze':
                if len(args) < 2:
                    return "Usage: analyze <code> <language>"
                code, language = args[0], args[1]
                analysis = self.analyze_code(code, language)
                return json.dumps(analysis, indent=2)
            
            elif command == 'generate':
                if len(args) < 2:
                    return "Usage: generate <language> <pattern> [params]"
                language, pattern = args[0], args[1]
                params = {}
                for arg in args[2:]:
                    if '=' in arg:
                        key, value = arg.split('=', 1)
                        params[key] = value
                return self.generate_code(language, pattern, **params)
            
            elif command == 'suggest':
                if len(args) < 2:
                    return "Usage: suggest <code> <language>"
                code, language = args[0], args[1]
                suggestions = self.suggest_improvements(code, language)
                return "Suggestions:\n" + "\n".join(f"- {s}" for s in suggestions)
            
            elif command == 'explain':
                if len(args) < 2:
                    return "Usage: explain <code> <language>"
                code, language = args[0], args[1]
                return self.explain_code(code, language)
            
            else:
                return f"Unknown command: {command}. Type 'help' for available commands."
                
        except Exception as e:
            return f"Error processing command: {str(e)}"
    
    def chat(self, user_input: str) -> str:
        """Chat interface for the AI"""
        if not user_input.strip():
            return "Hello! I'm your coding assistant. How can I help you today?"
        
        # Check if it's a command
        if user_input.startswith('/'):
            command_parts = user_input[1:].split()
            if command_parts:
                command = command_parts[0]
                args = command_parts[1:]
                return self.process_command(command, *args)
        
        # Natural language processing (simple)
        user_input_lower = user_input.lower()
        
        if any(word in user_input_lower for word in ['hello', 'hi', 'hey']):
            return f"Hello! I'm {self.name}, your coding assistant. How can I help you with your code today?"
        
        elif any(word in user_input_lower for word in ['help', 'what can you do']):
            return self.get_help()
        
        elif any(word in user_input_lower for word in ['python', 'javascript', 'html', 'css']):
            return f"I can help you with {user_input_lower} code! Try asking me to generate, analyze, or explain code."
        
        elif 'generate' in user_input_lower:
            return "I can generate code templates! Try: /generate python class name=MyClass"
        
        elif 'analyze' in user_input_lower:
            return "I can analyze your code! Try: /analyze 'your code here' python"
        
        else:
            return "I'm here to help with coding! Try asking me to:\n- Generate code templates\n- Analyze your code\n- Suggest improvements\n- Explain code\n\nOr type /help for commands."

# Example usage and testing
if __name__ == "__main__":
    ai = CodingAI()
    
    print("=== Studio Code AI Assistant ===")
    print(ai.get_help())
    print("\n" + "="*40)
    
    # Test examples
    test_code = """
def hello(name):
    print(f"Hello, {name}!")
    
class Calculator:
    def add(self, a, b):
        return a + b
    """
    
    print("\n=== Code Analysis Example ===")
    analysis = ai.analyze_code(test_code, 'python')
    print(json.dumps(analysis, indent=2))
    
    print("\n=== Code Generation Example ===")
    generated = ai.generate_code('python', 'class', name='User', method='greet')
    print(generated)
    
    print("\n=== Chat Example ===")
    print(ai.chat("Hello! Can you help me with Python?"))
    print(ai.chat("/generate python function name=calculate params=x,y body=return x+y return_value=result"))
