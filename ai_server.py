#!/usr/bin/env python3
"""
Studio Code AI - Flask Server
HTTP API server for the Python-based Coding AI
"""

from flask import Flask, request, jsonify, render_template_string
from flask_cors import CORS
import json
import re
import random
from datetime import datetime
from coding_ai import CodingAI

app = Flask(__name__)
CORS(app)  # Enable CORS for all routes

# Initialize the AI
ai = CodingAI()

@app.route('/')
def home():
    """Home page with API documentation"""
    return render_template_string('''
    <!DOCTYPE html>
    <html>
    <head>
        <title>Studio Code AI - API Server</title>
        <style>
            body { font-family: Arial, sans-serif; margin: 40px; }
            .endpoint { background: #f5f5f5; padding: 15px; margin: 10px 0; border-radius: 5px; }
            .method { background: #007bff; color: white; padding: 5px 10px; border-radius: 3px; }
            .url { background: #28a745; color: white; padding: 5px 10px; border-radius: 3px; }
            code { background: #e9ecef; padding: 2px 5px; border-radius: 3px; }
        </style>
    </head>
    <body>
        <h1>🤖 Studio Code AI - API Server</h1>
        <p>Your Python-based coding assistant is running!</p>
        
        <h2>API Endpoints</h2>
        
        <div class="endpoint">
            <span class="method">POST</span> <span class="url">/api/chat</span>
            <p>Chat with the AI assistant</p>
            <code>{"message": "Hello, can you help me with Python?"}</code>
        </div>
        
        <div class="endpoint">
            <span class="method">POST</span> <span class="url">/api/analyze</span>
            <p>Analyze code quality</p>
            <code>{"code": "def hello(): print('world')", "language": "python"}</code>
        </div>
        
        <div class="endpoint">
            <span class="method">POST</span> <span class="url">/api/generate</span>
            <p>Generate code templates</p>
            <code>{"language": "python", "pattern": "class", "params": {"name": "Calculator"}}</code>
        </div>
        
        <div class="endpoint">
            <span class="method">GET</span> <span class="url">/api/languages</span>
            <p>Get supported languages</p>
        </div>
        
        <div class="endpoint">
            <span class="method">GET</span> <span class="url">/api/patterns</span>
            <p>Get available code patterns</p>
            <code>?language=python</code>
        </div>
        
        <h2>Test the AI</h2>
        <p>Try the web interface: <a href="/ai">AI Chat Interface</a></p>
        
        <h2>Status</h2>
        <p>✅ Server is running</p>
        <p>✅ AI is initialized</p>
        <p>✅ API endpoints are active</p>
    </body>
    </html>
    ''')

@app.route('/ai')
def ai_interface():
    """AI Chat Interface"""
    return render_template_string('''
    <!DOCTYPE html>
    <html>
    <head>
        <title>Studio Code AI - Chat Interface</title>
        <style>
            body { font-family: Arial, sans-serif; margin: 0; padding: 20px; background: #f0f2f5; }
            .chat-container { max-width: 800px; margin: 0 auto; background: white; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
            .chat-header { background: #667eea; color: white; padding: 20px; border-radius: 10px 10px 0 0; text-align: center; }
            .chat-messages { height: 400px; overflow-y: auto; padding: 20px; }
            .message { margin: 10px 0; padding: 10px; border-radius: 5px; }
            .user-message { background: #007bff; color: white; margin-left: 20%; }
            .ai-message { background: #e9ecef; color: #333; margin-right: 20%; }
            .input-area { padding: 20px; border-top: 1px solid #eee; }
            .input-area input { width: 80%; padding: 10px; border: 1px solid #ddd; border-radius: 5px; }
            .input-area button { width: 18%; padding: 10px; background: #667eea; color: white; border: none; border-radius: 5px; cursor: pointer; }
        </style>
    </head>
    <body>
        <div class="chat-container">
            <div class="chat-header">
                <h1>🤖 Studio Code AI</h1>
                <p>Chat with your coding assistant</p>
            </div>
            <div class="chat-messages" id="messages">
                <div class="message ai-message">
                    <strong>AI:</strong> Hello! I'm your coding assistant. How can I help you today?
                </div>
            </div>
            <div class="input-area">
                <input type="text" id="userInput" placeholder="Type your message..." />
                <button onclick="sendMessage()">Send</button>
            </div>
        </div>
        
        <script>
            function sendMessage() {
                const input = document.getElementById('userInput');
                const message = input.value.trim();
                if (!message) return;
                
                // Add user message
                addMessage(message, 'user');
                input.value = '';
                
                // Send to API
                fetch('/api/chat', {
                    method: 'POST',
                    headers: {'Content-Type': 'application/json'},
                    body: JSON.stringify({message: message})
                })
                .then(response => response.json())
                .then(data => {
                    addMessage(data.response, 'ai');
                })
                .catch(error => {
                    addMessage('Sorry, I encountered an error. Please try again.', 'ai');
                });
            }
            
            function addMessage(text, sender) {
                const messages = document.getElementById('messages');
                const div = document.createElement('div');
                div.className = `message ${sender}-message`;
                div.innerHTML = `<strong>${sender === 'user' ? 'You' : 'AI'}:</strong> ${text}`;
                messages.appendChild(div);
                messages.scrollTop = messages.scrollHeight;
            }
            
            // Enter key support
            document.getElementById('userInput').addEventListener('keypress', function(e) {
                if (e.key === 'Enter') sendMessage();
            });
        </script>
    </body>
    </html>
    ''')

@app.route('/api/chat', methods=['POST'])
def chat():
    """Chat endpoint"""
    try:
        data = request.get_json()
        message = data.get('message', '')
        
        if not message:
            return jsonify({'error': 'Message is required'}), 400
        
        # Get AI response
        response = ai.chat(message)
        
        return jsonify({
            'status': 'success',
            'message': message,
            'response': response,
            'timestamp': datetime.now().isoformat()
        })
    
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/analyze', methods=['POST'])
def analyze_code():
    """Code analysis endpoint"""
    try:
        data = request.get_json()
        code = data.get('code', '')
        language = data.get('language', '')
        
        if not code or not language:
            return jsonify({'error': 'Code and language are required'}), 400
        
        # Analyze code
        analysis = ai.analyze_code(code, language)
        
        return jsonify({
            'status': 'success',
            'analysis': analysis,
            'timestamp': datetime.now().isoformat()
        })
    
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/generate', methods=['POST'])
def generate_code():
    """Code generation endpoint"""
    try:
        data = request.get_json()
        language = data.get('language', '')
        pattern = data.get('pattern', '')
        params = data.get('params', {})
        
        if not language or not pattern:
            return jsonify({'error': 'Language and pattern are required'}), 400
        
        # Generate code
        generated_code = ai.generate_code(language, pattern, **params)
        
        return jsonify({
            'status': 'success',
            'language': language,
            'pattern': pattern,
            'params': params,
            'code': generated_code,
            'timestamp': datetime.now().isoformat()
        })
    
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/suggest', methods=['POST'])
def suggest_improvements():
    """Code suggestions endpoint"""
    try:
        data = request.get_json()
        code = data.get('code', '')
        language = data.get('language', '')
        
        if not code or not language:
            return jsonify({'error': 'Code and language are required'}), 400
        
        # Get suggestions
        suggestions = ai.suggest_improvements(code, language)
        
        return jsonify({
            'status': 'success',
            'suggestions': suggestions,
            'timestamp': datetime.now().isoformat()
        })
    
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/explain', methods=['POST'])
def explain_code():
    """Code explanation endpoint"""
    try:
        data = request.get_json()
        code = data.get('code', '')
        language = data.get('language', '')
        
        if not code or not language:
            return jsonify({'error': 'Code and language are required'}), 400
        
        # Explain code
        explanation = ai.explain_code(code, language)
        
        return jsonify({
            'status': 'success',
            'explanation': explanation,
            'timestamp': datetime.now().isoformat()
        })
    
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/languages', methods=['GET'])
def get_languages():
    """Get supported languages"""
    try:
        return jsonify({
            'status': 'success',
            'languages': ai.languages,
            'count': len(ai.languages),
            'timestamp': datetime.now().isoformat()
        })
    
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/patterns', methods=['GET'])
def get_patterns():
    """Get available patterns for a language"""
    try:
        language = request.args.get('language', '')
        
        if not language:
            return jsonify({'error': 'Language parameter is required'}), 400
        
        if language not in ai.code_patterns:
            return jsonify({'error': f'Language {language} not supported'}), 400
        
        patterns = list(ai.code_patterns[language].keys())
        
        return jsonify({
            'status': 'success',
            'language': language,
            'patterns': patterns,
            'count': len(patterns),
            'timestamp': datetime.now().isoformat()
        })
    
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/help', methods=['GET'])
def get_help():
    """Get AI help information"""
    try:
        help_text = ai.get_help()
        
        return jsonify({
            'status': 'success',
            'help': help_text,
            'timestamp': datetime.now().isoformat()
        })
    
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/status', methods=['GET'])
def get_status():
    """Get server and AI status"""
    try:
        return jsonify({
            'status': 'success',
            'server': 'running',
            'ai_name': ai.name,
            'ai_version': ai.version,
            'supported_languages': len(ai.languages),
            'timestamp': datetime.now().isoformat()
        })
    
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.errorhandler(404)
def not_found(error):
    return jsonify({'error': 'Endpoint not found'}), 404

@app.errorhandler(500)
def internal_error(error):
    return jsonify({'error': 'Internal server error'}), 500

if __name__ == '__main__':
    print("🤖 Starting Studio Code AI Server...")
    print(f"✅ AI initialized: {ai.name} v{ai.version}")
    print(f"✅ Supported languages: {len(ai.languages)}")
    print("🌐 Server starting on http://localhost:5000")
    print("📱 AI Interface: http://localhost:5000/ai")
    print("📚 API Docs: http://localhost:5000")
    print("🚀 Press Ctrl+C to stop the server")
    
    try:
        app.run(host='0.0.0.0', port=5000, debug=True)
    except KeyboardInterrupt:
        print("\n🛑 Server stopped by user")
    except Exception as e:
        print(f"❌ Server error: {e}")
