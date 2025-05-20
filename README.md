# Common Lisp library to access Google Gemini LLM APIs

This library provides a Common Lisp interface to Google's Gemini Large Language Models. It supports various models like Gemini 1.5 Pro and Gemini 1.5 Flash, and offers functionalities including text generation, token counting, chat conversations, and streaming responses.

This project was originally forked from a library for Perplexity AI in the book [Loving Common Lisp](https://leanpub.com/lovinglisp) by Mark Watson.

## Setting your GOOGLE_API_KEY API key

Define the `GOOGLE_API_KEY` environment variable with the value of your Google API key. You can obtain an API key from the [Google AI Studio](https://aistudio.google.com/app/apikey).

## Dependencies

This library depends on:
- `uiop`
- `cl-json`
- `dexador`
- `alexandria`
- `fiveam` (for tests)

Ensure these are available in your Quicklisp local-projects or via ASDF.

## Usage

Load the library using Quicklisp or ASDF:
```common-lisp
(ql:quickload :gemini)
;; or
(asdf:load-system :gemini)
```

### Basic Generation

To generate text from a prompt:
```common-lisp
(gemini:generate "gemini-1.5-flash-latest" "In one sentence, explain how AI works to a child.")
;; => "AI is like a super smart computer brain that learns from information to answer questions and do tasks."
```
You can use other models like `"gemini-1.5-pro-latest"` as the first argument.

### Counting Tokens

To count the number of tokens a prompt will consume for a specific model:
```common-lisp
(gemini:count-tokens "gemini-1.5-flash-latest" "How many tokens is this sentence?")
;; => 8 (example output, actual may vary)
```

### Chat Conversations

To have a conversation with the model, provide a list of messages. Each message is a hash-table with a "role" ("user" or "model") and "text".
```common-lisp
(let ((chat-history (list
                      (alexandria:plist-hash-table '("role" "user" "text" "What is the capital of France?"))
                      (alexandria:plist-hash-table '("role" "model" "text" "The capital of France is Paris.")))))
  ;; Add a new user message to the history
  (push (alexandria:plist-hash-table '("role" "user" "text" "What is a famous landmark there?")) chat-history)
  ;; Get the model's response (note: chat-history is now newest-first, the API expects oldest-first,
  ;; but the current send-chat-message implementation in this library does not reverse it.
  ;; For correct multi-turn, the application should manage message order before passing to send-chat-message,
  ;; or the library function should be updated to handle it.)
  (gemini:send-chat-message "gemini-1.5-flash-latest" (reverse chat-history)))
;; => "A famous landmark in Paris is the Eiffel Tower." (example output)
```
*(Note: The example above uses `plist-hash-table` from `alexandria` for convenience in creating hash tables. Ensure your messages are correctly ordered for multi-turn conversations as per API requirements.)*

### Streaming Generation

For long generations, or to display text as it arrives, you can stream the response:
```common-lisp
(gemini:generate-streaming
 "gemini-1.5-flash-latest"
 "Tell me a short story about a brave Lisp macro."
 (lambda (text-chunk)
   (format t "~A" text-chunk)
   (force-output)))
;; This will print the story chunk by chunk as it's generated.
;; The function itself returns T upon completion.
```

## Available Functions

- `(gemini:generate model-id prompt)`: Generates text from a prompt using the specified model.
- `(gemini:count-tokens model-id prompt)`: Counts the tokens for a given prompt and model.
- `(gemini:send-chat-message model-id messages)`: Sends a chat history and gets a response from the model. `messages` should be a list of hash-tables, each specifying "role" and "text".
- `(gemini:generate-streaming model-id prompt callback-function)`: Generates text, streaming chunks to the `callback-function`.

## Running Tests

The library uses the FiveAM testing framework. To run the tests:
```common-lisp
(asdf:test-system :gemini)
```
This will execute the test suite defined in `tests/test-gemini.lisp`, which includes mocked API calls.

## Original Project Information

The original project from which this was adapted can be found in the repository https://github.com/mark-watson/loving-common-lisp. The `Makefile` mentioned in the original README for fetching library examples is specific to that book's comprehensive collection of projects. This current library is a focused adaptation for Gemini.
