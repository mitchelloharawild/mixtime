#include <cpp11.hpp>
#include <string>
#include <vector>
using namespace cpp11;

[[cpp11::register]]
strings mt_glue_fmt(std::string input, environment env, list units, list parts) {
  enum State { TEXT, EXPR, SINGLE_Q, DOUBLE_Q, BACKTICK, ESCAPE };
  
  std::vector<strings> expr_results;
  std::vector<size_t> expr_positions;
  
  State state = TEXT;
  State prev_state = TEXT;
  int brace_level = 0;
  size_t n = input.length();
  int result_length = 1;
  
  // First pass: find and evaluate expressions
  std::string expr_text;
  for (size_t i = 0; i < n; ++i) {
    char ch = input[i];
    
    switch (state) {
      case TEXT:
        if (ch == '{' && i + 1 < n && input[i + 1] != '{') {
          state = EXPR;
          brace_level = 1;
          expr_text.clear();
          expr_positions.push_back(i);
        } else if (ch == '{' || ch == '}') {
          ++i; // Skip escaped braces
        }
        break;
        
      case EXPR:
        if (ch == '{') {
          ++brace_level;
          expr_text += ch;
        } else if (ch == '}') {
          --brace_level;
          if (brace_level == 0) {
            // Evaluate expression
            function parse = package("base")["parse"];
            function eval_fn = package("base")["eval"];
            function display = package("mixtime")["mt_unit_display"];
            
            SEXP expr = parse(named_arg("text") = expr_text);
            SEXP value = eval_fn(expr, named_arg("envir") = env);
            SEXP result_sexp = display(value, named_arg("units") = units, named_arg("parts") = parts);
            strings result_vec(result_sexp);
            
            int len = result_vec.size();
            if (len > 1) {
              if (result_length == 1) {
                result_length = len;
              } else if (len != result_length) {
                stop("Incompatible lengths: %d and %d", result_length, len);
              }
            }
            
            expr_results.push_back(result_vec);
            state = TEXT;
          } else {
            expr_text += ch;
          }
        } else if (ch == '\'') {
          state = SINGLE_Q;
          expr_text += ch;
        } else if (ch == '"') {
          state = DOUBLE_Q;
          expr_text += ch;
        } else if (ch == '`') {
          state = BACKTICK;
          expr_text += ch;
        } else {
          expr_text += ch;
        }
        break;
        
      case ESCAPE:
        expr_text += ch;
        state = prev_state;
        break;
        
      case SINGLE_Q:
      case DOUBLE_Q:
      case BACKTICK:
        expr_text += ch;
        if (ch == '\\') {
          prev_state = state;
          state = ESCAPE;
        } else if ((state == SINGLE_Q && ch == '\'') ||
                   (state == DOUBLE_Q && ch == '"') ||
                   (state == BACKTICK && ch == '`')) {
          state = EXPR;
        }
        break;
    }
  }
  
  if (state != TEXT) {
    stop("Unterminated expression");
  }
  
  // Second pass: build result strings
  writable::strings result(result_length);
  size_t expr_idx = 0;
  
  for (int row = 0; row < result_length; ++row) {
    std::string str;
    str.reserve(input.length() * 2); // Pre-allocate
    
    expr_idx = 0;
    size_t next_expr_pos = expr_positions.empty() ? std::string::npos : expr_positions[0];
    
    for (size_t i = 0; i < n; ++i) {
      if (i == next_expr_pos) {
        // Skip to end of expression
        brace_level = 0;
        do {
          char ch = input[i++];
          if (ch == '{') ++brace_level;
          else if (ch == '}') --brace_level;
        } while (brace_level > 0 && i < n);
        --i;
        
        // Append expression result
        const strings& vals = expr_results[expr_idx];
        int idx = (vals.size() == 1) ? 0 : row;
        str += std::string(vals[idx]);
        
        ++expr_idx;
        next_expr_pos = (expr_idx < expr_positions.size()) ? expr_positions[expr_idx] : std::string::npos;
      } else if (input[i] == '{' && i + 1 < n && input[i + 1] == '{') {
        str += '{';
        ++i;
      } else if (input[i] == '}' && i + 1 < n && input[i + 1] == '}') {
        str += '}';
        ++i;
      } else if (input[i] != '{' && input[i] != '}') {
        str += input[i];
      }
    }
    
    result[row] = str;
  }
  
  return result;
}