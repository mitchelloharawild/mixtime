#include <cpp11.hpp>
#include <string>
#include <vector>
using namespace cpp11;

[[cpp11::register]]
list mt_glue_fmt(std::string input, environment env) {
  enum State { TEXT, EXPR, SINGLE_Q, DOUBLE_Q, BACKTICK, ESCAPE };

  struct ExprResult {
    cpp11::sexp value;
  };

  std::vector<ExprResult> expr_results;
  std::vector<size_t> expr_positions;

  State state = TEXT;
  State prev_state = TEXT;
  int brace_level = 0;
  size_t n = input.length();

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
            function parse = package("base")["parse"];
            function eval_fn = package("base")["eval"];

            SEXP expr = parse(named_arg("text") = expr_text);
            SEXP value = eval_fn(expr, named_arg("envir") = env);

            expr_results.push_back({value});
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

  // Second pass: build a single ordered list of segments and results
  // in order of appearance in the format string.
  writable::list out;

  size_t expr_idx = 0;
  std::string current_str;
  current_str.reserve(input.length());

  size_t next_expr_pos = expr_positions.empty() ? std::string::npos : expr_positions[0];

  auto flush_text = [&]() {
    writable::strings seg(1);
    seg[0] = current_str;
    out.push_back(seg);
    current_str.clear();
  };

  for (size_t i = 0; i < n; ++i) {
    if (i == next_expr_pos) {
      // Flush accumulated plain text before this expression
      flush_text();

      // Skip over the braced expression in the input
      brace_level = 0;
      do {
        char ch = input[i++];
        if (ch == '{') ++brace_level;
        else if (ch == '}') --brace_level;
      } while (brace_level > 0 && i < n);
      --i;

      out.push_back(expr_results[expr_idx].value);

      ++expr_idx;
      next_expr_pos = (expr_idx < expr_positions.size())
        ? expr_positions[expr_idx]
        : std::string::npos;

    } else if (input[i] == '{' && i + 1 < n && input[i + 1] == '{') {
      current_str += '{';
      ++i;
    } else if (input[i] == '}' && i + 1 < n && input[i + 1] == '}') {
      current_str += '}';
      ++i;
    } else if (input[i] != '{' && input[i] != '}') {
      current_str += input[i];
    }
  }

  // Append the final trailing text segment
  flush_text();

  return out;
}