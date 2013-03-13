import java.util.*;
import java.lang.Error;
import java.lang.Object;

class simpleparse {
  public static void main(String[] args) {

    String inp = args[0]; // get sting to parse
    Parser p = new Parser(inp);

    System.out.println( p.parseExpr() );
  
  }
}


class Parser {
  
  // private data members
  private TokenStream ts;

  // constructor
  public Parser(String r) {
    ts = new TokenStream(r);
  }
  
  // parses the token stream and returns the expression value
  public String parseExpr() {
    String expr;
    double result;

    if(ts.peek(0).type == "number") {  // Term
      if(ts.peek(1).value != "+" && ts.peek(1).value != "-") {
        return parseTerm();
      }
    }

    expr = parseExpr();

    if(ts.peek(0).value == "+") {  // Expression + Term
      ts.get();
      result = Double.parseDouble(expr) + Double.parseDouble(parseTerm());
      return String.valueOf(result);
    }
    if(ts.peek(0).value == "-") {  // Expression - Term
      ts.get();
      result = Double.parseDouble(expr) - Double.parseDouble(parseTerm());
      return String.valueOf(result);
    }
    return "error";
  }
      
  private String parseTerm() {
    String term;
    double result;

    if(ts.peek(0).type == "number") {  // Primary
      return parsePrimary();
    }

    term = parseTerm();

    if(ts.peek(0).value == "*") {  // Term * Primary
      ts.get();
      result = Double.parseDouble(term) * Double.parseDouble(parsePrimary());
      return String.valueOf(result);
    }
    if(ts.peek(0).value == "/") {  // Term / Primary
      ts.get();
      result = Double.parseDouble(term) / Double.parseDouble(parsePrimary());
      return String.valueOf(result);
    }
    return "error";
  }


  private String parsePrimary() {
    if(ts.peek(0).type == "number")  {
      return ts.get().value;
    }
    return "error";
  }
}

class TokenStream {

  // private data members
  private String raw;
  private ArrayList<Token> tokens = new ArrayList<Token>();
  private int pos;  // position in the token stream

  // constructor
  public TokenStream(String r) {
    raw = r;
    raw = raw.replace(" ","");  // remove spaces
    pos = 0;

    for(;;) {   // build a list of tokens
      tokens.add(getToken());
      if( tokens.get(tokens.size()-1).value == "empty") {
        break;
      }
    }
  }

  public Token get() {
    int temp = pos;
    pos++;
    return tokens.get(temp);
  }

  public Token peek(int i) {
    return tokens.get(pos+i);
  }


  // consumes the next token from raw
  private Token getToken() {
   
    if(raw.length() == 0) {
      return new Token("empty","");
    }
    if( Character.isDigit(raw.charAt(0) )) {
      return getNum();
    }
    if(raw.charAt(0) == '+') {
      return getOperator("+"); 
    }
    if(raw.charAt(0) == '-') {
      return getOperator("-");
    }
    if(raw.charAt(0) == '*') {
      return getOperator("*");
    }
    if(raw.charAt(0) == '/') {
      return getOperator("/");
    }

    return new Token("error", "");
  }

  // a number consists of a series of digits 
  private Token getNum() {
    String num = "";
      for(int i=0; i<raw.length(); i++) {
        if( !Character.isDigit(raw.charAt(i)) && raw.charAt(i) != '.' ) { 
           break;
      }
      num = num + raw.charAt(i);
    }
    raw = raw.substring(num.length());
    return new Token(num, "number"); 
  }

  private Token getOperator(String op) {
    Token t = new Token(op,"operator");   
    raw = raw.substring(1); // consume token from string
    return t;
  }

}


class Token { // basic datatype for building a parse tree

  public String value;
  public String type;

  public Token(String v, String t){
    value = v;
    type = t;
  
  }
}
