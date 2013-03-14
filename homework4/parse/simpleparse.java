import java.util.*;
import java.lang.Error;
import java.lang.Object;

class simpleparse {
  public static void main(String[] args) {

    String inp = args[0]; // get sting to parse
    String xRange     = args[1];
    String yRange     = args[2];
    String tRange     = args[3];

    Evaluator e = new Evaluator(inp); 
      
    System.out.println( e.evalyAt(0) );
  
  }
}


class Evaluator {

  // private data members
  private String raw;
  private String x;
  private String y;

  // constructor
  public Evaluator(String r) {
    raw = r;
    raw = raw.replace(" ","");  // remove spaces
    parseRaw();
  }

  public double evalxAt(double t) {
    Parser p = new Parser(bindVar(x,t));
    return Double.parseDouble(p.parseExpr());
  }

  public double evalyAt(double t) {
    Parser p = new Parser(bindVar(y,t));
    return Double.parseDouble(p.parseExpr());
  }

  private void parseRaw() {
    int split = raw.indexOf(',');
    x = raw.substring(1,split);
    y = raw.substring(split+1,raw.length()-1); 
  }

  private String bindVar(String s, double t) { // replace variable with number
    String val = String.valueOf(t);            // for evaluation   
    return s.replaceAll("t", val);
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
    String term;
    double result;
    result = Double.parseDouble(parseTerm());

    for(;;) {
      String peek = ts.peek(0).value;
      if(peek == "+") {
        ts.get();
        result += Double.parseDouble(parseTerm());
      }
      if(peek == "-") {
        ts.get();
        result -= Double.parseDouble(parseTerm());
      }
      if(peek!="+" && peek!="-") {  
        break;
      }
    }
    return String.valueOf(result);
  }


      
  private String parseTerm() { // operator precedence is reversed
    String primary;
    double result = 0;
    result = Double.parseDouble(parsePrimary());

    for(;;) {
      String peek = ts.peek(0).value;
      if(peek == "*") {
        ts.get();
        result *= Double.parseDouble(parsePrimary());
      }
      if(peek == "/") {
        ts.get();
        result /= Double.parseDouble(parsePrimary());
      }
      if(peek!="*" && peek!="/") {
        break;
      }
    }
    return String.valueOf(result);
  }
    

  private String parsePrimary() {
    String peek = ts.peek(0).type;
    if(peek == "number") {
      return ts.get().value;
    }
    if(peek == "paren") {
      ts.get();
      String e = parseExpr();
      ts.get();
      return e;
    }
    if(peek == "variable") {
      return "unbound variable";
    }
    return "error";
  }
}

class TokenStream {

  // private data members
  private String raw;
  private ArrayList<Token> tokens = new ArrayList<Token>();
  private int pos = 0;  // position in the token stream

  // constructor
  public TokenStream(String r) {
    raw = r;
    raw = raw.replace(" ","");  // remove spaces

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
    if(raw.substring(0,3) == "sin") {
      return getFunc("sin");
    }
    if(raw.substring(0,3) == "cos") {
      return getFunc("cos");
    }
    if(raw.substring(0,3) == "tan") {
      return getFunc("tan");
    }
    if(raw.substring(0,3) =="log") {
      return getFunc("log");
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
    if(raw.charAt(0) == '(') {
      return getParen("(");
    }
    if(raw.charAt(0) == ')') {
      return getParen(")");
    }
    if(raw.charAt(0) == '^') {
      return getOperator("^");
    }
    return new Token("error", "");
  }
  
  private Token getFunc(String type) {
    int start;
    int end;
    String args;

    start = raw.indexOf('(');
    end   = raw.indexOf(')');
    args  = raw.substring(start+1,end);

    return new Token(args,type);
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
    Token t = new Token(op, "operator");   
    raw = raw.substring(1); // consume token from string
    return t;
  }

  private Token getParen(String p) {
    Token t = new Token(p, "paren");
    raw = raw.substring(1);
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
