import java.util.*;
import java.lang.Error;
import java.lang.Object;

class simpleparse {
  public static void main(String[] args) {

    String inp = args[0]; // get sting to parse
   // Parser p = new Parser(inp);

    //System.out.println( p.parseExpr() );
  
  }
}

/*
class Parser {
  
  // private data members
  private TokenStream ts;

  // constructor
  public Parser(String r) {
    ts = new TokenStream(r);
  }
  
  // parses the token stream and returns the expression value
  public String parseExpr() {

    Token t0 = ts.getToken();

    if(t0.type == "number") {
      return parseTerm();
    }

    String expr = parseExpr();
   
      Token t1 = ts.getToken();
      double val = Double.parseDouble(t0.value);
      
      if(t1.value == "empty") {
        return val;
      }
      if(t1.value == "+") {
        return val + parseExpr();
      }
      if(t1.value == "-") {
        return val - parseExpr();
      }
    
    return "error";
  }
      
  private String parseTerm() {

    Token t0 = ts.getToken();
    if(t0.type == "number") {
      Token t1 = ts.getToken();
      double val = Double.parseDouble(t0.value);

      if(t1.value == "empty") {
        return val;
      }
      if(t1.value == "*") {
        return val * parseTerm();
      }
      if(t1.value == "/") {
        return val / parseTerm();
      }
    }
    return "error";
  }
}*/

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

  public Token peek() {
    return tokens.get(pos);
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
