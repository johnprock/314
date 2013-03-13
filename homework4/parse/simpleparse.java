import java.util.*;
import java.lang.Error;

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
  public int parseExpr() {

    Token t0 = ts.getToken();
    int value = Integer.parseInt(t0.value);

    if(t0.type == "number") {
      Token t1 = ts.getToken();
      
      if(t1.value == "empty"){
        return value;
      }
      else {
        return value + parseExpr();
      }
    
    } 
    return -1;
  }

}

class TokenStream {

  // private data members
  private String raw;

  // constructor
  public TokenStream(String r) {
    raw = r;
  }

  // consumes the next token from raw
  public Token getToken() {
   
    if(raw.length() == 0) {
      return new Token("empty","");
    }
    if( Character.isDigit(raw.charAt(0) )) {
      return getNum();
    }
    if(raw.charAt(0) == '+') {
      return getPlus(); 
    }
    return new Token("error", "");
  }

  // a number consists of a series of digits 
  private Token getNum() {
    String num = "";
        for(int i=0; i<raw.length(); i++) {
      if( !Character.isDigit(raw.charAt(i)) ) { 
         break;
      }
      num = num + raw.charAt(i);
    }
    raw = raw.substring(num.length());
    return new Token(num, "number"); 
  }

  private Token getPlus() {
    Token t = new Token("+","operator");   
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
