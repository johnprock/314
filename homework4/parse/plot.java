import java.util.*;
import java.lang.Error;
import java.lang.Object;
import java.lang.Math;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.awt.geom.*;

class Plot {

  // private data members
  String inp;
  String xrng;
  String yrng;
  String trng;


  // constructor
  Plot(String i, String x, String y, String t) {
    // these hold the command line arguments for processing
    // by our objects
    inp  = i;
    xrng = x;
    yrng = y;
    trng = t;
  }

  public void testEval(){
    Evaluator e = new Evaluator(inp);
    System.out.println(e.evalxAt(0));
  }


//---------------------INNER CLASSES----------------------//
    
protected class Grapher extends JPanel{ // Kalil's Class!
	
	private double[] xData; // the x points
	private double[] yData; // The y points
	private double[] tData = {1,2,3,4,5,6,7,8,9,10}; // To be determined by the t ranges
	private double[] yAxis; // To be determined by the y ranges
	private double[] xAxis; // To be determined by the x ranges
	private double xMin, xMax, yMin, yMax; // We're going to parse these out to get domain/range.
	private int padding = 30; // gives us some offset when we use paint component
	
	  // private data members
	private Evaluator eval;
	//		eval = new Evaluator(inp);
	// the evaluator will let you get the value
	// of the input function at any value of t
	// by calling the public methods evalxAt()
	// and evalyAt
	
	public double parseRange(){
	return 2.0;
	}
	
	// This should evaluate x at every value of t; push into an x array.
	public double[] fillxData(double[] tData){
		for (int i = 0; i < tData.length; i++){
		xData[i] = eval.evalxAt(tData[i]);
		}
		return xData;
	}
	
	// This should evaluate y at every value of t; push into a y array.
	public double[] fillyData(double[] tData){
		for(int i = 0; i < tData.length; i++){
		yData[i] = eval.evalyAt(tData[i]);
		}
		return yData;
	}
	
	// Pass the arrays (now parsed from the input) to the panel for plotting
	public void paintComponent(Graphics graph){
		super.paintComponent(graph); // Learned how to over-ride via Java website
		Graphics2D TDGraph = (Graphics2D) graph;
		TDGraph.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
             RenderingHints.VALUE_ANTIALIAS_ON);
			 
		int hFrame = getHeight();
		int wFrame = getWidth();
		
		// Draw x axis
		TDGraph.draw(new Line2D.Double(padding, padding, padding, hFrame-padding));
		// Draw y axis
		 TDGraph.draw(new Line2D.Double(padding, hFrame-padding, wFrame-padding, hFrame-padding));
	}

}

	// Create window
private void TwoDMagic() {
	JFrame frame = new JFrame("Parametric Plotter");
	frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	//JLabel aLabel = new JLabel("This is a test.");
	//frame.getContentPane().add(aLabel, BorderLayout.CENTER);
	
	// Display
	//frame.setSize(300,300);
	frame.setPreferredSize(new Dimension(900, 900));
	frame.setLocation(0,0);
	frame.pack();
	frame.setVisible(true);
	frame.add(new Grapher());
}

  // methods that plots stuff here
	
private class Evaluator {

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

  public double evalxAt(double t) { // returns the x value at t
    Parser p = new Parser(bindVar(x,t));
    return Double.parseDouble(p.parseExpr());
  }

  public double evalyAt(double t) { // returns the y value at t
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

private class Parser {
  
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
    double result;
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
    double result;

    String peek1 = ts.peek(0).type;
    String peek2 = ts.peek(1).value;

   if(ts.peek(0).value.equals("-")) { // negative number
     ts.get(); // clear -
     result = Double.parseDouble(parseExpr());
     result = (-1)*result;
     return String.valueOf(result);
   }

   if(peek1 == "number" && peek2 == "^") { // exponentiation
      double base = Double.parseDouble(ts.get().value);
      ts.get();
      double exp  = Double.parseDouble(ts.get().value);
      result = Math.pow(base, exp);
      return String.valueOf(result);
    }
    if(peek1 == "number") { // literal
      return ts.get().value;
    }
    if(peek2 == "paren") { // parenthetical expression
      ts.get();
      String e = parseExpr();
      ts.get();
      return e;
    }
    if(peek1 == "function") { // function call
      return parseFunction();
    }

    System.out.println("Primary parse error");
    System.exit(1);
    return "";
  }

  private String parseFunction() {
    double result;

    String peek = ts.peek(0).value;
    ts.get(); // clear function token
    ts.get(); // clear (
    result = Double.parseDouble(parseExpr());
    ts.get(); // clear )

    if(peek == "sin") {
      return String.valueOf(Math.sin(result));
    }
    if(peek == "cos") {
      return String.valueOf(Math.cos(result));
    }
    if(peek == "tan") {
      return String.valueOf(Math.tan(result));
    }
    if(peek == "log") {
      return String.valueOf(Math.log(result));
    }
    if(peek == "abs") {
      return String.valueOf(Math.abs(result));
    }
    if(peek == "exp") {
      return String.valueOf(Math.exp(result));
    }
    if(peek == "sinh") {
      return String.valueOf(Math.sinh(result));
    }
    if(peek == "cosh") {
      return String.valueOf(Math.cosh(result));
    }
    if(peek == "tanh") {
      return String.valueOf(Math.tanh(result));
    }
    System.out.println("Function parse error");
    System.exit(1);
    return "";

  }
}

private class TokenStream {

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
    if(raw.substring(0,3).equals("sin")) {
      return getFunc("sin");
    }
    if(raw.substring(0,4).equals("cosh")) {
      return getFunc("cosh");
    }
    if(raw.substring(0,4).equals("sinh")) {
      return getFunc("sinh");
    }
    if(raw.substring(0,4).equals("tanh")) {
      return getFunc("tanh");
    }
    if(raw.substring(0,3).equals("cos")) {
      return getFunc("cos");
    }
    if(raw.substring(0,3).equals("tan")) {
      return getFunc("tan");
    }
    if(raw.substring(0,3).equals("log")) {
      return getFunc("log");
    }
    if(raw.substring(0,3).equals("abs")) {
      return getFunc("abs");
    }
    if(raw.substring(0,3).equals("exp")) {
      return getFunc("exp");
    }
   
    System.out.println("Tokenization error");
    System.exit(1);
    return new Token("","");
  }
  
  private Token getFunc(String f) {
    raw = raw.substring(f.length());
    return new Token(f,"function");
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


private class Token { // basic datatype for building a parse tree

  public String value;
  public String type;

  public Token(String v, String t){
    value = v;
    type = t;
  
  }
}

  public static void main(String[] args) {

    String inp        = args[0]; // get sting to parse
    String x          = args[1];
    String y          = args[2];
    String t          = args[3];

	// dummy variables
	double yData[] = new double[100];
	double xData[] = new double[100];
    Plot p = new Plot(inp, x, y, t); 
    p.testEval();     
	//Plot.Grapher g = p.new Grapher();
	//g.TwoDMagic();
	p.TwoDMagic();
  }
}
