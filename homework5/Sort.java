import java.lang.*;
import java.util.*;
import java.awt.*;
import javax.swing.*;



class Animate extends JComponent {

  private int xcoord;
  private int ycoord;
  private int size;
  private String name;
  private Sort s;
  private Algorithm a;

  // constructor
  public Animate(int x, int y, String n, int sze, int type) {
    xcoord = x;
    ycoord = y;
    size = sze;
    name = n;
    switch(type) {
      case 0: a = new Bubble(size);
      case 1: a = new Insertion(size);
      case 2: a = new Shell(size);
      case 3: a = new Merge(size);
      case 4: a = new Quick(size);
      case 5: a = new Heap(size);
    }
  }

  public void paintComponent(Graphics g) {
    g.setColor(Color.black);
    g.drawRect (xcoord, ycoord, 150, 150);
    g.drawString(name, xcoord, ycoord + 165);
    drawBars(g);
  }

  private void drawBars(Graphics g) {

    g.setColor(Color.blue);
    int width = 150/size;
    int height = 150/size;
    Vector<Integer> v = a.get();

    for(int i=0; i<size; i++) {
      g.fill3DRect(xcoord+i*width, ycoord, width, v.get(i)*height, false);
    }
  }
}

class ProgFrame extends JFrame {

  public ProgFrame(int size) {

    setDefaultCloseOperation(TextFrame.EXIT_ON_CLOSE);
    setBounds(10,10,700,250);
    
    Animate a = new Animate(10, 20, "Bubble Sort", size, 0);

    add(a);
    setVisible(true);

  }
}



//-------------SORTING ALGORITHMS--------------//

 abstract class Algorithm {

    protected Vector<Integer> s;
    protected int i;
    protected int j;
    protected boolean done;

    // generates a shuffled sequence for sorting
    public Algorithm(int size) {
      i=0;
      j=0;
      done = false;
      s = new Vector<Integer>();
      for(int i=0; i<size; i++) { 
        s.add(i); 
      }
      Collections.shuffle(s);
    }

    //prints the sequence
    public void print() {
      System.out.println("--------------");
      for(int i=0; i<s.size(); i++) {
        System.out.println(s.get(i));
      }
     System.out.println("--------------");
    }
    public void test() {
      print();
      while(step() != 0);
      print();
    }


    // gets the sequence
    public Vector<Integer> get() {
      return s;
    }
    
    // performs one iteration of the sorting algorithm 
    // returns 0 if the sort if finished
    // returns 1 otherwise
    abstract public int step(); 
    

  }

  class Bubble extends Algorithm {

    public Bubble(int s) {
      super(s);
    }

    public int step() {
      if(i == s.size()-1) { // bubble floated to the top
        i=0;
        j++;
      }
      if(j == s.size()) { // float size times
        return 0;
      }
      if(s.get(i) > s.get(i+1)) {
        swap(i,i+1); 
      }
      i++;
      return 1;
    }

    private void swap(int a, int b) {
      int temp = s.get(a);
      s.set(a,s.get(b));
      s.set(b,temp);
    }

  }
  
  class Insertion extends Algorithm {
    
    public Insertion(int s) {
      super(s);
    }

    public int step() {
      return 0;
    }
  }

  class Shell extends Algorithm {

    public Shell(int s) {
      super(s);
    }
 
    public int step() {
      return 0;
    }
  }

  class Merge extends Algorithm {

    public Merge(int s) {
      super(s);
    }
 
    public int step() {
      return 0;
    }
  }

  class Quick extends Algorithm {

    public Quick(int s) {
      super(s);
    }
 
    public int step() {
      return 0;
    }
  }

  class Heap extends Algorithm {

    public Heap(int s) {
      super(s);
    }
 
    public int step() {
      return 0;
    }
  }
  


//------------MAIN-----------//

class Sort {

  public static void main(String[] args) {
    ProgFrame win = new ProgFrame( Integer.parseInt(args[0]) );
 }
}
