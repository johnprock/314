import java.lang.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;



class Animate extends JComponent {

  private int xcoord;
  private int ycoord;
  private int size;
  private String name;
  private Sort s;
  private Algorithm algo;

  // constructor
  public Animate(int x, int y, String n, int sze, int type) {
    xcoord = x;
    ycoord = y;
    size = sze;
    name = n;
    algo = new Bubble(sze);
    switch(type) {
      case 0 : algo = new Bubble(size);    break;
      case 1 : algo = new Insertion(size); break;
      case 2 : algo = new Shell(size);     break;
      case 3 : algo = new Merge(size);     break;
      case 4 : algo = new Quick(size);     break;
      case 5 : algo = new Heap(size);      break;
    }
  }

  public void paintComponent(Graphics g) {
    super.paintComponent(g);

    g.setColor(Color.black);
    g.drawRect (xcoord, ycoord, 150, 150);
    g.drawString(name, xcoord, ycoord + 165);
    drawBars(g);
    algo.step();
  }

  public void step() {
    algo.step();
  }

  private void drawBars(Graphics g) {

    g.setColor(Color.blue);
    int width = 150/size;
    int height = 150/size;
    Vector<Integer> v = algo.get();
    for(int i=0; i<size; i++) {
      g.fill3DRect(xcoord+i*width, ycoord, width, v.get(i)*height, false);
    }
  }
}

class ProgFrame extends JFrame {

  public ProgFrame(int size) {

    setDefaultCloseOperation(TextFrame.EXIT_ON_CLOSE);
    setBounds(10,10,700,250);
    JPanel p = new JPanel();
    p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));


    final Animate bubble = new Animate(10, 20, "Bubble Sort", size, 0);
    final Animate insertion = new Animate(10, 20, "Insertion Sort", size, 1);
    bubble.setSize(200,200);
    insertion.setSize(200,200);
    p.add(bubble,BorderLayout.PAGE_END );
    p.validate();
    p.add(insertion, BorderLayout.CENTER);
    p.validate();

    add(p);

    int delay = 20; //milliseconds
    ActionListener taskPerformer = new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        bubble.repaint();
        insertion.repaint();
      }
    };

    new javax.swing.Timer(delay, taskPerformer).start();

    setVisible(true);

  }
}



//-------------SORTING ALGORITHMS--------------//




abstract class Algorithm {

    protected Vector<Integer> s;
    protected boolean done;

    // generates a shuffled sequence for sorting
    public Algorithm(int size) {
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
      while(step() != 0)
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
    
    protected void swap(int a, int b) {
      Integer temp = s.get(a);
      s.set(a,s.get(b));
      s.set(b,temp);
    }


  }

  class Bubble extends Algorithm {

    int i;
    int j;

    public Bubble(int s) {
      super(s);
      i = 0;
      j = 0;
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

  }
  
  class Insertion extends Algorithm {
    
    Integer val;
    int i;
    int hole;
    boolean before;
    boolean after;

    public Insertion(int siz) {
      super(siz);
      i = 1; // i represent the hole poition
      hole = 1;
      before = true;
      after = false;
    }

    public int step() {
   
      if(before) { // outer loop
        if(i == s.size()) {
          return 0;
        }
        val = s.get(i);
        hole = i;
        before = false;
        return 1;
      }

      if(after) { 
        s.set(hole,val);
        i++;
        after = false;
        before = true;
        return 1;
      }

      else {
        if(hole > 0 && val < s.get(hole - 1)) {
          s.set(hole, s.get(hole-1));
          hole--;
          return 1;
        }
        else {
          after = true;
          return 1;
        }
      }
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
    Algorithm a = new Bubble(10);
  }
}
