import java.lang.*;
import java.util.*;
import java.awt.*;
import javax.swing.*;



class Animate extends JComponent {

  public void paint(Graphics g) {
    g.drawRect (50, 50, 100, 100);
    g.drawRect (150, 50, 100, 100);
    g.drawRect (250, 50, 100, 100);
    g.drawRect (350, 50, 100, 100);
    g.drawRect (450, 50, 100, 100);
    g.drawRect (550, 50, 100, 100);
  }
}

class TextFrame extends JFrame {

  public TextFrame() {
    setDefaultCloseOperation(TextFrame.EXIT_ON_CLOSE);
    setBounds(10,10,700,250);
    getContentPane().add(new Animate());
    setVisible(true);
  }
}


class Sort {

  private Algorithm bubble;
  private Algorithm insertion;
  private Algorithm shell;
  private Algorithm merge;
  private Algorithm quick;
  private Algorithm heap;


  public Sort(int size) {
    bubble = new Bubble(size);
  }

  public void run_test() {
    bubble.test();
  }


//-------------SORTING ALGORITHMS--------------//

 private abstract class Algorithm {

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

  private class Bubble extends Algorithm {

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
  
  private class Insertion {
  }

  private class Shell {
  }

  private class Merge {
  }

  private class Quick {
  }

  private class Heap {
  }


  //-------------MAIN--------------------//

  public static void main(String[] args) {
    Sort sort = new Sort(10);
    sort.run_test();

    TextFrame win = new TextFrame();
 }
}
