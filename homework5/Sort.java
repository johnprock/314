import java.lang.*;
import java.util.*;
import java.awt.*;
import javax.swing.*;


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

  public void draw() {
    JFrame frame = new JFrame("Animated Sorting");
    frame.setDefaultCloseOperation(frame.EXIT_ON_CLOSE);
    frame.setPreferredSize(new Dimension(1000,1000));
    frame.setLocation(0,0);
    frame.pack();
    frame.setVisible(true);
    frame.add(new Plot());

  }



  //----------------INNER CLASSES---------------//
  

 private class Plot extends JPanel {

   public void paintComponent(Graphics g) {
     super.paintComponent(g);
     this.setBackground(Color.WHITE);
   }
 }

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
      for(int i=0; i<s.size(); i++) {
        System.out.println(s.get(i));
      }
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
      if(i == s.size()) { // bubble floated to the top
        i=0;
        j++;
      }
      if(j == s.size()) { // float size times
        return 0;
      }
      if(s.get(i) > s.get(i+1)) {
        swap(i,i+1);
      }
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
    sort.draw();
  }

}
