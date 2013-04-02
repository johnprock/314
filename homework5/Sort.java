import java.lang.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;


  class Animate extends JComponent {
    private int xcoord;
    private int ycoord;
    private int size;
    private int p; // counts number of repaints
    private String name;
    Vector<Vector<Integer>> data;

    // constructor
    public Animate(int x, int y, String n, int sze, Vector<Vector<Integer>> _data) {
      xcoord = x;
      ycoord = y;
      size = sze;
      name = n;
      data = _data;
      p = 0;
    }

    public void paintComponent(Graphics g) {
      super.paintComponent(g);

      g.setColor(Color.black);
      g.drawString(name, xcoord, ycoord + 165);
      drawBars(g);

    }
  private void drawBars(Graphics g) {
 
      int width = (int) (150.0/(double)size);
      int height = (int) (150.0/(double)size);
      g.setColor(Color.black);
      g.drawRect (xcoord, ycoord, width*size, height*size);
      Vector<Integer> v = data.get(p);
      if(p < data.size()-1) {
        p++;
      }

      g.setColor(Color.blue);
      for(int i=0; i<v.size(); i++) {
        g.fill3DRect(xcoord+i*width, ycoord, width,
            v.get(i)*height + 1 , false);
      }
    }
  }




class Sort {

  static void swap(Vector<Integer> s, int a, int b) {
    Integer temp = s.get(a);
    s.set(a,s.get(b));
    s.set(b,temp);
  }

  static Vector<Vector<Integer>> BubbleSort(int size) {
    Vector<Vector<Integer>> data = new Vector<Vector<Integer>>();
    
    // generate random sequence
    Vector<Integer> s = new Vector<Integer>();
    for(int i=0; i<size; i++) {
      s.add(i);
    }
    Collections.shuffle(s);

    for(int i=0; i<size-1; i++) {
      for(int j=0; j<size-1; j++) {
        if(s.get(j) > s.get(j+1)) {     
          data.add(new Vector<Integer>(s));
          swap(s, j, j+1);
        }
      }
    }
    data.add(new Vector<Integer>(s));
    System.out.println(data.size());
    return data;
  }


  public static void main(String args[]) {
   
    JFrame frame = new JFrame();
    frame.setDefaultCloseOperation(TextFrame.EXIT_ON_CLOSE);
    frame.setBounds(10,10,1000,210);

    final Animate bubble = new Animate(10, 10, "Bubble Sort", 30, BubbleSort(30));
    frame.add(bubble);

    frame.setVisible(true);

    int delay = 50;
    ActionListener taskPerformer = new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        bubble.repaint();
      }
    };

    new javax.swing.Timer(delay, taskPerformer).start();

  }
}
