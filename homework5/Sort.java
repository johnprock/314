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


  static Vector<Integer> generate(int size) {
   // generate random sequence
    Vector<Integer> s = new Vector<Integer>();
    for(int i=0; i<size; i++) {
      s.add(i);
    }
    Collections.shuffle(s);
    return s;
  }


  //--------- BUBBLE SORT ---------//
  static Vector<Vector<Integer>> bubbleSort(int size) {
    Vector<Vector<Integer>> data = new Vector<Vector<Integer>>();
    Vector<Integer> s = generate(size);
   
    for(int i=0; i<size-1; i++) {
      for(int j=0; j<size-1; j++) {
        if(s.get(j) > s.get(j+1)) {     
          swap(s, j, j+1);
        }
        data.add(new Vector<Integer>(s));
      }
    }
    data.add(new Vector<Integer>(s));
    System.out.println(data.size());
    return data;
  }

  //------- INSERTION SORT --------//

  static Vector<Vector<Integer>> insertionSort(int size) {
    Vector<Vector<Integer>> data = new Vector<Vector<Integer>>();
    Vector<Integer> s = generate(size);

    for(int i=0; i<size; i++) {
      int val = s.get(i);
      int hole = i;

      while(hole > 0 && val < s.get(hole-1)) {
        s.set(hole,s.get(hole-1));
        hole--;
        data.add(new Vector<Integer>(s));
      }
      s.set(hole, val);
      data.add(new Vector<Integer>(s));
    }
    return data;
  }

  //----- SHELL SORT ------//

  static Vector<Vector<Integer>> shellSort(int size) {
    Vector<Vector<Integer>> data = new Vector<Vector<Integer>>();
    Vector<Integer> s = generate(size);
    
    int jump = size/2;
    while(jump > 0) {
      for(int i=jump; i<size; i++) {
        int temp = s.get(i);
        int j = i;
        while(j>=jump && s.get(j-jump) > temp) {
          s.set(j, s.get(j-jump));
          j -= jump;
          data.add(new Vector<Integer>(s));
        }
        s.set(j, temp);
        data.add(new Vector<Integer>(s));
      }
      jump = jump/2;
    }
    return data;
  }

  //----- MERGE SORT -----//

  static Vector<Vector<Integer>> mergeSort(int size) {
    Vector<Vector<Integer>> data = new Vector<Vector<Integer>>();
    Vector<Integer> s = generate(size);

    class Merge {
    }

    return data;
  }

  //---- QUICK SORT ----//

  static Vector<Vector<Integer>> quickSort(int size) {
    final Vector<Vector<Integer>> data = new Vector<Vector<Integer>>();
    Vector<Integer> s = generate(size);

    class QSort {

     Vector<Integer> swap(Vector<Integer> s, int a, int b) {
       Integer temp = s.get(a);
       s.set(a, s.get(b));
       s.set(b, temp);
       return s;
     }

     void qSort(Vector<Integer> seq, int l, int r) {
       if(l < r) {
        int p = partition(seq, l, r);
        qSort(seq, l, p-1);
        qSort(seq, p+1, r);
        data.add(new Vector<Integer>(seq));
       }
     }

     int partition(Vector<Integer> seq, int l, int r) {
      int x = seq.get(r);
      int i = l - 1;

      for(int j=1; j<=r; j++) {
        if(seq.get(j) <= x)
        {
          i++;
          seq = swap(seq, i, j);
        }
      }
      seq = swap(seq, i+1, r);
      return i+1;
      }

    }
    QSort q = new QSort();
    q.qSort(s,1,s.size()-1);
    
    return data;
  }

   //---- HEAP SORT ----//

 static Vector<Vector<Integer>> heapSort(int size) {
    Vector<Vector<Integer>> data = new Vector<Vector<Integer>>();
    Vector<Integer> s = generate(size);
    
    return data;
 }




    




  public static void main(String args[]) {

    int size = 50;
   
    JFrame frame = new JFrame();
//    frame.setDefaultCloseOperation(TextFrame.EXIT_ON_CLOSE);
    frame.setBounds(10,10,1000,210);

    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
    frame.add(panel);

    final Animate bubble = new Animate(10, 10, "Bubble Sort", size,
        bubbleSort(size)); 
    final Animate insert = new Animate(10, 10, "Insertion Sort", size,
        insertionSort(size));
    final Animate shell  = new Animate(10, 10, "Shell Sort", size,
        shellSort(size));
//    final Animate merge  = new Animate(10, 10, "Merge Sort", size,
//        mergeSort(size));
    final Animate quick  = new Animate(10, 10, "Quick Sort", size,
        quickSort(size));   
//    final Animate heap  = new Animate(10, 10, "Heap Sort", size,
//        heapSort(size));  
        
    
    panel.add(bubble);
    panel.add(insert);
    panel.add(shell);
//    panel.add(merge);
    panel.add(quick);
//    panel.add(heap);

    frame.setVisible(true);

    int delay = 50;
    ActionListener taskPerformer = new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        bubble.repaint();
        insert.repaint();
        shell.repaint();
//        merge.repaint();
        quick.repaint();
//        heap.repaint();
      }
    };

    new javax.swing.Timer(delay, taskPerformer).start();

  }
}


