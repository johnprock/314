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
 
      int box_x = 150;

      int width = (int) (box_x/(double)size);
      int height = (int) (box_x/(double)size);

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
    final Vector<Vector<Integer>> data = new Vector<Vector<Integer>>();
    Vector<Integer> s = generate(size);

    class Merge {
  

      Merge(Vector<Integer> _seq) {
        mergeSort(_seq, 0, _seq.size()-1);
      }

      Vector<Integer> mergeSort(Vector<Integer> seq, int left, int right) {
        if(left >= right) {
            return seq;
        }
      
        int mid = (left+right)/2;
        Vector<Integer> a = mergeSort(new Vector<Integer>(seq), left, mid);
        Vector<Integer> b = mergeSort(new Vector<Integer>(seq), mid+1, right);
        Vector<Integer> mer = merge(a , b, left, right, mid);
        data.add(new Vector<Integer>(mer));
        return mer;

      }

      Vector<Integer> merge(Vector<Integer> a, Vector<Integer> b, int left, int
        right, int mid) {

        Vector<Integer> c = new Vector<Integer>(a);
        
        int i = left;
        int j = mid + 1;
        int k = left;
        while(k <= right) {
         
          if(j > right) {
            c.set(k,a.get(i));
            i++;
            k++;
            data.add(new Vector<Integer>(c));
          }
          else
          if(i > mid) {
            c.set(k,b.get(j));
            j++;
            k++;
            data.add(new Vector<Integer>(c));
          }
          else
          if(a.get(i) <= b.get(j)) {
            c.set(k,a.get(i));
            i++;
            k++;
            data.add(new Vector<Integer>(c));
          }
          else {
            c.set(k,b.get(j));
            j++;
            k++;
            data.add(new Vector<Integer>(c));
          }
        }
        return c;
      }
    
    }
    Merge m = new Merge(s);
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
        int p = partition(seq, l, r, l); 
        qSort(seq, l, p-1);
        qSort(seq, p+1, r);
       }
     }

     int partition(Vector<Integer> seq, int left, int right, int pivot) { 
      
       int pval = seq.get(pivot);
       swap(seq, pivot, right);
       int store = left;
       for(int i=left; i<right; i++) {
         if(seq.get(i) <= pval) {
           swap(seq, i, store);
           store++;
         }
         data.add(new Vector<Integer>(seq));
       }
       swap(seq,store, right);
       data.add(new Vector<Integer>(seq));
       return store;
     }

    }

    QSort q = new QSort();
    q.qSort(s,0,s.size()-1);
    
    return data;
  }

   //---- HEAP SORT ----//

 static Vector<Vector<Integer>> heapSort(int size) {
    final Vector<Vector<Integer>> data = new Vector<Vector<Integer>>();
    Vector<Integer> s = generate(size);
   
    class HSort {
      Vector<Integer> seq;


       void swap(int a, int b) {
       Integer temp = seq.get(a);
       seq.set(a, seq.get(b));
       seq.set(b, temp);
       }


      
      
      HSort(Vector<Integer> _seq) {
        seq = new Vector<Integer>(_seq);
      }

      void heapSort(int count) {
        heapify(count);

        int end = count-1;
        while(end > 0) {
          swap(end, 0);
          data.add(new Vector<Integer>(seq));
          end--;
          siftDown(0, end);
        }
      }

      void heapify(int count) {
        int start = (count-2)/2;

        while(start >= 0) {
          siftDown(start, count-1);
          start--;
          data.add(new Vector<Integer>(seq));
        }
      }

      void siftDown(int start, int end) {
        int root = start;
        int child = 0;
        int swap = 0;

        while( root*2 +1 <= end ) {
          child = root*2 +1;
          swap = root;
          data.add(new Vector<Integer>(seq));
          if(seq.get(swap) < seq.get(child)) {
            swap = child;
          }
          if( child+1 <= end && seq.get(swap) < seq.get(child+1) ) {
            swap = child + 1;
          }
          if( swap != root ) {
            swap(root, swap);
            root = swap;
          }
          else {
            break;
          }
        }
      }
    }
    HSort h = new HSort(s);
    h.heapSort(size);
    return data;
 }




    




  public static void main(String args[]) {

    if(args.length < 2) {
      System.out.println("Correct usage: Java Sort <size> <delay>");
    }

    int size = Integer.parseInt(args[0]);
    int delay = Integer.parseInt(args[1]);

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
    final Animate merge  = new Animate(10, 10, "Merge Sort", size,
        mergeSort(size));
    final Animate quick  = new Animate(10, 10, "Quick Sort", size,
        quickSort(size));   
    final Animate heap  = new Animate(10, 10, "Heap Sort", size,
        heapSort(size));  
        
    
    panel.add(bubble);
    panel.add(insert);
    panel.add(shell);
    panel.add(merge);
    panel.add(quick);
    panel.add(heap);

    frame.setVisible(true);

    ActionListener taskPerformer = new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        bubble.repaint();
        insert.repaint();
        shell.repaint();
        merge.repaint();
        quick.repaint();
        heap.repaint();
      }
    };

    new javax.swing.Timer(delay, taskPerformer).start();

  }
}


