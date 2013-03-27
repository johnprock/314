import java.lang.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;



class AnimateSorting {

  public static void main(String[] args) {
    new AnimateSorting(); 
  }

  public AnimateSorting() {
    EventQueue.invokeLater(new Runnable() {   
      @Override
      public void run() {
        JFrame frame = new JFrame();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(100,100);
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
        frame.setLayout(new BoxLayout(frame, BoxLayout.Y_AXIS));

        JPanel p1 = new JPanel();
        JPanel p2 = new JPanel();
        p1.setLayout(new BoxLayout(p1, BoxLayout.X_AXIS));
        p2.setLayout(new BoxLayout(p2, BoxLayout.X_AXIS));
        frame.add(p1);
        frame.add(p2);


        Graph bubble = new Graph();
        Graph insert = new Graph();
        Graph shell = new Graph();
        Graph merge = new Graph();
        Graph quick = new Graph();
        Graph heap = new Graph(); 

        new Thread(new EvalThread(p1, bubble)).start();
        new Thread(new EvalThread(p1, insert)).start();
        new Thread(new EvalThread(p1, shell)).start();
        new Thread(new EvalThread(p2, merge)).start();
        new Thread(new EvalThread(p2, quick)).start();
        new Thread(new EvalThread(p2, heap)).start();

      }
    });
  }

  public static class EvalThread implements Runnable {

    private final JPanel panel;
    private Graph graph;


    public EvalThread(JPanel panel, Graph graph) {
      this.panel = panel;
      this.graph = graph;
    }

    @Override
    public void run() {
    
    }
  }
}

class Graph extends JComponent {

  public Graph() {
  }

  public void paintComponent(Graphics g) {
    super.paintComponent(g);
  }
}
