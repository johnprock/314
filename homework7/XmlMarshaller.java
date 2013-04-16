// Marshall works; unmarshall is able to get all of the setters and set all of the parts of the course class, but isn't recursively setting the methods of the Person class.

import java.lang.String;
import java.lang.Object;
import java.lang.Boolean;
import java.lang.reflect.*;
import java.util.*;
import java.lang.Class;
import java.io.File;

// JAXP APIs
//package dom;
import javax.xml.parsers.DocumentBuilder; 
import javax.xml.parsers.DocumentBuilderFactory;

// Exceptions
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException; 
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.*;


// Read XML
import java.io.OutputStreamWriter;
import java.io.PrintWriter;


// W3C Definitions for DOM
import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Entity;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
//eff the police

class XmlMarshaller {

    public static String marshall(Object bean) throws Exception {

      // obtain a list of getters and setters
      Class c = bean.getClass();
      Method[] methods = c.getMethods();
      String xml = "";
      Stack<String> stack = new Stack<String>();

      xml += openTag(c.getCanonicalName());

      // find all getters 
      for(int i=0; i<methods.length; i++) {
        String name = methods[i].getName(); 
        if(isGetter(name)) {
          xml += openTag(name.substring(3));
          xml += process(methods[i], bean);
          xml += closeTag(name.substring(3));
        }
      }
    

      xml += closeTag(c.getCanonicalName());

      return xml;
    }


    private static String process(Method method, Object bean) {
      
      try{
      Class<?> rtype = method.getReturnType();
      Object val = method.invoke(bean);

      if(rtype.getCanonicalName().equals("int")) {
          return val.toString() + "\n";
      }
      
      if(val instanceof String) {
          return val.toString() + "\n";
      }
      
      if(val instanceof List) {
        List list = (List) val;
        String result = "";
        for(int i=0; i<list.size(); i++) {
          result += marshall(list.get(i));
        }
        return result;
      }
  
      else {
        return marshall(val);
      }
      } catch (NullPointerException e) {System.out.println(e.getMessage());}
        catch (Exception e) {System.out.println(e.getMessage());}
      return "";
    }


    private static Boolean isGetter(String name) {
      if(name.substring(0,3).equals("get") && name.length() < 8) {
        return true;
      }
      if(name.substring(0,3).equals("get") &&
          !name.substring(3,8).equals("Class")) {
        return true;
      }
      return false;
    }

    private static String openTag(String name) {
      return "<" + name + ">\n";
    }

    private static String closeTag(String name) {
      return "</" + name + ">\n";
    }
 

    private static Course makeCourse()
    {
        Course course= new Course();
        course.setDepartment( "CSCE" );
        course.setNumber( 314 );
        course.setTitle( "Programming Languages" );

        Person person= new Person();
        person.setName( "Gabriel Dos Reis" );
        person.setId( 112358 );
        course.setTeacher( person );

        person= new Person();
        person.setName( "John Doe" );
        person.setId( 123456 );
        course.getStudents().add( person );

        person= new Person();
        person.setName( "Jane Roe" );
        person.setId( 234567 );
        course.getStudents().add( person );

        person= new Person();
        person.setName( "Wilma Flintstone" );
        person.setId( 345678 );
        course.getStudents().add( person );

        person= new Person();
        person.setName( "George Jetson" );
        person.setId( 456789 );
        course.getStudents().add( person );

        return course;
    }
	
	private static class eh implements ErrorHandler {
     
    private PrintWriter out;

   eh(PrintWriter out) {
        this.out = out;
    }

    private String getParseExceptionInfo(SAXParseException spe) {
        String systemId = spe.getSystemId();
        if (systemId == null) {
            systemId = "null";
        }

        String info = "URI=" + systemId + " Line=" + spe.getLineNumber() +
                      ": " + spe.getMessage();
        return info;
    }

    public void warning(SAXParseException spe) throws SAXException {
        out.println("Warning: " + getParseExceptionInfo(spe));
    }
        
    public void error(SAXParseException spe) throws SAXException {
        String message = "Error: " + getParseExceptionInfo(spe);
        throw new SAXException(message);
    }

    public void fatalError(SAXParseException spe) throws SAXException {
        String message = "Fatal Error: " + getParseExceptionInfo(spe);
        throw new SAXException(message);
    }
}


	public static Object unmarshall(String iFile) throws Exception {
		try{
		File inXML = new File(iFile);
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		DocumentBuilder db = dbf.newDocumentBuilder();
		Document doc = db.parse(inXML);
			
		doc.getDocumentElement().normalize();
			
		System.out.println("Root:" + doc.getDocumentElement().getNodeName());
		
		if (doc.hasChildNodes()){
			printNodes(doc.getChildNodes());
		}
			
		
		Object o = new Object();
		// Turns class into a class of the root node
		Class c = Class.forName(doc.getDocumentElement().getNodeName());
		// Turns object into an instance of the class
		o = c.newInstance();
		Method[] methods = c.getMethods();
		// Print setters
		for(int i = 0; i < methods.length; i++){
			String name = methods[i].getName();
			if(isSetter(methods[i].getName())){ // Iterates through all methods; for the set methods, we build the object and invoke
				System.out.println(name);
				System.out.println("Feeding node: " + doc.getChildNodes());
				buildObject(methods[i], o, doc.getChildNodes(), doc.getDocumentElement());
				System.out.println("We built using method: " + i);
			}
		}
		
		return o;
		} catch (Exception e){
		System.out.println(e.getMessage());
		//return "Whoops.";
		return null;
		}
	}
	
	public static boolean isSetter(String name){
		if( name.substring(0,3).equals("set") ) {
			return true;
		}
		else{
			return false;
		}
	}
	
	// Will call a method on an object if the value of the node and the return type of a method match
	public static Object buildObject(Method method, Object obj, NodeList ns, Node n){
		Class<?> rtype = method.getReturnType();
		try{
		Class c = Class.forName(n.getNodeName());
		System.out.println(c);
		
		// For any given leaf, query the method of the return type.
		for(int i =0; i < ns.getLength(); i++){
			Node temp = ns.item(i);
			String name = temp.getNodeName(); // number
			String val = temp.getTextContent(); // 314
			String methodname = method.toString();
			
			System.out.println("The name of the node: " + name);
			System.out.println("The size of the name: " + name.length());
			System.out.println("The value of the node: " + val);
			System.out.println("The method in use is: " + method);
			try{
				Field f = c.getDeclaredField(name);
				String fn = f.toString();
				if( fn.indexOf("int") != -1){
					int tempint = (int)Integer.parseInt(val);
					method.invoke(obj, tempint);
					System.out.println("Success for ints " + f + " " + name);
				}
			
				else if( ( fn.indexOf("String") != -1 ) && (name.indexOf("department") != -1 && (methodname.indexOf("Department") != -1)) ){
					method.invoke(obj, val);
					System.out.println("Success for department Strings " + f + " " + name + methodname);
				}
			
				else if( (fn.indexOf("Person") != -1) && (name.indexOf("teacher") != -1) ) {
					method.invoke(obj, val);
					System.out.println("Success for People! " + f + " " + name);
				}
				// else if( val instanceof String && (name.indexOf("students") != -1)){
					// System.out.println("This is a list: " + val);
					// List list = (List) val;
					// String result = "";
					// for(int k=0; i<list.size(); k++) {
					// result += buildObject(list.get(i));
					//}
				// }

				// else if(name.indexOf("#text") != -1){
					// System.out.println("WE ARE ACTUALLY USING THE TEXT ONE");
					// System.out.println("WE ARE ACTUALLY USING THE TEXT ONE");
					// buildObject(method, obj, temp.getChildNodes(), temp);
				// } 
				
				else{
					System.out.println(" The name of this node is: " + temp.getNodeName() );
				}
				
			} catch(Exception e){
				//buildObject(method, obj, temp.getChildNodes(), temp);
				System.out.println(e.toString());
				}
			
			if(temp.hasChildNodes()){
				buildObject(method, obj, temp.getChildNodes(), n);
			}
			
		}
		
		}	catch(Exception e){ e.toString();}
		return obj;
	}
	
		public static Document loadXML(String iFile){ 
		try{
			File inXML = new File(iFile);
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			DocumentBuilder db = dbf.newDocumentBuilder();
			Document doc = db.parse(inXML);
		
			doc.getDocumentElement().normalize();
			
			System.out.println("Root:" + doc.getDocumentElement().getNodeName());
			
			if (doc.hasChildNodes()){
				printNodes(doc.getChildNodes());
			}
			
			return doc;
		} catch (Exception e){
			System.out.println(e.getMessage());
			//return "Whoops.";
			return null;
			}
	}
	
		// utility function mkyong
	private static void printNodes(NodeList ns){
		//out.print(" 
		for(int i = 0; i < ns.getLength(); i++){
			Node temp = ns.item(i);
				// Check if element is actually a node
				if(temp.getNodeType() == Node.ELEMENT_NODE) {
					System.out.println("[BEGIN]: " + temp.getNodeName() );
					System.out.println("Node Name = " + temp.getNodeName() );
					System.out.println("Node Val =" + temp.getTextContent());
						if(temp.hasAttributes()) {
							NamedNodeMap nm = temp.getAttributes();
								for(int j = 0; j < nm.getLength(); j++) {
									Node n = nm.item(i);
									System.out.println(" Attribute name: " + n.getNodeName());
									System.out.println(" Attribute value: " + n.getNodeValue());
								}
						}
					if(temp.hasChildNodes()){
						// loop if the root has a child
						printNodes(temp.getChildNodes());
					}
					
					System.out.println("[END] : " + temp.getNodeName() + "\n");
					//System.out.println("End of node" 
				}
		}
	}
	


	
	
   
    public static void main(String[] args){
      try {
        //Course course = makeCourse();
        //System.out.println(marshall(course));
		unmarshall("test.xml");
      } catch(Exception e) { System.out.println("marshall error");}
    }
}
