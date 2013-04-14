import java.lang.String;
import java.lang.Object;
import java.lang.Boolean;
import java.lang.reflect.*;
import java.util.*;
import java.lang.Class;

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
        return "list\n";
      }
  
      else {
        System.out.println("Marshalling " + val.toString() + "...");
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
  

    public static Object unmarshall(String xml) throws Exception {
      Object o = new Object();
      return o;
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
   
    public static void main(String[] args){
      try {
        Course course = makeCourse();
        System.out.println(marshall(course));
      } catch(Exception e) { System.out.println("marshall error");}
    }
}
