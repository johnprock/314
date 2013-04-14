// Copyright (C) 2013, Gabriel Foust.
// All rights reserved.
// For instructional purposes only.

import java.util.Iterator;

public class Driver
{
    public static void main( String[] args ) throws Exception
    {
        Course course= makeCourse();

        String xml= XmlMarshaller.marshall( course );
        System.out.println( xml );

        Course course2= (Course)XmlMarshaller.unmarshall( xml );
        if (coursesEqual( course, course2 ))
            System.out.println( "Success" );
        else
            System.out.println( "Failure" ); 
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

    private static boolean coursesEqual( Course a, Course b )
    {
        if (a.getDepartment().equals( b.getDepartment() ) &&
            a.getNumber() == b.getNumber() &&
            a.getTitle().equals( b.getTitle() ) &&
            personsEqual( a.getTeacher(), b.getTeacher() ))
        {
            Iterator<Person> sa= a.getStudents().iterator();
            Iterator<Person> sb= b.getStudents().iterator();

            while (sa.hasNext() && sb.hasNext())
            {
                if (! personsEqual( sa.next(), sb.next() ))
                    return false;
            }

            return !(sa.hasNext() || sb.hasNext());
        }

        return false;
    }

    private static boolean personsEqual( Person a, Person b )
    {
        return
            a.getName().equals( b.getName() ) &&
            a.getId() == b.getId();
    }
}
