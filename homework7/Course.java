// Copyright (C) 2013, Gabriel Foust.
// All rights reserved.
// For instructional purposes only.

import java.util.List;
import java.util.ArrayList;

public class Course
{
    private String department;
    private int number;
    private String title;
    private Person teacher;
    private List<Person> students= new ArrayList<Person>();

    public String getDepartment() { return department; }
    public void setDepartment( String department ) { this.department= department; }

    public int getNumber() { return number; }
    public void setNumber( int number ) { this.number= number; }

    public String getTitle() { return title; }
    public void setTitle( String title ) { this.title= title; }

    public Person getTeacher() { return teacher; }
    public void setTeacher( Person teacher ) { this.teacher= teacher; }

    public List<Person> getStudents() { return students; }
    public void setStudents( List<Person> students ) { this.students= students; }
}
