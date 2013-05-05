// Author: Gabriel Dos Reis, http://www.axiomatics.org/~gdr/
// Copyright (C) 2013, Gabriel Dos Reis.
// All rights reserved.

import java.lang.*;
import java.io.*;

// The purpose of this class is to report a certain summary of class file.
// The assignment does not need to have this class extends ClassLoader.
// Using `Class.forName' is sufficient to get the interfaces. However,
// we could imagine equpping our class dissecter to look into
// non-standard places.  In that case, extending a ClassLoader is the way
// to go.  I leave that as a complimentary exercise to do.
public class ClassFileReporter extends ClassLoader {
   // Main entry point to this module.
   public static void main(String[] args) {
      // We need something to work one.
      if (args.length == 0) 
         fatalError("DissectClassFile requires at least one class file");

      for (String arg: args) {
         FileInputStream inFile = null;
         try {
            checkClassExtension(arg);
            inFile = new FileInputStream(arg);
            processClassFile(arg, inFile);
         }
         catch (Exception e) {
            fatalError(e.getMessage());
         }
         finally {
            try {
               if (inFile != null)
                  inFile.close();
            }
            catch (IOException e) {
               // OK, the system really is hosed.
            }
         }
      }
   }

   // print message on the standard error stream, and quit with non
   // zero exit status.
   private static void fatalError(String msg) {
      System.err.println(msg);
      System.exit(1);
   }

   // process a class file, supposedly represented by `inStream'.
   private static void processClassFile(String file, InputStream inStream)
      throws InvalidIndexException, EOFException, IOException {
      DataInputStream inData = new DataInputStream(inStream);
      ClassFileReporter reporter = new ClassFileReporter();
      reporter.readClassFile(inData);
      reporter.report(file);
      reporter.reportSummary();
   }

   // read the entire purported class file `input'.
   private void readClassFile(DataInputStream input)
      throws InvalidIndexException, EOFException, IOException {
      // 1. check magic number
      checkMagicNumber(input);
      
      // 2. Read version information.
      readVersionInformation(input);
      
      // 3. Read constant pool.
      readConstantPool(input);
      
      // 4. Read access flags
      reportAccessFlags(input);
      
      // 5. Read this class
      readThisClass(input);
      
      // 6. read super class
      readSuperClass(input);
      
      // 7. Read interfaces.
      readInterfaces(input);
      
      // 8. Read fields.
      read_field_info(input);
      
      // 9. Read methods.
      read_method_info(input);
      
      // 10. Read attributes.
      attributes = new attribute_info[input.readShort()];
      readAttributes(attributes, input);
   }

   // Print on the standard output a summary of the class file
   private void reportSummary() {
      reportVersion();
      reportThisClassName();
      reportSuperClass();
      reportConstantPoolSize();
      reportInterfaces();
      reportFields();
      reportMethods();
   }

   // `path' is supposed to be the name of a class file in the 
   // filesystem.  We insist that it must have ".class" extension.
   private static void checkClassExtension(String path) throws IOException {
      if (!path.endsWith(".class")) 
         throw new IOException
            ("file `" + path + "' does not have a .class extension");
   }

   // Ensures the first four bytes of `input' are the magic number
   // for a class file.
   private static void checkMagicNumber(InputStream input)
      throws IOException {
      final byte[] magicNumber = new byte[4];
      if (input.read(magicNumber, 0, 4) != 4)
         throw new IOException("input stream not a valid class file");
      if (!isMagicNumber(magicNumber)) {
         throw new IOException("first four bytes: "
                               + magicNumber[0] + magicNumber[1]
                               + magicNumber[2] + magicNumber[3]);
      }
   }

   // returns true if buf represents the 0xCAFEBABE.
   private static boolean isMagicNumber(byte[] buf) {
      final byte[] officialMagicNumber = {
         (byte)0xCA, (byte)0xFE, (byte)0xBA, (byte)0xBE
      };
      for (int i = 0; i < 4; ++i)
         if (buf[i] != officialMagicNumber[i])
            return false;
      return true;
   }

   // read the version information from the class file `input'.
   private final void readVersionInformation(DataInputStream input)
      throws EOFException, IOException {
      minorVersion = input.readShort();
      majorVersion = input.readShort();
   }

   // We know that `input' has the right magic number, and contains
   // version information.  Now we go on reading the constant pool.
   // All `immediate constants' (e.g. int, float, double, utf8 string),
   // are read and represented directly.  Other constants are resolved
   // lazily, in the sense that we read the indices but we don't resolve
   // them immediately.  One reason for doing it that way is we
   // cannot assume -a priori- that a constant can refer only previously
   // seen constants.  In particular, a constant may refer constants yet
   // to be processed.
   private final void readConstantPool(DataInputStream input)
      throws InvalidIndexException, EOFException, IOException {
      final int poolSize = input.readShort() - 1;
      constantPool = new cp_info[poolSize];

      int i = 0;
      while (i < poolSize)
         i = readConstant(i, input);
   }

   // Objects of this type signal invalid indices into the constant pool.
   private final class InvalidIndexException extends Exception {
      InvalidIndexException(short i) {
         super("invalid index: " + i);
      }
   };

   // a valid `index' into the constrant pool must be positive and
   // less then the size of the constant pool.  Note that the
   // length of constantPool has already been adjusted by 1.
   private final short validateIndex(short index)
      throws InvalidIndexException {
      if (index <= 0 || index > constantPool.length)
         throw new InvalidIndexException(index);
      return index;
   }

   // Read  a valid constant pool index from `input'.
   private final short readIndex(DataInputStream input)
      throws InvalidIndexException, EOFException, IOException {
      return validateIndex(input.readShort());
   }

   // The kind of constants present in a constant pool.  This is not
   // required by the exercise, but is a nice debugging tool, in
   // case such a practice is needed.
   private enum ConstantType {
      CONTANT_dummy_0,             // tag 0
      CONSTANT_Utf8,               // tag 1
      CONSTANT_dummy_2,            // tag 2
      CONSTANT_Integer,            // tag 3
      CONSTANT_Float,              // tag 4
      CONSTANT_Long,               // tag 5
      CONSTANT_Double,             // tag 6
      CONSTANT_Class,              // tag 7
      CONSTANT_String,             // tag 8
      CONSTANT_Fieldref,           // tag 9
      CONSTANT_Methodref,          // tag 10
      CONSTANT_InterfaceMethodref, // tag 11
      CONSTANT_NameAndType,        // tag 12
      CONSTANT_MethodHandle,       // tag 15
      CONSTANT_MethodType,         // tag 16
      CONSTANT_InvokeDynamic       // tag 18
   };

   // Common base class of items in the `constant pool'.
   private static abstract class cp_info {
      public abstract ConstantType getConstantType();
   };

   // A CONSTANT_Utf8_info object holds the UTF8 representation
   // of a string literal of length `n', read from the the `input' stream.
   private static final class CONSTANT_Utf8_info extends cp_info {
      CONSTANT_Utf8_info(DataInputStream input)
         throws EOFException, IOException {
         byte[] bytes = new byte[input.readShort()];
         input.read(bytes);
         text = new String(bytes,0,bytes.length, "utf8");
      }

      // returns the text of the string literal.
      public final String getString() { return text; }

      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_Utf8;
      }
      
      // byte representation of the UTF8 string.
      private final String text;
   };

   // A CONSTANT_Class_info object holds an index into the constant
   // pool for a class or interface.
   private final class CONSTANT_Class_info extends cp_info {
      CONSTANT_Class_info(DataInputStream input)
         throws InvalidIndexException, EOFException, IOException {
         name_index = readIndex(input);
      }

      // Return the Java-level fully qualified names version of the
      // JVM-level fully qualified names.  This means substituting
      // dots for slashs.
      public final String getName() {
         return getUtf8_info(name_index).getString().replace('/','.');
      }
      
      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_Class;
      }
      
      private final short name_index;
   };

   // A CONSTANT_NameAndType_info object holds indices into the constant
   // pool that describe the name and type of a member.
   private final class CONSTANT_NameAndType_info extends cp_info {
      CONSTANT_NameAndType_info(DataInputStream input)
         throws InvalidIndexException, EOFException, IOException {
         name_index = readIndex(input);
         descriptor_index = readIndex(input);
      }
      
      // returns the unqualified name of the name.
      public final String getName() {
         return getUtf8_info(name_index).getString();
      }

      // returns the type descriptor of the member.
      public final String getDescriptor() {
         return getUtf8_info(descriptor_index).getString();
      }

      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_NameAndType;
      }
      
      private final short name_index;
      private final short descriptor_index;
   };

   // A CONSTANT_Member_info object holds indices into the constant
   // pool that describe the enclosing class, type and name of the
   // member.  Please note that this is a convenience class that 
   // does not appear in the JVMS.  It serves here as factorization
   // of commonalities.  See the next three derived classes below.
   private abstract class CONSTANT_Member_info extends cp_info {
      CONSTANT_Member_info(DataInputStream input)
         throws InvalidIndexException, EOFException, IOException {
         class_index = readIndex(input);
         name_and_type_index = readIndex(input);
      }

      // returns the CONSTANT_Class_info that describes the enclosing
      // class of this member.
      public final CONSTANT_Class_info getEnclosingClass() {
         return getClass_info(class_index);
      }

      // returns the CONSTANT_NameAndType_info that describes the
      // name and type of this member.
      public final CONSTANT_NameAndType_info getNameAndType() {
         return (CONSTANT_NameAndType_info)
            constantPool[name_and_type_index - 1];
      }

      private final short class_index;
      private final short name_and_type_index;
   };

   private final class CONSTANT_Fieldref_info extends CONSTANT_Member_info {
      CONSTANT_Fieldref_info(DataInputStream input)
      throws InvalidIndexException, EOFException, IOException {
         super(input);
      }

      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_Fieldref;
      }
   };

   private final class CONSTANT_Methodref_info extends CONSTANT_Member_info {
      CONSTANT_Methodref_info(DataInputStream input)
      throws InvalidIndexException, EOFException, IOException {
         super(input);
      }

      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_Methodref;
      }
   };

   private final class CONSTANT_InterfaceMethodref_info
      extends CONSTANT_Member_info {
      CONSTANT_InterfaceMethodref_info(DataInputStream input)
         throws InvalidIndexException, EOFException, IOException {
         super(input);
      }

      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_InterfaceMethodref;
      }
   };

   // A CONSTANT_String_info object holds an index into the constant
   // pool for a string literal, represented in an UTF8 encoding.
   private final class CONSTANT_String_info extends cp_info {
      CONSTANT_String_info(DataInputStream input)
         throws InvalidIndexException, EOFException, IOException {
         index = readIndex(input);
      }

      public final String text() {
         return getUtf8_info(index).getString();
      }

      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_String;
      }

      private final short index;
   };

   // A CONSTANT_InterfaceMethodref_info object holds an integer
   // literal value stored in the constant pool.
   private static final class CONSTANT_Integer_info extends cp_info {
      CONSTANT_Integer_info(DataInputStream input)
         throws EOFException, IOException {
         value = input.readInt();
      }

      public final int getValue() { return value; }

      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_Integer;
      }

      private final int value;
   };

   // A CONSTANT_Float_info object holds a float literal value
   // stored in the constant pool.
   private static final class CONSTANT_Float_info extends cp_info {
      CONSTANT_Float_info(DataInputStream input)
         throws EOFException, IOException {
         value = input.readFloat();
      }

      public final float getValue() { return value; }

      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_Float;
      }

      private final float value;
   };

   // A CONSTANT_Long_info object holds a long literal value stored
   // in the constant pool.
   private static final class CONSTANT_Long_info extends cp_info {
      CONSTANT_Long_info(DataInputStream input)
         throws EOFException, IOException {
         value = input.readLong();
      }

      public final long getValue() { return value; }

      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_Long;
      }

      private final long value;
   };

   // A CONSTANT_Double_info object holds a double literal value stored
   // in the constant pool.
   private static final class CONSTANT_Double_info extends cp_info {
      CONSTANT_Double_info(DataInputStream input)
         throws EOFException, IOException {
         value = input.readDouble();
      }

      public final double getValue() { return value; }

      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_Double;
      }
      
      private final double value;
   };

   private class attribute_info {
      attribute_info(DataInputStream input)
         throws InvalidIndexException, EOFException, IOException {
         name_index = readIndex(input);
         info = new byte[input.readInt()];
         input.read(info);
      }

      public final String getName() {
         return getUtf8_info(name_index).getString();
      }

      public final byte[] getInfo() { return info; }
      
      private final short name_index;
      private final byte[] info;
   };
   

   // common base class for field_info and method_info.
   private class member_info {
      member_info(DataInputStream input)
         throws InvalidIndexException, EOFException, IOException {
         access_flags = input.readShort();
         name_index = readIndex(input);
         descriptor_index = readIndex(input);
         attributes = new attribute_info[input.readShort()];
         readAttributes(attributes, input);
      }

      public final String getName() {
         return getUtf8_info(name_index).getString();
      }

      public final String getDescriptor() {
         return getUtf8_info(descriptor_index).getString();
      }

      public final attribute_info[] getAttributes() {
         return attributes;
      }

      private final short access_flags;
      private final short name_index;
      private final short descriptor_index;
      private final attribute_info[] attributes;
   };

   private final class field_info extends member_info {
      field_info(DataInputStream input)
         throws InvalidIndexException, EOFException, IOException {
         super(input);
      }
   };

   private final class method_info extends member_info {
      method_info(DataInputStream input)
         throws InvalidIndexException, EOFException, IOException {
         super(input);
      }
   };

   private final class CONSTANT_MethodHandle_info extends cp_info {
      CONSTANT_MethodHandle_info(DataInputStream input)
         throws InvalidIndexException, EOFException, IOException {
         reference_kind = input.readByte();
         reference_index = readIndex(input);
      }

      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_MethodHandle;
      }

      private final byte reference_kind;
      private final short reference_index;
   }

   private final class CONSTANT_MethodType_info extends cp_info {
      CONSTANT_MethodType_info(DataInputStream input)
         throws InvalidIndexException, EOFException, IOException {
         descriptor_index = readIndex(input);
      }

      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_MethodType;
      }

      public final String getDescriptor() {
         return getUtf8_info(descriptor_index).getString();
      }

      private final short descriptor_index;
   }

   private final class CONSTANT_InvokeDynamic_info extends cp_info {
      CONSTANT_InvokeDynamic_info(DataInputStream input)
         throws InvalidIndexException, EOFException, IOException {
         bootstrap_method_attr_index = input.readShort();
         name_and_type_index = readIndex(input);
      }

      public final ConstantType getConstantType() {
         return ConstantType.CONSTANT_InvokeDynamic;
      }

      private final short bootstrap_method_attr_index;
      private final short name_and_type_index;
   }

   // Read an item from the constant pool, and create a representation
   // for it.  Return the next valid index into the constant pool.
   private final int readConstant(int index, DataInputStream input)
      throws InvalidIndexException, EOFException, IOException {
      // what kind of constant are we processing?
      final byte tag = input.readByte();
      switch (tag) {
      case 1: 
         constantPool[index] = new CONSTANT_Utf8_info(input);
         break;

      case 3:
         constantPool[index] = new CONSTANT_Integer_info(input);
         break;
         
      case 4:
         constantPool[index] = new CONSTANT_Float_info(input);
         break;

      case 5:
         constantPool[index] = new CONSTANT_Long_info(input);
         // skip next constant pool entry as mandated by the JVMS.
         ++index;
         break;
         
      case 6:
         constantPool[index] = new CONSTANT_Double_info(input);
         // skip next constant pool entry as mandated by the JVMS.
         ++index;
         break;

      case 7: 
         constantPool[index] = new CONSTANT_Class_info(input);
         break;
         
      case 8:
         constantPool[index] = new CONSTANT_String_info(input);
         break;

      case 9:
         constantPool[index] = new CONSTANT_Fieldref_info(input);
         break;
         
      case 10:
         constantPool[index] = new CONSTANT_Methodref_info(input);
         break;
         
      case 11:
         constantPool[index] = new CONSTANT_InterfaceMethodref_info(input);
         break;

      case 12:
         constantPool[index] =  new CONSTANT_NameAndType_info(input);
         break;

      case 15:
         constantPool[index] = new CONSTANT_MethodHandle_info(input);
         break;

      case 16:
         constantPool[index] = new CONSTANT_MethodType_info(input);
         break;

      case 18:
         constantPool[index] = new CONSTANT_InvokeDynamic_info(input);
         break;

      default:
         throw new IOException("unknown constant type: " + tag);
      }
      return index + 1;
   }

   // read the constant pool index for the class defined by this class file.
   private final void readThisClass(DataInputStream input)
      throws InvalidIndexException, EOFException, IOException {
      thisClass = getClass_info(readIndex(input));
   }

   // read the constant pool index for the super class of the
   // class defined by this class file.
   private final void readSuperClass(DataInputStream input)
      throws InvalidIndexException, EOFException, IOException {
      // the super class index may be zero, in which case thisClass
      // must be Object.  So, we can't use readIndex().
      final short index = input.readShort();
      if (index == 0)
         superClass = null;     // ??? we really meant Object.
      else
         superClass = getClass_info(validateIndex(index));
   }

   // read the indices of the interfaces of the class defined by
   // this class file.
   private final void readInterfaces(DataInputStream input)
      throws InvalidIndexException, EOFException, IOException {
      final short iCount = input.readShort();
      interfaces = new CONSTANT_Class_info[iCount];
      for (short i = 0; i < iCount; ++i) 
         interfaces[i] = getClass_info(readIndex(input));
   }

   // parse information about the fields of the class defined
   // by this class file.
   private final void read_field_info(DataInputStream input)
      throws InvalidIndexException, EOFException, IOException {
      fields = new field_info[input.readShort()];
      for (short i = 0; i < fields.length; ++i)
         fields[i] = new field_info(input);
   }

   // parse information about the methods of the class defined
   // by this class file.
   private final void read_method_info(DataInputStream input)
      throws InvalidIndexException, EOFException, IOException {
      methods = new method_info[input.readShort()];
      for (short i = 0; i < methods.length; ++i)
         methods[i] = new method_info(input);
   }

   // parse various attribute information.
   private final void readAttributes(attribute_info[] attrs,
                                     DataInputStream input)
      throws InvalidIndexException, EOFException, IOException {
      for (int i = 0; i < attrs.length; ++i) 
         attrs[i] = new attribute_info(input);
   }

   // print the name of the class defined by this class file.
   private final void reportThisClassName() {
      report("\tthis class name: " + thisClass.getName());
   }

   // print the name of super class's interfaces, if there.
   private final void reportSuperClassInterfaces() {
      try {
         Class s = Class.forName(superClass.getName());
         System.out.print("\t\tinterfaces:");
         for (Class i : s.getInterfaces())
            System.out.print(" " + i.getName());
         System.out.println();
      }
      catch(ClassNotFoundException e) {
         report("\tsuper class not found: " + e.getMessage());
      }
   }
   
   // print the name of the super class of this class file.
   private final void reportSuperClass() {
      if (superClass == null)
         report("\tsuper class name: <inexistent>");
      else {
         report("\tsuper class name: " + superClass.getName());
         reportSuperClassInterfaces();
      }
   }

   // print the size of the constant pool.
   private final void reportConstantPoolSize() {
      report("\tconstant pool size: " + constantPool.length);
   }

   // print the name of the interfaces of this class.
   private final void reportInterfaces() {
      report("\tnumber of interfaces: " + interfaces.length);
      for (int i = 0; i < interfaces.length; ++i)
         report("\t\t interface #" + i + ": " + interfaces[i].getName());
   }

   // print the name of the fields of this class.
   private final void reportFields() {
      report("\tnumber of fields: " + fields.length);
      for (int i = 0; i < fields.length; ++i)
         report("\t\t field #" + i + ": " + fields[i].getName());
      reportUnderbarNames();
   }

   private final int countUnderbarNames() {
      int n = 0;
      for (int i = 0; i < fields.length; ++i)
         if (fields[i].getName().contains("_"))
            ++n;
      return n;
   }

   // print the number of methods with a least one underscore character
   private final void reportUnderbarNames() {
      report("\tnumber of fields with underscore character: "
             + countUnderbarNames());
   }

   // Return true if the string argument contains at least
   // one character.
   static private boolean isCamelCaseName(String s) {
      for (int i = 0; i < s.length(); ++i)
         if (Character.isUpperCase(s.charAt(i)))
            return true;
      return false;
   }
   
   private final int countCamelCaseNames() {
      int n = 0;
      for (method_info m : methods)
         if (isCamelCaseName(m.getName()))
            ++n;
      return n;
   }

   // print the number of methods with a least one uppercase character
   private final void reportCamelCaseNames() {
      report("\tnumber of methods with at least one uppercase character: "
             + countCamelCaseNames());
   }

   // print the name of the methods of this class.
   private final void reportMethods() {
      report("\tnumber of methods: " + methods.length);
      for (int i = 0; i < methods.length; ++i)
         report("\t\t method #" + i + ": " + methods[i].getName());
      reportCamelCaseNames();
   }

   // returns the representation of a class constant at
   // entry indicated by `index' in the constant pool.
   private final CONSTANT_Class_info getClass_info(short index) {
      return (CONSTANT_Class_info) constantPool[index - 1];
   }

   // resolve the utf8 string at `index' entry in the constant pool.
   private final CONSTANT_Utf8_info getUtf8_info(short index) {
      return (CONSTANT_Utf8_info) constantPool[index - 1];
   }

   // return the `stringified' version of a set of `access specifiers'.
   private static String accessAsString(short access) {
      String flags = new String();
      if ((access & 0x0001) == 0x0001)
         flags += "public ";
      if ((access & 0x0002) == 0x0002)
         flags += "private ";
      if ((access & 0x0004) == 0x0004)
         flags += "protected ";
      if ((access & 0x0008) == 0x0008)
         flags += "static ";
      if ((access & 0x0010) == 0x0010)
         flags += "final ";
      if ((access & 0x0040) == 0x0040)
         flags += "volatile ";
      if ((access & 0x0080) == 0x0080)
         flags += "transient ";
      return flags;
   }

   // print a message on the standard output stream.
   private static void report(String s) {
      System.out.println(s);
   }

   // print the JVM version information for this class file.
   private final void reportVersion() {
      report("\tversion: " + majorVersion + "." + minorVersion);
   };

   // print the access specifiers for this class.
   private final void reportAccessFlags(DataInputStream input)
      throws EOFException, IOException {
      accessFlags = input.readShort();
   }

   private cp_info[] constantPool;
   private short majorVersion;
   private short minorVersion;
   private CONSTANT_Class_info thisClass;
   private CONSTANT_Class_info superClass;
   private short accessFlags;
   private CONSTANT_Class_info[] interfaces;
   private field_info[] fields;
   private method_info[] methods;
   private attribute_info[] attributes;
};
