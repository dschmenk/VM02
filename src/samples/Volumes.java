//
//  Volumes.java
//  
//
//  Created by Astro on 9/26/08.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//
import apple2.*;
import java.io.*;

public class Volumes
{
        public static void main(String args[]) throws IOException
        {
            String vols[];
            
            System.out.println("Online Volumes:");
            vols = ProDOS.online();
            for (int i = 0; i < vols.length; i++)
                System.out.println(vols[i] + "/");
            System.out.print("\nPress RETURN key...");
            System.in.read();
        }
}
