/*****************************************************************************************************************************************
** Filename: FileProc.scala
** Author: Bryant Hall
** Date: 8/29/2016
** Description: This program goes through a directory and counts up the occurence of each unique word in text files. It recurses through
directories and opens up zip files looking for text files. It then presents the results of this count to console and 
a output text file.  Requires a single argument which calls out the complete path of the folder to test. 
******************************************************************************************************************************************/

import java.io._
import scala.io.Source
import scala.collection.mutable.Map;
import java.io.File
import java.util.zip;

import java.util.Arrays;
import java.util.zip.ZipInputStream;


import sys.process._

object HelloWorld {
	//main entry point for program. It will check the input arguments and exit if incorrect number are supplied. Then analyze the contents of the 
	//directory and print the results out to the screen
	//Mana sucks
	def main(args: Array[String]) {
		//Check number of arguments, exit if wrong number supplied		
		if(args.length !=1){
			println("Usage: scala FileProc.scala <pathname>");
			System.exit(1);
		}     
      
		//declare map to store word count results 
		var results:Map[Int,Int] = Map()
      
		//get input path from commandline arguments and pass to process directory
	        val inputPath = args(0);  
	        processDirectory(inputPath, results);     		
		printresults(results);       
 	}
      

	//prints results for this program.  A map is passed into this function, if it has a entry, it will print results to console and to results.txt
	def printresults(results:Map[Int,Int]){
		if(results.size == 0){
			println("No text files found in directory");
			return
		}

		val writer = new PrintWriter(new File("Results.txt"))
		
        	//Print Out Results, sort them by key
		for( (key,value)  <- results.toSeq.sortBy(_._1)){
			try{
				//conditional grammer for number of entries
				if(value == 1){
					writer.write(value + " file has " + key + " word");
					print(value + " file has " + key + " word")
				}
				else{
					writer.write(value + " files have " + key + " word")
					print(value + " files have " + key + " word")
				}
				if(key >1){
					writer.write("s\n");
					println("s");
				}else{
					writer.write("\n");
					print("\n");
				}
			}catch{
				case ex: IOException => {
					println(ex);
					println("Unable to write results, terminating");
					System.exit(1);
				}
			}
		}
		println("Results May be found in Results.txt");
		writer.close();
	}
	
   	//Method used to unzip main folder. Takes in a target directory of zip file and uses Zip Input stream to unzip contents of files into temp directory that can be processed
	def unzipDirectory (inputPath:String) {
		//setup outputpath for unzipped files
		val outputPath = inputPath.dropRight(4);
		val endDirectory = new File(outputPath);
		
		//create new directory if it doesn't exist
		if(!(endDirectory.exists())){
			endDirectory.mkdir();
		}

		//open up stream for reading in inputfiles
		try{
			val ZipIO = new ZipInputStream( new FileInputStream(inputPath));
			var entry = ZipIO.getNextEntry();
			
			//Go through contents of zip folder. For each entry copy results over from zip folder to temp folder that was created
			//in directory
			while(entry != null){
				val fileName = entry.getName();
				val fp = outputPath + File.separator + fileName.substring(fileName.lastIndexOf("/")+1, fileName.length);
	
				//if a directory, create new directory for file
				if(entry.isDirectory){
					val dir = new File(fp);
					val result = dir.mkdir();
				}
		
				//if a file write contents to drive
				else{
					try{
						val zipout = new BufferedOutputStream(new FileOutputStream(fp));
						val  bytesin = Array.fill[Byte](2048)(0);
						var read =0;	
						//Write out contents of file				
						while({read = ZipIO.read(bytesin).toInt; read!= -1}){
							zipout.write(bytesin,0, read);
						}
						zipout.close();
					}catch{ 
						case ioe: IOException =>{
							println(ioe.toString());
							println("Could not find " + fp);
						}
					}
				}
				entry=ZipIO.getNextEntry();
			}
		}
		catch{
			case ex: IOException => {
				println(ex.toString());
				println("Unable to open zip file: " + inputPath + ".");
				return; 
			}
		}
	}

   	//Main method used to process directory contents. First gets every text file in main directory, then it decompresses zip files into temp folder, reads contents and then deletes temp folder. Finally calls it self recursively on any 
	//any directories present
	def processDirectory(inputPath:String, results: Map[Int, Int]){
        	//get every text file      
		val files = getListOfFiles(inputPath, List("txt"));     

	        //Get values from text files  in directory in a map. Use a regex expression to filter for only words, then group toghether.  Combine map with cumulative map results. 
		for( file <-files){
		       val list1 = Source.fromFile(file).getLines().flatMap(_.split("\\W+")).toList;
		       updateList(results, list1.size);              
		}

		//get every directory, call this function recursively to get all files in child folders
		val directories = getDirectories(inputPath)
		for( dirPath <-directories){
		        processDirectory(dirPath.toString,results);
		}
      
		//get every zip file, store unzipped file in new temp directory. Then call process directory on this new folder. Then call system call to recursively delete file. 
		val zipFiles  = getListOfFiles(inputPath,List("zip"));
		for(zipPath <- zipFiles){
			unzipDirectory(zipPath.getPath);
			val path = zipPath.getPath.dropRight(4);
			processDirectory(path, results);
			//remove temp directory after processing directory contents using unix command
			val result = "rm -r -f " + path !!;
		}
   	}
   
   
	//merge results from new file into results map. 
	def updateList(map1: Map[Int, Int], filesize: Int ): Map[Int,Int] = {
		//check if map2 contains values, add if true, otherwise add entry
	       	if(map1.contains(filesize)){
		        map1(filesize) += 1;      
	       	}
	       	else{
	        	map1 += (filesize -> 1);
	       	}
        return map1;
	}
	   

	//Function that returns a list of directories present in a folder. If no files present, return a empty list
	def getDirectories(dir:String): List[File]= {
       		val myDirectory = new File(dir)
       		if(myDirectory.isDirectory){
        		myDirectory.listFiles.filter(_.isDirectory).toList
	        }
        	//no directories, return empty list
		else{
			println("No Directories");           		
			List()
	       }
	}

	//functions gets list of files from a directory and returns the results.  User passes in a parameter specifying
	//which type of file function will end with. If none of the files are found, will return a empty list   
    	def getListOfFiles(dir: String, extensions: List[String]): List[File] = {
		val d = new File(dir)
		//check valid directory was provided, otherwise return empty list
		if (d.exists && d.isDirectory) {
			d.listFiles.filter { file => extensions.exists(file.getName.endsWith(_));}.toList;
        	} else {
			List[File]()
        	}
    	}
}
  
