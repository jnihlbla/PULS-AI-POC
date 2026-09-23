000010*** EDIT ALLOWED                                                          
000011                                                                          
000012*    EDI FTX FREE TEXT                                                    
000013*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000014*                                                                         
000015*    FUNCTION,                                                            
000016*    -SPECIFYING INFORMATION/TEXT RELATED TO THE MANIFEST                 
000017*    -SPECIFYING INFORMATION/TEXT RELATED TO THE CNI-SEGMENT              
000018*    -SPECIFYING INFORMATION/TEXT RELATED TO THE GOODS ITEM               
000019*                                                                         
000040*                                                                         
000100 01  WEDIFTX3.                                                            
000230     03 FTX3-IDPTYP                            PIC X(3).                  
000240*                                              FTX                        
000501     03 FTX3-LENGTH                            PIC 9(3).                  
000502*                                              LENGTH = 353               
000503*                                                                         
000515     03 FTX3-4451-SUBJECT-QUAL                 PIC X(3).                  
000516*                                                                         
000517     03 FTX3-C107-TEXT-LITERAL.                                           
000518*                                                                         
000519        05 FTX3-44401-FREE-TEXT-1              PIC X(70).                 
000520*                                                                         
000521        05 FTX3-44402-FREE-TEXT-2              PIC X(70).                 
000522*                                                                         
000523        05 FTX3-44403-FREE-TEXT-3              PIC X(70).                 
000524*                                                                         
000525        05 FTX3-44404-FREE-TEXT-4              PIC X(70).                 
000526*                                                                         
000527        05 FTX3-44405-FREE-TEXT-5              PIC X(70).                 
000528*                                                                         
000530*** END OF VILMAII-COPY LENGTH=359                                        
