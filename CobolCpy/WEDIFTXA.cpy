000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI FTX, FREE TEXT                                                   
000030*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000031*                                                                         
000032*    FUNCTION,                                                            
000033*    -TO GIVE INFORMATION AND/OR INSTRUCTIONS IN ADDITION                 
000040*                                                                         
000100 01  WEDIFTX.                                                             
000230     03 FTX-IDPTYP                             PIC X(03).                 
000240*                                              FTX                        
000501     03 FTX-LENGTH                             PIC 9(04).                 
000502*                                              LENGTH = 2609              
000503*                                                                         
000515     03 FTX-4451-TEXT-SUB-QUAL                 PIC X(03).                 
000516*                                              COI                        
000517*                                                                         
000518     03 FTX-4453-FREE-TEXT                     PIC X(03).                 
000519*                                                                         
000520     03 FTX-C107-FTX-TEXT-REF.                                            
000521*                                                                         
000522        05 FTX-4441-FREE-TEXT                  PIC X(17).                 
000523        05 FTX-1131-CODE                       PIC X(17).                 
000524        05 FTX-3055-CODE                       PIC X(03).                 
000525*                                                                         
000526     03 FTX-C108-FTX-TEXT-LITERAL.                                        
000527*                                                                         
000528        05 FTX-4440-FREE-TEXT                  PIC X(512).                
000529*          ORDER CLASS (1 BYTE)                                           
000530*                                                                         
000531        05 FTX-4440-FILLER                     PIC X(2048).               
000532*                                                                         
000533     03 FTX-3453-LANGUAGE-NAME                 PIC X(03).                 
000534*                                                                         
000535     03 FTX-4447-FREE-TEXT                     PIC X(03).                 
000540*                                                                         
000572*                                                                         
000580*** END OF VILMAII-COPY LENGTH=2615                                       
