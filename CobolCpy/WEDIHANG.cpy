000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI HAN HANDLING INSTRUCTION                                         
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -TO PROVIDE HANDLING INSTRUCTION                                     
000070*                                                                         
000100 01  WEDIHAN.                                                             
000230     03 HAN-IDPTYP                             PIC X(03).                 
000240*                                              HAN                        
000501     03 HAN-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 89                
000503*                                                                         
000515     03 HAN-C524-HANDL-INSTR.                                             
000516*                                                                         
000515       05 HAN-4079-HANDL-INSTR                 PIC X(03).                 
000516*                                                                         
000517       05 HAN-1131-CODE-LIST-QUAL              PIC X(03).                 
000518*                                                                         
000519       05 HAN-3055-CODE-LIST-RESP              PIC X(03).                 
000520*                                                                         
000515     03 HAN-C218-HAZ-MAT.                                                 
000516*                                                                         
000521       05 HAN-4078-HANDLING-INSTR              PIC X(70).                 
000522*                                                                         
000521       05 HAN-7419-HAZ-MAT-CLASS               PIC X(04).                 
000522*                                                                         
000521       05 HAN-1131-CODE-LIST-QUAL              PIC X(03).                 
000522*                                                                         
000521       05 HAN-3055-CODE-L-RESP-AGENCY          PIC X(03).                 
000522*                                                                         
000530*** END OF VILMAII-COPY LENGTH=92                                         
