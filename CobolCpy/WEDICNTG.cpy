000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI CNT CONTROL TOTAL                                                
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -TO PROVIDE MESSAGE CONTROL TOTAL                                    
000070*                                                                         
000100 01  WEDICNT.                                                             
000230     03 CNT-IDPTYP                             PIC X(03).                 
000240*                                              CNT                        
000501     03 CNT-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 24                
000503*                                                                         
000515     03 CNT-C270-CONTROL.                                                 
000516*                                                                         
000517        05 CNT-6069-CONTR-QUAL                 PIC X(03).                 
000518*                                                                         
000519        05 CNT-6066-CONTR-VALUE                PIC 9(18).                 
000520*                                                                         
000521        05 CNT-6411-MEA-UNIT-Q                 PIC X(03).                 
000522*                                                                         
000530*** END OF VILMAII-COPY LENGTH=30                                         
