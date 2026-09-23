000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI CNT CONTROL TOTAL                                                
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -TO SPECIFY TOTALE OF THE MANIFEST                                   
000070*                                                                         
000100 01  WEDICNT3.                                                            
000230     03 CNT3-IDPTYP                            PIC X(03).                 
000240*                                              CNT                        
000501     03 CNT3-LENGTH                            PIC 9(03).                 
000502*                                              LENGTH = 24                
000503*                                                                         
000515     03 CNT3-C270-CONTROL.                                                
000516*                                                                         
000517        05 CNT3-6069-CONTROL-QUAL              PIC X(03).                 
000518*                                                                         
000519        05 CNT3-6066-CONTROL-VALUE             PIC 9(15)V9(03).           
000520*                                                                         
000521        05 CNT3-6411-MESSURE-UNIT              PIC X(03).                 
000522*                                                                         
000530*** END OF VILMAII-COPY LENGTH=30                                         
