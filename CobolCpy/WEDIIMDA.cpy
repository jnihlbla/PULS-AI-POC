000010*** EDIT ALLOWED                                                          
000110                                                                          
000120*    EDI IMD GOODS ITEM DETAILS                                           
000130*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000140*                                                                         
000150*    FUNCTION,                                                            
000160*    -TO SPECIFY LINE ITEM - PART DESCRIPTION                             
000170*                                                                         
000180*                                                                         
000190 01  WEDIIMD.                                                             
000191     03 IMD-IDPTYP                             PIC X(3).                  
000192*                                              IMD                        
000193     03 IMD-LENGTH                             PIC 9(3).                  
000194*                                              LENGTH = 259               
000195     03 IMD-7077-PART-DESCR                    PIC X(03).                 
000196*                                              A                          
000197*                                                                         
000198     03 IMD-C273-PART-DESCRIPTION.                                        
000199*                                                                         
000200        05 IMD-7008-PART-DESCRIPTION           PIC X(256).                
000201*       PART DESCR.(25 BYTES)                                             
000202*                                                                         
000530*** END OF VILMAII-COPY LENGTH=265                                        
