000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI LOC, LOCATION                                                    
000030*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000031*                                                                         
000032*    FUNCTION,                                                            
000033*    -TO IDENTIFY A LOCATION APPLYING TO THE ENTIRE CONSIGNMENT           
000040*                                                                         
000100 01  WEDILOC.                                                             
000230     03 LOC-IDPTYP                             PIC X(03).                 
000240*                                              LOC                        
000250     03 LOC-LENGTH                             PIC 9(03).                 
000260*                                              LENGTH = 041               
000503*                                                                         
000515     03 LOC-3227-PLACE-DEL-QUAL                PIC X(03).                 
000516*                                              7                          
000517*                                                                         
000518     03 LOC-C517-LOC-IDENTIFICATION.                                      
000519*                                                                         
000520        05 LOC-3225-DELIVERY-CENTER            PIC X(35).                 
000521*       WAREHOUSE (2 BYTES)                                               
000522*                                                                         
000523        05 LOC-3055-AGREEMENT                  PIC X(03).                 
000524*                                              ZZZ                        
000525*                                                                         
000573*** END OF VILMAII-COPY LENGTH=47                                         
