000010*** EDIT ALLOWED                                                          
000020                                                                          
000030*    EDI QTY ITEM DETAILS                                                 
000040*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000050*                                                                         
000060*    FUNCTION,                                                            
000070*    -TO SPECIFY QUANTITY UNIT                                            
000090*                                                                         
000100 01  WEDIQTY.                                                             
000210     03 QTY-IDPTYP                             PIC X(03).                 
000211*                                              QTY                        
000212     03 QTY-LENGTH                             PIC 9(03).                 
000213*                                              LENGTH = 046               
000214*                                                                         
000220*                                                                         
000510     03 QTY-6063-QUANTITY-QUALIFIER            PIC X(3).                  
000511*                                              12                         
000512*                                                                         
000513     03 QTY-6060-QUANTITY                      PIC X(35).                 
000516                                                                          
000517     03 QTY-6411-FILLER                        PIC X(8).                  
000518*                                                                         
000530*** END OF VILMAII-COPY LENGTH=52                                         
