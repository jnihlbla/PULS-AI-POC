 00100*    CONSTRUCTED: 970415                                                  
 00110*    MODIFIED   : 021105                                                  
000200*    NAME: DELIVERYRECORD FOR FILE WITH DELIVERIES                        
000300*                                                                         
000400*                                                                         
000500 01  PI30INL2.                                                            
000600*                    *** PARTNUMBER                                       
000700   03 PARTNO                                  PIC 9(9).                   
000800*                    *** SUPPLIER NUMBER                                  
000810* THE SUPPNO HAS BEEN MODIFIED TO AN X-FIELD WITHOUT CHANGE IN            
000820*     THE COPYTEXT                                                        
000830*     THE RECEIVING VIR-PROGRAM FOR INLEVERANSER TAKES CARE               
000840*     OF THE CONVERSION                                                   
000900   03 SUPPNO                                  PIC X(5).                   
001200*                    *** DELIVERED QUANTITY                               
001300   03 DELQUANT                                PIC 9(6).                   
002100*** END OF VILMAII-COPY LENGTH= 20 OLD LENGTH= 20                         
