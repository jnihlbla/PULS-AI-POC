000100 01  RESP-W60135O1.                                                       
000200*                                 RESP-COPYTEXT FÖR W6013500              
000300     03 RESP-IDDC            PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 RESP-IDLOPNRM        PIC X(9).                                    
000700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000800*                                 (0VVDLLLLK)                             
000900*                                 SERIAL NO RECEIVING REPORT              
001000*                                 (0WWDLLLLC)                             
001100     03 RESP-FLGODK-ATTR     PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 RESP-FLGODK          PIC X.                                       
001400     03 RESP-ADINLOMR-PRT    PIC X(4).                                    
001500*                                 PRINTERPLACERING                        
001600*                                 PLACE OF A PRINTER                      
001700     03 RESP-RAD             OCCURS 14 TIMES.                             
001800        05 RESP-KOL-GRP      OCCURS 2 TIMES.                              
001900           07 RESP-IDLEVNR-KOLLI-ATTR                                     
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200           07 RESP-IDLEVNR-KOLLI                                          
002300                             PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER KOLLI                  
002500*                                 SUPPLIER NUMBER CASE                    
002600           07 RESP-IDOKOLLI-ATTR                                          
002700                             PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900           07 RESP-IDOKOLLI  PIC X(9).                                    
003000*                                 ODETTE KOLLINUMMER                      
003100*                                 ODETTE CASE NUMBER                      
003200*** END OF VILMAII-COPY LENGTH= 522 BYTES                                 
