000100 01  MID-W4I79401.                                                        
000200*                                 MIDCOPYTEXT TILL W40794.                
000300     03 MID-IDPRTLST         PIC X(8).                                    
000400*                                 LOGISK PRINTER+LISTA IDENTITET          
000500*                                 LOGICAL PRINTER+LIST IDENTITY           
000600     03 MID-IDPGM            PIC X(8).                                    
000700*                                 PROGRAM IDENTITET                       
000800*                                 PROGRAM INTENTITY                       
000900     03 MID-IDDC             PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 MID-KVPOST           PIC 9(7).                                    
001300*                                 RÄKNARE, ANTAL POSTER                   
001400*                                 RECORD COUNTER                          
001500     03 MID-RT-POST          OCCURS 13 TIMES.                             
001600        05 MID-IDDISTR       PIC X(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800*                                 DISTRICT NUMBER                         
001900        05 MID-IDKUNDNR      PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100*                                 CUSTOMER NO                             
002200        05 MID-IDRAPPNR      PIC X(7).                                    
002300*                                 RAPPORT NUMMER                          
002400*                                 DISCREPANCY REPORT NUMBER               
002500*** END OF VILMAII-COPY LENGTH= 246 BYTES                                 
