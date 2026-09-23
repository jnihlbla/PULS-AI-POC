000100 01  MID-W6I19901.                                                        
000200*                                 MIDCOPYTEXT TILL W60199.                
000300     03 MID-IDPRTLST         PIC X(8).                                    
000400*                                 LOGISK PRINTER+LISTA IDENTITET          
000500     03 MID-IDPGM            PIC X(8).                                    
000600*                                 PROGRAM IDENTITET                       
000700     03 MID-KVPOST           PIC 9(7).                                    
000800*                                 RÄKNARE, ANTAL POSTER                   
000900     03 MID-ILISTE-POST      OCCURS 12 TIMES.                             
001000        05 MID-IDLEVNR-KOLLI PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER KOLLI                  
001200        05 MID-IDOKOLLI      PIC 9(9).                                    
001300*                                 ODETTE KOLLINUMMER                      
001400        05 MID-IDINLVGN      PIC 9(3).                                    
001500*                                 VAGNSIDENTITET                          
001600        05 MID-ADINLOMR      PIC X(4).                                    
001700*                                 INLEVERANSOMRÅDE                        
001800        05 MID-ADLAGOMR      PIC 9(2).                                    
001900*                                 LAGEROMRÅDE                             
002000        05 MID-ADINLOMR-TORG PIC X(4).                                    
002100*                                 INLEVERANSOMRÅDE                        
002200        05 MID-IDILIST       PIC 9(5).                                    
002300*                                 INLÄGGNINGSLISTEIDENTITET               
002400        05 MID-KVRADER       PIC 9(5).                                    
002500*                                 ANTAL RADER                             
002600*** END OF VILMAII-COPY LENGTH= 467 BYTES                                 
