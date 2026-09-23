000100 01  MID-W6I19A01.                                                        
000200*                                 MIDCOPYTEXT TILL W6019A.                
000300     03 MID-IDPRTLST         PIC X(8).                                    
000400*                                 LOGISK PRINTER+LISTA IDENTITET          
000500     03 MID-IDPGM            PIC X(8).                                    
000600*                                 PROGRAM IDENTITET                       
000700     03 MID-KVPOST           PIC 9(7).                                    
000800*                                 RÄKNARE, ANTAL POSTER                   
000900     03 MID-PLATSSAETT-POST  OCCURS 12 TIMES.                             
001000        05 MID-IDARTNR       PIC 9(8).                                    
001100*                                 ARTIKELNUMMER                           
001200        05 MID-IDLEVNR-KOLLI PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER KOLLI                  
001400        05 MID-IDOKOLLI      PIC 9(9).                                    
001500*                                 ODETTE KOLLINUMMER                      
001600        05 MID-IDINLVGN      PIC 9(3).                                    
001700*                                 VAGNSIDENTITET                          
001800        05 MID-ADINLOMR      PIC X(4).                                    
001900*                                 INLEVERANSOMRÅDE                        
002000        05 MID-IDILIST       PIC 9(5).                                    
002100*                                 INLÄGGNINGSLISTEIDENTITET               
002200        05 MID-KVRADER       PIC 9(5).                                    
002300*                                 ANTAL RADER                             
002400*** END OF VILMAII-COPY LENGTH= 491 BYTES                                 
