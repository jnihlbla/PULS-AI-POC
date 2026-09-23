000100 01  REQU-W4I34701.                                                       
000200*                                 REQU-COPYTEXT FÖR PROGRAM 4347          
000300*                                                                         
000400     03 REQU-IDDISTR-KEY     PIC X(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 REQU-IDKUNDNR-KEY    PIC X(6).                                    
000700*                                 KUNDNUMMER                              
000800     03 REQU-IDORDNR-KEY     PIC X(5).                                    
000900*                                 ORDERNUMMER UTGÅR PD90                  
001000     03 REQU-IDPRODNR-KEY    PIC X(7).                                    
001100*                                 PRODUKTIONSNUMMER                       
001200     03 REQU-IDDC-KEY        PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 REQU-FLTRPTCHG       PIC X.                                       
001500*                                 SKA TRANSPORTNUMMER ÄNDRAS?             
001600*                                                                         
001700     03 REQU-IDTRPTNR        PIC X(3).                                    
001800*                                 TRANSPORTIDENTITET                      
001900     03 REQU-KDFRAKT         PIC X(2).                                    
002000*                                 FRAKTSÄTT DC TILL KUND                  
002100     03 REQU-IDPRODNR-START  PIC 9(7).                                    
002200*                                 PRODUKTIONSNUMMER                       
002300     03 REQU-IDKOLLI-START   PIC X(5).                                    
002400*                                 KOLLINUMMER                             
002500     03 REQU-KVRADER         PIC 9(5).                                    
002600*                                 ANTAL RADER                             
002700     03 REQU-RAD             OCCURS 500 TIMES.                            
002800*                                 RADER MED ALLA TRANSPORTER I EN         
002900*                                 ORDER.                                  
003000        05 REQU-VALFLAGGA-LINE                                            
003100                             PIC X.                                       
003200*                                 ALLMÄN FLAGGA                           
003300        05 REQU-IDKOLLI-LINE PIC X(5).                                    
003400*                                 KOLLINUMMER                             
003500        05 REQU-IDTRPTNR-LINE                                             
003600                             PIC X(3).                                    
003700*                                 TRANSPORTIDENTITET                      
003800        05 REQU-KDFRAKT-LINE PIC X(2).                                    
003900*                                 FRAKTSÄTT DC TILL KUND                  
004000*** END OF VILMAII-COPY LENGTH= 5547 BYTES                                
