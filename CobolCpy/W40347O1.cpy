000100 01  RESP-W40347O1.                                                       
000200*                                 RESP-COPYTEXT FOR PROGRAM 4347          
000300*                                                                         
000400     03 RESP-IDPRODNR-START  PIC 9(7).                                    
000500*                                 PRODUKTIONSNUMMER                       
000600     03 RESP-IDKOLLI-START   PIC X(5).                                    
000700*                                 KOLLINUMMER                             
000800     03 RESP-IDKOLLI-NEXT    PIC X(5).                                    
000900*                                 KOLLINUMMER                             
001000     03 RESP-IDDISTR-UT      PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 RESP-IDKUNDNR-UT     PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 RESP-IDORDNR-UT      PIC X(5).                                    
001500*                                 ORDERNUMMER UTGÅR PD90                  
001600     03 RESP-IDPRODNR-UT     PIC X(7).                                    
001700*                                 PRODUKTIONSNUMMER                       
001800     03 RESP-FLTRPTCHG-ATTRIB                                             
001900                             PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 RESP-FLTRPTCHG       PIC X.                                       
002200*                                 SKA TRANSPORTNUMMER ÄNDRAS?             
002300*                                                                         
002400     03 RESP-IDTRPTNR-ATTRIB PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 RESP-IDTRPTNR        PIC X(3).                                    
002700*                                 TRANSPORTIDENTITET                      
002800     03 RESP-KDFRAKT-ATTRIB  PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 RESP-KDFRAKT         PIC X(2).                                    
003100*                                 FRAKTSÄTT DC TILL KUND                  
003200     03 RESP-KVRADER         PIC 9(5).                                    
003300*                                 ANTAL RADER                             
003400     03 RESP-RAD             OCCURS 500 TIMES.                            
003500*                                 RADER MED ALLA TRANSPORTER I EN         
003600*                                 ORDER.                                  
003700        05 RESP-VALFLAGGA-LINE-ATTR                                       
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 RESP-VALFLAGGA-LINE                                            
004100                             PIC X.                                       
004200*                                 ALLMÄN FLAGGA                           
004300        05 RESP-IDKOLLI-LINE-ATTR                                         
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 RESP-IDKOLLI-LINE PIC Z(4)9.                                   
004700*                                 KOLLINUMMER                             
004800        05 RESP-IDTRPTNR-OLD-ATTR                                         
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 RESP-IDTRPTNR-OLD PIC Z(2)9.                                   
005200*                                 TRANSPORTIDENTITET                      
005300        05 RESP-IDTRPTNR-LINE-ATTR                                        
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 RESP-IDTRPTNR-LINE                                             
005700                             PIC X(3).                                    
005800*                                 TRANSPORTIDENTITET                      
005900        05 RESP-KDFRAKT-OLD-ATTR                                          
006000                             PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 RESP-KDFRAKT-OLD  PIC Z9.                                      
006300*                                 FRAKTSÄTT DC TILL KUND                  
006400        05 RESP-KDFRAKT-LINE-ATTR                                         
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700        05 RESP-KDFRAKT-LINE PIC X(2).                                    
006800*                                 FRAKTSÄTT DC TILL KUND                  
006900*** END OF VILMAII-COPY LENGTH= 14056 BYTES                               
