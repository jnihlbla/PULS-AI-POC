000100 01  MOD-W4O34701.                                                        
000200*                                 MOD FOR PROGRAM 4347                    
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MOD-IDORDNR-IN       PIC X(5).                                    
001200*                                 ORDERNUMMER UTGÅR PD90                  
001300     03 MOD-IDPRODNR-IN      PIC X(7).                                    
001400*                                 PRODUKTIONSNUMMER                       
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDISTR-UT       PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 MOD-IDORDNR-UT       PIC X(5).                                    
002200*                                 ORDERNUMMER UTGÅR PD90                  
002300     03 MOD-IDPRODNR-UT      PIC X(7).                                    
002400*                                 PRODUKTIONSNUMMER                       
002500     03 MOD-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-FLTRPTCHG-ATTRIB PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-FLTRPTCHG        PIC X.                                       
003000*                                 SKA TRANSPORTNUMMER ÄNDRAS?             
003100*                                                                         
003200     03 MOD-IDTRPTNR-ATTRIB  PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-IDTRPTNR         PIC X(3).                                    
003500*                                 TRANSPORTIDENTITET                      
003600     03 MOD-KDFRAKT-ATTRIB   PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-KDFRAKT          PIC X(2).                                    
003900*                                 FRAKTSÄTT DC TILL KUND                  
004000     03 MOD-RAD              OCCURS 12 TIMES.                             
004100*                                 RADER MED ALLA TRANSPORTER I EN         
004200*                                 ORDER.                                  
004300        05 MOD-VALFLAGGA-ATTRIB                                           
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 MOD-VALFLAGGA     PIC X.                                       
004700*                                 ALLMÄN FLAGGA                           
004800        05 MOD-IDKOLLI-ATTRIB                                             
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-IDKOLLI       PIC Z(4)9.                                   
005200*                                 KOLLINUMMER                             
005300        05 MOD-IDTRPTNR-OLD-ATTRIB                                        
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-IDTRPTNR-OLD  PIC Z(2)9.                                   
005700*                                 TRANSPORTIDENTITET                      
005800        05 MOD-IDTRPTNR-IN-ATTRIB                                         
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-IDTRPTNR-IN   PIC X(3).                                    
006200*                                 TRANSPORTIDENTITET                      
006300        05 MOD-KDFRAKT-OLD-ATTRIB                                         
006400                             PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-KDFRAKT-OLD   PIC Z9.                                      
006700*                                 FRAKTSÄTT DC TILL KUND                  
006800        05 MOD-KDFRAKT-IN-ATTRIB                                          
006900                             PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100        05 MOD-KDFRAKT-IN    PIC X(2).                                    
007200*                                 FRAKTSÄTT DC TILL KUND                  
007300     03 MOD-TEMFSINF         PIC X(55).                                   
007400*                                 INFORMATIONSMEDDELANDE                  
007500*** END OF VILMAII-COPY LENGTH= 495 BYTES                                 
