000100 01  MOD-W4O51201.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W4O51201                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-IDDC-IN          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MOD-IDDC-UT          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 MOD-KDORDKL-IN       PIC X.                                       
002100*                                 ORDERKLASS                              
002200     03 MOD-KDORDKL-UT       PIC X.                                       
002300*                                 ORDERKLASS                              
002400     03 MOD-KDFRAKT-IN       PIC X(2).                                    
002500*                                 FRAKTSÄTT DC TILL KUND                  
002600     03 MOD-KDFRAKT-UT       PIC X(2).                                    
002700*                                 FRAKTSÄTT DC TILL KUND                  
002800     03 MOD-KDORDSTA-IN      PIC X.                                       
002900*                                 VOLVOORDERSTATUS                        
003000     03 MOD-KDORDSTA-UT      PIC X.                                       
003100*                                 VOLVOORDERSTATUS                        
003200     03 MOD-IDTRANS-RAD      OCCURS 14 TIMES                              
003300                             PIC X(4).                                    
003400*                                 BILDNUMMER                              
003500     03 MOD-IDKUNDNR         OCCURS 14 TIMES                              
003600                             PIC Z(5)9.                                   
003700*                                 KUNDNUMMER                              
003800     03 MOD-IDORDNR7         OCCURS 14 TIMES                              
003900                             PIC Z(6)9.                                   
004000*                                 ORDERNUMMER                             
004100     03 MOD-IDDC             OCCURS 14 TIMES                              
004200                             PIC X(2).                                    
004300*                                 IDENTIFIERARE LAGER                     
004400     03 MOD-IDPRODNR         OCCURS 14 TIMES                              
004500                             PIC Z(6)9.                                   
004600*                                 PRODUKTIONSNUMMER                       
004700     03 MOD-KDFRAKT          OCCURS 14 TIMES                              
004800                             PIC Z9.                                      
004900*                                 FRAKTSÄTT DC TILL KUND                  
005000     03 MOD-KDORDKL          OCCURS 14 TIMES                              
005100                             PIC 9.                                       
005200*                                 ORDERKLASS                              
005300     03 MOD-KDORDSTA         OCCURS 14 TIMES                              
005400                             PIC X(2).                                    
005500*                                 VOLVOORDERSTATUS                        
005600     03 MOD-KVORDRAD-REG     OCCURS 14 TIMES                              
005700                             PIC Z(4)9.                                   
005800*                                 ANTAL REG ORDERRADER                    
005900     03 MOD-KVORDRAD-UTSKR   OCCURS 14 TIMES                              
006000                             PIC Z(4)9.                                   
006100*                                 ANTAL UTSKR ORDERRAD                    
006200     03 MOD-KVORDRAD-PACK    OCCURS 14 TIMES                              
006300                             PIC Z(4)9.                                   
006400*                                 ANTAL PACKADE ORDERRADER                
006500     03 MOD-TEMFSINF         PIC X(55).                                   
006600*                                 INFORMATIONSMEDDELANDE                  
006700*** END OF VILMAII-COPY LENGTH= 775 BYTES                                 
