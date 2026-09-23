000100 01  MOD-W4O70301.                                                        
000200*                                 MOD-COPYTEXT FOR PGM W4070300           
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDDISTR-IN       PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MOD-IDDISTR-UT       PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
002200*                                 KUNDNUMMER                              
002300     03 MOD-KDANMORS-IN      PIC X(2).                                    
002400*                                 ORSAK TILL LEVERANSANMÄRKNING           
002500     03 MOD-KDANMORS-UT      PIC X(2).                                    
002600*                                 ORSAK TILL LEVERANSANMÄRKNING           
002700     03 MOD-TIAAMMDD-FOM-ATTR                                             
002800                             PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-TIAAMMDD-FOM     PIC X(6).                                    
003100*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003200     03 MOD-TIAAMMDD-TOM-ATTR                                             
003300                             PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-TIAAMMDD-TOM     PIC X(6).                                    
003600*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003700     03 MOD-TIAAPP-TOM-ATTR  PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-TIAAPP-TOM       PIC X(4).                                    
004000*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
004100*                                 12 PER ÅR                               
004200     03 MOD-IDNODE-ATTR      PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-IDNODE           PIC X(8).                                    
004500*                                 VTAM NODE-NAMN                          
004600     03 MOD-TEMFSINF         PIC X(55).                                   
004700*                                 INFORMATIONSMEDDELANDE                  
004800*** END OF VILMAII-COPY LENGTH= 177 BYTES                                 
