000100 01  MOD-W4O65301.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 4653              
000300*                                 ORDER PLNNNING PER TRANSPORT            
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MOD-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MOD-TIRFSDAT-IN      PIC 9(6).                                    
001300*                                 KLART FÖR TRANSPORT ÅÅMMDD              
001400     03 MOD-TIRFSDAT-UT      PIC 9(6).                                    
001500*                                 KLART FÖR TRANSPORT ÅÅMMDD              
001600     03 MOD-IDTRPLOS-FR-IN   PIC X(3).                                    
001700*                                 TRANSPORTLÖSNING                        
001800     03 MOD-IDTRPLOS-FR-UT   PIC X(3).                                    
001900*                                 TRANSPORTLÖSNING                        
002000     03 MOD-IDTRPLOS-TO-IN   PIC X(3).                                    
002100*                                 TRANSPORTLÖSNING                        
002200     03 MOD-IDTRPLOS-TO-UT   PIC X(3).                                    
002300*                                 TRANSPORTLÖSNING                        
002400     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
002500*                                 GRUPP MED TABELL RADER                  
002600        05 MOD-IDTRPLOS      PIC X(3).                                    
002700*                                 TRANSPORTLÖSNING                        
002800        05 MOD-KVORDRAD      PIC Z(4)9.                                   
002900*                                 ANTAL ORDERRADER                        
003000        05 MOD-KVORDRAD-UTSKR                                             
003100                             PIC Z(4)9.                                   
003200*                                 ANTAL UTSKR ORDERRAD                    
003300        05 MOD-REPROCENT-UTSKR                                            
003400                             PIC Z(2)9.                                   
003500*                                 ALLMÄNT PROCENTTALSFÄLT                 
003600        05 MOD-KVORDRAD-PACK PIC Z(4)9.                                   
003700*                                 ANTAL PACKADE ORDERRADER                
003800        05 MOD-REPROCENT-PACK                                             
003900                             PIC Z(2)9.                                   
004000*                                 ALLMÄNT PROCENTTALSFÄLT                 
004100        05 MOD-KVKOLLI       PIC Z(3)9.                                   
004200*                                 ANTAL KOLLI                             
004300        05 MOD-FILLERX1      PIC X.                                       
004400        05 MOD-KVKOLLI-LAST  PIC Z(3)9.                                   
004500*                                 ANTAL LASTNINGSRAPPORTERADE             
004600*                                 KOLLIN                                  
004700        05 MOD-REPROCENT-LAST                                             
004800                             PIC Z(2)9.                                   
004900*                                 ALLMÄNT PROCENTTALSFÄLT                 
005000        05 MOD-KVKOLLI-FAKT  PIC Z(3)9.                                   
005100*                                 ANTAL FAKTURERADE KOLLIN                
005200        05 MOD-REPROCENT-FAKT                                             
005300                             PIC Z(2)9.                                   
005400*                                 ALLMÄNT PROCENTTALSFÄLT                 
005500     03 MOD-FILLERX5-TOT     PIC X(5).                                    
005600     03 MOD-KVORDRAD-TOT     PIC Z(4)9.                                   
005700*                                 ANTAL ORDERRADER                        
005800     03 MOD-KVORDRAD-UTSKR-TOT                                            
005900                             PIC Z(4)9.                                   
006000*                                 ANTAL UTSKR ORDERRAD                    
006100     03 MOD-REPROCENT-UTSKR-TOT                                           
006200                             PIC Z(2)9.                                   
006300*                                 ALLMÄNT PROCENTTALSFÄLT                 
006400     03 MOD-KVORDRAD-PACK-TOT                                             
006500                             PIC Z(4)9.                                   
006600*                                 ANTAL PACKADE ORDERRADER                
006700     03 MOD-REPROCENT-PACK-TOT                                            
006800                             PIC Z(2)9.                                   
006900*                                 ALLMÄNT PROCENTTALSFÄLT                 
007000     03 MOD-KVKOLLI-TOT      PIC Z(3)9.                                   
007100*                                 ANTAL KOLLI                             
007200     03 MOD-FILLERX1-TOT     PIC X.                                       
007300     03 MOD-KVKOLLI-LAST-TOT PIC Z(3)9.                                   
007400*                                 ANTAL LASTNINGSRAPPORTERADE             
007500*                                 KOLLIN                                  
007600     03 MOD-REPROCENT-LAST-TOT                                            
007700                             PIC Z(2)9.                                   
007800*                                 ALLMÄNT PROCENTTALSFÄLT                 
007900     03 MOD-KVKOLLI-FAKT-TOT PIC Z(3)9.                                   
008000*                                 ANTAL FAKTURERADE KOLLIN                
008100     03 MOD-REPROCENT-FAKT-TOT                                            
008200                             PIC Z(2)9.                                   
008300*                                 ALLMÄNT PROCENTTALSFÄLT                 
008400     03 MOD-TEMFSINF         PIC X(55).                                   
008500*                                 INFORMATIONSMEDDELANDE                  
008600*** END OF VILMAII-COPY LENGTH= 688 BYTES                                 
