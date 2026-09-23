000100 01  MOD-W4O65401.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 4654              
000300*                                 ORDER PLNNNING PER PRC                  
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
001600     03 MOD-IDPRC-FR-IN.                                                  
001700*                                 PRODUKTIONSKANAL                        
001800        05 MOD-IDPRCBAS      PIC X(3).                                    
001900*                                 PRC-BAS                                 
002000        05 MOD-IDPRCVAR      PIC X.                                       
002100*                                 PRC-VARIANT                             
002200     03 MOD-IDPRC-FR-UT.                                                  
002300*                                 PRODUKTIONSKANAL                        
002400        05 MOD-IDPRCBAS      PIC X(3).                                    
002500*                                 PRC-BAS                                 
002600        05 MOD-IDPRCVAR      PIC X.                                       
002700*                                 PRC-VARIANT                             
002800     03 MOD-IDPRC-TO-IN.                                                  
002900*                                 PRODUKTIONSKANAL                        
003000        05 MOD-IDPRCBAS      PIC X(3).                                    
003100*                                 PRC-BAS                                 
003200        05 MOD-IDPRCVAR      PIC X.                                       
003300*                                 PRC-VARIANT                             
003400     03 MOD-IDPRC-TO-UT.                                                  
003500*                                 PRODUKTIONSKANAL                        
003600        05 MOD-IDPRCBAS      PIC X(3).                                    
003700*                                 PRC-BAS                                 
003800        05 MOD-IDPRCVAR      PIC X.                                       
003900*                                 PRC-VARIANT                             
004000     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
004100*                                 GRUPP MED TABELL RADER                  
004200        05 MOD-IDPRC.                                                     
004300*                                 PRODUKTIONSKANAL                        
004400           07 MOD-IDPRCBAS   PIC X(3).                                    
004500*                                 PRC-BAS                                 
004600           07 MOD-IDPRCVAR   PIC X.                                       
004700*                                 PRC-VARIANT                             
004800        05 MOD-KVORDRAD      PIC Z(4)9.                                   
004900*                                 ANTAL ORDERRADER                        
005000        05 MOD-KVORDRAD-UTSKR                                             
005100                             PIC Z(4)9.                                   
005200*                                 ANTAL UTSKR ORDERRAD                    
005300        05 MOD-REPROCENT-UTSKR                                            
005400                             PIC Z(2)9.                                   
005500*                                 ALLMÄNT PROCENTTALSFÄLT                 
005600        05 MOD-KVORDRAD-PACK PIC Z(4)9.                                   
005700*                                 ANTAL PACKADE ORDERRADER                
005800        05 MOD-REPROCENT-PACK                                             
005900                             PIC Z(2)9.                                   
006000*                                 ALLMÄNT PROCENTTALSFÄLT                 
006100     03 MOD-KVORDRAD-TOT     PIC Z(4)9.                                   
006200*                                 ANTAL ORDERRADER                        
006300     03 MOD-KVORDRAD-UTSKR-TOT                                            
006400                             PIC Z(4)9.                                   
006500*                                 ANTAL UTSKR ORDERRAD                    
006600     03 MOD-REPROCENT-UTSKR-TOT                                           
006700                             PIC Z(2)9.                                   
006800*                                 ALLMÄNT PROCENTTALSFÄLT                 
006900     03 MOD-KVORDRAD-PACK-TOT                                             
007000                             PIC Z(4)9.                                   
007100*                                 ANTAL PACKADE ORDERRADER                
007200     03 MOD-REPROCENT-PACK-TOT                                            
007300                             PIC Z(2)9.                                   
007400*                                 ALLMÄNT PROCENTTALSFÄLT                 
007500     03 MOD-TEMFSINF         PIC X(55).                                   
007600*                                 INFORMATIONSMEDDELANDE                  
007700*** END OF VILMAII-COPY LENGTH= 452 BYTES                                 
