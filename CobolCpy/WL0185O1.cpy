000100 01  RESP-WL0185O1.                                                       
000200*                                 RESPONS FROM PGM WL0185                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-TIRFSDAT-KEY    PIC 9(6).                                    
000600*                                 KLART FÖR TRANSPORT ÅÅMMDD              
000700     03 RESP-IDPRC-FR-KEY.                                                
000800*                                 PRODUKTIONSKANAL                        
000900        05 RESP-IDPRCBAS     PIC X(3).                                    
001000*                                 PRC-BAS                                 
001100        05 RESP-IDPRCVAR     PIC X.                                       
001200*                                 PRC-VARIANT                             
001300     03 RESP-IDPRC-TO-KEY.                                                
001400*                                 PRODUKTIONSKANAL                        
001500        05 RESP-IDPRCBAS     PIC X(3).                                    
001600*                                 PRC-BAS                                 
001700        05 RESP-IDPRCVAR     PIC X.                                       
001800*                                 PRC-VARIANT                             
001900     03 RESP-KVORDRAD-TOT    PIC 9(5).                                    
002000*                                 ANTAL ORDERRADER                        
002100     03 RESP-KVORDRAD-UTSKR-TOT                                           
002200                             PIC 9(5).                                    
002300*                                 ANTAL UTSKR ORDERRAD                    
002400     03 RESP-REPROCENT-UTSKR-TOT                                          
002500                             PIC 9(3)V9(2).                               
002600*                                 ALLMÄNT PROCENTTALSFÄLT                 
002700     03 RESP-KVORDRAD-PACK-TOT                                            
002800                             PIC 9(5).                                    
002900*                                 ANTAL PACKADE ORDERRADER                
003000     03 RESP-REPROCENT-PACK-TOT                                           
003100                             PIC 9(3)V9(2).                               
003200*                                 ALLMÄNT PROCENTTALSFÄLT                 
003300     03 RESP-KVRADER-MAX1    PIC 9(5).                                    
003400*                                 MAX INDEX KOPPLAT TILL OCCURS N         
003500*                                 EDAN.                                   
003600     03 RESP-KVRADER-MAX2    PIC 9(5).                                    
003700*                                 MAX INDEX KOPPLAT TILL OCCURS N         
003800*                                 EDAN.                                   
003900     03 RESP-TABELLRAD       OCCURS 1 TO 500 TIMES                        
004000                             DEPENDING ON RESP-KVRADER-MAX1.              
004100*                                 GRUPP MED TABELL RADER                  
004200        05 RESP-IDPRC.                                                    
004300*                                 PRODUKTIONSKANAL                        
004400           07 RESP-IDPRCBAS  PIC X(3).                                    
004500*                                 PRC-BAS                                 
004600           07 RESP-IDPRCVAR  PIC X.                                       
004700*                                 PRC-VARIANT                             
004800        05 RESP-KVORDRAD     PIC 9(5).                                    
004900*                                 ANTAL ORDERRADER                        
005000        05 RESP-KVORDRAD-UTSKR                                            
005100                             PIC 9(5).                                    
005200*                                 ANTAL UTSKR ORDERRAD                    
005300        05 RESP-REPROCENT-UTSKR                                           
005400                             PIC 9(3)V9(2).                               
005500*                                 ALLMÄNT PROCENTTALSFÄLT                 
005600        05 RESP-KVORDRAD-PACK                                             
005700                             PIC 9(5).                                    
005800*                                 ANTAL PACKADE ORDERRADER                
005900        05 RESP-REPROCENT-PACK                                            
006000                             PIC 9(3)V9(2).                               
006100*                                 ALLMÄNT PROCENTTALSFÄLT                 
006200     03 RESP-TABELLKLI       OCCURS 1 TO 500 TIMES                        
006300                             DEPENDING ON RESP-KVRADER-MAX2.              
006400*                                 GRUPP MED EJ PACK KOLLIN                
006500        05 RESP-IDPRC-KLI.                                                
006600*                                 PRODUKTIONSKANAL                        
006700           07 RESP-IDPRCBAS  PIC X(3).                                    
006800*                                 PRC-BAS                                 
006900           07 RESP-IDPRCVAR  PIC X.                                       
007000*                                 PRC-VARIANT                             
007100        05 RESP-IDDISTR      PIC 9(4).                                    
007200*                                 DISTRIKTNUMMER                          
007300        05 RESP-IDPRODNR     PIC 9(7).                                    
007400*                                 PRODUKTIONSNUMMER                       
007500        05 RESP-IDKOLLI      PIC 9(5).                                    
007600*                                 KOLLINUMMER                             
007700*** END OF VILMAII-COPY LENGTH= 24551 BYTES                               
