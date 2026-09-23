000100 01  MOD-W6O33801.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 6338              
000300*                                 LEVERANS SPÄRR + NOTERING LDC           
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN-ATTR  PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 MOD-IDARTNR-IN       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDARTNR-UT       PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 MOD-IDSKYLT-IN       PIC X(3).                                    
001500*                                 NATIONALITETSTECKEN                     
001600*                                 SPRÅKIDENTIFIKATION                     
001700     03 MOD-IDSKYLT-UT       PIC X(3).                                    
001800*                                 NATIONALITETSTECKEN                     
001900*                                 SPRÅKIDENTIFIKATION                     
002000     03 MOD-BEART            PIC X(25).                                   
002100*                                 ARTIKELBENÄMNING                        
002200     03 MOD-TEARTNOT-GLOB    PIC X(40).                                   
002300*                                 ARTIKEL NOTERING                        
002400     03 MOD-KDLEVSP-GLOB     PIC Z9.                                      
002500*                                 SPÄRRKOD LEVERANS                       
002600     03 MOD-TISPARR-KVAL-GLOB                                             
002700                             PIC 9(6).                                    
002800*                                 SPÄRRAD DATUM KVALITETSFEL              
002900     03 MOD-IDUSER-SPKVAL-GLOB                                            
003000                             PIC X(8).                                    
003100*                                 ANVÄNDAR-ID KVALITETSPÄRR               
003200     03 MOD-KDLEVSP-DC11     PIC Z9.                                      
003300*                                 SPÄRRKOD LEVERANS                       
003400     03 MOD-KVSPARR-KVAL-DC11                                             
003500                             PIC Z(6)9.                                   
003600*                                 SPÄRRAT ANTAL KVALITETSFEL              
003700     03 MOD-TISPARR-KVAL-DC11                                             
003800                             PIC 9(6).                                    
003900*                                 SPÄRRAD DATUM KVALITETSFEL              
004000     03 MOD-IDUSER-SPKVAL-DC11                                            
004100                             PIC X(8).                                    
004200*                                 ANVÄNDAR-ID KVALITETSPÄRR               
004300     03 MOD-KVLS-DC11        PIC -(7)9.                                   
004400*                                 LAGERSALDO                              
004500     03 MOD-RAD              OCCURS 14 TIMES.                             
004600        05 MOD-KDLEVSP       PIC Z9.                                      
004700*                                 SPÄRRKOD LEVERANS                       
004800        05 MOD-KDLEVSP-UPD-ATTR                                           
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-KDLEVSP-UPD   PIC 9(2).                                    
005200*                                 SPÄRRKOD LEVERANS                       
005300        05 MOD-KVSPARR-KVAL  PIC Z(6)9.                                   
005400*                                 SPÄRRAT ANTAL KVALITETSFEL              
005500        05 MOD-KVSPARR-KVAL-UPD-ATTR                                      
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-KVSPARR-KVAL-UPD                                           
005900                             PIC 9(7).                                    
006000*                                 SPÄRRAT ANTAL KVALITETSFEL              
006100        05 MOD-TEKVAL-ATTR   PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-TEKVAL        PIC X(10).                                   
006400*                                 KVALITETSNOTERING SPÄRR                 
006500        05 MOD-TISPARR-KVAL  PIC 9(6).                                    
006600*                                 SPÄRRAD DATUM KVALITETSFEL              
006700        05 MOD-IDUSER-SPKVAL PIC X(8).                                    
006800*                                 ANVÄNDAR-ID KVALITETSPÄRR               
006900        05 MOD-KVLS          PIC -(7)9.                                   
007000*                                 LAGERSALDO                              
007100     03 MOD-TEMFSINF         PIC X(55).                                   
007200*                                 INFORMATIONSMEDDELANDE                  
007300*** END OF VILMAII-COPY LENGTH= 1021 BYTES                                
