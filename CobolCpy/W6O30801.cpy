000100 01  MOD-W6O30801.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 6308              
000300*                                 LEVERANS SPÄRR + NOTERING               
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
002000     03 MOD-IDDC-IN          PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 MOD-IDDC-UT          PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400     03 MOD-SHOW-IN          PIC X.                                       
002500     03 MOD-SHOW-UT          PIC X.                                       
002600     03 MOD-TYPE-IN          PIC X.                                       
002700     03 MOD-TYPE-UT          PIC X.                                       
002800     03 MOD-BEART            PIC X(25).                                   
002900*                                 ARTIKELBENÄMNING                        
003000     03 MOD-FLMAIL-ATTR      PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-FLMAIL           PIC X.                                       
003300*                                 ALLMÄN FLAGGA                           
003400     03 MOD-TIMAIL-KVAL      PIC 9(6).                                    
003500*                                 MAIL DATUM                              
003600     03 MOD-KDLEVSP-GLOB     PIC Z9.                                      
003700*                                 SPÄRRKOD LEVERANS                       
003800     03 MOD-KDLEVSP-GLOB-UPD-ATTR                                         
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-KDLEVSP-GLOB-UPD PIC Z9.                                      
004200*                                 SPÄRRKOD LEVERANS                       
004300     03 MOD-TEARTNOT-GLOB-ATTR                                            
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-TEARTNOT-GLOB    PIC X(25).                                   
004700     03 MOD-FLTEXT           PIC X.                                       
004800*                                 ALLMÄN FLAGGA                           
004900     03 MOD-IDDC-DC11        PIC X(2).                                    
005000*                                 IDENTIFIERARE LAGER                     
005100     03 MOD-KDLEVSP-DC11     PIC Z9.                                      
005200*                                 SPÄRRKOD LEVERANS                       
005300     03 MOD-KDLEVSP-DC11-UPD-ATTR                                         
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-KDLEVSP-DC11-UPD PIC Z9.                                      
005700*                                 SPÄRRKOD LEVERANS                       
005800     03 MOD-KVSPARR-KVAL-DC11                                             
005900                             PIC Z(6)9.                                   
006000*                                 SPÄRRAT ANTAL KVALITETSFEL              
006100     03 MOD-KVSPARR-KVAL-DC11-UPD-ATTR                                    
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-KVSPARR-KVAL-DC11-UPD                                         
006500                             PIC Z(6)9.                                   
006600*                                 SPÄRRAT ANTAL KVALITETSFEL              
006700     03 MOD-TISPARR-KVAL-DC11                                             
006800                             PIC 9(6).                                    
006900*                                 SPÄRRAD DATUM KVALITETSFEL              
007000     03 MOD-IDUSER-SPKVAL-DC11                                            
007100                             PIC X(8).                                    
007200*                                 ANVÄNDAR-ID KVALITETSPÄRR               
007300     03 MOD-KVLS-DC11        PIC -(7)9.                                   
007400*                                 LAGERSALDO                              
007500     03 MOD-RAD              OCCURS 12 TIMES.                             
007600*                                 RADINFORMATION                          
007700        05 MOD-IDDC          PIC X(2).                                    
007800*                                 IDENTIFIERARE LAGER                     
007900        05 MOD-KDLEVSP       PIC Z9.                                      
008000*                                 SPÄRRKOD LEVERANS                       
008100        05 MOD-KDLEVSP-UPD-ATTR                                           
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-KDLEVSP-UPD   PIC Z9.                                      
008500*                                 SPÄRRKOD LEVERANS                       
008600        05 MOD-KVSPARR-KVAL  PIC Z(6)9.                                   
008700*                                 SPÄRRAT ANTAL KVALITETSFEL              
008800        05 MOD-KVSPARR-KVAL-UPD-ATTR                                      
008900                             PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100        05 MOD-KVSPARR-KVAL-UPD                                           
009200                             PIC Z(6)9.                                   
009300*                                 SPÄRRAT ANTAL KVALITETSFEL              
009400        05 MOD-TEKVAL-ATTR   PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600        05 MOD-TEKVAL        PIC X(10).                                   
009700*                                 KVALITETSNOTERING SPÄRR                 
009800        05 MOD-TISPARR-KVAL  PIC 9(6).                                    
009900*                                 SPÄRRAD DATUM KVALITETSFEL              
010000        05 MOD-IDUSER-SPKVAL PIC X(8).                                    
010100*                                 ANVÄNDAR-ID KVALITETSPÄRR               
010200        05 MOD-KVLS          PIC -(7)9.                                   
010300*                                 LAGERSALDO                              
010400        05 MOD-FLDCLVL3      PIC X.                                       
010500*                                 DC MED LEVEL 3 I DC-TRÄDET              
010600        05 MOD-FLSPARR       PIC X.                                       
010700*                                 SPÄRRAD ORDER ?                         
010800     03 MOD-TEMFSINF         PIC X(55).                                   
010900*                                 INFORMATIONSMEDDELANDE                  
011000*** END OF VILMAII-COPY LENGTH= 967 BYTES                                 
