000100 01  MOD-W2O36301.                                                        
000200*                                 MOD-COPYTEXT FÖR W20363                 
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
001500     03 MOD-UTFALT.                                                       
001600*                                 UTDATA 2363                             
001700        05 MOD-BEART-ENG     PIC X(25).                                   
001800*                                 ENGELSK ARTIKELBENÄMNING                
001900        05 MOD-IDDC-GRP      OCCURS 4 TIMES                               
002000                             PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200        05 MOD-IDLEVNR-SLAG  OCCURS 4 TIMES                               
002300                             PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER                        
002500        05 MOD-FLORDSP       OCCURS 4 TIMES                               
002600                             PIC X.                                       
002700*                                 ORDERSPÄRR                              
002800        05 MOD-DAORDSP       OCCURS 4 TIMES                               
002900                             PIC 9(6).                                    
003000*                                 ANGER DATUM NÄR STATUS PÅ ORDER         
003100*                                 SPÄRRFLAGGA ÄNDRAS                      
003200*                                 YYYYMMDD                                
003300        05 MOD-IDUSER-ORDSP  OCCURS 4 TIMES                               
003400                             PIC X(8).                                    
003500*                                 ANVÄNDAR-ID SPÄRRA ORDER                
003600        05 MOD-FLSPBULK      OCCURS 4 TIMES                               
003700                             PIC X.                                       
003800*                                 FLAGGA SPÄRR MOT BULKORDER              
003900        05 MOD-DASPBULK      OCCURS 4 TIMES                               
004000                             PIC 9(6).                                    
004100*                                 ANGER DATUM NÄR STATUS PÅ FLAGG         
004200*                                 A FÖR ATT                               
004300*                                 SPÄRRA BULKORDER ÄNDRAS                 
004400*                                 YYYYMMDD                                
004500        05 MOD-IDUSER-SPBULK OCCURS 4 TIMES                               
004600                             PIC X(8).                                    
004700*                                 ANVÄNDAR-ID SPÄRRA BULKORDER            
004800        05 MOD-KVDAGAR-MANLT OCCURS 4 TIMES                               
004900                             PIC Z(2)9.                                   
005000*                                 ANTAL DAGAR                             
005100        05 MOD-IDPERSON-BUY  OCCURS 4 TIMES                               
005200                             PIC Z(2)9.                                   
005300*                                 PERSONKOD REFILLANSVARIG                
005400        05 MOD-FLREFERAL     PIC X.                                       
005500*                                 REFERALARTIKEL I LANDET                 
005600        05 MOD-IDUSER-REFERAL                                             
005700                             PIC X(8).                                    
005800*                                 ANVÄNDARENS SÄKERHETS ID                
005900        05 MOD-TIREGDAT-REFERAL                                           
006000                             PIC Z(6).                                    
006100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006200     03 MOD-INFALT.                                                       
006300*                                 INDATA 2363                             
006400        05 MOD-IDLEVNR-SLAG-GRP                                           
006500                             OCCURS 4 TIMES.                              
006600           07 MOD-IDLEVNR-SLAG-IN-ATTR                                    
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900           07 MOD-IDLEVNR-SLAG-IN                                         
007000                             PIC X(2).                                    
007100*                                 MFS BEHANDLING AV INPUTFÄLT             
007200        05 MOD-FLORDSP-GRP   OCCURS 4 TIMES.                              
007300           07 MOD-FLORDSP-IN-ATTR                                         
007400                             PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600           07 MOD-FLORDSP-IN PIC X(2).                                    
007700*                                 MFS BEHANDLING AV INPUTFÄLT             
007800        05 MOD-FLSPBULK-GRP  OCCURS 4 TIMES.                              
007900           07 MOD-FLSPBULK-IN-ATTR                                        
008000                             PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200           07 MOD-FLSPBULK-IN                                             
008300                             PIC X(2).                                    
008400*                                 MFS BEHANDLING AV INPUTFÄLT             
008500        05 MOD-IDPERSON-BUY-GRP                                           
008600                             OCCURS 4 TIMES.                              
008700           07 MOD-IDPERSON-BUY-IN-ATTR                                    
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000           07 MOD-IDPERSON-BUY-IN                                         
009100                             PIC X(2).                                    
009200*                                 MFS BEHANDLING AV INPUTFÄLT             
009300        05 MOD-FLREFERAL-IN-ATTR                                          
009400                             PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600        05 MOD-FLREFERAL-IN  PIC X.                                       
009700*                                 REFERALARTIKEL I LANDET                 
009800        05 MOD-TEARTNOT-ORDER-GRP                                         
009900                             OCCURS 4 TIMES.                              
010000           07 MOD-TEARTNOT-ORDER-IN-ATTR                                  
010100                             PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300           07 MOD-TEARTNOT-ORDER-IN                                       
010400                             PIC X(70).                                   
010500*                                 ARTIKEL NOTERING ORDER                  
010600     03 MOD-TEMFSINF         PIC X(55).                                   
010700*                                 INFORMATIONSMEDDELANDE                  
010800*** END OF VILMAII-COPY LENGTH= 688 BYTES                                 
