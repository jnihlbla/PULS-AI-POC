000100 01  RESP-WL0197O1.                                                       
000200*                                 RESPONS FROM PGM WL0197                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDTRPTNR-KEY    PIC Z(2)9.                                   
000600*                                 TRANSPORTIDENTITET                      
000700     03 RESP-TIRFSDAT-KEY    PIC 9(6).                                    
000800*                                 KLART FÖR TRANSPORT ÅÅMMDD              
000900     03 RESP-TIRFSTID-KEY    PIC 9(4).                                    
001000*                                 KLART FÖR TRANSPORT (TTMM)              
001100     03 RESP-IDPRC-KEY.                                                   
001200*                                 PRODUKTIONSKANAL                        
001300        05 RESP-IDPRCBAS     PIC X(3).                                    
001400*                                 PRC-BAS                                 
001500        05 RESP-IDPRCVAR     PIC X.                                       
001600*                                 PRC-VARIANT                             
001700     03 RESP-IDLOTNR-KEY     PIC Z(2)9.                                   
001800*                                 VAGN-NUMMER                             
001900     03 RESP-KVRADER         PIC Z(4)9.                                   
002000*                                 ANTAL RADER                             
002100     03 RESP-FLOK-PACKRAPP   PIC X.                                       
002200*                                 JA/NEJ-FLAGGA                           
002300     03 RESP-FLSKRIV-CLABEL  PIC X.                                       
002400*                                 J/Y = SKRIV BEGÄRD LISTA                
002500     03 RESP-FLSKRIV-DELNOTE PIC X.                                       
002600*                                 J/Y = SKRIV BEGÄRD LISTA                
002700     03 RESP-RAD             OCCURS 500 TIMES.                            
002800*                                 COPYTEXT FÖR MOD WL0197O1               
002900        05 RESP-IDPRODNR     PIC Z(7).                                    
003000*                                 PRODUKTIONSNUMMER                       
003100        05 RESP-IDPLKLST     PIC Z(3).                                    
003200*                                 PLOCKLISTNUMMER                         
003300        05 RESP-IDKOLLI      PIC Z(5).                                    
003400*                                 KOLLINUMMER                             
003500        05 RESP-KDKOLLI      PIC X(8).                                    
003600*                                 KOLLIKOD                                
003700        05 RESP-KDKOLLI-IN   PIC X(8).                                    
003800*                                 KOLLIKOD                                
003900        05 RESP-IDMSG-ERROR-RAD                                           
004000                             PIC X(3).                                    
004100*                                 FELMEDDELANDE ID                        
004200     03 RESP-WL0128I1.                                                    
004300*                                 REQUEST FOR PGM WL0128                  
004400*                                 PRINTING OF CASE LABEL                  
004500        05 RESP-L128-FLBG    PIC X.                                       
004600*                                 ALLMÄN FLAGGA                           
004700        05 RESP-L128-KVRADER PIC Z(4)9.                                   
004800*                                 ANTAL RADER                             
004900        05 RESP-L128-RAD     OCCURS 50 TIMES.                             
005000*                                 REQUEST-COPYTEXT FÖR WL0128             
005100*                                                                         
005200           07 RESP-L128-IDDC-KEY                                          
005300                             PIC X(2).                                    
005400*                                 IDENTIFIERARE LAGER                     
005500           07 RESP-L128-IDDISTR-KEY                                       
005600                             PIC Z(3)9.                                   
005700*                                 DISTRIKTNUMMER                          
005800           07 RESP-L128-IDKUNDNR-KEY                                      
005900                             PIC Z(5)9.                                   
006000*                                 KUNDNUMMER                              
006100           07 RESP-L128-IDORDNR-KEY                                       
006200                             PIC Z(4)9.                                   
006300*                                 ORDERNUMMER UTGÅR PD90                  
006400           07 RESP-L128-IDKOLLI-KEY                                       
006500                             PIC Z(4)9.                                   
006600*                                 KOLLINUMMER                             
006700           07 RESP-L128-IDPRODNR-KEY                                      
006800                             PIC Z(6)9.                                   
006900*                                 PRODUKTIONSNUMMER                       
007000           07 RESP-L128-CLABEL                                            
007100                             PIC X.                                       
007200*                                 J/Y = SKRIV BEGÄRD LISTA                
007300           07 RESP-L128-IDKOLLI-TOM                                       
007400                             PIC Z(4)9.                                   
007500*                                 KOLLINUMMER TILL OCH MED                
007600     03 RESP-WL0129I1.                                                    
007700*                                 REQUEST TO PGM  WL0129                  
007800*                                 PRINTING OF DELIVERY NOTE               
007900        05 RESP-L129-FLBG    PIC X.                                       
008000*                                 ALLMÄN FLAGGA                           
008100        05 RESP-L129-KVRADER PIC Z(4)9.                                   
008200*                                 ANTAL RADER                             
008300        05 RESP-L129-RAD     OCCURS 50 TIMES.                             
008400*                                 REQUEST TILL PGM WL0129                 
008500*                                                                         
008600           07 RESP-L129-IDDC-KEY                                          
008700                             PIC X(2).                                    
008800*                                 IDENTIFIERARE LAGER                     
008900           07 RESP-L129-IDDISTR-KEY                                       
009000                             PIC Z(3)9.                                   
009100*                                 DISTRIKTNUMMER                          
009200           07 RESP-L129-IDKUNDNR-KEY                                      
009300                             PIC Z(5)9.                                   
009400*                                 KUNDNUMMER                              
009500           07 RESP-L129-IDORDNR-KEY                                       
009600                             PIC Z(4)9.                                   
009700*                                 ORDERNUMMER UTGÅR PD90                  
009800           07 RESP-L129-IDKOLLI-KEY                                       
009900                             PIC Z(4)9.                                   
010000*                                 KOLLINUMMER                             
010100           07 RESP-L129-IDKOLLI-TOM                                       
010200                             PIC Z(4)9.                                   
010300*                                 KOLLINUMMER                             
010400           07 RESP-L129-FLSKRIV-DELNOTE                                   
010500                             PIC X.                                       
010600*                                 J/Y = SKRIV BEGÄRD LISTA                
010700*** END OF VILMAII-COPY LENGTH= 20192 BYTES                               
