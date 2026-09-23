000100 01  RESP-WL0121O1.                                                       
000200*                                 RESPONS FROM PGM WL0121                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-FLSKRIV-CLABEL  PIC X.                                       
000600*                                 J/Y = SKRIV BEGÄRD LISTA                
000700     03 RESP-FLSKRIV-DELNOTE PIC X.                                       
000800*                                 J/Y = SKRIV BEGÄRD LISTA                
000900     03 RESP-IDKOLLI-SAMP    PIC Z(5).                                    
001000*                                 SAMPACKNINGSKOLLINUMMER                 
001100     03 RESP-RAD             OCCURS 15 TIMES.                             
001200*                                 COPYTEXT FÖR MOD WL0121O1               
001300        05 RESP-IDPRODNR     PIC Z(7).                                    
001400*                                 PRODUKTIONSNUMMER                       
001500        05 RESP-IDPLKLST     PIC Z(3).                                    
001600*                                 PLOCKLISTNUMMER                         
001700        05 RESP-IDKOLLI      PIC Z(5).                                    
001800*                                 KOLLINUMMER                             
001900        05 RESP-KDKOLLI      PIC X(8).                                    
002000*                                 KOLLIKOD                                
002100        05 RESP-VKORDBTO-KOLLI                                            
002200                             PIC Z(6).Z.                                  
002300*                                 ORDERVIKT BRUTTO (KG)                   
002400        05 RESP-KDEMBTYP     PIC Z.                                       
002500*                                 EMBALLAGETYP                            
002600        05 RESP-DIKOLLIL     PIC Z(4).                                    
002700*                                 KOLLI-LÄNGD                             
002800        05 RESP-DIKOLLIB     PIC Z(3).                                    
002900*                                 KOLLI-BREDD                             
003000        05 RESP-DIKOLLIH     PIC Z(3).                                    
003100*                                 KOLLI-HÖJD                              
003200        05 RESP-FLAVSP       PIC X.                                       
003300*                                 ALLMÄN FELFLAGGA                        
003400        05 RESP-IDMSG-ERROR-RAD                                           
003500                             PIC X(3).                                    
003600*                                 FELMEDDELANDE ID                        
003700     03 RESP-WL0128I1.                                                    
003800*                                 REQUEST FOR PGM WL0128                  
003900*                                 PRINTING OF CASE LABEL                  
004000        05 RESP-L128-FLBG    PIC X.                                       
004100*                                 ALLMÄN FLAGGA                           
004200        05 RESP-L128-KVRADER PIC Z(4)9.                                   
004300*                                 ANTAL RADER                             
004400        05 RESP-L128-RAD     OCCURS 15 TIMES.                             
004500*                                 REQUEST-COPYTEXT FÖR WL0128             
004600*                                                                         
004700           07 RESP-L128-IDDC-KEY                                          
004800                             PIC X(2).                                    
004900*                                 IDENTIFIERARE LAGER                     
005000           07 RESP-L128-IDDISTR-KEY                                       
005100                             PIC Z(3)9.                                   
005200*                                 DISTRIKTNUMMER                          
005300           07 RESP-L128-IDKUNDNR-KEY                                      
005400                             PIC Z(5)9.                                   
005500*                                 KUNDNUMMER                              
005600           07 RESP-L128-IDORDNR-KEY                                       
005700                             PIC Z(4)9.                                   
005800*                                 ORDERNUMMER UTGÅR PD90                  
005900           07 RESP-L128-IDKOLLI-KEY                                       
006000                             PIC Z(4)9.                                   
006100*                                 KOLLINUMMER                             
006200           07 RESP-L128-IDPRODNR-KEY                                      
006300                             PIC Z(6)9.                                   
006400*                                 PRODUKTIONSNUMMER                       
006500           07 RESP-L128-CLABEL                                            
006600                             PIC X.                                       
006700*                                 J/Y = SKRIV BEGÄRD LISTA                
006800           07 RESP-L128-IDKOLLI-TOM                                       
006900                             PIC Z(4)9.                                   
007000*                                 KOLLINUMMER TILL OCH MED                
007100     03 RESP-WL0129I1.                                                    
007200*                                 REQUEST TO PGM  WL0129                  
007300*                                 PRINTING OF DELIVERY NOTE               
007400        05 RESP-L129-FLBG    PIC X.                                       
007500*                                 ALLMÄN FLAGGA                           
007600        05 RESP-L129-KVRADER PIC Z(4)9.                                   
007700*                                 ANTAL RADER                             
007800        05 RESP-L129-RAD     OCCURS 15 TIMES.                             
007900*                                 REQUEST TILL PGM WL0129                 
008000*                                                                         
008100           07 RESP-L129-IDDC-KEY                                          
008200                             PIC X(2).                                    
008300*                                 IDENTIFIERARE LAGER                     
008400           07 RESP-L129-IDDISTR-KEY                                       
008500                             PIC Z(3)9.                                   
008600*                                 DISTRIKTNUMMER                          
008700           07 RESP-L129-IDKUNDNR-KEY                                      
008800                             PIC Z(5)9.                                   
008900*                                 KUNDNUMMER                              
009000           07 RESP-L129-IDORDNR-KEY                                       
009100                             PIC Z(4)9.                                   
009200*                                 ORDERNUMMER UTGÅR PD90                  
009300           07 RESP-L129-IDKOLLI-KEY                                       
009400                             PIC Z(4)9.                                   
009500*                                 KOLLINUMMER                             
009600           07 RESP-L129-IDKOLLI-TOM                                       
009700                             PIC Z(4)9.                                   
009800*                                 KOLLINUMMER                             
009900           07 RESP-L129-FLSKRIV-DELNOTE                                   
010000                             PIC X.                                       
010100*                                 J/Y = SKRIV BEGÄRD LISTA                
010200     03 RESP-WL0198I1.                                                    
010300*                                 REQUEST TO PGM WL0198                   
010400        05 RESP-L198-IDDC-KEY                                             
010500                             PIC X(2).                                    
010600*                                 IDENTIFIERARE LAGER                     
010700        05 RESP-L198-IDANSTNR-KEY                                         
010800                             PIC Z(4)9.                                   
010900*                                 ANSTÄLLNINGSNUMMER                      
011000        05 RESP-L198-IDDISTR-KEY                                          
011100                             PIC Z(3)9.                                   
011200*                                 DISTRIKTNUMMER                          
011300        05 RESP-L198-IDKUNDNR-KEY                                         
011400                             PIC Z(5)9.                                   
011500*                                 KUNDNUMMER                              
011600        05 RESP-L198-IDORDNR-KEY                                          
011700                             PIC Z(4)9.                                   
011800*                                 ORDERNUMMER                             
011900        05 RESP-L198-IDKOLLI-KEY                                          
012000                             PIC Z(4)9.                                   
012100*                                 KOLLINUMMER                             
012200        05 RESP-L198-IDPRODNR-KEY                                         
012300                             PIC Z(6)9.                                   
012400*                                 PRODUKTIONSNUMMER                       
012500        05 RESP-L198-RAD     OCCURS 15 TIMES.                             
012600           07 RESP-L198-IDRADNR                                           
012700                             PIC Z(3)9.                                   
012800*                                 RADNUMMER                               
012900           07 RESP-L198-KDARTURS                                          
013000                             PIC X(2).                                    
013100*                                 ARTIKELURSPRUNGSKOD                     
013200*** END OF VILMAII-COPY LENGTH= 1780 BYTES                                
