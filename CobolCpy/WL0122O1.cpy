000100 01  RESP-WL0122O1.                                                       
000200*                                 RESPONS FROM PGM WL0122                 
000300     03 RESP-HEADER.                                                      
000400        05 RESP-IDDC-KEY     PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600        05 RESP-IDANSTNR-KEY PIC Z(5).                                    
000700*                                 ANSTÄLLNINGSNUMMER                      
000800        05 RESP-IDDISTR-KEY  PIC Z(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000        05 RESP-IDKUNDNR-KEY PIC Z(6).                                    
001100*                                 KUNDNUMMER                              
001200        05 RESP-IDORDNR-KEY  PIC Z(5).                                    
001300*                                 ORDERNUMMER UTGÅR PD90                  
001400        05 RESP-IDKOLLI-KEY  PIC Z(5).                                    
001500*                                 KOLLINUMMER                             
001600        05 RESP-IDPRODNR-KEY PIC Z(7).                                    
001700*                                 PRODUKTIONSNUMMER                       
001800        05 RESP-KDKOLLI      PIC X(8).                                    
001900*                                 KOLLIKOD                                
002000        05 RESP-VKORDBTO-KOLLI                                            
002100                             PIC Z(6).Z.                                  
002200*                                 ORDERVIKT BRUTTO (KG)                   
002300        05 RESP-FLSISTAK     PIC X.                                       
002400*                                 SISTA KOLLI I ORDERN?                   
002500        05 RESP-KDEMBTYP     PIC Z.                                       
002600*                                 EMBALLAGETYP                            
002700        05 RESP-DIKOLLIL     PIC Z(4).                                    
002800*                                 KOLLI-LÄNGD                             
002900        05 RESP-DIKOLLIB     PIC Z(3).                                    
003000*                                 KOLLI-BREDD                             
003100        05 RESP-DIKOLLIH     PIC Z(3).                                    
003200*                                 KOLLI-HÖJD                              
003300        05 RESP-ADFLGEO      PIC X(3).                                    
003400*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
003500        05 RESP-ADFLOMR      PIC X(3).                                    
003600*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
003700        05 RESP-ADRUTNIV     PIC X(3).                                    
003800*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
003900        05 RESP-IDKOLLI-FOM  PIC Z(5).                                    
004000*                                 KOLLINUMMER                             
004100        05 RESP-IDKOLLI-TOM  PIC Z(5).                                    
004200*                                 KOLLINUMMER                             
004300        05 RESP-FLSKRIV-CLABEL                                            
004400                             PIC X.                                       
004500*                                 J/Y = SKRIV BEGÄRD LISTA                
004600        05 RESP-FLSKRIV-DELNOTE                                           
004700                             PIC X.                                       
004800*                                 J/Y = SKRIV BEGÄRD LISTA                
004900        05 RESP-IDKOLLI-SAMP PIC Z(5).                                    
005000*                                 SAMPACKNINGSKOLLINUMMER                 
005100        05 RESP-RAD          OCCURS 200 TIMES.                            
005200           07 RESP-IDRADNR-FOM                                            
005300                             PIC Z(4).                                    
005400*                                 RADNUMMER                               
005500           07 RESP-IDRADNR-TOM                                            
005600                             PIC Z(4).                                    
005700*                                 RADNUMMER                               
005800           07 RESP-KVLEVART  PIC Z(7).                                    
005900*                                 LEVERERAT ANTAL STYCK                   
006000           07 RESP-IDMSG-ERROR-LINE                                       
006100                             PIC X(3).                                    
006200*                                 FELMEDDELANDE ID                        
006300     03 RESP-WL0123I1.                                                    
006400*                                 REQUEST TO PGM WL0123                   
006500        05 RESP-L123-IDDC-KEY                                             
006600                             PIC X(2).                                    
006700*                                 IDENTIFIERARE LAGER                     
006800        05 RESP-L123-IDANSTNR-KEY                                         
006900                             PIC 9(5).                                    
007000*                                 ANSTÄLLNINGSNUMMER                      
007100        05 RESP-L123-IDDISTR-KEY                                          
007200                             PIC 9(4).                                    
007300*                                 DISTRIKTNUMMER                          
007400        05 RESP-L123-IDKUNDNR-KEY                                         
007500                             PIC 9(6).                                    
007600*                                 KUNDNUMMER                              
007700        05 RESP-L123-IDORDNR-KEY                                          
007800                             PIC 9(5).                                    
007900*                                 ORDERNUMMER                             
008000        05 RESP-L123-IDKOLLI-KEY                                          
008100                             PIC 9(5).                                    
008200*                                 KOLLINUMMER                             
008300        05 RESP-L123-IDPRODNR-KEY                                         
008400                             PIC 9(7).                                    
008500*                                 PRODUKTIONSNUMMER                       
008600        05 RESP-L123-FLAGGA  PIC X.                                       
008700*                                 ALLMÄN FLAGGA                           
008800        05 RESP-L123-FLSVAR  PIC X.                                       
008900*                                 ALLMÄN SVARSFLAGGA                      
009000        05 RESP-L123-IDPLKLST-SPAR                                        
009100                             PIC 9(3).                                    
009200*                                 PLOCKLISTNUMMER                         
009300        05 RESP-L123-IDRADNR-SPAR                                         
009400                             PIC 9(4).                                    
009500*                                 RADNUMMER                               
009600     03 RESP-WL0128I1.                                                    
009700*                                 REQUEST FOR PGM WL0128                  
009800*                                 PRINTING OF CASE LABEL                  
009900        05 RESP-L128-FLBG    PIC X.                                       
010000*                                 ALLMÄN FLAGGA                           
010100        05 RESP-L128-KVRADER PIC 9(5).                                    
010200*                                 ANTAL RADER                             
010300        05 RESP-L128-RAD     OCCURS 50 TIMES.                             
010400*                                 REQUEST-COPYTEXT FÖR WL0128             
010500*                                                                         
010600           07 RESP-L128-IDDC-KEY                                          
010700                             PIC X(2).                                    
010800*                                 IDENTIFIERARE LAGER                     
010900           07 RESP-L128-IDDISTR-KEY                                       
011000                             PIC 9(4).                                    
011100*                                 DISTRIKTNUMMER                          
011200           07 RESP-L128-IDKUNDNR-KEY                                      
011300                             PIC 9(6).                                    
011400*                                 KUNDNUMMER                              
011500           07 RESP-L128-IDORDNR-KEY                                       
011600                             PIC 9(5).                                    
011700*                                 ORDERNUMMER UTGÅR PD90                  
011800           07 RESP-L128-IDKOLLI-KEY                                       
011900                             PIC 9(5).                                    
012000*                                 KOLLINUMMER                             
012100           07 RESP-L128-IDPRODNR-KEY                                      
012200                             PIC 9(7).                                    
012300*                                 PRODUKTIONSNUMMER                       
012400           07 RESP-L128-CLABEL                                            
012500                             PIC X.                                       
012600*                                 J/Y = SKRIV BEGÄRD LISTA                
012700           07 RESP-L128-IDKOLLI-TOM                                       
012800                             PIC 9(5).                                    
012900*                                 KOLLINUMMER TILL OCH MED                
013000     03 RESP-WL0129I1.                                                    
013100*                                 REQUEST TO PGM  WL0129                  
013200*                                 PRINTING OF DELIVERY NOTE               
013300        05 RESP-L129-FLBG    PIC X.                                       
013400*                                 ALLMÄN FLAGGA                           
013500        05 RESP-L129-KVRADER PIC 9(5).                                    
013600*                                 ANTAL RADER                             
013700        05 RESP-L129-RAD     OCCURS 50 TIMES.                             
013800*                                 REQUEST TILL PGM WL0129                 
013900*                                                                         
014000           07 RESP-L129-IDDC-KEY                                          
014100                             PIC X(2).                                    
014200*                                 IDENTIFIERARE LAGER                     
014300           07 RESP-L129-IDDISTR-KEY                                       
014400                             PIC 9(4).                                    
014500*                                 DISTRIKTNUMMER                          
014600           07 RESP-L129-IDKUNDNR-KEY                                      
014700                             PIC 9(6).                                    
014800*                                 KUNDNUMMER                              
014900           07 RESP-L129-IDORDNR-KEY                                       
015000                             PIC 9(5).                                    
015100*                                 ORDERNUMMER UTGÅR PD90                  
015200           07 RESP-L129-IDKOLLI-KEY                                       
015300                             PIC 9(5).                                    
015400*                                 KOLLINUMMER                             
015500           07 RESP-L129-IDKOLLI-TOM                                       
015600                             PIC 9(5).                                    
015700*                                 KOLLINUMMER                             
015800           07 RESP-L129-FLSKRIV-DELNOTE                                   
015900                             PIC X.                                       
016000*                                 J/Y = SKRIV BEGÄRD LISTA                
016100     03 RESP-WL0198I1.                                                    
016200*                                 REQUEST TO PGM WL0198                   
016300        05 RESP-L198-IDDC-KEY                                             
016400                             PIC X(2).                                    
016500*                                 IDENTIFIERARE LAGER                     
016600        05 RESP-L198-IDANSTNR-KEY                                         
016700                             PIC 9(5).                                    
016800*                                 ANSTÄLLNINGSNUMMER                      
016900        05 RESP-L198-IDDISTR-KEY                                          
017000                             PIC 9(4).                                    
017100*                                 DISTRIKTNUMMER                          
017200        05 RESP-L198-IDKUNDNR-KEY                                         
017300                             PIC 9(6).                                    
017400*                                 KUNDNUMMER                              
017500        05 RESP-L198-IDORDNR-KEY                                          
017600                             PIC 9(5).                                    
017700*                                 ORDERNUMMER                             
017800        05 RESP-L198-IDKOLLI-KEY                                          
017900                             PIC 9(5).                                    
018000*                                 KOLLINUMMER                             
018100        05 RESP-L198-IDPRODNR-KEY                                         
018200                             PIC 9(7).                                    
018300*                                 PRODUKTIONSNUMMER                       
018400        05 RESP-L198-RAD     OCCURS 15 TIMES.                             
018500           07 RESP-L198-IDRADNR                                           
018600                             PIC 9(4).                                    
018700*                                 RADNUMMER                               
018800           07 RESP-L198-KDARTURS                                          
018900                             PIC X(2).                                    
019000*                                 ARTIKELURSPRUNGSKOD                     
019100*** END OF VILMAII-COPY LENGTH= 7017 BYTES                                
