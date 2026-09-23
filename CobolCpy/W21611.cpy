000100 01  W21611.                                                              
000200*                                 LISTPOSTER SKROTORDER                   
000300*                                                                         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 IDANSK               PIC S9(3)           COMP-3.                  
000900*                                 ANSKAFFARNUMMER                         
001000     03 KVSKROT-KVAR         PIC S9(7)           COMP-3.                  
001100*                                 KVARLIGGANDE ANTAL                      
001200     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
001300*                                 ARTIKELSTANDARDPRIS                     
001400     03 BEART                PIC X(25).                                   
001500*                                 ARTIKELBENÄMNING                        
001600     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
001700*                                 LAGEROMRÅDE                             
001800     03 ADGANG               PIC S9(3)           COMP-3.                  
001900*                                 GÅNG                                    
002000     03 ADPLATS              PIC S9(5)           COMP-3.                  
002100*                                 LAGERPLATSNUMMER                        
002200     03 KDERS-UTG            PIC S9(3)           COMP-3.                  
002300*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
002400     03 KVRESS               PIC S9(7)           COMP-3.                  
002500*                                 RESERVERAT ANTAL ARTIKLAR               
002600     03 KVROS                PIC S9(7)           COMP-3.                  
002700*                                 RESTORDERSALDO                          
002800     03 SUTPO-TOT            PIC S9(7)           COMP-3.                  
002900*                                 TPO-KVANTITET, TOTAL                    
003000     03 C-LAGERDEL.                                                       
003100        05 KVTILLG-CDC       PIC S9(7)           COMP-3.                  
003200*                                 LAGERTILLGÅNG-CDC                       
003300        05 KVTILLG-SDC       PIC S9(7)           COMP-3.                  
003400*                                 LAGERTILLGÅNG-SDC                       
003500        05 KVAKS-CDC         PIC S9(7)           COMP-3.                  
003600*                                 DEL AV AK SOM LIGGER I CDC              
003700        05 KVAKS-SDC         PIC S9(7)           COMP-3.                  
003800*                                 DEL AV AK SOM LIGGER I SDC              
003900     03 BUFFERT.                                                          
004000*                                 BUFFERTPLATSER PER CLAGER               
004100        05 ADBUFFOMR-1       PIC S9(3)           COMP-3.                  
004200*                                 BUFFERTOMRÅDE                           
004300        05 ADBUFFGANG-1      PIC S9(3)           COMP-3.                  
004400*                                 BUFFERT GÅNG                            
004500        05 ADBUFFPL-1        PIC S9(5)           COMP-3.                  
004600*                                 BUFFERPLATSNUMMER                       
004700        05 ADBUFFOMR-2       PIC S9(3)           COMP-3.                  
004800*                                 BUFFERTOMRÅDE                           
004900        05 ADBUFFGANG-2      PIC S9(3)           COMP-3.                  
005000*                                 BUFFERT GÅNG                            
005100        05 ADBUFFPL-2        PIC S9(5)           COMP-3.                  
005200*                                 BUFFERPLATSNUMMER                       
005300        05 ADBUFFOMR-3       PIC S9(3)           COMP-3.                  
005400*                                 BUFFERTOMRÅDE                           
005500        05 ADBUFFGANG-3      PIC S9(3)           COMP-3.                  
005600*                                 BUFFERT GÅNG                            
005700        05 ADBUFFPL-3        PIC S9(5)           COMP-3.                  
005800*                                 BUFFERPLATSNUMMER                       
005900        05 ADBUFFOMR-4       PIC S9(3)           COMP-3.                  
006000*                                 BUFFERTOMRÅDE                           
006100        05 ADBUFFGANG-4      PIC S9(3)           COMP-3.                  
006200*                                 BUFFERT GÅNG                            
006300        05 ADBUFFPL-4        PIC S9(5)           COMP-3.                  
006400*                                 BUFFERPLATSNUMMER                       
006500     03 BEEMBLEM             OCCURS 20 TIMES                              
006600                             PIC X(5).                                    
006700*                                 EMBLEM                                  
006800     03 IDDISTR              PIC S9(5)           COMP-3.                  
006900*                                 DISTRIKTNUMMER                          
007000     03 IDKONTO              PIC S9(11)          COMP-3.                  
007100*                                 KONTO                                   
007200     03 IDANALYS             PIC X(12).                                   
007300*                                 ANALYSNUMMER                            
007400     03 IDKST                PIC X(10).                                   
007500*                                 KOSTNADSSTÄLLE                          
007600     03 KDARBTYP             PIC X(8).                                    
007700*                                 TYP AV ARBETE                           
007800     03 IDPERSON             PIC S9(3)           COMP-3.                  
007900*                                 PERSONKOD                               
008000     03 KVSKROT-BEORD        PIC S9(7)           COMP-3.                  
008100*                                 ANTAL SENASTE SKROTORDER                
008200     03 IDNAMN               PIC X(40).                                   
008300*                                 NAMN                                    
008400     03 IDAVD                PIC S9(5)           COMP-3.                  
008500*                                 DEN ANSTÄLLDES AVDELNING/               
008600*                                 KOSTNADSSTÄLLE                          
008700     03 IDUSER               PIC X(8).                                    
008800*                                 ANVÄNDARENS SÄKERHETS ID                
008900     03 BEANST               PIC X(25).                                   
009000*                                 ANSTÄLLDS NAMN                          
009100     03 DASKROT9-BEORD       PIC 9(8).                                    
009200*                                 BEORDRAD SKROTNINGSDATUM 9KOMP          
009300     03 TIDATETIME           OCCURS 10 TIMES                              
009400                             PIC X(14).                                   
009500*                                 DATUM OCH TID YYYYMMDDHHMMSS            
009600     03 IDUSER-GODK          OCCURS 10 TIMES                              
009700                             PIC X(8).                                    
009800*                                 ANVÄNDAR-ID GODKÄNNARE                  
009900     03 BEANST-GODK          OCCURS 10 TIMES                              
010000                             PIC X(25).                                   
010100*                                 GODKÄNNARES NAMN                        
010200     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
010300*                                 KUNDNUMMER                              
010400     03 TEMEMO               OCCURS 11 TIMES                              
010500                             PIC X(66).                                   
010600*                                 TEXTRAD MAIL                            
010700*** END OF VILMAII-COPY LENGTH= 1537 BYTES                                
