000100 01  2402-WDGX2402.                                                       
000200*                                 SKROTBEORDRING                          
000300     03 2402-IDARTNR         PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 2402-IDANALYS        PIC X(12).                                   
000600*                                 ANALYSNUMMER                            
000700     03 2402-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 2402-IDDISTR         PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 2402-IDKONTO         PIC S9(11)          COMP-3.                  
001200*                                 KONTO                                   
001300     03 2402-IDKST           PIC X(10).                                   
001400*                                 KOSTNADSSTÄLLE                          
001500     03 2402-IDPERSON        PIC S9(3)           COMP-3.                  
001600*                                 PERSONKOD                               
001700     03 2402-KDARBTYP        PIC X(8).                                    
001800*                                 TYP AV ARBETE                           
001900     03 2402-KVSKROT-BEORD   PIC S9(7)           COMP-3.                  
002000*                                 ANTAL SENASTE SKROTORDER                
002100     03 2402-KVSKROT-KVAR    PIC S9(7)           COMP-3.                  
002200*                                 KVARLIGGANDE ANTAL                      
002300     03 2402-IDUSER          PIC X(8).                                    
002400*                                 ANVÄNDARENS SÄKERHETS ID                
002500     03 2402-BEANST          PIC X(25).                                   
002600*                                 ANSTÄLLDS NAMN                          
002700     03 2402-DASKROT9-BEORD  PIC 9(8).                                    
002800*                                 BEORDRAD SKROTNINGSDATUM 9KOMP          
002900     03 2402-TIDATETIME      OCCURS 10 TIMES                              
003000                             PIC X(14).                                   
003100*                                 DATUM OCH TID YYYYMMDDHHMMSS            
003200     03 2402-IDUSER-GODK     OCCURS 10 TIMES                              
003300                             PIC X(8).                                    
003400*                                 ANVÄNDAR-ID GODKÄNNARE                  
003500     03 2402-BEANST-GODK     OCCURS 10 TIMES                              
003600                             PIC X(25).                                   
003700*                                 GODKÄNNARES NAMN                        
003800     03 2402-IDKUNDNR        PIC S9(7)           COMP-3.                  
003900*                                 KUNDNUMMER                              
004000     03 2402-KDERS-UTG       PIC S9(3)           COMP-3.                  
004100*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
004200     03 2402-KVTILLG-CDC     PIC S9(7)           COMP-3.                  
004300*                                 LAGERTILLGÅNG-CDC                       
004400     03 2402-KVTILLG-SDC     PIC S9(7)           COMP-3.                  
004500*                                 LAGERTILLGÅNG-SDC                       
004600     03 2402-KVAKS-CDC       PIC S9(7)           COMP-3.                  
004700*                                 DEL AV AK SOM LIGGER I CDC              
004800     03 2402-KVAKS-SDC       PIC S9(7)           COMP-3.                  
004900*                                 DEL AV AK SOM LIGGER I SDC              
005000     03 2402-BEEMBLEM        OCCURS 20 TIMES                              
005100                             PIC X(5).                                    
005200*                                 EMBLEM                                  
005300     03 2402-SUTPO-TOT       PIC S9(7)           COMP-3.                  
005400*                                 TPO-KVANTITET, TOTAL                    
005500*** END OF VILMAII-COPY LENGTH= 693 BYTES                                 
