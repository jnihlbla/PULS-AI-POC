000100 01  W41878A.                                                             
000200*                                 HUVUDPOST KNOTA VIA 712                 
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDLEVANM.                                                         
000700*                                 LEVERANSANMÄRKNINGSIDENTITET            
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 IDRAPPNR          PIC 9(7).                                    
001300*                                 RAPPORT NUMMER                          
001400     03 IDDC                 PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 ADKOPARE-1           PIC X(27).                                   
001700*                                 DEL AV KÖPARADRESS                      
001800     03 ADKOPARE-2           PIC X(27).                                   
001900*                                 DEL AV KÖPARADRESS                      
002000     03 BEKOPARE-1           PIC X(27).                                   
002100*                                 DEL AV KÖPARNAMN                        
002200     03 BEKOPARE-2           PIC X(27).                                   
002300*                                 DEL AV KÖPARNAMN                        
002400     03 FORSAKRAN-1          PIC X(38).                                   
002500     03 FORSAKRAN-2          PIC X(34).                                   
002600     03 DATUM                PIC S9(7)           COMP-3.                  
002700     03 IDLANDX2             PIC X(2).                                    
002800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002900     03 IDKNOTNR             PIC S9(7)           COMP-3.                  
003000*                                 KREDITNOTANUMMER                        
003100     03 IDVAT                PIC X(17).                                   
003200*                                 MOMSREGISTRERINGSNUMMER                 
003300     03 KDVALISO             PIC X(3).                                    
003400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003500     03 KDVALUT              PIC S9(3)           COMP-3.                  
003600*                                 VALUTAKOD                               
003700     03 PRKURS-LOC           PIC S9(6)V9(5)      COMP-3.                  
003800*                                 VALUTAKURS                              
003900     03 PRKURS-SDC           PIC S9(6)V9(5)      COMP-3.                  
004000*                                 VALUTAKURS                              
004100     03 REVALUTA-LOC         PIC S9(3)           COMP-3.                  
004200*                                 OMRÄKNINGSTAL FÖR VALUTA                
004300     03 REVALUTA-SDC         PIC S9(3)           COMP-3.                  
004400*                                 OMRÄKNINGSTAL FÖR VALUTA                
004500     03 TILEVANM             PIC S9(7)           COMP-3.                  
004600*                                 DATUM LEVERANSANMÄRKNING                
004700     03 TIRETILL             PIC S9(7)           COMP-3.                  
004800*                                 RETURTILLSTÅNDSDATUM                    
004900     03 SUKRENTO             PIC S9(11)V9(2)     COMP-3.                  
005000*                                 KREDITERAT VARUVÄRDE NETTO              
005100     03 SUKRENOT             PIC S9(9)V9(2)      COMP-3.                  
005200*                                 KREDITNOTASUMMA                         
005300     03 SUKREUTL             PIC S9(11)V9(2)     COMP-3.                  
005400*                                 KREDITNOTASUMMA OMRÄKNAT I KUND         
005500*                                 ENS VALUTA                              
005600     03 IDUSER-ADM           PIC X(8).                                    
005700*                                 ANVÄNDAR-ID ADMINISTRATIV KONTR         
005800     03 BEANST               PIC X(25).                                   
005900*                                 ANSTÄLLDS NAMN                          
006000     03 FLKREPRT             PIC X.                                       
006100*                                 UTSKRIFTSFLAGGA KREDITNOTA              
006200     03 TILLAEGGSKOSTNADER.                                               
006300        05 PRFOERS           PIC S9(7)V9(2)      COMP-3.                  
006400*                                 FÖRSÄKRINGSPREMIE                       
006500        05 PRFRAKT           PIC S9(7)V9(2)      COMP-3.                  
006600*                                 FRAKTKOSTNAD                            
006700        05 PRLEGKST          PIC S9(7)V9(2)      COMP-3.                  
006800*                                 LEGALISERINSKOSTNAD                     
006900        05 REEMBHNT          PIC S9(2)V9(1)      COMP-3.                  
007000*                                 EMB OCH HANTERINGSKOST (%)              
007100        05 RELANDCO          PIC S9(3)V9(2)      COMP-3.                  
007200*                                 LANDING COST PROCENT                    
007300        05 PRLANDCO          PIC S9(7)V9(2)      COMP-3.                  
007400*                                 LANDING COST                            
007500        05 PREMBHNT          PIC S9(7)V9(2)      COMP-3.                  
007600*                                 EMBALLAGE O HANTERINGSKOST              
007700        05 PRMOMS            PIC S9(7)V9(2)      COMP-3.                  
007800*                                 MERVÄRDESSKATT                          
007900     03 ATTESTANSVARIGA      OCCURS 5 TIMES.                              
008000        05 IDUSER-GODK       PIC X(8).                                    
008100*                                 ANVÄNDAR-ID GODKÄNNARE                  
008200        05 FILLER            PIC X.                                       
008300        05 BEANST-GODK       PIC X(25).                                   
008400*                                 GODKÄNNARES NAMN                        
008500        05 FILLER            PIC X.                                       
008600        05 TIUPPDAT          PIC 9(6).                                    
008700*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
008800*** END OF VILMAII-COPY LENGTH= 549 BYTES                                 
