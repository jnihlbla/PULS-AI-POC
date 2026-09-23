000100 01  W51095X.                                                             
000200*                                 POSTTYP 95X FAKTURAHUVUD                
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 KDFAKTYP             PIC X.                                       
000700*                                 FAKTURATYP                              
000800     03 IDFAKT               PIC S9(7)           COMP-3.                  
000900*                                 FAKTURANUMMER                           
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDDISTR              PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600     03 KDORDKL              PIC S9              COMP-3.                  
001700*                                 ORDERKLASS                              
001800     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001900*                                 FRAKTSÄTT DC TILL KUND                  
002000     03 FILLER               PIC X(2).                                    
002100     03 SUFKTBEL             PIC S9(9)V9(2)      COMP-3.                  
002200*                                 SUMMA FAKTURERAT BELOPP                 
002300     03 PRFRAKT              PIC S9(7)V9(2)      COMP-3.                  
002400*                                 FRAKTKOSTNAD                            
002500     03 PRFOERS              PIC S9(7)V9(2)      COMP-3.                  
002600*                                 FÖRSÄKRINGSPREMIE                       
002700     03 PREMBHNT             PIC S9(7)V9(2)      COMP-3.                  
002800*                                 EMBALLAGE O HANTERINGSKOST              
002900     03 PRLEGKST             PIC S9(7)V9(2)      COMP-3.                  
003000*                                 LEGALISERINSKOSTNAD                     
003100     03 PREXPKST             PIC S9(7)V9(2)      COMP-3.                  
003200*                                 EXPEDITIONSKOSTNADER                    
003300     03 PRAVDRAG             PIC S9(7)V9(2)      COMP-3.                  
003400*                                 AVDRAGSBELOPP                           
003500     03 PRMOMS               PIC S9(7)V9(2)      COMP-3.                  
003600*                                 MERVÄRDESSKATT                          
003700     03 SUFAKTRE             PIC S9(9)V9(2)      COMP-3.                  
003800*                                 FSG FAKTURERAT PRIS RESERVDELAR         
003900     03 SUFKTUTL             PIC S9(11)V9(2)     COMP-3.                  
004000*                                 FAKTURABELOPP I UTLÄNDSK VALUTA         
004100     03 IDKONTO-AVDRAG       PIC 9(10).                                   
004200*                                 KONTO FÖR AVDRAG                        
004300     03 IDKST                PIC X(10).                                   
004400*                                 KOSTNADSSTÄLLE                          
004500     03 IDKONTO              PIC 9(10).                                   
004600*                                 KONTO                                   
004700     03 DAFAKT               PIC 9(8).                                    
004800*                                 FAKTURERINGSDATUM (ÅÅÅÅMMDD)            
004900     03 BEKOPARE             PIC X(54).                                   
005000*                                 KÖPARNAMN                               
005100     03 ADKOPARE             PIC X(54).                                   
005200*                                 KÖPARADRESS                             
005300     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
005400*                                 VALUTAKURS                              
005500     03 KDVALISO             PIC X(3).                                    
005600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005700     03 BEVARREF             PIC X(10).                                   
005800*                                 VÅR REFERENS                            
005900     03 DASKEPPN             PIC 9(8).                                    
006000*                                 SKEPPNINGSDATUM  (ÅÅÅÅMMDD)             
006100     03 FLOVRLEV             PIC X.                                       
006200*                                 ÖVERLEVERANS                            
006300     03 IDLANDX2             PIC X(2).                                    
006400*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
006500     03 FILLER               PIC X(5).                                    
006600*** END OF VILMAII-COPY LENGTH= 257 BYTES                                 
