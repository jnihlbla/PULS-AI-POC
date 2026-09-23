000100 01  W42108.                                                              
000200*                                                                         
000300*                                 POSTTYP 621 SKAPAS I W421               
000400*                                 MED FÖRETAGSKOD                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDFTG                PIC 9(2).                                    
000800*                                 FÖRETAGSID EKONOM REDOVISNING           
000900     03 IDDISTR              PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300     03 IDDC                 PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 IDFAKT               PIC S9(7)           COMP-3.                  
001600*                                 FAKTURANUMMER                           
001700     03 IDORDNR              PIC S9(5)           COMP-3.                  
001800*                                 ORDERNUMMER UTGÅR PD90                  
001900     03 BEKUNDRF             PIC X(10).                                   
002000*                                 KUNDENS REFERENS                        
002100     03 BEVARREF             PIC X(10).                                   
002200*                                 VÅR REFERENS                            
002300     03 FLSISTAK             PIC S9              COMP-3.                  
002400*                                 SISTA KOLLI I ORDERN?                   
002500     03 KDBETVIL             PIC X(4).                                    
002600*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
002700     03 KDFAKTYP             PIC X.                                       
002800*                                 FAKTURATYP                              
002900     03 KDFRAKT              PIC S9(3)           COMP-3.                  
003000*                                 FRAKTSÄTT DC TILL KUND                  
003100     03 KDLEVVIL             PIC S9              COMP-3.                  
003200*                                 LEVERANSVILLKOR                         
003300     03 KDNOTES              PIC X(2).                                    
003400*                                 NOTERINGSKOD                            
003500     03 KDORDKL              PIC S9              COMP-3.                  
003600*                                 ORDERKLASS                              
003700     03 KDPERSON             PIC S9(3)           COMP-3.                  
003800*                                 PERSONKOD                               
003900     03 KVKOLLIO             PIC S9(5)           COMP-3.                  
004000*                                 ANTAL KOLLI PER ORDER                   
004100     03 KVRAD-UPD            PIC S9(7)           COMP-3.                  
004200*                                 ANTAL ORDERRADER                        
004300     03 KVRORAD-UPD          PIC S9(5)           COMP-3.                  
004400*                                 ANTAL RESTORDER-RADER                   
004500     03 TIAVBOKN             PIC S9(5)           COMP-3.                  
004600*                                 AVBOKNINGS-DATUM                        
004700     03 TIFAKT               PIC S9(5)           COMP-3.                  
004800*                                 FAKTURADATUM (ÅÅVVD) TIFAKT-002         
004900     03 TIPLLEVD             PIC S9(3)           COMP-3.                  
005000*                                 PLANERAD LEVERANSDAG (VVD)              
005100     03 VKORDBTO             PIC S9(6)V9(1)      COMP-3.                  
005200*                                 ORDERVIKT BRUTTO (KG)                   
005300     03 VLORD                PIC S9(4)V9(3)      COMP-3.                  
005400*                                 ORDER-VOLYM NETTO (M3)                  
005500     03 IDDEALER             PIC S9(7)           COMP-3.                  
005600*                                 DEALER KUNDNUMMER                       
005700*** END OF VILMAII-COPY LENGTH= 85 BYTES                                  
