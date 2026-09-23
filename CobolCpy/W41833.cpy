000100 01  W41833.                                                              
000200*                                 KREDITPOST TYP 720,728,729,             
000300*                                 ATT SKAPA HUVUD OCH RADPOSTER           
000400*                                 AV TILL EKONOMI FVB TILL LAB            
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 IDDISTR              PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500     03 IDKNOTNR             PIC S9(7)           COMP-3.                  
001600*                                 KREDITNOTANUMMER                        
001700     03 IDRAPPNR             PIC 9(7).                                    
001800*                                 RAPPORT NUMMER                          
001900     03 DAKRENOT             PIC 9(8).                                    
002000*                                 DATUM KREDITNOTA  (ÅÅÅÅMMDD)            
002100     03 PRLANDCO             PIC S9(7)V9(2)      COMP-3.                  
002200*                                 LANDING COST                            
002300     03 PRFRAKT              PIC S9(7)V9(2)      COMP-3.                  
002400*                                 FRAKTKOSTNAD                            
002500     03 PRLEGKST             PIC S9(7)V9(2)      COMP-3.                  
002600*                                 LEGALISERINSKOSTNAD                     
002700     03 PRFOERS              PIC S9(7)V9(2)      COMP-3.                  
002800*                                 FÖRSÄKRINGSPREMIE                       
002900     03 PRMOMS               PIC S9(7)V9(2)      COMP-3.                  
003000*                                 MERVÄRDESSKATT                          
003100     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
003200*                                 VALUTAKURS                              
003300     03 SUKRENTO             PIC S9(11)V9(2)     COMP-3.                  
003400*                                 KREDITERAT VARUVÄRDE NETTO              
003500     03 SUKRENOT             PIC S9(7)V9(2)      COMP-3.                  
003600*                                 KREDITNOTASUMMA                         
003700     03 SUKREUTL             PIC S9(11)V9(2)     COMP-3.                  
003800*                                 KREDITNOTASUMMA OMRÄKNAT I KUND         
003900*                                 ENS VALUTA                              
004000     03 KDANMORS             PIC X(2).                                    
004100*                                 ORSAK TILL LEVERANSANMÄRKNING           
004200     03 KVKREANT             PIC S9(7)           COMP-3.                  
004300*                                 KREDITERAT ANTAL                        
004400     03 FLLSBOK              PIC X.                                       
004500*                                 LAGERAVBOKNING                          
004600     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
004700*                                 ARTIKELPRIS NETTO                       
004800     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
004900*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005000     03 KDPRODSL             PIC S9(3)           COMP-3.                  
005100*                                 PRODUKTSLAG                             
005200     03 KDPSLLOC             PIC 9(2).                                    
005300*                                 PRODUKTSLAG LOKALT                      
005400     03 REVAT                PIC S9V9(4)         COMP-3.                  
005500*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
005600*** END OF VILMAII-COPY LENGTH= 110 BYTES                                 
