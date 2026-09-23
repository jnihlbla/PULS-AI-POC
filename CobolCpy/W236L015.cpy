000100 01  W236L015.                                                            
000200*                                 COPYTEXT FÖR LÄSNING AV                 
000300*                                 ART-INFO OCH LEV-INFO.                  
000400     03 KDCALL               PIC S9(3)           COMP-3.                  
000500      88 LAS-ART-INFO        VALUE +101.                                  
000600      88 LAS-LEV-INFO        VALUE +102.                                  
000700     03 KDSVAR               PIC X.                                       
000800      88 ART-INFO-FINNS      VALUE ' '.                                   
000900      88 LEV-INFO-FINNS      VALUE ' '.                                   
001000*                                 SVAR FRÅN SUBPROGRAM                    
001100     03 IO-AREA.                                                          
001200*                                 ART-AREA  WDK6                          
001300        05 IDARTNR           PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
001600*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
001700        05 IDANSK            PIC S9(3)           COMP-3.                  
001800*                                 ANSKAFFARNUMMER                         
001900        05 IDPROD            PIC S9(3)           COMP-3.                  
002000*                                 PRODUKTKOD                              
002100        05 KDHF              PIC S9(3)           COMP-3.                  
002200*                                 HUVUDFÖRRÅDSMÄRKNING   KDHF-002         
002300        05 TIAVIDAT-SEN      PIC S9(7)           COMP-3.                  
002400*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
002500        05 IDAVINR-SEN       PIC S9(7)           COMP-3.                  
002600*                                 AVINUMMER SENASTE INLEVERANS            
002700        05 KVAVIS-SEN        PIC S9(7)           COMP-3.                  
002800*                                 SENAST AVISERAT ANTAL                   
002900        05 IDLEVNR-SEN       PIC S9(5)           COMP-3.                  
003000*                                 SENASTE LEVERANTÖR                      
003100        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
003200*                                 ARTIKELSTANDARDPRIS                     
003300        05 FLTOPP            PIC X.                                       
003400*                                 TOPP-200-ARTIKEL                        
003500        05 KDGK              PIC S9              COMP-3.                  
003600*                                 GODSMOTTAGAREKOD                        
003700        05 KVAKS-E           PIC S9(7)           COMP-3.                  
003800*                                 DEL AV EFR TILL ANDRA CLAGRET           
003900        05 KVAKS             PIC S9(7)           COMP-3.                  
004000*                                 ANKOMSTSALDO                            
004100        05 KVRESS            PIC S9(7)           COMP-3.                  
004200*                                 RESERVERAT ANTAL ARTIKLAR               
004300        05 KVLS              PIC S9(7)           COMP-3.                  
004400*                                 LAGERSALDO                              
004500        05 KVROS             PIC S9(7)           COMP-3.                  
004600*                                 RESTORDERSALDO                          
004700        05 KVSLAGER          PIC S9(7)           COMP-3.                  
004800*                                 SÄKERHETSLAGER                          
004900        05 IDLEVNR           PIC S9(5)           COMP-3.                  
005000*                                 LEVERANTÖRNUMMER                        
005100        05 KVBR              PIC S9(7)           COMP-3.                  
005200*                                 BESTÄLLNINGSREST                        
005300*** END COPY W236L015C0  LENGTH=69                                        
