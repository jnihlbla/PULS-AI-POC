000100 01  W2216C.                                                              
000200*                                 LEVERANTÖRER FÖR ÖVERFÖRING             
000300*                                 VIA ODETTE LOCAL PO                     
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDLEVNR              PIC X(5).                                    
000700*                                 LEVERANTÖRNUMMER                        
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 KDPRODSL             PIC 9(3)            COMP-3.                  
001100*                                 PRODUKTSLAG        KDPRODSL-002         
001200     03 KDGK-SORT            PIC S9              COMP-3.                  
001300*                                 GODSMOTTAGAREKOD                        
001400     03 KDGK-URS             PIC S9              COMP-3.                  
001500*                                 GODSMOTTAGAREKOD                        
001600     03 IDLPLAN-LEV          PIC S9(7)           COMP-3.                  
001700*                                 LEVERANSPLANENR ODETTE                  
001800     03 IDLPLAN-ART          PIC S9(7)           COMP-3.                  
001900*                                 LEVERANSPLANENR ODETTE                  
002000     03 IDOVERFNR-LOP        PIC S9(5)           COMP-3.                  
002100*                                 ÖVERFÖRINGSNUMMER                       
002200     03 IDBEST               PIC S9(13)          COMP-3.                  
002300*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
002400*                                 PPP   = (PREFIX) INKÖPARNR              
002500*                                 BBBBBB= BESTÄLLARNR                     
002600*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
002700     03 IDKAT                PIC X(5).                                    
002800*                                 KATALOGBETECKNING                       
002900     03 IDLEVKND             PIC X(17).                                   
003000*                                 LEVERANTÖRENS KUNDIDENTITET             
003100     03 ADINPORT             PIC X(8).                                    
003200*                                 AVLASTNINGSPORT                         
003300     03 KVBEART              PIC S9(7)           COMP-3.                  
003400*                                 BESTÄLLT ANTAL STYCKEN                  
003500     03 KVDAGAR              PIC S9(3)           COMP-3.                  
003600*                                 ANTAL DAGAR                             
003700*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
