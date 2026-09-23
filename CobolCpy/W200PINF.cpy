000100 01  PINF-W200PINF.                                                       
000200*                                 LINK AREA FOR W200PINF                  
000300     03 PINF-INDATA.                                                      
000400        05 PINF-IDARTNR-IN   PIC 9(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 PINF-UTDATA.                                                      
000700        05 PINF-BEART-ENG    PIC X(25).                                   
000800*                                 ENGELSK ARTIKELBENÄMNING                
000900        05 PINF-KDERS        PIC S9(3)           COMP-3.                  
001000*                                 ERSÄTTNINGSKOD                          
001100        05 PINF-KDAVT        PIC S9              COMP-3.                  
001200*                                 AVTALSMÄRKNING                          
001300        05 PINF-IDANSK       PIC S9(3)           COMP-3.                  
001400*                                 ANSKAFFARNUMMER                         
001500        05 PINF-IDNAMN-ANSK  PIC X(40).                                   
001600*                                 NAMN                                    
001700        05 PINF-KDFARLIG     PIC S9              COMP-3.                  
001800*                                 KOD FÖR FARLIGT GODS                    
001900        05 PINF-IDLEVNR-MFG  PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100        05 PINF-IDLEVNR-SHIP PIC X(5).                                    
002200*                                 SKEPPANDE LEVERANTÖR                    
002300        05 PINF-KIT-DATA     OCCURS 100 TIMES.                            
002400           07 PINF-IDARTNR-SATS                                           
002500                             PIC 9(9).                                    
002600*                                 ARTIKELNUMMER FÖR SATS                  
002700           07 PINF-REANTPSA  PIC S9(2)V9(3)      COMP-3.                  
002800*                                 ANTAL PER SATS                          
002900     03 PINF-KDSVAR          PIC X.                                       
003000      88 PINF-KDSVAR-OK      VALUE ' '.                                   
003100      88 PINF-KDSVAR-FEL     VALUE 'F'.                                   
003200*                                                       KDSVAR-88         
003300*                                 SVARSKOD FRÅN SUBPROGRAM                
003400     03 PINF-IDMSG-ERROR     PIC X(3).                                    
003500*                                 FELMEDDELANDE ID                        
003600     03 PINF-IDELMT-ERROR    PIC X(16).                                   
003700*                                 DATAELEMENTIDENTITET                    
003800     03 PINF-FEL-TEXT        PIC X(25).                                   
003900*** END OF VILMAII-COPY LENGTH= 1335 BYTES                                
