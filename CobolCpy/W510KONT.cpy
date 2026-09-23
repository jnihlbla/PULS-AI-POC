000100 01  KONT-W510KONT.                                                       
000200*                                 LÄNKAREA FÖR KONTO- OCH ANALYS-         
000300*                                 NUMMERBESTÄMNING FÖR PEDAL              
000400*                                                                         
000500*                                 FYLL I IDDISTR OCH KDCALL               
000600*                                 MÖJLIGA VAL PÅ KDCALL FÖR ATT           
000700*                                 HÄMTA IDKONTO OCH IDANALYS              
000800*                                 PRLEGKST      KDCALL =  3               
000900*                                 PRFRAKT       KDCALL =  4               
001000*                                 PRFOERS       KDCALL =  5               
001100*                                 PRAVDRAG      KDCALL =  6               
001200*                                                                         
001300*                                 SVAR: IDKONTO/IDANALYS                  
001400*                                 OM EJ TRÄFF ÄR DESSA NOLL/BLANK         
001500*                                                                         
001600     03 KONT-KDCALL          PIC S9(3)           COMP-3.                  
001700*                                 ANROPSTYP                               
001800     03 KONT-IDDISTR         PIC S9(5)           COMP-3.                  
001900*                                 DISTRIKTNUMMER                          
002000     03 KONT-KDFRAKT         PIC S9(3)           COMP-3.                  
002100*                                 FRAKTSÄTT DC TILL KUND                  
002200     03 KONT-IDKST           PIC X(10).                                   
002300*                                 KOSTNADSSTÄLLE                          
002400     03 KONT-IDANALYS        PIC X(12).                                   
002500*                                 ANALYSNUMMER                            
002600     03 KONT-IDKONTO         PIC 9(10).                                   
002700*                                 KONTO                                   
002800*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  
