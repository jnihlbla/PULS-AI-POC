000100 01  W212R23.                                                             
000200*                                 UPPDATERING AVTAL                       
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDAVTAL              PIC S9(13)          COMP-3.                  
000900*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
001000*                                 PPP   = INKÖPARNR (PREFIX)              
001100*                                 BBBBB = BESTÄLLARNR                     
001200*                                 SSS   = SUFFIX                          
001300     03 IDLEVNR-AVT          PIC X(5).                                    
001400*                                 LEVERANTÖR ENLIGT AVTAL                 
001500     03 TIAVTAL              PIC S9(7)           COMP-3.                  
001600*                                 AVTALSDATUM  (ÅÅMMDD)                   
001700     03 KVAVTANT             PIC S9(7)           COMP-3.                  
001800*                                 ÅRSANTAL AVTAL                          
001900     03 KDBEH-AVT            PIC S9              COMP-3.                  
002000*                                 BEHANDLINGSKOD AVTAL                    
002100     03 TENOT-AVTPRIS        PIC X(20).                                   
002200*                                 NOTERING KÖPVILLKOR                     
002300     03 IDLEVNR-SHIP         PIC X(5).                                    
002400*                                 SKEPPANDE LEVERANTÖR                    
002500*** END OF VILMAII-COPY LENGTH= 54 BYTES                                  
