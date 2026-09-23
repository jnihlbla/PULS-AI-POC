000100 01  CDCU-W200CDCU.                                                       
000200*                                 LINK AREA FOR W200CDCU                  
000300     03 CDCU-IDARTNR         PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 CDCU-KDSVAR          PIC X.                                       
000600      88 CDCU-KDSVAR-OK      VALUE ' '.                                   
000700      88 CDCU-KDSVAR-FEL     VALUE 'F'.                                   
000800*                                                       KDSVAR-88         
000900*                                 SVARSKOD FRÅN SUBPROGRAM                
001000     03 CDCU-IDMSG-ERROR     PIC X(3).                                    
001100*                                 FELMEDDELANDE ID                        
001200     03 CDCU-IDELMT-ERROR    PIC X(16).                                   
001300*                                 DATAELEMENTIDENTITET                    
001400     03 CDCU-FEL-TEXT        PIC X(25).                                   
001500*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
