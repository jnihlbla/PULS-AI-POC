000100 01  IART-WDP321.                                                         
000200*                                 PERSONKODSREGISTER                      
000300*                                 ARTIKELINTERVALL PER IDPERSON           
000400*                                 FYSISK NYCKEL WDP321KY:                 
000500*                                 (IDLANDX2 + IDARTNR-FOM +               
000600*                                  IDARTNR-TOM)                           
000700     03 IART-IDLANDX2        PIC X(2).                                    
000800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000900     03 IART-IDARTNR-FOM     PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER FRÅN OCH MED              
001100     03 IART-IDARTNR-TOM     PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER TILL OCH MED              
001300*** END OF VILMAII-COPY LENGTH= 12 BYTES                                  
