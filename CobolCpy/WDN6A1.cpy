000100 01  MASA-WDN6A1.                                                         
000200*                                 KATALOG MASTERREGISTER                  
000300*                                 SECONDARY INDEX PÅ IDMODELL             
000400*                                 FYSISK NYCKEL: WDN6A1KY                 
000500*                                 (IDMODELL + IDARTNR +                   
000600*                                  IDFORDON + TIOMBRYT-9KOMPL)            
000700*                                 SECONDARY NYCKEL: WDN6ASEQ              
000800*                                 (IDMODELL + IDARTNR +                   
000900*                                  IDFORDON + TIOMBRYT-9KOMPL)            
001000     03 MASA-IDMODELL        PIC X(3).                                    
001100*                                 BILENS NUMERISKA MODELLBET.             
001200*                                 NUMERIC MODEL ID FOR A VECHICLE         
001300     03 MASA-IDARTNR         PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600     03 MASA-IDFORDON        PIC S9(3)           COMP-3.                  
001700*                                 FORDONSSLAG                             
001800*                                 VEHICLE TYPE ID                         
001900     03 MASA-TIOMBRYT-9KOMPL PIC S9(7)           COMP-3.                  
002000*                                 OMBRYTNINGSDATUM 9-KOMPLEMENT           
002100*                                 DATE OF PAGE MAKING UP   9-COMP         
002200     03 MASA-BEMODELL        PIC X(15).                                   
002300*                                 BILENS MODELLBESKRIVNING.               
002400*                                 MODEL DESCRIPTION FOR A VEHICLE         
002500*** END OF VILMAII-COPY LENGTH= 29 BYTES                                  
