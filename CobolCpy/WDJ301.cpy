000100 01  BEH-WDJ301.                                                          
000200*                                 SATSBEHOVREGISTER                       
000300*                                 SATSBEHOV SEGMENT                       
000400*                                 FYSISK NYCKEL: WDJ301KY                 
000500*                                 (IDARTNR + IDARTNR-ING +                
000600*                                  TIBEHOV)                               
000700*                                 S÷KBEGREPP: IDARTNR, IDARTNRI,          
000800*                                 TIBEHOV                                 
000900     03 BEH-IDARTNR          PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 BEH-IDARTNR-ING      PIC S9(9)           COMP-3.                  
001300*                                 ING≈ENDE ARTIKELNUMMER                  
001400     03 BEH-TIBEHOV          PIC S9(5)           COMP-3.                  
001500*                                 BEHOVSVECKA           (≈≈VV)            
001600     03 BEH-KVBEHOV          PIC S9(7)V9(2)      COMP-3.                  
001700*                                 BEHOVSSTORLEK                           
001800*** END COPY WDJ301CCC0  LENGTH=18                                        
