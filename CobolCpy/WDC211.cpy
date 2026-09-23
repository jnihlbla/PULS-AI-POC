000100 01  ART-WDC211.                                                          
000200*                                 PRIS-RABATT REGISTER                    
000300*                                 ARTIKELRABATTINFORMATION                
000400*                                 FYSISK NYCKEL: WDC211KY                 
000500*                                  (IDARTNR DASTADAT)                     
000600     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 ART-DASTADAT         PIC 9(8).                                    
001000*                                 GENERELLT STARTDATUM                    
001100*                                 GENERAL START DATE                      
001200     03 ART-TISTODAT         PIC S9(7)           COMP-3.                  
001300*                                 GENERELLT STOPPDATUM                    
001400*                                 GENERAL STOP DATE YYMMDD                
001500     03 ART-REARTRAB-DO      PIC S9(2)V9(2)      COMP-3.                  
001600*                                 ARTIKELRABATT DAGORDER                  
001700*                                 PARTS DISCOUNT DAYORDER                 
001800     03 ART-REARTRAB-BULK    PIC S9(2)V9(2)      COMP-3.                  
001900*                                 ARTIKELRABATT BULKORDER                 
002000*                                 PARTS DISCOUNT STOCKORDER               
002100*** END OF VILMAII-COPY LENGTH= 23 BYTES                                  
