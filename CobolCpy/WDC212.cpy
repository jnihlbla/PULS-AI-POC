000100 01  KAM-WDC212.                                                          
000200*                                 PRIS-RABATT REGISTER                    
000300*                                 KAMPANJRABATTINFORMATION                
000400*                                 FYSISK NYCKEL: WDC212KY                 
000500*                                  (KDARTKAM DASTADAT)                    
000600     03 KAM-KDARTKAM         PIC 9(5).                                    
000700*                                 TRANSFER KOD                            
000800*                                 TRANSFER CONDITION CODE                 
000900     03 KAM-DASTADAT         PIC 9(8).                                    
001000*                                 GENERELLT STARTDATUM                    
001100*                                 GENERAL START DATE                      
001200     03 KAM-TISTODAT         PIC S9(7)           COMP-3.                  
001300*                                 GENERELLT STOPPDATUM                    
001400*                                 GENERAL STOP DATE YYMMDD                
001500     03 KAM-REARTRAB-DO      PIC S9(2)V9(2)      COMP-3.                  
001600*                                 ARTIKELRABATT DAGORDER                  
001700*                                 PARTS DISCOUNT DAYORDER                 
001800     03 KAM-REARTRAB-BULK    PIC S9(2)V9(2)      COMP-3.                  
001900*                                 ARTIKELRABATT BULKORDER                 
002000*                                 PARTS DISCOUNT STOCKORDER               
002100*** END OF VILMAII-COPY LENGTH= 23 BYTES                                  
