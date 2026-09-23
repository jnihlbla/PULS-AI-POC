000100 01  W6120202.                                                            
000200*                                 REFILL KOLLIN   SOM                     
000300*                                 ÄR FAKTURERADE HOS CDC                  
000400*                                 MEN EJ MOTTAGNA HOS SDC                 
000500*                                 DVS R30                                 
000600     03 BEART                PIC X(25).                                   
000700*                                 ARTIKELBENÄMNING                        
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDFAKT               PIC S9(7)           COMP-3.                  
001300*                                 FAKTURANUMMER                           
001400     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001500*                                 KOLLINUMMER                             
001600     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800     03 IDKUNDRF             PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000     03 IDLBBET              PIC X(12).                                   
002100*                                 LASTBÄRARBETECKNING                     
002200     03 KVAVIS               PIC S9(7)           COMP-3.                  
002300*                                 AVISERAT ANTAL                          
002400     03 TIAVIDAT             PIC S9(7)           COMP-3.                  
002500*                                 AVISERINGSDATUM (YYMMDD)                
002600*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
