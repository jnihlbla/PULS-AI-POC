000100 01  W61244.                                                              
000200*                                 SDC REFILL AVVIKELSE VID INLEVE         
000300*                                 RANS                                    
000400     03 DAFAKT               PIC 9(8).                                    
000500*                                 FAKTURERINGSDATUM (ÅÅÅÅMMDD)            
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDDC-REC             PIC X(2).                                    
000900*                                 MOTTAGANDE LAGER                        
001000     03 IDDC-SEND            PIC X(2).                                    
001100*                                 SÄNDANDE LAGER                          
001200     03 IDFAKT               PIC S9(7)           COMP-3.                  
001300*                                 FAKTURANUMMER                           
001400     03 IDKUNDRF             PIC X(10).                                   
001500*                                 KUNDENS REFERENS (ORDERID)              
001600     03 IDLEVNR              PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800     03 KVANTAL              PIC S9(7)           COMP-3.                  
001900*                                 ANTAL                                   
002000     03 AVVIKELSETYP         PIC X(10).                                   
002100     03 PRARTBES-PR          PIC S9(7)V9(2)      COMP-3.                  
002200*                                 DETTA BESTÄLLNINGSPRIS (KR)             
002300*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  
