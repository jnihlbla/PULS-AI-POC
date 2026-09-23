000100 01  003-W4766403.                                                        
000200*                                 LINE   FÖR FAKTURATRANSAR FRÅN          
000300*                                 BILL-IT                                 
000400     03 003-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 003-IDFAKT           PIC S9(7)           COMP-3.                  
000700*                                 FAKTURANUMMER                           
000800     03 003-IDDISTR          PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 003-IDKUNDNR         PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 003-IDDC             PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 003-IDKUNDRF         PIC X(10).                                   
001500*                                 KUNDENS REFERENS (ORDERID)              
001600     03 003-IDKOLLI          PIC S9(5)           COMP-3.                  
001700*                                 KOLLINUMMER                             
001800     03 003-IDARTNR          PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 003-KVLEVART         PIC S9(7)           COMP-3.                  
002100*                                 LEVERERAT ANTAL STYCK                   
002200     03 003-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
002300*                                 ARTIKELPRIS NETTO                       
002400     03 003-PRAVCOST         PIC S9(7)V9(2)      COMP-3.                  
002500*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
002600     03 003-IDDC-LEV         PIC X(2).                                    
002700*                                 LEVERERANDE DC I EXPORTFLÖDET           
002800*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
