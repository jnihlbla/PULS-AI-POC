000100 01  001-W4766401.                                                        
000200*                                 HEADER FÖR FAKTURATRANSAR FRÅN          
000300*                                 BILL-IT                                 
000400     03 001-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 001-IDFAKT           PIC S9(7)           COMP-3.                  
000700*                                 FAKTURANUMMER                           
000800     03 001-IDDISTR          PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 001-IDKUNDNR         PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 001-IDDC             PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 001-IDKUNDRF         PIC X(10).                                   
001500*                                 KUNDENS REFERENS (ORDERID)              
001600     03 001-IDKOLLI          PIC S9(5)           COMP-3.                  
001700*                                 KOLLINUMMER                             
001800     03 001-TIFAKT           PIC S9(7)           COMP-3.                  
001900*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002000     03 001-IDLEVNR          PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200     03 001-KDFRAKT          PIC S9(3)           COMP-3.                  
002300*                                 FRAKTSÄTT DC TILL KUND                  
002400     03 001-KDVALISO         PIC X(3).                                    
002500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002600     03 001-PRKURS           PIC S9(6)V9(5)      COMP-3.                  
002700*                                 VALUTAKURS                              
002800     03 001-KDORDKL-MAX      PIC S9              COMP-3.                  
002900*                                 HÖGSTA ORDERKLASS I SKEPPNING           
003000     03 001-IDSHIPM          PIC 9(7).                                    
003100*                                 SKEPPNINGSNUMMER                        
003200     03 001-IDDC-LEV         PIC X(2).                                    
003300*                                 LEVERERANDE DC I EXPORTFLÖDET           
003400*** END OF VILMAII-COPY LENGTH= 59 BYTES                                  
