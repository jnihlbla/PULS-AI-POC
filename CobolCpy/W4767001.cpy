000100 01  BILL-W4767001.                                                       
000200*                                 RAD FÖR FAKTURATRANSAR FRÅN BIL         
000300*                                 L-IT                                    
000400     03 BILL-IDFAKT          PIC S9(7)           COMP-3.                  
000500*                                 FAKTURANUMMER                           
000600     03 BILL-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 BILL-IDKUNDNR        PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 BILL-KDFAKTYP        PIC X.                                       
001100*                                 FAKTURATYP                              
001200     03 BILL-IDARTNR         PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 BILL-BEART           PIC X(25).                                   
001500*                                 ARTIKELBENÄMNING                        
001600     03 BILL-IDKOLLI         PIC S9(5)           COMP-3.                  
001700*                                 KOLLINUMMER                             
001800     03 BILL-IDKUNDRF        PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000     03 BILL-KDARTURS        PIC X(2).                                    
002100*                                 ARTIKELURSPRUNGSKOD                     
002200     03 BILL-KVLEVART        PIC S9(7)           COMP-3.                  
002300*                                 LEVERERAT ANTAL STYCK                   
002400     03 BILL-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
002500*                                 ARTIKELPRIS NETTO                       
002600     03 BILL-KDVALISO        PIC X(3).                                    
002700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002800     03 BILL-VKARTNTO        PIC S9(4)V9(3)      COMP-3.                  
002900*                                 ARTIKELVIKT NETTO (KG)                  
003000     03 BILL-SUORDV-FAKT     PIC S9(9)V9(2)      COMP-3.                  
003100*                                 FAKTURERAT ORDERVÄRDE                   
003200     03 BILL-TIFAKT          PIC S9(7)           COMP-3.                  
003300*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003400*** END OF VILMAII-COPY LENGTH= 83 BYTES                                  
