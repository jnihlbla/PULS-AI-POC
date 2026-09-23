000100 01  W4758301-CTX.                                                        
000200*                                                                         
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDDC-SEND            PIC X(2).                                    
000900*                                 SÄNDANDE LAGER                          
001000     03 IDDC-REC             PIC X(2).                                    
001100*                                 MOTTAGANDE LAGER                        
001200     03 IDFAKT               PIC S9(7)           COMP-3.                  
001300*                                 FAKTURANUMMER                           
001400     03 TIFAKT               PIC S9(7)           COMP-3.                  
001500*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
001600     03 IDDISTR              PIC S9(5)           COMP-3.                  
001700*                                 DISTRIKTNUMMER                          
001800     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001900*                                 KUNDNUMMER                              
002000     03 KDFRAKT              PIC S9(3)           COMP-3.                  
002100*                                 FRAKTSÄTT DC TILL KUND                  
002200     03 KDVALISO             PIC X(3).                                    
002300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002400     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
002500*                                 VALUTAKURS                              
002600     03 KDORDKL-MAX          PIC S9              COMP-3.                  
002700*                                 HÖGSTA ORDERKLASS I SKEPPNING           
002800     03 IDKUNDRF             PIC X(10).                                   
002900*                                 KUNDENS REFERENS (ORDERID)              
003000     03 IDKOLLI              PIC S9(5)           COMP-3.                  
003100*                                 KOLLINUMMER                             
003200     03 KDKOLLI              PIC X(8).                                    
003300*                                 KOLLIKOD                                
003400     03 KVAVIS               PIC S9(7)           COMP-3.                  
003500*                                 AVISERAT ANTAL                          
003600     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
003700*                                 ARTIKELPRIS NETTO                       
003800     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
003900*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
004000*** END OF VILMAII-COPY LENGTH= 74 BYTES                                  
