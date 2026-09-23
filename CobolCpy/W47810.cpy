000100 01  W47810.                                                              
000200*                                 POSTBESKRIVNING FÖR LISTA FÖR           
000300*                                 FÖRSENADE ORDER                         
000400     03 IDTTYP               PIC X(3).                                    
000500*                                 TRANSAKTIONSTYP                         
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 KDPERSON             PIC S9(3)           COMP-3.                  
001100*                                 PERSONKOD                               
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 IDKUNDRF             PIC X(10).                                   
001500*                                 KUNDENS REFERENS (ORDERID)              
001600     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001700*                                 FRAKTSÄTT C1-C2 TILL KUND               
001800     03 KDORDKL              PIC S9              COMP-3.                  
001900*                                 ORDERKLASS                              
002000     03 TIREF1               PIC S9(7)           COMP-3.                  
002100*                                 KUNDREFERENS-DATUM 1   (ÅÅMMDD)         
002200     03 VKORDNTO             PIC S9(6)V9(1)      COMP-3.                  
002300*                                 ORDERVIKT NETTO (KG)                    
002400     03 VLORDNTO             PIC S9(4)V9(3)      COMP-3.                  
002500*                                 ORDERVOLYM NETTO (M3)                   
002600     03 TIORDREG             PIC S9(7)           COMP-3.                  
002700*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002800*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  
