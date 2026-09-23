000100 01  W47815.                                                              
000200*                                 POSTBESKRIVNING FÖR DAGLIG              
000300*                                 UPPFÖLJNINGSLISTA TILL RA               
000400     03 IDTTYP               PIC X(3).                                    
000500*                                 TRANSAKTIONSTYP                         
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001100*                                 PRODUKTIONSNUMMER                       
001200     03 IDKUNDRF             PIC X(10).                                   
001300*                                 KUNDENS REFERENS (ORDERID)              
001400     03 IDDC                 PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001700*                                 KOLLINUMMER                             
001800     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001900*                                 FRAKTSÄTT C1-C2 TILL KUND               
002000     03 VKORDNTO             PIC S9(6)V9(1)      COMP-3.                  
002100*                                 ORDERVIKT NETTO (KG)                    
002200     03 VLORDNTO             PIC S9(4)V9(3)      COMP-3.                  
002300*                                 ORDERVOLYM NETTO (M3)                   
002400     03 VKORDBTO             PIC S9(6)V9(1)      COMP-3.                  
002500*                                 ORDERVIKT BRUTTO (KG)                   
002600     03 VLORDBTO             PIC S9(4)V9(3)      COMP-3.                  
002700*                                 ORDERVOLYM BRUTTO (M3)                  
002800     03 IDLOTNR              PIC S9(3)           COMP-3.                  
002900*                                 VAGN-NUMMER                             
003000     03 KDORDLOT             PIC X(2).                                    
003100*                                 ORDERLOTTSALTERNATIV                    
003200     03 ADLEVPL              PIC S9(3)           COMP-3.                  
003300*                                 LEVERANSPLATS                           
003400     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
003500*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
