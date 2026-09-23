000100 01  FREF-W461011-CTX.                                                    
000200*                                 FAKTURA-REFERENS                        
000300*                                 TILL NOAC PT-011                        
000400     03 FREF-IDPTYP          PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 FREF-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 FREF-IDKUNDNR        PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 FREF-KDFAKTYP        PIC X.                                       
001100*                                 FAKTURATYP                              
001200     03 FREF-IDFAKT          PIC S9(7)           COMP-3.                  
001300*                                 FAKTURANUMMER                           
001400     03 FREF-KDSORT2         PIC S9(3)           COMP-3.                  
001500*                                 SORTERINGSFÄLT                          
001600     03 FREF-IDPRODNR        PIC S9(7)           COMP-3.                  
001700*                                 PRODUKTIONSNUMMER                       
001800     03 FREF-IDKOLLI         PIC S9(5)           COMP-3.                  
001900*                                 KOLLINUMMER                             
002000     03 FREF-IDARTNR         PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200     03 FREF-IDORDNR         PIC S9(7)           COMP-3.                  
002300*                                 ORDERNR             IDORDNR-002         
002400     03 FREF-TIORDREG        PIC S9(7)           COMP-3.                  
002500*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002600     03 FREF-KDORDKL         PIC S9              COMP-3.                  
002700*                                 ORDERKLASS                              
002800     03 FREF-BEKUNDRF        PIC X(15).                                   
002900*                                 KUNDENS REFERENS                        
003000     03 FREF-BEVOLREF        PIC X(10).                                   
003100*                                 VOLVO REFERENS                          
003200     03 FREF-BEVARREF        PIC X(10).                                   
003300*                                 VÅR REFERENS                            
003400     03 FREF-KDREFNOT        PIC X(2).                                    
003500*                                 FAKTURA NOTERINGAR                      
003600     03 FREF-KDFRAKT         PIC S9(3)           COMP-3.                  
003700*                                 FRAKTSÄTT DC TILL KUND                  
003800     03 FREF-IDTRPBO.                                                     
003900*                                 BOLLA-DOKUMENT IDENTITET                
004000        05 FREF-IDTRPBOT     PIC X.                                       
004100*                                 BOLLA-DOKUMENT TECKEN                   
004200        05 FREF-IDTRPBON     PIC S9(7)           COMP-3.                  
004300*                                 BOLLA-DOKUMENT NUMMER                   
004400     03 FREF-VKORDBTO-ORDER  PIC S9(6)V9(1)      COMP-3.                  
004500*                                 ORDERVIKT BRUTTO PER ORDER              
004600*** END OF VILMAII-COPY LENGTH= 86 BYTES                                  
