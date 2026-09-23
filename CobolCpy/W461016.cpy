000100 01  AVST-W461016.                                                        
000200*                                 SALDO AVSTÄMMNING                       
000300*                                 TILL NOAC PT-016                        
000400     03 AVST-IDPTYP          PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 AVST-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 AVST-IDKUNDNR        PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 AVST-IDORDNR         PIC S9(7)           COMP-3.                  
001100*                                 ORDERNR             IDORDNR-002         
001200     03 AVST-IDARTNR         PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 AVST-REKSIFFR        PIC S9              COMP-3.                  
001500*                                 KONTROLLSIFFRA                          
001600     03 AVST-BEVOLREF        PIC X(10).                                   
001700*                                 VOLVO REFERENS                          
001800     03 AVST-KVBEART         PIC S9(7)           COMP-3.                  
001900*                                 BESTÄLLT ANTAL ARTIKLAR                 
002000     03 AVST-KVRO            PIC S9(7)           COMP-3.                  
002100*                                 ANTAL RESTNOTERADE ARTIKLAR             
002200     03 FILLER               PIC X(6).                                    
002300*** END COPY W461016CC0  LENGTH=44                                        
