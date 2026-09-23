000100 01  AUTF-W403AUTF.                                                       
000200*                                 LÄNKAREA TILL W403AUTF - BILL-I         
000300*                                 T AUTOMATFAKTUROR                       
000400     03 AUTF-IDSHIPM         PIC 9(7).                                    
000500*                                 SKEPPNINGSNUMMER                        
000600     03 AUTF-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 AUTF-IDKUNDNR        PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 AUTF-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 AUTF-IDPRODNR        PIC S9(7)           COMP-3.                  
001300*                                 PRODUKTIONSNUMMER                       
001400     03 AUTF-PRFRAKT         PIC S9(7)V9(2)      COMP-3.                  
001500*                                 FRAKTKOSTNAD                            
001600     03 AUTF-TISKEPPN        PIC S9(7)           COMP-3.                  
001700*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
001800     03 AUTF-TISKPTID        PIC S9(7)           COMP-3.                  
001900*                                 SKEPPNINGSTID                           
002000     03 AUTF-IDPGM           PIC X(8).                                    
002100*                                 PROGRAM IDENTITET                       
002200*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
