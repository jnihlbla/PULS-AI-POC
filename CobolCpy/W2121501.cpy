000100 01  ERR-W2121501.                                                        
000200*                                 FELAKTIGA ARTIKLAR FRÅN EPIC            
000300*                                                                         
000400*                                 AGREEMENT/ANNULATION = PTYP BPA         
000500*                                                                         
000600*                                 CHANGE OF BUYER      = PTYP UBE         
000700*                                                                         
000800*                                 PROCES               = PTYP OOP         
000900*                                                                         
001000     03 ERR-IDPTYP           PIC X(3).                                    
001100*                                 POSTTYP                                 
001200     03 ERR-IDLEVNR-DC       PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 ERR-IDARTNR          PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600     03 ERR-IDDC             PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 ERR-IDLEVNR          PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000     03 ERR-IDLEVNR-SHIP     PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200     03 ERR-IDAVTAL          PIC X(12).                                   
002300*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
