000100 01  RETL-W611RETL.                                                       
000200*                                 LÄNKAREA TILL W611RETL -                
000300*                                 UTSKRIFT AV RETURLISTA                  
000400     03 RETL-IDPRTLST        PIC X(8).                                    
000500*                                 LOGISK PRINTER+LISTA IDENTITET          
000600     03 RETL-IDPGM           PIC X(8).                                    
000700*                                 PROGRAM IDENTITET                       
000800     03 RETL-FLSLUT          PIC X.                                       
000900*                                 AVSLUTNINGSFLAGGA                       
001000     03 RETL-IDLEVNR-KOLLI   PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER KOLLI                  
001200     03 RETL-IDOKOLLI        PIC 9(9).                                    
001300*                                 ODETTE KOLLINUMMER                      
001400     03 RETL-IDINLVGN        PIC 9(3).                                    
001500*                                 VAGNSIDENTITET                          
001600     03 RETL-ADINLOMR        PIC X(4).                                    
001700*                                 INLEVERANSOMRÅDE                        
001800     03 RETL-ADINLOMR-NXT    PIC X(4).                                    
001900*                                 INLEVERANSOMRÅDE NÄSTA                  
002000     03 RETL-IDLEVNR         PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200     03 RETL-IDFS            PIC X(8).                                    
002300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002400     03 RETL-TIAVIDAT        PIC S9(7)           COMP-3.                  
002500*                                 AVISERINGSDATUM (YYMMDD)                
002600     03 RETL-IDRADNR-INL     PIC S9(5)           COMP-3.                  
002700*                                 RADNUMMER INLEVERANS                    
002800     03 RETL-IDRADNR         PIC S9(5)           COMP-3.                  
002900*                                 RADNUMMER                               
003000*** END OF VILMAII-COPY LENGTH= 65 BYTES                                  
