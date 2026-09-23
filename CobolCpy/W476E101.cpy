000100 01  HUV-W476E101.                                                        
000200*                                 TRANSPORTRELEASEREG.POST                
000300*                                 TRANSPORT                               
000400     03 HUV-IDPTYP           PIC X(4).                                    
000500*                                 POSTTYP              IDPTYP-004         
000600     03 HUV-IDSHIPM          PIC 9(7).                                    
000700*                                 SKEPPNINGSNUMMER                        
000800     03 HUV-IDDISTR          PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 HUV-IDKUNDNR         PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 HUV-IDDC             PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 HUV-IDTRPTNR         PIC S9(3)           COMP-3.                  
001500*                                 TRANSPORTIDENTITET                      
001600     03 HUV-IDLBBET          PIC X(12).                                   
001700*                                 LASTBÄRARBETECKNING                     
001800     03 HUV-SUORDV           PIC S9(9)V9(2)      COMP-3.                  
001900*                                 SUMMA ORDERVÄRDE                        
002000     03 HUV-KDVALISO         PIC X(3).                                    
002100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002200     03 HUV-VKORDBTO         PIC S9(6)V9(1)      COMP-3.                  
002300*                                 ORDERVIKT BRUTTO (KG)                   
002400     03 HUV-VLORDBTO         PIC S9(4)V9(3)      COMP-3.                  
002500*                                 ORDERVOLYM BRUTTO (M3)                  
002600     03 HUV-KVKOLLI          PIC S9(5)           COMP-3.                  
002700*                                 ANTAL KOLLI                             
002800     03 HUV-KDORDKL          PIC S9              COMP-3.                  
002900*                                 ORDERKLASS                              
003000     03 HUV-IDLEVNR          PIC X(5).                                    
003100*                                 LEVERANTÖRNUMMER                        
003200*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  
