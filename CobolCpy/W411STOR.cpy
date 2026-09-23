000100 01  STOR-W411STOR.                                                       
000200*                                 LÄNKAREA TILL W411STOR -                
000300*                                 KONTROLLERA STORA UTTAG                 
000400     03 STOR-IDSYSTEM        PIC X(4).                                    
000500*                                 VOLVO VCCS SYSTEMNUMMER                 
000600     03 STOR-IDLEVNR         PIC X(5).                                    
000700*                                 LEVERANTÖRNUMMER                        
000800     03 STOR-IDKUNDRF-RO     PIC X(10).                                   
000900*                                 KUND REF PÅ RO                          
001000     03 STOR-BERADREF        PIC X(10).                                   
001100*                                 KUNDENS RADREFERENS                     
001200     03 STOR-FLFORBI         PIC X.                                       
001300*                                 FÖRBIORDERFLAGGA                        
001400     03 STOR-FLORDSPE        PIC X.                                       
001500*                                 SPECIALORDERFLAGGA                      
001600     03 STOR-FLOVRLEV        PIC X.                                       
001700*                                 ÖVERLEVERANS                            
001800     03 STOR-KDORDKL         PIC S9              COMP-3.                  
001900*                                 ORDERKLASS                              
002000     03 STOR-KDERS           PIC S9(3)           COMP-3.                  
002100*                                 ERSÄTTNINGSKOD                          
002200     03 STOR-KDPROTYP        PIC X.                                       
002300*                                 TYP AV PROFORMA                         
002400     03 STOR-KDVVKL          PIC S9              COMP-3.                  
002500*                                 VOLYMVÄRDESKLASS                        
002600     03 STOR-KVBEART-Q       PIC S9(7)           COMP-3.                  
002700*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002800     03 STOR-KVPB-SEP        PIC S9(6)V9(1)      COMP-3.                  
002900*                                 SEPARAT PERIODBEHOV                     
003000     03 STOR-KVSLUTKP        PIC S9(7)           COMP-3.                  
003100*                                 SLUTKÖPSSALDO                           
003200     03 STOR-RERF-ART        PIC S9V9(4)         COMP-3.                  
003300*                                 RANSONERINGSFAKTOR ARTIKEL              
003400     03 STOR-IDKAMPRF        PIC S9(7)           COMP-3.                  
003500*                                 KAMPANJREFERENS                         
003600     03 STOR-IDDISTR         PIC S9(5)           COMP-3.                  
003700*                                 DISTRIKTNUMMER                          
003800     03 STOR-KDORDBEK        PIC 9(2).                                    
003900*                                 ORDERBEKRÄFTELSEKOD                     
004000     03 STOR-KDPRODSL        PIC S9(3)           COMP-3.                  
004100*                                 PRODUKTSLAG                             
004200*** END OF VILMAII-COPY LENGTH= 63 BYTES                                  
