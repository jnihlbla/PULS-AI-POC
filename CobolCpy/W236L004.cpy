000100 01  W236L004-CTX.                                                        
000200     03 KDCALL               PIC S9(3)           COMP-3.                  
000300      88 LAS-LEVERANTORS-REG VALUE +106.                                  
000400      88 LAS-BEN-REG         VALUE +107.                                  
000500      88 LAS-CROSS-INDEX     VALUE +108.                                  
000600     03 KDSVAR               PIC X.                                       
000700      88 LEVERANTORS-REG-FINNS                                            
000800                             VALUE ' '.                                   
000900      88 CROSS-INDEX-FINNS   VALUE ' '.                                   
001000*                                 SVAR FRÅN SUBPROGRAM                    
001100     03 W236L004-001-GRP.                                                 
001200        05 IDLEVNR           PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400        05 IDARTNR           PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600        05 IDBENR            PIC S9              COMP-3.                  
001700*                                 BENÄMNINGSNUMMER                        
001800        05 IDSKYLT           PIC X(3).                                    
001900*                                 NATIONALITETSTECKEN                     
002000*                                 SPRÅKIDENTIFIKATION                     
002100     03 W236L004-002-GRP.                                                 
002200*                                 WDF1                                    
002300        05 KDSPRAK           PIC S9              COMP-3.                  
002400         88 KDSPRAK-SVENSKA  VALUE +0.                                    
002500         88 KDSPRAK-ENGELSKA VALUE +1.                                    
002600         88 KDSPRAK-FRANSKA  VALUE +2.                                    
002700         88 KDSPRAK-SPANSKA  VALUE +3.                                    
002800         88 KDSPRAK-TYSKA    VALUE +4.                                    
002900         88 KDSPRAK-ITALIENSKA                                            
003000                             VALUE +5.                                    
003100*                                 SPRÅKKOD                                
003200        05 BEART             PIC X(25).                                   
003300*                                 ARTIKELBENÄMNING                        
003400        05 BELEV             PIC X(30).                                   
003500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
003600*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
