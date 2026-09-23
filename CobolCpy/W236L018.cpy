000100 01  W236L018.                                                            
000200*                                 HÄMTAR SPRÅKKOD,BENÄMNING OCH           
000300*                                 LEV:S BENÄMNING KOMM. MELLAN            
000400*                                 W2362000 W2362010.                      
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-LEVERANTORS-REG VALUE +106.                                  
000700      88 LAS-BEN-REG         VALUE +107.                                  
000800      88 LAS-CROSS-INDEX     VALUE +108.                                  
000900     03 KDSVAR               PIC X.                                       
001000      88 LEVERANTORS-REG-FINNS                                            
001100                             VALUE ' '.                                   
001200      88 CROSS-INDEX-FINNS   VALUE ' '.                                   
001300*                                 SVAR FRÅN SUBPROGRAM                    
001400     03 NYCKLAR.                                                          
001500        05 IDLEVNR           PIC S9(5)           COMP-3.                  
001600*                                 LEVERANTÖRNUMMER                        
001700        05 IDARTNR           PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900        05 IDBENR            PIC S9              COMP-3.                  
002000*                                 BENÄMNINGSNUMMER                        
002100        05 IDSKYLT           PIC X(3).                                    
002200         88 TYSKA            VALUE 'D  '.                                 
002300         88 SPANSKA          VALUE 'E  '.                                 
002400         88 FRANSKA          VALUE 'F  '.                                 
002500         88 ENGELSKA         VALUE 'GB '.                                 
002600         88 ITALENSKA        VALUE 'I  '.                                 
002700         88 HOLLANDSKA       VALUE 'NL '.                                 
002800         88 PORTUGISISKA     VALUE 'P  '.                                 
002900         88 SVENSKA          VALUE 'S  '.                                 
003000         88 FINSKA           VALUE 'SF '.                                 
003100         88 AMERIKANSKA      VALUE 'USA'.                                 
003200*                                 NATIONALITETSTECKEN                     
003300     03 IO-AREA.                                                          
003400*                                 WDF1                                    
003500        05 KDSPRAK           PIC S9              COMP-3.                  
003600         88 KDSPRAK-SVENSKA  VALUE +0.                                    
003700         88 KDSPRAK-ENGELSKA VALUE +1.                                    
003800         88 KDSPRAK-FRANSKA  VALUE +2.                                    
003900         88 KDSPRAK-SPANSKA  VALUE +3.                                    
004000         88 KDSPRAK-TYSKA    VALUE +4.                                    
004100*                                 SPRÅKKOD                                
004200        05 BEART             PIC X(25).                                   
004300*                                 ARTIKELBENÄMNING                        
004400        05 BELEV             PIC X(30).                                   
004500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
004600*** END COPY W236L018C0  LENGTH=71                                        
