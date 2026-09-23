000100 01  W236L011.                                                            
000200*                                 COPYTEXT FÖR LÄSNING AV                 
000300*                                 LEV-INFO.  KOMM. MELLAN                 
000400*                                 W2362000 OCH W2362010.                  
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-LEV-INFO        VALUE +101.                                  
000700     03 KDSVAR               PIC X.                                       
000800      88 LEV-INFO-FINNS      VALUE ' '.                                   
000900*                                 SVAR FRÅN SUBPROGRAM                    
001000     03 IO-AREA.                                                          
001100*                                 WDF101                                  
001200        05 IDLEVNR           PIC S9(5)           COMP-3.                  
001300*                                 LEVERANTÖRNUMMER                        
001400        05 KDSPRAK           PIC S9              COMP-3.                  
001500         88 KDSPRAK-SVENSKA  VALUE +0.                                    
001600         88 KDSPRAK-ENGELSKA VALUE +1.                                    
001700         88 KDSPRAK-FRANSKA  VALUE +2.                                    
001800         88 KDSPRAK-SPANSKA  VALUE +3.                                    
001900         88 KDSPRAK-TYSKA    VALUE +4.                                    
002000*                                 SPRÅKKOD                                
002100        05 KDGK              PIC S9              COMP-3.                  
002200*                                 GODSMOTTAGAREKOD                        
002300        05 KVDAGAR-TTC1      PIC S9(3)           COMP-3.                  
002400*                                 DAGAR TULL- & TRANSPORT-TID  C1         
002500        05 KVDAGAR-TTC2      PIC S9(3)           COMP-3.                  
002600*                                 DAGAR TULL- & TRANSPORT-TID  C2         
002700        05 BELEV             PIC X(35).                                   
002800*                                 LEVERANTÖRSNAMN                         
002900        05 ADLEV-RAD1        PIC X(35).                                   
003000*                                 LEVERANTÖRENS GATUADRESS 1              
003100        05 ADLEV-RAD2        PIC X(35).                                   
003200*                                 LEVERANTÖRENS GATUADRESS 2              
003300        05 ADLEV-ORT         PIC X(40).                                   
003400*                                 LEVERANTÖRSADRESS ORT                   
003500        05 ADLEVLND          PIC X(20).                                   
003600*                                 LEVERANTÖRSADRESS LAND                  
003700*** END COPY W236L011C0  LENGTH=177                                       
