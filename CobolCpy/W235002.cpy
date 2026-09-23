000100 01  W235002.                                                             
000200*                                 TRANS MELLAN W23506 OCH W23504.         
000300*                                                                         
000400     03 ID.                                                               
000500*                                                                         
000600        05 IDFTG             PIC 9(2).                                    
000700*                                 FÖRETAGSID EKONOM REDOVISNING           
000800        05 IDLEVNR           PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000     03 DATUM.                                                            
001100        05 TIAARP            PIC S9(5)           COMP-3.                  
001200*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
001300*                                 12 PER ÅR                               
001400     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001500*                                 PRODUKTSLAG                             
001600     03 DATA.                                                             
001700        05 KVSENLEV          PIC S9(5)           COMP-3.                  
001800*                                 ANTAL FÖRSENADE LEVERANSER              
001900        05 KVTIDLEV          PIC S9(5)           COMP-3.                  
002000*                                 ANTAL FÖRTIDIGA LEVERANSER              
002100        05 KVLEV             PIC S9(5)           COMP-3.                  
002200*                                 ANTAL LEVERANSER                        
002300        05 MATVARDE          PIC S9V9(2)         COMP-3.                  
002400*                                 MÄTVÄRDE LEVERANTÖRSUPPFÖLJN.           
002500        05 SUSENLEV          PIC S9(9)V9(2)      COMP-3.                  
002600*                                 VÄRDE FÖRSENADE LEVERANSER              
002700        05 SUTIDLEV          PIC S9(9)V9(2)      COMP-3.                  
002800*                                 VÄRDE FÖRTIDIGA LEVERANSER              
002900        05 SULEV             PIC S9(9)V9(2)      COMP-3.                  
003000*                                 VÄRDE PLANERADE LEVERANSER              
003100*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
