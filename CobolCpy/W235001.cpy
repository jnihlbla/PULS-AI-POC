000100 01  W235001.                                                             
000200*                                 REGISTERPOST                            
000300*                                 LEVERANSBEDÖMNINGSREGISTER.             
000400*                                                                         
000500     03 ID.                                                               
000600*                                                                         
000700        05 KDPRODSL          PIC S9(3)           COMP-3.                  
000800*                                 PRODUKTSLAG                             
000900        05 IDLEVNR           PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 DATUM.                                                            
001200        05 TIAARP            PIC S9(5)           COMP-3.                  
001300*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
001400*                                 12 PER ÅR                               
001500     03 DATA.                                                             
001600        05 KVSENLEV          PIC S9(5)           COMP-3.                  
001700*                                 ANTAL FÖRSENADE LEVERANSER              
001800        05 KVTIDLEV          PIC S9(5)           COMP-3.                  
001900*                                 ANTAL FÖRTIDIGA LEVERANSER              
002000        05 KVLEV             PIC S9(5)           COMP-3.                  
002100*                                 ANTAL LEVERANSER                        
002200        05 MATVARDE          PIC S9V9(2)         COMP-3.                  
002300*                                 MÄTVÄRDE LEVERANTÖRSUPPFÖLJN.           
002400        05 SUSENLEV          PIC S9(9)V9(2)      COMP-3.                  
002500*                                 VÄRDE FÖRSENADE LEVERANSER              
002600        05 SUTIDLEV          PIC S9(9)V9(2)      COMP-3.                  
002700*                                 VÄRDE FÖRTIDIGA LEVERANSER              
002800        05 SULEV             PIC S9(9)V9(2)      COMP-3.                  
002900*                                 VÄRDE PLANERADE LEVERANSER              
003000*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  
