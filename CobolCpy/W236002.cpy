000100 01  W236002-CTX.                                                         
000200*                                 POSTEN INNEHÅLLER SORTDEL OCH           
000300*                                 DATADEL                                 
000400*                                                                         
000500     03 W236002-001-GRP.                                                  
000600        05 IDANSK            PIC S9(3)           COMP-3.                  
000700*                                 ANSKAFFARNUMMER                         
000800        05 IDLEVNR           PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000        05 PRIORITET         PIC S9              COMP-3.                  
001100*                                 PRIORITET VID FÖRSENAT AVROP            
001200        05 IDARTNR           PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400        05 IDPTYP            PIC X(3).                                    
001500*                                 POSTTYP                                 
001600     03 W236002-002-GRP.                                                  
001700        05 SEN-TIAVROP-AVS   PIC S9(5)           COMP-3.                  
001800*                                 AVROPSVECKA   (ÅÅVV)                    
001900        05 SEN-KVAVROP-DIFF  PIC S9(7)           COMP-3.                  
002000*                                 AVROPSKVANTITET                         
002100        05 TILEVBSK-AVS-002  PIC S9(5)           COMP-3.                  
002200*                                 AVSÄNDNINGSVECKA     (ÅÅVV)             
002300*                                 ENL LEVERANSBESKED                      
002400        05 KVAVIS-BSKKVAR    PIC S9(7)           COMP-3.                  
002500*                                 LEV. BESK. ANT. EFTER AVBOKNING         
002600*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
