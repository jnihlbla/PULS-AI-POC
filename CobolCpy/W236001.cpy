000100 01  W236001.                                                             
000200*                                 POSTEN INNEHÅLLER SORTDEL OCH           
000300*                                 LEVERANTÖRSDATA.                        
000400*                                                                         
000500     03 SORT-BGRP.                                                        
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
001600     03 DATA.                                                             
001700        05 BEART             PIC X(25).                                   
001800*                                 ARTIKELBENÄMNING                        
001900        05 TIAVIDAT-SEN      PIC S9(7)           COMP-3.                  
002000*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
002100        05 IDAVINR-SEN       PIC S9(7)           COMP-3.                  
002200*                                 AVINUMMER SENASTE INLEVERANS            
002300        05 KVAVIS-SEN        PIC S9(7)           COMP-3.                  
002400*                                 SENAST AVISERAT ANTAL                   
002500        05 BELEV             PIC X(30).                                   
002600*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
002700        05 NASTA-TIAVROP-AVS PIC S9(5)           COMP-3.                  
002800*                                 AVROPSVECKA   (ÅÅVV)                    
002900        05 NASTA-KVAVROP     PIC S9(7)           COMP-3.                  
003000*                                 AVROPSKVANTITET                         
003100*** END OF VILMAII-COPY LENGTH= 90 BYTES                                  
