000100 01  W33522-SORT.                                                         
000200*                                 COPY-TEXT FÖR SORT I W33522             
000300     03 SORT-1               PIC X(3).                                    
000400     03 SORT-2               PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 SORT-3               PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 SORT-4               PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 IDPROMR.                                                          
001100*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001200        05 IDMARKBO          PIC X.                                       
001300*                                 MARKNADSBOLAGSKOD                       
001400*                                                                         
001500        05 IDPROMRN          PIC X(2).                                    
001600*                                 PRISOMRÅDE LÖPNUMMER                    
001700     03 IDARTNR              PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900     03 IDDISTR              PIC S9(5)           COMP-3.                  
002000*                                 DISTRIKTNUMMER                          
002100     03 BEART-SVE            PIC X(25).                                   
002200*                                 SVENSK ARTIKELBENÄMNING                 
002300     03 BEART-ENG            PIC X(25).                                   
002400*                                 ENGELSK ARTIKELBENÄMNING                
002500     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002600*                                 PRODUKTSLAG                             
002700     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
002800*                                 FUNKTIONSGRUPP                          
002900     03 MO-NOR-PRIS          PIC S9(7)V9(2)      COMP-3.                  
003000*                                 ARTIKELPRIS NETTO                       
003100     03 DO-NOR-PRIS          PIC S9(7)V9(2)      COMP-3.                  
003200*                                 ARTIKELPRIS NETTO                       
003300     03 PRARTBTO-MARK        PIC S9(7)V9(2)      COMP-3.                  
003400*                                 BRUTTOPRIS PER MARKNAD (FOB)            
003500     03 KDERS                PIC S9(3)           COMP-3.                  
003600*                                 ERSÄTTNINGSKOD                          
003700*** END OF VILMAII-COPY LENGTH= 101 BYTES                                 
