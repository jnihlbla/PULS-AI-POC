000100 01  W11225.                                                              
000200*                                 COPYTEXT FÖR FILEN W11225               
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDARTNR-STR          PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDARTNR-ING          PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 W11225-003           OCCURS 10 TIMES.                             
001000*                                 COPYTEXT FÖR FILEN W11225               
001100        05 IDSKYLT           PIC X(3).                                    
001200*                                 NATIONALITETSTECKEN                     
001300*                                 SPRÅKIDENTIFIKATION                     
001400        05 BEART             PIC X(25).                                   
001500*                                 ARTIKELBENÄMNING                        
001600     03 IDTSPEC              PIC X(15).                                   
001700*                                 LIKARTADE STRUKTURER                    
001800     03 IDSTRTYP             PIC X.                                       
001900*                                 STRUKTURTYP                             
002000     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002100*                                 PRODUKTSLAG                             
002200     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
002300*                                 FUNKTIONSGRUPP                          
002400     03 TIREGDAT             PIC S9(7)           COMP-3.                  
002500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002600     03 KDERS                PIC S9(3)           COMP-3.                  
002700*                                 ERSÄTTNINGSKOD                          
002800     03 IDAO                 PIC X(10).                                   
002900*                                 ÄNDRINGSORDERNUMMER                     
003000     03 IDLEVNR              PIC X(5).                                    
003100*                                 LEVERANTÖRNUMMER                        
003200     03 IDBERED              PIC S9(3)           COMP-3.                  
003300*                                 BEREDARENUMMER                          
003400     03 BELEV                PIC X(35).                                   
003500*                                 LEVERANTÖRSNAMN                         
003600     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
003700*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
003800     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
003900*                                 ARTIKELNS SJÄLVKOSTNAD                  
004000     03 VKART                PIC S9(7)           COMP-3.                  
004100*                                 ARTIKELVIKT (G)                         
004200     03 REANTPSA             PIC S9(2)V9(3)      COMP-3.                  
004300*                                 ANTAL PER SATS                          
004400     03 BELEVART             PIC X(30).                                   
004500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
004600     03 KDSORT               PIC X(2).                                    
004700*                                 SORT-KOD                                
004800*** END OF VILMAII-COPY LENGTH= 421 BYTES                                 
