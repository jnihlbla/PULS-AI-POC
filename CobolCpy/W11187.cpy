000100 01  W11187.                                                              
000200*                                 CPY-TEXT TILL W11187-FILEN              
000300*                                 SKAPAS UTIFRÅN W91042 INFÖR             
000400*                                 PROGRAMMET W11188                       
000500     03 IDPROJ               PIC X(4).                                    
000600*                                 PARTS PROJEKTIDENTITET                  
000700     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
000800*                                 FUNKTIONSGRUPP                          
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 BEART-USA            PIC X(25).                                   
001200*                                 AMERIKANSK ART.BENÄMNING                
001300     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001400*                                 PRODUKTSLAG                             
001500     03 KDBPSR               PIC S9              COMP-3.                  
001600*                                 BASLAGERFÖRSLAGSNIVÅ                    
001700     03 TIFINLV              PIC S9(5)           COMP-3.                  
001800*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
001900     03 IDARTNR-MOTSV        PIC S9(9)           COMP-3.                  
002000*                                 MOTSVARANDE ARTIKEL                     
002100     03 KDUART               PIC X.                                       
002200*                                 UNDANTAGSARTIKEL                        
002300     03 KDFARLIG             PIC S9              COMP-3.                  
002400*                                 KOD FÖR FARLIGT GODS                    
002500     03 FLLSRDEL             PIC X.                                       
002600*                                 LEVERERAS SOM RESDEL                    
002700     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
002800*                                 ARTIKELSTANDARDPRIS                     
002900     03 KDPSLLOC             PIC 9(2).                                    
003000*                                 PRODUKTSLAG LOKALT                      
003100     03 KDERS                PIC S9(3)           COMP-3.                  
003200*                                 ERSÄTTNINGSKOD                          
003300     03 KDERS-UTG            PIC S9(3)           COMP-3.                  
003400*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
003500     03 FLERS                PIC X.                                       
003600*                                 TILLKOMMANDE ARTIKEL ?                  
003700     03 FLJANEJ-US-CAN       PIC X.                                       
003800*                                 JA/NEJ-FLAGGA                           
003900     03 FLJANEJ-JPN          PIC X.                                       
004000*                                 JA/NEJ-FLAGGA                           
004100     03 FLJANEJ-AUS          PIC X.                                       
004200*                                 JA/NEJ-FLAGGA                           
004300*** END OF VILMAII-COPY LENGTH= 66 BYTES                                  
