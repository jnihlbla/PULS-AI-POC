000100 01  W3715C.                                                              
000200*                                 UPPFÖLJNING AV FÖRSÄLJNING OCH          
000300*                                 BYTES I REGISTERFORM                    
000400*                                 LIGGER I 2 ÅR INNAN DET RENSAS          
000500*                                 UR REGISTRET                            
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
000900*                                 OBJEKTNUMMER                            
001000     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001100*                                 PRODUKTSLAG                             
001200     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001300*                                 FUNKTIONSGRUPP                          
001400     03 BEART                PIC X(25).                                   
001500*                                 ARTIKELBENÄMNING                        
001600     03 IDDISTR              PIC S9(5)           COMP-3.                  
001700*                                 DISTRIKTNUMMER                          
001800     03 KDMARK-BUDG          PIC S9(3)           COMP-3.                  
001900*                                 MARKNADSKOD BUDGET 96 MARKNADER         
002000     03 BEMARKN              PIC X(24).                                   
002100*                                 MARKNADSBENÄMNING                       
002200     03 TIFSGVV              PIC 9(7).                                    
002300*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
002400     03 SUARTFSG             PIC S9(9)V9(2)      COMP-3.                  
002500*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
002600*                                                                         
002700     03 SULEVANT             PIC S9(9)           COMP-3.                  
002800*                                 SUMMA LEVERERAT ANTAL                   
002900*                                 AV 1 ARTIKEL                            
003000     03 KVRETUR-URSP         PIC S9(7)           COMP-3.                  
003100*                                 ANTAL OBJEKT RETURER.                   
003200     03 KVRETUR-GODK         PIC S9(7)           COMP-3.                  
003300*                                 ANTAL GODKÄNDA BYTESOBJEKT              
003400     03 IDTABNR              PIC S9(3)           COMP-3.                  
003500*                                 TABELLNUMMER                            
003600     03 IDBYTRAP             PIC S9(7)           COMP-3.                  
003700*                                 RAPPORTNUMMER  BYTES                    
003800     03 IDDC                 PIC X(2).                                    
003900*                                 IDENTIFIERARE LAGER                     
004000*** END OF VILMAII-COPY LENGTH= 103 BYTES                                 
