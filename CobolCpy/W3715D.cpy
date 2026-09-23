000100 01  W3715D.                                                              
000200*                                 BYTESUPPFÖLJNINGSREGISTRET              
000300*                                                                         
000400*                                                                         
000500     03 IDARTNR              PIC Z(7)9.                                   
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 SEMI-1               PIC X.                                       
000900     03 IDARTNR-OBJ          PIC Z(7)9.                                   
001000*                                 OBJEKTNUMMER                            
001100     03 SEMI-2               PIC X.                                       
001200     03 KDPRODSL             PIC Z9.                                      
001300*                                 PRODUKTSLAG                             
001400*                                 PRODUCT GROUP                           
001500     03 SEMI-3               PIC X.                                       
001600     03 IDFKNGRP             PIC Z(3)9.                                   
001700*                                 FUNKTIONSGRUPP                          
001800*                                 FUNCTION GROUP                          
001900     03 SEMI-4               PIC X.                                       
002000     03 BEART                PIC X(25).                                   
002100*                                 ARTIKELBENÄMNING                        
002200*                                 PART DESCRIPTION                        
002300     03 SEMI-5               PIC X.                                       
002400     03 IDDISTR              PIC Z(3)9.                                   
002500*                                 DISTRIKTNUMMER                          
002600*                                 DISTRICT NUMBER                         
002700     03 SEMI-6               PIC X.                                       
002800     03 KDMARK-BUDG          PIC Z(3).                                    
002900*                                 MARKNADSKOD BUDGET 96 MARKNADER         
003000*                                 MARKET CODE BUDGET (96 MARKETS)         
003100     03 SEMI-7               PIC X.                                       
003200     03 TIFSGVV              PIC Z(7).                                    
003300*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
003400*                                 PART SALES WEEK                         
003500     03 SEMI-8               PIC X.                                       
003600     03 SULEVANT             PIC -(8)9.                                   
003700*                                 SUMMA LEVERERAT ANTAL                   
003800*                                 AV 1 ARTIKEL                            
003900*                                 SUMMARY DELIVERED OF AN ITEM            
004000     03 SEMI-9               PIC X.                                       
004100     03 KVRETUR-GODK         PIC Z(7).                                    
004200*                                 ANTAL GODKÄNDA BYTESOBJEKT              
004300*                                 QUANTITY APPROVED OBJECT                
004400     03 SEMI-10              PIC X.                                       
004500     03 IDTABNR              PIC Z(2)9.                                   
004600*                                 TABELLNUMMER                            
004700*                                 TABELNUMBER                             
004800     03 SEMI-11              PIC X.                                       
004900     03 IDBYTRAP             PIC Z(6)9.                                   
005000*                                 RAPPORTNUMMER  BYTES                    
005100*                                 REPORTNUMBER   EXCHANGE                 
005200     03 SEMI-12              PIC X.                                       
005300     03 IDDC                 PIC X(2).                                    
005400*                                 IDENTIFIERARE LAGER                     
005500*                                 WAREHOUSE IDENTIFIER                    
005600*** END OF VILMAII-COPY LENGTH= 101 BYTES                                 
