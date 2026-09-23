000100 01  W4260601.                                                            
000200*                                 POST  FÖR KVAL-KONTROLL-LISTA           
000300*                                 RECORD FOR QUAL-CONTROL-LISTING         
000400*                                                                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 IDKVAOMR             PIC X.                                       
000900*                                 KVALITET KONTROLLOMRÅDE                 
001000*                                 QUALITY CONTROL AREA                    
001100     03 BEKVAOMR             PIC X(10).                                   
001200*                                 KVALITET KONTROLLOMRÅDESNAMN            
001300*                                 QUALITY NAME OF CONTROL AREA            
001400     03 IDKVAGRP             PIC 9(3).                                    
001500*                                 KVALITET KONTROLLGRUPP                  
001600*                                 QUALITY CONTROL GROUP                   
001700     03 TIAARP               PIC S9(5)           COMP-3.                  
001800*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
001900*                                 12 PER ÅR                               
002000*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
002100*                                 12 PER YEAR                             
002200     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
002300*                                 LAGEROMRÅDE                             
002400*                                 AREA                                    
002500     03 ADGANG               PIC S9(3)           COMP-3.                  
002600*                                 GÅNG                                    
002700*                                 AISLE                                   
002800     03 ADPLATS              PIC S9(5)           COMP-3.                  
002900*                                 LAGERPLATSNUMMER                        
003000*                                 LOCATION                                
003100     03 IDARTNR              PIC S9(9)           COMP-3.                  
003200*                                 ARTIKELNUMMER                           
003300*                                 PART NUMBER                             
003400     03 KVLS                 PIC S9(7)           COMP-3.                  
003500*                                 LAGERSALDO                              
003600*                                 STOCK BALANCE                           
003700     03 BEART                PIC X(25).                                   
003800*                                 ARTIKELBENÄMNING                        
003900*                                 PART DESCRIPTION                        
004000     03 BEFT                 PIC S9(3)           COMP-3.                  
004100*                                 FÖRPACKNINGSTYP                         
004200*                                 PACKAGING TYPE                          
004300*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
