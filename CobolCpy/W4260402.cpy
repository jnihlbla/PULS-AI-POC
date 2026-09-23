000100 01  W4260402.                                                            
000200*                                 KVALITET FELLISTA                       
000300*                                 QUALITY CONTROL ERROR LISTING           
000400*                                 .                                       
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
002200     03 IDARTNR              PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400*                                 PART NUMBER                             
002500     03 BEART                PIC X(25).                                   
002600*                                 ARTIKELBENÄMNING                        
002700*                                 PART DESCRIPTION                        
002800     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
002900*                                 LAGEROMRÅDE                             
003000*                                 AREA                                    
003100     03 ADGANG               PIC S9(3)           COMP-3.                  
003200*                                 GÅNG                                    
003300*                                 AISLE                                   
003400     03 ADPLATS              PIC S9(5)           COMP-3.                  
003500*                                 LAGERPLATSNUMMER                        
003600*                                 LOCATION                                
003700     03 IDKVAFEL             PIC 9(2).                                    
003800*                                 KVALITET FELKOD FÖR ARTIKEL             
003900*                                 ERROR CODE FOR PARTNUMBER               
004000     03 KVKVAFPO             PIC S9(3)           COMP-3.                  
004100*                                 KVALITET POÄNG FÖR FELKOD               
004200*                                 QUALITY POINT FOR ERROR CODE            
004300     03 BEKVAFEL             PIC X(40).                                   
004400*                                 KVALITET FELKODSBETECKNING              
004500*                                 ERROR FOR ERROR CODE                    
004600     03 BEKVAFGR             PIC X(20).                                   
004700*                                 KVALITET ALLVARLIGHETSGRAD FELK         
004800*                                 OD                                      
004900*                                 QUALITY SERIOUS FOR ERROR CODE          
005000*** END OF VILMAII-COPY LENGTH= 120 BYTES                                 
