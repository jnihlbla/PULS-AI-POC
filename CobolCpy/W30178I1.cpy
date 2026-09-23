000100 01  MID-W30178I1.                                                        
000200*                                 MID-COPYTEXT TILL PGM W30178            
000300*                                 RENOVATOR CONFIRMATION                  
000400     03 MID-IDDISTR          PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600*                                 DISTRICT NUMBER                         
000700     03 MID-DAORDREG         PIC 9(8).                                    
000800*                                 ORDERDATUM (≈≈≈≈MMDD)                   
000900*                                 ORDER DATE (YYYYMMDD)                   
001000     03 MID-IDORDER          PIC 9(7).                                    
001100*                                 VOLVO PARTS ORDERNUMMER                 
001200*                                 VOLVO PARTS ORDER NUMBER                
001300     03 MID-IDDIARAD         PIC 9(6).                                    
001400*                                 F÷RSTA RAD ATT VISA                     
001500*                                 FIRST LINE TO SHOW                      
001600     03 MID-KVDIARAD-MAX     PIC 9(6).                                    
001700*                                 ANTAL RADER ATT VISA                    
001800*                                 NUMBER OF LINES TO BE SHOWN             
001900     03 MID-FLVISA           PIC X.                                       
002000*                                 ALLMƒN FLAGGA                           
002100*                                 GENERAL FLAG                            
002200     03 MID-INPUT.                                                        
002300        05 MID-NYRAD.                                                     
002400           07 MID-IDARTNR-NY PIC 9(8).                                    
002500*                                 ARTIKELNUMMER                           
002600*                                 PART NUMBER                             
002700           07 MID-KVAVBART-NY                                             
002800                             PIC 9(7).                                    
002900*                                 AVBOKAT ANTAL ARTIKLAR                  
003000*                                 ALLOCATED QUANTITY                      
003100        05 MID-TABELLINPUT.                                               
003200           07 MID-TABELLRAD  OCCURS 500 TIMES.                            
003300              09 MID-IDARTNR-RAD                                          
003400                             PIC 9(8).                                    
003500*                                 ARTIKELNUMMER                           
003600*                                 PART NUMBER                             
003700              09 MID-IDRADNR-RAD                                          
003800                             PIC 9(4).                                    
003900*                                 RADNUMMER                               
004000*                                 LINE NO                                 
004100              09 MID-KVAVBART-RAD                                         
004200                             PIC 9(7).                                    
004300*                                 AVBOKAT ANTAL ARTIKLAR                  
004400*                                 ALLOCATED QUANTITY                      
004500*** END OF VILMAII-COPY LENGTH= 9547 BYTES                                
