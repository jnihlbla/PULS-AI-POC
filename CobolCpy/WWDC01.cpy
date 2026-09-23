000100*** EDIT ALLOWED                                                          
000200******************************************************************        
000300**                                                                        
000402**                     W W D C 0 1                                        
000500**                                                                        
000600**   USAGE :                                                              
000700**                                                                        
000800**         THIS COPY TEXT CONTAINS ALL THE VALID DC CODES                 
000901**         FOR DC WITH LDC CLEARING OF REPAIR ORDER                       
001000**                                                                        
001700**                                                                        
001800**   IMPORTANT :                                                          
001900**                                                                        
002000**         IF THERE IS ANY CHANGE MADE TO THIS COPYTEXT, THEN ALL         
002100**         PROGRAMS THAT HAS THIS COPYTEXT MUST BE RECOMPILED.            
002110**         ... AND THE SAME DC'S TO CONSTANT W412PD02                     
002200**                                                                        
002300******************************************************************        
002400                                                                          
002500*                                                                         
002800 01  DC01-IDDC             PIC X(2).                                      
002900*                                                                         
420002       88  REPAIR-CLEARING     VALUE  '1A' THRU '1Z'                      
430001                                      '3A' '3B' '3C'                      
430002                                      '3E' '3G' '3H'                      
430003                                      '3J' '3K'                           
430005                                      '3L' '3M' '3N'                      
430006                                      '3O' '3P' '3R'                      
430007                                      '3S' '3T'.                          
430009       88  REPAIR-CLEARING-SDC VALUE  '21' '24' '25'                      
430010                                      '26'.                               
520000*                                                                         
866601*** END COPY WWDC01    LENGTH=2                                           
