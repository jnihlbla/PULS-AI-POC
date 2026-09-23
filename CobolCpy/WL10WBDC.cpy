000100 01  WBDC-AREA.                                                           
000200*                                 LINK AREA TO PGM WL10WBDC.              
000300*                                 IT RETURNS INFO ABOUT A DC              
000400*                                 - DOES IT USE PULS WEB?                 
000500*                                 - WHICH MANAGMENT FOLLOW-UP             
000600*                                   GROUP DOES IT BELONG TO?              
000700*                                                                         
000800*                                 INPUT ARGUMEN:                          
000900*                                  IDDC         THE DC                    
001000*                                                                         
001100*                                 OUTPUT ARGUMENTS:                       
001200*                                  FLWEBDC      YES IF WEB DC             
001300*                                  KDMFUP       OUTPUT MSG TEXT           
001400*                                                                         
001500     03 WBDC-IDDC            PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 WBDC-FLWEBDC         PIC X.                                       
001900*                                 DC MED WEB GRÄNSSNITT                   
002000*                                 DC WITH WEB INTERFACE                   
002100     03 WBDC-KDMFUP          PIC X(2).                                    
002200*                                 RAPPORTGRUPP  MA/CN/PF/NA               
002300*                                 REPORT GROUP  MA/CN/PF/NA               
002400*** END OF VILMAII-COPY LENGTH= 5 BYTES                                   
