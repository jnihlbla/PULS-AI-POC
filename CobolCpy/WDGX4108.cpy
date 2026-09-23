000100 01  4108-WDGX4108.                                                       
000200*                                 TIDSGRÄNSER BILDVISNING                 
000300*                                 LEVERANSANMÄRKNING/RETUR                
000400*                                 FYSISK NYCKEL: KDSEGKEY                 
000500     03 4108-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4108-KVDAGAR-LAP     PIC S9(3)           COMP-3.                  
000900*                                 MAX LIGGTID I DAGAR FÖR LEVANM          
001000*                                 INNAN RAD MARKERAS                      
001100     03 4108-KVDAGAR-RTAOSEA PIC S9(3)           COMP-3.                  
001200*                                 MAX LIGGTID I DGR FÖR AVISERAT          
001300*                                 OVERSEAS-RT INNAN RAD MARKERAS          
001400     03 4108-KVDAGAR-RTAOVR  PIC S9(3)           COMP-3.                  
001500*                                 MAX LIGGTID I DGR FÖR AVISERAT          
001600*                                 EUROPA-RT INNAN RAD MARKERAS            
001700     03 4108-KVDAGAR-RTM     PIC S9(3)           COMP-3.                  
001800*                                 MAX LIGGTID I DAGAR FÖR                 
001900*                                 MOTTAGET RT INNAN RAD MARKERAS          
002000     03 4108-KVDAGAR-RTP     PIC S9(3)           COMP-3.                  
002100*                                 MAX LIGGTID I DAGAR FÖR                 
002200*                                 PÅBÖRJAT RT INNAN RAD MARKERAS          
002300     03 4108-KVDAGAR-KLIARB  PIC S9(3)           COMP-3.                  
002400*                                 MAX LIGGTID I DAGAR FÖR KOLLI           
002500*                                 INNAN RAD MARKERAS                      
002600     03 4108-KVDAGAR-KLIAVV  PIC S9(3)           COMP-3.                  
002700*                                 ANTAL DAGAR INNAN ETT SAKNAT            
002800*                                 RETURKOLLI VISAS                        
002900*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
