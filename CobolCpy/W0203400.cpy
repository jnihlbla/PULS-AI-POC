000100 01  W0203400.                                                            
000200*                                 ÅFPOSTER KOMPLETTERADE MED INFO         
000300*                                 .                                       
000400*                                 MARKNADSSTATISTIK I RSREDO              
000500*                                                                         
000600     03 TIAAMM1              PIC S9(5)           COMP-3.                  
000700*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
000800*                                 12 PER ÅR                               
000900     03 TIAAMM2              PIC S9(5)           COMP-3.                  
001000*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
001100*                                 12 PER ÅR                               
001200     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400     03 BENAMN               PIC X(20).                                   
001500     03 IDSRSMKD             PIC 9(3).                                    
001600*                                 MARKNAD ENLIGT SRS                      
001700     03 SULVLV               PIC S9(13)V9(2)     COMP-3.                  
001800*                                 SUMMA LAGERVÄRDE LOKAL VALUTA           
001900     03 SUFSGLV-12           PIC S9(13)V9(2)     COMP-3.                  
002000*                                 SUMMA FSG I LOKAL VALUTA                
002100     03 SULVLV-PASSIVE       PIC S9(13)V9(2)     COMP-3.                  
002200*                                 SUMMA LAGERVÄRDE LOKAL VALUTA           
002300     03 SULVLV-DEAD          PIC S9(13)V9(2)     COMP-3.                  
002400*                                 SUMMA LAGERVÄRDE LOKAL VALUTA           
002500     03 RELEVBER             PIC S9(4)V9(1)      COMP-3.                  
002600*                                 LEVERANSBEREDSKAPSINDEX                 
002700     03 FILLERX12            PIC X(12).                                   
002800*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
