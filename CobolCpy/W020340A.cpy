000100 01  W020340A.                                                            
000200*                                 ÅFPOSTER KOMPLETTERADE MED INFO         
000300*                                 .                                       
000400*                                 MARKNADSSTATISTIK I RSREDO              
000500*                                                                         
000600     03 DAAAPP1              PIC S9(7)           COMP-3.                  
000700*                                 ÅR - PLANERINGSPERIOD (ÅÅÅÅRP)          
000800     03 DAAAPP2              PIC S9(7)           COMP-3.                  
000900*                                 ÅR - PLANERINGSPERIOD (ÅÅÅÅRP)          
001000     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 BENAMN               PIC X(20).                                   
001300     03 IDSRSMKD             PIC 9(3).                                    
001400*                                 MARKNAD ENLIGT SRS                      
001500     03 SULVLV               PIC S9(13)V9(2)     COMP-3.                  
001600*                                 SUMMA LAGERVÄRDE LOKAL VALUTA           
001700     03 SUFSGLV-12           PIC S9(13)V9(2)     COMP-3.                  
001800*                                 SUMMA FSG I LOKAL VALUTA                
001900     03 SULVLV-PASSIVE       PIC S9(13)V9(2)     COMP-3.                  
002000*                                 SUMMA LAGERVÄRDE LOKAL VALUTA           
002100     03 SULVLV-DEAD          PIC S9(13)V9(2)     COMP-3.                  
002200*                                 SUMMA LAGERVÄRDE LOKAL VALUTA           
002300     03 RELEVBER             PIC S9(4)V9(1)      COMP-3.                  
002400*                                 LEVERANSBEREDSKAPSINDEX                 
002500     03 FILLERX10            PIC X(10).                                   
002600*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
