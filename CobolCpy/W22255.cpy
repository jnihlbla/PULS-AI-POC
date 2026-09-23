000100 01  W22255.                                                              
000200*                                 REGISTER FÖR UPPDAT AV                  
000300*                                 KVPB-JUST                               
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDPROJ               PIC X(4).                                    
000700*                                 PARTS PROJEKTIDENTITET                  
000800     03 PBJUST               OCCURS 2 TIMES.                              
000900        05 KVPB-JUST         PIC S9(6)V9(1)      COMP-3.                  
001000*                                 PERIODBEHOVSJUSTERING                   
001100        05 TIPBJUST          PIC S9(5)           COMP-3.                  
001200*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
001300     03 KDERS                PIC S9(3)           COMP-3.                  
001400*                                 ERSÄTTNINGSKOD                          
001500     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001600*                                 PRODUKTSLAG                             
001700     03 IDANSK               PIC S9(3)           COMP-3.                  
001800*                                 ANSKAFFARNUMMER                         
001900*** END OF VILMAII-COPY LENGTH= 29 BYTES                                  
