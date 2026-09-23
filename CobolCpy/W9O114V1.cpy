000100 01  W9O114V1.                                                            
000200*                                 COPYTEXT F÷R MOD W9O114V1               
000300*                                 TRANS 9114 (W90114T)                    
000400*                                 DELIVERY INFORMATION CDC                
000500     03 IDTRANS              PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 IDMFSFEL             PIC X(3).                                    
000800*                                 MFS FELMEDDELANDE NUMMER                
000900     03 IDARTNR              PIC 9(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 KVROS-DAG            PIC 9(7).                                    
001200*                                 RESTORDERSALDO, KLASS 1                 
001300     03 KVROS-BULK           PIC 9(7).                                    
001400*                                 RESTORDERSALDO, KLASS 2-4               
001500     03 KVOKS-VOR            PIC 9(7).                                    
001600*                                 ORDERK÷SALDO, VOR                       
001700     03 KVAKS                PIC 9(7).                                    
001800*                                 ANKOMSTSALDO                            
001900     03 LEVBSK-GRUPP         OCCURS 4 TIMES.                              
002000*                                                                         
002100        05 TILEVBSK-DISP     PIC 9(6).                                    
002200*                                 LEV. BESK. DISPONIBEL(≈≈MMDD)           
002300        05 KVAVIS-BSKKVAR    PIC 9(7).                                    
002400*                                 LEV. BESK. ANT. EFTER AVBOKNING         
002500     03 AVROP-GRUPP          OCCURS 5 TIMES.                              
002600*                                                                         
002700        05 TIDATUM-INL       PIC 9(6).                                    
002800*                                 DATUM ENLIGT KDDATFORM                  
002900        05 KVAVROP           PIC 9(7).                                    
003000*                                 AVROPSKVANTITET                         
003100     03 TELEVBSK-EXT         PIC X(80).                                   
003200*                                 LEVERANSBESKED F÷R EXTERNT              
003300     03 TELEVBSK-EXT2        PIC X(80).                                   
003400*                                 LEVERANSBESKED F÷R EXTERNT              
003500     03 TELEVBSK-EXT3        PIC X(80).                                   
003600*                                 LEVERANSBESKED F÷R EXTERNT              
003700     03 TELEVBSK-EXT4        PIC X(80).                                   
003800*                                 LEVERANSBESKED F÷R EXTERNT              
003900*** END OF VILMAII-COPY LENGTH= 481 BYTES                                 
