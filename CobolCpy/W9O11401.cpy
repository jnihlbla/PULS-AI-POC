000100 01  W9O11401.                                                            
000200*                                 COPYTEXT FÖR MOD W9O11401               
000300*                                 TRANS 9114 (W9T114)                     
000400*                                 DELIVERY INFORMATION CDC                
000500     03 IDTRANS              PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 TEMFSFEL             PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900     03 IDARTNR-IN           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 IDARTNR-UT           PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 BEART-SVE            PIC X(25).                                   
001400*                                 SVENSK ARTIKELBENÄMNING                 
001500     03 BEART-ENG            PIC X(25).                                   
001600*                                 ENGELSK ARTIKELBENÄMNING                
001700     03 KVROS-DAG            PIC -(6)9.                                   
001800*                                 RESTORDERSALDO, KLASS 1                 
001900     03 KVROS-BULK           PIC -(6)9.                                   
002000*                                 RESTORDERSALDO, KLASS 2-4               
002100     03 KVOKS-VOR            PIC -(6)9.                                   
002200*                                 ORDERKÖSALDO, VOR                       
002300     03 KVAKS                PIC Z(6)9.                                   
002400*                                 ANKOMSTSALDO                            
002500     03 LEVBSK-GRUPP         OCCURS 4 TIMES.                              
002600*                                                                         
002700        05 TILEVBSK-DISP     PIC X(6).                                    
002800*                                 LEV. BESK. DISPONIBEL(ÅÅMMDD)           
002900        05 KVAVIS-BSKKVAR    PIC X(7).                                    
003000*                                 LEV. BESK. ANT. EFTER AVBOKNING         
003100     03 AVROP-GRUPP          OCCURS 5 TIMES.                              
003200*                                                                         
003300        05 TIDATUM-INL       PIC X(6).                                    
003400*                                 DATUM ENLIGT KDDATFORM                  
003500        05 KVAVROP           PIC Z(6)9.                                   
003600*                                 AVROPSKVANTITET                         
003700     03 TELEVBSK-EXT         PIC X(80).                                   
003800*                                 LEVERANSBESKED FÖR EXTERNT              
003900     03 TELEVBSK-EXT2        PIC X(80).                                   
004000*                                 LEVERANSBESKED FÖR EXTERNT              
004100     03 TELEVBSK-EXT3        PIC X(80).                                   
004200*                                 LEVERANSBESKED FÖR EXTERNT              
004300     03 TELEVBSK-EXT4        PIC X(80).                                   
004400*                                 LEVERANSBESKED FÖR EXTERNT              
004500     03 TEMFSINF             PIC X(55).                                   
004600*                                 INFORMATIONSMEDDELANDE                  
004700*** END OF VILMAII-COPY LENGTH= 632 BYTES                                 
