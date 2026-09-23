000100 01  W9O11501.                                                            
000200*                                 COPYTEXT FÖR MOD W9O11501               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDARTNR-UT           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 IDDC-IN              PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 IDDC-UT              PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 BEART-SVE            PIC X(25).                                   
001600*                                 SVENSK ARTIKELBENÄMNING                 
001700     03 BEART-ENG            PIC X(25).                                   
001800*                                 ENGELSK ARTIKELBENÄMNING                
001900     03 GRP-RAD              OCCURS 8 TIMES.                              
002000*                                                                         
002100        05 IDDC              PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300        05 KVAVIS            PIC Z(7).                                    
002400*                                 AVISERAT ANTAL                          
002500        05 TIBERANK          PIC X(6).                                    
002600*                                 BERÄKNAD ANKOMSTDATUM                   
002700        05 KVAKS             PIC X(7).                                    
002800*                                 ANKOMSTSALDO                            
002900        05 KVROS-DAG         PIC -(6)9.                                   
003000*                                 RESTORDERSALDO, KLASS 1                 
003100        05 KVROS-BULK        PIC -(6)9.                                   
003200*                                 RESTORDERSALDO, KLASS 2-4               
003300        05 KVOKS-DAG         PIC -(6)9.                                   
003400*                                 ORDERKÖSALDO, KLASS 1                   
003500        05 KVOKS-BULK        PIC -(6)9.                                   
003600*                                 ORDERKÖSALDO, KLASS 2-4                 
003700     03 TELEVBSK-EXT         PIC X(80).                                   
003800*                                 LEVERANSBESKED FÖR EXTERNT              
003900     03 TELEVBSK-EXT2        PIC X(80).                                   
004000*                                 LEVERANSBESKED FÖR EXTERNT              
004100     03 TEMFSINF             PIC X(55).                                   
004200*                                 INFORMATIONSMEDDELANDE                  
004300*** END OF VILMAII-COPY LENGTH= 731 BYTES                                 
