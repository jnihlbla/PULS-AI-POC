000100 01  W5O105001.                                                           
000200*                                 COPYTEXT FÖR MID W5O10501               
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
001500     03 TIJUSTDA1-ENTER      PIC 9(6).                                    
001600*                                 JUSTERINGSDATUM                         
001700     03 TIJUSTDA2-ENTER      PIC 9(6).                                    
001800*                                 JUSTERINGSDATUM                         
001900     03 TIJUSTDA1-NEXT       PIC 9(6).                                    
002000*                                 JUSTERINGSDATUM                         
002100     03 TIJUSTDA2-NEXT       PIC 9(6).                                    
002200*                                 JUSTERINGSDATUM                         
002300     03 LINES                OCCURS 12 TIMES.                             
002400        05 COL               OCCURS 2 TIMES                               
002500                             INDEXED IX-COL.                              
002600           07 TIJUSTDA       PIC 9(5).                                    
002700*                                 JUSTERINGSDATUM                         
002800           07 KDJUSTYP       PIC X(2).                                    
002900           07 KVJUSTKV       PIC -(6)9.                                   
003000*                                 JUSTERAD KVANTITET                      
003100        05 COL1              OCCURS 2 TIMES                               
003200                             INDEXED IX-COL1.                             
003300           07 TIJUSTDA-SDC   PIC 9(5).                                    
003400*                                 JUSTERINGSDATUM                         
003500           07 KDJUSTYP-SDC   PIC X(2).                                    
003600           07 KVJUSTKV-SDC   PIC -(6)9.                                   
003700*                                 JUSTERAD KVANTITET                      
003800           07 IDDC           PIC X(2).                                    
003900*                                 IDENTIFIERARE LAGER                     
004000     03 SUINVJUST-C1         PIC -(9)9.                                   
004100*                                 JUSTERAD SUMMERAD KVANTITET             
004200     03 SUINVJUST-C2         PIC -(9)9.                                   
004300*                                 JUSTERAD SUMMERAD KVANTITET             
004400     03 TEMFSINF             PIC X(55).                                   
004500*                                 INFORMATIONSMEDDELANDE                  
004600*** END OF VILMAII-COPY LENGTH= 885 BYTES                                 
