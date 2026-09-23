000100 01  W5I10601.                                                            
000200     03 IDARTNR-IN           PIC X(9).                                    
000300*                                 ARTIKELNUMMER                           
000400     03 IDARTNR-UT           PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 IDDC-IN              PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 IDDC-UT              PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 KOLLA-PF11-DOLD      PIC X.                                       
001100     03 KVLS                 PIC X(8).                                    
001200*                                 LAGERSALDO                              
001300     03 KVUTRS               PIC X(8).                                    
001400*                                 UTREDNINGSSALDO                         
001500     03 KVJUSTKV-IN          PIC X(6).                                    
001600*                                 JUSTERAD KVANTITET                      
001700     03 KDAVVTYP             PIC X.                                       
001800*                                 AVVIKELSETYP                            
001900*                                 1=POSITIV.  2=NEGATIV                   
002000     03 FLANTAL              PIC X.                                       
002100*                                 ANTALJUSTERINGSFLAGGA                   
002200*                                 1 = JA. ANNAT = NEJ.                    
002300*                                 5 = AVV. VID REFILL AV S-LAGER          
002400*                                 5 ANVÄNDS ENDAST I INVENTERING          
002500     03 FLSLACK              PIC X.                                       
002600*                                 FLAGGA SLÄCKNING AV INVENTERING         
002700*                                 1 = JA. ANNAT = NEJ.                    
002800     03 FLFLYTTN             PIC X.                                       
002900*                                 FLAGGA FÖR FLYTTNING                    
003000*                                 1 = JA                                  
003100     03 FILLER               OCCURS 6 TIMES.                              
003200        05 TIJUSTDA          PIC X(5).                                    
003300*                                 JUSTERINGSDATUM                         
003400        05 KVJUSTKV-UT       PIC X(8).                                    
003500*                                                    KVJUSTKV-003         
003600*                                 ANTAL FÖR JUST AV LAGERSALDO            
003700        05 KDJUSTYP          PIC X.                                       
003800*                                 JUSTERINGSTYP                           
003900     03 KVLS-LAGR            PIC X(8).                                    
004000*                                 LAGERSALDO                              
004100     03 KVUTRS-LAGR          PIC X(8).                                    
004200*                                 UTREDNINGSSALDO                         
004300*** END OF VILMAII-COPY LENGTH= 149 BYTES                                 
