000100 01  W2O12201.                                                            
000200*                                 MOD-AREA FÖR ORDERING. GRAF             
000300     03 TRANS                PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 IDARTNR-IN           PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 IDARTNR-UT           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 KDCLAGER-IN          PIC X.                                       
001000*                                 CENTRALLAGERKOD                         
001100     03 KDCLAGER-UT          PIC X.                                       
001200*                                 CENTRALLAGERKOD                         
001300     03 MESSAGE              PIC X(40).                                   
001400*                                 MEDDELANDEFÄLT PÅ RAD 1                 
001500     03 BENAMNING            PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700     03 KDLTK                PIC X.                                       
001800*                                 LAGERTILLHÖRIGHETSKOD                   
001900     03 DIAGRAM.                                                          
002000        05 DIAGRAMRAD        OCCURS 16 TIMES.                             
002100           07 YAXELVARDE     PIC Z(6)9.                                   
002200           07 YAXEL          PIC X.                                       
002300           07 STAPELBITAR    OCCURS 16 TIMES.                             
002400              09 STAPELBIT   PIC X(3).                                    
002500              09 STAPELBIT-NUM REDEFINES STAPELBIT                        
002600                             PIC 9(3).                                    
002700              09 STAPELSLUT  PIC X.                                       
002800*** END COPY W2O12201C0  LENGTH=1242                                      
