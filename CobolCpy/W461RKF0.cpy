000100 01  RKF-W461RKF0.                                                        
000200*                                 EXCHANGE  TABLE                         
000300*                                 RECORD TYP  RKF                         
000400     03 RKF-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RKF-KDCLAGER         PIC 9.                                       
000700      88 RKF-KDCLAGER-BADA   VALUE 0.                                     
000800      88 RKF-KDCLAGER-C1     VALUE 1.                                     
000900      88 RKF-KDCLAGER-C2     VALUE 2.                                     
001000*                                 CENTRAL WAREHOUSE CODE                  
001100     03 RKF-IDTABNR          PIC 9(3).                                    
001200*                                 TABELNUMBER                             
001300     03 RKF-IDARTNR          PIC 9(9).                                    
001400*                                 PART NUMBER                             
001500     03 RKF-REKSIFFR         PIC 9.                                       
001600*                                 PART NO CHECK DIGIT                     
001700     03 FILLER               PIC X(63).                                   
001800*** END COPY W461RKF0C0  LENGTH=80                                        
