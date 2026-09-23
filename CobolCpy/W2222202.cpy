000100 01  W2222202.                                                            
000200*                                 9/26/78 2202    PB-SEP                  
000300*                                 UPPDATERAD PÅ SATSARTIKEL.              
000400*                                                                         
000500     03 IDHTYP               PIC X(4).                                    
000600*                                 HÄNDELSETYP                             
000700     03 DATA-2202.                                                        
000800        05 IDARTNR-SATS      PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER FÖR SATS                  
001000        05 KDCLAGER          PIC S9              COMP-3.                  
001100*                                 CENTRALLAGERKOD                         
001200        05 KVPB-SEP-NY       PIC S9(6)V9(1)      COMP-3.                  
001300*                                 SEPARAT PERIODBEHOV                     
001400        05 KVPB-SEP-GAMMAL   PIC S9(6)V9(1)      COMP-3.                  
001500*                                 SEPARAT PERIODBEHOV                     
001600*** END COPY W2222202C0  LENGTH=18                                        
