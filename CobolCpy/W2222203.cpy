000100 01  W2222203.                                                            
000200*                                 9/26/78 2203    SATSSTRUKTUR            
000300*                                 UPPDATERAD (M.A.P. ING ARTIKEL)         
000400*                                                                         
000500     03 IDHTYP               PIC X(4).                                    
000600*                                 HÄNDELSETYP                             
000700     03 DATA-2203.                                                        
000800        05 IDARTNR-ING       PIC S9(9)           COMP-3.                  
000900*                                 INGÅENDE ARTIKELNUMMER                  
001000        05 IDARTNR-SATS      PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER FÖR SATS                  
001200        05 KVPB-SEP-TOT      PIC S9(6)V9(1)      COMP-3.                  
001300*                                 PB-SEP TOTALT C1+C2                     
001400        05 REANTPSA-NY       PIC S9(2)V9(3)      COMP-3.                  
001500*                                 ANTAL PER SATS                          
001600        05 REANTPSA-GAMMAL   PIC S9(2)V9(3)      COMP-3.                  
001700*                                 ANTAL PER SATS                          
001800        05 KDISATS           PIC X.                                       
001900*                                 STATUSKOD I SATS                        
002000        05 TIBEHDAT          PIC S9(5)           COMP-3.                  
002100*                                 BEHANDLINGSDATUM  (ÅÅVV)                
002200*** END COPY W2222203C0  LENGTH=28                                        
