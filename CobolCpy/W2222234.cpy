000100 01  W2222234.                                                            
000200*                                                                         
000300*                                 2234 - SATSSTRUKTUR UPPDATERAD          
000400*                                 (M.A.P. ING ARTIKEL)                    
000500*                                                                         
000600     03 IDHTYP               PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 DATA-2234.                                                        
000900        05 IDARTNR-ING       PIC S9(9)           COMP-3.                  
001000*                                 INGÅENDE ARTIKELNUMMER                  
001100        05 IDARTNR-SATS      PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER FÖR SATS                  
001300        05 KVPB-SEP-TOT      PIC S9(6)V9(1)      COMP-3.                  
001400*                                 PB-SEP TOTALT C1+C2                     
001500        05 REANTPSA-NY       PIC S9(2)V9(3)      COMP-3.                  
001600*                                 ANTAL PER SATS                          
001700        05 REANTPSA-GAMMAL   PIC S9(2)V9(3)      COMP-3.                  
001800*                                 ANTAL PER SATS                          
001900        05 KDISATS           PIC X.                                       
002000*                                 STATUSKOD I SATS                        
002100        05 TIBEHDAT          PIC S9(5)           COMP-3.                  
002200*                                 BEHANDLINGSDATUM  (ÅÅVV)                
002300*** END COPY W2222234C0  LENGTH=28                                        
