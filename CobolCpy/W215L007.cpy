000100 01  W215L007.                                                            
000200*                                 LÄNKAREA I W215 MOT                     
000300*                                 HÄNDELSEREGISTRET                       
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 NYUPPLAGG-HAENDELSETRAN                                          
000700                             VALUE +19.                                   
000800     03 FLJANEJ              PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 WDG3KEY.                                                          
001300*                                 WDG3 ROT NYCKEL                         
001400*                                                                         
001500        05 IDHTYP            PIC X(4).                                    
001600*                                 HÄNDELSETYP                             
001700        05 NYCKEL-VALFRI     PIC X(26).                                   
001800     03 IOAREA.                                                           
001900*                                                                         
002000        05 IDARTNR-SATS      PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER FÖR SATS                  
002200        05 FLAGGA-LPKNTL-ING PIC X.                                       
002300*                                 KONTROLL AV ING.ARTIKLARS LEV.P         
002400*                                 LANER                                   
002500*** END COPY W215L007C0  LENGTH=39                                        
