000100 01  W215L002.                                                            
000200*                                 LÄNKAREA I W215 MOT                     
000300*                                 SATSSTRUKTUR OCH INGÅENDE               
000400*                                 ARTIKLAR                                
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 LAES-FORSTA-ART-I-SATS                                           
000800                             VALUE +3.                                    
000900      88 LAES-NASTA-ART-I-SATS                                            
001000                             VALUE +4.                                    
001100      88 BORTTAG-ART-I-SATS  VALUE +5.                                    
001200      88 BORTTAG-KOPPL-ART-I-SATS                                         
001300                             VALUE +6.                                    
001400      88 UPPDATERA-ART-I-SATS                                             
001500                             VALUE +7.                                    
001600      88 LAES-FORSTA-GAELL-ART                                            
001700                             VALUE +21.                                   
001800      88 LAES-NAESTA-GAELL-ART                                            
001900                             VALUE +22.                                   
002000      88 UPPL-HTR-2234       VALUE +23.                                   
002100     03 FLJANEJ-ANROP        PIC X.                                       
002200      88 ANROP-OK            VALUE 'J'.                                   
002300      88 ANROP-FEL           VALUE 'N'.                                   
002400*                                 JA/NEJ-FLAGGA FÖR R2XX                  
002500     03 IDARTNR-SATS         PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER FÖR SATS                  
002700     03 IDARTNR-ING          PIC S9(9)           COMP-3.                  
002800*                                 INGÅENDE ARTIKELNUMMER                  
002900     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
003000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003100     03 IOAREA.                                                           
003200*                                                                         
003300        05 REANTPSA          PIC S9(2)V9(3)      COMP-3.                  
003400*                                 ANTAL PER SATS                          
003500        05 KDISATS           PIC X.                                       
003600*                                 STATUSKOD I SATS                        
003700        05 TISTADAT          PIC S9(7)           COMP-3.                  
003800*                                 GENERELLT STARTDATUM                    
003900        05 TISTODAT          PIC S9(7)           COMP-3.                  
004000*                                 GENERELLT STOPPDATUM                    
004100*** END COPY W215L002C0  LENGTH=29                                        
