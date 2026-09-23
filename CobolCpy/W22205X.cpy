000100 01  OUT2-W22205X.                                                        
000200*                                 FAIR DISTRIBUTION FROM REFILL T         
000300*                                 O AZURE DATALAKE                        
000400     03 OUT2-IDARTNR         PIC X(9)                                     
000500                             VALUE SPACES.                                
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 OUT2-TIAAVV          PIC 9(4)                                     
000900                             VALUE ZEROS.                                 
001000*                                 ÅR - VECKA  (ÅÅVV)                      
001100*                                 YEAR - WEEK  (YYWW)                     
001200     03 OUT2-KVPB-SEP        PIC Z(5)9.9                                  
001300                             VALUE ZEROS.                                 
001400*                                 SEPARAT PERIODBEHOV                     
001500*                                 SEPARATE PERIOD REQUIREMENTS            
001600     03 OUT2-KVPB-SATS       PIC Z(5)9.9                                  
001700                             VALUE ZEROS.                                 
001800*                                 SATS-PERIODBEHOV                        
001900*                                 KIT PERIOD REQUIREMENTS                 
002000     03 OUT2-KVPB-TPO        PIC Z(5)9.9                                  
002100                             VALUE ZEROS.                                 
002200*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
002300*                                 PERIODICAL DEMAND TPO1 AND TPO2         
002400*                                                                         
002500     03 OUT2-KVPB-SDC        PIC Z(5)9.9                                  
002600                             VALUE ZEROS.                                 
002700*                                 PERIODBEHOV FÖR SAMTL SDC:ER            
002800*                                 PERIOD DEMAND FOR ALL SDC:S             
002900     03 OUT2-KVPB-NDC        PIC Z(5)9.9                                  
003000                             VALUE ZEROS.                                 
003100*                                 PERIODBEHOV (PROGNOS)                   
003200*                                 PERIOD REQUIREMENTS                     
003300     03 OUT2-KVPB-TREND      PIC -(6)9.9                                  
003400                             VALUE ZEROS.                                 
003500*                                 PERIODTRENDVÄRDE                        
003600     03 OUT2-KVPB-PLAN       PIC Z(5)9.9                                  
003700                             VALUE ZEROS.                                 
003800*                                 PLANERAT PERIODBEHOV                    
003900*                                 PLANNED PERIOD REQUIREMENTS             
004000*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
