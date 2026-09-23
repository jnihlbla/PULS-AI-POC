000100 01  MOD-W4O14101.                                                        
000200*                                 MOD-COPYTEXT FÖR W40141                 
000300*                                 PARTS WITH DELIVERY BLOCK               
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDARTNR-NEW-ATTR PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 MOD-IDARTNR-NEW      PIC Z(8)9.                                   
001500*                                 ARTIKELNUMMER                           
001600     03 MOD-TIAAMMDD-LAST    PIC 9(6).                                    
001700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001800     03 MOD-KVANTAL-ACTIVE   PIC Z(4).                                    
001900*                                 ANTAL               KVANTAL-007         
002000     03 MOD-KVANTAL-TOTAL    PIC Z(4).                                    
002100*                                 ANTAL               KVANTAL-007         
002200     03 MOD-TABELLRAD        OCCURS 14 TIMES.                             
002300*                                 GRUPP MED TABELL RADER                  
002400        05 MOD-KDCMD-ATTR    PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600        05 MOD-KDCMD         PIC X.                                       
002700*                                 RAD-UPPDATERINGSKOMMANDO                
002800*                                  BLANK  = INGENTING                     
002900*                                  D , B  = DELETE                        
003000*                                  R , Ä  = REPLACE                       
003100*                                  I,N,A  = INSERT                        
003200*                                  S , V  = SELECT                        
003300*                                  P , P  = PRINT                         
003400*                                  C , K  = COPY                          
003500        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 MOD-IDARTNR       PIC Z(8)9.                                   
003800*                                 ARTIKELNUMMER                           
003900        05 MOD-IDUSER-ASTA   PIC X(8).                                    
004000*                                 ID SOM STARTAR ART TILL MIC             
004100        05 MOD-TISTADAT      PIC 9(6).                                    
004200*                                 GENERELLT STARTDATUM                    
004300        05 MOD-TISTAMIN      PIC X(5).                                    
004400*                                 KLOCKSLAG (TIMMAR/MIN.) START           
004500        05 MOD-IDUSER-ASTO   PIC X(8).                                    
004600*                                 ID SOM STOPPAR ART TILL MIC             
004700        05 MOD-TISTODAT      PIC 9(6).                                    
004800*                                 GENERELLT STOPPDATUM                    
004900        05 MOD-TISTOMIN      PIC X(5).                                    
005000*                                 KLOCKSLAG (TIMMAR/MIN.) STOP            
005100     03 MOD-TEMFSINF         PIC X(55).                                   
005200*                                 INFORMATIONSMEDDELANDE                  
005300*** END OF VILMAII-COPY LENGTH= 870 BYTES                                 
