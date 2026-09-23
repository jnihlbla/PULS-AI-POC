000100 01  UTIL-W271UTIL.                                                       
000200*                                 UTILITY PROGRAM FOR REFILL - NO         
000300*                                  DB UPD                                 
000400     03 UTIL-IN-UTDATA.                                                   
000500        05 UTIL-INDATA.                                                   
000600           07 UTIL-KDCALL    PIC S9(3)           COMP-3.                  
000700*                                 ANROPSTYP                               
000800*                                 CALL TYPE                               
000900           07 UTIL-IDARTNR   PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200           07 UTIL-IDDC      PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500           07 UTIL-IDDC-REF  PIC X(2).                                    
001600*                                 SÄNDANDE LAGER FÖR REFILL               
001700*                                 SENDING WAREHOUSE FOR REFILL            
001800           07 UTIL-TIAAVVD   PIC S9(5)           COMP-3.                  
001900*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
002000*                                 YEAR - WEEK - DAY  (YYWWD)              
002100           07 UTIL-LEADTIME  PIC S9(5)           COMP-3.                  
002200           07 UTIL-FLSIM     PIC X.                                       
002300           07 UTIL-KVPB-REF  PIC S9(6)V9(1)      COMP-3.                  
002400*                                 PERIODBEHOV REFILLING                   
002500*                                 FORECAST REFILLING                      
002600           07 UTIL-KVPBREOI  PIC S9(6)V9(1)      COMP-3.                  
002700*                                 PERIODBEHOV FÖR REFILL OI               
002800*                                 PERIOD REQUIREM. REFILLING OI           
002900           07 UTIL-FILLER    PIC X(200).                                  
003000        05 UTIL-UTDATA.                                                   
003100           07 UTIL-KVPB-TOT  PIC S9(6)V9(1)      COMP-3.                  
003200*                                 TOTALT PERIODBEHOV                      
003300           07 UTIL-KVPB-TOT-NOSEAS                                        
003400                             PIC S9(6)V9(1)      COMP-3.                  
003500*                                 TOTALT PERIODBEHOV                      
003600           07 UTIL-PBTOT-V   OCCURS 156 TIMES                             
003700                             PIC S9(6)V9(1)      COMP-3.                  
003800*                                 TOTALT PERIODBEHOV                      
003900           07 UTIL-FLPB-JUST PIC X.                                       
004000*                                 PERIODBEHOVSJUSTERING FLAGGA            
004100           07 UTIL-FLFFC     PIC X.                                       
004200           07 UTIL-FLSEAS    PIC X.                                       
004300           07 UTIL-FILLER    PIC X(198).                                  
004400           07 UTIL-TEXT      PIC X(25).                                   
004500           07 UTIL-KDSVAR    PIC X.                                       
004600            88 UTIL-KDSVAR-OK                                             
004700                             VALUE ' '.                                   
004800            88 UTIL-KDSVAR-FEL                                            
004900                             VALUE 'F'.                                   
005000*                                                       KDSVAR-88         
005100*                                 SVARSKOD FRÅN SUBPROGRAM                
005200*                                                       KDSVAR-88         
005300*                                 RETURN CODE FROM SUBPROGRAM             
005400        05 UTIL-FILLER       PIC X(100).                                  
005500*** END OF VILMAII-COPY LENGTH= 1185 BYTES                                
