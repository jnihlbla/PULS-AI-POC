000100 01  UTUP-W271UTUP.                                                       
000200*                                 UPDATE UTILITY PROGRAM FOR REFI         
000300*                                 LL                                      
000400     03 UTUP-IN-UTDATA.                                                   
000500        05 UTUP-INDATA.                                                   
000600           07 UTUP-KDCALL    PIC S9(3)           COMP-3.                  
000700*                                 ANROPSTYP                               
000800*                                 CALL TYPE                               
000900           07 UTUP-IDARTNR   PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200           07 UTUP-IDDC      PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500           07 UTUP-IDDC-REF  PIC X(2).                                    
001600*                                 SÄNDANDE LAGER FÖR REFILL               
001700*                                 SENDING WAREHOUSE FOR REFILL            
001800           07 UTUP-TIAAMMDD  PIC 9(6).                                    
001900*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002000*                                 YEAR - MONTH - DAY  (YYMMDD)            
002100           07 UTUP-LEADTIME  PIC S9(5)           COMP-3.                  
002200           07 UTUP-FLSIM     PIC X.                                       
002300           07 UTUP-KVPB-REF  PIC S9(6)V9(1)      COMP-3.                  
002400*                                 PERIODBEHOV REFILLING                   
002500*                                 FORECAST REFILLING                      
002600           07 UTUP-KVPBREOI  PIC S9(6)V9(1)      COMP-3.                  
002700*                                 PERIODBEHOV FÖR REFILL OI               
002800*                                 PERIOD REQUIREM. REFILLING OI           
002900           07 UTUP-FILLER    PIC X(100).                                  
003000        05 UTUP-UTDATA.                                                   
003100           07 UTUP-KDSVAR    PIC X.                                       
003200            88 UTUP-KDSVAR-OK                                             
003300                             VALUE ' '.                                   
003400            88 UTUP-KDSVAR-FEL                                            
003500                             VALUE 'F'.                                   
003600*                                                       KDSVAR-88         
003700*                                 SVARSKOD FRÅN SUBPROGRAM                
003800*                                                       KDSVAR-88         
003900*                                 RETURN CODE FROM SUBPROGRAM             
004000           07 UTUP-TEXT      PIC X(25).                                   
004100           07 UTUP-KVBEHOV-VECKA                                          
004200                             PIC S9(7)V9(2)      COMP-3.                  
004300*                                 BEHOV PER VECKA                         
004400           07 UTUP-LEADTID-BEHOV                                          
004500                             PIC S9(7)V9(2).                              
004600           07 UTUP-WEEKLY-DEMANDS                                         
004700                             OCCURS 156 TIMES.                            
004800              09 UTUP-KVBEHOV-V                                           
004900                             PIC S9(7)V9(2).                              
005000              09 UTUP-LT-BEHOV-V                                          
005100                             PIC S9(7)V9(2).                              
005200           07 UTUP-FLPB-JUST PIC X.                                       
005300*                                 PERIODBEHOVSJUSTERING FLAGGA            
005400           07 UTUP-FLFFC     PIC X.                                       
005500           07 UTUP-FLSEAS    PIC X.                                       
005600           07 UTUP-FILLER    PIC X(100).                                  
005700*** END OF VILMAII-COPY LENGTH= 3080 BYTES                                
