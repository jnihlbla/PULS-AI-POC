000100 01  TRAN-W411TRAN.                                                       
000200*                                 LÄNKAREA TILL W411TRAN -                
000300*                                 BESTÄM TRANSPORT                        
000400*                                                                         
000500*                                 ------------------------------          
000600*                                  SVARSKODEN TRAN-KDSVAR                 
000700*                                  KAN HA FÖLJANDE VÄRDEN:                
000800*                                                                         
000900*                                  0 - OK                                 
001000*                                  1 - ANGIVET TRANSPORTID                
001100*                                        SAKNAS                           
001200*                                  2 - FELAKTIGT RFS-DATUM                
001300*                                  3 - FELAKTIG RFS-TID                   
001400*                                  4 - TRANSPORT FRAMTAGEN UTAN           
001500*                                        HÄNSYN TILL SATT RFS             
001600*                                 ------------------------------          
001700*                                                                         
001800     03 TRAN-INDATA.                                                      
001900*                                                                         
002000        05 TRAN-IDSYSTEM     PIC X(4).                                    
002100*                                 VOLVO VCAS SYSTEMNUMMER                 
002200        05 TRAN-IDTRP.                                                    
002300*                                 TRANSPORTIDENTITET                      
002400           07 TRAN-IDTRPLOS  PIC X(3).                                    
002500*                                 TRANSPORTLÖSNING                        
002600           07 TRAN-IDTRPVAR  PIC X(2).                                    
002700*                                 TRANSPORTLÖSNINGSGRUPP                  
002800        05 TRAN-IDDC         PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000        05 TRAN-KDORDKL      PIC S9              COMP-3.                  
003100*                                 ORDERKLASS                              
003200        05 TRAN-KDTRPKAT     PIC X.                                       
003300*                                 TRANSPORTKATEGORI                       
003400        05 TRAN-KDTPOTYP     PIC S9              COMP-3.                  
003500*                                 TYP AV TIDPLANERAD ORDER                
003600        05 TRAN-FLORDSPE     PIC X.                                       
003700*                                 SPECIALORDERFLAGGA                      
003800        05 TRAN-FLOVRLEV     PIC X.                                       
003900*                                 ÖVERLEVERANS                            
004000        05 TRAN-TIREGDAT     PIC S9(7)           COMP-3.                  
004100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004200        05 TRAN-TIHHMM-REG   PIC S9(5)           COMP-3.                  
004300*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004400        05 TRAN-TIRFS        PIC S9(11)          COMP-3.                  
004500*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
004600        05 TRAN-KVLEDTIM-0   PIC S9(3)V9(2)      COMP-3.                  
004700*                                 LEDTID KL 0                             
004800        05 TRAN-KVLEDTIM-1   PIC S9(3)V9(2)      COMP-3.                  
004900*                                 LEDTID KL 1                             
005000        05 TRAN-KVLEDTIM-2   PIC S9(3)V9(2)      COMP-3.                  
005100*                                 LEDTID KL 2                             
005200        05 TRAN-KVLEDTIM-3   PIC S9(3)V9(2)      COMP-3.                  
005300*                                 LEDTID KL 3                             
005400        05 TRAN-KVLEDTIM-4   PIC S9(3)V9(2)      COMP-3.                  
005500*                                 LEDTID KL 4                             
005600     03 TRAN-UTDATA.                                                      
005700*                                                                         
005800        05 TRAN-TITRPAVT.                                                 
005900*                                 TRANSPORTAVGÅNGSTIDPUNKT                
006000           07 TRAN-TIAAMMDD  PIC S9(7)           COMP-3.                  
006100*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
006200           07 TRAN-TIHHMM    PIC S9(5)           COMP-3.                  
006300*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
006400        05 TRAN-KDSVAR       PIC X.                                       
006500*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
006600*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
