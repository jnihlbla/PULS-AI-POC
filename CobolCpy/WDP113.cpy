000100 01  JOB-WDP113.                                                          
000200*                                 JOBB-INFO I SUBMIT-DATABAS              
000300*                                 FYSISK NYCKEL IDJOB                     
000400     03 JOB-IDJOB            PIC X(8).                                    
000500*                                 JOBBNAMN                                
000600     03 JOB-TIREGDAT         PIC S9(7)           COMP-3.                  
000700*                                 REGISTRERINGSDATUM (≈≈MMDD)             
000800     03 JOB-TIUPPDAT         PIC S9(7)           COMP-3.                  
000900*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
001000     03 JOB-TIUPPTID         PIC S9(9)           COMP-3.                  
001100*                                 UPPDATERINGSTID  (TTMMSSTH)             
001200     03 JOB-KDTRSTAT         PIC S9              COMP-3.                  
001300*                                 TRANSAKTIONSSTATUS F÷R                  
001400*                                 IMS DC SUBMIT   (KDTRSTAT-6011)         
001500*                                  0=P≈B÷RJAD                             
001600*                                  1=KLAR F≈R STARTAS FLERA GGR           
001700*                                  2=KLAR F≈R STARTAS EN G≈NG             
001800*                                  3=STARTAD F≈R STARTAS IGEN             
001900*                                  4=STARTAD F≈R EJ STARTAS IGEN          
002000*                                  7=K÷RD OK                              
002100*                                  8=K÷RD ABEND                           
002200*                                  9=SKALL RENSAS                         
002300     03 JOB-BEJOB            PIC X(25).                                   
002400*                                 JOB BESKRIVNING                         
002500     03 JOB-FILLER           PIC X(37).                                   
002600*** END COPY WDP113CCC0  LENGTH=84                                        
