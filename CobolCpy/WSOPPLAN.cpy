000100 01  SOP-RECORD.                                                          
000200*                                 DATA FR≈N PLAN-KOMMANDOT                
000300*                                 DATA FROM COMMAND PLAN                  
000400     03 SOP-PROC-NAME        PIC X(10).                                   
000500*                                 PROCESSNAMN                             
000600*                                 PROCESS NAME                            
000700     03 SOP-ACTPASS-DATE     PIC S9(7)           COMP-3.                  
000800*                                 AKTIVERINGS- PASSIVERINGS-DATUM         
000900*                                 (≈≈MMDD)                                
001000*                                 ACTIVATION / PASSIVATION DATE           
001100*                                 (YYMMDD)                                
001200     03 SOP-START-TYPE       PIC X(4).                                    
001300*                                 STARTTYP                                
001400*                                 START-TYPE                              
001500     03 SOP-CATALOG          PIC X(100).                                  
001600     03 SOP-PREDECESSORS     PIC X(100).                                  
001700     03 SOP-SUCCESSORS       PIC X(100).                                  
001800     03 SOP-LAST-ACTPASS-DATE                                             
001900                             PIC S9(7)           COMP-3.                  
002000*                                 AKTIVERINGS- PASSIVERINGS-DATUM         
002100*                                 (≈≈MMDD)                                
002200*                                 ACTIVATION / PASSIVATION DATE           
002300*                                 (YYMMDD)                                
002400     03 SOP-LAST-EXEC-DATE   PIC S9(7)           COMP-3.                  
002500*                                 EXEKVERINGSDATUM     (≈≈MMDD)           
002600*                                 EXECUTION DATE       (YYMMDD)           
002700     03 SOP-LAST-START-TIME  PIC S9(5)           COMP-3.                  
002800*                                 STARTTID    (HH.MM)                     
002900*                                 START TIME  (HH.MM)                     
003000     03 SOP-LAST-END-TIME    PIC S9(5)           COMP-3.                  
003100*                                 STOPPTID  (HH.MM)                       
003200*                                 END TIME  (HH.MM)                       
003300     03 SOP-LAST-EXEC-TIME   PIC S9(5)           COMP-3.                  
003400*                                 K÷RNINGSTID (MINUTER)                   
003500*                                 EXECUTION TIME (MINUTES)                
003600     03 SOP-MEAN-EXEC-TIME   PIC S9(5)           COMP-3.                  
003700*                                 K÷RNINGSTID (MINUTER)                   
003800*                                 EXECUTION TIME (MINUTES)                
003900     03 SOP-PARENT-PROC-NAME PIC X(10).                                   
004000*                                 PROCESSNAMN                             
004100*                                 PROCESS NAME                            
004200     03 SOP-PROC-TYPE        PIC X.                                       
004300      88 SOP-SYSTEM-PROCESS  VALUE 'S'.                                   
004400      88 SOP-ROUTINE-PROCESS VALUE 'R'.                                   
004500      88 SOP-JOB-PROCESS     VALUE 'J'.                                   
004600      88 SOP-PROCEDURE-PROCESS                                            
004700                             VALUE 'P'.                                   
004800*                                 PROCESSTYP                              
004900*                                 PROCESS TYPE                            
005000     03 SOP-PROC-PRIO        PIC X.                                       
005100*                                 PRIORITETSKOD                           
005200*                                 PRIORITY CODE                           
005300     03 SOP-VD-OUTPUT        PIC X.                                       
005400      88 SOP-PAPER-OUTPUT    VALUE 'P'.                                   
005500      88 SOP-COM-OUTPUT      VALUE 'C'.                                   
005600      88 SOP-PAPER-COM-OUTPUT                                             
005700                             VALUE 'B'.                                   
005800      88 SOP-NO-OUTPUT       VALUE ' '.                                   
005900*                                 TYP AV OUTPUT P≈ VOLVODATA              
006000*                                 TYPE OF OUTPUT AT VOLVODATA             
006100     03 SOP-OUT-TIME-LIMIT   PIC S9(5)           COMP-3.                  
006200*                                 FƒRDIGTID F÷R OUTPUTHANTERING           
006300*                                 OUTPUT TIME LIMIT                       
006400*** END COPY WSOPPLANC0  LENGTH=354                                       
