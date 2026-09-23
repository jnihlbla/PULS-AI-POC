000100 01  SOP20-W98020.                                                        
000200     03 SOP20-IDDDPREFIX     PIC X(2).                                    
000300*                                 PREFIX PÅ DD-NAMN (0-2 TECKEN)          
000400     03 SOP20-KDSOPFUNK      PIC X.                                       
000500      88 SOP20-OPEN          VALUE '0'.                                   
000600      88 SOP20-ORDER-PROC    VALUE 'O'.                                   
000700      88 SOP20-CANCEL-PROC   VALUE 'C'.                                   
000800      88 SOP20-ACTIVATE-PROC VALUE 'A'.                                   
000900      88 SOP20-PASSIVATE-PROC                                             
001000                             VALUE 'P'.                                   
001100      88 SOP20-START-PROC    VALUE 'S'.                                   
001200      88 SOP20-END-PROC      VALUE 'E'.                                   
001300      88 SOP20-HOLD-PROC     VALUE 'H'.                                   
001400      88 SOP20-RELEASE-PROC  VALUE 'R'.                                   
001500      88 SOP20-PROC-INFO     VALUE 'I'.                                   
001600      88 SOP20-PROC-INFO2    VALUE 'J'.                                   
001700      88 SOP20-CALENDAR-INFO VALUE 'K'.                                   
001800      88 SOP20-CALENDAR-INFO2                                             
001900                             VALUE 'L'.                                   
002000      88 SOP20-ABEND-PROC    VALUE 'Z'.                                   
002100      88 SOP20-SET-SYMB-VAL  VALUE 'V'.                                   
002200      88 SOP20-PLAN-PROC     VALUE 'B'.                                   
002300      88 SOP20-CLOSE         VALUE '9'.                                   
002400*                                 FUNKTIONSTYP TILL SOP PROGRAM           
002500     03 SOP20-IDPROCESS      PIC X(10).                                   
002600*                                 PROCESSNAMN                             
002700     03 SOP20-TIAPDAT        PIC S9(7)           COMP-3.                  
002800*                                 AKTIVERINGS- PASSIVERINGS-DATUM         
002900*                                 (ÅÅMMDD)                                
003000     03 SOP20-KDRET          PIC S9(4)           COMP.                    
003100*                                 RETURKOD                                
003200     03 SOP20-KDMEDD         PIC S9(3)           COMP-3.                  
003300*                                 MEDDELANDEKOD                           
003400     03 SOP20-KDPROCSTRT     PIC X(4).                                    
003500*                                 STARTTYP                                
003600     03 SOP20-KDPROCSTAT     PIC X.                                       
003700      88 SOP20-PASSIVE-STATUS                                             
003800                             VALUE 'P'.                                   
003900      88 SOP20-WAITING-STATUS                                             
004000                             VALUE 'W'.                                   
004100      88 SOP20-STARTED-STATUS                                             
004200                             VALUE 'S'.                                   
004300      88 SOP20-ENDED-STATUS  VALUE 'E'.                                   
004400*                                 PROCESS-STATUS                          
004500     03 SOP20-TECATALOG      PIC X(200).                                  
004600     03 SOP20-TEPRED         PIC X(100).                                  
004700     03 SOP20-TESUCC         PIC X(100).                                  
004800     03 SOP20-TIAPDAT-SENAST PIC S9(7)           COMP-3.                  
004900*                                 AKTIVERINGS- PASSIVERINGS-DATUM         
005000*                                 (ÅÅMMDD)                                
005100     03 SOP20-TIEXDAT-SENAST PIC S9(7)           COMP-3.                  
005200*                                 EXEKVERINGSDATUM     (ÅÅMMDD)           
005300     03 SOP20-TIMINUT-START  PIC S9(5)           COMP-3.                  
005400*                                 STARTTID    (HH.MM)                     
005500     03 SOP20-TIMINUT-STOPP  PIC S9(5)           COMP-3.                  
005600*                                 STOPPTID  (HH.MM)                       
005700     03 SOP20-TIEXEC-SENAST  PIC S9(5)           COMP-3.                  
005800*                                 KÖRNINGSTID (MINUTER)                   
005900     03 SOP20-TIEXEC-MEDEL   PIC S9(5)           COMP-3.                  
006000*                                 KÖRNINGSTID (MINUTER)                   
006100     03 SOP20-TEACTQ         PIC X(100).                                  
006200     03 SOP20-TEPASSQ        PIC X(100).                                  
006300     03 SOP20-FLHOLD         PIC X.                                       
006400*                                 ÄR PROCESSEN I HOLD?                    
006500     03 SOP20-TEATTN         PIC X(15).                                   
006600*                                 OBSERVERA-TEXT                          
006700     03 SOP20-IDPROCESS-PARENT                                            
006800                             PIC X(10).                                   
006900*                                 PROCESSNAMN                             
007000     03 SOP20-KDPROCMTYP     PIC X.                                       
007100      88 SOP20-SYSTEM-PROCESS                                             
007200                             VALUE 'S'.                                   
007300      88 SOP20-ROUTINE-PROCESS                                            
007400                             VALUE 'R'.                                   
007500      88 SOP20-JOB-PROCESS   VALUE 'J'.                                   
007600      88 SOP20-PROCEDURE-PROCESS                                          
007700                             VALUE 'P'.                                   
007800*                                 PROCESSTYP                              
007900     03 SOP20-KDPROCPRIO     PIC X.                                       
008000*                                 PRIORITETSKOD                           
008100     03 SOP20-KDVOUT         PIC X.                                       
008200      88 SOP20-PAPER-OUTPUT  VALUE 'P'.                                   
008300      88 SOP20-COM-OUTPUT    VALUE 'C'.                                   
008400      88 SOP20-PAPER-COM-OUTPUT                                           
008500                             VALUE 'B'.                                   
008600      88 SOP20-NO-OUTPUT     VALUE ' '.                                   
008700*                                 TYP AV OUTPUT PÅ VOLVODATA              
008800     03 SOP20-TITMPDUR       PIC S9(7)           COMP-3.                  
008900*                                 VARAKTIGHET PÅ TEMPÄNDRING              
009000*                                 (ÅÅMMDD ELLER ANTAL GGR 1-99)           
009100     03 SOP20-TESYMBV        PIC X(1000).                                 
009200     03 SOP20-TERES          PIC X(100).                                  
009300     03 SOP20-ID-ABEND-JOB   PIC X(10).                                   
009400*                                 PROCESSNAMN                             
009500     03 SOP20-FLBATCH        PIC X.                                       
009600*                                 EXEKVERING I BATCH?                     
009700     03 SOP20-KDANR          PIC S9(4)           COMP.                    
009800*                                 AKTIVERINGSNUMMER                       
