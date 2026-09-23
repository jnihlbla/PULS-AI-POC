000100 01  SOP-PARM-AREA.                                                       
000200*                                 ANVÄNDS VID ANROP AV SOPSUB             
000300*                                 USED AT CALLS TO SOPSUB                 
000400*                                 ------------------------------          
000500*                                 THE FIRST 2 FIELDS MUST ALWAYS          
000600*                                 BE SPECIFIED   (SPACE = NO              
000700*                                 PREFIX)  AND "PROC-NAME" MUST           
000800*                                 ALWAYS BE SPECIFIED EXCEPT FOR          
000900*                                 FUNCTION "K".                           
001000*                                                                         
001100*                                 "ACTPASS-DATE" IS USED BY               
001200*                                 FUNCTION "O", "C" AND "K"               
001300*                                 DATE=000000 MEANS CURRENT DATE.         
001400*                                 DATE=000001 MEANS TOMORROW.             
001500*                                                                         
001600*                                 THE FIELDS BETWEEN "START-TYPE"         
001700*                                 AND "OUT-TIME-LIMIT" ARE                
001800*                                 RETURNED BY FUNCTION "I".               
001900*                                 "CATALOG" WILL CONTAIN A STRING         
002000*                                 OF CATALOG WORDS SEPARATED BY           
002100*                                 A SPACE. IF STATUS IS "WAITING"         
002200*                                 "PREDECESSORS"  WILL CONTAIN A          
002300*                                 SIMILAR STRING OF PROCESSES             
002400*                                 WHICH ARE PREVENTING IT FROM            
002500*                                 STARTING. OTHERWISE IT WILL             
002600*                                 CONTAIN THE DIRECT PREDECES-            
002700*                                 SORS PREFIXED BY A "<".                 
002800*                                 IF THE PROCESS IS "WAITING"             
002900*                                 OR "STARTED", "SUCCESSORS"              
003000*                                 WILL CONTAIN A SIMILAR LIST OF          
003100*                                 PROCESSES WHICH ARE WAITING FOR         
003200*                                 THIS PROCESS TO END.                    
003300*                                 OTHERWISE IT WILL CONTAIN THE           
003400*                                 DIRECT SUCCESSORS PREFIXED BY           
003500*                                 A ">".                                  
003600*                                 "ACT-QUEUE" WILL CONTAIN A              
003700*                                 STRING OF DATES TO WHICH THE            
003800*                                 PROCESS HAS BEEN ORDERED.               
003900*                                 "PASS-QUEUE" WILL CONTAIN THE           
004000*                                 DATES TO WHICH IT HAS BEEN              
004100*                                 CANCELLED.                              
004200*                                                                         
004300*                                 AT RETURN FROM SOP, "RETCODE"=          
004400*                                 0 - 16 DEPENDING ON THE RESULT          
004500*                                 OF THE CALL.  IF RETCODE > 0            
004600*                                 "MSGCODE" CONTAINS AN ERROR             
004700*                                 NUMBER.                                 
004800     03 SOP-DDPREFIX         PIC X(2).                                    
004900*                                 PREFIX PÅ DD-NAMN (0-2 TECKEN)          
005000*                                 PREFIX ON DD NAMES (0-2 CHARS)          
005100     03 SOP-SOPFUNC          PIC X.                                       
005200      88 SOP-ORDER-PROC      VALUE 'O'.                                   
005300      88 SOP-CANCEL-PROC     VALUE 'C'.                                   
005400      88 SOP-ACTIVATE-PROC   VALUE 'A'.                                   
005500      88 SOP-PASSIVATE-PROC  VALUE 'P'.                                   
005600      88 SOP-START-PROC      VALUE 'S'.                                   
005700      88 SOP-END-PROC        VALUE 'E'.                                   
005800      88 SOP-HOLD-PROC       VALUE 'H'.                                   
005900      88 SOP-RELEASE-PROC    VALUE 'R'.                                   
006000      88 SOP-PROC-INFO       VALUE 'I'.                                   
006100      88 SOP-CALENDAR-INFO   VALUE 'K'.                                   
006200      88 SOP-ABEND-PROC      VALUE 'Z'.                                   
006300      88 SOP-SET-SYMB-VAL    VALUE 'V'.                                   
006400*                                 FUNKTIONSTYP TILL SOP PROGRAM           
006500*                                 ACTION TYPE FOR SOP PROGRAM             
006600     03 SOP-PROC-NAME        PIC X(10).                                   
006700*                                 PROCESSNAMN                             
006800*                                 PROCESS NAME                            
006900     03 SOP-ACTPASS-DATE     PIC S9(7)           COMP-3.                  
007000*                                 AKTIVERINGS- PASSIVERINGS-DATUM         
007100*                                 (ÅÅMMDD)                                
007200*                                 ACTIVATION / PASSIVATION DATE           
007300*                                 (YYMMDD)                                
007400     03 SOP-RETCODE          PIC S9(4)           COMP.                    
007500*                                 RETURKOD                                
007600*                                 RETURN CODE                             
007700     03 SOP-MSGCODE          PIC S9(3)           COMP-3.                  
007800*                                 MEDDELANDEKOD                           
007900*                                 MESSAGE CODE                            
008000     03 SOP-START-TYPE       PIC X(4).                                    
008100*                                 STARTTYP                                
008200*                                 START-TYPE                              
008300     03 SOP-PROC-STATUS      PIC X.                                       
008400      88 SOP-PASSIVE-STATUS  VALUE 'P'.                                   
008500      88 SOP-WAITING-STATUS  VALUE 'W'.                                   
008600      88 SOP-STARTED-STATUS  VALUE 'S'.                                   
008700      88 SOP-ENDED-STATUS    VALUE 'E'.                                   
008800*                                 PROCESS-STATUS                          
008900*                                 PROCESS STATUS                          
009000     03 SOP-CATALOG          PIC X(100).                                  
009100     03 SOP-PREDECESSORS     PIC X(100).                                  
009200     03 SOP-SUCCESSORS       PIC X(100).                                  
009300     03 SOP-LAST-ACTPASS-DATE                                             
009400                             PIC S9(7)           COMP-3.                  
009500*                                 AKTIVERINGS- PASSIVERINGS-DATUM         
009600*                                 (ÅÅMMDD)                                
009700*                                 ACTIVATION / PASSIVATION DATE           
009800*                                 (YYMMDD)                                
009900     03 SOP-LAST-EXEC-DATE   PIC S9(7)           COMP-3.                  
010000*                                 EXEKVERINGSDATUM     (ÅÅMMDD)           
010100*                                 EXECUTION DATE       (YYMMDD)           
010200     03 SOP-LAST-START-TIME  PIC S9(5)           COMP-3.                  
010300*                                 STARTTID    (HH.MM)                     
010400*                                 START TIME  (HH.MM)                     
010500     03 SOP-LAST-END-TIME    PIC S9(5)           COMP-3.                  
010600*                                 STOPPTID  (HH.MM)                       
010700*                                 END TIME  (HH.MM)                       
010800     03 SOP-LAST-EXEC-TIME   PIC S9(5)           COMP-3.                  
010900*                                 KÖRNINGSTID (MINUTER)                   
011000*                                 EXECUTION TIME (MINUTES)                
011100     03 SOP-MEAN-EXEC-TIME   PIC S9(5)           COMP-3.                  
011200*                                 KÖRNINGSTID (MINUTER)                   
011300*                                 EXECUTION TIME (MINUTES)                
011400     03 SOP-ACT-QUEUE        PIC X(100).                                  
011500     03 SOP-PASS-QUEUE       PIC X(100).                                  
011600     03 SOP-PROC-HOLD        PIC X.                                       
011700*                                 ÄR PROCESSEN I HOLD?                    
011800*                                 IS THE PROCESS HELD?                    
011900     03 SOP-ATTN-TEXT        PIC X(15).                                   
012000*                                 OBSERVERA-TEXT                          
012100*                                 ATTENTION TEXT                          
012200     03 SOP-PARENT-PROC-NAME PIC X(10).                                   
012300*                                 PROCESSNAMN                             
012400*                                 PROCESS NAME                            
012500     03 SOP-PROC-TYPE        PIC X.                                       
012600      88 SOP-SYSTEM-PROCESS  VALUE 'S'.                                   
012700      88 SOP-ROUTINE-PROCESS VALUE 'R'.                                   
012800      88 SOP-JOB-PROCESS     VALUE 'J'.                                   
012900      88 SOP-PROCEDURE-PROCESS                                            
013000                             VALUE 'P'.                                   
013100*                                 PROCESSTYP                              
013200*                                 PROCESS TYPE                            
013300     03 SOP-PROC-PRIO        PIC X.                                       
013400*                                 PRIORITETSKOD                           
013500*                                 PRIORITY CODE                           
013600     03 SOP-VD-OUTPUT        PIC X.                                       
013700      88 SOP-PAPER-OUTPUT    VALUE 'P'.                                   
013800      88 SOP-COM-OUTPUT      VALUE 'C'.                                   
013900      88 SOP-PAPER-COM-OUTPUT                                             
014000                             VALUE 'B'.                                   
014100      88 SOP-NO-OUTPUT       VALUE ' '.                                   
014200*                                 TYP AV OUTPUT PÅ VOLVODATA              
014300*                                 TYPE OF OUTPUT AT VOLVODATA             
014400     03 SOP-TEMP-JCL-DURATION                                             
014500                             PIC S9(7)           COMP-3.                  
014600*                                 VARAKTIGHET PÅ TEMPÄNDRING              
014700*                                 (ÅÅMMDD ELLER ANTAL GGR 1-99)           
014800*                                 DURATION OF TEMPORARY CHANGE            
014900*                                 (YYMMDD OR NBR OF TIMES 1-99)           
015000     03 SOP-SYMBOLIC-VARIABLES                                            
015100                             PIC X(500).                                  
015200*** END COPY WSOPAREAC0  LENGTH=1079                                      
