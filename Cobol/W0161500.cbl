000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0161500.                                                
000300 AUTHOR.         ANDRÉ KJELL.                                             
000400 DATE-WRITTEN.   16/12/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        FETCH DATA FROM VCOM.                                            
001000*        THIS PROGRAM IS A REPLACEMENT FOR W0161200 THAT CAN              
001100*        CREATE DIFFERENT OUTPUT FILES DEPENDING ON SENDER-TAG            
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  IF A PROBLEM OCCURS. SEE DISPLAY                        
001500*                                                                         
001600                                                                          
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300                                                                          
002400*          --- INPUT PARAMETERS                                           
002500     SELECT INPARM                     ASSIGN TO W01615D1.                
002600                                                                          
002700*          --- SYMBOLIC PARAMETER TO SOP ORDER                            
002800     SELECT SOPPARM                    ASSIGN TO W01615D2.                
002810                                                                          
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100                                                                          
003200 FILE SECTION.                                                            
003300                                                                          
003400 FD  INPARM                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800 01  INPARM-RECORD   PIC X(80).                                           
003900                                                                          
004000 FD  SOPPARM                                                              
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400 01  SOPPARM-RECORD   PIC X(80).                                          
004500                                                                          
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W0161500'.            
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  INPARM-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-INPARM                       VALUE 'J'.                   
005410                                                                          
005500     EJECT                                                                
006110*    -- WORK AREAS FOR CONSTRUCTING THE OUTPUT DSNAME                     
006200 01  W-PART1                     PIC X(40).                               
006300 01  W-PART2                     PIC X(40).                               
006310                                                                          
006311*    -- "OUTFILE" CORRESPONDS TO AN FD NAME FOR NORMAL FILES              
006312 01  OUTFILE                     PIC S9(9)   COMP.                        
006313                                                                          
006320 01  W-RECORD-COUNT              PIC S9(9)   COMP-3 VALUE ZERO.           
006330                                                                          
006400     EJECT                                                                
006500 01  GENERAL-SUBPROGRAMS.                                                 
006600*                                                                         
006700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006800     03  DSCONR                  PIC X(8)    VALUE 'DSCONR '.             
006900     03  DSRECV                  PIC X(8)    VALUE 'DSRECV '.             
006910     03  DSRLSE                  PIC X(8)    VALUE 'DSRLSE '.             
007000     03  WDYNALC                 PIC X(8)    VALUE 'WDYNALC'.             
007100     03  WFILWRT                 PIC X(8)    VALUE 'WFILWRT'.             
007200                                                                          
007300*    --- PARAMETERS TO ABEND                                              
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007800                                                                          
007900 01  ERROR-TEXT.                                                          
008000     03  FILLER                  PIC X(8)    VALUE 'ERROR:'.              
008100     03  ERROR-TEXT-STR          PIC X(80)   VALUE SPACE.                 
008110                                                                          
008200     EJECT                                                                
008300 01  INPARM-AREA-START           PIC X(24)   VALUE                        
008400                                 'INPARM-AREA-START  '.                   
008600 01  INPARM-AREA                 PIC X(80).                               
008700                                                                          
008701                                                                          
008710 01  SOPPARM-AREA-START          PIC X(24)   VALUE                        
008720                                 'SOPPARM-AREA-START '.                   
008730 01  SOPPARM-AREA                PIC X(80).                               
008740                                                                          
008800     EJECT                                                                
008900*    -- CONTROL PARAMETERS READ VIA INPARM FILE                           
009000 01  INPARM-EXPEDITER            PIC X(8)    VALUE SPACE.                 
009100 01  INPARM-FILE-PATTERN         PIC X(44)   VALUE SPACE.                 
009200 01  INPARM-RECFM                PIC XX      VALUE SPACE.                 
009300 01  INPARM-LRECL                PIC X(4)    VALUE SPACE.                 
009400 01  INPARM-LRECL-NUM            PIC 9(4)    VALUE ZERO BINARY.           
009500 01  LL                          PIC 9(4)    VALUE ZERO BINARY.           
009600                                                                          
009700*    -- SENDER TAG FROM VCOM                                              
009800 01  W-SENDERTAG                 PIC X(20).                               
009900                                                                          
010000*    -- FILE NAME CONSTRUCTED FROM FILE PATTERN AND SENDERTAG             
010100 01  OUTPUT-FILE-NAME            PIC X(44).                               
010200                                                                          
010300     EJECT                                                                
010400 01  VCOM-AREA-START             PIC X(16)   VALUE                        
010500                                 'VCOM-AREA-START '.                      
010600*01  -COPY W0028  -PRE VCOM-                                              
010700                                                                          
010800     EJECT                                                                
010900 01      DYN-PARM.                                                        
011000   03    DYN-INIT            PIC X       VALUE 'I'.                       
011100   03    DYN-OPEN            PIC X       VALUE 'O'.                       
011200   03    DYN-WRITE           PIC X       VALUE ' '.                       
011300   03    DYN-CLOSE           PIC X       VALUE 'C'.                       
011400                                                                          
011500 01   -COPY WDYNAREA                                                      
011600                                                                          
011700     EJECT                                                                
011800 PROCEDURE DIVISION.                                                      
011900 MAIN SECTION.                                                            
012000     SKIP2                                                                
012100                                                                          
012200     PERFORM A-INIT                                                       
012300     PERFORM B-PROCESS-INPARM                                             
012400     PERFORM C-VCOM-START                                                 
012600     PERFORM D-ALLOC-OPEN-OUTPUT-FILE                                     
012700                                                                          
012710     PERFORM E-VCOM-RECEIVE                                               
012800     PERFORM UNTIL VCOM-RC > ZERO                                         
013000       PERFORM F-WRITE-OUTPUT-RECORD                                      
013010       PERFORM E-VCOM-RECEIVE                                             
013100     END-PERFORM                                                          
013200                                                                          
013300     PERFORM G-CLOSE-OUTPUT-FILE                                          
013310     PERFORM H-WRITE-SOPPARM                                              
013400     PERFORM Z-FINIT                                                      
013410     PERFORM ZZ-VCOM-SLUT                                                 
013500                                                                          
013600     MOVE ZERO TO RETURN-CODE                                             
013700     GOBACK                                                               
013800     .                                                                    
013900     EJECT                                                                
014000 A-INIT SECTION.                                                          
014100                                                                          
014200     OPEN INPUT INPARM                                                    
014300* -- OUTPUT FILE IS OPENED VIA WDYNALC                                    
014310     OPEN OUTPUT SOPPARM                                                  
014600     .                                                                    
014700     EJECT                                                                
014800 B-PROCESS-INPARM SECTION.                                                
014900                                                                          
015000     PERFORM S01-READ-INPARM                                              
015100     IF END-OF-INPARM                                                     
015200        MOVE 'EXPEDITER NAME MISSING IN PARAMETER FILE.'                  
015300        TO  ERROR-TEXT-STR                                                
015400        PERFORM S99-ABEND                                                 
015500     ELSE                                                                 
015600       MOVE INPARM-AREA TO INPARM-EXPEDITER                               
015700     END-IF                                                               
015800                                                                          
015900     PERFORM S01-READ-INPARM                                              
016000     IF END-OF-INPARM                                                     
016100        MOVE 'OUTPUT FILE PATTERN MISSING IN PARAMETER FILE.'             
016200        TO  ERROR-TEXT-STR                                                
016300        PERFORM S99-ABEND                                                 
016400     ELSE                                                                 
016500       UNSTRING INPARM-AREA DELIMITED BY ALL SPACE                        
016600       INTO INPARM-FILE-PATTERN                                           
016700            INPARM-RECFM                                                  
016800            INPARM-LRECL COUNT IN LL                                      
016900                                                                          
017000       IF INPARM-RECFM NOT = 'FB' AND NOT = 'VB'                          
017100          MOVE 'OUTPUT FILE RECORD FORMAT MUST BE FB OR VB'               
017200          TO  ERROR-TEXT-STR                                              
017300          PERFORM S99-ABEND                                               
017400       END-IF                                                             
017500                                                                          
017600       IF LL = 0 OR INPARM-LRECL(1:LL) NOT NUMERIC                        
017700          MOVE 'OUTPUT FILE RECORD LENGTH MISSING OR NOT NUMERIC'         
017800          TO  ERROR-TEXT-STR                                              
017900          PERFORM S99-ABEND                                               
018000       ELSE                                                               
018100          MOVE INPARM-LRECL(1:LL) TO INPARM-LRECL-NUM                     
018200       END-IF                                                             
018300     END-IF                                                               
018400     .                                                                    
018500                                                                          
018600                                                                          
018700     EJECT                                                                
018800 C-VCOM-START SECTION.                                                    
018900                                                                          
019000     MOVE +9999            TO VCOM-MAXLENGTH                              
019100     MOVE INPARM-EXPEDITER TO VCOM-EXPEDITER                              
019200                                                                          
019300     CALL DSCONR USING VCOM-RC                                            
019400                       VCOM-DISTID                                        
019500                       VCOM-SECUR                                         
019600                       VCOM-TIMEOUT                                       
019700                       VCOM-SENDERTAG                                     
019800                       VCOM-EXPEDITER                                     
019900                       VCOM-RECTYPE                                       
020000                                                                          
020100     DISPLAY 'DSCONR RC        = ' VCOM-RC                                
020200     DISPLAY 'DSCONR DISTID    = ' VCOM-DISTID                            
020300     DISPLAY 'DSCONR SECUR     = ' VCOM-SECUR                             
020400     DISPLAY 'DSCONR TIMEOUT   = ' VCOM-TIMEOUT                           
020500     DISPLAY 'DSCONR SENDERTAG = ' VCOM-SENDERTAG                         
020600     DISPLAY 'DSCONR EXPEDITER = ' VCOM-EXPEDITER                         
020700     DISPLAY 'DSCONR RECTYPE   = ' VCOM-RECTYPE                           
020800                                                                          
020900     IF VCOM-RC NOT = ZERO                                                
021000       MOVE  'ERROR CODE FROM VCOM DSCONR' TO ERROR-TEXT-STR              
021100       PERFORM S99-ABEND                                                  
021200     END-IF                                                               
021300                                                                          
021400     MOVE VCOM-SENDERTAG    TO W-SENDERTAG                                
021500     .                                                                    
021600                                                                          
021700     EJECT                                                                
021800 D-ALLOC-OPEN-OUTPUT-FILE SECTION.                                        
021900                                                                          
021911*--  INITIERA DCB I WFILWRT SÅ PROGRAMMET VET                             
021912*--  VILKEN FIL DEN SKA ÖPPNA OCH SKRIVA SEDAN                            
021920     CALL WFILWRT                     USING OUTFILE                       
021930                                            DYN-INIT                      
021940                                                                          
021950     IF DYN-INIT = 'F'                                                    
021960       MOVE 'INIT CALL TO WFILWRT FAILED'                                 
021961         TO ERROR-TEXT-STR                                                
021970       PERFORM S99-ABEND                                                  
021991     END-IF                                                               
021992                                                                          
021993                                                                          
022000*--  BERÄKNA FILENS NAMN                                                  
022100     UNSTRING INPARM-FILE-PATTERN DELIMITED BY '*'                        
022200         INTO  W-PART1 W-PART2                                            
022300     MOVE SPACE TO OUTPUT-FILE-NAME                                       
022400     STRING W-PART1 DELIMITED BY SPACE                                    
022500            W-SENDERTAG DELIMITED BY SPACE                                
022600            W-PART2 DELIMITED BY SPACE                                    
022700       INTO OUTPUT-FILE-NAME                                              
022800                                                                          
022900     DISPLAY 'ALLOCATING OUTPUT FILE: ' OUTPUT-FILE-NAME                  
023000                                                                          
023100     MOVE 'C'                         TO  DYN-FREE                        
023200     MOVE OUTPUT-FILE-NAME            TO  DYN-DSNAME                      
023300     MOVE '+1'                        TO  DYN-GENMBR                      
023400     MOVE INPARM-RECFM                TO  DYN-RECFM                       
023500     MOVE 'N'                         TO  DYN-DISP1                       
023600     MOVE 'C'                         TO  DYN-DISP2                       
023700     MOVE 'D'                         TO  DYN-DISP3                       
023800     MOVE 'PSEB'                      TO  DYN-DATACLASS                   
023900     MOVE 'NOBACKUP'                  TO  DYN-MGMCLASS                    
024000     MOVE INPARM-LRECL-NUM            TO  DYN-LRECL                       
024100     MOVE +0                          TO  DYN-BLKSIZE                     
024200     MOVE 'R'                         TO  DYN-RLSE                        
024300                                                                          
024400     CALL WDYNALC                  USING  OUTFILE                         
024500                                          DYN-AREA                        
024700     IF DYN-KDSVAR-OK                                                     
024800       DISPLAY 'OUTPUT FILE ALLOCATED - OK'                               
024900     ELSE                                                                 
025000       MOVE 'OUTPUT FILE COULD NOT BE ALLOCATED'                          
025100         TO ERROR-TEXT-STR                                                
025200       PERFORM S99-ABEND                                                  
025300     END-IF                                                               
025310                                                                          
025311*    -- ÖPPNA FILEN FÖR SKRIVNING                                         
025320     CALL WFILWRT                  USING OUTFILE                          
025330                                         DYN-OPEN                         
025340                                                                          
025341     IF DYN-OPEN = 'F'                                                    
025342       MOVE SPACE TO ERROR-TEXT-STR                                       
025343       STRING 'OUTPUT FILE ' DELIMITED BY SIZE                            
025344              DYN-DSNAME     DELIMITED BY SPACE                           
025345              DYN-GENMBR     DELIMITED BY SIZE                            
025346              ' COULD NOT BE OPENED.' DELIMITED BY SIZE                   
025347         INTO ERROR-TEXT-STR                                              
025349       PERFORM S99-ABEND                                                  
025350     ELSE                                                                 
025351       DISPLAY 'OUTPUT FILE OPENED  OK'                                   
025352     END-IF                                                               
025400     .                                                                    
025500                                                                          
025600     EJECT                                                                
025700 E-VCOM-RECEIVE SECTION.                                                  
025800                                                                          
025900     CALL DSRECV USING VCOM-RC                                            
026000                       VCOM-DISTID                                        
026100                       VCOM-MAXLENGTH                                     
026200                       VCOM-ACTLENGTH                                     
026300                       VCOM-DATA                                          
026400                                                                          
026500     IF VCOM-RC > ZERO AND NOT = 45                                       
026600                                                                          
026700       DISPLAY 'DSRECV RC        = ' VCOM-RC                              
026800       DISPLAY 'DSRECV DISTID    = ' VCOM-DISTID                          
026900       DISPLAY 'DSRECV MAXLENGTH = ' VCOM-MAXLENGTH                       
027000       DISPLAY 'DSRECV ACTLENGTH = ' VCOM-ACTLENGTH                       
027100       DISPLAY 'DSRECV DATA      = ' VCOM-DATA                            
027200                                                                          
027300       MOVE 'ERROR CODE FROM VCOM DSRECV ' TO ERROR-TEXT-STR              
027400       PERFORM S99-ABEND                                                  
027500     END-IF                                                               
027600     .                                                                    
027700                                                                          
027800     EJECT                                                                
027900 F-WRITE-OUTPUT-RECORD SECTION.                                           
028000                                                                          
028122     MOVE VCOM-ACTLENGTH              TO DYN-LRECL                        
028130                                                                          
028140     CALL WFILWRT                  USING OUTFILE                          
028150                                         DYN-WRITE                        
028160                                         DYN-RECFM                        
028170                                         DYN-LRECL                        
028180                                         VCOM-DATA                        
028190                                                                          
028191     IF DYN-WRITE  = 'F'                                                  
028193       MOVE  'ERROR WHEN WRITING DATA TO OUTPUT FILE'                     
028196         TO ERROR-TEXT-STR                                                
028197       PERFORM S99-ABEND                                                  
028198     ELSE                                                                 
028199       ADD 1 TO W-RECORD-COUNT                                            
028200     END-IF                                                               
028210     .                                                                    
028300                                                                          
028400     EJECT                                                                
028500 G-CLOSE-OUTPUT-FILE   SECTION.                                           
028600                                                                          
028710     CALL WFILWRT                     USING OUTFILE                       
028720                                            DYN-CLOSE                     
028730     IF DYN-CLOSE = 'F'                                                   
028740       MOVE  'ERROR WHEN CLOSING OUTPUT FILE'                             
028750         TO ERROR-TEXT-STR                                                
028760       PERFORM S99-ABEND                                                  
028770     ELSE                                                                 
028771       DISPLAY W-RECORD-COUNT ' RECORDS WRITTEN TO OUTPUT FILE'           
028780     END-IF                                                               
028800     .                                                                    
028810                                                                          
028820     EJECT                                                                
028830 H-WRITE-SOPPARM    SECTION.                                              
028840                                                                          
028850     MOVE SPACE TO SOPPARM-AREA                                           
028860     STRING ' VCOM(' DELIMITED BY SIZE                                    
028861        W-SENDERTAG  DELIMITED BY SPACE                                   
028862        ')'          DELIMITED BY SIZE                                    
028880       INTO SOPPARM-AREA                                                  
028895                                                                          
028896     WRITE SOPPARM-RECORD FROM SOPPARM-AREA                               
028897     .                                                                    
028900                                                                          
029000     EJECT                                                                
029100 Z-FINIT SECTION.                                                         
029200                                                                          
029300     CLOSE INPARM SOPPARM                                                 
029400     .                                                                    
029500     EJECT                                                                
029510 ZZ-VCOM-SLUT SECTION.                                                    
029520                                                                          
029530     MOVE ZERO TO VCOM-RC                                                 
029540     CALL DSRLSE USING VCOM-RC                                            
029550                       VCOM-DISTID                                        
029560                       VCOM-RVALUE                                        
029570     DISPLAY 'DSRLSE RC     = ' VCOM-RC                                   
029580     DISPLAY 'DSRLSE DISTID = ' VCOM-DISTID                               
029590     DISPLAY 'DSRLSE RVALUE = ' VCOM-RVALUE                               
029591     .                                                                    
029600 S01-READ-INPARM  SECTION.                                                
029700                                                                          
029800     READ INPARM INTO INPARM-AREA                                         
029900     AT END                                                               
030000        MOVE HIGH-VALUE TO INPARM-AREA                                    
030100        SET END-OF-INPARM TO TRUE                                         
030200     END-READ                                                             
030300     .                                                                    
030310                                                                          
030400     EJECT                                                                
030500 S99-ABEND SECTION.                                                       
030600                                                                          
030700     DISPLAY ERROR-TEXT                                                   
030800     CALL ABEND USING RKOD-ABEND-WITH-DUMP                                
030900     .                                                                    
