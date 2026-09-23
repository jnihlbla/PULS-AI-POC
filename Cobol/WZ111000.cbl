000100*COMPOPT DB2BIND=YES                                                      
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ111000.                                                
000400 AUTHOR.         ANDRE KJELL.                                             
000500 DATE-WRITTEN.   08-12-22.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        GENERAL PROGRAM TO SEND FILE-DATA ASYNCHRONOUSLY                 
001000*        VIA THE WZ01SEND DISPATCHER SUB PROGRAM.                         
001100*                                                                         
001200*        A CONTROL FILE CONTAINING RECEIVING ADDRESS                      
001300*        OPTIONAL "EXTRA" ADDRESS DATA (VCOM ENDER TAG)                   
001400*        AND OPTIONAL RETURN ADDRESS, IS ALSO READ.                       
001500*                                                                         
001600*        THE ACTUAL DATA IS READ FROM A SEPARAE FILE.                     
001700*                                                                         
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- DATA TO BE TRANSMITTED                                     
002800     SELECT INDATA                     ASSIGN TO WZ1110D1.                
002900     SKIP2                                                                
003000*          --- CONTROL PARAMETERS                                         
003100     SELECT CTLDATA                    ASSIGN TO WZ1110D2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  INDATA                                                               
003800     RECORDING       V                                                    
003900     RECORD IS VARYING FROM 1 TO 3000 CHARACTERS                          
004000            DEPENDING ON IN-KVDLEN                                        
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300 01  FILLER                     PIC X(3000).                              
004400     SKIP3                                                                
004500 FD  CTLDATA                                                              
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900 01  FILLER                     PIC X(80).                                
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300 77  IDPGM                       PIC X(8)    VALUE 'WZ111000'.            
005400 77  YES                         PIC X       VALUE 'J'.                   
005500 77  NOO                         PIC X       VALUE 'N'.                   
005600     SKIP2                                                                
005700 01  ERRTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
006000 77  KDRC-DISPLAY                PIC Z(3)9.                               
006100                                                                          
006200 77  INDATA-EOF-SW               PIC X       VALUE 'N'.                   
006300     88  END-OF-INDATA                       VALUE 'Y'.                   
006400                                                                          
006500 77  CTLDATA-EOF-SW              PIC X       VALUE 'N'.                   
006600     88  END-OF-CTLDATA                      VALUE 'Y'.                   
006700                                                                          
006800 77  VBAR                        PIC X       VALUE X'BB'.                 
006900 77  BROKEN-VBAR                 PIC X       VALUE X'CC'.                 
007000 77  UNDERSCORE                  PIC X       VALUE '_'.                   
007100                                                                          
007200     EJECT                                                                
007300 01  GENERAL-SUBPROGRAMS.                                                 
007400*                                                                         
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007700     SKIP3                                                                
007800*    --- PARAMETERS TO ABEND                                              
007900                                                                          
008000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008300     EJECT                                                                
008400 01  IN-AREA-START               PIC X(16)   VALUE  'IN-AREA'.            
008500                                                                          
008600 01  IN-KVDLEN                   PIC 9(9)   BINARY.                       
008700                                                                          
008800 01  IN-AREA                     PIC X(3000).                             
008900     EJECT                                                                
009000 01  CTL-AREA-START              PIC X(16)   VALUE 'CTL-AREA'.            
009100                                                                          
009200     SKIP2                                                                
009300 01  CTL-AREA.                                                            
009400     03  CTL-DATA                PIC X(72).                               
009500     03  FILLER                  PIC X(8).                                
009600     EJECT                                                                
009700                                                                          
009800 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
009900     SKIP3                                                                
010000 01  -COPY WZ01SEND                                                       
010100     EJECT                                                                
010200     SKIP3                                                                
010300 01  SEND-AREA                   PIC X(3000).                             
010400     SKIP3                                                                
010500                                                                          
010600 PROCEDURE DIVISION.                                                      
010700 MAIN SECTION.                                                            
010800                                                                          
010900     SKIP2                                                                
011000     PERFORM A-INIT                                                       
011100     PERFORM S04-SEND-OPEN                                                
011200                                                                          
011300     PERFORM S01-READ-INDATA                                              
011400     PERFORM UNTIL END-OF-INDATA                                          
011500*      -- FIX FOR W403 AND PICK-BY-VOICE                                  
011600       INSPECT IN-AREA CONVERTING BROKEN-VBAR TO VBAR                     
011700                                                                          
011800       PERFORM S04-SEND-MESSAGE-FROM-INAREA                               
011900       PERFORM S01-READ-INDATA                                            
012000     END-PERFORM                                                          
012100                                                                          
012200     PERFORM S04-SEND-CLOSE                                               
012300     PERFORM Z-FINIT                                                      
012400                                                                          
012500     MOVE ZERO TO RETURN-CODE                                             
012600     GOBACK                                                               
012700     .                                                                    
012800     EJECT                                                                
012900 A-INIT SECTION.                                                          
013000                                                                          
013100     OPEN INPUT INDATA CTLDATA                                            
013200                                                                          
013300                                                                          
013400*    -- FETCH DESTINATION ADDRESS FROM CONTROL FILE                       
013500     PERFORM S02-READ-CTLDATA                                             
013600     IF END-OF-CTLDATA                                                    
013700       STRING 'WZ1110  CONTROL PARAMETER FILE MISSING'                    
013800       DELIMITED BY SIZE INTO ERRTEXT                                     
013900       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
014000     END-IF                                                               
014100                                                                          
014200     MOVE  SPACE       TO SEND-ADDISPABS                                  
014300     UNSTRING CTL-DATA DELIMITED BY SPACE                                 
014400                     INTO SEND-ADDISPABS                                  
014500                                                                          
014600*    -- FETCH OPTIONAL "EXTRA" ADDRESS FROM SAME FILE                     
014700     MOVE SPACE        TO SEND-ADDISPXTRA                                 
014800     PERFORM S02-READ-CTLDATA                                             
014900     IF NOT END-OF-CTLDATA                                                
015000       UNSTRING CTL-DATA DELIMITED BY SPACE                               
015100                     INTO SEND-ADDISPXTRA                                 
015200       INSPECT SEND-ADDISPXTRA CONVERTING UNDERSCORE TO SPACE             
015300     END-IF                                                               
015400                                                                          
015500*    -- FETCH OPTIONAL RETURN ADDRESS FROM SAME FILE                      
015600     MOVE SPACE        TO SEND-ADDISPABS-RETURN                           
015700     IF NOT END-OF-CTLDATA                                                
015800       PERFORM S02-READ-CTLDATA                                           
015900       IF NOT END-OF-CTLDATA                                              
016000         UNSTRING CTL-DATA DELIMITED BY SPACE                             
016100                     INTO SEND-ADDISPABS-RETURN                           
016200       END-IF                                                             
016300     END-IF                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 Z-FINIT SECTION.                                                         
016700                                                                          
016800     CLOSE INDATA CTLDATA                                                 
016900     .                                                                    
017000     EJECT                                                                
017100 S01-READ-INDATA  SECTION.                                                
017200                                                                          
017300     READ INDATA INTO IN-AREA                                             
017400     AT END                                                               
017500        SET END-OF-INDATA TO TRUE                                         
017600     END-READ                                                             
017700     .                                                                    
017800     EJECT                                                                
017900 S02-READ-CTLDATA SECTION.                                                
018000                                                                          
018100     READ CTLDATA INTO CTL-AREA                                           
018200     AT END                                                               
018300        SET END-OF-CTLDATA TO TRUE                                        
018400     END-READ                                                             
018500     .                                                                    
018600     EJECT                                                                
018700 S04-SEND-OPEN SECTION.                                                   
018800                                                                          
018900     MOVE 'OPEN'                     TO SEND-KDFUNC                       
019000     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
019100     DISPLAY 'OPENING ' SEND-ADDISPABS                                    
019200                                                                          
019300     IF SEND-KDRC > 0                                                     
019400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
019500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
019600       DELIMITED BY SIZE INTO ERRTEXT                                     
019700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
019800     END-IF                                                               
019900     .                                                                    
020000     SKIP3                                                                
020100 S04-SEND-MESSAGE-FROM-INAREA SECTION.                                    
020200                                                                          
020300     MOVE 'PUT'                      TO SEND-KDFUNC                       
020400     CALL WZ01SEND USING SEND-CONTROL-AREA IN-KVDLEN IN-AREA              
020500     DISPLAY 'SENDING ' IN-AREA(1:IN-KVDLEN)                              
020600                                                                          
020700     IF SEND-KDRC > 0                                                     
020800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
020900       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
021000       DELIMITED BY SIZE INTO ERRTEXT                                     
021100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
021200     END-IF                                                               
021300     .                                                                    
021400                                                                          
021500 S04-SEND-CLOSE SECTION.                                                  
021600                                                                          
021700     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
021800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
021900                                                                          
022000     IF SEND-KDRC > 0                                                     
022100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
022200       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
022300       DELIMITED BY SIZE INTO ERRTEXT                                     
022400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022500     END-IF                                                               
022600     .                                                                    
