000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W510WHSU.                                                
000300 AUTHOR.         MAMATHA SHETTY.                                          
000400 DATE-WRITTEN.   23/08/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS IS A SUBPROGRAM TO READ THE SUPPLIER NUMBER FROM DLI        
001000*                                                                         
001100*        THE PROGRAM READS     WDB6                                       
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP2                                                                
002700 FILE SECTION.                                                            
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100 77  IDPGM                       PIC X(8)    VALUE 'W510WHSU'.            
003200 77  YES                         PIC X       VALUE 'J'.                   
003300 77  NOO                         PIC X       VALUE 'N'.                   
003400     EJECT                                                                
003500 01  W-DATE                      PIC 9(6)    VALUE ZERO.                  
003600 01  FILLER REDEFINES W-DATE.                                             
003700     03  W-DATE-AAMM             PIC 9(4).                                
003800     03  W-DATE-DD               PIC 9(2).                                
003900     EJECT                                                                
004000 01  GENERAL-SUBPROGRAMS.                                                 
004100*                                                                         
004200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004500     SKIP2                                                                
004600 01  ERROR-TEXT.                                                          
004700     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
004800     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
004900     EJECT                                                                
005000*    --- AREAS FOR IMS-SECTIONS                                           
005100*                                                                         
005200     EJECT                                                                
005300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
005400     SKIP3                                                                
005410 01  KEYS-FOR-DLI.                                                        
005495     03  W-IDLEVNR-DC-X.                                                  
005496         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
006600*    --- STATUS-KOD FRÅN IMS                                              
006700 01  STATUS-WS                   PIC XX.                                  
006800     88  SEGMENT-FOUND                       VALUE '  '.                  
006900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
007000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
007010     88  SEGMENT-END-OF-DB                   VALUE 'GB'.                  
007100     SKIP2                                                                
007200 01  GOOD-STATUSCODES.                                                    
007300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007400     SKIP3                                                                
007500 01  SSA1                        PIC X(64).                               
007800     EJECT                                                                
007900*    --- IMS FUNCTION CODES                                               
008000*01  -COPY W0003                                                          
008100     EJECT                                                                
008200*    ---  DLI INPUT-OUTPUT AREA                                           
008300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
008400 01  DLI-IO-WDB601.                                                       
008500*    03  -COPY WDB601                                                     
008600 LINKAGE SECTION.                                                         
008700*                                                                         
008800 01  W510WHSU-AREA.                                                       
008900*    03  -COPY W510WHSU                                                   
009000     EJECT                                                                
009100                                                                          
009200*01  -COPY W0008  -PRE WDB6-                                              
009300     05  FILLER                  PIC X.                                   
009400     EJECT                                                                
009500 PROCEDURE DIVISION  USING W510WHSU-AREA WDB6-PCB.                        
009600 MAIN SECTION.                                                            
009700                                                                          
009920                                                                          
009950*    PERFORM A-INIT                                                       
009960     PERFORM B-READ-WDB601                                                
010000     MOVE ZERO TO RETURN-CODE                                             
010100     GOBACK                                                               
010200     .                                                                    
010300     EJECT                                                                
010310 B-READ-WDB601 SECTION.                                                   
010320     MOVE WHSU-IDLEVNR             TO W-IDLEVNR                           
010330     PERFORM IMS-GN-WDB601-IDLEVNR                                        
010340     PERFORM UNTIL SEGMENT-END-OF-DB                                      
010350       IF SEGMENT-FOUND                                                   
010360         IF DCS-IDLEVNR-DC = W-IDLEVNR                                    
010390           MOVE ' '          TO WHSU-KDSVAR                               
010391           MOVE DCS-IDDC     TO WHSU-IDDC                                 
010392           MOVE DCS-IDLEGSEL TO WHSU-IDLEGSEL                             
010393         ELSE                                                             
010394           MOVE '2'      TO WHSU-KDSVAR                                   
010395         END-IF                                                           
010396       END-IF                                                             
010397       PERFORM IMS-GN-WDB601-IDLEVNR                                      
010398     END-PERFORM                                                          
010410     .                                                                    
010500     EJECT                                                                
012900* --- IMS SECTIONS  ---                                                   
013000                                                                          
013100 IMS-GN-WDB601-IDLEVNR SECTION.                                           
013200     STRING 'WDB601  (IDLEVNDC =' W-IDLEVNR-DC-X ')'                      
013210          DELIMITED BY SIZE INTO SSA1                                     
013420     MOVE '    GB' TO GOOD-STATUSCODES                                    
013500     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
013900     MOVE WDB6-STATUS-CODE            TO STATUS-WS                        
014000     PERFORM IMS-STATUSCHECK                                              
014100     .                                                                    
014200     EJECT                                                                
014400                                                                          
014500 IMS-STATUSCHECK SECTION.                                                 
014600                                                                          
014700     SET STATUS-IX TO 1                                                   
014800     SEARCH GOOD-STATUS                                                   
014900       AT END                                                             
015000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015100           DELIMITED BY SIZE INTO ERROR-TEXT                              
015200         DISPLAY ERROR-TEXT                                               
015300         CALL FELLOG                                                      
015400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
015500         CONTINUE                                                         
015600     END-SEARCH                                                           
015700     .                                                                    
