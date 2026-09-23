000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W510CURR.                                                
000300 AUTHOR.         BHAT ARCHANA.                                            
000400 DATE-WRITTEN.   20/03/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS IS A SUBPROGRAM TO READ THE CURRENCY DB TO FETCH            
000900*        THE CURRENCY RATE                                                
001000*                                                                         
001100*        THE PROGRAM READS     WDG2                                       
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
003100 77  IDPGM                       PIC X(8)    VALUE 'W510CURR'.            
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
005500 01  KEYS-FOR-DLI.                                                        
005600     03  W-WDGX9305-X.                                                    
005700         05  W-IDHTYP             PIC X(4)    VALUE '9305'.               
005800         05  W-KDVALISO-HUV       PIC X(3)    VALUE SPACE.                
005900         05  W-KDVALTYP           PIC X(1)    VALUE SPACE.                
006000         05  FILLER               PIC X(22)   VALUE LOW-VALUE.            
006100     03  W-KDVALISO-X.                                                    
006200         05  W-KDVALISO-ROW       PIC X(3)    VALUE SPACE.                
006300     03  W-TISTADA9-X.                                                    
006400         05  W-TISTADAT-9KOMPL    PIC S9(7)   VALUE ZERO COMP-3.          
006500     SKIP2                                                                
006600*    --- STATUS-KOD FRÅN IMS                                              
006700 01  STATUS-WS                   PIC XX.                                  
006800     88  SEGMENT-FOUND                       VALUE '  '.                  
006900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
007000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
007100     SKIP2                                                                
007200 01  GOOD-STATUSCODES.                                                    
007300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007400     SKIP3                                                                
007500 01  SSA1                        PIC X(64).                               
007600 01  SSA2                        PIC X(64).                               
007700 01  SSA3                        PIC X(64).                               
007800     EJECT                                                                
007900*    --- IMS FUNCTION CODES                                               
008000*01  -COPY W0003                                                          
008100     EJECT                                                                
008200*    ---  DLI INPUT-OUTPUT AREA                                           
008300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
008400 01  DLI-IO-WDGX9308.                                                     
008500*    03  -COPY WDGX9308                                                   
008600 LINKAGE SECTION.                                                         
008700*                                                                         
008800 01  W510CURR-AREA.                                                       
008900*    03  -COPY W510CURR                                                   
009000     EJECT                                                                
009100                                                                          
009200*01  -COPY W0008  -PRE 9305-                                              
009300     05  FILLER                  PIC X.                                   
009400     EJECT                                                                
009500 PROCEDURE DIVISION  USING W510CURR-AREA 9305-PCB.                        
009600 MAIN SECTION.                                                            
009900                                                                          
010000     PERFORM A-GET-CURRENCY                                               
010100                                                                          
010200     MOVE ZERO TO RETURN-CODE                                             
010300     GOBACK                                                               
010400     .                                                                    
010500     EJECT                                                                
010600 A-GET-CURRENCY SECTION.                                                  
010700                                                                          
010800     MOVE SPACES                   TO CURR-KDSVAR                         
010900                                                                          
011000     MOVE CURR-KDVALISO-HUV        TO W-KDVALISO-HUV                      
011100     MOVE CURR-KDVALISO-ROW        TO W-KDVALISO-ROW                      
011200     MOVE CURR-KDVALTYP            TO W-KDVALTYP                          
011300                                                                          
011400     MOVE CURR-TIAAMM              TO W-DATE-AAMM                         
011500     MOVE 01                       TO W-DATE-DD                           
011600     COMPUTE W-TISTADAT-9KOMPL = 9999999 - W-DATE                         
011700                                                                          
011800     PERFORM IMS-GU-WDGX9308                                              
011900     IF SEGMENT-MISSING                                                   
012000       MOVE '2'                    TO CURR-KDSVAR                         
012100     ELSE                                                                 
012200       MOVE ' '                    TO CURR-KDSVAR                         
012300       MOVE 9308-PRKURS            TO CURR-PRKURS-NEW                     
012400       MOVE 9308-REVALUTA-TO       TO CURR-REVALUTA-TO                    
012500       MOVE 9308-REVALUTA-FROM     TO CURR-REVALUTA-FROM                  
012600       MOVE 9308-TISTADAT          TO CURR-TISTADAT                       
012700       MOVE 9308-TIREGDAT          TO CURR-TIREGDAT                       
012800     END-IF                                                               
012900     .                                                                    
013000     EJECT                                                                
013100* --- IMS SECTIONS  ---                                                   
013200                                                                          
013300 IMS-GU-WDGX9308 SECTION.                                                 
013400     STRING 'WDG201  (WDGXKEY  =' W-WDGX9305-X ')'                        
013500             DELIMITED BY SIZE INTO SSA1                                  
013600     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
013700             DELIMITED BY SIZE INTO SSA2                                  
013800     STRING 'WDGX9308(TISTADA9 =' W-TISTADA9-X ')'                        
013900             DELIMITED BY SIZE INTO SSA3                                  
014000     MOVE '  GE'   TO GOOD-STATUSCODES                                    
014100     CALL CBLTDLI USING GU 9305-PCB DLI-IO-WDGX9308 SSA1 SSA2 SSA3        
014200     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
014300     PERFORM IMS-STATUSCHECK                                              
014400     .                                                                    
014500     SKIP3                                                                
014600                                                                          
014700 IMS-STATUSCHECK SECTION.                                                 
014800                                                                          
014900     SET STATUS-IX TO 1                                                   
015000     SEARCH GOOD-STATUS                                                   
015100       AT END                                                             
015200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015300           DELIMITED BY SIZE INTO ERROR-TEXT                              
015400         DISPLAY ERROR-TEXT                                               
015500         CALL FELLOG                                                      
015600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
015700         CONTINUE                                                         
015800     END-SEARCH                                                           
015900     .                                                                    
