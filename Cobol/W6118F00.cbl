001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W6118F00.                                                
001400 AUTHOR.         UMESH JAIN.                                              
001500 DATE-WRITTEN.   09/12/21.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        TO UPDATE WDK611 WITH NEW GATE(ADINPORT)                         
002100*                                                                         
002210*        THE PROGRAM UPDATES   WDK6                                       
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003101     SKIP2                                                                
003102*          --- PART NUMBERS WITH UPDATED GATE                             
003110     SELECT W6118C                     ASSIGN TO W6118FD1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003701     SKIP3                                                                
003702 FD  W6118C                                                               
003703     RECORDING       F                                                    
003704     BLOCK CONTAINS  0.                                                   
003705                                                                          
003710*01  -COPY W6118C01      -L.                                              
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W6118F00'.            
004200 01  CHKP-VAR.                                                            
004300     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004800     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004900 77  YES                         PIC X       VALUE 'J'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005100     SKIP2                                                                
005200 01  ERROR-TEXT.                                                          
005300     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
005400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005601                                                                          
005602 77  W6118C-EOF-SW               PIC X       VALUE 'N'.                   
005610     88  END-OF-W6118C                       VALUE 'Y'.                   
005900     EJECT                                                                
006000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES TODAYS-DATE.                                        
006200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006400     03  TODAYS-DATE-DAY         PIC 9(2).                                
006500     EJECT                                                                
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  IN-AREA-START               PIC X(24)   VALUE                        
007403                                             'IN-AREA-START'.             
007404     SKIP2                                                                
007405                                                                          
007410*01  AREA -COPY W6118C01     -PRE IN-                                     
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  KEYS-TILL-DLI.                                                       
008001     03  W-IDARTNR-X.                                                     
008002         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008003     03  W-KDSEGKEY-X.                                                    
008010         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
008100     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FOUND                       VALUE '  '.                  
008500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008700     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
008800     88  IMS-NOT-OK                          VALUE 'XD'.                  
008900     SKIP2                                                                
009000 01  GOOD-STATUSCODES.                                                    
009100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNCTION CODES                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100                                                                          
010201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
010202 01  DLI-IO-WDK601.                                                       
010203*    03  -COPY WDK601                                                     
010204     EJECT                                                                
010205 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010206 01  DLI-IO-WDK611.                                                       
010210*    03  -COPY WDK611                                                     
010300                                                                          
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011000*01  -COPY W0009   -PRE MSG-                                              
011101                                                                          
011102*01  -COPY W0008  -PRE WDK6-                                              
011110     05  FILLER                  PIC X.                                   
011400     EJECT                                                                
011501 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
011502 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
011600                                                                          
011900     PERFORM A-INIT                                                       
012010     PERFORM S01-READ-W6118C                                              
012100     PERFORM UNTIL END-OF-W6118C                                          
012200       IF CHKP-ANT > CHKP-MAX                                             
012300         PERFORM X-TAKE-CHECKPOINT                                        
012400       END-IF                                                             
012500                                                                          
012600       MOVE IN-IDARTNR TO W-IDARTNR                                       
012700       PERFORM IMS-GHU-WDK611                                             
012800       IF SEGMENT-FOUND                                                   
012810         MOVE IN-ADINPORT TO CLAG-ADINPORT                                
012900         PERFORM IMS-REPL-WDK611                                          
012910         ADD +7 TO CHKP-ANT                                               
012920       END-IF                                                             
013000                                                                          
013110       PERFORM S01-READ-W6118C                                            
013200     END-PERFORM                                                          
013400                                                                          
013500     PERFORM Z-FINIT                                                      
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014400     PERFORM IMS-RESTART                                                  
014601                                                                          
014610     OPEN INPUT W6118C                                                    
015310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     .                                                                    
015800     EJECT                                                                
015900 Z-FINIT SECTION.                                                         
016410     CLOSE W6118C                                                         
016602     MOVE 'S' TO POSTSUM-OPKOD                                            
016610     CALL POSTSUM USING POSTSUM-PARM                                      
016800     .                                                                    
016901     EJECT                                                                
016902 S01-READ-W6118C  SECTION.                                                
016904     READ W6118C INTO IN-AREA                                             
016905     AT END                                                               
016907        SET END-OF-W6118C TO TRUE                                         
016908                                                                          
016909     NOT AT END                                                           
016910        MOVE 'W6118C' TO POSTSUM-FDNAMN                                   
016911        MOVE 'W6118FD1' TO POSTSUM-DDNAMN2                                
016912        MOVE SPACE TO POSTSUM-TRANSTYP                                    
016913        CALL POSTSUM USING POSTSUM-PARM                                   
016916     END-READ                                                             
016920     .                                                                    
017200     EJECT                                                                
017300 X-TAKE-CHECKPOINT   SECTION.                                             
017500* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
017600* --- SAVE DATABASE KEYS IF NECESSARY                                     
018000     PERFORM IMS-CHECKPOINT                                               
018100     MOVE ZERO TO CHKP-ANT                                                
018200* --- REREAD DATABASE IF NECESSARY                                        
018300     .                                                                    
018400     EJECT                                                                
018500* --- IMS SECTIONS  ---                                                   
018712 IMS-GHU-WDK611 SECTION.                                                  
018714     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
018715          DELIMITED BY SIZE INTO SSA1                                     
018716     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
018717          DELIMITED BY SIZE INTO SSA2                                     
018718     MOVE '  GE' TO GOOD-STATUSCODES                                      
018719     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
018720     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
018721     PERFORM IMS-STATUSCHECK                                              
018722     .                                                                    
018724 IMS-REPL-WDK611 SECTION.                                                 
018726     MOVE '  ' TO GOOD-STATUSCODES                                        
018727     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
018728     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
018729     PERFORM IMS-STATUSCHECK                                              
018730     .                                                                    
018900 IMS-RESTART SECTION.                                                     
019100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019200     MOVE '  ' TO GOOD-STATUSCODES                                        
019300     CALL CBLTDLI USING XRST MSG-PCB                                      
019400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019500                        CHKP-AREA-LENGTH CHKP-AREA                        
019600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019700     PERFORM IMS-STATUSCHECK                                              
019800     .                                                                    
020000 IMS-CHECKPOINT SECTION.                                                  
020200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020300     MOVE '  XD' TO GOOD-STATUSCODES                                      
020400     CALL CBLTDLI USING CHKP MSG-PCB                                      
020500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020600                        CHKP-AREA-LENGTH CHKP-AREA                        
020700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020800     PERFORM IMS-STATUSCHECK                                              
020900                                                                          
021000     IF IMS-NOT-OK                                                        
021100       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
021110                                        TO ERROR-TEXT-STR                 
021200       DISPLAY ERROR-TEXT                                                 
021300       CALL FELLOG                                                        
021400     END-IF                                                               
021500     .                                                                    
021700 IMS-STATUSCHECK SECTION.                                                 
021900     SET STATUS-IX TO 1                                                   
022000     SEARCH GOOD-STATUS                                                   
022100       AT END                                                             
022200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022300           DELIMITED BY SIZE INTO ERROR-TEXT                              
022400         DISPLAY ERROR-TEXT                                               
022500         CALL FELLOG                                                      
022600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022700         CONTINUE                                                         
022800     END-SEARCH                                                           
022900     .                                                                    
