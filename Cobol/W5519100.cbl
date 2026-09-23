001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W5519100.                                                
001400 AUTHOR.         ARCHANA BHAT.                                            
001500 DATE-WRITTEN.   15/08/24.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        THIS PROGRAM UPDATES TIUPPDAT ON WDR4                            
002100*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W5519100'.            
004900 77  YES                         PIC X       VALUE 'J'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005100     SKIP2                                                                
005200 01  ERROR-TEXT.                                                          
005300     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005900     EJECT                                                                
006000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES TODAYS-DATE.                                        
006200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006400     03  TODAYS-DATE-DAY         PIC 9(2).                                
006500     EJECT                                                                
006510 01  WS-UPD-DATE                 PIC 9(6)    VALUE ZERO.                  
006520 01  FILLER REDEFINES WS-UPD-DATE.                                        
006530     03  WS-UPD-YEAR             PIC 9(2).                                
006540     03  WS-UPD-MONTH            PIC 9(2).                                
006550     03  WS-UPD-DAY              PIC 9(2).                                
006560     EJECT                                                                
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  KEYS-TILL-DLI.                                                       
008000     03  W-WDGXKEY-X.                                                     
008001         05  W-IDHTYP            PIC X(4)    VALUE '5119'.                
008002         05  W-WDGXKEY           PIC X(26)   VALUE LOW-VALUE.             
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
010201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5120'.                    
010202 01  DLI-IO-WDGX5120.                                                     
010210*    03  -COPY WDGX5120                                                   
010300                                                                          
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011000*01  -COPY W0009   -PRE MSG-                                              
011101                                                                          
011102*01  -COPY W0008  -PRE 5120-                                              
011110     05  FILLER                  PIC X.                                   
011400     EJECT                                                                
011501 PROCEDURE DIVISION  USING MSG-PCB 5120-PCB.                              
011502 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING MSG-PCB 5120-PCB.                              
011600                                                                          
011800     SKIP2                                                                
011900     PERFORM A-INIT                                                       
012500     PERFORM IMS-GET-WDGX5120                                             
012510     IF SEGMENT-FOUND                                                     
012511        MOVE WS-UPD-DATE TO 5120-TIUPPDAT                                 
012512        PERFORM IMS-REPL-WDGX5120                                         
012520     END-IF                                                               
013300                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014200     SKIP2                                                                
014300                                                                          
015200                                                                          
015300     ACCEPT TODAYS-DATE    FROM DATE                                      
015400     COMPUTE WS-UPD-YEAR = TODAYS-DATE-YEAR - 1                           
015500     MOVE TODAYS-DATE-MONTH  TO WS-UPD-MONTH                              
015510     MOVE TODAYS-DATE-DAY    TO WS-UPD-DAY                                
015600     .                                                                    
015800     EJECT                                                                
018500* --- IMS SECTIONS  ---                                                   
018600                                                                          
018701     EJECT                                                                
018702 IMS-GET-WDGX5120 SECTION.                                                
018703                                                                          
018706     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
018707          DELIMITED BY SIZE INTO SSA1                                     
018708     MOVE 'WDR413  '        TO SSA2                                       
018709     MOVE '  GE'            TO GOOD-STATUSCODES                           
018710     CALL CBLTDLI USING GHU 5120-PCB DLI-IO-WDGX5120 SSA1 SSA2            
018711     MOVE 5120-STATUS-CODE  TO STATUS-WS                                  
018712     PERFORM IMS-STATUSCHECK                                              
018713     .                                                                    
018714     SKIP3                                                                
018715 IMS-REPL-WDGX5120 SECTION.                                               
018716                                                                          
018717     MOVE '  ' TO GOOD-STATUSCODES                                        
018718     CALL CBLTDLI USING REPL 5120-PCB DLI-IO-WDGX5120                     
018719     MOVE 5120-STATUS-CODE TO STATUS-WS                                   
018720     PERFORM IMS-STATUSCHECK                                              
018730     .                                                                    
018800     EJECT                                                                
021700 IMS-STATUSCHECK SECTION.                                                 
021800     SKIP2                                                                
021900     SET STATUS-IX TO 1                                                   
022000     SEARCH GOOD-STATUS                                                   
022100       AT END                                                             
022200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
022300           DELIMITED BY SIZE INTO ERROR-TEXT                              
022400         DISPLAY ERROR-TEXT                                               
022500         CALL FELLOG                                                      
022600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022700         CONTINUE                                                         
022800     END-SEARCH                                                           
022900     .                                                                    
