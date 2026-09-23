001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W476SHNO.                                                
001300 AUTHOR.         KARANDE DIGAMBAR.                                        
001400 DATE-WRITTEN.   02/08/09.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        SUBPROGRAM TO TAKE IDSHIPM                                       
001900*                                                                         
002000*        THE PROGRAM UPDATES   WDGX4518                                   
002100*                                                                         
002200*    ABENDCODES:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W476SHNO'.            
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004800     EJECT                                                                
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100     SKIP2                                                                
006800 01  ERRTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
007000     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
007400     EJECT                                                                
007500*    --- AREAS FOR IMS-SECTIONS                                           
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008100 01  KEYS-TO-DLI.                                                         
008101                                                                          
008110     03  W-4517-X.                                                        
008120         05  W-IDHTYP            PIC X(4)    VALUE '4517'.                
008130         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
008140                                                                          
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FOUND                       VALUE '  '.                  
008600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008800     SKIP2                                                                
008900 01  GOOD-STATUSCODES.                                                    
009000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNCTION CODES                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4518'.                    
010100 01  DLI-IO-WDGX4518.                                                     
010200*    03  -COPY WDGX4518                                                   
010210                                                                          
010300     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010600*                                                                         
010610*   -COPY W476SHNO                                                        
010620     EJECT                                                                
010621                                                                          
010630*01  -COPY W0008      -PRE 4517-                                          
010640     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010800                                                                          
010801 PROCEDURE DIVISION  USING  SHNO-W476SHNO  4517-PCB.                      
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING  SHNO-W476SHNO  4517-PCB.                      
010900                                                                          
011100                                                                          
011110     PERFORM IMS-GHU-WDGX4518                                             
011111                                                                          
011120     MOVE 4518-IDSHIPM-NEXT       TO SHNO-IDSHIPM                         
011121                                                                          
011130     IF 4518-IDSHIPM-NEXT         =  4518-IDSHIPM-MAX                     
011140       MOVE 4518-IDSHIPM-MIN      TO 4518-IDSHIPM-NEXT                    
011150     ELSE                                                                 
011160       ADD 1                      TO 4518-IDSHIPM-NEXT                    
011170     END-IF                                                               
011171                                                                          
011180     PERFORM IMS-REPL-WDGX4518                                            
012700                                                                          
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200                                                                          
015500* --- IMS SECTIONS  ---                                                   
015600                                                                          
015700 IMS-GHU-WDGX4518             SECTION.                                    
015710                                                                          
015720     STRING 'WDR101  (WDGXKEY  =' W-4517-X   ')'                          
015730             DELIMITED BY SIZE INTO SSA1                                  
015740     MOVE 'WDGX4518' TO SSA2                                              
015750     MOVE '  ' TO GOOD-STATUSCODES                                        
015760     CALL CBLTDLI USING GHU 4517-PCB DLI-IO-WDGX4518  SSA1 SSA2           
015770     MOVE 4517-STATUS-CODE TO STATUS-WS                                   
015780     PERFORM IMS-STATUSCHECK                                              
015790     .                                                                    
015791     SKIP2                                                                
015792 IMS-REPL-WDGX4518 SECTION.                                               
015793     MOVE '  ' TO GOOD-STATUSCODES                                        
015794     CALL CBLTDLI USING REPL 4517-PCB DLI-IO-WDGX4518                     
015795     MOVE 4517-STATUS-CODE TO STATUS-WS                                   
015796     PERFORM IMS-STATUSCHECK                                              
015797     .                                                                    
015798     EJECT                                                                
015800                                                                          
015900 IMS-STATUSCHECK SECTION.                                                 
016000                                                                          
016100     SET STATUS-IX TO 1                                                   
016200     SEARCH GOOD-STATUS                                                   
016300       AT END                                                             
016400         STRING ' FEL STATUSCODE FROM IMS: ' STATUS-WS                    
016500           DELIMITED BY SIZE INTO ERRTEXT                                 
016600         DISPLAY ERRTEXT                                                  
016700         CALL FELLOG                                                      
016800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
