000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0154200.                                                
000300 AUTHOR.         RICHARD.                                                 
000400 DATE-WRITTEN.   01/10/31.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        HELP-DATABAS (WDP5)                                              
000900*        RENSAR EN DAG GAMLA USER-SEGMENT,                                
001000*        SOM BLIVIT UPPLAGDA VID FRÅGA                                    
001100*        OCH DÄR ÅTERGÅNG GJORTS PÅ FEL SÄTT.                             
001200*        RENSAR EN DAG GAMLA RÖTTER,                                      
001300*        SOM SAKNAR INFORMATIONSSIDOR (BARN-SEGMENT).                     
001400*        MAX 200 TAS BORT PER KÖRNING (INGEN CHECKPOINT).                 
001500*        PROGRAMMET KAN KÖRAS FLERA GÅNGER,                               
001600*        MEN KÖRS NORMALT EN GÅNG PER DAG.                                
001700                                                                          
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 DATA DIVISION.                                                           
002200                                                                          
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600 77  IDPGM                     PIC X(08)   VALUE 'W0154200'.              
002700 77  W-COMPILED                PIC X(16)   VALUE SPACE.                   
002800 77  FELTEXT                   PIC X(80)   VALUE SPACE.                   
002900 77  JA                        PIC X       VALUE 'J'.                     
003000 77  NEJ                       PIC X       VALUE 'N'.                     
003100 77  WS-ANT-RENS               PIC S9(7)   VALUE +0    COMP-3.            
003200 77  WS-ANT-RENSTOM            PIC S9(7)   VALUE +0    COMP-3.            
003300                                                                          
003400 01  W-CURRDATE                  PIC 9(12)   VALUE ZERO.                  
003500 01  FILLER REDEFINES W-CURRDATE.                                         
003600   03  W-DATE                    PIC 9(6).                                
003700   03  W-TIME                    PIC 9(6).                                
003800                                                                          
003900                                                                          
004000                                                                          
004100 01  DYNAMISKA-SUBPROGRAM.                                                
004200   03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.              
004300   03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.              
004400                                                                          
004500     EJECT                                                                
004600 01  FILLER                    PIC X(16)   VALUE 'IMS-WS'.                
004700                                                                          
004800*    --- STATUS-KOD FRÅN IMS                                              
004900 01  STATUS-WS                   PIC XX.                                  
005000     88  SEGMENT-FINNS                       VALUE '  '.                  
005100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
005200     88  BASEN-SLUT                          VALUE 'GB'.                  
005300                                                                          
005400                                                                          
005500 01  GODK-STATUSKODER.                                                    
005600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
005700                                                                          
005800                                                                          
005900 01  SSA1                        PIC X(64).                               
006000                                                                          
006100     EJECT                                                                
006200*    --- IMS FUNKTIONSKODER                                               
006300*01  -COPY W0003                                                          
006400                                                                          
006500     EJECT                                                                
006600*    ---  DLI INPUT-OUTPUT AREA                                           
006700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDP501'.         
006800                                                                          
006900 01  DLI-IO-WDP501.                                                       
007000*  03  -COPY WDP501 -PRE WDP5-                                            
007100                                                                          
007200     EJECT                                                                
007300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDP512'.         
007400                                                                          
007500 01  DLI-IO-WDP512.                                                       
007600*  03  -COPY WDP512 -PRE WDP5-                                            
007700                                                                          
007800     EJECT                                                                
007900 LINKAGE SECTION.                                                         
008000*01  -COPY W0009 -PRE MSG-                                                
008100                                                                          
008200*01  -COPY W0008 -PRE WDP5-                                               
008300     05  FILLER                  PIC X.                                   
008400                                                                          
008500     EJECT                                                                
008600 PROCEDURE DIVISION  USING MSG-PCB WDP5-PCB.                              
008700 MAIN SECTION.                                                            
008800     ENTRY 'DLITCBL' USING MSG-PCB WDP5-PCB.                              
008900                                                                          
009000     PERFORM A-INIT                                                       
009100                                                                          
009200     PERFORM IMS-GET-WDP501                                               
009300     PERFORM UNTIL BASEN-SLUT OR WS-ANT-RENS > 199                        
009400       IF WDP5-INFO-IDDOKTYP = 'USER    '                                 
009500           AND WDP5-INFO-TIREGDAT < W-DATE                                
009600         PERFORM IMS-DELETE                                               
009700         ADD +1 TO WS-ANT-RENS                                            
009800       ELSE                                                               
009900         PERFORM IMS-GNP-WDP512                                           
010000         IF SEGMENT-SAKNAS                                                
010100           PERFORM IMS-GHU-WDP501                                         
010200           PERFORM IMS-DELETE                                             
010300           ADD +1 TO WS-ANT-RENSTOM                                       
010400         END-IF                                                           
010500       END-IF                                                             
010600       PERFORM IMS-GET-WDP501                                             
010700     END-PERFORM                                                          
010800                                                                          
010900     PERFORM Z-FINIT                                                      
011000                                                                          
011100     MOVE ZERO TO RETURN-CODE                                             
011200     GOBACK                                                               
011300     .                                                                    
011400     EJECT                                                                
011500 A-INIT SECTION.                                                          
011600                                                                          
011700     MOVE WHEN-COMPILED TO W-COMPILED                                     
011800     MOVE FUNCTION CURRENT-DATE (3:12) TO W-CURRDATE                      
011900     .                                                                    
012000                                                                          
012100                                                                          
012200 Z-FINIT SECTION.                                                         
012300                                                                          
012400     DISPLAY ' W-CURRDATE         = ' W-CURRDATE                          
012500     DISPLAY ' ANTAL WDP5 RENSADE = ' WS-ANT-RENS                         
012600     DISPLAY ' ANTAL WDP5 TOMROT  = ' WS-ANT-RENSTOM                      
012700     DISPLAY '                      '                                     
012800     .                                                                    
012900                                                                          
013000     EJECT                                                                
013100* --- IMS SEKTIONER ---                                                   
013200                                                                          
013300 IMS-GET-WDP501 SECTION.                                                  
013400                                                                          
013500     MOVE 'WDP501   ' TO SSA1                                             
013600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
013700     CALL CBLTDLI USING GHN WDP5-PCB DLI-IO-WDP501 SSA1                   
013800     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
013900     PERFORM IMS-STATUSKONTROLL                                           
014000     .                                                                    
014100                                                                          
014200                                                                          
014300 IMS-GHU-WDP501 SECTION.                                                  
014400                                                                          
014500     STRING 'WDP501  (WDP501KY =' WDP5-INFO-IDSKYLT                       
014600                                  WDP5-INFO-IDDOKTYP                      
014700                                  WDP5-INFO-IDDOK   ')'                   
014800            DELIMITED BY SIZE INTO SSA1                                   
014900     MOVE '  ' TO GODK-STATUSKODER                                        
015000     CALL CBLTDLI USING GHU WDP5-PCB DLI-IO-WDP501 SSA1                   
015100     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
015200     PERFORM IMS-STATUSKONTROLL                                           
015300     .                                                                    
015400                                                                          
015500                                                                          
015600 IMS-GNP-WDP512 SECTION.                                                  
015700                                                                          
015800     MOVE 'WDP512   ' TO SSA1                                             
015900     MOVE '  GE' TO GODK-STATUSKODER                                      
016000     CALL CBLTDLI USING GNP WDP5-PCB DLI-IO-WDP512 SSA1                   
016100     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
016200     PERFORM IMS-STATUSKONTROLL                                           
016300     .                                                                    
016400                                                                          
016500                                                                          
016600 IMS-DELETE       SECTION.                                                
016700                                                                          
016800     MOVE '    ' TO GODK-STATUSKODER                                      
016900     CALL CBLTDLI USING DLET WDP5-PCB DLI-IO-WDP501                       
017000     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
017100     PERFORM IMS-STATUSKONTROLL                                           
017200     .                                                                    
017300                                                                          
017400                                                                          
017500 IMS-STATUSKONTROLL SECTION.                                              
017600                                                                          
017700     SET STATUS-IX TO 1                                                   
017800     SEARCH GODK-STATUS                                                   
017900       AT END                                                             
018000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
018100           DELIMITED BY SIZE INTO FELTEXT                                 
018200         CALL FELLOG                                                      
018300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
018400         CONTINUE                                                         
018500     END-SEARCH                                                           
018600     .                                                                    
