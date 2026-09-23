000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W0153200.                                                
000400 AUTHOR.         RICHARD.                                                 
000500 DATE-WRITTEN.   96/09/12.                                                
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION:                                                            
001000*        USER-DATABAS.                                                    
001010*        RENSAR DUBELETTER SOM UPPSTÅR NÄR EN USER                        
001100*        ÄR PÅLOGGAD PÅ MER ÄN EN TERMINAL.                               
001200*        MAX 200 TAS BORT PER KÖRNING (INGEN CHECKPOINT).                 
002110                                                                          
002120                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002401                                                                          
002410     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002510                                                                          
002520*    -- CHECKED BY WY2000                                                 
002530                                                                          
002600 77  IDPGM                     PIC X(08)   VALUE 'W0153200'.              
002610 77  W-COMPILED                PIC X(16)   VALUE SPACE.                   
002900 77  FELTEXT                   PIC X(80)   VALUE SPACE.                   
003100 77  JA                        PIC X       VALUE 'J'.                     
003200 77  NEJ                       PIC X       VALUE 'N'.                     
003402 77  WS-DATE                   PIC 9(6)    VALUE ZERO.                    
003403 77  WS-BEANST                 PIC X(8)    VALUE SPACE.                   
003404 77  WS-ANT-RENS               PIC S9(3)   VALUE +0    COMP-3.            
004200                                                                          
004300                                                                          
004310                                                                          
004400 01  DYNAMISKA-SUBPROGRAM.                                                
004600   03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.              
004700   03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.              
004810                                                                          
005300     EJECT                                                                
005400 01  FILLER                    PIC X(16)   VALUE 'IMS-WS'.                
005500                                                                          
006900*    --- STATUS-KOD FRÅN IMS                                              
007000 01  STATUS-WS                   PIC XX.                                  
007100     88  SEGMENT-FINNS                       VALUE '  '.                  
007200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
007300     88  BASEN-SLUT                          VALUE 'GB'.                  
007410                                                                          
007500                                                                          
007600 01  GODK-STATUSKODER.                                                    
007700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007800     SKIP3                                                                
007900 01  SSA1                        PIC X(64).                               
008000     EJECT                                                                
008100*    --- IMS FUNKTIONSKODER                                               
008200*01  -COPY W0003                                                          
008400     EJECT                                                                
008500*    ---  DLI INPUT-OUTPUT AREA                                           
008600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-KOMA01'.         
008700                                                                          
008800 01  DLI-IO-KOMA01.                                                       
009200*  03  -COPY WDP701 -PRE USER-                                            
009400     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010600*01  -COPY W0009 -PRE MSG-                                                
010800                                                                          
010900*01  -COPY W0008 -PRE USER-                                               
011100     05  FILLER                  PIC X.                                   
011600     EJECT                                                                
011700 PROCEDURE DIVISION  USING MSG-PCB USER-PCB.                              
011710 MAIN SECTION.                                                            
011800     ENTRY 'DLITCBL' USING MSG-PCB USER-PCB.                              
012000                                                                          
013700     PERFORM A-INIT                                                       
013710                                                                          
013720     PERFORM IMS-GET-USER                                                 
013800     PERFORM UNTIL BASEN-SLUT OR WS-ANT-RENS > 199                        
013810       MOVE USER-INIT-BEANST TO WS-BEANST                                 
013840       IF WS-BEANST = 'IDUSER ='                                          
013841           AND USER-INIT-TIUPPDAT < WS-DATE                               
013844         PERFORM IMS-DELETE                                               
013845         ADD +1 TO WS-ANT-RENS                                            
014000       END-IF                                                             
015300       PERFORM IMS-GET-USER                                               
015400     END-PERFORM                                                          
015510                                                                          
015520     PERFORM Z-FINIT                                                      
015600                                                                          
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
015910     EJECT                                                                
015911 A-INIT SECTION.                                                          
015912                                                                          
015913     MOVE WHEN-COMPILED TO W-COMPILED                                     
016018     ACCEPT WS-DATE FROM DATE                                             
016047     .                                                                    
016077     EJECT                                                                
016078 Z-FINIT SECTION.                                                         
016081                                                                          
016082     DISPLAY ' ANTAL USER RENSADE = ' WS-ANT-RENS                         
016090     DISPLAY '                              '                             
016091     .                                                                    
016092     EJECT                                                                
016100* --- IMS SEKTIONER ---                                                   
016200                                                                          
018900 IMS-GET-USER SECTION.                                                    
019000                                                                          
019100     MOVE 'WLUSEA01 ' TO SSA1                                             
019300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
019400     CALL CBLTDLI USING GHN USER-PCB DLI-IO-KOMA01 SSA1                   
019500     MOVE USER-STATUS-CODE TO STATUS-WS                                   
019600     PERFORM IMS-STATUSKONTROLL                                           
019700     .                                                                    
021800     SKIP3                                                                
021900 IMS-DELETE       SECTION.                                                
022000                                                                          
022100     MOVE '    ' TO GODK-STATUSKODER                                      
022200     CALL CBLTDLI USING DLET USER-PCB DLI-IO-KOMA01                       
022300     MOVE USER-STATUS-CODE TO STATUS-WS                                   
022400     PERFORM IMS-STATUSKONTROLL                                           
022500     .                                                                    
022600     EJECT                                                                
022700 IMS-STATUSKONTROLL SECTION.                                              
022800                                                                          
022900     SET STATUS-IX TO 1                                                   
023000     SEARCH GODK-STATUS                                                   
023100       AT END                                                             
023200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023300           DELIMITED BY SIZE INTO FELTEXT                                 
023400         CALL FELLOG                                                      
023500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023510         CONTINUE                                                         
023600     END-SEARCH                                                           
023700     .                                                                    
