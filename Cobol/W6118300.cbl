001300 ID DIVISION.                                                             
001400     SKIP2                                                                
001500 PROGRAM-ID.     W6118300.                                                
001600*AUTHOR.         BERT ANDERSSON.                                          
001700*DATE-WRITTEN.   92/04/30.                                                
001800                                                                          
001900*    REMARKS.                                                             
002000*                                                                         
002100*    FUNKTION:                                                            
002200*        PROGRAMMET ÄR EN BMP SOM LÄSER W6G1 (W6PLAA) OCH                 
002300*        SÄTTER OM ITEM FLKVARED TILL NEJ. DVS DE OMRÅDEN                 
002400*        SOM HAR HAFT REDUCERAD KONTROLL UNDER DAGEN SKALL                
002500*        FLAGGAN TAS BORT FRÅN.                                           
002600*                                                                         
002700*        PROGRAMMET UPPDATERAR W6PLAA (W6G1)                              
002800*                                                                         
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003700     SKIP2                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP2                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W6118300'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700     SKIP2                                                                
004800 01  FELTEXT.                                                             
004900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005100     SKIP2                                                                
005110*      --- VALID IDDC CODES                                               
005120*                                                                         
005130*01    -COPY WWDC99                                                       
005130*01    -COPY WWDCKONS                                                     
005140       EJECT                                                              
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300*                                                                         
005400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005600*                                                                         
005700     EJECT                                                                
005800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
005900     SKIP3                                                                
006000 01  NYCKLAR-TILL-DLI.                                                    
006100     03  W-W6GXKEY-6005-X.                                                
006200         05  FILLER              PIC  X(04)   VALUE '6005'.               
006210         05  W-6005-IDDC         PIC  X(2)    VALUE SPACE.                
006220         05  FILLER              PIC  X(24)   VALUE LOW-VALUE.            
006221                                                                          
006500     SKIP2                                                                
006600*    --- STATUS-KOD FRÅN IMS                                              
006700 01  STATUS-WS                   PIC XX.                                  
006800     88  SEGMENT-FINNS                       VALUE '  '.                  
006900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
007000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
007100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
007200     88  IMS-EJ-OK                           VALUE 'XD'.                  
007300     SKIP2                                                                
007400 01  GODK-STATUSKODER.                                                    
007500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007600     SKIP3                                                                
007700 01  SSA1                        PIC X(64).                               
007800 01  SSA2                        PIC X(64).                               
007900     SKIP2                                                                
008000*    --- IMS FUNKTIONSKODER                                               
008100*01  -COPY W0003                                                          
008200     SKIP2                                                                
008300*    ---  DLI INPUT-OUTPUT AREA                                           
008400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
008500     SKIP3                                                                
008600 01  DLI-IO-AREA.                                                         
008700     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
008800     SKIP3                                                                
009200     03  W6PLAA11 REDEFINES IO-AREA.                                      
009300*        05  -COPY W6GX6006                                               
009400     EJECT                                                                
009500 LINKAGE SECTION.                                                         
009600                                                                          
009700*01  -COPY W0009   -PRE MSG-                                              
009800     SKIP2                                                                
009900*01  -COPY W0008  -PRE PLAA-                                              
010000     05  FILLER                  PIC X.                                   
010100     EJECT                                                                
010200 PROCEDURE DIVISION  USING MSG-PCB PLAA-PCB.                              
010300     ENTRY 'DLITCBL' USING MSG-PCB PLAA-PCB.                              
010400                                                                          
010500     PERFORM A-INIT                                                       
010510                                                                          
010520     MOVE  WC-CDC-SE TO W-6005-IDDC                                       
010610     PERFORM IMS-GU-PLAA01                                                
010620     PERFORM IMS-GHNP-PLAA11                                              
010800                                                                          
011000     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
011200       PERFORM B-KONTROLL-EV-UPPDAT                                       
011410       PERFORM IMS-GHNP-PLAA11                                            
011500     END-PERFORM                                                          
011510                                                                          
011600     MOVE WC-CDC-TR TO W-6005-IDDC                                        
011710     PERFORM IMS-GU-PLAA01                                                
011720     PERFORM IMS-GHNP-PLAA11                                              
011801                                                                          
011802     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
011803       PERFORM B-KONTROLL-EV-UPPDAT                                       
011804       PERFORM IMS-GHNP-PLAA11                                            
011805     END-PERFORM                                                          
011806                                                                          
012200     MOVE ZERO TO RETURN-CODE                                             
012300     GOBACK                                                               
012400     .                                                                    
012500     EJECT                                                                
012600 A-INIT SECTION.                                                          
012800     CONTINUE                                                             
013100     .                                                                    
013200     SKIP2                                                                
014300 B-KONTROLL-EV-UPPDAT   SECTION.                                          
014400                                                                          
014410     MOVE W-6005-IDDC   TO WS-IDDC                                        
014500     IF CDC-SE                                                            
014510       IF 6006-KDINLOMR = 'FB' OR 'FBP'                                   
014600         IF 6006-FLKVARED = JA                                            
014700           MOVE NEJ                 TO 6006-FLKVARED                      
014800           PERFORM IMS-REPL-PLAA                                          
014900         END-IF                                                           
015000       END-IF                                                             
015100     ELSE                                                                 
015110       IF 6006-KDINLOMR = 'FB' OR 'FBP'                                   
015120         IF 6006-FLKVARED = JA                                            
015130           MOVE NEJ                 TO 6006-FLKVARED                      
015140           PERFORM IMS-REPL-PLAA                                          
015150         END-IF                                                           
015160       END-IF                                                             
015161     END-IF                                                               
015170     .                                                                    
015200     SKIP2                                                                
015700* --- IMS SEKTIONER ---                                                   
015800     SKIP2                                                                
015900 IMS-GU-PLAA01 SECTION.                                                   
015910                                                                          
016000     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
016100          DELIMITED BY SIZE INTO SSA1                                     
016200     MOVE '  GE' TO GODK-STATUSKODER                                      
016300     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1                      
016400     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
016500     PERFORM IMS-STATUSKONTROLL                                           
016600     .                                                                    
016700     SKIP2                                                                
016800 IMS-GHNP-PLAA11 SECTION.                                                 
016900                                                                          
017100     MOVE 'W6PLAA11'                TO SSA1                               
017110     MOVE '  GE' TO GODK-STATUSKODER                                      
017200     CALL CBLTDLI USING GHNP PLAA-PCB DLI-IO-AREA SSA1                    
017300     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
017400     PERFORM IMS-STATUSKONTROLL                                           
017500     .                                                                    
017600     SKIP3                                                                
017700 IMS-REPL-PLAA SECTION.                                                   
017800                                                                          
017900     MOVE '  ' TO GODK-STATUSKODER                                        
018000     CALL CBLTDLI USING REPL PLAA-PCB DLI-IO-AREA                         
018100     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
018200     PERFORM IMS-STATUSKONTROLL                                           
018300     .                                                                    
018400     SKIP2                                                                
018500 IMS-STATUSKONTROLL SECTION.                                              
018600                                                                          
018700     SET STATUS-IX TO 1                                                   
018800     SEARCH GODK-STATUS                                                   
018900       AT END                                                             
019000         MOVE 'FELAKTIG STATUSKOD FRÅN IMS' TO FELTEXT-STR                
019100         DISPLAY FELTEXT                                                  
019200         CALL FELLOG                                                      
019300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
019400         CONTINUE                                                         
019500     END-SEARCH                                                           
019600     .                                                                    
