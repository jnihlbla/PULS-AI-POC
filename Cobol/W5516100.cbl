001100 ID DIVISION.                                                             
001200                                                                          
001300 PROGRAM-ID.     W5516100.                                                
001400*AUTHOR.         KARL JOHAN HANSSON.                                      
001500*DATE-WRITTEN.   94/09/19.                                                
001600                                                                          
001700*    REMARKS                                                              
001800*                                                                         
001900*    FUNKTION:                                                            
002210*        PROGRAMMET UPPDATERAR HÄNDELSEBASEN WLXXEH (WDR4) MED            
002220*        LADD-DATUM NÄR LADDNINGEN AV WDC6 (SPIS-BASEN) SKER.             
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 -  . . . .                                                 
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100                                                                          
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003700                                                                          
003800 DATA DIVISION.                                                           
003900                                                                          
004000 FILE SECTION.                                                            
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W5516100'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004910     SKIP2                                                                
004920 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200                                                                          
005300 01  DATUM                       PIC S9(7)   VALUE ZERO  COMP-3.          
006200                                                                          
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     EJECT                                                                
007400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007500     SKIP3                                                                
007600 01  NYCKLAR-TILL-DLI.                                                    
007701     03  W-WDGXKEY-X.                                                     
007710         05  W-IDHTYP            PIC X(4)     VALUE '5119'.               
007720         05  W-WDGXKEY           PIC X(26)    VALUE LOW-VALUE.            
007800     SKIP2                                                                
007900*    --- STATUS-KOD FRÅN IMS                                              
008000 01  STATUS-WS                   PIC XX.                                  
008100     88  SEGMENT-FINNS                       VALUE '  '.                  
008200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008500     88  IMS-EJ-OK                           VALUE 'XD'.                  
008600     SKIP2                                                                
008700 01  GODK-STATUSKODER.                                                    
008800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008900     SKIP3                                                                
009000 01  SSA1                        PIC X(64).                               
009100 01  SSA2                        PIC X(64).                               
009200     EJECT                                                                
009300*    --- IMS FUNKTIONSKODER                                               
009400*01  -COPY W0003                                                          
009500     EJECT                                                                
009700*    ---  DLI INPUT-OUTPUT AREA                                           
009800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
009900                                                                          
010000 01  DLI-IO-AREA.                                                         
010100     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
010201     SKIP3                                                                
010202     03  WLXXEH11 REDEFINES IO-AREA.                                      
010210*        05  -COPY WDGX5120  -PRE XXEH-                                   
010500     EJECT                                                                
010600 LINKAGE SECTION.                                                         
010700                                                                          
010800*01  -COPY W0009   -PRE MSG-                                              
010901                                                                          
010902*01  -COPY W0008  -PRE XXEH-                                              
010910     05  FILLER                  PIC X.                                   
011200     EJECT                                                                
011301 PROCEDURE DIVISION  USING MSG-PCB XXEH-PCB.                              
011310     ENTRY 'DLITCBL' USING MSG-PCB XXEH-PCB.                              
011400                                                                          
011700     ACCEPT DATUM FROM DATE                                               
012000                                                                          
012100     PERFORM IMS-GHU-XXEH11                                               
012200     IF SEGMENT-FINNS                                                     
012300       MOVE DATUM      TO XXEH-5120-TIUPPDAT                              
012400       PERFORM IMS-REPL-XXEH11                                            
012500     END-IF                                                               
012800                                                                          
013200     MOVE ZERO TO RETURN-CODE                                             
013300     GOBACK                                                               
015500     .                                                                    
015900     EJECT                                                                
016000* --- IMS SEKTIONER ---                                                   
016100     SKIP3                                                                
016201     EJECT                                                                
016202 IMS-GHU-XXEH11 SECTION.                                                  
016203                                                                          
016204     STRING 'WLXXEH01(WDGXKEY  =' W-WDGXKEY-X ')'                         
016205          DELIMITED BY SIZE INTO SSA1                                     
016206     MOVE 'WLXXEH11 '        TO SSA2                                      
016207     MOVE '  GE'             TO GODK-STATUSKODER                          
016208     CALL CBLTDLI USING GHU XXEH-PCB DLI-IO-AREA SSA1 SSA2                
016209     MOVE XXEH-STATUS-CODE   TO STATUS-WS                                 
016210     PERFORM IMS-STATUSKONTROLL                                           
016211     .                                                                    
016212     SKIP3                                                                
016213 IMS-REPL-XXEH11 SECTION.                                                 
016214                                                                          
016215     MOVE '  ' TO GODK-STATUSKODER                                        
016216     CALL CBLTDLI USING REPL XXEH-PCB DLI-IO-AREA                         
016217     MOVE XXEH-STATUS-CODE TO STATUS-WS                                   
016218     PERFORM IMS-STATUSKONTROLL                                           
016220     .                                                                    
016300     EJECT                                                                
016400 IMS-STATUSKONTROLL SECTION.                                              
016500     SKIP2                                                                
016600     SET STATUS-IX TO 1                                                   
016700     SEARCH GODK-STATUS                                                   
016800       AT END                                                             
016900         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
017000         DISPLAY FELTEXT                                                  
017100         CALL FELLOG                                                      
017200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017300         CONTINUE                                                         
017400     END-SEARCH                                                           
017500     .                                                                    
