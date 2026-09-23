000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4264900.                                                
000400*AUTHOR.         ELAINE CURTSSON.                                         
000500*DATE-WRITTEN.   92/01/30.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UPPDATERAR KONTROLLRAPPORTS VERIFIKATIONSNUMMERSERIE             
001100*        PÅ W6G1. ANDRA SIFFRAN I VERNR ÄR ÅRSSIFFRA.                     
001200*        IDHTYP 6002 INNEHÅLLER 4 VERNR SERIER.                           
001300*        EFTER SPLIT ANVÄNDS NR 3 OCH 4 FÖR PVS KONTROLL-                 
001400*        RAPPORTER.                                                       
001500*        PROGRAMMET UPPDATERAR W6LOPB (W6G1)                              
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400     SKIP2                                                                
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W4264900'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
003910 77  W-DIV-ANS                   PIC S9(3) COMP-3 VALUE ZERO.             
003920 77  W-REMAINDER                 PIC S9(3) COMP-3 VALUE ZERO.             
004000     SKIP2                                                                
004100 01  W-IDVERNR                   PIC 9(8).                                
004600     SKIP2                                                                
004700 01  FELTEXT.                                                             
004800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005000     SKIP2                                                                
005100 01  DAGENS-DATUM                PIC 9(6).                                
005200 01  FILLER REDEFINES DAGENS-DATUM.                                       
005300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005600     EJECT                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100*                                                                         
006200     EJECT                                                                
006300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006400     SKIP3                                                                
006500 01  NYCKLAR-TILL-DLI.                                                    
006600     03  W-W6GX-6001-KEY-X.                                               
006700         05  W-IDHTYP-6001       PIC X(04)    VALUE '6001'.               
006800         05  W-FILLER            PIC X(26)    VALUE LOW-VALUE.            
006900     SKIP2                                                                
006910 01  W-SET-YEAR                  PIC X       VALUE 'N'.                   
006920     88  EVEN-YEAR                           VALUE 'J'.                   
006930     88  ODD-YEAR                            VALUE 'N'.                   
007000*    --- STATUS-KOD FRÅN IMS                                              
007100 01  STATUS-WS                   PIC XX.                                  
007200     88  SEGMENT-FINNS                       VALUE '  '.                  
007300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
007400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
007500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
007600     88  IMS-EJ-OK                           VALUE 'XD'.                  
007700     SKIP2                                                                
007800 01  GODK-STATUSKODER.                                                    
007900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008000     SKIP3                                                                
008100 01  SSA1                        PIC X(64).                               
008200 01  SSA2                        PIC X(64).                               
008300     EJECT                                                                
008400*    --- IMS FUNKTIONSKODER                                               
008500*01  -COPY W0003                                                          
008600     EJECT                                                                
008700*    ---  DLI INPUT-OUTPUT AREA                                           
008800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
008900     SKIP3                                                                
009000 01  DLI-IO-AREA.                                                         
009100     03  IO-AREA                 PIC X(110)  VALUE SPACE.                 
009200     SKIP3                                                                
009300     03  W6LOPB11 REDEFINES IO-AREA.                                      
009400*        05  -COPY W6GX6002                                               
009500     EJECT                                                                
009600 LINKAGE SECTION.                                                         
009700                                                                          
009800*01  -COPY W0009   -PRE MSG-                                              
009900     EJECT                                                                
010000*01  -COPY W0008  -PRE LOPB-                                              
010100     05  FILLER                  PIC X.                                   
010200     EJECT                                                                
010300 PROCEDURE DIVISION  USING MSG-PCB LOPB-PCB.                              
010400     ENTRY 'DLITCBL' USING MSG-PCB LOPB-PCB.                              
010500                                                                          
010600* * * * * * * * * * * * * * * * * * * * * * *                             
010700*   UPPDATERAR VERIFIKATIONSNUMMERSERIE     *                             
010800*                                           *                             
010900*   INDEX 1 =  LV OCH C1   ANVÄNDS EJ       *                             
011000*   INDEX 2 =    "    C2   ANVÄNDS EJ       *                             
011100*   INDEX 3 =  PV OCH C1                    *                             
011200*   INDEX 4 =    "    C2                    *                             
011300*                                           *                             
011400* * * * * * * * * * * * * * * * * * * * * * *                             
011500     ACCEPT DAGENS-DATUM FROM DATE                                        
011510     DIVIDE DAGENS-DATUM-AAR BY 2                                         
011520            GIVING W-DIV-ANS                                              
011530            REMAINDER W-REMAINDER                                         
011540                                                                          
011550     IF W-REMAINDER = ZERO                                                
011560       MOVE JA       TO W-SET-YEAR                                        
011570     ELSE                                                                 
011580       MOVE NEJ      TO W-SET-YEAR                                        
011590     END-IF                                                               
011600                                                                          
011700     PERFORM IMS-GU-W6LOPB01                                              
011800     PERFORM IMS-GHNP-W6LOPB11                                            
011900                                                                          
012000     MOVE '1'        TO 6002-KDSEGKEY                                     
012100                                                                          
012200*   VERNR                                                                 
012300*                                                                         
012310*   FOR CHINA IDFTG 60                                                    
012312     IF EVEN-YEAR                                                         
012313       MOVE 60000001         TO 6002-IDVERNR-MIN (1)                      
012314                                6002-IDVERNR-AKT (1)                      
012315       MOVE 60050000         TO 6002-IDVERNR-MAX (1)                      
012316     ELSE                                                                 
012317       MOVE 60050001         TO 6002-IDVERNR-MIN (1)                      
012318                                6002-IDVERNR-AKT (1)                      
012319       MOVE 60099999         TO 6002-IDVERNR-MAX (1)                      
012320     END-IF                                                               
012321                                                                          
012400*   FOR VCCS IDFTG 57                                                     
012410     IF EVEN-YEAR                                                         
012420       MOVE 57000001         TO 6002-IDVERNR-MIN (3)                      
012430                                6002-IDVERNR-AKT (3)                      
012440       MOVE 57050000         TO 6002-IDVERNR-MAX (3)                      
012450     ELSE                                                                 
012460       MOVE 57050001         TO 6002-IDVERNR-MIN (3)                      
012470                                6002-IDVERNR-AKT (3)                      
012480       MOVE 57099999         TO 6002-IDVERNR-MAX (3)                      
012490     END-IF                                                               
012491                                                                          
012492*   FOR USA IDFTG 53                                                      
012493     IF EVEN-YEAR                                                         
012494       MOVE 53000001         TO 6002-IDVERNR-MIN (4)                      
012495                                6002-IDVERNR-AKT (4)                      
012496       MOVE 53050000         TO 6002-IDVERNR-MAX (4)                      
012497     ELSE                                                                 
012498       MOVE 53050001         TO 6002-IDVERNR-MIN (4)                      
012499                                6002-IDVERNR-AKT (4)                      
012500       MOVE 53099999         TO 6002-IDVERNR-MAX (4)                      
012501     END-IF                                                               
014900                                                                          
015000     PERFORM IMS-REPL-W6LOPB11                                            
015100                                                                          
015200     MOVE ZERO TO RETURN-CODE                                             
015300                                                                          
015400     GOBACK                                                               
015500     .                                                                    
015600     EJECT                                                                
015700* --- IMS SEKTIONER ---                                                   
015800     SKIP3                                                                
015900     EJECT                                                                
016000 IMS-GU-W6LOPB01 SECTION.                                                 
016100     STRING 'W6LOPB01(W6GXKEY  =' W-W6GX-6001-KEY-X ')'                   
016200          DELIMITED BY SIZE INTO SSA1                                     
016300     MOVE '  ' TO GODK-STATUSKODER                                        
016400     CALL CBLTDLI USING GU LOPB-PCB DLI-IO-AREA SSA1                      
016500     MOVE LOPB-STATUS-CODE TO STATUS-WS                                   
016600     PERFORM IMS-STATUSKONTROLL                                           
016700     .                                                                    
016800     SKIP2                                                                
016900 IMS-GHNP-W6LOPB11 SECTION.                                               
017000     MOVE 'W6LOPB11 '         TO SSA1                                     
017100     MOVE '  ' TO GODK-STATUSKODER                                        
017200     CALL CBLTDLI USING GHNP LOPB-PCB DLI-IO-AREA SSA1                    
017300     MOVE LOPB-STATUS-CODE TO STATUS-WS                                   
017400     PERFORM IMS-STATUSKONTROLL                                           
017500     .                                                                    
017600     SKIP2                                                                
017700 IMS-REPL-W6LOPB11 SECTION.                                               
017800     MOVE '  ' TO GODK-STATUSKODER                                        
017900     CALL CBLTDLI USING REPL LOPB-PCB DLI-IO-AREA                         
018000     MOVE LOPB-STATUS-CODE TO STATUS-WS                                   
018100     PERFORM IMS-STATUSKONTROLL                                           
018200     .                                                                    
018300     EJECT                                                                
018400 IMS-STATUSKONTROLL SECTION.                                              
018500     SKIP2                                                                
018600     SET STATUS-IX TO 1                                                   
018700     SEARCH GODK-STATUS                                                   
018800       AT END                                                             
018900         MOVE 'FEL STATUS'  TO FELTEXT-STR                                
019000         DISPLAY FELTEXT                                                  
019100         CALL FELLOG                                                      
019200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
019300     END-SEARCH                                                           
019400     .                                                                    
