000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2615800.                                                
000300 AUTHOR.         P-A HELGEGREN.                                           
000400 DATE-WRITTEN.   SEP 2007.                                                
000500*        PROGRAMMET ÄR ETT SB-PROGRAM SOM LÄSER WDK6 OCH                  
000600*        SKRIVER UT ARTKLAR MED TISKROT-AUTO > DAGENS-DATUM               
000700     EJECT                                                                
000800 ENVIRONMENT DIVISION.                                                    
000900 INPUT-OUTPUT SECTION.                                                    
001000 FILE-CONTROL.                                                            
001100     SKIP2                                                                
001200*---------------------------------------                                  
001300*                                        OUTPUT                           
001400                                                                          
001500         SELECT UTFIL          ASSIGN TO UT-S-W26158D1.                   
001600                                                                          
001700     SKIP3                                                                
001800 DATA DIVISION.                                                           
001900 FILE SECTION.                                                            
002000 FD  UTFIL                                                                
002100     RECORDING F                                                          
002200     BLOCK 0                                                              
002300     LABEL RECORD STANDARD.                                               
002400                                                                          
002500*01  POST    -COPY W26159 -PRE UT- -L.                                    
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800     SKIP3                                                                
002900                                                                          
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  JA                      PIC X        VALUE 'J'.                      
003300 77  NEJ                     PIC X        VALUE 'N'.                      
003400                                                                          
003500 01  W-ANTAL-POST            PIC S9(7)    COMP-3 VALUE ZERO.              
003600 01  WS-DAGENS-DATUM         PIC S9(7)    COMP-3 VALUE ZERO.              
003700     SKIP3                                                                
003800 01  DYNAMISKA-SUBPROGRAM.                                                
003900  03 POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.                 
004000  03 CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.                 
004200  03 FELLOG                  PIC X(8)    VALUE 'FELLOG '.                 
004300  03 WDATKONV                PIC X(8)    VALUE 'WDATKONV'.                
004400     EJECT                                                                
004500 01  FILLER  PIC X(16)  VALUE 'POSTSUM'.                                  
004600                                                                          
004700*01          -COPY W0005    -PRE POSTSUM-                                 
004800     EJECT                                                                
004900 01  FILLER  PIC X(16)  VALUE 'WDATKONV'.                                 
005000                                                                          
005100*01          -COPY WDATAREA                                               
005200     EJECT                                                                
005300 01  FILLER  PIC X(32)  VALUE 'AREA FÖR POST PÅ UTFIL'.                   
005400                                                                          
005500*01  AREA    -COPY W26159   -PRE UT-                                      
005600     EJECT                                                                
005700 01  FILLER                  PIC X(8)  VALUE 'IMS-WS  '.                  
005800     SKIP3                                                                
005900 01  NYCKLAR-TILL-DLI.                                                    
006000     03  W-IDARTNR-X.                                                     
006100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
006200                                                                          
006300 01  STATUS-WS               PIC X(2).                                    
006400     88  SEGMENT-FINNS                 VALUE '  '.                        
006500     88  SEGMENT-SLUT                  VALUE 'GB'.                        
006600     88  SEGMENT-SAKNAS                VALUE 'GE'.                        
006700                                                                          
006800 01  GODK-STATUSKODER.                                                    
006900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
007000     SKIP3                                                                
007100 01  SSA1                        PIC X(64).                               
007200 01  SSA2                        PIC X(64).                               
007300                                                                          
007400     EJECT                                                                
007500*01  -COPY W0003                                                          
007600     EJECT                                                                
007700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
007800                                                                          
007900 01  DLI-IO-AREA             PIC X(900).                                  
008000     SKIP3                                                                
008100*01  SEGM01      -COPY WDK601 -RED DLI-IO-AREA                            
008200     EJECT                                                                
008300*01  SEGM30      -COPY WDK611 -RED DLI-IO-AREA                            
008400     EJECT                                                                
008500 LINKAGE SECTION.                                                         
008600*01  -COPY W0008   -PRE WDK6-                                             
008700    05 FILLER         PIC X.                                              
008800     EJECT                                                                
008900 PROCEDURE DIVISION   USING WDK6-PCB.                                     
009000     ENTRY 'DLITCBL'  USING WDK6-PCB.                                     
009100                                                                          
009200     PERFORM A-INITIERA                                                   
009300                                                                          
009400     PERFORM IMS-GET-WDK6                                                 
009500     PERFORM UNTIL SEGMENT-SLUT                                           
009600*****OR W-ANTAL-POST > 1000                                               
009700        EVALUATE WDK6-SEG-NAME-FB                                         
009800           WHEN  'WDK601  '                                               
009900                 PERFORM B-FLYTTA-WDK601                                  
010000           WHEN  'WDK611  '                                               
010100                 IF CLAG-TISKROT-AUTO NUMERIC  AND                        
010200                    CLAG-TISKROT-AUTO > WS-DAGENS-DATUM                   
010300                   PERFORM D-FLYTTA-WDK611                                
010400                   PERFORM S10-SKAPA-UTPOST                               
010500                 END-IF                                                   
010600        END-EVALUATE                                                      
010700        PERFORM IMS-GET-WDK6                                              
010800     END-PERFORM                                                          
010900     PERFORM Z-FINIT                                                      
011000     MOVE ZERO TO RETURN-CODE                                             
011100     GOBACK                                                               
011200     .                                                                    
011300     EJECT                                                                
011400 A-INITIERA SECTION.                                                      
011500                                                                          
011600     OPEN OUTPUT UTFIL                                                    
011700     MOVE 'W26158'           TO POSTSUM-PROGNAMN                          
011800     PERFORM S02-NOLLSTALL-UTAREA                                         
011900     MOVE 'IDAG'             TO DAT-KDDATFORM                             
012000     CALL WDATKONV USING DAT-KDDATFORM                                    
012100                         DAT-I-TIDATUM                                    
012200                         DAT-O-TIDATUM                                    
012300                         DAT-KDSVAR                                       
012400     IF DAT-KDSVAR-OK                                                     
012500       MOVE DAT-TIAAMMDD     TO WS-DAGENS-DATUM                           
012600     ELSE                                                                 
012700       MOVE ZERO             TO WS-DAGENS-DATUM                           
012800     END-IF                                                               
012900     .                                                                    
013000     EJECT                                                                
013100 B-FLYTTA-WDK601 SECTION.                                                 
013200     SKIP2                                                                
013300     MOVE ART-IDARTNR        TO UT-IDARTNR                                
013400     MOVE ART-KDPRODSL       TO UT-KDPRODSL                               
013500     MOVE ART-IDFKNGRP       TO UT-IDFKNGRP                               
013600     .                                                                    
013700     SKIP3                                                                
013800 D-FLYTTA-WDK611 SECTION.                                                 
013900     SKIP2                                                                
014000     MOVE CLAG-IDANSK        TO  UT-IDANSK                                
014100     MOVE CLAG-TISKROT-AUTO  TO  UT-TISKROT-AUTO                          
014200     .                                                                    
014300     EJECT                                                                
014400 S01-SKRIV-POST SECTION.                                                  
014500     WRITE UT-POST FROM UT-AREA                                           
014600     MOVE 'UTFIL'    TO POSTSUM-FDNAMN                                    
014700     MOVE 'W26158D1' TO POSTSUM-DDNAMN2                                   
014800     MOVE 'POST'     TO POSTSUM-TRANSTYP                                  
014900     CALL POSTSUM USING POSTSUM-PARM                                      
015000     ADD +1          TO W-ANTAL-POST                                      
015100     .                                                                    
015200     EJECT                                                                
015300 S02-NOLLSTALL-UTAREA SECTION.                                            
015400     MOVE +0 TO UT-IDARTNR                                                
015500                UT-KDPRODSL                                               
015600                UT-IDFKNGRP                                               
015700                UT-IDANSK                                                 
015800                UT-TISKROT-AUTO                                           
015900     .                                                                    
016000     EJECT                                                                
016100 S10-SKAPA-UTPOST SECTION.                                                
016200     PERFORM S01-SKRIV-POST                                               
016300     .                                                                    
016400     EJECT                                                                
016500 Z-FINIT SECTION.                                                         
016600                                                                          
016700     CLOSE UTFIL                                                          
016800     MOVE 'S' TO POSTSUM-OPKOD                                            
016900     CALL POSTSUM USING POSTSUM-PARM                                      
017000     .                                                                    
017100     EJECT                                                                
017200 IMS-GET-WDK6 SECTION.                                                    
017300     SKIP2                                                                
017400     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
017500     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA                           
017600     MOVE WDK6-STATUS-CODE   TO STATUS-WS                                 
017700     PERFORM IMS-STATUSKONTROLL                                           
017800     SKIP2                                                                
017900     .                                                                    
018000     SKIP3                                                                
018100 IMS-STATUSKONTROLL SECTION.                                              
018200     SKIP2                                                                
018300     SET STATUS-IX  TO 1                                                  
018400     SEARCH GODK-STATUS   AT END CALL FELLOG                              
018500     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
018600     CONTINUE                                                             
018700     END-SEARCH                                                           
018800     .                                                                    
