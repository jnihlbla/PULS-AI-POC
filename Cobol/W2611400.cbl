000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2611400.                                                
000300 AUTHOR.         IDK, GÖTEBORG.                                           
000400 DATE-WRITTEN.   APRIL 1978.                                              
000500     REMARKS.                                                             
000600     EJECT                                                                
000700 ENVIRONMENT DIVISION.                                                    
000800 INPUT-OUTPUT SECTION.                                                    
000900 FILE-CONTROL.                                                            
001000     SKIP2                                                                
001100*--------------------------------------- UTFIL                            
001200     SKIP1                                                                
001300         SELECT UTFIL          ASSIGN TO UT-S-W26114D1.                   
001400     EJECT                                                                
001500 DATA DIVISION.                                                           
001600 FILE SECTION.                                                            
001700 FD  UTFIL                                                                
001800     RECORDING F                                                          
001900     BLOCK 0                                                              
002000     LABEL RECORD STANDARD.                                               
002100     SKIP1                                                                
002200*01  POST    -COPY W261226       -PRE UT-    -L.                          
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500     SKIP3                                                                
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 01  KONSTANTER.                                                          
002900  03 JA                      PIC X       VALUE 'J'.                       
003000  03 NEJ                     PIC X       VALUE 'N'.                       
003100     SKIP3                                                                
003200 01  FILLER                  PIC X(16)    VALUE 'SWITCHAR' .              
003300                                                                          
003400 01  SW.                                                                  
003500     03  SW-FIRST-ARTIKEL    PIC X       VALUE 'J'.                       
003600         88 FIRST-ARTIKEL                VALUE 'J'.                       
003700                                                                          
003800*01  -COPY WWPRODSL                                                       
003900                                                                          
004000 01  DYNAMISKA-SUBPROGRAM.                                                
004100  03 POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.                 
004200  03 CBLTDLI                PIC X(8)    VALUE  'CBLTDLI'.                 
004300  03 FELLOG                  PIC X(8)    VALUE 'FELLOG '.                 
004400     EJECT                                                                
004500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
004600     SKIP3                                                                
004700 01  NYCKLAR-TILL-DLI.                                                    
004800     03  W-IDARTNR-X.                                                     
004900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
005000     03  W-KDSEGKEY-X.                                                    
005100         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
005200     03  W-IDPROENH-X.                                                    
005300         05  W-IDPROENH          PIC S9(8)   VALUE ZERO COMP-3.           
005400 01  SSA1                        PIC X(64).                               
005500 01  SSA2                        PIC X(64).                               
005600******************************************************************        
005700*    AREA FÖR SPARANDE AV SB-LÄST WDK6                                    
005800******************************************************************        
005900                                                                          
006000 01  FILLER                  PIC X(16)  VALUE 'WDK6-AREA      '.          
006100                                                                          
006200*01  WDK601-AREA -COPY WDK601                                             
006300     EJECT                                                                
006400                                                                          
006500*01  WDK611-AREA -COPY WDK611                                             
006600     EJECT                                                                
006700*--------------------------------------- PARAMETRAR TILL POSTSUM          
006800     SKIP1                                                                
006900*01          -COPY W0005         -PRE POSTSUM-                            
007000     EJECT                                                                
007100*--------------------------------------- AREA FÖR POST PÅ UTFIL           
007200     SKIP1                                                                
007300*01  AREA    -COPY W261226       -PRE UT-                                 
007400     EJECT                                                                
007500 01  IMS-WS.                                                              
007600     03   FILLER     PIC X(8)  VALUE 'IMS-WS'.                            
007700*----------------------------------STATUSKODER FRÅN IMS                   
007800     03  STATUS-WS   PIC XX.                                              
007900         88  SEGMENT-FINNS       VALUE '  '.                              
008000         88  SEGMENT-SLUT        VALUE 'GB'.                              
008100                                                                          
008200     03  GODK-STATUSKODER.                                                
008300      05  GODK-STATUS  OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
008400                                                                          
008500     EJECT                                                                
008600*----------------------------------IMS-CALL FUNKTIONER                    
008700*01              -COPY W0003                                              
008800 01  DLI-IO-AREA             PIC X(1000).                                 
008900     EJECT                                                                
009000 LINKAGE SECTION.                                                         
009100*01    -COPY W0008         -PRE WDK6-                                     
009200          05  FILLER      PIC   XX.                                       
009300     EJECT                                                                
009400 PROCEDURE DIVISION USING WDK6-PCB.                                       
009500     ENTRY 'CBLTDLI'  USING WDK6-PCB.                                     
009600     PERFORM A-INITIERA                                                   
009700     PERFORM IMS-GET-WDK6                                                 
009800     PERFORM UNTIL SEGMENT-SLUT                                           
009900         EVALUATE WDK6-SEG-NAME-FB                                        
010000             WHEN 'WDK601  '                                              
010100                IF FIRST-ARTIKEL                                          
010200                   MOVE NEJ TO SW-FIRST-ARTIKEL                           
010300                ELSE                                                      
010400                   PERFORM B-BEHANDLA-ARTIKEL                             
010500                END-IF                                                    
010600                MOVE DLI-IO-AREA TO WDK601-AREA                           
010700             WHEN 'WDK611  '                                              
010800                MOVE DLI-IO-AREA TO WDK611-AREA                           
010900         END-EVALUATE                                                     
011000         PERFORM IMS-GET-WDK6                                             
011100     END-PERFORM                                                          
011200     PERFORM B-BEHANDLA-ARTIKEL                                           
011300     PERFORM Z-FINIT                                                      
011400     MOVE ZERO TO RETURN-CODE                                             
011500     GOBACK                                                               
011600     .                                                                    
011700     EJECT                                                                
011800******************************************************************        
011900*                                                                *        
012000*        INITIERA.                                               *        
012100*                                                                *        
012200******************************************************************        
012300     SKIP2                                                                
012400 A-INITIERA SECTION.                                                      
012500     SKIP1                                                                
012600     OPEN OUTPUT UTFIL                                                    
012700     MOVE '226'              TO UT-IDPTYP                                 
012800     MOVE 'W26114'           TO POSTSUM-PROGNAMN                          
012900     MOVE ZERO TO UT-IDARTNR                                              
013000     .                                                                    
013100     EJECT                                                                
013200******************************************************************        
013300*                                                                *        
013400*    SKAPA OCH SKRIV UTPOST OM KDERS-UTG = 0                   *          
013500*                                                                *        
013600******************************************************************        
013700     SKIP2                                                                
013800 B-BEHANDLA-ARTIKEL SECTION.                                              
013900     SKIP1                                                                
014000     MOVE ART-KDPRODSL       TO TEST-KDPRODSL                             
014100     IF  ART-KDERS-UTG = ZERO AND KDPRODSL-VOLVO-BIMA                     
014200         MOVE ART-IDARTNR    TO UT-IDARTNR                                
014300         MOVE ART-IDFKNGRP   TO UT-IDFKNGRP                               
014400         MOVE CLAG-VLARTNTO  TO UT-VLARTNTO                               
014500         PERFORM S01-SKRIV-UTFIL                                          
014600     END-IF                                                               
014700     .                                                                    
014800     EJECT                                                                
014900******************************************************************        
015000*                                                                *        
015100*        SKRIV EN POST PÅ UTFIL.                                 *        
015200*                                                                *        
015300******************************************************************        
015400     SKIP2                                                                
015500 S01-SKRIV-UTFIL SECTION.                                                 
015600     SKIP1                                                                
015700     WRITE UT-POST           FROM UT-AREA                                 
015800     SKIP1                                                                
015900     MOVE 'W26115'           TO POSTSUM-FDNAMN                            
016000     MOVE 'W26114D1'         TO POSTSUM-DDNAMN2                           
016100     MOVE UT-IDPTYP          TO POSTSUM-TRANSTYP                          
016200     CALL POSTSUM USING POSTSUM-PARM                                      
016300     .                                                                    
016400 IMS-GET-WDK6 SECTION.                                                    
016500     SKIP3                                                                
016600     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
016700     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA                           
016800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
016900     PERFORM IMS-STATUSKONTROLL                                           
017000     .                                                                    
017100 IMS-STATUSKONTROLL SECTION.                                              
017200     SKIP3                                                                
017300     SET STATUS-IX TO 1                                                   
017400     SEARCH GODK-STATUS AT END CALL FELLOG                                
017500       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
017600       CONTINUE                                                           
017700     END-SEARCH                                                           
017800     .                                                                    
017900 Z-FINIT SECTION.                                                         
018000     MOVE 'S'             TO POSTSUM-OPKOD                                
018100     CALL POSTSUM USING POSTSUM-PARM                                      
018200     CLOSE UTFIL                                                          
018300     EJECT                                                                
018400     .                                                                    
