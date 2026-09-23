000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6118600.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/08/26.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        RENSAR BORT SEGMENT PÅ LASTBÄRAWREGISTRET (W6LASA) OM            
001100*        TIREGDAT ÄR ÄLDRE ÄN 4 ARBETSDAGAR                               
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR W6LASA (W6G2)                              
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  FEL FRÅN WORKDAY                                        
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200     SKIP2                                                                
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(8)    VALUE 'W6118600'.            
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800     SKIP2                                                                
003900 01  FELTEXT.                                                             
004000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004200     EJECT                                                                
004300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004400 01  FILLER REDEFINES DAGENS-DATUM.                                       
004500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004800     EJECT                                                                
004900*      --- VALID IDDC CODES                                               
005000*                                                                         
005100*01    -COPY WWDCKONS                                                     
005200       EJECT                                                              
005300 01  DYNAMISKA-SUBPROGRAM.                                                
005400*                                                                         
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
005900     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
006000     EJECT                                                                
006100*    --- PARAMETRAR TILL ABEND                                            
006200                                                                          
006300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006500*    --- PARAMETRAR TILL DATKORT                                          
006600*                                                                         
006700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61186'.              
006800     SKIP2                                                                
006900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007000     SKIP2                                                                
007100*01  -COPY WDATKORT                                                       
007200     EJECT                                                                
007300*01  -COPY WORKAREA                                                       
007400*                                                                         
007500     EJECT                                                                
007600 01  WS-IDDC-TABL.                                                        
007700     03  WS-IDDC                 OCCURS 6 TIMES                           
007800                                 PIC XX.                                  
007900     SKIP2                                                                
008000 01  WS-INDX                     PIC 9       VALUE 0.                     
008100 01  WS-MAX-ARRAY                PIC 9       VALUE 6.                     
008200*                                                                         
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008500     SKIP3                                                                
008600 01  NYCKLAR-TILL-DLI.                                                    
008700     03  W-W6GXKEY-6107-X.                                                
008800         05  FILLER              PIC X(4)    VALUE '6107'.                
008900         05  W-6107-IDDC         PIC X(2)    VALUE SPACE.                 
009000         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
009100     SKIP2                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FINNS                       VALUE '  '.                  
009500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009800     88  IMS-EJ-OK                           VALUE 'XD'.                  
009900     SKIP2                                                                
010000 01  GODK-STATUSKODER.                                                    
010100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010200     SKIP3                                                                
010300 01  SSA1                        PIC X(64).                               
010400 01  SSA2                        PIC X(64).                               
010500     EJECT                                                                
010600*    --- IMS FUNKTIONSKODER                                               
010700*01  -COPY W0003                                                          
010800     EJECT                                                                
010900*    ---  DLI INPUT-OUTPUT AREA                                           
011000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011100     SKIP3                                                                
011200 01  DLI-IO-AREA.                                                         
011300     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
011400     SKIP3                                                                
011500     03  W6LASA11 REDEFINES IO-AREA.                                      
011600*        05  -COPY W6GX6108  -PRE LASA-                                   
011700     EJECT                                                                
011800 LINKAGE SECTION.                                                         
011900                                                                          
012000*01  -COPY W0009   -PRE MSG-                                              
012100     EJECT                                                                
012200*01  -COPY W0008  -PRE LASA-                                              
012300     05  FILLER                  PIC X.                                   
012400     EJECT                                                                
012500 PROCEDURE DIVISION  USING MSG-PCB LASA-PCB.                              
012600     ENTRY 'DLITCBL' USING MSG-PCB LASA-PCB.                              
012700                                                                          
012800     SKIP2                                                                
012900     PERFORM A-INIT                                                       
013000                                                                          
013100     PERFORM UNTIL WS-INDX = WS-MAX-ARRAY                                 
013200         MOVE WS-IDDC(WS-INDX) TO W-6107-IDDC                             
013300                                                                          
013400         PERFORM IMS-GHU-LASA-LASA11                                      
013500         PERFORM UNTIL SEGMENT-SAKNAS                                     
013600             PERFORM B-TAG-EV-BORT-LASA11                                 
013700             PERFORM IMS-GHNP-LASA-LASA11                                 
013800         END-PERFORM                                                      
013900                                                                          
014000         ADD 1                 TO WS-INDX                                 
014100     END-PERFORM                                                          
014200                                                                          
014300     MOVE ZERO TO RETURN-CODE                                             
014400     GOBACK                                                               
014500     .                                                                    
014600     EJECT                                                                
014700 A-INIT SECTION.                                                          
014800                                                                          
014900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
015000     MOVE D-AAR                TO DAGENS-DATUM-AAR                        
015100     MOVE D-MAANAD             TO DAGENS-DATUM-MAANAD                     
015200     MOVE D-DAG                TO DAGENS-DATUM-DAG                        
015300     MOVE WC-CDC-SE            TO WS-IDDC(1)                              
015400     MOVE WC-CDC-TR            TO WS-IDDC(2)                              
015500     MOVE WC-NDC-CN-71         TO WS-IDDC(3)                              
015600     MOVE WC-NDC-CN-72         TO WS-IDDC(4)                              
015700     MOVE WC-NDC-CN-73         TO WS-IDDC(5)                              
015800     MOVE WC-NDC-CN-74         TO WS-IDDC(6)                              
015900     MOVE 1                    TO WS-INDX                                 
016000     .                                                                    
016100     EJECT                                                                
016200 B-TAG-EV-BORT-LASA11 SECTION.                                            
016300                                                                          
016400     MOVE +001                 TO WORK-KDCALL                             
016500     IF W-6107-IDDC = '12'                                                
016600       MOVE '91'               TO WORK-IDDC                               
016700     ELSE                                                                 
016800       MOVE W-6107-IDDC        TO WORK-IDDC                               
016900     END-IF                                                               
017000     MOVE LASA-6108-TIREGDAT   TO WORK-TIAAMMDD-FOM                       
017100     MOVE DAGENS-DATUM         TO WORK-TIAAMMDD-TOM                       
017200                                                                          
017300     CALL WORKDAY USING WORK-KDCALL                                       
017400               WORK-DATE-AREA WORK-KDSVAR                                 
017500                                                                          
017600     IF WORK-KDSVAR-OK                                                    
017700         IF WORK-KVWORKD       > 4                                        
017800             PERFORM IMS-DLET-LASA-LASA11                                 
017900         END-IF                                                           
018000     ELSE                                                                 
018100                                                                          
018200         MOVE 'FEL UR WORKDAY'          TO FELTEXT-STR(1:14)              
018300         MOVE LASA-6108-IDLBBET         TO FELTEXT-STR(16:12)             
018400         MOVE LASA-6108-ADINLOMR-LPL    TO FELTEXT-STR(29:4)              
018500         MOVE WORK-TIAAMMDD-FOM         TO FELTEXT-STR(34:6)              
018600         MOVE WORK-TIAAMMDD-TOM         TO FELTEXT-STR(41:6)              
018700         MOVE WORK-IDDC                 TO FELTEXT-STR(48:2)              
018800         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
018900     END-IF                                                               
019000     .                                                                    
019100     EJECT                                                                
019200 IMS-GHU-LASA-LASA11 SECTION.                                             
019300     STRING 'W6LASA01*P(W6GXKEY  =' W-W6GXKEY-6107-X ')'                  
019400          DELIMITED BY SIZE INTO SSA1                                     
019500     MOVE 'W6LASA11'           TO SSA2                                    
019600     MOVE '  GE' TO GODK-STATUSKODER                                      
019700     CALL CBLTDLI USING GHU LASA-PCB DLI-IO-AREA SSA1 SSA2                
019800     MOVE LASA-STATUS-CODE TO STATUS-WS                                   
019900     PERFORM IMS-STATUSKONTROLL                                           
020000     .                                                                    
020100     SKIP3                                                                
020200 IMS-GHNP-LASA-LASA11 SECTION.                                            
020300     MOVE 'W6LASA11'           TO SSA1                                    
020400     MOVE '  GE' TO GODK-STATUSKODER                                      
020500     CALL CBLTDLI USING GHNP LASA-PCB DLI-IO-AREA SSA1                    
020600     MOVE LASA-STATUS-CODE TO STATUS-WS                                   
020700     PERFORM IMS-STATUSKONTROLL                                           
020800     .                                                                    
020900     SKIP3                                                                
021000 IMS-DLET-LASA-LASA11  SECTION.                                           
021100                                                                          
021200     MOVE '  ' TO GODK-STATUSKODER                                        
021300     CALL CBLTDLI USING DLET LASA-PCB DLI-IO-AREA                         
021400     MOVE LASA-STATUS-CODE TO STATUS-WS                                   
021500     PERFORM IMS-STATUSKONTROLL                                           
021600     .                                                                    
021700     SKIP3                                                                
021800 IMS-STATUSKONTROLL SECTION.                                              
021900     SKIP2                                                                
022000     SET STATUS-IX TO 1                                                   
022100     SEARCH GODK-STATUS                                                   
022200       AT END                                                             
022300         MOVE 'FEL VID DL1 ANROP' TO FELTEXT-STR                          
022400         DISPLAY FELTEXT                                                  
022500         CALL FELLOG                                                      
022600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022700         CONTINUE                                                         
022800     END-SEARCH                                                           
022900     .                                                                    
