000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6126600.                                                
000300 AUTHOR.         MONICA BERGSTRÖM.                                        
000400 DATE-WRITTEN.   97/06/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KOMMENTAR                                                        
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- FÖRVÄNTADE LEVERANSER                                      
002500     SELECT W61262                     ASSIGN TO W61266D1.                
002600     SKIP2                                                                
002700*          --- KOMPLETTERADE TRANSAR                                      
002800     SELECT W61266                     ASSIGN TO W61266D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W61262                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W61262      -L.                                                
003900     SKIP3                                                                
004000 FD  W61266                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY W61262 -PRE  UT-  -L.                                     
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 01  WS-FLNYART                  PIC X       VALUE 'N'.                   
005100                                                                          
005200 77  IDPGM                       PIC X(8)    VALUE 'W6126600'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500                                                                          
005600 77  W61262-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-W61262                       VALUE 'J'.                   
005800     EJECT                                                                
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006400     EJECT                                                                
006500*01  -COPY WWDC99                                                         
006600     EJECT                                                                
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800*                                                                         
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     SKIP2                                                                
007400*    --- PARAMETRAR TILL ABEND                                            
007500                                                                          
007600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007900     SKIP2                                                                
008000 01  FELTEXT.                                                             
008100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL POSTSUM                                          
008500*                                                                         
008600*01  -COPY W0005   -PRE  POSTSUM-                                         
008700     EJECT                                                                
008800 01  IN-AREA-START               PIC X(24)   VALUE                        
008900                                 'IN-AREA-START  '.                       
009000     SKIP2                                                                
009100                                                                          
009200*01  AREA -COPY W61262     -PRE IN-                                       
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009500     SKIP3                                                                
009600 01  NYCKLAR-TILL-DLI.                                                    
009700     03  W-IDSKYLT-X.                                                     
009800         05  W-IDSKYLT           PIC X(3)    VALUE 'USA'.                 
009900     03  W-IDARTNR-X.                                                     
010000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010100     03  W-IDDC-X.                                                        
010200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010300     SKIP2                                                                
010400*    --- STATUS-KOD FRÅN IMS                                              
010500 01  STATUS-WS                   PIC XX.                                  
010600     88  SEGMENT-FINNS                       VALUE '  '.                  
010700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010900     SKIP2                                                                
011000 01  GODK-STATUSKODER.                                                    
011100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011200     SKIP3                                                                
011300 01  SSA1                        PIC X(64).                               
011400 01  SSA2                        PIC X(64).                               
011500     EJECT                                                                
011600*    --- IMS FUNKTIONSKODER                                               
011700*01  -COPY W0003                                                          
011800     EJECT                                                                
011900*    ---  DLI INPUT-OUTPUT AREA                                           
012000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS11'.                    
012100 01  DLI-IO-WLARTS11.                                                     
012200*    03  -COPY WDK711  -PRE ARTS-                                         
012300     EJECT                                                                
012400 LINKAGE SECTION.                                                         
012500                                                                          
012600*01  -COPY W0008  -PRE ARTS-                                              
012700     05  FILLER                  PIC X.                                   
012800     EJECT                                                                
012900 PROCEDURE DIVISION  USING ARTS-PCB.                                      
013000 MAIN SECTION.                                                            
013100     ENTRY 'DLITCBL' USING ARTS-PCB.                                      
013200                                                                          
013300     PERFORM A-INIT                                                       
013400                                                                          
013500     PERFORM S01-LAES-W61262                                              
013600     PERFORM UNTIL END-OF-W61262                                          
013700       MOVE NEJ        TO IN-FLNYART                                      
013800       MOVE IN-IDARTNR TO W-IDARTNR                                       
013900       MOVE IN-IDDC    TO W-IDDC                                          
014000                          WS-IDDC                                         
014100                                                                          
014200       IF CDC-SE                                                          
014300         CONTINUE                                                         
014400       ELSE                                                               
014500         PERFORM IMS-GET-ARTS-SLAG                                        
014600         PERFORM S30-NYART-KONTROLLERING                                  
014700       END-IF                                                             
014800       PERFORM S11-SKRIV-W61266                                           
014900       PERFORM S01-LAES-W61262                                            
015000     END-PERFORM                                                          
015100                                                                          
015200     PERFORM Z-FINIT                                                      
015300                                                                          
015400     MOVE ZERO TO RETURN-CODE                                             
015500     GOBACK                                                               
015600     .                                                                    
015700     EJECT                                                                
015800 A-INIT SECTION.                                                          
015900                                                                          
016000     OPEN INPUT  W61262                                                   
016100                                                                          
016200     OPEN OUTPUT W61266                                                   
016300                                                                          
016400     .                                                                    
016500     EJECT                                                                
016600 Z-FINIT SECTION.                                                         
016700     CLOSE W61262                                                         
016800           W61266                                                         
016900     SKIP2                                                                
017000     MOVE 'S' TO POSTSUM-OPKOD                                            
017100     CALL POSTSUM USING POSTSUM-PARM                                      
017200     .                                                                    
017300     EJECT                                                                
017400 S01-LAES-W61262 SECTION.                                                 
017500     READ W61262 INTO IN-AREA                                             
017600     AT END                                                               
017700        MOVE HIGH-VALUE TO IN-AREA                                        
017800        SET END-OF-W61262 TO TRUE                                         
017900                                                                          
018000     NOT AT END                                                           
018100        MOVE 'W61262' TO POSTSUM-FDNAMN                                   
018200        MOVE 'W61266D1' TO POSTSUM-DDNAMN2                                
018300        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
018400        CALL POSTSUM USING POSTSUM-PARM                                   
018500     END-READ                                                             
018600     .                                                                    
018700     EJECT                                                                
018800 S11-SKRIV-W61266 SECTION.                                                
018900                                                                          
019000     WRITE   UT-POST FROM IN-AREA                                         
019100                                                                          
019200     MOVE 'UT'     TO POSTSUM-TRANSTYP                                    
019300     MOVE 'W61266' TO POSTSUM-FDNAMN                                      
019400     MOVE 'W61266D2' TO POSTSUM-DDNAMN2                                   
019500     CALL POSTSUM USING POSTSUM-PARM                                      
019600     .                                                                    
019700     EJECT                                                                
019800 S30-NYART-KONTROLLERING SECTION.                                         
019900                                                                          
020000     SKIP2                                                                
020100     PERFORM IMS-GET-ARTS-SLAG                                            
020200*    KONTROLL OM ARTIKELN ÄR PLATSSATT I NDC-LAGERET                      
020300     IF   ARTS-SLAG-ADLAGOMR  = 0     AND                                 
020400          ARTS-SLAG-ADGANG    = 0     AND                                 
020500          ARTS-SLAG-ADPLATS   = 0                                         
020600       MOVE JA   TO  IN-FLNYART                                           
020700     ELSE                                                                 
020800       MOVE NEJ  TO  IN-FLNYART                                           
020900     END-IF                                                               
021000     .                                                                    
021100     EJECT                                                                
021200* --- IMS SEKTIONER ---                                                   
021300 IMS-GET-ARTS-SLAG SECTION.                                               
021400                                                                          
021500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
021600          DELIMITED BY SIZE INTO SSA1                                     
021700     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
021800          DELIMITED BY SIZE INTO SSA2                                     
021900     MOVE SPACE TO GODK-STATUSKODER                                       
022000     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2             
022100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
022200     PERFORM IMS-STATUSKONTROLL                                           
022300                                                                          
022400     .                                                                    
022500     EJECT                                                                
022600 IMS-STATUSKONTROLL SECTION.                                              
022700                                                                          
022800     SET STATUS-IX TO 1                                                   
022900     SEARCH GODK-STATUS                                                   
023000       AT END                                                             
023100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023200           DELIMITED BY SIZE INTO FELTEXT                                 
023300         DISPLAY FELTEXT                                                  
023400         CALL FELLOG                                                      
023500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023600         CONTINUE                                                         
023700     END-SEARCH                                                           
023800     .                                                                    
