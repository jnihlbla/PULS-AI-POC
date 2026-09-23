000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2211400.                                                
000300 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000400 DATE-WRITTEN.   24 OKTOBER 2001.                                         
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄGGER UPP LARM PÅ LARMBAS 2224 (WDR5) VIA 2191                  
001000*                                                                         
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- LARMPOSTER FRÅN W22107                                     
002100     SELECT W22114                     ASSIGN TO W22114D1.                
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W22114                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000                                                                          
003100*01  -COPY W22114        -L.                                              
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W2211400'.            
003800 01  W-W22114-KVPOST-IN          PIC S9(5)   VALUE ZERO COMP-3.           
003900 01  CHKP-VAR.                                                            
004000    03  CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004100    03  CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004200    03  CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004300    03  CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004400    03  CHKP-ANT                 PIC S9(3)   VALUE +0.                    
004500    03  CHKP-MAX                 PIC S9(3)   VALUE +200.                  
004600    03  CHKP-TOT                 PIC S9(7)   VALUE ZERO.                  
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  WS-TIUPPDAT                 PIC S9(7) COMP-3 VALUE ZERO.             
005000 77  WS-TIUPPTID                 PIC S9(9) COMP-3 VALUE ZERO.             
005010*                                                                         
005020*01  -COPY WWDCKONS                                                       
005100     SKIP2                                                                
005200 01  FELTEXT.                                                             
005300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005500                                                                          
005600 77  W22114-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-W22114                       VALUE 'J'.                   
005800     EJECT                                                                
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006400     EJECT                                                                
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL POSTSUM                                          
007300*                                                                         
007400*01  -COPY W0005   -PRE  POSTSUM-                                         
007500     EJECT                                                                
007600 01  IN-AREA-START               PIC X(24)   VALUE                        
007700                                             'IN-AREA-START'.             
007800     SKIP2                                                                
007900                                                                          
008000*01  AREA -COPY W22114       -PRE IN-                                     
008100*                                                                         
008200     EJECT                                                                
008300                                                                          
008400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008500     SKIP3                                                                
008600*    --- STATUS-KOD FRÅN IMS                                              
008700 01  STATUS-WS                   PIC XX.                                  
008800     88  SEGMENT-FINNS                       VALUE '  '.                  
008900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009200     88  IMS-EJ-OK                           VALUE 'XD'.                  
009300     SKIP2                                                                
009400 01  GODK-STATUSKODER.                                                    
009500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009600     SKIP3                                                                
009700 01  SSA1                        PIC X(64).                               
009800 01  SSA2                        PIC X(64).                               
009900     EJECT                                                                
010000*    --- IMS FUNKTIONSKODER                                               
010100*01  -COPY W0003                                                          
010200     EJECT                                                                
010300*    ---  DLI INPUT-OUTPUT AREA                                           
010400                                                                          
010500                                                                          
010600*    --- DATA-AREA FÖR TRANSAKTION                                        
010700 01  FILLER                      PIC X(16) VALUE 'MSG-IO-AREA'.           
010800*01  -COPY WMSGAREA.                                                      
010900     EJECT                                                                
011000     05 FILLER REDEFINES MSG-MID-OUT.                                     
011100*       07  MID -COPY W2I19101   -PRE 2191-                               
011200     EJECT                                                                
011300                                                                          
011400*    --- KOMMUNIKATIONSAREA FÖR DISPATCHER                                
011500 01  FILLER                  PIC X(16) VALUE 'MSG-KOM-WMSGKOM'.           
011600*01  -COPY WMSGKOM                                                        
011700     EJECT                                                                
011800                                                                          
011900                                                                          
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200                                                                          
012300*01  -COPY W0009   -PRE MSG-                                              
012400     EJECT                                                                
012500*01  -COPY W0009   -PRE ALT-                                              
012600     EJECT                                                                
012700*01  -COPY W0009  -PRE KOMA-                                              
012800     EJECT                                                                
012900 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB.                      
013000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB.                      
013100 MAIN SECTION.                                                            
013200                                                                          
013300     SKIP2                                                                
013400     PERFORM A-INIT                                                       
013500     PERFORM S01-LAES-W22114                                              
013600     PERFORM UNTIL END-OF-W22114                                          
013700       IF CHKP-ANT > CHKP-MAX                                             
013800         PERFORM X-TAG-CHECKPOINT                                         
013900       END-IF                                                             
014000                                                                          
014100       PERFORM B-SKAPA-KOMA-TRANS                                         
014200       PERFORM C-SKICKA-TRANS-TILL-KOMA                                   
014300                                                                          
014400       ADD +1 TO CHKP-ANT                                                 
014500                                                                          
014600       PERFORM S01-LAES-W22114                                            
014700     END-PERFORM                                                          
014800                                                                          
014900                                                                          
015000     PERFORM Z-FINIT                                                      
015100                                                                          
015200     MOVE ZERO TO RETURN-CODE                                             
015300     GOBACK                                                               
015400     .                                                                    
015500     EJECT                                                                
015600 A-INIT SECTION.                                                          
015700     SKIP2                                                                
015800                                                                          
015900     OPEN INPUT W22114                                                    
016000                                                                          
016100     PERFORM IMS-RESTART                                                  
016200                                                                          
016300                                                                          
016400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016500     PERFORM AA-INITIERA-WMSGKOM-AREAN                                    
016600     .                                                                    
016700     EJECT                                                                
016800 AA-INITIERA-WMSGKOM-AREAN SECTION.                                       
016900                                                                          
017000     ACCEPT WS-TIUPPDAT FROM DATE                                         
017100     ACCEPT WS-TIUPPTID FROM TIME                                         
017200     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
017300     MOVE +54                    TO MSG-KOM-KVLL                          
017400     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
017500     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
017600     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
017700     MOVE 'W2I19101'             TO MSG-KOM-IDCPYTXT                      
017800     MOVE 'ANSKLARM'             TO MSG-KOM-IDSNDNOD                      
017900     MOVE 'W2211400'             TO MSG-KOM-IDSNDJOB                      
018000     MOVE WS-TIUPPDAT            TO MSG-KOM-TIREGDAT                      
018100     MOVE WS-TIUPPTID            TO MSG-KOM-TIKLOCK                       
018200     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
018300     .                                                                    
018400     EJECT                                                                
018500 B-SKAPA-KOMA-TRANS SECTION.                                              
018600     SKIP2                                                                
018700     MOVE LENGTH OF 2191-MID-W2I19101                                     
018800                                  TO   MSG-KVLL                           
018900     ADD  +17                     TO   MSG-KVLL                           
019000     MOVE   LOW-VALUE             TO   MSG-KDZ1                           
019100                                       MSG-KDZ2                           
019200     MOVE   'W2T191X'             TO   MSG-KDTRANS-1                      
019300     MOVE   '2191'                TO   MSG-IDTRANS-1                      
019400     MOVE   '1'                   TO   MSG-KDMFSFOR-1                     
019500     SKIP2                                                                
019600*    --- FLYTTA MIDDEN                                                    
019700     MOVE IN-IDARTNR             TO 2191-MID-IDARTNR                      
019800     MOVE IN-IDANSK              TO 2191-MID-IDANSK                       
019900     MOVE IN-KDLARM              TO 2191-MID-KDLARM                       
019910     MOVE IN-IDLEVNR             TO 2191-MID-IDLEVNR                      
020000     MOVE ZERO                   TO 2191-MID-KDCLAGER                     
020100                                    2191-MID-TISENBEK-DAG                 
020200                                    2191-MID-TISENBEK-KL                  
020300                                    2191-MID-IDDISTR                      
020400                                    2191-MID-IDKUNDNR                     
020500     MOVE SPACE                  TO 2191-MID-IDKR                         
020600                                    2191-MID-IDKUNDRF                     
020700     MOVE 'J'                    TO 2191-MID-FLNYLARM                     
020800     MOVE WC-CDC-SE              TO 2191-MID-IDDC                         
020900     .                                                                    
021000     EJECT                                                                
021100 C-SKICKA-TRANS-TILL-KOMA SECTION.                                        
021200     SKIP2                                                                
021300     CALL W006KOM USING MSG-PCB                                           
021400                        ALT-PCB                                           
021500                        KOMA-PCB                                          
021600                        MSG-KOM-WMSGKOM                                   
021700                        MSG-IO-AREA                                       
021800                                                                          
021900     MOVE 'W22114'      TO POSTSUM-FDNAMN                                 
022000     MOVE 'W2T191X'     TO POSTSUM-DDNAMN2                                
022100     MOVE 'TRAN'        TO POSTSUM-TRANSTYP                               
022200     CALL POSTSUM USING POSTSUM-PARM                                      
022300                                                                          
022400     .                                                                    
022500     EJECT                                                                
022600 Z-FINIT SECTION.                                                         
022700                                                                          
022800                                                                          
022900     CLOSE W22114                                                         
023000     SKIP2                                                                
023100     MOVE 'S' TO POSTSUM-OPKOD                                            
023200     CALL POSTSUM USING POSTSUM-PARM                                      
023300     .                                                                    
023400     EJECT                                                                
023500 S01-LAES-W22114  SECTION.                                                
023600     SKIP2                                                                
023700     READ W22114 INTO IN-AREA                                             
023800     AT END                                                               
023900        SET END-OF-W22114 TO TRUE                                         
024000                                                                          
024100     NOT AT END                                                           
024200        MOVE 'W22114'   TO POSTSUM-FDNAMN                                 
024300        MOVE 'W22114D1' TO POSTSUM-DDNAMN2                                
024400        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
024500        CALL POSTSUM USING POSTSUM-PARM                                   
024600                                                                          
024700        ADD 1 TO W-W22114-KVPOST-IN                                       
024800     END-READ                                                             
024900     .                                                                    
025000     EJECT                                                                
025100 X-TAG-CHECKPOINT   SECTION.                                              
025200                                                                          
025300* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
025400* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
025500     PERFORM IMS-CHECKPOINT                                               
025600     MOVE ZERO TO CHKP-ANT                                                
025700* --- LÄS OM DATABAS OM DET BEHÖVS                                        
025800     .                                                                    
025900     EJECT                                                                
026000* --- IMS SEKTIONER ---                                                   
026100 IMS-RESTART SECTION.                                                     
026200     SKIP2                                                                
026300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026400     MOVE '  ' TO GODK-STATUSKODER                                        
026500     CALL CBLTDLI USING XRST MSG-PCB                                      
026600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026700                        CHKP-AREA-LENGTH CHKP-AREA                        
026800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026900     PERFORM IMS-STATUSKONTROLL                                           
027000     .                                                                    
027100     EJECT                                                                
027200 IMS-CHECKPOINT SECTION.                                                  
027300     SKIP2                                                                
027400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027500     MOVE '  XD' TO GODK-STATUSKODER                                      
027600     CALL CBLTDLI USING CHKP MSG-PCB                                      
027700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027800                        CHKP-AREA-LENGTH CHKP-AREA                        
027900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028000     PERFORM IMS-STATUSKONTROLL                                           
028100                                                                          
028200     IF IMS-EJ-OK                                                         
028300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
028400       DISPLAY FELTEXT                                                    
028500       CALL FELLOG                                                        
028600     END-IF                                                               
028700     .                                                                    
028800     EJECT                                                                
028900                                                                          
029000 IMS-STATUSKONTROLL SECTION.                                              
029100     SKIP2                                                                
029200     SET STATUS-IX TO 1                                                   
029300     SEARCH GODK-STATUS                                                   
029400       AT END                                                             
029500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
029600           DELIMITED BY SIZE INTO FELTEXT                                 
029700         DISPLAY FELTEXT                                                  
029800         CALL FELLOG                                                      
029900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030000         CONTINUE                                                         
030100     END-SEARCH                                                           
030200     .                                                                    
