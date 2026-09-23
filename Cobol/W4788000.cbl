000100 ID DIVISION.                                                             
000200*                                                                         
000300 PROGRAM-ID.             W4788000.                                        
000400*AUTHOR.                 ERIK KÅREBY.                                     
000500*DATE-WRITTEN.           DEC 1981.                                        
000600                                                                          
000700*REMARKS.                                                                 
000800                                                                          
000900*    FUNKTION:                                                            
001000*            PROGRAMMET SKAPAR EN FIL FÖR EJ FAKTURERADE ORDER.           
001100                                                                          
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400     SKIP2                                                                
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800                                                                          
001900*- - - - - - - - - - - - - - UTFIL:                                       
002000                                                                          
002100     SELECT  EFR                      ASSIGN  UT-S-W47880D1.              
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400                                                                          
002500 FILE SECTION.                                                            
002600                                                                          
002700 FD  EFR                                                                  
002800     RECORDING   F                                                        
002900     BLOCK CONTAINS 0.                                                    
003000                                                                          
003100*01  POST     -COPY W4788002 -PRE EFR-  -L.                               
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
003700 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4788000'.            
003800     SKIP2                                                                
003900*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300     SKIP2                                                                
004400*- - - - - - - - - - - - - -  UTAREA                                      
004500                                                                          
004600*01  AREA    -COPY W4788002     -PRE EFR-.                                
004700     EJECT                                                                
004800 01  DYNAMISKA-SUBPROGRAM.                                                
004900   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
005000   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
005100   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005200   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005300     SKIP3                                                                
005400 01  HELP-FAELT.                                                          
005500   03  ANTAL                     PIC S9(7)   COMP-3.                      
005600   03  WS-IDFAKT                 PIC  9(7).                               
005700     EJECT                                                                
005800*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
005900                                                                          
006000 01  RETURKODER.                                                          
006100   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16  COMP SYNC.        
006200     SKIP2                                                                
006300*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
006400                                                                          
006500*01  -COPY W0005       -PRE POSTSUM-.                                     
006600     EJECT                                                                
006700*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
006800*                                                                         
006900 01  IMS-WS.                                                              
007000   03  FILLER                    PIC X(8)    VALUE 'IMS-WS  '.            
007100     SKIP3                                                                
007200 01  W-NYCKLAR.                                                           
007300   03  W-IDPURAD-X.                                                       
007400       05 W-IDPURAD             PIC S9(5)   COMP-3.                       
007500     SKIP2                                                                
007600   03  W-IDPRODNR-X.                                                      
007700       05 W-IDPRODNR            PIC S9(7)   COMP-3.                       
007800     SKIP2                                                                
007900   03  W-IDKOLLI-X.                                                       
008000       05 W-IDKOLLI             PIC S9(5)   COMP-3.                       
008100     SKIP2                                                                
008200*                            *** STATUSKOD FRÅN IMS                       
008300   03  STATUS-WS                 PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008600     88  BASEN-SLUT                          VALUE 'GB'.                  
008700     SKIP3                                                                
008800   03  GODK-STATUSKODER.                                                  
008900     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100   03  SSA1                      PIC X(64).                               
009200   03  SSA2                      PIC X(64).                               
009300     EJECT                                                                
009400*01  -COPY W0003                                                          
009500     EJECT                                                                
009600 01  DLI-IO-E401.                                                         
009700*  03  -COPY WDE401                                                       
009800     EJECT                                                                
009900 01  DLI-IO-E411.                                                         
010000*  03  -COPY WDE411                                                       
010100     EJECT                                                                
010200 01  DLI-IO-E421.                                                         
010300*  03  -COPY WDE421                                                       
010400     EJECT                                                                
010500 01  DLI-IO-E611.                                                         
010600*  04  -COPY WDE611                                                       
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900     SKIP3                                                                
011000*01  -COPY W0008 -PRE WDE4-.                                              
011100     05  FILLER              PIC X.                                       
011200                                                                          
011300*01  -COPY W0008 -PRE WDE6-.                                              
011400     05  FILLER              PIC X.                                       
011500     EJECT                                                                
011600 PROCEDURE DIVISION USING WDE4-PCB WDE6-PCB.                              
011700     ENTRY 'DLITCBL' USING WDE4-PCB WDE6-PCB.                             
011800     SKIP3                                                                
011900     PERFORM A-INIT                                                       
012000                                                                          
012100     PERFORM IMS-GN-WDE401                                                
012200                                                                          
012300     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
012400       PERFORM IMS-GNP-WDE411                                             
012500                                                                          
012600       PERFORM UNTIL SEGMENT-SAKNAS                                       
012700         IF ORAD-FLDIRLEV = NEJ                                           
012800           MOVE KORD-IDDC       TO EFR-IDDC                               
012900           MOVE KORD-IDDISTR    TO EFR-IDDISTR                            
013000           MOVE KORD-IDKUNDNR   TO EFR-IDKUNDNR                           
013100           MOVE KORD-IDKUNDRF   TO EFR-IDKUNDRF                           
013200           MOVE ORAD-IDARTNR    TO EFR-IDARTNR                            
013300           MOVE ORAD-IDPURAD    TO EFR-IDPURAD                            
013301                                   W-IDPURAD                              
013310           MOVE SPACE           TO EFR-FLDELPAC                           
013320           MOVE ZERO            TO EFR-IDFAKT                             
013500                                                                          
013600           IF ORAD-KVLEVART = 0                                           
013700             IF ORAD-KDRADSTA < 4                                         
013800*------------------- EJ PACKNINGSRAPPORTERADE RADER                       
013900               MOVE ORAD-KVAVBART TO EFR-KVLEVART                         
014000               IF ORAD-KDRADSTA < 4                                       
014100                 MOVE  NEJ        TO EFR-FLDELPAC                         
014200               ELSE                                                       
014300                 MOVE  JA         TO EFR-FLDELPAC                         
014400               END-IF                                                     
014500               PERFORM S01-SKRIV-EFR                                      
014600             END-IF                                                       
014700           ELSE                                                           
014800             MOVE      NEJ        TO EFR-FLDELPAC                         
014900             IF ORAD-KDRADSTA = 4 OR 5                                    
015000*------------------- FIX FÖR ATT KLARA KONV. PACKADE MED AVV.             
015100               MOVE ORAD-KVLEVART TO ORAD-KVAVBART                        
015200             END-IF                                                       
015300             COMPUTE ANTAL = ORAD-KVAVBART - ORAD-KVLEVART                
015400             IF ANTAL > ZERO                                              
015500*------------------- DELVIS PACKNINGSRAPPORTERADE RADER                   
015600               MOVE ANTAL          TO EFR-KVLEVART                        
015700               MOVE JA             TO EFR-FLDELPAC                        
015800               PERFORM S01-SKRIV-EFR                                      
015900             END-IF                                                       
016000             PERFORM IMS-GNP-WDE421                                       
016100             PERFORM UNTIL SEGMENT-SAKNAS                                 
016200               IF SEGMENT-FINNS                                           
016300                 MOVE KKOLLI-IDPRODNR TO W-IDPRODNR                       
016400                 MOVE KKOLLI-IDKOLLI  TO W-IDKOLLI                        
016500                 PERFORM IMS-GU-WDE611                                    
016600*FIX                                                                      
016700                 IF SEGMENT-SAKNAS                                        
016800                   DISPLAY 'P/K = ' W-IDPRODNR W-IDKOLLI                  
016900                 END-IF                                                   
017000*FIX                                                                      
017100               END-IF                                                     
017200*FIX...SEGM-FINNS                                                         
017300               IF SEGMENT-FINNS                                           
017400                                                                          
017500*                MOVE KOLLI-IDFAKT    TO WS-IDFAKT                        
017600*                MOVE WS-IDFAKT       TO EFR-IDFAKT                       
017700                                                                          
017730                                                                          
017800                 IF KOLLI-IDFAKT = ZERO                                   
017810                 MOVE KOLLI-IDKOLLI   TO WS-IDFAKT                        
017820                 MOVE WS-IDFAKT       TO EFR-IDFAKT                       
017900                   MOVE KKOLLI-KVLEVART TO EFR-KVLEVART                   
018000                   PERFORM S01-SKRIV-EFR                                  
018100                 END-IF                                                   
018300               END-IF                                                     
018400*FIX                                                                      
018500               PERFORM IMS-GNP-WDE421                                     
018600             END-PERFORM                                                  
018700           END-IF                                                         
018800         END-IF                                                           
018900         PERFORM IMS-GNP-WDE411                                           
019000       END-PERFORM                                                        
019100       PERFORM IMS-GN-WDE401                                              
019200                                                                          
019300     END-PERFORM                                                          
019400     PERFORM Z-FINIT                                                      
019500     MOVE ZERO TO RETURN-CODE                                             
019600     GOBACK                                                               
019700     .                                                                    
019800     EJECT                                                                
019900 A-INIT SECTION.                                                          
020000     SKIP3                                                                
020100     OPEN OUTPUT EFR                                                      
020200                                                                          
020300     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
020400                                                                          
020500     MOVE SPACE TO EFR-AREA                                               
020600     MOVE 001 TO EFR-IDPTYP                                               
020700     MOVE ZERO TO EFR-IDARTNR                                             
020800                  EFR-KVLEVART                                            
020900     .                                                                    
021000     EJECT                                                                
021100 S01-SKRIV-EFR SECTION.                                                   
021200     SKIP2                                                                
021300     WRITE EFR-POST FROM EFR-AREA                                         
021400     MOVE 'EFR' TO POSTSUM-FDNAMN                                         
021500     MOVE 'W47880D1' TO POSTSUM-DDNAMN2                                   
021600     MOVE '001'  TO POSTSUM-TRANSTYP                                      
021700     CALL POSTSUM USING POSTSUM-PARM                                      
021800     .                                                                    
021900     SKIP3                                                                
022000 Z-FINIT   SECTION.                                                       
022100     SKIP2                                                                
022200     CLOSE  EFR                                                           
022300                                                                          
022400*- - - - - - - - - - - - - - -  SKRIV UT ANTAL LÄSTA OCH                  
022500*                               SKRIVNA POSTER                            
022600     MOVE 'S' TO POSTSUM-OPKOD                                            
022700     CALL POSTSUM USING POSTSUM-PARM                                      
022800     .                                                                    
022900     EJECT                                                                
023000* --- IMS-SEKTIONER --- *                                                 
023100                                                                          
023200 IMS-GN-WDE401 SECTION.                                                   
023300     SKIP3                                                                
023400     MOVE 'WDE401 ' TO SSA1                                               
023500     MOVE '  GB' TO GODK-STATUSKODER                                      
023600     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-E401 SSA1                      
023700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
023800     PERFORM IMS-STATUSKONTROLL                                           
023900     .                                                                    
024000     SKIP3                                                                
024100 IMS-GNP-WDE411 SECTION.                                                  
024200     SKIP3                                                                
024300     MOVE 'WDE411 ' TO SSA1                                               
024400     MOVE '  GE' TO GODK-STATUSKODER                                      
024500     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E411 SSA1                     
024600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
024700     PERFORM IMS-STATUSKONTROLL                                           
024800     .                                                                    
024900     SKIP3                                                                
025000 IMS-GNP-WDE421 SECTION.                                                  
025100                                                                          
025200     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
025300             DELIMITED BY SIZE INTO SSA1                                  
025400     MOVE 'WDE421 ' TO SSA2                                               
025500     MOVE '  GE' TO GODK-STATUSKODER                                      
025600     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E421 SSA1 SSA2                
025700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
025800     PERFORM IMS-STATUSKONTROLL                                           
025900     .                                                                    
026000     EJECT                                                                
026100 IMS-GU-WDE611 SECTION.                                                   
026200                                                                          
026300     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
026400             DELIMITED BY SIZE INTO SSA1                                  
026500     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
026600             DELIMITED BY SIZE INTO SSA2                                  
026700     MOVE '  GE' TO GODK-STATUSKODER                                      
026800     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E611 SSA1 SSA2                 
026900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
027000     PERFORM IMS-STATUSKONTROLL.                                          
027100     SKIP3                                                                
027200 IMS-STATUSKONTROLL SECTION.                                              
027300     SET STATUS-IX TO 1                                                   
027400     SEARCH GODK-STATUS                                                   
027500       AT END CALL FELLOG                                                 
027600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
027700     END-SEARCH                                                           
027800     .                                                                    
