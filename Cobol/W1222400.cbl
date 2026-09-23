000100     SKIP2                                                                
000200 ID DIVISION.                                                             
000300*                                                                         
000400 PROGRAM-ID.             W1222400.                                        
000500*AUTHOR.                 P DAHLÖF.                                        
000600*DATE-WRITTEN.           AUG 1987.                                        
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*            RENSNING AV BENÄMNINGSREGISTRET FRÅN WDD3                    
001100*            UTSKRIFT SKER PÅ FIL W12225                                  
001200                                                                          
001300*    RENSNING SKER:                                                       
001400*            PÅ ALLA B-MÄRKTA ARTIKLAR PÅ FIL W12207                      
001500*                                                                         
001600*            (DVS ARTIKELN HAR ERSÄTTNINGSKOD 52 SAMT                     
001700*            ERSÄTTNINGSDATUM ÄLDRE ÄN 6 MÅNADER ELLER                    
001800*            NÄR ARTIKELN HAR ERSÄTTNINGSKOD 29 SAMT                      
001900*            ERSÄTTNINGSDATUM ÄLDRE ÄN 6 ÅR.)                             
002000*                                                                         
002100                                                                          
002200                                                                          
002300     EJECT                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500 INPUT-OUTPUT SECTION.                                                    
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*- - - - - - - - - - - - - - INFIL:                                       
002900     SELECT W12207         ASSIGN TO     W12224D1.                        
003000*- - - - - - - - - - - - - - UTFIL:                                       
003100     SELECT W12225         ASSIGN TO     W12224D2.                        
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD   W12207                                                              
003700      RECORDING F                                                         
003800      BLOCK CONTAINS 0.                                                   
003900*01   POST -COPY W12207   -PRE W12207-    -L.                             
004000*- - - - - - - - - - - - - -  UTFIL                                       
004100 FD   W12225                                                              
004200      RECORDING F                                                         
004300      BLOCK CONTAINS 0.                                                   
004400*01   UTPOST -COPY W12225    -L.                                          
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800*    -- CHECKED BY WY2000                                                 
004900 77  IDPGM                       PIC X(8)    VALUE 'W1222400'.            
005000     SKIP2                                                                
005100*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
005200                                                                          
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005410 77  W-DLET-WDD312               PIC 9(7)    VALUE ZERO.                  
005500*                                  *** DIV DATUM FÄLT                     
005600     SKIP2                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
005900   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
006000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006200     SKIP3                                                                
006300*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
006400     EJECT                                                                
006500*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
006600     SKIP1                                                                
006700*01  -COPY W0005     -PRE  POSTSUM-.                                      
006800     EJECT                                                                
006900                                                                          
007000*- - - - - - - - - - - - - -  PARAMETRAR TILL DATUMKORT                   
007100                                                                          
007200                                                                          
007300*- - - - - - - - - - - - - -  ARBETSAREA FÖR INFIL                        
007400 01  FILLER                      PIC X(24)    VALUE 'INFIL'.              
007500                                                                          
007600*01  AREA    -COPY W12207   -PRE W-IN-.                                   
007700*- - - - - - - - - - - - - -  ARBETSAREA FÖR UTFIL                        
007800 01  FILLER                      PIC X(24)    VALUE 'UTFIL'.              
007900                                                                          
008000*01  AREA    -COPY W12225   -PRE W-UT-.                                   
008100 77  W12207-EOF                  PIC X       VALUE 'N'.                   
008200     EJECT                                                                
008300*- - - - - - - - -PARAMETER-AREOR TILL IMS-SUBPGM-SEKTIONER.              
008400 01  FILLER                      PIC X(16) VALUE 'IMS-WS'.                
008500     SKIP3                                                                
008600*- - - - - - - - -STATUSKOD FRÅN IMS                                      
008700 01  STATUS-WS                   PIC X(2).                                
008800   88  SEGMENT-FINNS         VALUE '  '.                                  
008900   88  SEGMENT-SAKNAS        VALUE 'GE'.                                  
009000   88  SEGMENT-FINNS-REDAN   VALUE 'II'.                                  
009100   SKIP3                                                                  
009200 01  GODK-STATUSKODER.                                                    
009300    03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
009400 01  SSA1                        PIC X(32).                               
009500 01  SSA2                        PIC X(32).                               
009600*- - - - - - - - - - - - - - NYCKLAR TILL DLI.                            
009700 01  NYCKLAR-TILL-DLI.                                                    
009800   03   W-IDARTNR-X.                                                      
009900     05 W-IDARTNR                PIC S9(9)  COMP-3.                       
010000   03   W-IDSKYLT-X.                                                      
010100     05 W-IDSKYLT                PIC  X(3)  VALUE 'S  '.                  
010200*                                                                         
010300*01   -COPY W0003                                                         
010400     EJECT                                                                
010500 01  FILLER                      PIC X(16)    VALUE                       
010600                                              'DLI-IO-AREA'.              
010700 01  DLI-IO-AREA.                                                         
010800   03  IO-AREA                   PIC X(130) VALUE SPACE.                  
010900*03  WLBENA01  -COPY WDD301  -RED IO-AREA.                                
011000                                                                          
011100                                                                          
011200*03  WLBENA11  -COPY WDD311  -RED IO-AREA.                                
011300     EJECT                                                                
011400                                                                          
011500*03  WLBENA12  -COPY WDD312  -RED IO-AREA.                                
011600     EJECT                                                                
011700                                                                          
011800 LINKAGE SECTION.                                                         
011900     SKIP3                                                                
012000*01  -COPY W0008       -PRE BENA-.                                        
012100 05  FILLER             PIC X.                                            
012200     EJECT                                                                
012300 PROCEDURE DIVISION USING BENA-PCB.                                       
012400     ENTRY 'DLITCBL' USING                                                
012500     BENA-PCB.                                                            
012600     PERFORM A-INIT                                                       
012700     PERFORM S11-LAES-INFIL                                               
012800     PERFORM UNTIL                                                        
012900      NOT ( W12207-EOF = NEJ )                                            
013000       PERFORM B-RENSA-UTFAELT                                            
013100       MOVE   W-IN-IDARTNR      TO W-IDARTNR                              
013200                                   W-UT-IDARTNR                           
013300       MOVE W-IN-UTFIL-TYP      TO W-UT-UTFIL-TYP                         
013400       IF W-IN-UTFIL-TYP = 'B'                                            
013500         PERFORM IMS-GU-BENA01                                            
013600         MOVE BEN-KDHOMONYM    TO W-UT-KDHOMONYM                          
013700         PERFORM IMS-GNP-BENA11                                           
013800         MOVE TEXT-BEART       TO W-UT-BEART                              
013900         PERFORM IMS-GHNP-BENA12                                          
014100         PERFORM IMS-DELETE-BENA                                          
014110         ADD 1 TO W-DLET-WDD312                                           
014200       END-IF                                                             
014300       PERFORM S12-SKRIV-UTPOST                                           
014400       PERFORM S11-LAES-INFIL                                             
014500     END-PERFORM                                                          
014600     SKIP2                                                                
014700     PERFORM Z-FINIT                                                      
014800     MOVE ZERO TO RETURN-CODE                                             
014900     GOBACK                                                               
015000     CONTINUE.                                                            
015100     EJECT                                                                
015200 A-INIT SECTION.                                                          
015300     SKIP3                                                                
015400     OPEN INPUT  W12207                                                   
015500     OPEN OUTPUT W12225                                                   
015600     MOVE IDPGM             TO POSTSUM-PROGNAMN                           
015700     CONTINUE.                                                            
015800     EJECT                                                                
015900 B-RENSA-UTFAELT SECTION.                                                 
016000     MOVE ZERO             TO W-UT-IDARTNR                                
016100                              W-UT-KDHOMONYM                              
016200     MOVE SPACE            TO W-UT-BEART                                  
016300                              W-UT-UTFIL-TYP                              
016400     CONTINUE.                                                            
016500 S11-LAES-INFIL SECTION.                                                  
016600     READ W12207 INTO W-IN-AREA                                           
016700         AT END MOVE JA    TO W12207-EOF                                  
016800     END-READ                                                             
016900     IF W12207-EOF = NEJ                                                  
017000       MOVE 'W12224D1'   TO POSTSUM-DDNAMN2                               
017100       MOVE 'W12207'     TO POSTSUM-FDNAMN                                
017200       CALL POSTSUM USING POSTSUM-PARM                                    
017300     END-IF                                                               
017400     CONTINUE.                                                            
017500     EJECT                                                                
017600 S12-SKRIV-UTPOST SECTION.                                                
017700     WRITE UTPOST FROM W-UT-AREA                                          
017800     MOVE 'W12224D2'   TO POSTSUM-DDNAMN2                                 
017900     MOVE 'W12225'     TO POSTSUM-FDNAMN                                  
018000     CALL POSTSUM USING POSTSUM-PARM                                      
018100     CONTINUE.                                                            
018200     EJECT                                                                
018300 Z-FINIT SECTION.                                                         
018400     SKIP2                                                                
018410     DISPLAY 'ANTAL BORTTAGNA WDD312 SEGMENT: ' W-DLET-WDD312             
018500     CLOSE W12207                                                         
018600     CLOSE W12225                                                         
018700     MOVE 'S'      TO POSTSUM-OPKOD                                       
018800     CALL POSTSUM USING POSTSUM-PARM                                      
018900     CONTINUE.                                                            
019000     EJECT                                                                
019100*- - - - - - - - - - - - - - - - - - - -IMS-SECTIONER.                    
019200 IMS-GU-BENA01 SECTION.                                                   
019300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
019400            DELIMITED BY SIZE INTO SSA1                                   
019500     MOVE '  '                TO GODK-STATUSKODER                         
019600     CALL CBLTDLI USING GU   BENA-PCB DLI-IO-AREA SSA1                    
019700     MOVE BENA-STATUS-CODE  TO STATUS-WS                                  
019800     PERFORM IMS-STATUSKONTROLL                                           
019900     CONTINUE.                                                            
020000     SKIP3                                                                
020100 IMS-GNP-BENA11 SECTION.                                                  
020200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
020300            DELIMITED BY SIZE INTO SSA1                                   
020400     MOVE '  '                TO GODK-STATUSKODER                         
020500     CALL CBLTDLI USING GNP  BENA-PCB DLI-IO-AREA SSA1                    
020600     MOVE BENA-STATUS-CODE  TO STATUS-WS                                  
020700     PERFORM IMS-STATUSKONTROLL                                           
020800     CONTINUE.                                                            
020900     SKIP3                                                                
021000 IMS-GHNP-BENA12 SECTION.                                                 
021100     STRING 'WLBENA12(IDARTNR  =' W-IDARTNR-X ')'                         
021200            DELIMITED BY SIZE INTO SSA1                                   
021300     MOVE '  '                TO GODK-STATUSKODER                         
021400     CALL CBLTDLI USING GHNP BENA-PCB DLI-IO-AREA SSA1                    
021500     MOVE BENA-STATUS-CODE  TO STATUS-WS                                  
021600     PERFORM IMS-STATUSKONTROLL                                           
021700     CONTINUE.                                                            
021800     SKIP3                                                                
021900 IMS-DELETE-BENA SECTION.                                                 
022000     MOVE '  '              TO GODK-STATUSKODER                           
022100     CALL CBLTDLI USING DLET  BENA-PCB DLI-IO-AREA                        
022200     MOVE BENA-STATUS-CODE  TO STATUS-WS                                  
022300     PERFORM IMS-STATUSKONTROLL                                           
022400     CONTINUE.                                                            
022500     SKIP3                                                                
022600 IMS-STATUSKONTROLL SECTION.                                              
022700     SET STATUS-IX TO 1                                                   
022800     SEARCH GODK-STATUS                                                   
022900       AT END CALL FELLOG                                                 
023000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
023100     END-SEARCH                                                           
023200     CONTINUE                                                             
023300            CONTINUE.                                                     
023400 IMS-EXIT. EXIT.                                                          
023500     CONTINUE.                                                            
