000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2210900.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   00/12/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        RENSAR GAMLA HÄNDELSER WDD4                                      
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDD4                                       
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200     SKIP3                                                                
002300 FILE SECTION.                                                            
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(8)    VALUE 'W2210900'.            
003000 01  CHKP-VAR.                                                            
003100     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003200     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
003300     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
003400     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
003500     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
003600     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 01  RKOD                    PIC S9(4)               COMP SYNC.           
004000     SKIP2                                                                
004100 01  FELTEXT.                                                             
004200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004400     EJECT                                                                
004500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004600 01  FILLER REDEFINES DAGENS-DATUM.                                       
004700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005000 01  DAGENS-DATUM-MINUS-2VV      PIC 9(8)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM-MINUS-2VV.                             
005200     03  DAGENS-SS-MINUS-2VV     PIC 9(2).                                
005300     03  DAGENS-AAMMDD-MINUS-2VV PIC 9(6).                                
005400 01  DAGENS-DATUM-MINUS-2VV-9KOMPL PIC 9(8)    VALUE ZERO.                
005500     EJECT                                                                
005510*      --- VALID IDDC CODES                                               
005520*01  -COPY WWDCKONS                                                       
005530     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006100     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
006200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006300     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
006400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL DATKORT                                          
007200*                                                                         
007300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22109'.              
007400     SKIP2                                                                
007500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007600     SKIP2                                                                
007700*01  -COPY WDATKORT                                                       
007800     EJECT                                                                
007900*01  -COPY WORKAREA                                                       
008000     EJECT                                                                
008100*01  -COPY WDATAREA                                                       
008200     EJECT                                                                
008300* VARIABLER TILL SUBPROGRAM W009VADD                                      
008400 01  DATUM-AAVV                  PIC S9(5)  COMP-3.                       
008500 01  ANTAL-VECKOR                PIC S9(3)  COMP-3.                       
008600*                                                                         
008700     EJECT                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900     SKIP3                                                                
009000 01  NYCKLAR-TILL-DLI.                                                    
009100     03  W-WDD401-X.                                                      
009200         05  W-WDD401KY          PIC X(13)    VALUE SPACE.                
009300         05  FILLER REDEFINES W-WDD401KY.                                 
009400             07  W-DAREGDAT-9KOMPL  PIC 9(8).                             
009500             07  W-TIKLOCK-9KOMPL   PIC S9(9) COMP-3.                     
009600     SKIP2                                                                
009610     03  W-IDDC-X.                                                        
009620         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
009630                                                                          
009700*    --- STATUS-KOD FRÅN IMS                                              
009800 01  STATUS-WS                   PIC XX.                                  
009900     88  SEGMENT-FINNS                       VALUE '  '.                  
010000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010300     88  IMS-EJ-OK                           VALUE 'XD'.                  
010400     SKIP2                                                                
010500 01  GODK-STATUSKODER.                                                    
010600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010700     SKIP3                                                                
010800 01  SSA1                        PIC X(64).                               
010900 01  SSA2                        PIC X(64).                               
011000     EJECT                                                                
011100*    --- IMS FUNKTIONSKODER                                               
011200*01  -COPY W0003                                                          
011300     EJECT                                                                
011400*    ---  DLI INPUT-OUTPUT AREA                                           
011500                                                                          
011600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD401'.                      
011700 01  DLI-IO-WDD401.                                                       
011800*    03  -COPY WDD401                                                     
011900                                                                          
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200                                                                          
012300*01  -COPY W0009   -PRE MSG-                                              
012400                                                                          
012500*01  -COPY W0008  -PRE WDD4-                                              
012600     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012800 PROCEDURE DIVISION  USING MSG-PCB WDD4-PCB.                              
012900 MAIN SECTION.                                                            
013000     ENTRY 'DLITCBL' USING MSG-PCB WDD4-PCB.                              
013100                                                                          
013200*------------------------                                                 
013300*---- FLYTTA SUBPROGRAM CALL TILL RÄTT STÄLLE --                          
013400     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
013500*    MOVE AAMMDD TO DAT-I-TIDATUM                                         
013600                                                                          
013700*    CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
013800*                    DAT-O-TIDATUM DAT-KDSVAR                             
013900                                                                          
014000*    IF DAT-KDSVAR-OK                                                     
014100*      ......                                                             
014200*    ELSE                                                                 
014300*      .........                                                          
014400*    END-IF                                                               
014500*------------------------                                                 
014600*---- FLYTTA SUBPROGRAM CALL TILL RÄTT STÄLLE --                          
014700*    MOVE AAVV        TO DATUM-AAVV                                       
014800*    MOVE -2          TO ANTAL-VECKOR                                     
014900                                                                          
015000*    CALL W009VADD USING DATUM-AAVV ANTAL-VECKOR                          
015100*------------------------------                                           
015200     SKIP2                                                                
015300     PERFORM A-INIT                                                       
015400     MOVE DAGENS-DATUM-MINUS-2VV-9KOMPL TO W-DAREGDAT-9KOMPL              
015500     MOVE ZERO                          TO W-TIKLOCK-9KOMPL               
015600     PERFORM IMS-GET-WDD401                                               
015700     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
015800       IF CHKP-ANT > CHKP-MAX                                             
015900         PERFORM X-TAG-CHECKPOINT                                         
016000       END-IF                                                             
016100                                                                          
016200       IF LAK-DAREGDAT-9KOMPL > DAGENS-DATUM-MINUS-2VV-9KOMPL             
016300          PERFORM IMS-DLET-WDD401                                         
016400          DISPLAY ' DLET IDARTNR  ' LAK-IDARTNR ' ' LAK-IDDC ' '          
016500                  LAK-DAREGDAT-9KOMPL                                     
016600       END-IF                                                             
016700                                                                          
016800       PERFORM IMS-GET-WDD401                                             
016900     END-PERFORM                                                          
017000                                                                          
017100                                                                          
017200     PERFORM Z-FINIT                                                      
017300                                                                          
017400     MOVE ZERO TO RETURN-CODE                                             
017500     GOBACK                                                               
017600     .                                                                    
017700     EJECT                                                                
017800 A-INIT SECTION.                                                          
017900     SKIP2                                                                
018000                                                                          
018100     PERFORM IMS-RESTART                                                  
018200                                                                          
018300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018400                                                                          
018500                                                                          
018600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
018700     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
018800     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
018900     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
019000     DISPLAY ' DAGENS DATUM         ' DAGENS-DATUM                        
019100                                                                          
019200     MOVE DAGENS-DATUM   TO WORK-TIAAMMDD-TOM                             
019300     MOVE 3              TO WORK-KDCALL                                   
019400     MOVE 11             TO WORK-KVWORKD                                  
019501     MOVE WC-CDC-SE      TO WORK-IDDC                                     
019510                            W-IDDC                                        
019600                                                                          
019700     CALL WORKDAY USING                                                   
019800          WORK-KDCALL                                                     
019900          WORK-DATE-AREA                                                  
020000          WORK-KDSVAR                                                     
020100                                                                          
020200     IF WORK-KDSVAR-OK                                                    
020300        MOVE WORK-TIAAMMDD-FOM TO DAGENS-AAMMDD-MINUS-2VV                 
020400        MOVE 20                TO DAGENS-SS-MINUS-2VV                     
020500        DISPLAY ' DAGENS DATUM - 2VV ' DAGENS-DATUM-MINUS-2VV             
020600     ELSE                                                                 
020700       DISPLAY '*** W22109, FEL I WORKDAY '                               
020800       MOVE +25 TO RKOD                                                   
020900       CALL ABEND USING RKOD                                              
021000     END-IF                                                               
021100                                                                          
021200     COMPUTE DAGENS-DATUM-MINUS-2VV-9KOMPL =                              
021300             99999999 - DAGENS-DATUM-MINUS-2VV                            
021400     DISPLAY ' DAGENS DATUM-2VV-9 ' DAGENS-DATUM-MINUS-2VV-9KOMPL         
021500     .                                                                    
021600     EJECT                                                                
021700 Z-FINIT SECTION.                                                         
021800                                                                          
021900     MOVE 'S' TO POSTSUM-OPKOD                                            
022000     CALL POSTSUM USING POSTSUM-PARM                                      
022100     .                                                                    
022200     EJECT                                                                
022300 X-TAG-CHECKPOINT   SECTION.                                              
022400                                                                          
022500* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
022600* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
022700     MOVE LAK-DAREGDAT-9KOMPL TO W-DAREGDAT-9KOMPL                        
022800     MOVE LAK-TIKLOCK-9KOMPL  TO W-TIKLOCK-9KOMPL                         
022900     PERFORM IMS-CHECKPOINT                                               
023000     MOVE ZERO TO CHKP-ANT                                                
023100* --- LÄS OM DATABAS OM DET BEHÖVS                                        
023200     PERFORM IMS-GET-WDD401                                               
023300                                                                          
023400     .                                                                    
023500     EJECT                                                                
023600* --- IMS SEKTIONER ---                                                   
023700                                                                          
023800 IMS-GET-WDD401   SECTION.                                                
023900                                                                          
024000     STRING 'WDD401  (WDD401KY>=' W-WDD401-X                              
024100                    '&IDDC     =' W-IDDC-X ')'                            
024200          DELIMITED BY SIZE INTO SSA1                                     
024300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
024400     CALL CBLTDLI USING GHN WDD4-PCB DLI-IO-WDD401   SSA1                 
024500     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
024600     PERFORM IMS-STATUSKONTROLL                                           
024700     IF SEGMENT-FINNS                                                     
024800        MOVE 'W22109'   TO POSTSUM-FDNAMN                                 
024900        MOVE 'WDD401  ' TO POSTSUM-DDNAMN2                                
025000        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
025100        CALL POSTSUM USING POSTSUM-PARM                                   
025200     END-IF                                                               
025300     .                                                                    
025400     SKIP3                                                                
025500 IMS-DLET-WDD401   SECTION.                                               
025600                                                                          
025700     MOVE '  ' TO GODK-STATUSKODER                                        
025800     CALL CBLTDLI USING DLET WDD4-PCB DLI-IO-WDD401                       
025900     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
026000     PERFORM IMS-STATUSKONTROLL                                           
026100     IF SEGMENT-FINNS                                                     
026200        MOVE 'W22109'   TO POSTSUM-FDNAMN                                 
026300        MOVE 'WDD401  ' TO POSTSUM-DDNAMN2                                
026400        MOVE 'DLET'     TO POSTSUM-TRANSTYP                               
026500        CALL POSTSUM USING POSTSUM-PARM                                   
026600     END-IF                                                               
026700     ADD  +1   TO CHKP-ANT                                                
026800     .                                                                    
026900     EJECT                                                                
027000 IMS-RESTART SECTION.                                                     
027100     SKIP2                                                                
027200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027300     MOVE '  ' TO GODK-STATUSKODER                                        
027400     CALL CBLTDLI USING XRST MSG-PCB                                      
027500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027600                        CHKP-AREA-LENGTH CHKP-AREA                        
027700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027800     PERFORM IMS-STATUSKONTROLL                                           
027900     .                                                                    
028000     SKIP3                                                                
028100 IMS-CHECKPOINT SECTION.                                                  
028200     SKIP2                                                                
028300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
028400     MOVE '  XD' TO GODK-STATUSKODER                                      
028500     CALL CBLTDLI USING CHKP MSG-PCB                                      
028600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
028700                        CHKP-AREA-LENGTH CHKP-AREA                        
028800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028900     PERFORM IMS-STATUSKONTROLL                                           
029000                                                                          
029100     IF IMS-EJ-OK                                                         
029200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
029300       DISPLAY FELTEXT                                                    
029400       CALL FELLOG                                                        
029500     END-IF                                                               
029600     .                                                                    
029700     EJECT                                                                
029800 IMS-STATUSKONTROLL SECTION.                                              
029900     SKIP2                                                                
030000     SET STATUS-IX TO 1                                                   
030100     SEARCH GODK-STATUS                                                   
030200       AT END                                                             
030300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
030400           DELIMITED BY SIZE INTO FELTEXT                                 
030500         DISPLAY FELTEXT                                                  
030600         CALL FELLOG                                                      
030700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030800         CONTINUE                                                         
030900     END-SEARCH                                                           
031000     .                                                                    
