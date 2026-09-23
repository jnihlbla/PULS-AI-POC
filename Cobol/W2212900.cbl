000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2210900.                                                
000300 AUTHOR.         INGER STENING.                                           
000400 DATE-WRITTEN.   13/04/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        RENSAR GAMLA HÄNDELSER WDD4 FÖR NDC                              
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
003900 01  RKOD                        PIC S9(4)   VALUE +32 COMP SYNC.         
004000 77  IX-NDC                      PIC S9(4)   VALUE +0  COMP SYNC.         
004100 77  IX-NDC-MAX                  PIC S9(4)   VALUE +10 COMP SYNC.         
004200     SKIP2                                                                
004300 01  FELTEXT.                                                             
004400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004600     EJECT                                                                
004700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004800 01  FILLER REDEFINES DAGENS-DATUM.                                       
004900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005200 01  DAGENS-DATUM-MINUS-2VV      PIC 9(8)    VALUE ZERO.                  
005300 01  FILLER REDEFINES DAGENS-DATUM-MINUS-2VV.                             
005400     03  DAGENS-SS-MINUS-2VV     PIC 9(2).                                
005500     03  DAGENS-AAMMDD-MINUS-2VV PIC 9(6).                                
005600 01  DAGENS-DATUM-MINUS-2VV-9KOMPL PIC 9(8)    VALUE ZERO.                
005700     EJECT                                                                
005800*      --- VALID IDDC CODES                                               
005900*01  -COPY WWDCKONS                                                       
006000     EJECT                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200*                                                                         
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006600     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
006700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006800     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL POSTSUM                                          
007300*                                                                         
007400*01  -COPY W0005   -PRE  POSTSUM-                                         
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL DATKORT                                          
007700*                                                                         
007800 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22109'.              
007900     SKIP2                                                                
008000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
008100     SKIP2                                                                
008200*01  -COPY WDATKORT                                                       
008300     EJECT                                                                
008400*01  -COPY WORKAREA                                                       
008500     EJECT                                                                
008600*01  -COPY WDATAREA                                                       
008700     EJECT                                                                
008800* VARIABLER TILL SUBPROGRAM W009VADD                                      
008900 01  DATUM-AAVV                  PIC S9(5)  COMP-3.                       
009000 01  ANTAL-VECKOR                PIC S9(3)  COMP-3.                       
009100*                                                                         
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009400     SKIP3                                                                
009500 01  NYCKLAR-TILL-DLI.                                                    
009600     03  W-WDD401-X.                                                      
009700         05  W-WDD401KY          PIC X(13)    VALUE SPACE.                
009800         05  FILLER REDEFINES W-WDD401KY.                                 
009900             07  W-DAREGDAT-9KOMPL  PIC 9(8).                             
010000             07  W-TIKLOCK-9KOMPL   PIC S9(9) COMP-3.                     
010100     SKIP2                                                                
010200     03  W-IDDC-X.                                                        
010300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010400                                                                          
010500*    --- STATUS-KOD FRÅN IMS                                              
010600 01  STATUS-WS                   PIC XX.                                  
010700     88  SEGMENT-FINNS                       VALUE '  '.                  
010800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011100     88  IMS-EJ-OK                           VALUE 'XD'.                  
011200     SKIP2                                                                
011300 01  GODK-STATUSKODER.                                                    
011400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011500     SKIP3                                                                
011600 01  SSA1                        PIC X(64).                               
011700 01  SSA2                        PIC X(64).                               
011800     EJECT                                                                
011900*    --- IMS FUNKTIONSKODER                                               
012000*01  -COPY W0003                                                          
012100     EJECT                                                                
012200*    ---  DLI INPUT-OUTPUT AREA                                           
012300                                                                          
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD401'.                      
012500 01  DLI-IO-WDD401.                                                       
012600*    03  -COPY WDD401                                                     
012700                                                                          
012800     EJECT                                                                
012900 LINKAGE SECTION.                                                         
013000                                                                          
013100*01  -COPY W0009   -PRE MSG-                                              
013200                                                                          
013300*01  -COPY W0008  -PRE WDD4-                                              
013400     05  FILLER                  PIC X.                                   
013500     EJECT                                                                
013600 PROCEDURE DIVISION  USING MSG-PCB WDD4-PCB.                              
013700 MAIN SECTION.                                                            
013800     ENTRY 'DLITCBL' USING MSG-PCB WDD4-PCB.                              
013900                                                                          
014000     MOVE +1 TO IX-NDC                                                    
014100     PERFORM UNTIL IX-NDC > IX-NDC-MAX                                    
014200       PERFORM A-INIT                                                     
014300       MOVE DAGENS-DATUM-MINUS-2VV-9KOMPL TO W-DAREGDAT-9KOMPL            
014400       MOVE ZERO                          TO W-TIKLOCK-9KOMPL             
014500       PERFORM IMS-GET-WDD401                                             
014600       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
014700         IF CHKP-ANT > CHKP-MAX                                           
014800           PERFORM X-TAG-CHECKPOINT                                       
014900         END-IF                                                           
015000                                                                          
015100         IF LAK-DAREGDAT-9KOMPL > DAGENS-DATUM-MINUS-2VV-9KOMPL           
015200           PERFORM IMS-DLET-WDD401                                        
015300         END-IF                                                           
015400                                                                          
015500         PERFORM IMS-GET-WDD401                                           
015600       END-PERFORM                                                        
015700       ADD +1 TO IX-NDC                                                   
015800     END-PERFORM                                                          
015900                                                                          
016000                                                                          
016100     PERFORM Z-FINIT                                                      
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 A-INIT SECTION.                                                          
016800     SKIP2                                                                
016900                                                                          
017000     PERFORM IMS-RESTART                                                  
017100                                                                          
017200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017300                                                                          
017400                                                                          
017500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
017600     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
017700     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
017800     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
017900     DISPLAY ' DAGENS DATUM         ' DAGENS-DATUM                        
018000                                                                          
018100     MOVE DAGENS-DATUM   TO WORK-TIAAMMDD-TOM                             
018200     MOVE 3              TO WORK-KDCALL                                   
018300     MOVE 11             TO WORK-KVWORKD                                  
018400                                                                          
018500     IF IX-NDC = +1                                                       
018600       MOVE WC-NDC-CN-71 TO WORK-IDDC                                     
018800     END-IF                                                               
018900     IF IX-NDC = +2                                                       
019000       MOVE WC-NDC-CN-72 TO WORK-IDDC                                     
019200     END-IF                                                               
019300     IF IX-NDC = +3                                                       
019400       MOVE WC-NDC-CN-73 TO WORK-IDDC                                     
019600     END-IF                                                               
019610     IF IX-NDC = +4                                                       
019620       MOVE WC-NDC-CN-74 TO WORK-IDDC                                     
019630     END-IF                                                               
019700     IF IX-NDC = +5                                                       
019710       MOVE WC-NDC-US-RU TO WORK-IDDC                                     
019730     END-IF                                                               
019780     IF IX-NDC = +6                                                       
019790       MOVE WC-NDC-US-LA TO WORK-IDDC                                     
019792     END-IF                                                               
019793     IF IX-NDC = +7                                                       
019794       MOVE WC-NDC-US-SE TO WORK-IDDC                                     
019796     END-IF                                                               
019797     IF IX-NDC = +8                                                       
019798       MOVE WC-NDC-US-CH TO WORK-IDDC                                     
019800     END-IF                                                               
019801     IF IX-NDC = +9                                                       
019802       MOVE WC-NDC-US-JA TO WORK-IDDC                                     
019803     END-IF                                                               
019804     IF IX-NDC = +10                                                      
019805       MOVE WC-NDC-US-DA TO WORK-IDDC                                     
019806     END-IF                                                               
019808     MOVE WORK-IDDC      TO W-IDDC                                        
019810                                                                          
019900     CALL WORKDAY USING                                                   
020000          WORK-KDCALL                                                     
020100          WORK-DATE-AREA                                                  
020200          WORK-KDSVAR                                                     
020300                                                                          
020400     IF WORK-KDSVAR-OK                                                    
020500        MOVE WORK-TIAAMMDD-FOM TO DAGENS-AAMMDD-MINUS-2VV                 
020600        MOVE 20                TO DAGENS-SS-MINUS-2VV                     
020700        DISPLAY ' DAGENS DATUM - 2VV ' DAGENS-DATUM-MINUS-2VV             
020800     ELSE                                                                 
020900       DISPLAY '*** W22109, FEL I WORKDAY '                               
021000       MOVE +25 TO RKOD                                                   
021100       CALL ABEND USING RKOD                                              
021200     END-IF                                                               
021300                                                                          
021400     COMPUTE DAGENS-DATUM-MINUS-2VV-9KOMPL =                              
021500             99999999 - DAGENS-DATUM-MINUS-2VV                            
021600     DISPLAY ' DAGENS DATUM-2VV-9 ' DAGENS-DATUM-MINUS-2VV-9KOMPL         
021700     .                                                                    
021800     EJECT                                                                
021900 Z-FINIT SECTION.                                                         
022000                                                                          
022100     MOVE 'S' TO POSTSUM-OPKOD                                            
022200     CALL POSTSUM USING POSTSUM-PARM                                      
022300     .                                                                    
022400     EJECT                                                                
022500 X-TAG-CHECKPOINT   SECTION.                                              
022600                                                                          
022700* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
022800* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
022900     MOVE LAK-DAREGDAT-9KOMPL TO W-DAREGDAT-9KOMPL                        
023000     MOVE LAK-TIKLOCK-9KOMPL  TO W-TIKLOCK-9KOMPL                         
023100     PERFORM IMS-CHECKPOINT                                               
023200     MOVE ZERO TO CHKP-ANT                                                
023300* --- LÄS OM DATABAS OM DET BEHÖVS                                        
023400     PERFORM IMS-GET-WDD401                                               
023500                                                                          
023600     .                                                                    
023700     EJECT                                                                
023800* --- IMS SEKTIONER ---                                                   
023900                                                                          
024000 IMS-GET-WDD401   SECTION.                                                
024100                                                                          
024200     STRING 'WDD401  (WDD401KY>=' W-WDD401-X                              
024300                    '&IDDC     =' W-IDDC-X ')'                            
024400          DELIMITED BY SIZE INTO SSA1                                     
024500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
024600     CALL CBLTDLI USING GHN WDD4-PCB DLI-IO-WDD401   SSA1                 
024700     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
024800     PERFORM IMS-STATUSKONTROLL                                           
024900     IF SEGMENT-FINNS                                                     
025000        MOVE 'W22109'   TO POSTSUM-FDNAMN                                 
025100        MOVE 'WDD401  ' TO POSTSUM-DDNAMN2                                
025200        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
025300        CALL POSTSUM USING POSTSUM-PARM                                   
025400     END-IF                                                               
025500     .                                                                    
025600     SKIP3                                                                
025700 IMS-DLET-WDD401   SECTION.                                               
025800                                                                          
025900     MOVE '  ' TO GODK-STATUSKODER                                        
026000     CALL CBLTDLI USING DLET WDD4-PCB DLI-IO-WDD401                       
026100     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
026200     PERFORM IMS-STATUSKONTROLL                                           
026300     IF SEGMENT-FINNS                                                     
026400        MOVE 'W22109'   TO POSTSUM-FDNAMN                                 
026500        MOVE 'WDD401  ' TO POSTSUM-DDNAMN2                                
026600        MOVE 'DLET'     TO POSTSUM-TRANSTYP                               
026700        CALL POSTSUM USING POSTSUM-PARM                                   
026800     END-IF                                                               
026900     ADD  +1   TO CHKP-ANT                                                
027000     .                                                                    
027100     EJECT                                                                
027200 IMS-RESTART SECTION.                                                     
027300     SKIP2                                                                
027400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027500     MOVE '  ' TO GODK-STATUSKODER                                        
027600     CALL CBLTDLI USING XRST MSG-PCB                                      
027700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027800                        CHKP-AREA-LENGTH CHKP-AREA                        
027900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028000     PERFORM IMS-STATUSKONTROLL                                           
028100     .                                                                    
028200     SKIP3                                                                
028300 IMS-CHECKPOINT SECTION.                                                  
028400     SKIP2                                                                
028500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
028600     MOVE '  XD' TO GODK-STATUSKODER                                      
028700     CALL CBLTDLI USING CHKP MSG-PCB                                      
028800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
028900                        CHKP-AREA-LENGTH CHKP-AREA                        
029000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029100     PERFORM IMS-STATUSKONTROLL                                           
029200                                                                          
029300     IF IMS-EJ-OK                                                         
029400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
029500       DISPLAY FELTEXT                                                    
029600       CALL FELLOG                                                        
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000 IMS-STATUSKONTROLL SECTION.                                              
030100     SKIP2                                                                
030200     SET STATUS-IX TO 1                                                   
030300     SEARCH GODK-STATUS                                                   
030400       AT END                                                             
030500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
030600           DELIMITED BY SIZE INTO FELTEXT                                 
030700         DISPLAY FELTEXT                                                  
030800         CALL FELLOG                                                      
030900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031000         CONTINUE                                                         
031100     END-SEARCH                                                           
031200     .                                                                    
