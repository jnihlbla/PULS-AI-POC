000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0070900.                                                
000300 AUTHOR.         MATS VINNEFORS.                                          
000400 DATE-WRITTEN.   FEBRUARI 1984.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.   TP-SUBMIT-PROGRAM. KONTROLLERAR BEHÖRIGHET ATT           
000800*                KÖRA INMATAT JOBB OCH SUBMITTAR ÖNSKAT JOBB.             
000900*                                                                         
001000*    INDATA.                                                              
001100*        TRANSAKTION: W0T709                                              
001200*        MID:         W0I70901                                            
001300*    UTDATA.                                                              
001400*        MOD:         W0O7..01     OM INDATA FRÅN ANNAN BILD              
001500*                     W0O70901     OM INDATA FRÅN EGEN BILD               
001600*    SUBPROGRAM.                                                          
001700*        FELLOG                                                           
001800*                                                                         
001900*                                                                         
002000*            ÄNDRAT AV STEFANO GIOBBI                                     
002100*                                                                         
002200*            ÄNDRAT   FRÅN             TILL                               
002300*        88  EJ-MOD   VALUE '4218'     VALUE '4291'                       
002400*                                                                         
002500*                                                                         
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200*                                                                         
003300     SELECT INTRDR          ASSIGN TO UT-S-INTRDR.                        
003400*           JES2 JOBB-KÖ                                                  
003500                                                                          
003600                                                                          
003700 DATA DIVISION.                                                           
003800                                                                          
003900 FILE SECTION.                                                            
004000                                                                          
004100 FD  INTRDR                                                               
004200     LABEL RECORD STANDARD                                                
004300     RECORDING MODE F.                                                    
004400                                                                          
004500 01  INTERNAL-READER             PIC X(80).                               
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(8)    VALUE 'W0070900'.            
005100 77  OK                          PIC X(1)    VALUE 'O'.                   
005200 77  FEL                         PIC X(1)    VALUE 'F'.                   
005300 77  SECURITY-TEST               PIC X(1)    VALUE 'F'.                   
005400 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +129 COMP SYNC.        
005500 77  IX                          PIC S9(1)   COMP-3.                      
005600                                                                          
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000                                                                          
006100                                                                          
006200 01  W-MODNAMN                   PIC X(8).                                
006300     SKIP2                                                                
006400 01  W-IDTRANS.                                                           
006500*                                                                         
006600     03  W-TRANS-SIFF-1          PIC X(1).                                
006700     03  W-TRANS-SIFF-2          PIC X(1).                                
006800     03  W-TRANS-SIFF-3          PIC X(1).                                
006900     03  W-TRANS-SIFF-4          PIC X(1).                                
007000                                                                          
007100 01  FILLER REDEFINES W-IDTRANS  PIC X(4).                                
007200*                                                                         
007300     88  EGEN-BILD                           VALUE '0709'.                
007400     88  EJ-MOD                              VALUE '4291'.                
007500     EJECT                                                                
007600 01  NYCKLAR-TILL-DLI.                                                    
007700     03  W-WDP101KY-6001-X.                                               
007800         05  FILLER              PIC X(4)    VALUE '6001'.                
007900         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
008000     SKIP2                                                                
008100     03  W-WDP101KY-6011-X.                                               
008200         05  FILLER              PIC X(4)    VALUE '6011'.                
008300         05  W-IDRUTIN-6011      PIC X(8).                                
008400         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
008500     SKIP2                                                                
008600     03  W-WDP101KY-6021-X.                                               
008700         05  FILLER              PIC X(4)    VALUE '6021'.                
008800         05  W-IDRUTIN-6021      PIC X(8).                                
008900         05  W-IDJOB             PIC X(8).                                
009000         05  LOWVALUE            PIC X(10)   VALUE LOW-VALUE.             
009100     SKIP2                                                                
009200     03  W-IDUSER-X.                                                      
009300         05  W-IDUSER            PIC X(8).                                
009400     03  W-IDJOB1-X.                                                      
009500         05  W-IDJOB1            PIC X(8).                                
009600     EJECT                                                                
009700 01  MEDDELANDE.                                                          
009800*                                                                         
009900     03  W-RETT.                                                          
010000         05  FILLER.                                                      
010100             07  STARTAT-JOBB-S  PIC X(8).                                
010200             07  FILLER          PIC X(11)   VALUE ' ÄR STARTAT'.         
010300         05  FILLER.                                                      
010400             07  STARTAT-JOBB-E  PIC X(8).                                
010500             07  FILLER          PIC X(11)   VALUE ' IS STARTED'.         
010600     03  FILLER REDEFINES W-RETT.                                         
010700         05  RETT OCCURS 2       PIC X(19).                               
010800     SKIP2                                                                
010900 01  FELMEDDELANDE.                                                       
011000*                                                                         
011100     03  W-FEL-1.                                                         
011200         05  FILLER              PIC X(26)                                
011300         VALUE 'EJ AUKTORISERAD ANVÄNDARE '.                              
011400         05  FILLER              PIC X(26)                                
011500         VALUE 'UNAUTHORIZED USER         '.                              
011600     03  FILLER REDEFINES W-FEL-1.                                        
011700         05  FEL-1 OCCURS 2      PIC X(26).                               
011800                                                                          
011900     03  W-FEL-2.                                                         
012000         05  FILLER              PIC X(26)                                
012100         VALUE 'JOBB ELLER RUTIN SAKNAS   '.                              
012200         05  FILLER              PIC X(26)                                
012300         VALUE 'JOB OR ROUTINE IS MISSING '.                              
012400     03  FILLER REDEFINES W-FEL-2.                                        
012500         05  FEL-2 OCCURS 2      PIC X(26).                               
012600                                                                          
012700     03  W-FEL-3.                                                         
012800         05  FILLER              PIC X(36)                                
012900         VALUE 'FEL STATUS. JOBBET FÅR EJ STARTAS   '.                    
013000         05  FILLER              PIC X(36)                                
013100         VALUE 'WRONG STATUS. JOB MAY NOT BE STARTED'.                    
013200     03  FILLER REDEFINES W-FEL-3.                                        
013300         05  FEL-3 OCCURS 2      PIC X(36).                               
013400                                                                          
013500     03  W-FEL-4.                                                         
013600         05  FILLER              PIC X(30)                                
013700         VALUE 'TRYCK PF11 FÖR ATT STARTA JOBB'.                          
013800         05  FILLER              PIC X(30)                                
013900         VALUE 'PRESS PF11 TO START JOB       '.                          
014000     03  FILLER REDEFINES W-FEL-4.                                        
014100         05  FEL-4 OCCURS 2      PIC X(30).                               
014200                                                                          
014300     03  W-FEL-5.                                                         
014400         05  FILLER              PIC X(30)                                
014500         VALUE 'TRYCK PF23 FÖR ATT STARTA JOBB'.                          
014600         05  FILLER              PIC X(30)                                
014700         VALUE 'PRESS PF23 TO START JOB       '.                          
014800     03  FILLER REDEFINES W-FEL-5.                                        
014900         05  FEL-5 OCCURS 2      PIC X(30).                               
015000     EJECT                                                                
015100*                        ****    TP-AREOR                                 
015200 01  FILLER                      PIC X(16)   VALUE ' MFS-WS   '.          
015300     SKIP2                                                                
015400*01  MID -COPY W0I70901                                                   
015500     EJECT                                                                
015600*01  -COPY WMSGAREA                                                       
015700     EJECT                                                                
015800*    03  MOD -COPY W0O70901  -RED MSG-AREA.                               
015900     EJECT                                                                
016000*01  -COPY WMFSAREA.                                                      
016100     EJECT                                                                
016200******************************************************************        
016300*****                                                                     
016400*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016500*****                                                                     
016600 01  IMS-WS.                                                              
016700     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
016800     SKIP3                                                                
016900*****                    **** STATUS-KOD FRÅN IMS                         
017000     03  STATUS-WS               PIC X(2).                                
017100         88  SEGMENT-FINNS                   VALUE '  '.                  
017200         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
017300     SKIP3                                                                
017400     03  GODK-STATUSKODER.                                                
017500         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
017600     SKIP3                                                                
017700 01  SSA1                        PIC X(64).                               
017800 01  SSA2                        PIC X(64).                               
017900     EJECT                                                                
018000*                            IMS FUNKTIONSKODER                           
018100*01  -COPY W0003                                                          
018200     EJECT                                                                
018300*                            DLI INPUT-OUTPUT AREA                        
018400 01  DLI-IO-AREA.                                                         
018500     03  IO-AREA                 PIC X(100)  VALUE SPACE.                 
018600     SKIP3                                                                
018700*    03  WLJCLD01  -COPY WDP101  -RED IO-AREA.                            
018800     EJECT                                                                
018900*    03  WLJCLD11  -COPY WDP111  -RED IO-AREA.                            
019000     EJECT                                                                
019100*    03  WLJCLC12  -COPY WDP113  -RED IO-AREA.                            
019200     EJECT                                                                
019300*    03  WLJCLD12  -COPY WDP114  -RED IO-AREA.                            
019400     EJECT                                                                
019500 LINKAGE SECTION.                                                         
019600                                                                          
019700*01  -COPY W0009     -PRE MSG-                                            
019800     EJECT                                                                
019900*01  -COPY W0008     -PRE JCLA-                                           
020000         05  FILLER              PIC X.                                   
020100                                                                          
020200*01  -COPY W0008     -PRE JCLC-                                           
020300         05  FILLER              PIC X.                                   
020400     EJECT                                                                
020500*01  -COPY W0008     -PRE JCLD-                                           
020600         05  FILLER              PIC X.                                   
020700     EJECT                                                                
020800 PROCEDURE DIVISION USING MSG-PCB JCLA-PCB JCLC-PCB JCLD-PCB.             
020900 MAIN SECTION.                                                            
021000     ENTRY 'DLITCBL' USING MSG-PCB JCLA-PCB JCLC-PCB JCLD-PCB.            
021100                                                                          
021200     PERFORM IMS-GET-MSG                                                  
021300     IF SEGMENT-FINNS                                                     
021400       PERFORM A-INIT-SPARA-INPUT                                         
021500       PERFORM IMS-GET-6021-ROT                                           
021600       IF SEGMENT-FINNS                                                   
021700         IF MFS-UPDATE                                                    
021800           PERFORM B-TESTA-SECURITY                                       
021900           IF SECURITY-TEST = OK                                          
022000             PERFORM C-STARTA-JOB                                         
022100           END-IF                                                         
022200         ELSE                                                             
022300           IF W-IDTRANS = '0704' OR '0705'                                
022400                       OR '0707' OR '0708'                                
022500             MOVE FEL-5(IX) TO MOD-TEMFSFEL                               
022600           ELSE                                                           
022700             IF W-IDTRANS = '0701' OR '0702'                              
022800                         OR '0703' OR '0706'                              
022900               MOVE 'W0O70901' TO MFS-IDMOD                               
023000               MOVE '0709'     TO MOD-IDTRANS                             
023100             ELSE                                                         
023200               MOVE FEL-4 (IX) TO MOD-TEMFSFEL                            
023300             END-IF                                                       
023400           END-IF                                                         
023500         END-IF                                                           
023600       ELSE                                                               
023700         MOVE FEL-2 (IX) TO MOD-TEMFSFEL                                  
023800       END-IF                                                             
023900       IF EJ-MOD                                                          
024000         CONTINUE                                                         
024100       ELSE                                                               
024200         PERFORM IMS-INSERT-MSG                                           
024300       END-IF                                                             
024400     END-IF                                                               
024500                                                                          
024600     MOVE ZERO TO RETURN-CODE                                             
024700     GOBACK                                                               
024800     .                                                                    
024900     EJECT                                                                
025000 A-INIT-SPARA-INPUT SECTION.                                              
025100                                                                          
025200     IF MSG-DUBBLA-TRANSKODER                                             
025300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I70901                 
025400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS W-IDTRANS                        
025500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025600     ELSE                                                                 
025700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I70901                  
025800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS W-IDTRANS                        
025900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026000     END-IF                                                               
026100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026200                                                                          
026300     MOVE LOW-VALUE TO MSG-AREA                                           
026400                                                                          
026500     IF MID-IDRUTIN-IN = ALL '+'                                          
026600       IF MID-IDRUTIN-UT = ALL '+'                                        
026700         MOVE LOW-VALUE TO W-IDRUTIN-6011 W-IDRUTIN-6021                  
026800         MOVE MFS-RENSA-FAELT TO MOD-IDRUTIN-UT                           
026900       ELSE                                                               
027000         MOVE MID-IDRUTIN-UT TO MOD-IDRUTIN-UT W-IDRUTIN-6011             
027100                                               W-IDRUTIN-6021             
027200       END-IF                                                             
027300     ELSE                                                                 
027400       MOVE MID-IDRUTIN-IN TO MOD-IDRUTIN-UT W-IDRUTIN-6011               
027500                                             W-IDRUTIN-6021               
027600     END-IF                                                               
027700                                                                          
027800     IF MID-IDJOB-IN = ALL '+'                                            
027900       IF MID-IDJOB-UT = ALL '+'                                          
028000         MOVE LOW-VALUE TO W-IDJOB W-IDJOB1                               
028100         MOVE MFS-RENSA-FAELT TO MOD-IDJOB-UT                             
028200       ELSE                                                               
028300         MOVE MID-IDJOB-UT TO MOD-IDJOB-UT W-IDJOB W-IDJOB1               
028400       END-IF                                                             
028500     ELSE                                                                 
028600       MOVE MID-IDJOB-IN TO MOD-IDJOB-UT W-IDJOB W-IDJOB1                 
028700     END-IF                                                               
028800                                                                          
028900     IF EGEN-BILD                                                         
029000       MOVE 'W0O70901' TO MFS-IDMOD                                       
029100       MOVE '0709' TO MOD-IDTRANS                                         
029200     ELSE                                                                 
029300       STRING 'W' W-TRANS-SIFF-1 'O' W-TRANS-SIFF-2                       
029400                  W-TRANS-SIFF-3 W-TRANS-SIFF-4 '02'                      
029500              DELIMITED BY SIZE INTO W-MODNAMN                            
029600       MOVE W-MODNAMN TO MFS-IDMOD                                        
029700       MOVE MFS-IDTRANS TO MOD-IDTRANS                                    
029800     END-IF                                                               
029900                                                                          
030000     IF ENGLISH-TEXT                                                      
030100       MOVE 2 TO IX                                                       
030200       MOVE 'N' TO MFS-KDHUVOMR                                           
030300     ELSE                                                                 
030400       MOVE 1 TO IX                                                       
030500     END-IF                                                               
030600                                                                          
030700     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
030800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
030900                             MOD-TEMFSINF                                 
031000                             MOD-IDRUTIN-IN                               
031100                             MOD-IDJOB-IN                                 
031200     .                                                                    
031300     EJECT                                                                
031400 B-TESTA-SECURITY SECTION.                                                
031500                                                                          
031600     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
031700     PERFORM IMS-GET-6021-KNTL                                            
031800     IF SEGMENT-FINNS                                                     
031900       MOVE OK TO SECURITY-TEST                                           
032000     ELSE                                                                 
032100       PERFORM IMS-GET-6011-KNTL                                          
032200       IF SEGMENT-FINNS                                                   
032300         MOVE OK TO SECURITY-TEST                                         
032400       ELSE                                                               
032500         PERFORM IMS-GET-6001-KNTL                                        
032600         IF SEGMENT-FINNS                                                 
032700           MOVE OK TO SECURITY-TEST                                       
032800         ELSE                                                             
032900           MOVE FEL-1 (IX) TO MOD-TEMFSFEL                                
033000           MOVE FEL TO SECURITY-TEST                                      
033100         END-IF                                                           
033200       END-IF                                                             
033300     END-IF                                                               
033400     .                                                                    
033500     EJECT                                                                
033600 C-STARTA-JOB SECTION.                                                    
033700                                                                          
033800     MOVE W-IDJOB TO STARTAT-JOBB-S STARTAT-JOBB-E                        
033900     PERFORM IMS-GET-HOLD-6011-JOB                                        
034000     IF JOB-KDTRSTAT = 1 OR 2 OR 3 OR 5                                   
034100       IF JOB-KDTRSTAT = 1 OR 2                                           
034200         ADD 2 TO JOB-KDTRSTAT                                            
034300       END-IF                                                             
034400       IF JOB-KDTRSTAT = 5                                                
034500         MOVE 3 TO JOB-KDTRSTAT                                           
034600       END-IF                                                             
034700       ACCEPT JOB-TIUPPDAT FROM DATE                                      
034800       ACCEPT JOB-TIUPPTID FROM TIME                                      
034900       PERFORM IMS-REPLACE                                                
035000       OPEN OUTPUT INTRDR                                                 
035100                                                                          
035200       PERFORM IMS-GET-6021-JCL                                           
035300       PERFORM UNTIL SEGMENT-SAKNAS                                       
035400         WRITE INTERNAL-READER FROM JCL-TEJCL                             
035500         PERFORM IMS-GET-6021-JCL                                         
035600       END-PERFORM                                                        
035700                                                                          
035800       MOVE RETT (IX) TO MOD-TEMFSINF                                     
035900       CLOSE INTRDR                                                       
036000     ELSE                                                                 
036100       MOVE FEL-3 (IX) TO MOD-TEMFSFEL                                    
036200     END-IF                                                               
036300     .                                                                    
036400     EJECT                                                                
036500* IMS SEKTIONER                                                           
036600                                                                          
036700 IMS-GET-MSG SECTION.                                                     
036800                                                                          
036900     MOVE '  QC' TO GODK-STATUSKODER                                      
037000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
037100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037200     PERFORM IMS-STATUSKONTROLL                                           
037300     .                                                                    
037400     SKIP3                                                                
037500 IMS-INSERT-MSG SECTION.                                                  
037600                                                                          
037700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
037800     MOVE SPACE TO GODK-STATUSKODER                                       
037900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
038000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038100     PERFORM IMS-STATUSKONTROLL                                           
038200     .                                                                    
038300     EJECT                                                                
038400 IMS-GET-6001-KNTL SECTION.                                               
038500                                                                          
038600     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6001-X ')'                   
038700            DELIMITED BY SIZE INTO SSA1                                   
038800     STRING 'WLJCLA11(IDUSER   =' W-IDUSER-X ')'                          
038900            DELIMITED BY SIZE INTO SSA2                                   
039000     MOVE '  GE' TO GODK-STATUSKODER                                      
039100     CALL CBLTDLI USING GU JCLA-PCB DLI-IO-AREA SSA1 SSA2                 
039200     MOVE JCLA-STATUS-CODE TO STATUS-WS                                   
039300     PERFORM IMS-STATUSKONTROLL                                           
039400     .                                                                    
039500     SKIP3                                                                
039600 IMS-GET-6011-KNTL SECTION.                                               
039700                                                                          
039800     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6011-X ')'                   
039900            DELIMITED BY SIZE INTO SSA1                                   
040000     STRING 'WLJCLA11(IDUSER   =' W-IDUSER-X ')'                          
040100            DELIMITED BY SIZE INTO SSA2                                   
040200     MOVE '  GE' TO GODK-STATUSKODER                                      
040300     CALL CBLTDLI USING GU JCLA-PCB DLI-IO-AREA SSA1 SSA2                 
040400     MOVE JCLA-STATUS-CODE TO STATUS-WS                                   
040500     PERFORM IMS-STATUSKONTROLL                                           
040600     .                                                                    
040700     EJECT                                                                
040800 IMS-GET-HOLD-6011-JOB SECTION.                                           
040900                                                                          
041000     STRING 'WLJCLC01(WDP101KY =' W-WDP101KY-6011-X ')'                   
041100            DELIMITED BY SIZE INTO SSA1                                   
041200     STRING 'WLJCLC12(IDJOB    =' W-IDJOB1-X ')'                          
041300            DELIMITED BY SIZE INTO SSA2                                   
041400     MOVE '  ' TO GODK-STATUSKODER                                        
041500     CALL CBLTDLI USING GHU JCLC-PCB DLI-IO-AREA SSA1 SSA2                
041600     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
041700     PERFORM IMS-STATUSKONTROLL                                           
041800     .                                                                    
041900     SKIP3                                                                
042000 IMS-REPLACE SECTION.                                                     
042100                                                                          
042200     MOVE '  ' TO GODK-STATUSKODER                                        
042300     CALL CBLTDLI USING REPL JCLC-PCB DLI-IO-AREA                         
042400     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
042500     PERFORM IMS-STATUSKONTROLL                                           
042600     .                                                                    
042700     EJECT                                                                
042800 IMS-GET-6021-ROT SECTION.                                                
042900                                                                          
043000     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-6021-X ')'                   
043100            DELIMITED BY SIZE INTO SSA1                                   
043200     MOVE '  GE' TO GODK-STATUSKODER                                      
043300     CALL CBLTDLI USING GU JCLD-PCB DLI-IO-AREA SSA1                      
043400     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
043500     PERFORM IMS-STATUSKONTROLL                                           
043600     .                                                                    
043700     SKIP3                                                                
043800 IMS-GET-6021-KNTL SECTION.                                               
043900                                                                          
044000     STRING 'WLJCLD11(IDUSER   =' W-IDUSER-X ')'                          
044100            DELIMITED BY SIZE INTO SSA1                                   
044200     MOVE '  GE' TO GODK-STATUSKODER                                      
044300     CALL CBLTDLI USING GNP JCLD-PCB DLI-IO-AREA SSA1                     
044400     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
044500     PERFORM IMS-STATUSKONTROLL                                           
044600     .                                                                    
044700     SKIP3                                                                
044800 IMS-GET-6021-JCL SECTION.                                                
044900                                                                          
045000     MOVE 'WLJCLD12 ' TO SSA1                                             
045100     MOVE '  GE' TO GODK-STATUSKODER                                      
045200     CALL CBLTDLI USING GNP JCLD-PCB DLI-IO-AREA SSA1                     
045300     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
045400     PERFORM IMS-STATUSKONTROLL                                           
045500     .                                                                    
045600     EJECT                                                                
045700 IMS-STATUSKONTROLL SECTION.                                              
045800     SET STATUS-IX TO 1                                                   
045900     SEARCH GODK-STATUS                                                   
046000       AT END                                                             
046100         CALL FELLOG                                                      
046200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
046300         CONTINUE                                                         
046400     END-SEARCH.                                                          
