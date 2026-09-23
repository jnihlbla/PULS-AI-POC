000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2215600.                                    
000300*              PROGRAM CONVERTED BY                                       
000400*              COBOL CONVERSION AID PO 5785-ABJ                           
000500*              CONVERSION DATE 05/25/91 19:26:35.                         
000600*AUTHOR.                     IDK, GÖTEBORG.                               
000700*DATE-WRITTEN.               JUNI 1979.                                   
000800*    SKIP3                                                                
000900*REMARKS.                                                                 
001000                                                                          
001100*    FUNKTION.                                                            
001200*        PROGRAMMMET LÄSER IGENOM LEVERANSPLANEREGISTRET I FYSISK         
001300*        SEKVENS. ÖVERFLÖDIGA SEGMENT DELETAS.DESSUTOM RENSAS             
001400*        FÖRSENADE LEVERANSBESKED OCH FÄRDIGRAPPORTERADE                  
001500*        INLEVERANSER/AVROP.                                              
001600                                                                          
001700*    SUBPROGRAM.                                                          
001800*        W2215610    SUBPROGRAM SOM SKÖTER SAMTLIGA IMS-CALL.             
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200 FILE-CONTROL.                                                            
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800*    -COPY WY2000W3                                                       
002900     SKIP3                                                                
003000*    -COPY WY2000W1                                                       
003100     SKIP3                                                                
003330 77  W-CHKP-RAKNARE              PIC S9(5)   VALUE +0    COMP-3.          
003340 77  W-CHKP-MAX                  PIC S9(5)   VALUE +900  COMP-3.          
003350 77  CHKP-ID                     PIC X(8)    VALUE 'W2215600'.            
003360 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
003370 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
003380 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
003390 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
003400*                                                                         
003500*--------------------------------------- KONSTANTER                       
003600 01  KONSTANTER.                                                          
003700     05  JA                  PIC X       VALUE 'J'.                       
003800     05  NEJ                 PIC X       VALUE 'N'.                       
003900     SKIP3                                                                
004000*--------------------------------------- FLAGGOR                          
004100 01  FLAGGOR.                                                             
004200     05  FL-WDD904-FINNS     PIC X       VALUE 'N'.                       
004300     SKIP3                                                                
004400*--------------------------------------- ALLMÄNNA ARBETSAREOR             
004500 01  W.                                                                   
004600     05  W-TIAAVV-AKTUELL    PIC 9(4).                                    
004700     05  FILLER              REDEFINES W-TIAAVV-AKTUELL.                  
004800         10  W-TIAA-AKTUELL  PIC 9(2).                                    
004900         10  W-TIVV-AKTUELL  PIC 9(2).                                    
005000     05  W-TIAAVV-BORTTAG    PIC S9(5)               COMP-3.              
005100     05  W-TIAAVV-BORTTAG-AVROP  PIC S9(5)           COMP-3.              
005200     05  W-JFWDATUM-BORTTAG  PIC S9(5)               COMP-3.              
005300     05  W-JFWDATUM-FORSENING PIC S9(5)              COMP-3.              
005400     05  WS-TIAAVVD.                                                      
005500         10  WS-TIAAVV       PIC S9(4).                                   
005600         10  FILLER          PIC S9(1).                                   
005700     05  W-IDARTNR           PIC S9(9)               COMP-3.              
005800     05  W-IDDC              PIC X(2).                                    
005810     05  W-IDLEVNR           PIC X(5).                                    
005900     05  W-AAVV              PIC S9(5).                                   
006000     05  W-AAVV-X  REDEFINES W-AAVV.                                      
006100         10  W-NOLL          PIC 9.                                       
006200         10  W-AA            PIC 9(2).                                    
006300         10  W-VV            PIC 9(2).                                    
006400     05  DAGENS-DATUM-PACK   PIC S9(7)               COMP-3.              
006500     05  DAGENSDATUM         PIC 9(6).                                    
006600     05  FILLER        REDEFINES DAGENSDATUM.                             
006700         10  DAGENS-AAR      PIC 9(2).                                    
006800         10  DAGENS-MANAD    PIC 9(2).                                    
006900         10  DAGENS-DAG      PIC 9(2).                                    
007000 01  SPAR-AREA.                                                           
007100     05  SPAR-IDARTNR        PIC S9(9)               COMP-3.              
007110     05  SPAR-IDDC           PIC X(2).                                    
007200     05  SPAR-IDLEVNR        PIC X(5).                                    
007300     05  SPAR-KVBR           PIC S9(7)               COMP-3.              
007500     05  SPAR-KVBEST-PL      PIC S9(7)               COMP-3.              
007600     05  SPAR-TIAVROP-INL    PIC S9(5)               COMP-3.              
007700     05  SPAR-TIAVROP-AVS    PIC S9(5)               COMP-3.              
007800     05  SPAR-IDLOPNRM-PL    PIC S9(9)               COMP-3.              
007900     05  SPAR-KVAVROP-AVB    PIC S9(7)               COMP-3.              
008000     05  SPAR-TILEVBSK-AVS   PIC S9(7)               COMP-3.              
008100     SKIP3                                                                
008101*01  -COPY WWDCKONS                                                       
008110     EJECT                                                                
008200*--------------------------------------- DYNAMISKA SUBPROGRAM             
008300 01  DYNAMISKA-SUBPROGRAM.                                                
008400     05  W2215610            PIC X(8)    VALUE 'W2215610'.                
008500     05  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
008600     05  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
008700     05  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
008710     03  CBLTDLI             PIC X(8)        VALUE 'CBLTDLI '.            
008720     03  FELLOG              PIC X(8)        VALUE 'FELLOG  '.            
008800     EJECT                                                                
008900*--------------------------------------- BYTES-ARTIKEL                    
009000 01  FILLER                  PIC X(16)   VALUE 'BYTES-ARTIKEL'.           
009100 01  TEST-IDARTNR            PIC 9(9)    COMP-3.                          
009200*01  FILLER  -COPY WWBYT02   -RED TEST-IDARTNR.                           
009300     EJECT                                                                
009400*--------------------------------------- PARAMETRAR TILL DATKORT          
009500                                                                          
009600 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22156'.                  
009700                                                                          
009800 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
009900                                                                          
010000*01  -COPY WDATKORT                                                       
010100                                                                          
010200     EJECT                                                                
010300*--------------------------------------- PARAMETRAR TILL W009VADD         
010400                                                                          
010500 01  W009VADD-DATUM          PIC S9(5)               COMP-3.              
010600                                                                          
010700 01  W009VADD-ANTAL          PIC S9(3)               COMP-3.              
010800     EJECT                                                                
010900*----------------------------------------PARAMETRAR TILL WDATKONV         
011000 01  FILLER                  PIC X(16)   VALUE 'WDATKONV        '.        
011100*01   -COPY WDATAREA.                                                     
011200     EJECT                                                                
011300*--------------------------------------- LÄNKAREA FÖR CALL MOT            
011400*                                        LEVERANSPLANEREGISTRET           
011500                                                                          
011600*01  AREA -COPY W221L561 -PRE LINK1-                                      
011700     EJECT                                                                
011800*-------------------------------------- LÄNKAREA FÖR VISSA BORTTAG        
011900*01  AREA -COPY W221L561 -PRE LINK2-                                      
012000     EJECT                                                                
012010 01  IMS-WS.                                                              
012020     03  FILLER              PIC X(8)        VALUE 'IMS-WS  '.            
012030                                                                          
012040*                            *** STATUSKOD FRÅN IMS                       
012050     03 STATUS-WS            PIC XX.                                      
012060         88  SEGMENT-FINNS                   VALUE '  '.                  
012070         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
012080                                                                          
012081     03  SSA1                PIC X(60).                                   
012085     SKIP3                                                                
012086     03  GODK-STATUSKODER.                                                
012087         10  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.           
012090 01  W-WDD901KY-X.                                                        
012091     03  W-IDARTNR-D9        PIC S9(9)               COMP-3.              
012092     03  W-IDDC-D9           PIC X(2).                                    
012093                                                                          
012094*01  -COPY W0003                                                          
012095     EJECT                                                                
012096                                                                          
012098 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDD901'.          
012099 01  DLI-IO-WDD901.                                                       
012100*    03  -COPY WDD901    -PRE WDD901-                                     
012101     EJECT                                                                
012110 LINKAGE SECTION.                                                         
012120*01  -COPY W0009 -PRE MSG-                                                
012200     SKIP3                                                                
012300*01  -COPY W0008 -PRE INLB-L-                                             
012400     05 FILLER               PIC X.                                       
012500     SKIP3                                                                
012600*01  -COPY W0008 -PRE INLB-U-                                             
012700     05 FILLER               PIC X.                                       
012800     SKIP3                                                                
012900*01  -COPY W0008 -PRE W6INLA-                                             
013000     05 FILLER               PIC X.                                       
013100     EJECT                                                                
013200 PROCEDURE DIVISION  USING MSG-PCB INLB-L-PCB INLB-U-PCB                  
013210                           W6INLA-PCB.                                    
013300     ENTRY 'DLITCBL' USING MSG-PCB INLB-L-PCB INLB-U-PCB                  
013310                           W6INLA-PCB.                                    
013400                                                                          
013500     PERFORM A-INITIERA                                                   
013600                                                                          
013700     SET  LINK1-LAES-ROT  TO TRUE                                         
013800     CALL W2215610   USING LINK1-AREA INLB-L-PCB INLB-U-PCB               
013900                                                 W6INLA-PCB               
014000     SKIP3                                                                
014100     PERFORM UNTIL LINK1-POST-SAKNAS                                      
014200                                                                          
014300       IF  INLB-L-SEG-NAME-FB = 'WLINLB01'                                
014400         PERFORM  B-BEHANDLA-WDD901                                       
014500       ELSE                                                               
014600         IF  INLB-L-SEG-NAME-FB = 'WLINLB11'                              
014700           PERFORM  C-BEHANDLA-WDD902-LEV                                 
014800         ELSE                                                             
015200           IF  INLB-L-SEG-NAME-FB = 'WLINLB22'                            
015300             PERFORM  G-BEHANDLA-WDD904-SPEC                              
015400           ELSE                                                           
015500             IF  INLB-L-SEG-NAME-FB = 'WLINLB23'                          
015600               PERFORM E-BEHANDLA-WDD905-AVROP-AVBOK                      
015700             ELSE                                                         
015800               IF  INLB-L-SEG-NAME-FB = 'WLINLB24'                        
015900                 PERFORM F-BEHANDLA-WDD924-TILEVBSK                       
016000               ELSE                                                       
016100                 IF  INLB-L-SEG-NAME-FB = 'WLINLB25'                      
016200                   PERFORM H-BEHANDLA-WDD925-IDLEVBSK                     
016300                 ELSE                                                     
016400                                                                          
016500                   SET  LINK1-LAES-NASTA-UNDER-ROT  TO TRUE               
016600                   CALL W2215610 USING LINK1-AREA INLB-L-PCB              
016700                                                  INLB-U-PCB              
016800                                                  W6INLA-PCB              
016900                 END-IF                                                   
017000               END-IF                                                     
017100             END-IF                                                       
017200           END-IF                                                         
017400         END-IF                                                           
017500       END-IF                                                             
017600       IF  LINK1-POST-SAKNAS                                              
017601                                                                          
017610         IF W-CHKP-RAKNARE          >  W-CHKP-MAX                         
017611            DISPLAY 'CHECKPOINT'                                          
017620            PERFORM IMS-CHECKPOINT                                        
017630            MOVE ZERO               TO W-CHKP-RAKNARE                     
017631            MOVE SPAR-IDARTNR       TO W-IDARTNR-D9                       
017632            MOVE SPAR-IDDC          TO W-IDDC-D9                          
017633            PERFORM IMS-GU-WDD901                                         
017640         END-IF                                                           
017700                                                                          
017800         SET  LINK1-LAES-ROT  TO TRUE                                     
017900         CALL W2215610  USING LINK1-AREA INLB-L-PCB                       
018000                                         INLB-U-PCB W6INLA-PCB            
018100       END-IF                                                             
018200     END-PERFORM                                                          
018300     SKIP1                                                                
018400     MOVE ZERO  TO RETURN-CODE                                            
018500     GOBACK                                                               
018600     .                                                                    
018700     EJECT                                                                
018800******************************************************************        
018900*                                                                *        
019000*    INITIERING                                                  *        
019100*    LÄS DATUMKORT                                               *        
019200*                                                                *        
019300******************************************************************        
019400                                                                          
019500 A-INITIERA SECTION.                                                      
019600                                                                          
019610     PERFORM IMS-RESTART                                                  
019620     MOVE ZERO  TO W-CHKP-RAKNARE                                         
019630                                                                          
019700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
019800     MOVE ZERO TO W-NOLL                                                  
019900     MOVE D-AAR TO W-TIAA-AKTUELL                                         
020000     MOVE D-VECKA TO W-TIVV-AKTUELL                                       
020100     MOVE W-TIAAVV-AKTUELL TO W009VADD-DATUM                              
020200     MOVE -6 TO W009VADD-ANTAL                                            
020300     CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                    
020400     MOVE W009VADD-DATUM TO W-TIAAVV-BORTTAG                              
020500                                                                          
020600     MOVE ZERO      TO W-IDARTNR                                          
020700     MOVE WC-CDC-SE TO W-IDDC                                             
020710     MOVE SPACE     TO W-IDLEVNR                                          
020800*                                                                         
020900     MOVE W-TIAAVV-AKTUELL TO  W009VADD-DATUM                             
021000     MOVE -7 TO W009VADD-ANTAL                                            
021100     CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                    
021200     MOVE W009VADD-DATUM TO W-TIAAVV-BORTTAG-AVROP                        
021300     MOVE D-AAR              TO DAGENS-AAR                                
021400     MOVE D-MAANAD           TO DAGENS-MANAD                              
021500     MOVE D-DAG              TO DAGENS-DAG                                
021600     MOVE DAGENSDATUM        TO DAGENS-DATUM-PACK                         
021700**************** * * * * * * * * * * * ******************                 
021800     DISPLAY 'W-TIAAVV-AKTUELL       = ' W-TIAAVV-AKTUELL                 
021900     DISPLAY 'W-TIAAVV-BORTTAG-AVROP = ' W-TIAAVV-BORTTAG-AVROP           
022000     .                                                                    
022100**************** * * * * * * * * * * * ******************                 
022200     EJECT                                                                
022300 B-BEHANDLA-WDD901 SECTION.                                               
022400     SKIP1                                                                
022500     MOVE LOW-VALUE          TO SPAR-AREA                                 
022600     MOVE ZERO TO SPAR-KVBEST-PL                                          
022700     MOVE NEJ TO FL-WDD904-FINNS                                          
022800     MOVE LINK1-IDARTNR      TO SPAR-IDARTNR                              
022900     MOVE LINK1-IDDC         TO SPAR-IDDC                                 
023000                                                                          
023100     SET  LINK1-LAES-NASTA-UNDER-ROT  TO TRUE                             
023200     CALL W2215610  USING LINK1-AREA INLB-L-PCB                           
023300                                     INLB-U-PCB W6INLA-PCB                
023400     .                                                                    
023500     EJECT                                                                
023600 C-BEHANDLA-WDD902-LEV SECTION.                                           
023700     SKIP1                                                                
023800     MOVE LINK1-IDLEVNR  TO SPAR-IDLEVNR                                  
023900     MOVE LINK1-KVBR     TO SPAR-KVBR                                     
024000     MOVE NEJ TO FL-WDD904-FINNS                                          
024100                                                                          
024200     SET  LINK1-LAES-NASTA-UNDER-ROT  TO TRUE                             
024300     CALL W2215610 USING LINK1-AREA INLB-L-PCB INLB-U-PCB                 
024400                                               W6INLA-PCB                 
024500     SKIP1                                                                
024600     IF  LINK1-POST-FINNS                                                 
024700     AND INLB-L-SEG-NAME-FB NOT = 'WLINLB11'                              
024800       CONTINUE                                                           
024900     ELSE                                                                 
025000       IF  SPAR-KVBR NOT > ZERO                                           
025100         MOVE LINK1-AREA    TO LINK2-AREA                                 
025200         MOVE SPAR-IDARTNR   TO LINK2-IDARTNR                             
025210         MOVE SPAR-IDDC      TO LINK2-IDDC                                
025300         MOVE SPAR-IDLEVNR   TO LINK2-IDLEVNR                             
025400                                                                          
025500         SET  LINK2-GHU-WDD902  TO TRUE                                   
025600         CALL W2215610 USING LINK2-AREA INLB-L-PCB                        
025700                                            INLB-U-PCB W6INLA-PCB         
025800         SET  LINK2-DELETE  TO TRUE                                       
025900         CALL W2215610 USING LINK2-AREA INLB-L-PCB                        
026000                                            INLB-U-PCB W6INLA-PCB         
026010         ADD +1 TO W-CHKP-RAKNARE                                         
026100       END-IF                                                             
026200     END-IF                                                               
026300     .                                                                    
026400     EJECT                                                                
030100 G-BEHANDLA-WDD904-SPEC SECTION.                                          
030200     SKIP1                                                                
030300     MOVE LINK1-KVBEST-PL  TO SPAR-KVBEST-PL                              
030400                                                                          
030500     MOVE JA TO FL-WDD904-FINNS                                           
030600                                                                          
030700     SET  LINK1-LAES-NASTA-UNDER-ROT  TO TRUE                             
030800     CALL W2215610 USING LINK1-AREA INLB-L-PCB INLB-U-PCB                 
030900                                               W6INLA-PCB                 
031000     .                                                                    
031100     EJECT                                                                
031200 E-BEHANDLA-WDD905-AVROP-AVBOK SECTION.                                   
031300     SKIP1                                                                
031400     MOVE LINK1-TIAVROP-INL  TO SPAR-TIAVROP-INL                          
031500     MOVE LINK1-TIAVROP-AVS  TO SPAR-TIAVROP-AVS                          
031600     SKIP1                                                                
031700     MOVE SPAR-IDARTNR TO TEST-IDARTNR                                    
031800                                                                          
031900     MOVE LINK1-TIAVROP-INL        TO TMP1-YYWW                           
032000     MOVE W-TIAAVV-BORTTAG-AVROP   TO TMP2-YYWW                           
032100     PERFORM WY2000P3                                                     
032200     IF  LINK1-KDAVROP = 9                                                
032300     AND (  TMP1-YYWW < TMP2-YYWW  )                                      
032400                                                                          
032500     OR  (  LINK1-KDAVROP    = 2                                          
032600     AND    SPAR-KVBR        = ZERO                                       
032700     AND    SPAR-IDLEVNR NOT = '1002 '                                    
032800     AND   (SPAR-KVBEST-PL   = ZERO  OR  FL-WDD904-FINNS = NEJ)           
032900     AND    NOT BYT02-RENOV    )                                          
033000                                                                          
033100       SET  LINK1-LAES-NASTA-UNDER-ROT  TO TRUE                           
033200       CALL W2215610 USING LINK1-AREA INLB-L-PCB                          
033300                                          INLB-U-PCB W6INLA-PCB           
033400       IF  INLB-L-SEG-NAME-FB = 'WLINLB31'                                
033500         PERFORM UNTIL ( LINK1-POST-SAKNAS                                
033600                    OR   INLB-L-SEG-NAME-FB  NOT =  'WLINLB31' )          
033700           MOVE LINK1-IDLOPNRM-PL TO SPAR-IDLOPNRM-PL                     
033800           MOVE LINK1-KVAVROP-AVB TO SPAR-KVAVROP-AVB                     
033900                                                                          
034000           SET  LINK1-LAES-INL-PARTI  TO TRUE                             
034100           CALL W2215610 USING LINK1-AREA INLB-L-PCB                      
034200                                          INLB-U-PCB W6INLA-PCB           
034300           IF  LINK1-POST-SAKNAS                                          
034400           OR  LINK1-FLKLAR = JA                                          
034500             MOVE SPAR-IDARTNR     TO LINK1-IDARTNR                       
034510             MOVE SPAR-IDDC        TO LINK1-IDDC                          
034600             MOVE SPAR-IDLEVNR     TO LINK1-IDLEVNR                       
034700             MOVE SPAR-TIAVROP-AVS TO LINK1-TIAVROP-AVS                   
034800             MOVE SPAR-IDLOPNRM-PL TO LINK1-IDLOPNRM-PL                   
034900                                                                          
035000             SET  LINK1-GHU-WDD906  TO TRUE                               
035100             CALL W2215610 USING LINK1-AREA INLB-L-PCB                    
035200                                            INLB-U-PCB                    
035300                                            W6INLA-PCB                    
035400             SET  LINK1-DELETE  TO TRUE                                   
035500             CALL W2215610 USING LINK1-AREA INLB-L-PCB                    
035600                                            INLB-U-PCB                    
035700                                            W6INLA-PCB                    
035710             ADD +1 TO W-CHKP-RAKNARE                                     
035800           END-IF                                                         
035900                                                                          
036000           SET  LINK1-LAES-NASTA-UNDER-ROT  TO TRUE                       
036100           CALL W2215610 USING LINK1-AREA INLB-L-PCB                      
036200                                          INLB-U-PCB W6INLA-PCB           
036300         END-PERFORM                                                      
036400       ELSE                                                               
036500         MOVE LINK1-AREA       TO LINK2-AREA                              
036600         MOVE SPAR-IDARTNR     TO LINK2-IDARTNR                           
036610         MOVE SPAR-IDDC        TO LINK2-IDDC                              
036700         MOVE SPAR-IDLEVNR     TO LINK2-IDLEVNR                           
036800         MOVE SPAR-TIAVROP-AVS TO LINK2-TIAVROP-AVS                       
036900                                                                          
037000         SET  LINK2-GHU-WDD905  TO TRUE                                   
037100         CALL W2215610 USING LINK2-AREA INLB-L-PCB                        
037200                                        INLB-U-PCB W6INLA-PCB             
037300         SET  LINK2-DELETE  TO TRUE                                       
037400         CALL W2215610 USING LINK2-AREA INLB-L-PCB                        
037500                                        INLB-U-PCB W6INLA-PCB             
037510         ADD +1 TO W-CHKP-RAKNARE                                         
037600       END-IF                                                             
037700     ELSE                                                                 
037800       MOVE LINK1-TIAVROP-INL        TO TMP1-YYWW                         
037900       MOVE W-TIAAVV-BORTTAG-AVROP   TO TMP2-YYWW                         
038000       PERFORM WY2000P3                                                   
038100       IF  LINK1-KVAVROP <= ZERO                                          
038200       AND TMP1-YYWW  <  TMP2-YYWW                                        
038300         MOVE LINK1-AREA       TO LINK2-AREA                              
038400         MOVE SPAR-IDARTNR     TO LINK2-IDARTNR                           
038410         MOVE SPAR-IDDC        TO LINK2-IDDC                              
038500         MOVE SPAR-IDLEVNR     TO LINK2-IDLEVNR                           
038600         MOVE SPAR-TIAVROP-AVS TO LINK2-TIAVROP-AVS                       
038700* ----                                                                    
038800         DISPLAY 'WDD905'                                                 
038900                 ' SPAR-IDARTNR     = ' SPAR-IDARTNR                      
038910                 ' SPAR-IDDC        = ' SPAR-IDDC                         
039000                 ' SPAR-IDLEVNR     = ' SPAR-IDLEVNR                      
039100                 ' SPAR-TIAVROP-AVS = ' SPAR-TIAVROP-AVS                  
039200         DISPLAY ' -"-  '                                                 
039300                 ' LINK2-KVAVROP    = ' LINK2-KVAVROP                     
039400* ------                                                                  
039500         SET  LINK2-GHU-WDD905  TO TRUE                                   
039600         CALL W2215610 USING LINK2-AREA INLB-L-PCB INLB-U-PCB             
039700                                        W6INLA-PCB                        
039800         SET  LINK2-DELETE  TO TRUE                                       
039900         CALL W2215610 USING LINK2-AREA INLB-L-PCB INLB-U-PCB             
040000                                        W6INLA-PCB                        
040010         ADD +1 TO W-CHKP-RAKNARE                                         
040100       END-IF                                                             
040200                                                                          
040300       SET  LINK1-LAES-NASTA-UNDER-ROT  TO TRUE                           
040400       CALL W2215610 USING LINK1-AREA INLB-L-PCB INLB-U-PCB               
040500                                      W6INLA-PCB                          
040600     END-IF                                                               
040700     .                                                                    
040800     EJECT                                                                
040900 F-BEHANDLA-WDD924-TILEVBSK SECTION.                                      
041000     SKIP1                                                                
041100******************************************************************        
041200*                                                                *        
041300*    BEHANDLA LEVERANSBESKED                                     *        
041400*    UPPDATERA FLSENLEV PÅ LEVERANSBESKED VARS BERÄKNADE         *        
041500*    INLEVERANSVECKA ÄR UPPNÅDD                                  *        
041600*    OM NÅGOT LEVERANSBESKED ÄR FÖRSENAT MED MER ÄN 6 VECKOR,    *        
041700*    BORTTAGES ALL LEVERANSBESKED PÅ DENNA ARTIKEL/LEVERANTÖR    *        
041800*                                                                *        
041900*    EFTERSOM VISSA SEGMENT INNEHÅLLER TVÅ LEVERANSBESKED (NYTT  *        
042000*    FR O M MARS 1990 - TVÅ KVANTITETER) TAS HÄNSYN TILL DETTA.  *        
042100*    SEGMENT DELETAS ENDAST OM BÄGGE LEVERANSBESKEDEN ÄR NOLLADE.*        
042200*                                                                *        
042300******************************************************************        
042400                                                                          
042500     PERFORM UNTIL LINK1-POST-SAKNAS                                      
042600             OR    INLB-L-SEG-NAME-FB NOT =  'WLINLB24'                   
042700                                                                          
042800       MOVE LINK1-TILEVBSK-AVS TO SPAR-TILEVBSK-AVS                       
042900                                                                          
043000       IF  SPAR-IDARTNR = W-IDARTNR                                       
043100       AND SPAR-IDDC    = W-IDDC                                          
043110       AND SPAR-IDLEVNR = W-IDLEVNR                                       
043200         MOVE SPAR-IDARTNR      TO LINK1-IDARTNR                          
043210         MOVE SPAR-IDDC         TO LINK1-IDDC                             
043300         MOVE SPAR-IDLEVNR      TO LINK1-IDLEVNR                          
043400         MOVE SPAR-TILEVBSK-AVS TO LINK1-TILEVBSK-AVS                     
043500                                                                          
043600         SET  LINK1-GHU-WDD924  TO TRUE                                   
043700         CALL W2215610 USING LINK1-AREA INLB-L-PCB                        
043800                                        INLB-U-PCB W6INLA-PCB             
043900         SET  LINK1-DELETE  TO TRUE                                       
044000         CALL W2215610 USING LINK1-AREA INLB-L-PCB                        
044100                                        INLB-U-PCB W6INLA-PCB             
044110         ADD +1 TO W-CHKP-RAKNARE                                         
044200       ELSE                                                               
044300         IF  LINK1-KVAVIS-BSKKVARC1 > 0                                   
044400         AND LINK1-KVAVIS-BSKKVARC2 > 0                                   
044500           PERFORM FC-BEH-DUBBLA-LEVERANSBESKED                           
044600         ELSE                                                             
044700           IF  LINK1-FLSENLEVC1 = JA                                      
044800           OR  LINK1-FLSENLEVC2 = JA                                      
044900             PERFORM FA-BERAKNA-JFWDATUM-BORTTAG                          
045000             MOVE W-JFWDATUM-BORTTAG   TO TMP1-YYWW                       
045100             MOVE W-TIAAVV-BORTTAG     TO TMP2-YYWW                       
045200             PERFORM WY2000P3                                             
045300             IF  TMP1-YYWW < TMP2-YYWW                                    
045400               MOVE SPAR-IDARTNR      TO W-IDARTNR                        
045410               MOVE SPAR-IDDC         TO W-IDDC                           
045500               MOVE SPAR-IDLEVNR      TO W-IDLEVNR                        
045600               MOVE SPAR-IDARTNR      TO LINK1-IDARTNR                    
045700               MOVE SPAR-IDLEVNR      TO LINK1-IDLEVNR                    
045800               MOVE SPAR-TILEVBSK-AVS TO LINK1-TILEVBSK-AVS               
045900                                                                          
046000               SET  LINK1-GHU-WDD924  TO TRUE                             
046100               CALL W2215610 USING LINK1-AREA INLB-L-PCB                  
046200                                            INLB-U-PCB W6INLA-PCB         
046300                                                                          
046400               SET  LINK1-DELETE  TO TRUE                                 
046500               CALL W2215610 USING LINK1-AREA INLB-L-PCB                  
046600                                            INLB-U-PCB W6INLA-PCB         
046610               ADD +1 TO W-CHKP-RAKNARE                                   
046700             END-IF                                                       
046800           ELSE                                                           
046900             PERFORM FB-BERAKNA-JFWDATUM-FORSENING                        
047000             MOVE W-JFWDATUM-FORSENING   TO TMP1-YYWW                     
047100             MOVE W-TIAAVV-AKTUELL       TO TMP2-YYWW                     
047200             PERFORM WY2000P3                                             
047300             IF TMP1-YYWW <= TMP2-YYWW                                    
047400               MOVE SPAR-IDARTNR      TO LINK1-IDARTNR                    
047410               MOVE SPAR-IDDC         TO LINK1-IDDC                       
047500               MOVE SPAR-IDLEVNR      TO LINK1-IDLEVNR                    
047600               MOVE SPAR-TILEVBSK-AVS TO LINK1-TILEVBSK-AVS               
047700               MOVE     JA            TO LINK1-FLSENLEVC1                 
047800                                         LINK1-FLSENLEVC2                 
047900                                                                          
048000               SET  LINK1-GHU-WDD924  TO TRUE                             
048100               CALL W2215610 USING LINK1-AREA INLB-L-PCB                  
048200                                            INLB-U-PCB W6INLA-PCB         
048300                                                                          
048400               SET  LINK1-REPL-WDD924  TO TRUE                            
048500               CALL W2215610 USING LINK1-AREA INLB-L-PCB                  
048600                                            INLB-U-PCB W6INLA-PCB         
048610               ADD +1 TO W-CHKP-RAKNARE                                   
048700             END-IF                                                       
048800           END-IF                                                         
048900         END-IF                                                           
049000       END-IF                                                             
049100                                                                          
049200       SET  LINK1-LAES-NASTA-UNDER-ROT  TO TRUE                           
049300       CALL W2215610 USING LINK1-AREA INLB-L-PCB                          
049400                                      INLB-U-PCB W6INLA-PCB               
049500     END-PERFORM                                                          
049600     .                                                                    
049700     EJECT                                                                
049800******************************************************************        
049900*                                                                *        
050000*    BERAKNA JFWDATUM BORTTAG                                    *        
050100*                                                                *        
050200******************************************************************        
050300                                                                          
050400 FA-BERAKNA-JFWDATUM-BORTTAG SECTION.                                     
050500                                                                          
050600     MOVE LINK1-TILEVBSK-INLC1   TO TMP1-YYMMDD                           
050700     MOVE LINK1-TILEVBSK-INLC2   TO TMP2-YYMMDD                           
050800     PERFORM WY2000P1                                                     
050900     IF  TMP1-YYMMDD > TMP2-YYMMDD                                        
051000       MOVE LINK1-TILEVBSK-INLC1 TO DAT-I-TIDATUM                         
051100     ELSE                                                                 
051200       MOVE LINK1-TILEVBSK-INLC2 TO DAT-I-TIDATUM                         
051300     END-IF                                                               
051400     PERFORM S01-KONVERTERA-TILEVBSK                                      
051500     MOVE WS-TIAAVV TO W-JFWDATUM-BORTTAG                                 
051600     .                                                                    
051700     EJECT                                                                
051800******************************************************************        
051900*                                                                *        
052000*    BERAKNA JFWDATUM FORSENING                                  *        
052100*                                                                *        
052200******************************************************************        
052300                                                                          
052400 FB-BERAKNA-JFWDATUM-FORSENING SECTION.                                   
052500                                                                          
052600     IF  LINK1-TILEVBSK-INLC1 = ZERO                                      
052700       MOVE LINK1-TILEVBSK-INLC2 TO DAT-I-TIDATUM                         
052800     ELSE                                                                 
052900       IF  LINK1-TILEVBSK-INLC2 = ZERO                                    
053000         MOVE LINK1-TILEVBSK-INLC1 TO DAT-I-TIDATUM                       
053100       ELSE                                                               
053200         MOVE LINK1-TILEVBSK-INLC1   TO TMP1-YYMMDD                       
053300         MOVE LINK1-TILEVBSK-INLC2   TO TMP2-YYMMDD                       
053400         PERFORM WY2000P1                                                 
053500         IF  TMP1-YYMMDD < TMP2-YYMMDD                                    
053600           MOVE LINK1-TILEVBSK-INLC1 TO DAT-I-TIDATUM                     
053700         ELSE                                                             
053800           MOVE LINK1-TILEVBSK-INLC2 TO DAT-I-TIDATUM                     
053900         END-IF                                                           
054000       END-IF                                                             
054100     END-IF                                                               
054200     PERFORM S01-KONVERTERA-TILEVBSK                                      
054300     MOVE WS-TIAAVV TO W-JFWDATUM-FORSENING                               
054400     .                                                                    
054500     EJECT                                                                
054600******************************************************************        
054700*                                                                *        
054800*    BEHANDLA DUBBLA LEVERANSBESKED                              *        
054900*                                                                *        
055000******************************************************************        
055100                                                                          
055200 FC-BEH-DUBBLA-LEVERANSBESKED SECTION.                                    
055300                                                                          
055400     MOVE LINK1-TILEVBSK-INLC1 TO DAT-I-TIDATUM                           
055500     PERFORM S01-KONVERTERA-TILEVBSK                                      
055600     IF LINK1-FLSENLEVC1 = JA                                             
055700       MOVE WS-TIAAVV TO W-JFWDATUM-BORTTAG                               
055800       MOVE W-JFWDATUM-BORTTAG   TO TMP1-YYWW                             
055900       MOVE W-TIAAVV-BORTTAG     TO TMP2-YYWW                             
056000       PERFORM WY2000P3                                                   
056100       IF TMP1-YYWW < TMP2-YYWW                                           
056200         MOVE ZERO TO LINK1-TILEVBSK-INLC1                                
056300         LINK1-KVAVIS-BSKKVARC1                                           
056400         MOVE SPACE TO LINK1-FLSENLEVC1                                   
056500       END-IF                                                             
056600     ELSE                                                                 
056700       MOVE WS-TIAAVV TO W-JFWDATUM-FORSENING                             
056800       MOVE W-JFWDATUM-FORSENING   TO TMP1-YYWW                           
056900       MOVE W-TIAAVV-AKTUELL       TO TMP2-YYWW                           
057000       PERFORM WY2000P3                                                   
057100       IF TMP1-YYWW <= TMP2-YYWW                                          
057200         MOVE JA TO LINK1-FLSENLEVC1                                      
057300       END-IF                                                             
057400     END-IF                                                               
057500     MOVE LINK1-TILEVBSK-INLC2 TO DAT-I-TIDATUM                           
057600     PERFORM S01-KONVERTERA-TILEVBSK                                      
057700     IF LINK1-FLSENLEVC2 = JA                                             
057800       MOVE WS-TIAAVV TO W-JFWDATUM-BORTTAG                               
057900       MOVE W-JFWDATUM-BORTTAG   TO TMP1-YYWW                             
058000       MOVE W-TIAAVV-BORTTAG     TO TMP2-YYWW                             
058100       PERFORM WY2000P3                                                   
058200       IF TMP1-YYWW < TMP2-YYWW                                           
058300         MOVE ZERO TO LINK1-TILEVBSK-INLC2                                
058400         LINK1-KVAVIS-BSKKVARC2                                           
058500         MOVE SPACE TO LINK1-FLSENLEVC2                                   
058600       END-IF                                                             
058700     ELSE                                                                 
058800       MOVE WS-TIAAVV TO W-JFWDATUM-FORSENING                             
058900       MOVE W-JFWDATUM-FORSENING   TO TMP1-YYWW                           
059000       MOVE W-TIAAVV-AKTUELL       TO TMP2-YYWW                           
059100       PERFORM WY2000P3                                                   
059200       IF TMP1-YYWW <= TMP2-YYWW                                          
059300         MOVE JA TO LINK1-FLSENLEVC2                                      
059400       END-IF                                                             
059500     END-IF                                                               
059600     MOVE SPAR-IDARTNR TO LINK1-IDARTNR                                   
059610     MOVE SPAR-IDDC    TO LINK1-IDDC                                      
059700     MOVE SPAR-IDLEVNR TO LINK1-IDLEVNR                                   
059800     MOVE SPAR-TILEVBSK-AVS TO LINK1-TILEVBSK-AVS                         
059900     IF  LINK1-KVAVIS-BSKKVARC1 = 0                                       
060000     AND LINK1-KVAVIS-BSKKVARC2 = 0                                       
060100       MOVE SPAR-IDARTNR TO W-IDARTNR                                     
060200       MOVE SPAR-IDDC    TO W-IDDC                                        
060210       MOVE SPAR-IDLEVNR TO W-IDLEVNR                                     
060300                                                                          
060400       SET  LINK1-GHU-WDD924  TO TRUE                                     
060500       CALL W2215610 USING LINK1-AREA INLB-L-PCB                          
060600                                      INLB-U-PCB W6INLA-PCB               
060700                                                                          
060800       SET  LINK1-REPL-WDD924  TO TRUE                                    
060900       CALL W2215610 USING LINK1-AREA INLB-L-PCB                          
061000                                      INLB-U-PCB W6INLA-PCB               
061010       ADD +1 TO W-CHKP-RAKNARE                                           
061100     ELSE                                                                 
061200                                                                          
061300       SET  LINK1-GHU-WDD924  TO TRUE                                     
061400       CALL W2215610 USING LINK1-AREA INLB-L-PCB                          
061500                                      INLB-U-PCB W6INLA-PCB               
061600                                                                          
061700       SET  LINK1-REPL-WDD924  TO TRUE                                    
061800       CALL W2215610 USING LINK1-AREA INLB-L-PCB                          
061900                                      INLB-U-PCB W6INLA-PCB               
061910       ADD +1 TO W-CHKP-RAKNARE                                           
062000     END-IF                                                               
062100     .                                                                    
062200     EJECT                                                                
062300                                                                          
062400 H-BEHANDLA-WDD925-IDLEVBSK SECTION.                                      
062500                                                                          
062600     IF LINK1-IDLEVBSK = 2                                                
062700        MOVE LINK1-TIBORT        TO TMP1-YYMMDD                           
062800        MOVE DAGENS-DATUM-PACK   TO TMP2-YYMMDD                           
062900        PERFORM WY2000P1                                                  
063000        IF TMP1-YYMMDD < TMP2-YYMMDD                                      
063100           MOVE SPAR-IDARTNR    TO LINK2-IDARTNR                          
063110           MOVE SPAR-IDDC       TO LINK2-IDDC                             
063200           MOVE SPAR-IDLEVNR    TO LINK2-IDLEVNR                          
063300           MOVE +2              TO LINK2-IDLEVBSK                         
063400                                                                          
063500           SET  LINK2-GHU-WDD925  TO TRUE                                 
063600           CALL W2215610 USING LINK2-AREA INLB-L-PCB                      
063700                                          INLB-U-PCB                      
063800                                          W6INLA-PCB                      
063900           SET  LINK2-DELETE  TO TRUE                                     
064000           CALL W2215610 USING LINK2-AREA INLB-L-PCB                      
064100                                          INLB-U-PCB                      
064200                                          W6INLA-PCB                      
064210           ADD +1 TO W-CHKP-RAKNARE                                       
064300        END-IF                                                            
064400     END-IF                                                               
064500                                                                          
064600     SET  LINK1-LAES-NASTA-UNDER-ROT  TO TRUE                             
064700     CALL W2215610 USING LINK1-AREA INLB-L-PCB                            
064800                                    INLB-U-PCB                            
064900                                    W6INLA-PCB                            
065000     .                                                                    
065100     EJECT                                                                
065200                                                                          
065300                                                                          
065400 S01-KONVERTERA-TILEVBSK SECTION.                                         
065500                                                                          
065600     IF DAT-I-TIDATUM > 0                                                 
065700       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
065800       CALL WDATKONV USING DAT-KDDATFORM                                  
065900       DAT-I-TIDATUM                                                      
066000       DAT-O-TIDATUM                                                      
066100       DAT-KDSVAR                                                         
066200       IF DAT-KDSVAR-OK                                                   
066300         MOVE DAT-TIAAVVD TO WS-TIAAVVD                                   
066400       ELSE                                                               
066500         MOVE ZERO        TO WS-TIAAVVD                                   
066600       END-IF                                                             
066700     ELSE                                                                 
066800       MOVE ZERO TO WS-TIAAVVD                                            
066900     END-IF                                                               
067000     IF WS-TIAAVVD = ZERO                                                 
067100       MOVE LINK1-TILEVBSK-AVS TO DAT-I-TIDATUM                           
067200       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
067300       CALL WDATKONV USING DAT-KDDATFORM                                  
067400       DAT-I-TIDATUM                                                      
067500       DAT-O-TIDATUM                                                      
067600       DAT-KDSVAR                                                         
067700       IF DAT-KDSVAR-OK                                                   
067800         MOVE DAT-TIAAVVD TO WS-TIAAVVD                                   
067900       ELSE                                                               
068000         MOVE ZERO        TO WS-TIAAVVD                                   
068100       END-IF                                                             
068200       MOVE WS-TIAAVV TO W009VADD-DATUM                                   
068300       MOVE +2 TO W009VADD-ANTAL                                          
068400       CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                  
068500       MOVE W009VADD-DATUM TO WS-TIAAVV                                   
068600     END-IF                                                               
068700     .                                                                    
068800     EJECT                                                                
069110*****  IMS SECTIONER   ****                                               
069120 IMS-RESTART  SECTION.                                                    
069130                                                                          
069140     MOVE SPACE TO MSG-IO-AREA-1                                          
069150     MOVE '  ' TO GODK-STATUSKODER                                        
069160     CALL CBLTDLI USING XRST MSG-PCB                                      
069170                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
069180                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
069190     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
069191     PERFORM IMS-STATUSKONTROLL                                           
069192     .                                                                    
069193     SKIP3                                                                
069200 IMS-CHECKPOINT  SECTION.                                                 
069300                                                                          
069400     MOVE CHKP-ID TO MSG-IO-AREA-1                                        
069500     MOVE '  XD' TO GODK-STATUSKODER                                      
069600     CALL CBLTDLI USING CHKP MSG-PCB                                      
069700                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
069800                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
069900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070000     PERFORM IMS-STATUSKONTROLL                                           
070100     .                                                                    
070200     EJECT                                                                
070300 IMS-GU-WDD901 SECTION.                                                   
070400                                                                          
070610     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
070620     DELIMITED BY SIZE INTO SSA1                                          
070700     MOVE '    '             TO GODK-STATUSKODER                          
070800     CALL CBLTDLI USING GU INLB-L-PCB DLI-IO-WDD901  SSA1                 
070900     MOVE INLB-L-STATUS-CODE TO STATUS-WS                                 
071000     PERFORM IMS-STATUSKONTROLL                                           
071100     .                                                                    
071200     SKIP3                                                                
071300 IMS-STATUSKONTROLL SECTION.                                              
071400                                                                          
071500     SET STATUS-IX TO 1                                                   
071600     SEARCH GODK-STATUS  AT END  CALL FELLOG                              
071700     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
071800     CONTINUE                                                             
071900     END-SEARCH                                                           
072000     .                                                                    
072100     EJECT                                                                
072200*    -COPY WY2000P1                                                       
072300     EJECT                                                                
072400*    -COPY WY2000P3                                                       
