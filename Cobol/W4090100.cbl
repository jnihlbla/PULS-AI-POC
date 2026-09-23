000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4090100.                                                
000400 AUTHOR.         STIG MüLLER.                                             
000500     DATE-WRITTEN.   JAN   85.                                            
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*--- MODIFIERAD SEPTEMBER 1990 AV JAN-ERIK FRANTZEN                       
001000*    BYTT UT DATABAS RDA2 MOT WDQ2 .                                      
001100*---                                                                      
001200*    FUNKTION.                                                            
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T901                                              
001600*        MID:         W4I90101                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W4O90101                                            
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200                                                                          
002300 WORKING-STORAGE SECTION.                                                 
002400*    -COPY WY2000W1                                                       
002500     SKIP3                                                                
002600 77   PROGRAM-NAMN           VALUE 'W4090100'                             
002700                                 PIC X(8).                                
002800 77  JA                        PIC X       VALUE 'J'.                     
002900 77  NEJ                       PIC X       VALUE 'N'.                     
003000 77  NYCKLAR-OK                PIC X       VALUE 'J'.                     
003100 77  TECKEN                    PIC X.                                     
003200 77  TID                       PIC X.                                     
003300 77  IDDISTR-WS                PIC X(4)    VALUE SPACE.                   
003400 77  IDKUNDNR-WS               PIC X(6)    VALUE SPACE.                   
003500 77  IDORDNR-WS                PIC X(7)    VALUE SPACE.                   
003600 77  TIREGDAT-WS               PIC X(6)    VALUE SPACE.                   
003700 77  W-TIREGDAT                PIC S9(7)   VALUE +0    COMP-3.            
003800 77  IX                        PIC S9(3)   VALUE +0    COMP SYNC.         
003900 77  MAX-RADER                 PIC S9(3)   VALUE +13   COMP SYNC.         
004000 77  SPRAK-INDEX               PIC S9(1)   COMP-3.                        
004100 77  WS-IDTRANS                PIC X(4).                                  
004200   88  WS-GODKAEND-BILD        VALUE '4901' '4902'.                       
004300   88  WS-4902-BILD            VALUE '4902'.                              
004400   88  EGEN-MID                VALUE '4901'.                              
004500     SKIP2                                                                
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004900     EJECT                                                                
005000 01    NYCKLAR-TILL-DLI.                                                  
005100     03  W-WDQ2C1KY-MIN-X.                                                
005200         05 W-IDDISTR-MIN        PIC S9(5)   VALUE ZERO  COMP-3.          
005300         05 W-IDKUNDNR-MIN       PIC S9(7)   VALUE ZERO  COMP-3.          
005400         05 W-IDKUNDRF-MIN       PIC  X(10)  VALUE LOW-VALUE.             
005500     SKIP3                                                                
005600     03  W-WDQ2C1KY-MAX-X.                                                
005700         05 W-IDDISTR-MAX        PIC S9(5)   VALUE ZERO  COMP-3.          
005800         05 W-IDKUNDNR-MAX       PIC S9(7)   VALUE ZERO  COMP-3.          
005900         05 FILLER               PIC  X(10)  VALUE HIGH-VALUE.            
006000     SKIP3                                                                
006100     03 W-IDORDER-X.                                                      
006200         05 W-IDORDER            PIC S9(7)   VALUE ZERO  COMP-3.          
006300     EJECT                                                                
006400 01    MEDDELANDE.                                                        
006500   03    FEL1.                                                            
006600     05  FILLER                  PIC X(40)                                
006700         VALUE 'NYCKEL ÄR INTE NUMERISK'.                                 
006800     05  FILLER                  PIC X(40)                                
006900         VALUE 'KEY IS NOT NUMERIC'.                                      
007000   03    FILLER REDEFINES FEL1.                                           
007100     05  FEL-1 OCCURS 2          PIC X(40).                               
007200     SKIP3                                                                
007300   03    MED1.                                                            
007400     05  FILLER                  PIC X(79)                                
007500         VALUE 'FÖR MERA INFORMATION, TRYCK PF8  '.                       
007600     05  FILLER                  PIC X(79)                                
007700         VALUE 'FOR MORE INFORMATION, PRESS PF8  '.                       
007800   03    FILLER REDEFINES MED1.                                           
007900     05  MED-1 OCCURS 2          PIC X(79).                               
008000     SKIP3                                                                
008100   03    MED2.                                                            
008200     05  FILLER                  PIC X(79)                                
008300         VALUE 'INGA ORDER EXISTERAR        '.                            
008400     05  FILLER                  PIC X(79)                                
008500         VALUE 'NO ORDER EXIST        '.                                  
008600   03    FILLER REDEFINES MED2.                                           
008700     05  MED-2 OCCURS 2          PIC X(79).                               
008800     EJECT                                                                
008900******************************************************************        
009000*                                                                         
009100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
009200*                                                                         
009300 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
009400     SKIP3                                                                
009500*01    MID -COPY W4I90101.                                                
009600     EJECT                                                                
009700*01    -COPY WMSGAREA                                                     
009800     EJECT                                                                
009900*  03    MOD -COPY W4O90101 -RED MSG-AREA.                                
010000     EJECT                                                                
010100*01    -COPY WMFSAREA                                                     
010200     EJECT                                                                
010300******************************************************************        
010400*                                                                         
010500*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600*                                                                         
010700 01    IMS-WS.                                                            
010800   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
010900     SKIP3                                                                
011000*                        **** STATUS-KOD FRÅN IMS                         
011100   03    STATUS-WS               PIC XX.                                  
011200     88    SEGMENT-FINNS                     VALUE '  '.                  
011300     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
011400     88    BASEN-SLUT                        VALUE 'GB'.                  
011500     SKIP3                                                                
011600   03    GODK-STATUSKODER.                                                
011700     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
011800     SKIP3                                                                
011900 01    SSA1                      PIC X(128).                              
012000 01    SSA2                      PIC X(64).                               
012100     EJECT                                                                
012200*                            IMS FUNKTIONSKODER                           
012300*01    -COPY W0003                                                        
012400     EJECT                                                                
012500*                            DLI INPUT-OUTPUT AREA                        
012600 01    DLI-IO-AREA.                                                       
012700   03    IO-AREA                 PIC X(700)  VALUE SPACE.                 
012800     SKIP3                                                                
012900*  03    WLORQI01 -COPY WDQ201            -RED IO-AREA.                   
013000     EJECT                                                                
013100*  03    WLORQL01 -COPY WDQ2C1            -RED IO-AREA.                   
013200     EJECT                                                                
013300 LINKAGE SECTION.                                                         
013400*01    -COPY W0009     -PRE MSG-                                          
013500     EJECT                                                                
013600*01    -COPY W0008     -PRE ORQL-                                         
013700     05  FILLER                  PIC X.                                   
013800                                                                          
013900*01    -COPY W0008     -PRE ORQI-                                         
014000     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014200 PROCEDURE DIVISION  USING MSG-PCB ORQL-PCB ORQI-PCB.                     
014300     ENTRY 'DLITCBL' USING MSG-PCB ORQL-PCB ORQI-PCB.                     
014400     PERFORM IMS-GET-MSG                                                  
014500     IF SEGMENT-FINNS                                                     
014600        PERFORM A-INIT-SPARA-INPUT                                        
014700        PERFORM B-KOLLA-NYCKLAR                                           
014800        MOVE 1 TO IX                                                      
014900        IF NYCKLAR-OK                        =  JA                        
015000           IF MFS-NEXT                                                    
015100              PERFORM IMS-GU-ORQL                                         
015200           ELSE                                                           
015300              PERFORM IMS-GN-ORQL                                         
015400           END-IF                                                         
015500           PERFORM UNTIL IX                  >  MAX-RADER                 
015600              IF SEGMENT-FINNS                                            
015700                 PERFORM E-FLYTTA-TILL-RADER                              
015800                 PERFORM IMS-GN-ORQL                                      
015900              ELSE                                                        
016000                 IF IX = 1                                                
016100                    MOVE MED-2 (SPRAK-INDEX) TO MOD-MESSAGE-RAD23         
016200                 END-IF                                                   
016300                 MOVE MFS-RENSA-FAELT        TO MOD-RAD (IX)              
016400                 ADD +1                      TO IX                        
016500              END-IF                                                      
016600           END-PERFORM                                                    
016700           IF SEGMENT-FINNS                                               
016800              PERFORM H-BLADDRING                                         
016900              MOVE MED-1 (SPRAK-INDEX)       TO MOD-MESSAGE-RAD23         
017000           END-IF                                                         
017100        ELSE                                                              
017200           MOVE FEL-1 (SPRAK-INDEX) TO MOD-MESSAGE-RAD1                   
017300           PERFORM UNTIL IX > MAX-RADER                                   
017400              MOVE MFS-RENSA-FAELT  TO MOD-RAD(IX)                        
017500              ADD +1                TO IX                                 
017600           END-PERFORM                                                    
017700        END-IF                                                            
017800        COMPUTE MSG-KVLL = LENGTH OF MOD-W4O90101 + 4                     
017900        PERFORM IMS-INSERT-MSG                                            
018000     END-IF                                                               
018100     MOVE ZERO TO RETURN-CODE                                             
018200     GOBACK                                                               
018300     .                                                                    
018400     EJECT                                                                
018500 A-INIT-SPARA-INPUT SECTION.                                              
018600     IF MSG-DUBBLA-TRANSKODER                                             
018700        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I90101                
018800        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
018900        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
019000     ELSE                                                                 
019100        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I90101                
019200        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
019300        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
019400     END-IF                                                               
019500     IF ENGLISH-TEXT                                                      
019600        MOVE +2                            TO SPRAK-INDEX                 
019700     ELSE                                                                 
019800        MOVE +1                            TO SPRAK-INDEX                 
019900     END-IF                                                               
020000                                                                          
020100     MOVE MSG-KDTRTYP                      TO MFS-KDTRTYP                 
020200     MOVE MSG-IDPFK                        TO MFS-IDPFK                   
020300     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
020400                                                                          
020500     IF NOT EGEN-MID                                                      
020600       MOVE SPACE                          TO MFS-KDTRTYP                 
020700       MOVE '7'                            TO MFS-IDPFK                   
020800     END-IF                                                               
020900                                                                          
021000     MOVE LOW-VALUE TO MSG-AREA                                           
021100     MOVE 'W4O90101' TO MFS-IDMOD                                         
021200     MOVE '4901' TO MOD-IDTRANS                                           
021300                                                                          
021400     MOVE LOW-VALUE                   TO W-WDQ2C1KY-MIN-X                 
021500     MOVE HIGH-VALUE                  TO W-WDQ2C1KY-MAX-X                 
021600                                                                          
021700     IF MID-IDDISTR-IN = ALL '+'                                          
021800        MOVE MID-IDDISTR-UT           TO IDDISTR-WS                       
021900        INSPECT IDDISTR-WS REPLACING LEADING SPACE BY ZERO                
022000     ELSE                                                                 
022100        MOVE MID-IDDISTR-IN           TO IDDISTR-WS                       
022200        MOVE SPACE                    TO MID-IDKUNDNR-UT                  
022300        MOVE ' '                      TO MFS-IDPFK                        
022400     END-IF                                                               
022500     IF MID-IDKUNDNR-IN = ALL '+'                                         
022600        MOVE MID-IDKUNDNR-UT          TO IDKUNDNR-WS                      
022700           IF IDKUNDNR-WS NOT = SPACE                                     
022800              INSPECT IDKUNDNR-WS REPLACING LEADING SPACE BY ZERO         
022900           END-IF                                                         
023000     ELSE                                                                 
023100        MOVE MID-IDKUNDNR-IN          TO IDKUNDNR-WS                      
023200        MOVE ' '                      TO MFS-IDPFK                        
023300     END-IF                                                               
023400     IF MID-IDORDNR-IN = ALL '+'                                          
023500        MOVE MID-IDORDNR-UT           TO IDORDNR-WS                       
023600        INSPECT IDORDNR-WS REPLACING LEADING SPACE BY ZERO                
023700     ELSE                                                                 
023800        MOVE MID-IDORDNR-IN           TO IDORDNR-WS                       
023900        MOVE ' '                      TO MFS-IDPFK                        
024000     END-IF                                                               
024100     IF MID-TIORDREG-IN = ALL '+'                                         
024200        MOVE MID-TIORDREG-UT          TO TIREGDAT-WS                      
024300        INSPECT TIREGDAT-WS REPLACING LEADING SPACE BY ZERO               
024400     ELSE                                                                 
024500        MOVE MID-TIORDREG-IN          TO TIREGDAT-WS                      
024600        MOVE ' '                      TO MFS-IDPFK                        
024700     END-IF                                                               
024800     IF MID-IDTECKEN-IN = ALL '+'                                         
024900        MOVE MID-IDTECKEN-UT          TO TECKEN                           
025000     ELSE                                                                 
025100        IF MID-IDTECKEN-IN = '0'                                          
025200          MOVE SPACE                  TO MID-IDTECKEN-IN                  
025300        END-IF                                                            
025400        MOVE ZERO                     TO MID-IDORDNR-SPAR                 
025500        MOVE MID-IDTECKEN-IN          TO TECKEN                           
025600     END-IF                                                               
025700                                                                          
025800     MOVE IDDISTR-WS                 TO MOD-IDDISTR-UT                    
025900     MOVE IDKUNDNR-WS                TO MOD-IDKUNDNR-UT                   
026000     MOVE IDORDNR-WS                 TO MOD-IDORDNR-UT                    
026100     MOVE TIREGDAT-WS                TO MOD-TIORDREG-UT                   
026200     MOVE TECKEN                     TO MOD-IDTECKEN-UT                   
026300     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
026400     IF MOD-IDKUNDNR-UT = ZERO                                            
026500        MOVE '     0'                TO MOD-IDKUNDNR-UT                   
026600     ELSE                                                                 
026700        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
026800     END-IF                                                               
026900     INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE               
027000     INSPECT MOD-TIORDREG-UT REPLACING LEADING ZERO BY SPACE              
027100                                                                          
027200     PERFORM AB-RENSA-MOD                                                 
027300     .                                                                    
027400     EJECT                                                                
027500 AB-RENSA-MOD         SECTION.                                            
027600                                                                          
027700     MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR-IN                             
027800                               MOD-IDKUNDNR-IN                            
027900                               MOD-IDORDNR-IN                             
028000                               MOD-TIORDREG-IN                            
028100                               MOD-IDTECKEN-IN                            
028200                               MOD-MESSAGE-RAD1                           
028300                               MOD-MESSAGE-RAD23                          
028400                                                                          
028500     IF NOT WS-GODKAEND-BILD                                              
028600       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
028700                               MOD-IDKUNDNR-UT                            
028800                               MOD-IDORDNR-UT                             
028900                               MOD-IDTECKEN-UT                            
029000                               MOD-TIORDREG-UT                            
029100     END-IF                                                               
029200                                                                          
029300     IF WS-4902-BILD                                                      
029400       MOVE MFS-RENSA-FAELT TO MOD-IDORDNR-UT                             
029500                               MOD-IDTECKEN-UT                            
029600                               MOD-TIORDREG-UT                            
029700                                                                          
029800       MOVE '0000000'       TO IDORDNR-WS                                 
029900                               TIREGDAT-WS                                
030000     END-IF                                                               
030100     EJECT                                                                
030200                                                                          
030300     .                                                                    
030400 B-KOLLA-NYCKLAR SECTION.                                                 
030500     SKIP2                                                                
030600     MOVE NEJ                            TO NYCKLAR-OK                    
030700     IF IDDISTR-WS NUMERIC                                                
030800        MOVE IDDISTR-WS                  TO W-IDDISTR-MIN                 
030900                                            W-IDDISTR-MAX                 
031000        IF IDORDNR-WS NUMERIC                                             
031100           IF IDORDNR-WS NOT             =  ZERO                          
031200              MOVE IDORDNR-WS            TO W-IDKUNDRF-MIN                
031300           END-IF                                                         
031400           IF TIREGDAT-WS NUMERIC                                         
031500              IF TIREGDAT-WS NOT         =  ZERO                          
031600                 MOVE TIREGDAT-WS        TO W-TIREGDAT                    
031700              END-IF                                                      
031800              IF IDKUNDNR-WS NUMERIC                                      
031900                 MOVE IDKUNDNR-WS        TO W-IDKUNDNR-MIN                
032000                                            W-IDKUNDNR-MAX                
032100                 MOVE JA                 TO NYCKLAR-OK                    
032200              ELSE                                                        
032300                 IF IDKUNDNR-WS          =  SPACE                         
032400                    MOVE JA              TO NYCKLAR-OK                    
032500                 END-IF                                                   
032600              END-IF                                                      
032700           END-IF                                                         
032800        END-IF                                                            
032900     END-IF                                                               
033000                                                                          
033100     IF MFS-NEXT                      AND                                 
033200        MID-IDDISTR-SPAR           >  ZERO                                
033300           MOVE MID-IDDISTR-SPAR   TO W-IDDISTR-MIN                       
033400           MOVE MID-IDKUNDNR-SPAR  TO W-IDKUNDNR-MIN                      
033500           MOVE MID-IDORDNR-SPAR   TO W-IDKUNDRF-MIN                      
033600     END-IF                                                               
033700     IF TECKEN                     =  '<' OR '>' OR '='                   
033800        CONTINUE                                                          
033900     ELSE                                                                 
034000        MOVE ' '                   TO TECKEN                              
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400 E-FLYTTA-TILL-RADER SECTION.                                             
034500                                                                          
034600     MOVE SEQC-IDORDNR7               TO MOD-IDORDNR-RAD (IX)             
034700     MOVE SEQC-IDKUNDNR               TO MOD-IDKUNDNR-RAD (IX)            
034800     MOVE SEQC-IDORDER                TO W-IDORDER                        
034900     MOVE NEJ                         TO TID                              
035000     PERFORM IMS-GU-ORQI                                                  
035100     IF OHUV-KDTPOTYP > ZERO                                              
035200        CONTINUE                                                          
035300     ELSE                                                                 
035400        IF TECKEN                     =  SPACE                            
035500           MOVE JA                    TO TID                              
035600        ELSE                                                              
035700           EVALUATE TECKEN                                                
035800           WHEN  '<'                                                      
035900              MOVE OHUV-TIREGDAT   TO TMP1-YYMMDD                         
036000              MOVE W-TIREGDAT      TO TMP2-YYMMDD                         
036100              PERFORM WY2000P1                                            
036200              IF TMP1-YYMMDD < TMP2-YYMMDD                                
036300                 MOVE JA              TO TID                              
036400              END-IF                                                      
036500           WHEN  '='                                                      
036600              IF OHUV-TIREGDAT        =  W-TIREGDAT                       
036700                 MOVE JA              TO TID                              
036800              END-IF                                                      
036900           WHEN  '>'                                                      
037000              MOVE OHUV-TIREGDAT   TO TMP1-YYMMDD                         
037100              MOVE W-TIREGDAT      TO TMP2-YYMMDD                         
037200              PERFORM WY2000P1                                            
037300              IF TMP1-YYMMDD > TMP2-YYMMDD                                
037400                 MOVE JA              TO TID                              
037500              END-IF                                                      
037600           END-EVALUATE                                                   
037700        END-IF                                                            
037800        IF TID                        =  JA                               
037900           MOVE OHUV-TIREGDAT         TO MOD-TIORDREG (IX)                
038000           MOVE OHUV-KDORDKL          TO MOD-KDORDKL (IX)                 
038100           MOVE MFS-RENSA-FAELT       TO MOD-KDORDKL-IMP (IX)             
038200           MOVE OHUV-BEKUNDRF         TO MOD-BEVOLREF (IX)                
038300           MOVE OHUV-BEVARREF         TO MOD-BEVARREF (IX)                
038400           ADD +1                     TO IX                               
038500        END-IF                                                            
038600     END-IF                                                               
038700     .                                                                    
038800     EJECT                                                                
038900 H-BLADDRING SECTION.                                                     
039000     SKIP2                                                                
039100     MOVE SEQC-IDDISTR        TO MOD-IDDISTR-SPAR                         
039200     MOVE SEQC-IDKUNDNR       TO MOD-IDKUNDNR-SPAR                        
039300     MOVE SEQC-IDORDER        TO W-IDORDER                                
039400     MOVE SEQC-IDORDNR7       TO MOD-IDORDNR-SPAR                         
039500     PERFORM IMS-GU-ORQI                                                  
039600     IF OHUV-KDTPOTYP         >  ZERO                                     
039700        CONTINUE                                                          
039800     ELSE                                                                 
039900        MOVE OHUV-TIREGDAT    TO MOD-TIORDREG-SPAR                        
040000     END-IF                                                               
040100     .                                                                    
040200     EJECT                                                                
040300* IMS SEKTIONER                                                           
040400                                                                          
040500 IMS-GET-MSG SECTION.                                                     
040600     MOVE '  QC' TO GODK-STATUSKODER                                      
040700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
040800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040900     PERFORM IMS-STATUSKONTROLL                                           
041000     SKIP3                                                                
041100     .                                                                    
041200 IMS-INSERT-MSG SECTION.                                                  
041300     IF ENGLISH-TEXT                                                      
041400        MOVE 'N' TO MFS-KDHUVOMR                                          
041500     END-IF                                                               
041600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
041700     MOVE SPACE TO GODK-STATUSKODER                                       
041800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
041900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
042000     PERFORM IMS-STATUSKONTROLL                                           
042100     .                                                                    
042200     EJECT                                                                
042300 IMS-GN-ORQL  SECTION.                                                    
042400     STRING 'WLORQL01(WDQ2C1KY>=' W-WDQ2C1KY-MIN-X                        
042500                    '&WDQ2C1KY<=' W-WDQ2C1KY-MAX-X                        
042600                    '&FLBORT   =N)'                                       
042700            DELIMITED BY SIZE INTO SSA1                                   
042800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
042900     CALL CBLTDLI USING GN ORQL-PCB DLI-IO-AREA SSA1                      
043000     MOVE ORQL-STATUS-CODE TO STATUS-WS                                   
043100     PERFORM IMS-STATUSKONTROLL                                           
043200     .                                                                    
043300 IMS-GU-ORQL  SECTION.                                                    
043400     STRING 'WLORQL01(WDQ2C1KY>=' W-WDQ2C1KY-MIN-X                        
043500                    '&WDQ2C1KY<=' W-WDQ2C1KY-MAX-X                        
043600                    '&FLBORT   =N)'                                       
043700            DELIMITED BY SIZE INTO SSA1                                   
043800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
043900     CALL CBLTDLI USING GU ORQL-PCB DLI-IO-AREA SSA1                      
044000     MOVE ORQL-STATUS-CODE TO STATUS-WS                                   
044100     PERFORM IMS-STATUSKONTROLL                                           
044200     .                                                                    
044300 IMS-GU-ORQI  SECTION.                                                    
044400     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
044500            DELIMITED BY SIZE INTO SSA1                                   
044600     MOVE '  ' TO GODK-STATUSKODER                                        
044700     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA SSA1                      
044800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
044900     PERFORM IMS-STATUSKONTROLL                                           
045000     EJECT                                                                
045100     .                                                                    
045200 IMS-STATUSKONTROLL SECTION.                                              
045300     SET STATUS-IX TO 1                                                   
045400     SEARCH GODK-STATUS AT END CALL FELLOG                                
045500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
045600     END-SEARCH                                                           
045700     .                                                                    
045800     EJECT                                                                
045900*    -COPY WY2000P1                                                       
