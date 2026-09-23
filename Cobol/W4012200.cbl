000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4012200.                                                
000300 AUTHOR.         INGER NILSSON.                                           
000400 DATE-WRITTEN.   MARS  90.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        UPPDATERING OCH FRÅGEPROGRAM                                     
000900*                                                                         
001000*    INDATA.                                                              
001100*        TRANSAKTION: W4T122                                              
001200*        MID:         W4I12201                                            
001300*                                                                         
001400*    UTDATA.                                                              
001500*        MOD:         W4O12201                                            
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900 DATA DIVISION.                                                           
002000     EJECT                                                                
002100 WORKING-STORAGE SECTION.                                                 
002200                                                                          
002300*    -- CHECKED BY WY2000                                                 
002400 77  IDPGM                   PIC X(8)    VALUE 'W4012200'.                
002500 77  JA                      PIC X       VALUE 'J'.                       
002600 77  NEJ                     PIC X       VALUE 'N'.                       
002700 77  DC-IX                   PIC S9(9)   VALUE +0   COMP SYNC.            
002800 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
002900 77  MAX-RADER               PIC S9(9)   VALUE +13  COMP SYNC.            
003000 77  IDKVAOMR-WS             PIC X(1).                                    
003100 77  IDKVAGRP-WS             PIC X(3).                                    
003200 77  TIAARP-WS               PIC X(4).                                    
003300 77  KDKVASTA-WS             PIC X(1).                                    
003400 77  W-TIAARP                PIC X(1).                                    
003500 77  W-DAT-TIAAMMDD          PIC 9(6).                                    
003600                                                                          
003700 77  W-INDATA                PIC X.                                       
003800 77  W-FEL-UPDATE            PIC X       VALUE 'N'.                       
003900 77  INDATA-SW               PIC X       VALUE 'J'.                       
004000   88  INDATA-OK                         VALUE 'J'.                       
004100   88  INDATA-FEL                        VALUE 'N'.                       
004200                                                                          
004300 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
004400   88  NYCKLAR-OK                        VALUE 'J'.                       
004500   88  NYCKLAR-FEL                       VALUE 'N'.                       
004600                                                                          
004700 77  ALLT-SW                 PIC X       VALUE 'J'.                       
004800   88  ALLT-OK                           VALUE 'J'.                       
004900                                                                          
005000 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
005100   88  EGEN-MID                          VALUE '4122'.                    
005200   88  GODK-MID                          VALUE '4121' '4123'.             
005300     EJECT                                                                
005400*   -COPY WWKVAOMR                                                        
005500     EJECT                                                                
005600*   --- VALID IDDC CODES                                                  
005700*                                                                         
005800*01  -COPY WWDC99                                                         
005900     EJECT                                                                
006000 01  GENERELLA-SUBPROGRAM.                                                
006100   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
006200   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
006300   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
006400   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
006500   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
006600     EJECT                                                                
006700*   -COPY WDATAREA                                                        
006800     EJECT                                                                
006900*   -COPY WMEDAREA                                                        
007000     EJECT                                                                
007100******************************************************************        
007200*                                                                         
007300*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
007400*                                                                         
007500 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
007600     SKIP3                                                                
007700*01  MID -COPY W4I12201                                                   
007800     EJECT                                                                
007900*01  -COPY WMSGAREA                                                       
008000     EJECT                                                                
008100*  03  MOD -COPY W4O12201 -RED MSG-AREA.                                  
008200     EJECT                                                                
008300*01  -COPY WMFSAREA                                                       
008400     EJECT                                                                
008500******************************************************************        
008600*                                                                         
008700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008800*                                                                         
008900 01  IMS-WS.                                                              
009000   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
009100     SKIP3                                                                
009200 01  NYCKLAR-TILL-DLI.                                                    
009300                                                                          
009400   03  W-WDGX-4823-KEY-X.                                                 
009500     04  W-IDHTYP-4823      PIC  X(04)  VALUE '4823'.                     
009600     04  FILLER             PIC  X(26)  VALUE LOW-VALUE.                  
009700                                                                          
009800   03  W-W6H601KY-X.                                                      
009900     04  W-IDDC             PIC  X(02).                                   
010000     04  W-IDKVAOMR         PIC  X(01).                                   
010100     04  W-IDKVAGRP         PIC  9(03).                                   
010200     04  W-DAREGDAT         PIC  9(08).                                   
010300                                                                          
010400   03  W2-W6H6ASEQ-X.                                                     
010500     04  W2-IDDC            PIC  X(02).                                   
010600     04  W2-IDKVAOMR        PIC  X(01).                                   
010700     04  W2-IDKVAGRP-X.                                                   
010800       05  W2-IDKVAGRP      PIC  9(03).                                   
010900     04  W2-DAREGDAT-X.                                                   
011000       05  W2-DAREGDAT      PIC  9(08).                                   
011100     04  W2-KDKVASTA-X.                                                   
011200       05  W2-KDKVASTA      PIC  X(01).                                   
011300                                                                          
011400   03  W3-W6H6ASEQ-X.                                                     
011500     04  W3-IDDC            PIC  X(02).                                   
011600     04  W3-IDKVAOMR        PIC  X(01).                                   
011700     04  W3-IDKVAGRP-X.                                                   
011800       05  W3-IDKVAGRP      PIC  9(03).                                   
011900     04  W3-DAREGDAT-X.                                                   
012000       05  W3-DAREGDAT      PIC  9(08).                                   
012100     04  W3-KDKVASTA-X.                                                   
012200       05  W3-KDKVASTA      PIC  X(01).                                   
012300                                                                          
012400   03  W4-W6H6ASEQ-X.                                                     
012500     04  W4-IDDC            PIC  X(02).                                   
012600     04  W4-IDKVAOMR        PIC  X(01).                                   
012700     04  W4-IDKVAGRP        PIC  9(03).                                   
012800     04  W4-DAREGDAT        PIC  9(08).                                   
012900     04  W4-KDKVASTA        PIC  X(01).                                   
013000                                                                          
013100*                        **** STATUS-KOD FRÅN IMS                         
013200   03  STATUS-WS             PIC XX.                                      
013300     88  SEGMENT-FINNS                   VALUE '  '.                      
013400     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
013500     88  SEGMENT-SLUT                    VALUE 'GB'.                      
013600     SKIP3                                                                
013700   03  GODK-STATUSKODER.                                                  
013800     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013900     SKIP3                                                                
014000 01    SSA1                  PIC X(128).                                  
014100 01    SSA2                  PIC X(64).                                   
014200     EJECT                                                                
014300*                            IMS FUNKTIONSKODER                           
014400*01    -COPY W0003                                                        
014500     EJECT                                                                
014600*                            DLI INPUT-OUTPUT AREA                        
014700 01  DLI-IO-AREA1.                                                        
014800   03  IO-AREA1              PIC X(850) VALUE SPACE.                      
014900     SKIP3                                                                
015000*  03  W6KVAB01  -COPY W6H601    -RED IO-AREA1.                           
015100 01  DLI-IO-AREA2.                                                        
015200   03  IO-AREA2              PIC X(15)  VALUE SPACE.                      
015300     SKIP3                                                                
015400*  03  WDGX4824  -COPY WDGX4824  -RED IO-AREA2                            
015500     EJECT                                                                
015600 LINKAGE SECTION.                                                         
015700*01  -COPY W0009     -PRE MSG-                                            
015800     EJECT                                                                
015900*01  -COPY W0008     -PRE W6H6A-                                          
016000     05  FILLER              PIC X.                                       
016100     EJECT                                                                
016200*01  -COPY W0008     -PRE W6H6-                                           
016300     05  FILLER              PIC X.                                       
016400     EJECT                                                                
016500*01  -COPY W0008     -PRE XXJZ-                                           
016600     05  FILLER              PIC X.                                       
016700     EJECT                                                                
016800 PROCEDURE DIVISION  USING MSG-PCB W6H6A-PCB W6H6-PCB XXJZ-PCB.           
016900 MAIN SECTION.                                                            
017000     ENTRY 'DLITCBL' USING MSG-PCB W6H6A-PCB W6H6-PCB XXJZ-PCB.           
017100     PERFORM IMS-GET-MSG                                                  
017200     IF SEGMENT-FINNS                                                     
017300       PERFORM A-INIT                                                     
017400       PERFORM B-KOLLA-NYCKLAR                                            
017500       IF NYCKLAR-OK                                                      
017600         IF MFS-UPDATE                                                    
017700           PERFORM G-KOLLA-INPUT                                          
017800           IF INDATA-OK                                                   
017900             PERFORM H-UPPDATERA                                          
018000           END-IF                                                         
018100         ELSE                                                             
018200           IF MFS-FIRST                                                   
018300             PERFORM C-FOERSTA-SIDA                                       
018400           ELSE                                                           
018500             IF MFS-NEXT                                                  
018600               PERFORM D-NAESTA-SIDA                                      
018700             ELSE                                                         
018800               PERFORM E-SAMMA-SIDA                                       
018900             END-IF                                                       
019000           END-IF                                                         
019100         END-IF                                                           
019200         IF ALLT-OK                                                       
019300            PERFORM F-LAES-VISA-INFO                                      
019400         END-IF                                                           
019500       END-IF                                                             
019600       MOVE LENGTH OF MOD-W4O12201 TO MSG-KVLL                            
019700       ADD                +4       TO MSG-KVLL                            
019800       PERFORM IMS-INSERT-MSG                                             
019900     END-IF                                                               
020000                                                                          
020100     MOVE ZERO TO RETURN-CODE                                             
020200     GOBACK                                                               
020300     .                                                                    
020400     EJECT                                                                
020500 A-INIT SECTION.                                                          
020600                                                                          
020700     IF MSG-DUBBLA-TRANSKODER                                             
020800        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I12201                
020900        MOVE MSG-IDTRANS-2     TO MFS-IDTRANS                             
021000        MOVE MSG-KDMFSFOR-2    TO MFS-KDMFSFOR                            
021100     ELSE                                                                 
021200        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I12201                 
021300        MOVE MSG-IDTRANS-1     TO MFS-IDTRANS                             
021400        MOVE MSG-KDMFSFOR-1    TO MFS-KDMFSFOR                            
021500     END-IF                                                               
021600                                                                          
021700     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
021800     MOVE MSG-IDPFK            TO MFS-IDPFK                               
021900     MOVE MFS-IDTRANS          TO W-IDTRANS                               
022000                                                                          
022100     MOVE LOW-VALUE            TO MSG-AREA                                
022200     MOVE 'W4O122N1'           TO MFS-IDMOD                               
022300     MOVE '4122'               TO MOD-IDTRANS                             
022400     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
022500                                                                          
022600     IF NOT EGEN-MID                                                      
022700       MOVE SPACE TO MFS-KDTRTYP                                          
022800       MOVE '7'   TO MFS-IDPFK                                            
022900     END-IF                                                               
023000                                                                          
023100     IF ENGLISH-TEXT                                                      
023200       MOVE 'GB'  TO MED-IDSKYLT                                          
023300     ELSE                                                                 
023400       MOVE 'S '  TO MED-IDSKYLT                                          
023500     END-IF                                                               
023600                                                                          
023700     MOVE 'IDAG'          TO DAT-KDDATFORM                                
023800     CALL WDATKONV     USING DAT-KDDATFORM                                
023900                             DAT-I-TIDATUM                                
024000                             DAT-O-TIDATUM                                
024100                             DAT-KDSVAR                                   
024200     MOVE DAT-TIAAMMDD    TO W-DAT-TIAAMMDD                               
024300     .                                                                    
024400     EJECT                                                                
024500 B-KOLLA-NYCKLAR SECTION.                                                 
024600                                                                          
024700     MOVE JA TO NYCKLAR-SW                                                
024800                                                                          
024900     IF NOT EGEN-MID                                                      
025000        MOVE SPACE           TO MID-IDKVAOMR-IN                           
025100        MOVE ZERO            TO MID-IDKVAGRP-IN                           
025200        MOVE ZERO            TO MID-TIAARP-IN                             
025300        MOVE SPACE           TO MID-KDKVASTA-IN                           
025400     END-IF                                                               
025500                                                                          
025600     MOVE MFS-RENSA-FAELT    TO MOD-IDKVAOMR-IN                           
025700                                MOD-IDKVAGRP-IN                           
025800                                MOD-TIAARP-IN                             
025900                                MOD-KDKVASTA-IN                           
026000                                                                          
026100     MOVE LOW-VALUE          TO W2-W6H6ASEQ-X                             
026200     MOVE HIGH-VALUE         TO W3-W6H6ASEQ-X                             
026300                                                                          
026400     IF MID-IDKVAOMR-IN = ALL '+'                                         
026500       MOVE MID-IDKVAOMR-UT  TO IDKVAOMR-WS                               
026600     ELSE                                                                 
026700       MOVE MID-IDKVAOMR-IN  TO IDKVAOMR-WS                               
026800       MOVE '7'              TO MFS-IDPFK                                 
026900       MOVE SPACE            TO MFS-KDTRTYP                               
027000     END-IF                                                               
027100                                                                          
027200     MOVE MID-IDDC-IN  TO WS-IDDC                                         
027300*                                                                         
027400     IF NOT CDC-SE AND NOT SDC-NL AND NOT SDC-GB AND                      
027500        NOT SDC-ES AND NOT SDC-IT AND                                     
027600        NOT SDC-AT                                                        
027700       MOVE MID-IDDC-UT TO WS-IDDC                                        
027800     ELSE                                                                 
027900       MOVE MID-IDDC-IN  TO WS-IDDC                                       
028000       MOVE '7'              TO MFS-IDPFK                                 
028100       MOVE SPACE            TO MFS-KDTRTYP                               
028200     END-IF                                                               
028300                                                                          
028400     IF NOT CDC-SE AND NOT SDC-NL AND NOT SDC-GB AND                      
028500        NOT SDC-ES AND NOT SDC-IT AND                                     
028600        NOT SDC-AT                                                        
028700       MOVE NEJ TO NYCKLAR-SW                                             
028800     ELSE                                                                 
028900       EVALUATE TRUE                                                      
029000          WHEN CDC-SE MOVE  +1 TO  DC-IX                                  
029100          WHEN SDC-NL MOVE  +2 TO  DC-IX                                  
029200          WHEN SDC-GB MOVE  +4 TO  DC-IX                                  
029300          WHEN SDC-ES MOVE  +5 TO  DC-IX                                  
029400          WHEN SDC-IT MOVE  +6 TO  DC-IX                                  
029500          WHEN SDC-AT MOVE  +7 TO  DC-IX                                  
029600          WHEN OTHER  MOVE NEJ TO NYCKLAR-SW                              
029700       END-EVALUATE                                                       
029800     END-IF                                                               
029900                                                                          
030000     IF NYCKLAR-FEL                                                       
030010         CONTINUE                                                         
030020     ELSE                                                                 
030100       IF (IDKVAOMR-WS = KVA-IDKVAOMR (DC-IX 1)                           
030200                     OR  KVA-IDKVAOMR (DC-IX 2)                           
030300                     OR  KVA-IDKVAOMR (DC-IX 3)                           
030400                     OR  KVA-IDKVAOMR (DC-IX 4)                           
030500                     OR  KVA-IDKVAOMR (DC-IX 5)                           
030600                     OR  KVA-IDKVAOMR (DC-IX 6) )                         
030700       OR MFS-UPDATE                                                      
030800           CONTINUE                                                       
030900       ELSE                                                               
031000           MOVE NEJ            TO NYCKLAR-SW                              
031100       END-IF                                                             
031110     END-IF                                                               
031200                                                                          
031300     IF MID-IDKVAGRP-IN = ALL '+'                                         
031400       MOVE MID-IDKVAGRP-UT  TO IDKVAGRP-WS                               
031500       INSPECT IDKVAGRP-WS REPLACING LEADING SPACE BY ZERO                
031600     ELSE                                                                 
031700       IF MID-IDKVAGRP-IN NUMERIC                                         
031800          MOVE MID-IDKVAGRP-IN TO IDKVAGRP-WS                             
031900          MOVE '7'           TO MFS-IDPFK                                 
032000          MOVE SPACE         TO MFS-KDTRTYP                               
032100       ELSE                                                               
032200          MOVE NEJ           TO NYCKLAR-SW                                
032300       END-IF                                                             
032400     END-IF                                                               
032500                                                                          
032600     IF MID-TIAARP-IN = ALL '+'                                           
032700       MOVE MID-TIAARP-UT    TO TIAARP-WS                                 
032800       INSPECT TIAARP-WS REPLACING LEADING SPACE BY ZERO                  
032900     ELSE                                                                 
033000       MOVE MID-TIAARP-IN    TO TIAARP-WS                                 
033100       MOVE '7'              TO MFS-IDPFK                                 
033200       MOVE SPACE            TO MFS-KDTRTYP                               
033300     END-IF                                                               
033400                                                                          
033500     IF TIAARP-WS > ZERO                                                  
033600        MOVE TIAARP-WS       TO DAT-I-TIDATUM                             
033700                                                                          
033800        MOVE 'AARP'          TO DAT-KDDATFORM                             
033900        CALL WDATKONV     USING DAT-KDDATFORM                             
034000                                DAT-I-TIDATUM                             
034100                                DAT-O-TIDATUM                             
034200                                DAT-KDSVAR                                
034300        IF DAT-KDSVAR-OK                                                  
034400           MOVE DAT-TIAAMMDD TO W2-DAREGDAT                               
034500           MOVE DAT-TISEKEL  TO W2-DAREGDAT (1:2)                         
034600        ELSE                                                              
034700           MOVE NEJ          TO NYCKLAR-SW                                
034800        END-IF                                                            
034900     END-IF                                                               
035000                                                                          
035100     IF MID-KDKVASTA-IN = ALL '+'                                         
035200       MOVE MID-KDKVASTA-UT  TO KDKVASTA-WS                               
035300     ELSE                                                                 
035400       IF MID-KDKVASTA-IN NUMERIC                                         
035500          MOVE MID-KDKVASTA-IN TO KDKVASTA-WS                             
035600          MOVE '7'           TO MFS-IDPFK                                 
035700          MOVE SPACE         TO MFS-KDTRTYP                               
035800       ELSE                                                               
035900          MOVE NEJ           TO NYCKLAR-SW                                
036000       END-IF                                                             
036100     END-IF                                                               
036200                                                                          
036300     IF GODK-MID OR NYCKLAR-OK                                            
036400        MOVE WS-IDDC         TO W-IDDC                                    
036500                                W2-IDDC                                   
036600                                W3-IDDC                                   
036700                                W4-IDDC                                   
036800                                MOD-IDDC-UT                               
036900        MOVE IDKVAOMR-WS     TO W2-IDKVAOMR W3-IDKVAOMR                   
037000        MOVE IDKVAGRP-WS     TO W2-IDKVAGRP                               
037100        IF W2-IDKVAGRP NOT = ZERO                                         
037200           MOVE W2-IDKVAGRP  TO W3-IDKVAGRP                               
037300        END-IF                                                            
037400                                                                          
037500        IF TIAARP-WS NOT = ZERO                                           
037600           MOVE W2-DAREGDAT  TO W3-DAREGDAT                               
037700        END-IF                                                            
037800                                                                          
037900        MOVE KDKVASTA-WS     TO W2-KDKVASTA                               
038000        IF W2-KDKVASTA NOT = SPACE                                        
038100           MOVE W2-KDKVASTA  TO W3-KDKVASTA                               
038200        END-IF                                                            
038300                                                                          
038400        MOVE IDKVAOMR-WS     TO MOD-IDKVAOMR-UT                           
038500        MOVE IDKVAGRP-WS     TO MOD-IDKVAGRP-UT                           
038600        INSPECT MOD-IDKVAGRP-UT REPLACING LEADING ZERO BY SPACE           
038700        MOVE TIAARP-WS       TO MOD-TIAARP-UT                             
038800        MOVE KDKVASTA-WS     TO MOD-KDKVASTA-UT                           
038900     ELSE                                                                 
039000        MOVE MFS-RENSA-FAELT TO MOD-IDKVAOMR-UT                           
039100                                MOD-IDKVAGRP-UT                           
039200                                MOD-TIAARP-UT                             
039300                                MOD-KDKVASTA-UT                           
039400                                MOD-IDDC-UT                               
039500     END-IF                                                               
039600                                                                          
039700     IF NYCKLAR-FEL                                                       
039800        MOVE '401'           TO MED-IDMFSFEL                              
039900        CALL WMEDKONV USING MED-WMEDAREA                                  
040000        MOVE MED-MFSFEL      TO MOD-TEMFSFEL                              
040100        PERFORM MFS-RENSA-FAELT-UT                                        
040200     END-IF                                                               
040300     .                                                                    
040400     EJECT                                                                
040500 C-FOERSTA-SIDA SECTION.                                                  
040600                                                                          
040700     PERFORM MFS-RENSA-FAELT-UT                                           
040800     MOVE JA TO ALLT-SW                                                   
040900     .                                                                    
041000     EJECT                                                                
041100 D-NAESTA-SIDA SECTION.                                                   
041200                                                                          
041300     MOVE MID-IDKVAOMR-NX     TO W4-IDKVAOMR                              
041400     MOVE MID-IDKVAGRP-NX     TO W4-IDKVAGRP                              
041500     MOVE MID-TIREGDAT-NX     TO W4-DAREGDAT                              
041600     IF MID-TIREGDAT-NX NOT = ZERO                                        
041700       IF MID-TIREGDAT-NX < 500000                                        
041800         MOVE 20              TO W4-DAREGDAT (1:2)                        
041900       ELSE                                                               
042000         IF MID-TIREGDAT-NX < 999999                                      
042100           MOVE 19            TO W4-DAREGDAT (1:2)                        
042200         ELSE                                                             
042300           MOVE 99999999      TO W4-DAREGDAT                              
042400         END-IF                                                           
042500       END-IF                                                             
042600     END-IF                                                               
042700     MOVE MID-KDKVASTA-NX     TO W4-KDKVASTA                              
042800     MOVE JA                  TO ALLT-SW                                  
042900     .                                                                    
043000     EJECT                                                                
043100 E-SAMMA-SIDA SECTION.                                                    
043200                                                                          
043300     MOVE +1  TO INDX                                                     
043400     MOVE JA  TO W-INDATA                                                 
043500                                                                          
043600     PERFORM UNTIL INDX > MAX-RADER OR W-INDATA = NEJ                     
043700       IF MID-VALKOD (INDX) NOT = ALL '+'                                 
043800          MOVE NEJ               TO W-INDATA                              
043900       END-IF                                                             
044000                                                                          
044100       ADD +1                    TO INDX                                  
044200     END-PERFORM                                                          
044300                                                                          
044400     IF W-INDATA = JA                                                     
044500        MOVE MID-IDKVAOMR-EN     TO W4-IDKVAOMR                           
044600        MOVE MID-IDKVAGRP-EN     TO W4-IDKVAGRP                           
044700        MOVE MID-TIREGDAT-EN     TO W4-DAREGDAT                           
044800        IF MID-TIREGDAT-EN NOT = ZERO                                     
044900          IF MID-TIREGDAT-EN < 500000                                     
045000            MOVE 20              TO W4-DAREGDAT (1:2)                     
045100          ELSE                                                            
045200            IF MID-TIREGDAT-EN < 999999                                   
045300              MOVE 19            TO W4-DAREGDAT (1:2)                     
045400            ELSE                                                          
045500              MOVE 99999999      TO W4-DAREGDAT                           
045600            END-IF                                                        
045700          END-IF                                                          
045800        END-IF                                                            
045900        MOVE MID-KDKVASTA-EN     TO W4-KDKVASTA                           
046000        MOVE JA                  TO ALLT-SW                               
046100     ELSE                                                                 
046200        MOVE NEJ                 TO ALLT-SW                               
046300        MOVE '003'               TO MED-IDMFSFEL                          
046400        CALL WMEDKONV USING MED-WMEDAREA                                  
046500        MOVE MED-MFSFEL          TO MOD-TEMFSFEL                          
046600        PERFORM MFS-ROR-EJ-FAELT-UT                                       
046700        PERFORM MFS-LAS-IN-IGEN                                           
046800     END-IF                                                               
046900     .                                                                    
047000     EJECT                                                                
047100 F-LAES-VISA-INFO SECTION.                                                
047200                                                                          
047300     PERFORM IMS-GU-W6KVAB01-SEQ                                          
047400                                                                          
047500     IF SEGMENT-SAKNAS                                                    
047600        PERFORM IMS-GN-W6KVAB01                                           
047700     END-IF                                                               
047800                                                                          
047900     MOVE +1 TO INDX                                                      
048000                                                                          
048100     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-RADER                     
048200                                                                          
048300        IF INDX = +1                                                      
048400           MOVE OMR-IDKVAOMR     TO MOD-IDKVAOMR-EN                       
048500           MOVE OMR-IDKVAGRP     TO MOD-IDKVAGRP-EN                       
048600           MOVE OMR-DAREGDAT (3:6)    TO MOD-TIREGDAT-EN                  
048700           MOVE OMR-KDKVASTA     TO MOD-KDKVASTA-EN                       
048800        END-IF                                                            
048900                                                                          
049000        MOVE MFS-RENSA-FAELT     TO MOD-VALKOD   (INDX)                   
049100        MOVE OMR-IDKVAOMR        TO MOD-IDKVAOMR (INDX)                   
049200        MOVE OMR-IDKVAGRP        TO MOD-IDKVAGRP (INDX)                   
049300        MOVE OMR-DAREGDAT (3:6)  TO MOD-TIREGDAT (INDX)                   
049400        IF OMR-TIKVAKON = +0                                              
049500           MOVE MFS-RENSA-FAELT  TO MOD-TIKVAKON (INDX)                   
049600        ELSE                                                              
049700           MOVE OMR-TIKVAKON     TO MOD-TIKVAKON (INDX)                   
049800        END-IF                                                            
049900        MOVE OMR-KVART           TO MOD-KVART    (INDX)                   
050000        MOVE OMR-KDKVASTA        TO MOD-KDKVASTA (INDX)                   
050100        IF OMR-TIUPPDAT = +0                                              
050200           MOVE MFS-RENSA-FAELT  TO MOD-TIUPPDAT (INDX)                   
050300        ELSE                                                              
050400           MOVE OMR-TIUPPDAT     TO MOD-TIUPPDAT (INDX)                   
050500        END-IF                                                            
050600                                                                          
050700        ADD +1                   TO INDX                                  
050800                                                                          
050900        PERFORM IMS-GN-W6KVAB01                                           
051000                                                                          
051100        IF SEGMENT-FINNS                                                  
051200           MOVE OMR-IDKVAOMR     TO MOD-IDKVAOMR-NX                       
051300           MOVE OMR-IDKVAGRP     TO MOD-IDKVAGRP-NX                       
051400           MOVE OMR-DAREGDAT (3:6)  TO MOD-TIREGDAT-NX                    
051500           MOVE OMR-KDKVASTA     TO MOD-KDKVASTA-NX                       
051600        ELSE                                                              
051700           MOVE MOD-IDKVAOMR-EN  TO MOD-IDKVAOMR-NX                       
051800           MOVE MOD-IDKVAGRP-EN  TO MOD-IDKVAGRP-NX                       
051900           MOVE MOD-TIREGDAT-EN  TO MOD-TIREGDAT-NX                       
052000           MOVE MOD-KDKVASTA-EN  TO MOD-KDKVASTA-NX                       
052100        END-IF                                                            
052200                                                                          
052300     END-PERFORM                                                          
052400                                                                          
052500     PERFORM UNTIL INDX > MAX-RADER                                       
052600       MOVE MFS-RENSA-FAELT     TO MOD-VALKOD   (INDX)                    
052700                                   MOD-IDKVAOMR (INDX)                    
052800                                   MOD-IDKVAGRP (INDX)                    
052900                                   MOD-TIREGDAT (INDX)                    
053000                                   MOD-TIKVAKON (INDX)                    
053100                                   MOD-KVART    (INDX)                    
053200                                   MOD-KDKVASTA (INDX)                    
053300                                   MOD-TIUPPDAT (INDX)                    
053400                                                                          
053500       ADD +1                   TO INDX                                   
053600     END-PERFORM                                                          
053700                                                                          
053800     IF NOT MFS-UPDATE                                                    
053900        IF SEGMENT-FINNS                                                  
054000           MOVE '105'           TO MED-IDMFSFEL                           
054100           CALL WMEDKONV USING MED-WMEDAREA                               
054200           MOVE MED-MFSFEL      TO MOD-TEMFSINF                           
054300        END-IF                                                            
054400     END-IF                                                               
054500     .                                                                    
054600     EJECT                                                                
054700 G-KOLLA-INPUT SECTION.                                                   
054800     SKIP2                                                                
054900     MOVE NEJ   TO W-INDATA                                               
055000     MOVE +1    TO INDX                                                   
055100                                                                          
055200     PERFORM UNTIL INDX > MAX-RADER                                       
055300                                                                          
055400       MOVE JA  TO INDATA-SW                                              
055500                                                                          
055600       IF MID-VALKOD (INDX) NOT = '+' AND ' '                             
055700          MOVE MID-IDKVAOMR (INDX) TO W-IDKVAOMR                          
055800          MOVE MID-IDKVAGRP (INDX) TO W-IDKVAGRP                          
055900          INSPECT W-IDKVAGRP REPLACING LEADING SPACE BY ZERO              
056000          MOVE MID-TIREGDAT (INDX) TO W-DAREGDAT                          
056100          IF MID-TIREGDAT (INDX) NOT = ZERO                               
056200            IF MID-TIREGDAT (INDX) < 500000                               
056300              MOVE 20              TO W-DAREGDAT (1:2)                    
056400            ELSE                                                          
056500              IF MID-TIREGDAT (INDX) < 999999                             
056600                MOVE 19            TO W-DAREGDAT (1:2)                    
056700              ELSE                                                        
056800                MOVE 99999999      TO W-DAREGDAT                          
056900              END-IF                                                      
057000            END-IF                                                        
057100          END-IF                                                          
057200                                                                          
057300          PERFORM IMS-GU-W6KVAB01                                         
057400          IF SEGMENT-FINNS                                                
057500             IF   W-DAT-TIAAMMDD    = OMR-TIUPPDAT                        
057600             AND (MID-VALKOD (INDX) = '1' OR '2')                         
057700                MOVE NEJ           TO INDATA-SW                           
057800             ELSE                                                         
057900                MOVE JA               TO W-INDATA                         
058000                IF MID-VALKOD (INDX) > '3'                                
058100                OR MID-VALKOD (INDX) ALPHABETIC                           
058200                   MOVE NEJ            TO INDATA-SW                       
058300                ELSE                                                      
058400                   IF MID-VALKOD (INDX) = '0'                             
058500                      IF OMR-KDKVASTA > '1'                               
058600                         MOVE NEJ          TO INDATA-SW                   
058700                      END-IF                                              
058800                   ELSE                                                   
058900                      IF MID-VALKOD (INDX) = '1'                          
059000                         IF OMR-KDKVASTA = '3'                            
059100                            MOVE NEJ       TO INDATA-SW                   
059200                         END-IF                                           
059300                      ELSE                                                
059400                         IF MID-VALKOD (INDX) = '2'                       
059500                            IF OMR-KDKVASTA = '0'                         
059600                               MOVE NEJ    TO INDATA-SW                   
059700                            END-IF                                        
059800                         ELSE                                             
059900                            IF MID-VALKOD (INDX) = '3'                    
060000                               IF OMR-KDKVASTA NOT = +2                   
060100                                  MOVE NEJ  TO INDATA-SW                  
060200                               END-IF                                     
060300                            END-IF                                        
060400                         END-IF                                           
060500                      END-IF                                              
060600                   END-IF                                                 
060700                END-IF                                                    
060800             END-IF                                                       
060900          END-IF                                                          
061000       END-IF                                                             
061100                                                                          
061200       IF INDATA-FEL                                                      
061300          MOVE JA                 TO W-FEL-UPDATE                         
061400          MOVE MFS-ALFA-FAELT-FEL TO MOD-VALKOD-ATTR (INDX)               
061500       ELSE                                                               
061600          MOVE MFS-ALFA-FAELT-RAETT TO MOD-VALKOD-ATTR (INDX)             
061700       END-IF                                                             
061800                                                                          
061900       ADD +1                     TO INDX                                 
062000                                                                          
062100     END-PERFORM                                                          
062200                                                                          
062300     IF W-FEL-UPDATE = JA                                                 
062400        MOVE NEJ               TO INDATA-SW                               
062500        MOVE NEJ               TO ALLT-SW                                 
062600        MOVE '001'             TO MED-IDMFSFEL                            
062700        CALL WMEDKONV USING MED-WMEDAREA                                  
062800        MOVE MED-MFSFEL        TO MOD-TEMFSFEL                            
062900        PERFORM MFS-ROR-EJ-FAELT-UT                                       
063000     ELSE                                                                 
063100        IF W-INDATA = NEJ                                                 
063200           MOVE NEJ                  TO INDATA-SW                         
063300           MOVE NEJ                  TO ALLT-SW                           
063400           MOVE '011'                TO MED-IDMFSFEL                      
063500           CALL WMEDKONV USING MED-WMEDAREA                               
063600           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
063700           PERFORM MFS-ROR-EJ-FAELT-UT                                    
063800        END-IF                                                            
063900     END-IF                                                               
064000     .                                                                    
064100     EJECT                                                                
064200 H-UPPDATERA SECTION.                                                     
064300     SKIP2                                                                
064400     MOVE +1  TO INDX                                                     
064500                                                                          
064600     PERFORM UNTIL INDX > MAX-RADER                                       
064700       IF MID-VALKOD (INDX) NOT = '+' AND ' '                             
064800          MOVE MID-IDKVAOMR  (INDX)  TO W-IDKVAOMR                        
064900          MOVE MID-IDKVAGRP  (INDX)  TO W-IDKVAGRP                        
065000          INSPECT W-IDKVAGRP REPLACING LEADING SPACE BY ZERO              
065100          MOVE MID-TIREGDAT  (INDX)  TO W-DAREGDAT                        
065200          IF MID-TIREGDAT  (INDX) NOT = ZERO                              
065300            IF MID-TIREGDAT  (INDX) < 500000                              
065400              MOVE 20                TO W-DAREGDAT (1:2)                  
065500            ELSE                                                          
065600              IF MID-TIREGDAT  (INDX) < 999999                            
065700                MOVE 19              TO W-DAREGDAT (1:2)                  
065800              ELSE                                                        
065900                MOVE 99999999        TO W-DAREGDAT                        
066000              END-IF                                                      
066100            END-IF                                                        
066200          END-IF                                                          
066300          PERFORM IMS-GHU-W6KVAB01                                        
066400                                                                          
066500          IF SEGMENT-FINNS                                                
066600             MOVE MID-VALKOD (INDX)  TO OMR-KDKVASTA                      
066700             MOVE W-DAT-TIAAMMDD     TO OMR-TIUPPDAT                      
066800                                                                          
066900             IF  OMR-KDKVASTA = '2'                                       
067000             AND OMR-TIKVAKON = +0                                        
067100                 MOVE W-DAT-TIAAMMDD TO OMR-TIKVAKON                      
067200             END-IF                                                       
067300             PERFORM IMS-REPL-W6KVAB01                                    
067400             IF MID-VALKOD (INDX) = '1' OR '2'                            
067500                PERFORM HA-SKRIV-LISTPOST                                 
067600             END-IF                                                       
067700          END-IF                                                          
067800       END-IF                                                             
067900                                                                          
068000       ADD +1                        TO INDX                              
068100     END-PERFORM                                                          
068200                                                                          
068300     MOVE JA                         TO ALLT-SW                           
068400     MOVE '101'                     TO MED-IDMFSINF                       
068500     CALL WMEDKONV USING MED-WMEDAREA                                     
068600     MOVE MED-MFSINF                 TO MOD-TEMFSINF                      
068700     PERFORM MFS-FORM-ATTR                                                
068800     PERFORM MFS-RENSA-FAELT-IN                                           
068900     .                                                                    
069000     EJECT                                                                
069100 HA-SKRIV-LISTPOST SECTION.                                               
069200     SKIP2                                                                
069300     MOVE OMR-IDDC         TO 4824-IDDC                                   
069400     MOVE OMR-IDKVAOMR     TO 4824-IDKVAOMR                               
069500     MOVE OMR-IDKVAGRP     TO 4824-IDKVAGRP                               
069600     MOVE OMR-DAREGDAT (3:6) TO 4824-TIREGDAT                             
069700     PERFORM IMS-ISRT-WLXXJZ11                                            
069800     .                                                                    
069900     EJECT                                                                
070000 MFS-RENSA-FAELT-IN SECTION.                                              
070100     SKIP2                                                                
070200     MOVE +1 TO INDX                                                      
070300     PERFORM UNTIL INDX > MAX-RADER                                       
070400       MOVE MFS-RENSA-FAELT TO MOD-VALKOD (INDX)                          
070500       ADD +1 TO INDX                                                     
070600     END-PERFORM                                                          
070700     .                                                                    
070800     SKIP2                                                                
070900 MFS-RENSA-FAELT-UT SECTION.                                              
071000     SKIP2                                                                
071100     MOVE MFS-RENSA-FAELT TO MOD-IDKVAOMR-EN                              
071200                             MOD-IDKVAGRP-EN                              
071300                             MOD-TIREGDAT-EN                              
071400                             MOD-KDKVASTA-EN                              
071500                                                                          
071600                             MOD-IDKVAOMR-NX                              
071700                             MOD-IDKVAGRP-NX                              
071800                             MOD-TIREGDAT-NX                              
071900                             MOD-KDKVASTA-NX                              
072000                                                                          
072100     PERFORM MFS-RENSA-FAELT-UT-RAD                                       
072200     .                                                                    
072300     EJECT                                                                
072400 MFS-RENSA-FAELT-UT-RAD SECTION.                                          
072500     SKIP2                                                                
072600     MOVE +1 TO INDX                                                      
072700     PERFORM UNTIL INDX > MAX-RADER                                       
072800       MOVE MFS-RENSA-FAELT TO MOD-VALKOD          (INDX)                 
072900                               MOD-IDKVAOMR        (INDX)                 
073000                               MOD-IDKVAGRP        (INDX)                 
073100                               MOD-TIREGDAT        (INDX)                 
073200                               MOD-TIKVAKON        (INDX)                 
073300                               MOD-KVART           (INDX)                 
073400                               MOD-KDKVASTA        (INDX)                 
073500                               MOD-TIUPPDAT        (INDX)                 
073600       ADD +1 TO INDX                                                     
073700     END-PERFORM                                                          
073800     .                                                                    
073900     EJECT                                                                
074000 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
074100     SKIP2                                                                
074200     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDKVAOMR-EN                          
074300                                 MOD-IDKVAGRP-EN                          
074400                                 MOD-TIREGDAT-EN                          
074500                                 MOD-KDKVASTA-EN                          
074600                                                                          
074700                                 MOD-IDKVAOMR-NX                          
074800                                 MOD-IDKVAGRP-NX                          
074900                                 MOD-TIREGDAT-NX                          
075000                                 MOD-KDKVASTA-NX                          
075100                                                                          
075200     MOVE +1 TO INDX                                                      
075300     PERFORM UNTIL INDX > MAX-RADER                                       
075400       MOVE MFS-ROER-EJ-FAELT TO MOD-VALKOD          (INDX)               
075500                                 MOD-IDKVAOMR        (INDX)               
075600                                 MOD-IDKVAGRP        (INDX)               
075700                                 MOD-TIREGDAT        (INDX)               
075800                                 MOD-TIKVAKON        (INDX)               
075900                                 MOD-KVART           (INDX)               
076000                                 MOD-KDKVASTA        (INDX)               
076100                                 MOD-TIUPPDAT        (INDX)               
076200       ADD +1 TO INDX                                                     
076300     END-PERFORM                                                          
076400     .                                                                    
076500     EJECT                                                                
076600 MFS-FORM-ATTR SECTION.                                                   
076700     SKIP2                                                                
076800     MOVE +1 TO INDX                                                      
076900     PERFORM UNTIL INDX > MAX-RADER                                       
077000       MOVE MFS-FORMATETS-ATTR    TO MOD-VALKOD-ATTR (INDX)               
077100       ADD +1 TO INDX                                                     
077200     END-PERFORM                                                          
077300     .                                                                    
077400     SKIP2                                                                
077500 MFS-LAS-IN-IGEN SECTION.                                                 
077600     SKIP2                                                                
077700     MOVE +1 TO INDX                                                      
077800     PERFORM UNTIL INDX > MAX-RADER                                       
077900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VALKOD-ATTR (INDX)               
078000       ADD +1 TO INDX                                                     
078100     END-PERFORM                                                          
078200     .                                                                    
078300     EJECT                                                                
078400* IMS SEKTIONER                                                           
078500     SKIP3                                                                
078600 IMS-GET-MSG SECTION.                                                     
078700                                                                          
078800     MOVE '  QC' TO GODK-STATUSKODER                                      
078900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
079000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
079100     PERFORM IMS-STATUSKONTROLL                                           
079200     .                                                                    
079300     SKIP3                                                                
079400 IMS-INSERT-MSG SECTION.                                                  
079500                                                                          
079600     IF NOT ENGLISH-TEXT                                                  
079700       MOVE '0' TO MFS-KDHUVOMR                                           
079800     END-IF                                                               
079900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
080000     MOVE SPACE TO GODK-STATUSKODER                                       
080100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
080200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
080300     PERFORM IMS-STATUSKONTROLL                                           
080400     .                                                                    
080500     EJECT                                                                
080600 IMS-GU-W6KVAB01-SEQ SECTION.                                             
080700                                                                          
080800     STRING 'W6KVAB01(W6H6ASEQ =' W4-W6H6ASEQ-X ')'                       
080900          DELIMITED BY SIZE INTO SSA1                                     
081000     MOVE '  GE'              TO GODK-STATUSKODER                         
081100     CALL CBLTDLI USING GU W6H6A-PCB DLI-IO-AREA1 SSA1                    
081200     MOVE W6H6A-STATUS-CODE TO STATUS-WS                                  
081300     PERFORM IMS-STATUSKONTROLL                                           
081400     .                                                                    
081500     SKIP2                                                                
081600 IMS-GN-W6KVAB01 SECTION.                                                 
081700                                                                          
081800     STRING 'W6KVAB01(W6H6ASEQ=>' W2-W6H6ASEQ-X                           
081900                    '&W6H6ASEQ=<' W3-W6H6ASEQ-X                           
082000                    '&DAREGDAT=>' W2-DAREGDAT-X                           
082100                    '&DAREGDAT=<' W3-DAREGDAT-X                           
082200                    '&KDKVASTA=>' W2-KDKVASTA-X                           
082300                    '&KDKVASTA=<' W3-KDKVASTA-X ')'                       
082400          DELIMITED BY SIZE INTO SSA1                                     
082500     MOVE '  GE'              TO GODK-STATUSKODER                         
082600     CALL CBLTDLI USING GN W6H6A-PCB DLI-IO-AREA1 SSA1                    
082700     MOVE W6H6A-STATUS-CODE TO STATUS-WS                                  
082800     PERFORM IMS-STATUSKONTROLL                                           
082900     .                                                                    
083000     SKIP2                                                                
083100 IMS-GU-W6KVAB01 SECTION.                                                 
083200                                                                          
083300     STRING 'W6KVAB01(W6H601KY =' W-W6H601KY-X ')'                        
083400          DELIMITED BY SIZE INTO SSA1                                     
083500     MOVE '  GE' TO GODK-STATUSKODER                                      
083600     CALL CBLTDLI USING GU W6H6-PCB DLI-IO-AREA1 SSA1                     
083700     MOVE W6H6-STATUS-CODE TO STATUS-WS                                   
083800     PERFORM IMS-STATUSKONTROLL                                           
083900     .                                                                    
084000     SKIP2                                                                
084100 IMS-GHU-W6KVAB01 SECTION.                                                
084200                                                                          
084300     STRING 'W6KVAB01(W6H601KY =' W-W6H601KY-X ')'                        
084400          DELIMITED BY SIZE INTO SSA1                                     
084500     MOVE '  GE' TO GODK-STATUSKODER                                      
084600     CALL CBLTDLI USING GHU W6H6-PCB DLI-IO-AREA1 SSA1                    
084700     MOVE W6H6-STATUS-CODE TO STATUS-WS                                   
084800     PERFORM IMS-STATUSKONTROLL                                           
084900     .                                                                    
085000     SKIP2                                                                
085100 IMS-REPL-W6KVAB01 SECTION.                                               
085200                                                                          
085300     MOVE '  ' TO GODK-STATUSKODER                                        
085400     CALL CBLTDLI USING REPL W6H6-PCB DLI-IO-AREA1                        
085500     MOVE W6H6-STATUS-CODE TO STATUS-WS                                   
085600     PERFORM IMS-STATUSKONTROLL                                           
085700     .                                                                    
085800     EJECT                                                                
085900 IMS-ISRT-WLXXJZ11 SECTION.                                               
086000                                                                          
086100     STRING 'WLXXJZ01(WDGXKEY  =' W-WDGX-4823-KEY-X ')'                   
086200          DELIMITED BY SIZE INTO SSA1                                     
086300     MOVE 'WLXXJZ11 '         TO SSA2                                     
086400     MOVE '  II' TO GODK-STATUSKODER                                      
086500     CALL CBLTDLI USING ISRT XXJZ-PCB DLI-IO-AREA2 SSA1 SSA2              
086600     MOVE XXJZ-STATUS-CODE TO STATUS-WS                                   
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     .                                                                    
086900     SKIP3                                                                
087000 IMS-STATUSKONTROLL SECTION.                                              
087100                                                                          
087200     SET STATUS-IX TO 1                                                   
087300     SEARCH GODK-STATUS                                                   
087400       AT END CALL FELLOG                                                 
087500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
087600     END-SEARCH                                                           
087700     .                                                                    
