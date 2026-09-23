000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4012300.                                                
000400 AUTHOR.         LENA LINDBLOM.                                           
000500 DATE-WRITTEN.   MARS. 90.                                                
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION.                                                            
001000*        UPPDATERING,NYUPPLÄGGNING,BORTTAG OCH FRÅGEPROGRAM               
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T123                                              
001400*        MID:         W4I12301                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O12301                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002301                                                                          
002310*    -- CHECKED BY WY2000                                                 
002400 77  IDPGM                   PIC X(8)    VALUE 'W4012300'.                
002500 77  JA                      PIC X       VALUE 'J'.                       
002600 77  NEJ                     PIC X       VALUE 'N'.                       
003000 77  IX-RAD                  PIC S9(9)   VALUE +0   COMP SYNC.            
003100 77  IX-SPALT                PIC S9(9)   VALUE +0   COMP SYNC.            
003200 77  MAX-RADER               PIC S9(9)   VALUE +14  COMP SYNC.            
003300 77  MAX-SPALTER             PIC S9(9)   VALUE +4   COMP SYNC.            
003500 77  WS-IDKVAOMR             PIC X       VALUE SPACE.                     
003600 77  WS-IDKVAGRP             PIC X(3)    VALUE SPACE.                     
003700 77  WS-TIAARP               PIC X(4)    VALUE SPACE.                     
003800 77  WS-DAREGDAT             PIC 9(8)    VALUE ZERO.                      
004100 77  LAGERINTERVALL-SW       PIC X.                                       
004200                                                                          
004420 77  ARTIKEL-SW              PIC X       VALUE 'J'.                       
004430   88  ARTIKEL-OK                        VALUE 'J'.                       
004500   88  ARTIKEL-FEL                       VALUE 'N'.                       
004600                                                                          
004700 77  INDATA-SW               PIC X       VALUE 'J'.                       
004800   88  INDATA-OK                         VALUE 'J'.                       
004900   88  INDATA-FEL                        VALUE 'N'.                       
005000                                                                          
005100 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
005200   88  NYCKLAR-OK                        VALUE 'J'.                       
005300   88  NYCKLAR-FEL                       VALUE 'N'.                       
005400                                                                          
005500 77  VISA-SW                 PIC X       VALUE 'J'.                       
005600   88  VISA-INFO                         VALUE 'J'.                       
005700                                                                          
005800 77  ALLT-SW                 PIC X       VALUE 'J'.                       
005900   88  ALLT-OK                           VALUE 'J'.                       
006000                                                                          
006100 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
006200   88  EGEN-MID                          VALUE '4123'.                    
006300   88  GODK-MID                          VALUE '4115' '4121'              
006400                                               '4122'.                    
006500     EJECT                                                                
006501*    --- VALID IDDC CODES                                                 
006502*                                                                         
006503*01  -COPY WWDC99                                                         
006510     EJECT                                                                
006600 01  GENERELLA-SUBPROGRAM.                                                
006700   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
006800   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
006900   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
007000   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
007100   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
007200     EJECT                                                                
007300*   -COPY WDECAREA                                                        
007500     EJECT                                                                
007600*   -COPY WMEDAREA                                                        
007800     EJECT                                                                
007900*   -COPY WDATAREA                                                        
008100     EJECT                                                                
008200******************************************************************        
008300*                                                                         
008400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
008500*                                                                         
008600 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
008700     SKIP3                                                                
008800*01  MID -COPY W4I12301                                                   
009000     EJECT                                                                
009100*01  -COPY WMSGAREA                                                       
009300     EJECT                                                                
009400*  03  MOD -COPY W4O12301 -RED MSG-AREA.                                  
009600     EJECT                                                                
009700*01  -COPY WMFSAREA                                                       
009900     EJECT                                                                
010000******************************************************************        
010100*                                                                         
010200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010300*                                                                         
010400 01  IMS-WS.                                                              
010500   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
010600     SKIP3                                                                
010700 01  NYCKLAR-TILL-DLI.                                                    
010800   03  W-IDARTNR-X.                                                       
010900     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
011000   03  W-KVAB01-KEY-X.                                                    
011100     05  W-IDDC              PIC XX      VALUE SPACE.                     
011200     05  W-IDKVAOMR          PIC X       VALUE SPACE.                     
011300     05  W-IDKVAGRP          PIC 9(3)    VALUE ZERO.                      
011400     05  W-DAREGDAT          PIC  9(8)   VALUE ZERO.                      
011500   03  W-KVAB11-KEY-X.                                                    
011600     05  W-IDARTNR-11        PIC S9(9)   VALUE ZERO  COMP-3.              
011700     05  W-IDKVAFEL-11       PIC 9(2)    VALUE ZERO.                      
011800     SKIP3                                                                
011900   03  W-WDGX-4821-KEY-X.                                                 
012000     05  W-IDHTYP-4821       PIC X(04)   VALUE '4821'.                    
012100     05  FILLER              PIC X(26)   VALUE LOW-VALUE.                 
012200   03  W-WDGX-4822-KEY-X.                                                 
012300     05  W-IDKVAFEL          PIC X(02)   VALUE SPACE.                     
012400     05  FILLER              PIC X(03)   VALUE LOW-VALUE.                 
012500     SKIP3                                                                
012513   03  W-W6H501KY-MIN-X.                                                  
012514      05  W-IDDC-MIN         PIC  X(02)  VALUE LOW-VALUE.                 
012515      05  W-IDKVAOMR-MIN     PIC  X(01)  VALUE SPACE.                     
012516      05  W-IDKVATRG-MIN     PIC  9(02)  VALUE ZERO.                      
012517      05  W-IDKVAGRP-MIN     PIC  9(03)  VALUE ZERO.                      
012518      05  W-ADLAGOMR-FOM-MIN PIC S9(03)  VALUE +0    COMP-3.              
012519      05  W-ADGANG-FOM-MIN   PIC S9(03)  VALUE +0    COMP-3.              
012520      05  W-ADPLATS-FOM-MIN  PIC S9(05)  VALUE +0    COMP-3.              
012521     SKIP3                                                                
012522   03  W-W6H501KY-MAX-X.                                                  
012523      05  W-IDDC-MAX         PIC  X(02)  VALUE HIGH-VALUE.                
012525      05  W-IDKVAOMR-MAX     PIC  X(01)  VALUE HIGH-VALUE.                
012526      05  W-IDKVATRG-MAX     PIC  9(02)  VALUE 99.                        
012527      05  W-IDKVAGRP-MAX     PIC  9(03)  VALUE 999.                       
012528      05  W-ADLAGOMR-FOM-MAX PIC S9(03)  VALUE +999  COMP-3.              
012529      05  W-ADGANG-FOM-MAX   PIC S9(03)  VALUE +999  COMP-3.              
012530      05  W-ADPLATS-FOM-MAX  PIC S9(05)  VALUE +99999 COMP-3.             
012531                                                                          
012540                                                                          
013300*                        **** STATUS-KOD FRÅN IMS                         
013400   03  STATUS-WS             PIC XX.                                      
013500     88  SEGMENT-FINNS                   VALUE '  '.                      
013600     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
013700     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
013800     SKIP3                                                                
013900   03  GODK-STATUSKODER.                                                  
014000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014100     SKIP3                                                                
014200 01    SSA1                  PIC X(64).                                   
014300 01    SSA2                  PIC X(64).                                   
014500     EJECT                                                                
014600*                            IMS FUNKTIONSKODER                           
014700*01    -COPY W0003                                                        
014900     EJECT                                                                
015000*                            DLI INPUT-OUTPUT AREA                        
015100 01  DLI-IO-AREA.                                                         
015200   03  IO-AREA               PIC X(400)  VALUE SPACE.                     
015300     SKIP3                                                                
015400*  03  W6KVAB01  -COPY W6H601  -PRE KVAB-  -RED IO-AREA.                  
015600     EJECT                                                                
015700*  03  W6KVAB11  -COPY W6H611  -PRE KVAB-  -RED IO-AREA.                  
015900     EJECT                                                                
016000*  03  WLXXJY01  -COPY WDGX01  -PRE XXJY-  -RED IO-AREA.                  
016200     EJECT                                                                
016300*  03  WLXXJY11  -COPY WDGX4822  -PRE XXJY-  -RED IO-AREA.                
016500     EJECT                                                                
016600 01  DLI-IO-AREA2.                                                        
016700   03  IO-AREA2              PIC X(50)  VALUE SPACE.                      
016800     SKIP3                                                                
016900*  03  W6H501    -COPY W6H501                -RED IO-AREA2.               
017100     EJECT                                                                
017200 01  DLI-IO-AREA3.                                                        
017500*  03  WDK611    -COPY WDK611     -PRE ARTC-                              
017700     EJECT                                                                
017800 LINKAGE SECTION.                                                         
017900*01  -COPY W0009     -PRE MSG-                                            
018100     EJECT                                                                
018200*01  -COPY W0008     -PRE KVAB-                                           
018400     05  FILLER              PIC X(23).                                   
018500     EJECT                                                                
018600*01  -COPY W0008     -PRE XXJY-                                           
018800     05  FILLER              PIC X(50).                                   
018900     EJECT                                                                
019000*01  -COPY W0008     -PRE KVAA-                                           
019200     05  FILLER              PIC X(50).                                   
019300     EJECT                                                                
019400*01  -COPY W0008     -PRE ARTC-                                           
019600     05  FILLER              PIC X(50).                                   
019700     EJECT                                                                
019800 PROCEDURE DIVISION USING  MSG-PCB KVAB-PCB XXJY-PCB                      
019900                                   KVAA-PCB ARTC-PCB.                     
019910 MAIN SECTION.                                                            
020000     ENTRY 'DLITCBL' USING MSG-PCB KVAB-PCB XXJY-PCB                      
020100                                   KVAA-PCB ARTC-PCB.                     
020200                                                                          
020300     PERFORM IMS-GET-MSG                                                  
020400     IF SEGMENT-FINNS                                                     
020500       PERFORM A-INIT                                                     
020600       PERFORM B-KOLLA-NYCKLAR                                            
020700       IF NYCKLAR-OK                                                      
020800         IF MFS-UPDATE                                                    
020900           PERFORM G-KOLLA-INPUT                                          
021000           IF INDATA-OK                                                   
021100             PERFORM H-UPPDATERA                                          
021200           END-IF                                                         
021300         ELSE                                                             
021400           IF MFS-FIRST                                                   
021500             PERFORM C-FOERSTA-SIDA                                       
021600           ELSE                                                           
021700             IF MFS-NEXT                                                  
021800               PERFORM D-NAESTA-SIDA                                      
021900             ELSE                                                         
022000               PERFORM E-SAMMA-SIDA                                       
022100             END-IF                                                       
022200           END-IF                                                         
022300           IF ALLT-OK AND VISA-INFO                                       
022400             PERFORM S01-LAES-VISA-INFO                                   
022500             PERFORM MFS-FORM-ATTR                                        
022600           END-IF                                                         
022700         END-IF                                                           
022800       END-IF                                                             
022801       MOVE LENGTH OF MOD-W4O12301 TO MSG-KVLL                            
022810       ADD                +4       TO MSG-KVLL                            
023000       PERFORM IMS-INSERT-MSG                                             
023100     END-IF                                                               
023200                                                                          
023300     MOVE ZERO TO RETURN-CODE                                             
023400     GOBACK                                                               
023500     .                                                                    
023600     EJECT                                                                
023700 A-INIT SECTION.                                                          
023800                                                                          
023900     IF MSG-DUBBLA-TRANSKODER                                             
024000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I12301                 
024100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
024200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024300     ELSE                                                                 
024400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I12301                  
024500       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
024600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024700     END-IF                                                               
024800                                                                          
024900     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
025000     MOVE MSG-IDPFK TO MFS-IDPFK                                          
025100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
025200                                                                          
025300     MOVE LOW-VALUE TO MSG-AREA                                           
025400     MOVE 'W4O123N1' TO MFS-IDMOD                                         
025500     MOVE '4123' TO MOD-IDTRANS                                           
025600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
025700                                                                          
025800     IF NOT EGEN-MID                                                      
025900       MOVE SPACE TO MFS-KDTRTYP                                          
026000       MOVE '7' TO MFS-IDPFK                                              
026100     END-IF                                                               
026200                                                                          
026300     IF ENGLISH-TEXT                                                      
026600       MOVE 'GB ' TO MED-IDSKYLT                                          
026700     ELSE                                                                 
027000       MOVE 'S  ' TO MED-IDSKYLT                                          
027100     END-IF                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 B-KOLLA-NYCKLAR SECTION.                                                 
027500                                                                          
027600     IF NOT EGEN-MID                                                      
027700       MOVE SPACE TO MID-IDKVAOMRI                                        
027800       MOVE ZERO TO MID-IDKVAGRPI                                         
027900                    MID-TIAARPI                                           
028000     END-IF                                                               
028100     MOVE JA TO NYCKLAR-SW                                                
028200     MOVE MFS-RENSA-FAELT TO MOD-IDKVAOMRI                                
028300                             MOD-IDKVAGRPI                                
028400                             MOD-TIAARPI                                  
028500                                                                          
028600     IF MID-IDKVAOMRI = ALL '+'                                           
028700       MOVE MID-IDKVAOMRU TO  WS-IDKVAOMR                                 
028800     ELSE                                                                 
028900       MOVE MID-IDKVAOMRI   TO WS-IDKVAOMR                                
029000       MOVE '7'             TO MFS-IDPFK                                  
029100       MOVE SPACE           TO MFS-KDTRTYP                                
029200     END-IF                                                               
029300                                                                          
029400     IF MID-IDKVAGRPI = ALL '+'                                           
029500       MOVE MID-IDKVAGRPU TO  WS-IDKVAGRP                                 
029600       INSPECT WS-IDKVAGRP REPLACING LEADING SPACE BY ZERO                
029700     ELSE                                                                 
029800       MOVE MID-IDKVAGRPI   TO WS-IDKVAGRP                                
029900       MOVE '7'             TO MFS-IDPFK                                  
030000       MOVE SPACE           TO MFS-KDTRTYP                                
030100     END-IF                                                               
030200                                                                          
030300     IF MID-TIAARPI = ALL '+'                                             
030400       MOVE MID-TIAARPU TO  WS-TIAARP                                     
030500     ELSE                                                                 
030600       MOVE MID-TIAARPI     TO WS-TIAARP                                  
030700       MOVE '7'             TO MFS-IDPFK                                  
030800       MOVE SPACE           TO MFS-KDTRTYP                                
030900     END-IF                                                               
031000                                                                          
031011     MOVE MID-IDDCIN TO WS-IDDC                                           
031012                                                                          
031013     IF NOT CDC-SE AND NOT SDC-NL AND NOT SDC-GB AND                      
031014        NOT SDC-ES AND NOT SDC-IT AND                                     
031015        NOT SDC-AT                                                        
031020       MOVE MID-IDDCUT     TO WS-IDDC                                     
031030     ELSE                                                                 
031040       MOVE MID-IDDCIN      TO WS-IDDC                                    
031050       MOVE '7'             TO MFS-IDPFK                                  
031060       MOVE SPACE           TO MFS-KDTRTYP                                
031070     END-IF                                                               
031080                                                                          
031090     IF NOT CDC-SE AND NOT SDC-NL AND NOT SDC-GB AND                      
031091        NOT SDC-ES AND NOT SDC-IT AND                                     
031092        NOT SDC-AT                                                        
031094       MOVE NEJ TO NYCKLAR-SW                                             
031095     END-IF                                                               
031096                                                                          
031100     MOVE WS-TIAARP TO DAT-I-TIDATUM                                      
031200     MOVE 'AARP' TO DAT-KDDATFORM                                         
031300                                                                          
031400     CALL WDATKONV USING DAT-KDDATFORM,                                   
031500                         DAT-I-TIDATUM,                                   
031600                         DAT-O-TIDATUM,                                   
031700                         DAT-KDSVAR                                       
031800                                                                          
031900     IF DAT-KDSVAR-OK                                                     
032000       MOVE DAT-TIAAMMDD TO WS-DAREGDAT                                   
032010       MOVE DAT-TISEKEL  TO WS-DAREGDAT (1:2)                             
032100     ELSE                                                                 
032200       MOVE NEJ TO NYCKLAR-SW                                             
032300     END-IF                                                               
032400                                                                          
032500     IF GODK-MID OR NYCKLAR-OK                                            
032600       MOVE WS-IDKVAOMR TO MOD-IDKVAOMRU                                  
032610                                                                          
032700       MOVE WS-IDKVAGRP TO MOD-IDKVAGRPU                                  
032800       INSPECT MOD-IDKVAGRPU REPLACING LEADING ZERO BY SPACE              
032810                                                                          
032900       MOVE WS-TIAARP TO MOD-TIAARPU                                      
033001                                                                          
033010       MOVE WS-IDDC   TO MOD-IDDCUT                                       
033100     ELSE                                                                 
033200       MOVE MFS-RENSA-FAELT TO MOD-IDKVAOMRU                              
033300                               MOD-IDKVAGRPU                              
033400                               MOD-TIAARPU                                
033410                               MOD-IDDCUT                                 
033500     END-IF                                                               
033600                                                                          
033700     IF NYCKLAR-OK                                                        
033800       MOVE WS-IDDC     TO W-IDDC     W-IDDC-MIN                          
033810                                      W-IDDC-MAX                          
033900       MOVE WS-IDKVAOMR TO W-IDKVAOMR W-IDKVAOMR-MIN                      
033910                                      W-IDKVAOMR-MAX                      
034000       MOVE WS-IDKVAGRP TO W-IDKVAGRP W-IDKVAGRP-MIN                      
034010                                      W-IDKVAGRP-MAX                      
034100       MOVE WS-DAREGDAT TO W-DAREGDAT                                     
034200       PERFORM IMS-GU-W6KVAB01                                            
034300       IF SEGMENT-SAKNAS                                                  
034400         MOVE '010' TO MED-IDMFSFEL                                       
034500         MOVE NEJ TO NYCKLAR-SW                                           
034600       ELSE                                                               
034700         IF KVAB-OMR-KDKVASTA = '0'                                       
034800           MOVE '401' TO MED-IDMFSFEL                                     
034900           MOVE NEJ TO NYCKLAR-SW                                         
035000         ELSE                                                             
035100           IF KVAB-OMR-KDKVASTA = '3'                                     
035200             IF MFS-UPDATE                                                
035300               MOVE '007' TO MED-IDMFSFEL                                 
035400               MOVE NEJ TO NYCKLAR-SW                                     
035500             END-IF                                                       
035600           END-IF                                                         
035700         END-IF                                                           
035800       END-IF                                                             
035900     ELSE                                                                 
036010       MOVE '401' TO MED-IDMFSFEL                                         
036100       MOVE NEJ TO NYCKLAR-SW                                             
036200     END-IF                                                               
036300                                                                          
036400     IF NYCKLAR-FEL                                                       
036500       CALL WMEDKONV USING MED-WMEDAREA                                   
036600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
036700       PERFORM MFS-RENSA-FAELT-IN                                         
036800       PERFORM MFS-RENSA-FAELT-UT                                         
036900     END-IF                                                               
037000     .                                                                    
037100     EJECT                                                                
037200 C-FOERSTA-SIDA SECTION.                                                  
037300                                                                          
037400     PERFORM MFS-RENSA-FAELT-UT                                           
037500     MOVE SPACE TO W-IDKVAOMR                                             
037600     MOVE ZERO TO W-IDKVAGRP                                              
037700                  W-DAREGDAT                                              
037800     MOVE JA TO ALLT-SW                                                   
037900     .                                                                    
038000     EJECT                                                                
038100 D-NAESTA-SIDA SECTION.                                                   
038200                                                                          
038300     INSPECT MID-IDARTNR-NX REPLACING LEADING SPACE BY ZERO               
038400     MOVE MID-IDARTNR-NX TO W-IDARTNR-11                                  
038410                                                                          
038500     INSPECT MID-IDKVAFEL-NX REPLACING LEADING SPACE BY ZERO              
038600     MOVE MID-IDKVAFEL-NX TO W-IDKVAFEL-11                                
038610                                                                          
038700     MOVE JA TO ALLT-SW                                                   
038800     .                                                                    
038900     EJECT                                                                
039000 E-SAMMA-SIDA SECTION.                                                    
039100                                                                          
039200     IF MID-INPUT = ALL '+'                                               
039300       MOVE NEJ TO VISA-SW                                                
039400       PERFORM MFS-RENSA-FAELT-IN                                         
039500       PERFORM S02-OEPPNA-FAELT-IN                                        
039600     ELSE                                                                 
039700       MOVE NEJ TO ALLT-SW                                                
039800       MOVE '003' TO MED-IDMFSFEL                                         
039900       CALL WMEDKONV USING MED-WMEDAREA                                   
040000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
040100       PERFORM MFS-ROR-EJ-FAELT-IN                                        
040200       PERFORM MFS-ROR-EJ-FAELT-UT                                        
040300       PERFORM MFS-LAS-IN-IGEN                                            
040400     END-IF                                                               
040500     .                                                                    
040600     EJECT                                                                
040700 G-KOLLA-INPUT SECTION.                                                   
040800                                                                          
040900     MOVE JA  TO INDATA-SW                                                
041000     IF MID-INPUT = ALL '+'                                               
041100       MOVE '011' TO MED-IDMFSFEL                                         
041200       MOVE NEJ TO INDATA-SW                                              
041300     ELSE                                                                 
041400       MOVE +1 TO IX-RAD                                                  
041500       PERFORM UNTIL IX-RAD > MAX-RADER                                   
041600         MOVE +1 TO IX-SPALT                                              
041700         PERFORM UNTIL IX-SPALT > MAX-SPALTER                             
041800           IF MID-KDCMD-IN (IX-RAD IX-SPALT) = ALL '+' OR SPACE           
041900             PERFORM GA-KOLLA-INPUT-UPPLAEGG                              
042000           ELSE                                                           
042100             PERFORM GB-KOLLA-INPUT-BORTTAG                               
042200           END-IF                                                         
042300           ADD +1 TO IX-SPALT                                             
042400         END-PERFORM                                                      
042500         ADD +1 TO IX-RAD                                                 
042600       END-PERFORM                                                        
042700     END-IF                                                               
042800     IF INDATA-FEL                                                        
042900       MOVE '001'      TO MED-IDMFSFEL                                    
043000       CALL WMEDKONV USING MED-WMEDAREA                                   
043100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
043200       PERFORM MFS-ROR-EJ-FAELT-IN                                        
043300       PERFORM MFS-ROR-EJ-FAELT-UT                                        
043400     END-IF                                                               
043500     .                                                                    
043600     EJECT                                                                
043700 GA-KOLLA-INPUT-UPPLAEGG SECTION.                                         
043800                                                                          
043900     MOVE MFS-ALFA-FAELT-RAETT TO                                         
044000             MOD-KDCMD-UT-ATTR (IX-RAD IX-SPALT)                          
044100     IF MID-IDARTNR-IN (IX-RAD IX-SPALT) = ALL '+'                        
044200       IF MID-IDKVAFEL-IN (IX-RAD IX-SPALT) = ALL '+'                     
044300         MOVE MFS-NUM-FAELT-RAETT TO                                      
044400             MOD-IDARTNR-UT-ATTR (IX-RAD IX-SPALT)                        
044500             MOD-IDKVAFEL-UT-ATTR (IX-RAD IX-SPALT)                       
044600       ELSE                                                               
044700         MOVE NEJ TO INDATA-SW                                            
044800         MOVE MFS-NUM-FAELT-FEL TO                                        
044900                 MOD-IDARTNR-UT-ATTR (IX-RAD IX-SPALT)                    
045000         MOVE MFS-NUM-FAELT-RAETT TO                                      
045100                 MOD-IDKVAFEL-UT-ATTR (IX-RAD IX-SPALT)                   
045200       END-IF                                                             
045300     ELSE                                                                 
045400       PERFORM GAB-KOLLA-ARTIKEL                                          
045500       IF ARTIKEL-OK                                                      
045600         MOVE MFS-NUM-FAELT-RAETT TO                                      
045700                MOD-IDARTNR-UT-ATTR (IX-RAD IX-SPALT)                     
045800                MOD-IDKVAFEL-UT-ATTR (IX-RAD IX-SPALT)                    
045900         IF MID-IDKVAFEL-IN (IX-RAD IX-SPALT) = ALL '+'                   
046000           MOVE NEJ TO INDATA-SW                                          
046100           MOVE MFS-NUM-FAELT-FEL TO                                      
046200                  MOD-IDKVAFEL-UT-ATTR (IX-RAD IX-SPALT)                  
046300         ELSE                                                             
046400           MOVE MID-IDKVAFEL-IN (IX-RAD IX-SPALT) TO                      
046500                      W-IDKVAFEL                                          
046600           PERFORM IMS-GU-WLXXJY11                                        
046700           IF SEGMENT-SAKNAS                                              
046800             MOVE NEJ TO INDATA-SW                                        
046900             MOVE MFS-NUM-FAELT-FEL TO                                    
047000                   MOD-IDKVAFEL-UT-ATTR (IX-RAD IX-SPALT)                 
047100           END-IF                                                         
047200         END-IF                                                           
047300       ELSE                                                               
047400         MOVE MFS-NUM-FAELT-FEL TO                                        
047500                 MOD-IDARTNR-UT-ATTR (IX-RAD IX-SPALT)                    
047600         MOVE MFS-NUM-FAELT-RAETT TO                                      
047700                 MOD-IDKVAFEL-UT-ATTR (IX-RAD IX-SPALT)                   
047800       END-IF                                                             
047900     END-IF                                                               
048000     .                                                                    
048100     EJECT                                                                
048200 GAB-KOLLA-ARTIKEL SECTION.                                               
048300                                                                          
048400     MOVE JA                    TO ARTIKEL-SW                             
048500     MOVE MID-IDARTNR-IN (IX-RAD IX-SPALT) TO W-IDARTNR                   
048600     PERFORM IMS-GU-WLARTC11                                              
048700     IF SEGMENT-FINNS                                                     
048800        PERFORM IMS-GU-W6KVAA01-MIN-MAX                                   
049000        MOVE NEJ                 TO LAGERINTERVALL-SW                     
049100        PERFORM UNTIL SEGMENT-SAKNAS                                      
049200           IF (ARTC-CLAG-ADLAGOMR < GRP-ADLAGOMR-FOM OR                   
049300                                  > GRP-ADLAGOMR-TOM)                     
049400           OR (ARTC-CLAG-ADGANG   < GRP-ADGANG-FOM   OR                   
049500                                  > GRP-ADGANG-TOM)                       
049600           OR (ARTC-CLAG-ADPLATS  < GRP-ADPLATS-FOM  OR                   
049700                                  > GRP-ADPLATS-TOM)                      
049800              CONTINUE                                                    
049900           ELSE                                                           
050000              MOVE JA            TO LAGERINTERVALL-SW                     
050100           END-IF                                                         
050200           PERFORM IMS-GN-W6KVAA01-MIN-MAX                                
050300        END-PERFORM                                                       
050400        IF LAGERINTERVALL-SW = NEJ                                        
050500           MOVE NEJ              TO ARTIKEL-SW                            
050600           MOVE NEJ              TO INDATA-SW                             
050700        END-IF                                                            
050800     ELSE                                                                 
050900        MOVE NEJ                 TO ARTIKEL-SW                            
051000        MOVE NEJ                 TO INDATA-SW                             
051100     END-IF                                                               
051200     .                                                                    
051300     EJECT                                                                
051400 GB-KOLLA-INPUT-BORTTAG SECTION.                                          
051500                                                                          
051600     MOVE MFS-NUM-FAELT-RAETT TO                                          
051700            MOD-IDARTNR-UT-ATTR (IX-RAD IX-SPALT)                         
051800            MOD-IDKVAFEL-UT-ATTR (IX-RAD IX-SPALT)                        
051900     MOVE MFS-ALFA-FAELT-RAETT TO                                         
052000            MOD-KDCMD-UT-ATTR (IX-RAD IX-SPALT)                           
052100     IF MID-KDCMD-IN (IX-RAD IX-SPALT) = 'B'                              
052200       IF MID-IDARTNR-IN (IX-RAD IX-SPALT) = ALL '+'                      
052300         MOVE NEJ TO INDATA-SW                                            
052400         MOVE MFS-NUM-FAELT-FEL TO                                        
052500                MOD-IDARTNR-UT-ATTR (IX-RAD IX-SPALT)                     
052600       ELSE                                                               
052700         IF MID-IDKVAFEL-IN (IX-RAD IX-SPALT) = ALL '+'                   
052800           MOVE NEJ TO INDATA-SW                                          
052900           MOVE MFS-NUM-FAELT-FEL TO                                      
053000                   MOD-IDKVAFEL-UT-ATTR (IX-RAD IX-SPALT)                 
053100         ELSE                                                             
053200           MOVE MID-IDARTNR-IN (IX-RAD IX-SPALT) TO                       
053300                     W-IDARTNR-11                                         
053400           MOVE MID-IDKVAFEL-IN (IX-RAD IX-SPALT) TO                      
053500                     W-IDKVAFEL-11                                        
053600           PERFORM IMS-GU-W6KVAB11                                        
053700           IF SEGMENT-SAKNAS                                              
053800             MOVE NEJ TO INDATA-SW                                        
053900             MOVE MFS-NUM-FAELT-FEL TO                                    
054000                      MOD-IDARTNR-UT-ATTR (IX-RAD IX-SPALT)               
054100           END-IF                                                         
054200         END-IF                                                           
054300       END-IF                                                             
054400     ELSE                                                                 
054500       MOVE NEJ TO INDATA-SW                                              
054600       MOVE MFS-ALFA-FAELT-FEL TO                                         
054700              MOD-KDCMD-UT-ATTR (IX-RAD IX-SPALT)                         
054800     END-IF                                                               
054900     .                                                                    
055000     EJECT                                                                
055100 H-UPPDATERA SECTION.                                                     
055200                                                                          
055300     MOVE +1 TO IX-RAD                                                    
055400     PERFORM UNTIL IX-RAD > MAX-RADER                                     
055500       MOVE +1 TO IX-SPALT                                                
055600       PERFORM UNTIL IX-SPALT > MAX-SPALTER                               
055700         IF MID-IDARTNR-IN (IX-RAD IX-SPALT) NOT = ALL '+'                
055800           MOVE MID-IDARTNR-IN (IX-RAD IX-SPALT) TO W-IDARTNR-11          
055900           MOVE MID-IDKVAFEL-IN (IX-RAD IX-SPALT) TO                      
056000                                W-IDKVAFEL-11                             
056100           PERFORM IMS-GHU-W6KVAB11                                       
056200           IF SEGMENT-FINNS                                               
056300             IF MID-KDCMD-IN (IX-RAD IX-SPALT) NOT = ALL '+'              
056400                                                  AND SPACE               
056500               PERFORM IMS-DLET-KVAB                                      
056600             ELSE                                                         
056700               MOVE MID-IDKVAFEL-IN (IX-RAD IX-SPALT) TO                  
056800                                KVAB-ART-IDKVAFEL                         
056900               PERFORM IMS-REPL-KVAB                                      
057000             END-IF                                                       
057100           ELSE                                                           
057200             IF SEGMENT-SAKNAS                                            
057300               MOVE MID-IDARTNR-IN (IX-RAD IX-SPALT) TO                   
057400                                KVAB-ART-IDARTNR                          
057500               MOVE MID-IDKVAFEL-IN (IX-RAD IX-SPALT) TO                  
057600                                KVAB-ART-IDKVAFEL                         
057700               PERFORM IMS-ISRT-W6KVAB11                                  
057800             END-IF                                                       
057900           END-IF                                                         
058000         END-IF                                                           
058100         ADD +1 TO IX-SPALT                                               
058200       END-PERFORM                                                        
058300       ADD +1 TO IX-RAD                                                   
058400     END-PERFORM                                                          
058500                                                                          
058600     MOVE '101' TO MED-IDMFSINF                                           
058700     CALL WMEDKONV USING MED-WMEDAREA                                     
058800     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
058900     PERFORM MFS-FORM-ATTR                                                
059000     PERFORM MFS-RENSA-FAELT-IN                                           
059100     .                                                                    
059200     EJECT                                                                
059300 S01-LAES-VISA-INFO SECTION.                                              
059400                                                                          
059500     MOVE WS-IDDC     TO W-IDDC                                           
059600     MOVE WS-IDKVAOMR TO W-IDKVAOMR                                       
059700     MOVE WS-IDKVAGRP TO W-IDKVAGRP                                       
059800     MOVE WS-DAREGDAT TO W-DAREGDAT                                       
059810                                                                          
059900     PERFORM IMS-GU-W6KVAB01                                              
060000     PERFORM IMS-GNP-W6KVAB11                                             
060100     IF SEGMENT-SAKNAS                                                    
060200       MOVE '017' TO MED-IDMFSFEL                                         
060300       CALL WMEDKONV USING MED-WMEDAREA                                   
060400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
060500       PERFORM MFS-RENSA-FAELT-UT                                         
060600     ELSE                                                                 
060700       PERFORM S01A-LAES-VISA-ART                                         
060800     END-IF                                                               
060900     .                                                                    
061000     EJECT                                                                
061100 S01A-LAES-VISA-ART SECTION.                                              
061200                                                                          
061300     MOVE +1 TO IX-RAD                                                    
061400     IF SEGMENT-FINNS                                                     
061500       MOVE KVAB-ART-IDARTNR TO MOD-IDARTNR-EN                            
061600       MOVE KVAB-ART-IDKVAFEL TO MOD-IDKVAFEL-EN                          
061700     ELSE                                                                 
061800       MOVE ZERO TO MOD-IDARTNR-EN                                        
061900       MOVE ZERO TO MOD-IDKVAFEL-EN                                       
062000     END-IF                                                               
062100                                                                          
062200     PERFORM UNTIL IX-RAD > MAX-RADER                                     
062300       MOVE +1 TO IX-SPALT                                                
062400       PERFORM UNTIL IX-SPALT > MAX-SPALTER                               
062500         IF SEGMENT-FINNS                                                 
062600           MOVE KVAB-ART-IDARTNR TO                                       
062700                     MOD-IDARTNR-UT (IX-RAD IX-SPALT)                     
062800           MOVE KVAB-ART-IDKVAFEL TO                                      
062900                     MOD-IDKVAFEL-UT (IX-RAD IX-SPALT)                    
063000           PERFORM IMS-GNP-W6KVAB11                                       
063100         ELSE                                                             
063200           MOVE MFS-RENSA-FAELT TO                                        
063300                          MOD-IDARTNR-UT (IX-RAD IX-SPALT)                
063400                          MOD-IDKVAFEL-UT (IX-RAD IX-SPALT)               
063500         END-IF                                                           
063600         MOVE MFS-RENSA-FAELT TO MOD-KDCMD-UT (IX-RAD IX-SPALT)           
063700         ADD +1 TO IX-SPALT                                               
063800       END-PERFORM                                                        
063900       ADD +1 TO IX-RAD                                                   
064000     END-PERFORM                                                          
064100                                                                          
064200     IF SEGMENT-FINNS                                                     
064300       MOVE KVAB-ART-IDARTNR TO MOD-IDARTNR-NX                            
064400       MOVE KVAB-ART-IDKVAFEL TO MOD-IDKVAFEL-NX                          
064500       MOVE '105' TO MED-IDMFSINF                                         
064600       CALL WMEDKONV USING MED-WMEDAREA                                   
064700       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
064800     ELSE                                                                 
064900       MOVE ZERO TO MOD-IDARTNR-NX                                        
065000                    MOD-IDKVAFEL-NX                                       
065100     END-IF                                                               
065200     .                                                                    
065300     EJECT                                                                
065400 S02-OEPPNA-FAELT-IN SECTION.                                             
065500                                                                          
065600     MOVE +1 TO IX-RAD                                                    
065700     PERFORM UNTIL IX-RAD > MAX-RADER                                     
065800       MOVE +1 TO IX-SPALT                                                
065900       PERFORM UNTIL IX-SPALT > MAX-SPALTER                               
066000         MOVE MFS-OEPPNA-NUM-FAELT TO                                     
066100                    MOD-IDARTNR-UT-ATTR (IX-RAD IX-SPALT)                 
066200                    MOD-IDKVAFEL-UT-ATTR (IX-RAD IX-SPALT)                
066300         MOVE MFS-OEPPNA-ALFA-FAELT TO                                    
066400                    MOD-KDCMD-UT-ATTR (IX-RAD IX-SPALT)                   
066500         ADD +1 TO IX-SPALT                                               
066600       END-PERFORM                                                        
066700       ADD +1 TO IX-RAD                                                   
066800     END-PERFORM                                                          
066900     .                                                                    
067000     EJECT                                                                
067100 MFS-RENSA-FAELT-UT SECTION.                                              
067200                                                                          
067300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-NX                               
067400                             MOD-IDKVAFEL-NX                              
067500                             MOD-IDARTNR-EN                               
067600                             MOD-IDKVAFEL-EN                              
067700     .                                                                    
067800     SKIP2                                                                
067900 MFS-RENSA-FAELT-IN SECTION.                                              
068000                                                                          
068100     MOVE +1 TO IX-RAD                                                    
068200     PERFORM UNTIL IX-RAD > MAX-RADER                                     
068300       MOVE +1 TO IX-SPALT                                                
068400       PERFORM UNTIL IX-SPALT > MAX-SPALTER                               
068500         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT (IX-RAD IX-SPALT)         
068600                                 MOD-IDKVAFEL-UT (IX-RAD IX-SPALT)        
068700                                 MOD-KDCMD-UT (IX-RAD IX-SPALT)           
068800         ADD +1 TO IX-SPALT                                               
068900       END-PERFORM                                                        
069000       ADD +1 TO IX-RAD                                                   
069100     END-PERFORM                                                          
069200     .                                                                    
069300     EJECT                                                                
069400 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
069500                                                                          
069600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-NX                             
069700                               MOD-IDKVAFEL-NX                            
069800                               MOD-IDARTNR-EN                             
069900                               MOD-IDKVAFEL-EN                            
070000     .                                                                    
070100     EJECT                                                                
070200 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
070300                                                                          
070400     MOVE +1 TO IX-RAD                                                    
070500     PERFORM UNTIL IX-RAD > MAX-RADER                                     
070600       MOVE +1 TO IX-SPALT                                                
070700       PERFORM UNTIL IX-SPALT > MAX-SPALTER                               
070800         MOVE MFS-ROER-EJ-FAELT TO                                        
070900                    MOD-IDARTNR-UT (IX-RAD IX-SPALT)                      
071000                    MOD-IDKVAFEL-UT (IX-RAD IX-SPALT)                     
071100                    MOD-KDCMD-UT (IX-RAD IX-SPALT)                        
071200         ADD +1 TO IX-SPALT                                               
071300       END-PERFORM                                                        
071400       ADD +1 TO IX-RAD                                                   
071500     END-PERFORM                                                          
071600     .                                                                    
071700     EJECT                                                                
071800 MFS-FORM-ATTR SECTION.                                                   
071900                                                                          
072000     MOVE +1 TO IX-RAD                                                    
072100     PERFORM UNTIL IX-RAD > MAX-RADER                                     
072200       MOVE +1 TO IX-SPALT                                                
072300       PERFORM UNTIL IX-SPALT > MAX-SPALTER                               
072400          MOVE MFS-FORMATETS-ATTR TO                                      
072500                    MOD-IDARTNR-UT-ATTR (IX-RAD IX-SPALT)                 
072600                    MOD-IDKVAFEL-UT-ATTR (IX-RAD IX-SPALT)                
072700                    MOD-KDCMD-UT-ATTR (IX-RAD IX-SPALT)                   
072800          ADD +1 TO IX-SPALT                                              
072900       END-PERFORM                                                        
073000       ADD +1 TO IX-RAD                                                   
073100     END-PERFORM                                                          
073200     .                                                                    
073300     SKIP2                                                                
073400 MFS-LAS-IN-IGEN SECTION.                                                 
073500                                                                          
073600     MOVE +1 TO IX-RAD                                                    
073700     PERFORM UNTIL IX-RAD > MAX-RADER                                     
073800       MOVE +1 TO IX-SPALT                                                
073900       PERFORM UNTIL IX-SPALT > MAX-SPALTER                               
074000         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
074100                  MOD-IDARTNR-UT-ATTR (IX-RAD IX-SPALT)                   
074200                  MOD-IDKVAFEL-UT-ATTR (IX-RAD IX-SPALT)                  
074300                  MOD-KDCMD-UT-ATTR (IX-RAD IX-SPALT)                     
074400         ADD +1 TO IX-SPALT                                               
074500       END-PERFORM                                                        
074600       ADD +1 TO IX-RAD                                                   
074700     END-PERFORM                                                          
074800     .                                                                    
074900     EJECT                                                                
075000* IMS SEKTIONER                                                           
075100     SKIP3                                                                
075200 IMS-GET-MSG SECTION.                                                     
075300                                                                          
075400     MOVE '  QC' TO GODK-STATUSKODER                                      
075500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
075600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075700     PERFORM IMS-STATUSKONTROLL                                           
075800     .                                                                    
075900     SKIP3                                                                
076000 IMS-INSERT-MSG SECTION.                                                  
076100                                                                          
076200     IF NOT ENGLISH-TEXT                                                  
076300       MOVE '0' TO MFS-KDHUVOMR                                           
076400     END-IF                                                               
076500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
076600     MOVE SPACE TO GODK-STATUSKODER                                       
076700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
076800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076900     PERFORM IMS-STATUSKONTROLL                                           
077000     .                                                                    
077100     EJECT                                                                
077200 IMS-GU-W6KVAB01 SECTION.                                                 
077300                                                                          
077400     STRING 'W6KVAB01(W6H601KY =' W-KVAB01-KEY-X ')'                      
077500          DELIMITED BY SIZE INTO SSA1                                     
077600     MOVE '  GE' TO GODK-STATUSKODER                                      
077700     CALL CBLTDLI USING GU KVAB-PCB DLI-IO-AREA SSA1                      
077800     MOVE KVAB-STATUS-CODE TO STATUS-WS                                   
077900     PERFORM IMS-STATUSKONTROLL                                           
078000     .                                                                    
078100 IMS-GU-W6KVAB11 SECTION.                                                 
078200                                                                          
078300     STRING 'W6KVAB01(W6H601KY =' W-KVAB01-KEY-X ')'                      
078400          DELIMITED BY SIZE INTO SSA1                                     
078500     STRING 'W6KVAB11(W6H611KY =' W-KVAB11-KEY-X ')'                      
078600          DELIMITED BY SIZE INTO SSA2                                     
078700     MOVE '  GE' TO GODK-STATUSKODER                                      
078800     CALL CBLTDLI USING GU KVAB-PCB DLI-IO-AREA SSA1 SSA2                 
078900     MOVE KVAB-STATUS-CODE TO STATUS-WS                                   
079000     PERFORM IMS-STATUSKONTROLL                                           
079100     .                                                                    
079200     EJECT                                                                
079300 IMS-GU-WLXXJY11 SECTION.                                                 
079400                                                                          
079500     STRING 'WLXXJY01(WDGXKEY  =' W-WDGX-4821-KEY-X ')'                   
079600          DELIMITED BY SIZE INTO SSA1                                     
079700     STRING 'WLXXJY11(WDGXKEY  =' W-WDGX-4822-KEY-X ')'                   
079800          DELIMITED BY SIZE INTO SSA2                                     
079900     MOVE '  GE' TO GODK-STATUSKODER                                      
080000     CALL CBLTDLI USING GU XXJY-PCB DLI-IO-AREA SSA1 SSA2                 
080100     MOVE XXJY-STATUS-CODE TO STATUS-WS                                   
080200     PERFORM IMS-STATUSKONTROLL                                           
080300     .                                                                    
080400                                                                          
080500 IMS-GHU-W6KVAB11 SECTION.                                                
080600                                                                          
080700     STRING 'W6KVAB01(W6H601KY =' W-KVAB01-KEY-X ')'                      
080800          DELIMITED BY SIZE INTO SSA1                                     
080900     STRING 'W6KVAB11(W6H611KY =' W-KVAB11-KEY-X ')'                      
081000          DELIMITED BY SIZE INTO SSA2                                     
081100     MOVE '  GE' TO GODK-STATUSKODER                                      
081200     CALL CBLTDLI USING GHU KVAB-PCB DLI-IO-AREA SSA1 SSA2                
081300     MOVE KVAB-STATUS-CODE TO STATUS-WS                                   
081400     PERFORM IMS-STATUSKONTROLL                                           
081500     .                                                                    
081600                                                                          
081700 IMS-GNP-W6KVAB11 SECTION.                                                
081800                                                                          
081900     STRING 'W6KVAB11(W6H611KY=>' W-KVAB11-KEY-X ')'                      
082000          DELIMITED BY SIZE INTO SSA1                                     
082100     MOVE '  GE' TO GODK-STATUSKODER                                      
082200     CALL CBLTDLI USING GNP KVAB-PCB DLI-IO-AREA SSA1                     
082300     MOVE KVAB-STATUS-CODE TO STATUS-WS                                   
082400     PERFORM IMS-STATUSKONTROLL                                           
082500     .                                                                    
082600     EJECT                                                                
082631 IMS-GU-W6KVAA01-MIN-MAX SECTION.                                         
082632                                                                          
082633     STRING 'W6KVAA01(W6H501KY=>' W-W6H501KY-MIN-X                        
082634                    '&W6H501KY=<' W-W6H501KY-MAX-X ')'                    
082635          DELIMITED BY SIZE INTO SSA1                                     
082636     MOVE '  GE'   TO GODK-STATUSKODER                                    
082637     CALL CBLTDLI USING GU KVAA-PCB DLI-IO-AREA2 SSA1                     
082638     MOVE KVAA-STATUS-CODE TO STATUS-WS                                   
082639     PERFORM IMS-STATUSKONTROLL                                           
082640     .                                                                    
082641     SKIP2                                                                
082642 IMS-GN-W6KVAA01-MIN-MAX SECTION.                                         
082643                                                                          
082644     STRING 'W6KVAA01(W6H501KY=>' W-W6H501KY-MIN-X                        
082645                    '&W6H501KY=<' W-W6H501KY-MAX-X ')'                    
082646          DELIMITED BY SIZE INTO SSA1                                     
082647     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
082648     CALL CBLTDLI USING GN KVAA-PCB DLI-IO-AREA2 SSA1                     
082649     MOVE KVAA-STATUS-CODE TO STATUS-WS                                   
082650     PERFORM IMS-STATUSKONTROLL                                           
082651     .                                                                    
084500     EJECT                                                                
084600 IMS-GU-WLARTC11 SECTION.                                                 
084700                                                                          
084800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
084900                     DELIMITED BY SIZE INTO SSA1                          
085100     MOVE   'WLARTC11(KDSEGKEY =1)'      TO SSA2                          
085300     MOVE '  GE'                TO GODK-STATUSKODER                       
085400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA3 SSA1 SSA2                
085500     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
085600     PERFORM IMS-STATUSKONTROLL                                           
085700     .                                                                    
085800     EJECT                                                                
085900 IMS-ISRT-W6KVAB11 SECTION.                                               
086000                                                                          
086100     STRING 'W6KVAB01(W6H601KY =' W-KVAB01-KEY-X ')'                      
086200          DELIMITED BY SIZE INTO SSA1                                     
086300     MOVE 'W6KVAB11 ' TO SSA2                                             
086400     MOVE '  II' TO GODK-STATUSKODER                                      
086500     CALL CBLTDLI USING ISRT KVAB-PCB DLI-IO-AREA SSA1 SSA2               
086600     MOVE KVAB-STATUS-CODE TO STATUS-WS                                   
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     .                                                                    
086900                                                                          
087000 IMS-REPL-KVAB SECTION.                                                   
087100                                                                          
087200     MOVE '  ' TO GODK-STATUSKODER                                        
087300     CALL CBLTDLI USING REPL KVAB-PCB DLI-IO-AREA                         
087400     MOVE KVAB-STATUS-CODE TO STATUS-WS                                   
087500     PERFORM IMS-STATUSKONTROLL                                           
087600     .                                                                    
087700                                                                          
087800 IMS-DLET-KVAB SECTION.                                                   
087900                                                                          
088000     MOVE '  ' TO GODK-STATUSKODER                                        
088100     CALL CBLTDLI USING DLET KVAB-PCB DLI-IO-AREA                         
088200     MOVE KVAB-STATUS-CODE TO STATUS-WS                                   
088300     PERFORM IMS-STATUSKONTROLL                                           
088400     .                                                                    
088510     SKIP3                                                                
088600 IMS-STATUSKONTROLL SECTION.                                              
088700                                                                          
088800     SET STATUS-IX TO 1                                                   
088900     SEARCH GODK-STATUS                                                   
089000       AT END CALL FELLOG                                                 
089100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
089200     END-SEARCH                                                           
089300     .                                                                    
