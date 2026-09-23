000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4012100.                                                
000300 AUTHOR.         INGER NILSSON.                                           
000400 DATE-WRITTEN.   MARS  90.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        UPPDATERING,NYUPPLÄGGNING,BORTTAG OCH FRÅGEPROGRAM               
000900*                                                                         
001000*    INDATA.                                                              
001100*        TRANSAKTION: W4T121                                              
001200*        MID:         W4I12101                                            
001300*                                                                         
001400*    UTDATA.                                                              
001500*        MOD:         W4O12101                                            
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900 DATA DIVISION.                                                           
002000     EJECT                                                                
002100 WORKING-STORAGE SECTION.                                                 
002200                                                                          
002300*    -- CHECKED BY WY2000                                                 
002400 77  IDPGM                   PIC X(8)    VALUE 'W4012100'.                
002500 77  JA                      PIC X       VALUE 'J'.                       
002600 77  NEJ                     PIC X       VALUE 'N'.                       
002700 77  SPRAK-IX                PIC S9(9)   VALUE +0   COMP SYNC.            
002800 77  DC-IX                   PIC S9(9)   VALUE +0   COMP SYNC.            
002900 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
003000 77  MAX-RADER               PIC S9(9)   VALUE +10  COMP SYNC.            
003100 77  IDKVAOMR-WS             PIC X(1)    VALUE SPACE.                     
003200 77  IDKVATRG-WS             PIC X(2)    VALUE SPACE.                     
003300 77  IDKVAGRP-WS             PIC X(3)    VALUE SPACE.                     
003400 77  W-GRP-IDKVAOMR          PIC X(1)    VALUE SPACE.                     
003500 77  W-GRP-IDKVATRG          PIC 9(2)    VALUE ZERO.                      
003600 77  W-GRP-IDKVAGRP          PIC 9(3)    VALUE ZERO.                      
003700 77  W-LAST-W6H501           PIC X(1)    VALUE 'N'.                       
003800                                                                          
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
005100   88  EGEN-MID                          VALUE '4121'.                    
005200   88  GODK-MID                          VALUE '4122' '4123'.             
005300     EJECT                                                                
005400*   -COPY WWKVAOMR                                                        
005500     EJECT                                                                
005600*   --- VALID IDDC CODES                                                  
005700*                                                                         
005800*01 -COPY WWDC99                                                          
005900     EJECT                                                                
006000 01  GENERELLA-SUBPROGRAM.                                                
006100   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
006200   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
006300   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
006400   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
006500     EJECT                                                                
006600*   -COPY WMEDAREA                                                        
006700     EJECT                                                                
006800******************************************************************        
006900*                                                                         
007000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
007100*                                                                         
007200 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
007300     SKIP3                                                                
007400*01  MID -COPY W4I12101                                                   
007500     EJECT                                                                
007600*01  -COPY WMSGAREA                                                       
007700     EJECT                                                                
007800*  03  MOD -COPY W4O12101 -RED MSG-AREA.                                  
007900     EJECT                                                                
008000*01  -COPY WMFSAREA                                                       
008100     EJECT                                                                
008200******************************************************************        
008300*                                                                         
008400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008500*                                                                         
008600 01  IMS-WS.                                                              
008700   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
008800     SKIP3                                                                
008900 01  NYCKLAR-TILL-DLI.                                                    
009000   03  W-W6H501KY-X.                                                      
009100      05  W-IDDC             PIC  X(02)  VALUE SPACE.                     
009200      05  W-IDKVAOMR         PIC  X(01)  VALUE SPACE.                     
009300      05  W-IDKVATRG         PIC  9(02)  VALUE ZERO.                      
009400      05  W-IDKVAGRP         PIC  9(03)  VALUE ZERO.                      
009500      05  W-ADLAGOMR-FOM     PIC S9(03)  VALUE +0    COMP-3.              
009600      05  W-ADGANG-FOM       PIC S9(03)  VALUE +0    COMP-3.              
009700      05  W-ADPLATS-FOM      PIC S9(05)  VALUE +0    COMP-3.              
009800     SKIP3                                                                
009900   03  W-W6H501KY-MIN-X.                                                  
010000      05  W-IDDC-MIN         PIC  X(02)  VALUE LOW-VALUE.                 
010100      05  W-IDKVAOMR-MIN     PIC  X(01)  VALUE LOW-VALUE.                 
010200      05  W-IDKVATRG-MIN     PIC  9(02)  VALUE ZERO.                      
010300      05  W-IDKVAGRP-MIN     PIC  9(03)  VALUE ZERO.                      
010400      05  W-ADLAGOMR-FOM-MIN PIC S9(03)  VALUE +0    COMP-3.              
010500      05  W-ADGANG-FOM-MIN   PIC S9(03)  VALUE +0    COMP-3.              
010600      05  W-ADPLATS-FOM-MIN  PIC S9(05)  VALUE +0    COMP-3.              
010700     SKIP3                                                                
010800   03  W-W6H501KY-MAX-X.                                                  
010900      05  W-IDDC-MAX         PIC  X(02)  VALUE HIGH-VALUE.                
011000      05  W-IDKVAOMR-MAX     PIC  X(01)  VALUE HIGH-VALUE.                
011100      05  W-IDKVATRG-MAX     PIC  9(02)  VALUE 99.                        
011200      05  W-IDKVAGRP-MAX     PIC  9(03)  VALUE 999.                       
011300      05  W-ADLAGOMR-FOM-MAX PIC S9(03)  VALUE +999  COMP-3.              
011400      05  W-ADGANG-FOM-MAX   PIC S9(03)  VALUE +999  COMP-3.              
011500      05  W-ADPLATS-FOM-MAX  PIC S9(05)  VALUE +99999 COMP-3.             
011600     SKIP3                                                                
011700*                        **** STATUS-KOD FRÅN IMS                         
011800   03  STATUS-WS             PIC XX.                                      
011900     88  SEGMENT-FINNS                   VALUE '  '.                      
012000     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
012100     88  SEGMENT-SLUT                    VALUE 'GB'.                      
012200     SKIP3                                                                
012300   03  GODK-STATUSKODER.                                                  
012400     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012500     SKIP3                                                                
012600 01    SSA1                  PIC X(64).                                   
012700 01    SSA2                  PIC X(64).                                   
012800     EJECT                                                                
012900*                            IMS FUNKTIONSKODER                           
013000*01    -COPY W0003                                                        
013100     EJECT                                                                
013200*                            DLI INPUT-OUTPUT AREA                        
013300 01  DLI-IO-AREA.                                                         
013400   03  IO-AREA               PIC X(900) VALUE SPACE.                      
013500*  03  W6KVAA01  -COPY W6H501    -RED IO-AREA.                            
013600     EJECT                                                                
013700 LINKAGE SECTION.                                                         
013800*01  -COPY W0009     -PRE MSG-                                            
013900     EJECT                                                                
014000*01  -COPY W0008     -PRE W6H5-                                           
014100     05  FILLER             PIC X.                                        
014200     EJECT                                                                
014300 PROCEDURE DIVISION USING  MSG-PCB W6H5-PCB.                              
014400 MAIN SECTION.                                                            
014500     ENTRY 'DLITCBL' USING MSG-PCB W6H5-PCB.                              
014600                                                                          
014700     PERFORM IMS-GET-MSG                                                  
014800     IF SEGMENT-FINNS                                                     
014900       PERFORM A-INIT                                                     
015000       PERFORM B-KOLLA-NYCKLAR                                            
015100       IF NYCKLAR-OK                                                      
015200         IF MFS-UPDATE                                                    
015300           PERFORM G-KOLLA-INPUT                                          
015400           IF INDATA-OK                                                   
015500             PERFORM H-UPPDATERA                                          
015600           END-IF                                                         
015700         ELSE                                                             
015800           IF MFS-FIRST                                                   
015900             PERFORM C-FOERSTA-SIDA                                       
016000           ELSE                                                           
016100             IF MFS-NEXT                                                  
016200               PERFORM D-NAESTA-SIDA                                      
016300             ELSE                                                         
016400               PERFORM E-SAMMA-SIDA                                       
016500             END-IF                                                       
016600           END-IF                                                         
016700         END-IF                                                           
016800         IF ALLT-OK                                                       
016900            PERFORM F-LAES-VISA-INFO                                      
017000         END-IF                                                           
017100       END-IF                                                             
017200       MOVE LENGTH OF MOD-W4O12101 TO MSG-KVLL                            
017300       ADD                +4       TO MSG-KVLL                            
017400       PERFORM IMS-INSERT-MSG                                             
017500     END-IF                                                               
017600                                                                          
017700     MOVE ZERO TO RETURN-CODE                                             
017800     GOBACK                                                               
017900     .                                                                    
018000     EJECT                                                                
018100 A-INIT SECTION.                                                          
018200                                                                          
018300     IF MSG-DUBBLA-TRANSKODER                                             
018400        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I12101                
018500        MOVE MSG-IDTRANS-2     TO MFS-IDTRANS                             
018600        MOVE MSG-KDMFSFOR-2    TO MFS-KDMFSFOR                            
018700     ELSE                                                                 
018800        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I12101                 
018900        MOVE MSG-IDTRANS-1     TO MFS-IDTRANS                             
019000        MOVE MSG-KDMFSFOR-1    TO MFS-KDMFSFOR                            
019100     END-IF                                                               
019200                                                                          
019300     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
019400     MOVE MSG-IDPFK            TO MFS-IDPFK                               
019500     MOVE MFS-IDTRANS          TO W-IDTRANS                               
019600                                                                          
019700     MOVE LOW-VALUE            TO MSG-AREA                                
019800     MOVE 'W4O121N1'           TO MFS-IDMOD                               
019900     MOVE '4121'               TO MOD-IDTRANS                             
020000     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
020100                                                                          
020200     IF ENGLISH-TEXT                                                      
020300       MOVE 'GB ' TO MED-IDSKYLT                                          
020400     ELSE                                                                 
020500       MOVE 'S  ' TO MED-IDSKYLT                                          
020600     END-IF                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 B-KOLLA-NYCKLAR SECTION.                                                 
021000                                                                          
021100     MOVE JA TO NYCKLAR-SW                                                
021200                                                                          
021300     IF NOT EGEN-MID                                                      
021400        MOVE SPACE           TO MID-IDKVAOMR-IN                           
021500                                MID-IDKVATRG-IN                           
021600                                MID-IDKVAGRP-IN                           
021700     END-IF                                                               
021800                                                                          
021900     MOVE MFS-RENSA-FAELT    TO MOD-IDKVAOMR-IN                           
022000                                MOD-IDKVATRG-IN                           
022100                                MOD-IDKVAGRP-IN                           
022200     MOVE MID-IDDC-IN TO WS-IDDC                                          
022300*                                                                         
022400     IF NOT CDC-SE AND NOT SDC-NL AND NOT SDC-GB AND                      
022500        NOT SDC-ES AND NOT SDC-IT AND                                     
022600        NOT SDC-AT                                                        
022700       MOVE MID-IDDC-UT    TO WS-IDDC                                     
022800     ELSE                                                                 
022900       MOVE MID-IDDC-IN      TO WS-IDDC                                   
023000       MOVE '7'              TO MFS-IDPFK                                 
023100       MOVE SPACE            TO MFS-KDTRTYP                               
023200     END-IF                                                               
023300                                                                          
023400     IF NOT CDC-SE AND NOT SDC-NL AND NOT SDC-GB AND                      
023500        NOT SDC-ES AND NOT SDC-IT AND                                     
023600        NOT SDC-AT                                                        
023700       MOVE NEJ TO NYCKLAR-SW                                             
023800     ELSE                                                                 
023900       EVALUATE TRUE                                                      
024000          WHEN CDC-SE    MOVE  +1 TO  DC-IX                               
024100          WHEN SDC-NL    MOVE  +2 TO  DC-IX                               
024200          WHEN SDC-GB    MOVE  +4 TO  DC-IX                               
024300          WHEN SDC-ES    MOVE  +5 TO  DC-IX                               
024400          WHEN SDC-IT    MOVE  +6 TO  DC-IX                               
024500          WHEN SDC-AT    MOVE  +7 TO  DC-IX                               
024600          WHEN OTHER  MOVE NEJ TO NYCKLAR-SW                              
024700       END-EVALUATE                                                       
024800     END-IF                                                               
024900                                                                          
025000     IF MID-IDKVAOMR-IN = ALL '+'                                         
025100       MOVE MID-IDKVAOMR-UT  TO IDKVAOMR-WS                               
025200     ELSE                                                                 
025300       MOVE MID-IDKVAOMR-IN  TO IDKVAOMR-WS                               
025400       MOVE '7'              TO MFS-IDPFK                                 
025500       MOVE SPACE            TO MFS-KDTRTYP                               
025600     END-IF                                                               
025700                                                                          
025800     IF NYCKLAR-FEL                                                       
025810       CONTINUE                                                           
025820     ELSE                                                                 
025900       IF (IDKVAOMR-WS = KVA-IDKVAOMR (DC-IX 1)                           
026000                     OR  KVA-IDKVAOMR (DC-IX 2)                           
026100                     OR  KVA-IDKVAOMR (DC-IX 3)                           
026200                     OR  KVA-IDKVAOMR (DC-IX 4)                           
026300                     OR  KVA-IDKVAOMR (DC-IX 5)                           
026400                     OR  KVA-IDKVAOMR (DC-IX 6))                          
026500       OR MFS-UPDATE                                                      
026600           CONTINUE                                                       
026700       ELSE                                                               
026800           MOVE NEJ            TO NYCKLAR-SW                              
026900       END-IF                                                             
026910     END-IF                                                               
027000                                                                          
027100     IF MID-IDKVATRG-IN = ALL '+'                                         
027200       MOVE MID-IDKVATRG-UT  TO IDKVATRG-WS                               
027300     ELSE                                                                 
027400       MOVE MID-IDKVATRG-IN  TO IDKVATRG-WS                               
027500       MOVE '7'              TO MFS-IDPFK                                 
027600       MOVE SPACE            TO MFS-KDTRTYP                               
027700     END-IF                                                               
027800                                                                          
027900     IF MID-IDKVAGRP-IN = ALL '+'                                         
028000       MOVE MID-IDKVAGRP-UT  TO IDKVAGRP-WS                               
028100       INSPECT IDKVAGRP-WS REPLACING LEADING SPACE BY ZERO                
028200     ELSE                                                                 
028300       MOVE MID-IDKVAGRP-IN  TO IDKVAGRP-WS                               
028400       MOVE '7'              TO MFS-IDPFK                                 
028500       MOVE SPACE            TO MFS-KDTRTYP                               
028600     END-IF                                                               
028700                                                                          
028800                                                                          
028900     IF GODK-MID OR NYCKLAR-OK                                            
029000        MOVE IDKVAOMR-WS     TO W-IDKVAOMR                                
029100                                W-IDKVAOMR-MIN                            
029200                                W-IDKVAOMR-MAX                            
029300        MOVE IDKVATRG-WS     TO W-IDKVATRG                                
029400                                W-IDKVATRG-MIN                            
029500                                W-IDKVATRG-MAX                            
029600        MOVE IDKVAGRP-WS     TO W-IDKVAGRP                                
029700                                W-IDKVAGRP-MIN                            
029800                                W-IDKVAGRP-MAX                            
029900        MOVE WS-IDDC         TO W-IDDC                                    
030000                                W-IDDC-MIN                                
030100                                W-IDDC-MAX                                
030200        MOVE W-IDKVAOMR      TO MOD-IDKVAOMR-UT                           
030300        MOVE W-IDKVATRG      TO MOD-IDKVATRG-UT                           
030400        MOVE W-IDKVAGRP      TO MOD-IDKVAGRP-UT                           
030500        MOVE W-IDDC          TO MOD-IDDC-UT                               
030600     ELSE                                                                 
030700        MOVE MFS-RENSA-FAELT TO MOD-IDKVAOMR-UT                           
030800                                MOD-IDKVATRG-UT                           
030900                                MOD-IDKVAGRP-UT                           
031000                                MOD-IDDC-UT                               
031100     END-IF                                                               
031200                                                                          
031300     IF NYCKLAR-FEL                                                       
031400        MOVE '401'           TO MED-IDMFSFEL                              
031500        CALL WMEDKONV USING MED-WMEDAREA                                  
031600        MOVE MED-MFSFEL      TO MOD-TEMFSFEL                              
031700        PERFORM MFS-RENSA-FAELT-IN                                        
031800        PERFORM MFS-RENSA-FAELT-UT                                        
031900     END-IF                                                               
032000     .                                                                    
032100     EJECT                                                                
032200 C-FOERSTA-SIDA SECTION.                                                  
032300                                                                          
032400     PERFORM MFS-RENSA-FAELT-IN                                           
032500     PERFORM MFS-RENSA-FAELT-UT                                           
032600     MOVE JA TO ALLT-SW                                                   
032700     .                                                                    
032800     EJECT                                                                
032900 D-NAESTA-SIDA SECTION.                                                   
033000                                                                          
033100     MOVE MID-IDKVAOMR-NX     TO W-IDKVAOMR                               
033200     MOVE MID-IDKVATRG-NX     TO W-IDKVATRG                               
033300     MOVE MID-IDKVAGRP-NX     TO W-IDKVAGRP                               
033400     MOVE MID-ADLAGOMR-FOM-NX TO W-ADLAGOMR-FOM                           
033500     MOVE MID-ADGANG-FOM-NX   TO W-ADGANG-FOM                             
033600     MOVE MID-ADPLATS-FOM-NX  TO W-ADPLATS-FOM                            
033700     MOVE JA                  TO ALLT-SW                                  
033800     .                                                                    
033900     EJECT                                                                
034000 E-SAMMA-SIDA SECTION.                                                    
034100                                                                          
034200     IF MID-INPUT = ALL '+'                                               
034300        MOVE MID-IDKVAOMR-EN     TO W-IDKVAOMR                            
034400        MOVE MID-IDKVATRG-EN     TO W-IDKVATRG                            
034500        MOVE MID-IDKVAGRP-EN     TO W-IDKVAGRP                            
034600        MOVE MID-ADLAGOMR-FOM-EN TO W-ADLAGOMR-FOM                        
034700        MOVE MID-ADGANG-FOM-EN   TO W-ADGANG-FOM                          
034800        MOVE MID-ADPLATS-FOM-EN  TO W-ADPLATS-FOM                         
034900        MOVE JA                  TO ALLT-SW                               
035000     ELSE                                                                 
035100        MOVE NEJ                 TO ALLT-SW                               
035200        MOVE '003'               TO MED-IDMFSFEL                          
035300        CALL WMEDKONV USING MED-WMEDAREA                                  
035400        MOVE MED-MFSFEL          TO MOD-TEMFSFEL                          
035500        PERFORM MFS-ROR-EJ-FAELT-IN                                       
035600        PERFORM MFS-ROR-EJ-FAELT-UT                                       
035700        PERFORM MFS-LAS-IN-IGEN                                           
035800     END-IF                                                               
035900     .                                                                    
036000     EJECT                                                                
036100 F-LAES-VISA-INFO SECTION.                                                
036200                                                                          
036300     PERFORM IMS-GU-W6KVAA01-FROM                                         
036400                                                                          
036500     MOVE +1 TO INDX                                                      
036600                                                                          
036700     PERFORM UNTIL SEGMENT-SLUT OR INDX > MAX-RADER OR                    
036800       SEGMENT-SAKNAS                                                     
036900                                                                          
037000       IF INDX = +1                                                       
037100          MOVE GRP-IDKVAOMR     TO MOD-IDKVAOMR-EN                        
037200          MOVE GRP-IDKVATRG     TO MOD-IDKVATRG-EN                        
037300          MOVE GRP-IDKVAGRP     TO MOD-IDKVAGRP-EN                        
037400          MOVE GRP-ADLAGOMR-FOM TO MOD-ADLAGOMR-FOM-EN                    
037500          MOVE GRP-ADGANG-FOM   TO MOD-ADGANG-FOM-EN                      
037600          MOVE GRP-ADPLATS-FOM  TO MOD-ADPLATS-FOM-EN                     
037700          IF MFS-KDTRTYP = 'U' AND NOT MID-KDCMD-DELETE                   
037800             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
037900                                   MOD-IDKVAOMR-ATTR     (INDX)           
038000                                   MOD-IDKVATRG-ATTR     (INDX)           
038100                                   MOD-IDKVAGRP-ATTR     (INDX)           
038200                                   MOD-ADLAGOMR-FOM-ATTR (INDX)           
038300                                   MOD-ADGANG-FOM-ATTR   (INDX)           
038400                                   MOD-ADPLATS-FOM-ATTR  (INDX)           
038500                                   MOD-ADLAGOMR-TOM-ATTR (INDX)           
038600                                   MOD-ADGANG-TOM-ATTR   (INDX)           
038700                                   MOD-ADPLATS-TOM-ATTR  (INDX)           
038800          END-IF                                                          
038900       END-IF                                                             
039000                                                                          
039100                                                                          
039200       MOVE GRP-IDKVAOMR        TO MOD-IDKVAOMR      (INDX)               
039300       MOVE GRP-IDKVATRG        TO MOD-IDKVATRG      (INDX)               
039400       MOVE GRP-IDKVAGRP        TO MOD-IDKVAGRP      (INDX)               
039500       MOVE GRP-ADLAGOMR-FOM    TO MOD-ADLAGOMR-FOM  (INDX)               
039600       MOVE GRP-ADGANG-FOM      TO MOD-ADGANG-FOM    (INDX)               
039700       MOVE GRP-ADPLATS-FOM     TO MOD-ADPLATS-FOM   (INDX)               
039800       MOVE GRP-ADLAGOMR-TOM    TO MOD-ADLAGOMR-TOM  (INDX)               
039900       MOVE GRP-ADGANG-TOM      TO MOD-ADGANG-TOM    (INDX)               
040000       MOVE GRP-ADPLATS-TOM     TO MOD-ADPLATS-TOM   (INDX)               
040100                                                                          
040200       ADD +1                   TO INDX                                   
040300       PERFORM IMS-GN-W6KVAA01-FROM                                       
040400     END-PERFORM                                                          
040500                                                                          
040600     IF INDX > MAX-RADER AND NOT (SEGMENT-SLUT OR SEGMENT-SAKNAS)         
040700        MOVE GRP-IDKVAOMR         TO MOD-IDKVAOMR-NX                      
040800        MOVE GRP-IDKVATRG         TO MOD-IDKVATRG-NX                      
040900        MOVE GRP-IDKVAGRP         TO MOD-IDKVAGRP-NX                      
041000        MOVE GRP-ADLAGOMR-FOM     TO MOD-ADLAGOMR-FOM-NX                  
041100        MOVE GRP-ADGANG-FOM       TO MOD-ADGANG-FOM-NX                    
041200        MOVE GRP-ADPLATS-FOM      TO MOD-ADPLATS-FOM-NX                   
041300        MOVE '105'                 TO MED-IDMFSFEL                        
041400        CALL WMEDKONV USING MED-WMEDAREA                                  
041500        MOVE MED-MFSFEL            TO MOD-TEMFSINF                        
041600     ELSE                                                                 
041700        MOVE MOD-IDKVAOMR-EN      TO MOD-IDKVAOMR-NX                      
041800        MOVE MOD-IDKVATRG-EN      TO MOD-IDKVATRG-NX                      
041900        MOVE MOD-IDKVAGRP-EN      TO MOD-IDKVAGRP-NX                      
042000        MOVE MOD-ADLAGOMR-FOM-EN  TO MOD-ADLAGOMR-FOM-NX                  
042100        MOVE MOD-ADGANG-FOM-EN    TO MOD-ADGANG-FOM-NX                    
042200        MOVE MOD-ADPLATS-FOM-EN   TO MOD-ADPLATS-FOM-NX                   
042300     END-IF                                                               
042400                                                                          
042500     PERFORM UNTIL INDX > MAX-RADER                                       
042600       MOVE MFS-RENSA-FAELT       TO MOD-IDKVAOMR      (INDX)             
042700                                     MOD-IDKVATRG      (INDX)             
042800                                     MOD-IDKVAGRP      (INDX)             
042900                                     MOD-ADLAGOMR-FOM  (INDX)             
043000                                     MOD-ADGANG-FOM    (INDX)             
043100                                     MOD-ADPLATS-FOM   (INDX)             
043200                                     MOD-ADLAGOMR-TOM  (INDX)             
043300                                     MOD-ADGANG-TOM    (INDX)             
043400                                     MOD-ADPLATS-TOM   (INDX)             
043500                                                                          
043600       ADD +1                     TO INDX                                 
043700     END-PERFORM                                                          
043800                                                                          
043900     PERFORM MFS-RENSA-FAELT-IN                                           
044000     .                                                                    
044100     EJECT                                                                
044200 G-KOLLA-INPUT SECTION.                                                   
044300                                                                          
044400     MOVE JA  TO INDATA-SW                                                
044500                                                                          
044600     IF MID-INPUT = ALL '+'                                               
044700        MOVE '011'                   TO MED-IDMFSFEL                      
044800        CALL WMEDKONV USING MED-WMEDAREA                                  
044900        MOVE MED-MFSFEL              TO MOD-TEMFSFEL                      
045000        PERFORM MFS-ROR-EJ-FAELT-IN                                       
045100        PERFORM MFS-ROR-EJ-FAELT-UT                                       
045200        MOVE NEJ                     TO INDATA-SW                         
045300        MOVE NEJ                     TO ALLT-SW                           
045400     ELSE                                                                 
045500        PERFORM MFS-LAS-IN-IGEN                                           
045600     END-IF                                                               
045700                                                                          
045800     IF INDATA-OK                                                         
045900       IF MID-IDKVAOMR = KVA-IDKVAOMR (DC-IX 1)                           
046000                     OR  KVA-IDKVAOMR (DC-IX 2)                           
046100                     OR  KVA-IDKVAOMR (DC-IX 3)                           
046200                     OR  KVA-IDKVAOMR (DC-IX 4)                           
046300                     OR  KVA-IDKVAOMR (DC-IX 5)                           
046400                     OR  KVA-IDKVAOMR (DC-IX 6)                           
046500          MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDKVAOMR-ATTR-UPP             
046600       ELSE                                                               
046700          MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDKVAOMR-ATTR-UPP             
046800          MOVE NEJ                   TO INDATA-SW                         
046900       END-IF                                                             
047000                                                                          
047100       IF MID-IDKVAGRP NUMERIC                                            
047200          MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDKVAGRP-ATTR-UPP             
047300       ELSE                                                               
047400          MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKVAGRP-ATTR-UPP             
047500          MOVE NEJ                   TO INDATA-SW                         
047600       END-IF                                                             
047700                                                                          
047800       IF MID-IDKVATRG NUMERIC                                            
047900          MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDKVATRG-ATTR-UPP             
048000       ELSE                                                               
048100          MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKVATRG-ATTR-UPP             
048200          MOVE NEJ                   TO INDATA-SW                         
048300       END-IF                                                             
048400                                                                          
048500       IF MID-ADLAGOMR-FOM NUMERIC                                        
048600          MOVE MFS-NUM-FAELT-RAETT   TO MOD-ADLAGOMR-FOM-ATTR-UPP         
048700       ELSE                                                               
048800          MOVE MFS-NUM-FAELT-FEL     TO MOD-ADLAGOMR-FOM-ATTR-UPP         
048900          MOVE NEJ                   TO INDATA-SW                         
049000       END-IF                                                             
049100                                                                          
049200       IF MID-ADGANG-FOM NUMERIC                                          
049300          MOVE MFS-NUM-FAELT-RAETT   TO MOD-ADGANG-FOM-ATTR-UPP           
049400       ELSE                                                               
049500          MOVE MFS-NUM-FAELT-FEL     TO MOD-ADGANG-FOM-ATTR-UPP           
049600          MOVE NEJ                   TO INDATA-SW                         
049700       END-IF                                                             
049800                                                                          
049900       IF MID-ADPLATS-FOM NUMERIC                                         
050000          MOVE MFS-NUM-FAELT-RAETT   TO MOD-ADPLATS-FOM-ATTR-UPP          
050100       ELSE                                                               
050200          MOVE MFS-NUM-FAELT-FEL     TO MOD-ADPLATS-FOM-ATTR-UPP          
050300          MOVE NEJ                   TO INDATA-SW                         
050400       END-IF                                                             
050500                                                                          
050600       IF INDATA-OK                                                       
050700         MOVE MID-IDKVAOMR          TO W-IDKVAOMR                         
050800         MOVE MID-IDKVATRG          TO W-IDKVATRG                         
050900         MOVE MID-IDKVAGRP          TO W-IDKVAGRP                         
051000         MOVE MID-ADLAGOMR-FOM      TO W-ADLAGOMR-FOM                     
051100         MOVE MID-ADGANG-FOM        TO W-ADGANG-FOM                       
051200         MOVE MID-ADPLATS-FOM       TO W-ADPLATS-FOM                      
051300         PERFORM IMS-GU-W6KVAA01                                          
051400         IF MID-KDCMD-DELETE                                              
051500           IF SEGMENT-SAKNAS                                              
051600              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKVAOMR-ATTR-UPP            
051700              MOVE MFS-NUM-FAELT-FEL TO MOD-IDKVATRG-ATTR-UPP             
051800              MOVE MFS-NUM-FAELT-FEL TO MOD-IDKVAGRP-ATTR-UPP             
051900              MOVE MFS-NUM-FAELT-FEL TO MOD-ADLAGOMR-FOM-ATTR-UPP         
052000              MOVE MFS-NUM-FAELT-FEL TO MOD-ADGANG-FOM-ATTR-UPP           
052100              MOVE MFS-NUM-FAELT-FEL TO MOD-ADPLATS-FOM-ATTR-UPP          
052200              MOVE NEJ              TO INDATA-SW                          
052300           END-IF                                                         
052400         ELSE                                                             
052500          IF SEGMENT-SAKNAS OR MID-ADLAGOMR-TOM NOT = ALL '+'             
052600           IF MID-ADLAGOMR-TOM NUMERIC                                    
052700             MOVE MFS-NUM-FAELT-RAETT TO MOD-ADLAGOMR-TOM-ATTR-UPP        
052800           ELSE                                                           
052900             MOVE MFS-NUM-FAELT-FEL TO MOD-ADLAGOMR-TOM-ATTR-UPP          
053000             MOVE NEJ               TO INDATA-SW                          
053100           END-IF                                                         
053200          END-IF                                                          
053300                                                                          
053400          IF SEGMENT-SAKNAS OR MID-ADGANG-TOM NOT = ALL '+'               
053500            IF MID-ADGANG-TOM NUMERIC                                     
053600               MOVE MFS-NUM-FAELT-RAETT TO MOD-ADGANG-TOM-ATTR-UPP        
053700            ELSE                                                          
053800               MOVE MFS-NUM-FAELT-FEL  TO MOD-ADGANG-TOM-ATTR-UPP         
053900               MOVE NEJ                TO INDATA-SW                       
054000            END-IF                                                        
054100          END-IF                                                          
054200                                                                          
054300          IF SEGMENT-SAKNAS OR MID-ADPLATS-TOM NOT = ALL '+'              
054400            IF MID-ADPLATS-TOM NUMERIC                                    
054500              MOVE MFS-NUM-FAELT-RAETT TO MOD-ADPLATS-TOM-ATTR-UPP        
054600            ELSE                                                          
054700              MOVE MFS-NUM-FAELT-FEL TO MOD-ADPLATS-TOM-ATTR-UPP          
054800              MOVE NEJ              TO INDATA-SW                          
054900            END-IF                                                        
055000          END-IF                                                          
055100         END-IF                                                           
055200       END-IF                                                             
055300                                                                          
055400       IF INDATA-OK AND NOT MID-KDCMD-DELETE                              
055500         MOVE W-IDKVAOMR TO W-IDKVAOMR-MIN                                
055600                            W-IDKVAOMR-MAX                                
055700         MOVE W-IDKVAGRP TO W-IDKVAGRP-MIN                                
055800                            W-IDKVAGRP-MAX                                
055900         PERFORM IMS-GU-W6KVAA01-MIN-MAX                                  
056000         IF SEGMENT-FINNS                                                 
056100           IF W-IDKVATRG NOT = GRP-IDKVATRG                               
056200             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDKVATRG-ATTR-UPP           
056300             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDKVAGRP-ATTR-UPP           
056400             MOVE NEJ                  TO INDATA-SW                       
056500           END-IF                                                         
056600         END-IF                                                           
056700       END-IF                                                             
056800                                                                          
056900       IF INDATA-FEL                                                      
057000          MOVE NEJ                   TO ALLT-SW                           
057100          MOVE '001'                 TO MED-IDMFSFEL                      
057200          CALL WMEDKONV USING MED-WMEDAREA                                
057300          MOVE MED-MFSFEL            TO MOD-TEMFSFEL                      
057400          PERFORM MFS-ROR-EJ-FAELT-IN                                     
057500          PERFORM MFS-ROR-EJ-FAELT-UT                                     
057600       END-IF                                                             
057700     END-IF                                                               
057800                                                                          
057900     .                                                                    
058000     EJECT                                                                
058100 H-UPPDATERA SECTION.                                                     
058200     SKIP2                                                                
058300     MOVE MID-IDKVAOMR         TO W-IDKVAOMR                              
058400                                  W-IDKVAOMR-MIN                          
058500                                  W-IDKVAOMR-MAX                          
058600     MOVE MID-IDKVATRG         TO W-IDKVATRG                              
058700                                  W-IDKVATRG-MIN                          
058800                                  W-IDKVATRG-MAX                          
058900     MOVE MID-IDKVAGRP         TO W-IDKVAGRP                              
059000                                  W-IDKVAGRP-MIN                          
059100                                  W-IDKVAGRP-MAX                          
059200     MOVE MID-ADLAGOMR-FOM     TO W-ADLAGOMR-FOM                          
059300                                  W-ADLAGOMR-FOM-MIN                      
059400                                  W-ADLAGOMR-FOM-MAX                      
059500     MOVE MID-ADGANG-FOM       TO W-ADGANG-FOM                            
059600                                  W-ADGANG-FOM-MIN                        
059700                                  W-ADGANG-FOM-MAX                        
059800     MOVE MID-ADPLATS-FOM      TO W-ADPLATS-FOM                           
059900                                  W-ADPLATS-FOM-MIN                       
060000                                  W-ADPLATS-FOM-MAX                       
060100     PERFORM IMS-GHU-W6KVAA01                                             
060200     IF SEGMENT-FINNS                                                     
060300        IF MID-KDCMD-DELETE                                               
060400           PERFORM IMS-DLET-W6KVAA01                                      
060500           MOVE MID-IDKVAOMR-EN      TO W-IDKVAOMR                        
060600                                        W-IDKVAOMR-MIN                    
060700                                        W-IDKVAOMR-MAX                    
060800           MOVE MID-IDKVATRG-EN      TO W-IDKVATRG                        
060900                                        W-IDKVATRG-MIN                    
061000                                        W-IDKVATRG-MAX                    
061100           MOVE MID-IDKVAGRP-EN      TO W-IDKVAGRP                        
061200                                        W-IDKVAGRP-MIN                    
061300                                        W-IDKVAGRP-MAX                    
061400           MOVE MID-ADLAGOMR-FOM-EN  TO W-ADLAGOMR-FOM                    
061500                                        W-ADLAGOMR-FOM-MIN                
061600                                        W-ADLAGOMR-FOM-MAX                
061700           MOVE MID-ADGANG-FOM-EN    TO W-ADGANG-FOM                      
061800                                        W-ADGANG-FOM-MIN                  
061900                                        W-ADGANG-FOM-MAX                  
062000           MOVE MID-ADPLATS-FOM-EN   TO W-ADPLATS-FOM                     
062100                                        W-ADPLATS-FOM-MIN                 
062200                                        W-ADPLATS-FOM-MAX                 
062300        ELSE                                                              
062400           IF MID-ADLAGOMR-TOM NOT = ALL '+'                              
062500              MOVE MID-ADLAGOMR-TOM  TO GRP-ADLAGOMR-TOM                  
062600           END-IF                                                         
062700           IF MID-ADGANG-TOM   NOT = ALL '+'                              
062800              MOVE MID-ADGANG-TOM    TO GRP-ADGANG-TOM                    
062900           END-IF                                                         
063000           IF MID-ADPLATS-TOM  NOT = ALL '+'                              
063100              MOVE MID-ADPLATS-TOM   TO GRP-ADPLATS-TOM                   
063200           END-IF                                                         
063300           PERFORM IMS-REPL-W6KVAA01                                      
063400        END-IF                                                            
063500     ELSE                                                                 
063600        MOVE W-IDDC            TO GRP-IDDC                                
063700        MOVE MID-IDKVAOMR      TO GRP-IDKVAOMR                            
063800        MOVE MID-IDKVATRG      TO GRP-IDKVATRG                            
063900        MOVE MID-IDKVAGRP      TO GRP-IDKVAGRP                            
064000        MOVE MID-ADLAGOMR-FOM  TO GRP-ADLAGOMR-FOM                        
064100        MOVE MID-ADGANG-FOM    TO GRP-ADGANG-FOM                          
064200        MOVE MID-ADPLATS-FOM   TO GRP-ADPLATS-FOM                         
064300        MOVE MID-ADLAGOMR-TOM  TO GRP-ADLAGOMR-TOM                        
064400        MOVE MID-ADGANG-TOM    TO GRP-ADGANG-TOM                          
064500        MOVE MID-ADPLATS-TOM   TO GRP-ADPLATS-TOM                         
064600        PERFORM IMS-ISRT-W6KVAA01                                         
064700     END-IF                                                               
064800                                                                          
064900     MOVE JA                   TO ALLT-SW                                 
065000     MOVE '101'                TO MED-IDMFSINF                            
065100     CALL WMEDKONV USING MED-WMEDAREA                                     
065200     MOVE MED-MFSINF           TO MOD-TEMFSINF                            
065300     PERFORM MFS-FORM-ATTR                                                
065400     PERFORM MFS-RENSA-FAELT-IN                                           
065500     .                                                                    
065600     EJECT                                                                
065700 MFS-RENSA-FAELT-IN SECTION.                                              
065800     SKIP2                                                                
065900     MOVE MFS-RENSA-FAELT TO MOD-IDKVAOMR-UPP                             
066000                             MOD-IDKVATRG-UPP                             
066100                             MOD-IDKVAGRP-UPP                             
066200                             MOD-ADLAGOMR-FOM-UPP                         
066300                             MOD-ADGANG-FOM-UPP                           
066400                             MOD-ADPLATS-FOM-UPP                          
066500                             MOD-ADLAGOMR-TOM-UPP                         
066600                             MOD-ADGANG-TOM-UPP                           
066700                             MOD-ADPLATS-TOM-UPP                          
066800                             MOD-KDCMD-UPP                                
066900     .                                                                    
067000     SKIP2                                                                
067100 MFS-RENSA-FAELT-UT SECTION.                                              
067200     SKIP2                                                                
067300     MOVE MFS-RENSA-FAELT TO MOD-IDKVAOMR-EN                              
067400                             MOD-IDKVATRG-EN                              
067500                             MOD-IDKVAGRP-EN                              
067600                             MOD-ADLAGOMR-FOM-EN                          
067700                             MOD-ADGANG-FOM-EN                            
067800                             MOD-ADPLATS-FOM-EN                           
067900                                                                          
068000                             MOD-IDKVAOMR-NX                              
068100                             MOD-IDKVATRG-NX                              
068200                             MOD-IDKVAGRP-NX                              
068300                             MOD-ADLAGOMR-FOM-NX                          
068400                             MOD-ADGANG-FOM-NX                            
068500                             MOD-ADPLATS-FOM-NX                           
068600                                                                          
068700     PERFORM MFS-RENSA-FAELT-UT-RAD                                       
068800     .                                                                    
068900     EJECT                                                                
069000 MFS-RENSA-FAELT-UT-RAD SECTION.                                          
069100     SKIP2                                                                
069200     MOVE +1 TO INDX                                                      
069300     PERFORM UNTIL INDX > MAX-RADER                                       
069400       MOVE MFS-RENSA-FAELT TO MOD-IDKVAOMR        (INDX)                 
069500                               MOD-IDKVATRG        (INDX)                 
069600                               MOD-IDKVAGRP        (INDX)                 
069700                               MOD-ADLAGOMR-FOM    (INDX)                 
069800                               MOD-ADGANG-FOM      (INDX)                 
069900                               MOD-ADPLATS-FOM     (INDX)                 
070000                               MOD-ADLAGOMR-TOM    (INDX)                 
070100                               MOD-ADGANG-TOM      (INDX)                 
070200                               MOD-ADPLATS-TOM     (INDX)                 
070300       ADD +1 TO INDX                                                     
070400     END-PERFORM                                                          
070500     .                                                                    
070600     SKIP2                                                                
070700 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
070800     SKIP2                                                                
070900     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDKVAOMR-UPP                         
071000                                 MOD-IDKVATRG-UPP                         
071100                                 MOD-IDKVAGRP-UPP                         
071200                                 MOD-ADLAGOMR-FOM-UPP                     
071300                                 MOD-ADGANG-FOM-UPP                       
071400                                 MOD-ADPLATS-FOM-UPP                      
071500                                 MOD-ADLAGOMR-TOM-UPP                     
071600                                 MOD-ADGANG-TOM-UPP                       
071700                                 MOD-ADPLATS-TOM-UPP                      
071800                                 MOD-KDCMD-UPP                            
071900     .                                                                    
072000     EJECT                                                                
072100 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
072200     SKIP2                                                                
072300     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDKVAOMR-EN                          
072400                                 MOD-IDKVATRG-EN                          
072500                                 MOD-IDKVAGRP-EN                          
072600                                 MOD-ADLAGOMR-FOM-EN                      
072700                                 MOD-ADGANG-FOM-EN                        
072800                                 MOD-ADPLATS-FOM-EN                       
072900                                                                          
073000                                 MOD-IDKVAOMR-NX                          
073100                                 MOD-IDKVATRG-NX                          
073200                                 MOD-IDKVAGRP-NX                          
073300                                 MOD-ADLAGOMR-FOM-NX                      
073400                                 MOD-ADGANG-FOM-NX                        
073500                                 MOD-ADPLATS-FOM-NX                       
073600                                                                          
073700     MOVE +1 TO INDX                                                      
073800     PERFORM UNTIL INDX > MAX-RADER                                       
073900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDKVAOMR        (INDX)               
074000                                 MOD-IDKVATRG        (INDX)               
074100                                 MOD-IDKVAGRP        (INDX)               
074200                                 MOD-ADLAGOMR-FOM    (INDX)               
074300                                 MOD-ADGANG-FOM      (INDX)               
074400                                 MOD-ADPLATS-FOM     (INDX)               
074500                                 MOD-ADLAGOMR-TOM    (INDX)               
074600                                 MOD-ADGANG-TOM      (INDX)               
074700                                 MOD-ADPLATS-TOM     (INDX)               
074800       ADD +1 TO INDX                                                     
074900     END-PERFORM                                                          
075000     .                                                                    
075100     EJECT                                                                
075200 MFS-FORM-ATTR SECTION.                                                   
075300     SKIP2                                                                
075400     MOVE MFS-FORMATETS-ATTR  TO MOD-IDKVAOMR-ATTR-UPP                    
075500                                 MOD-IDKVATRG-ATTR-UPP                    
075600                                 MOD-IDKVAGRP-ATTR-UPP                    
075700                                 MOD-ADLAGOMR-FOM-ATTR-UPP                
075800                                 MOD-ADGANG-FOM-ATTR-UPP                  
075900                                 MOD-ADPLATS-FOM-ATTR-UPP                 
076000                                 MOD-ADLAGOMR-TOM-ATTR-UPP                
076100                                 MOD-ADGANG-TOM-ATTR-UPP                  
076200                                 MOD-ADPLATS-TOM-ATTR-UPP                 
076300                                 MOD-KDCMD-ATTR-UPP                       
076400     .                                                                    
076500     SKIP2                                                                
076600 MFS-LAS-IN-IGEN SECTION.                                                 
076700     SKIP2                                                                
076800     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKVAOMR-ATTR-UPP                  
076900                                   MOD-IDKVATRG-ATTR-UPP                  
077000                                   MOD-IDKVAGRP-ATTR-UPP                  
077100                                   MOD-ADLAGOMR-FOM-ATTR-UPP              
077200                                   MOD-ADGANG-FOM-ATTR-UPP                
077300                                   MOD-ADPLATS-FOM-ATTR-UPP               
077400                                   MOD-ADLAGOMR-TOM-ATTR-UPP              
077500                                   MOD-ADGANG-TOM-ATTR-UPP                
077600                                   MOD-ADPLATS-TOM-ATTR-UPP               
077700                                   MOD-KDCMD-ATTR-UPP                     
077800     .                                                                    
077900     EJECT                                                                
078000* IMS SEKTIONER                                                           
078100     SKIP3                                                                
078200 IMS-GET-MSG SECTION.                                                     
078300                                                                          
078400     MOVE '  QC' TO GODK-STATUSKODER                                      
078500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
078600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
078700     PERFORM IMS-STATUSKONTROLL                                           
078800     .                                                                    
078900     SKIP3                                                                
079000 IMS-INSERT-MSG SECTION.                                                  
079100                                                                          
079200     IF NOT ENGLISH-TEXT                                                  
079300       MOVE '0' TO MFS-KDHUVOMR                                           
079400     END-IF                                                               
079500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
079600     MOVE SPACE TO GODK-STATUSKODER                                       
079700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
079800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
079900     PERFORM IMS-STATUSKONTROLL                                           
080000     .                                                                    
080100     EJECT                                                                
080200 IMS-GU-W6KVAA01-MIN-MAX SECTION.                                         
080300                                                                          
080400     STRING 'W6KVAA01(W6H501KY=>' W-W6H501KY-MIN-X                        
080500                    '&W6H501KY=<' W-W6H501KY-MAX-X ')'                    
080600          DELIMITED BY SIZE INTO SSA1                                     
080700     MOVE '  GE'   TO GODK-STATUSKODER                                    
080800     CALL CBLTDLI USING GU W6H5-PCB DLI-IO-AREA SSA1                      
080900     MOVE W6H5-STATUS-CODE TO STATUS-WS                                   
081000     PERFORM IMS-STATUSKONTROLL                                           
081100     .                                                                    
081200*    SKIP2                                                                
081300*IMS-GN-W6KVAA01-MIN-MAX SECTION.                                         
081400*                                                                         
081500*    STRING 'W6KVAA01(W6H501KY=>' W-W6H501KY-MIN-X                        
081600*                   '&W6H501KY=<' W-W6H501KY-MAX-X ')'                    
081700*         DELIMITED BY SIZE INTO SSA1                                     
081800*    MOVE '  GEGB'   TO GODK-STATUSKODER                                  
081900*    CALL CBLTDLI USING GN W6H5-PCB DLI-IO-AREA SSA1                      
082000*    MOVE W6H5-STATUS-CODE TO STATUS-WS                                   
082100*    PERFORM IMS-STATUSKONTROLL                                           
082200*    .                                                                    
082300     SKIP2                                                                
082400 IMS-GU-W6KVAA01-FROM SECTION.                                            
082500                                                                          
082600     STRING 'W6KVAA01(W6H501KY=>' W-W6H501KY-X ')'                        
082700          DELIMITED BY SIZE INTO SSA1                                     
082800     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
082900     CALL CBLTDLI USING GU W6H5-PCB DLI-IO-AREA SSA1                      
083000     MOVE W6H5-STATUS-CODE TO STATUS-WS                                   
083100     PERFORM IMS-STATUSKONTROLL                                           
083200     .                                                                    
083300     SKIP2                                                                
083400 IMS-GN-W6KVAA01-FROM SECTION.                                            
083500                                                                          
083600     STRING 'W6KVAA01(W6H501KY=>' W-W6H501KY-X ')'                        
083700          DELIMITED BY SIZE INTO SSA1                                     
083800     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
083900     CALL CBLTDLI USING GN W6H5-PCB DLI-IO-AREA SSA1                      
084000     MOVE W6H5-STATUS-CODE TO STATUS-WS                                   
084100     PERFORM IMS-STATUSKONTROLL                                           
084200     .                                                                    
084300     SKIP2                                                                
084400 IMS-GU-W6KVAA01 SECTION.                                                 
084500                                                                          
084600     STRING 'W6KVAA01(W6H501KY =' W-W6H501KY-X ')'                        
084700          DELIMITED BY SIZE INTO SSA1                                     
084800     MOVE '  GE' TO GODK-STATUSKODER                                      
084900     CALL CBLTDLI USING GU W6H5-PCB DLI-IO-AREA SSA1                      
085000     MOVE W6H5-STATUS-CODE TO STATUS-WS                                   
085100     PERFORM IMS-STATUSKONTROLL                                           
085200     .                                                                    
085300     SKIP2                                                                
085400 IMS-GHU-W6KVAA01 SECTION.                                                
085500                                                                          
085600     STRING 'W6KVAA01(W6H501KY =' W-W6H501KY-X ')'                        
085700          DELIMITED BY SIZE INTO SSA1                                     
085800     MOVE '  GE' TO GODK-STATUSKODER                                      
085900     CALL CBLTDLI USING GHU W6H5-PCB DLI-IO-AREA SSA1                     
086000     MOVE W6H5-STATUS-CODE TO STATUS-WS                                   
086100     PERFORM IMS-STATUSKONTROLL                                           
086200     .                                                                    
086300     SKIP2                                                                
086400 IMS-ISRT-W6KVAA01 SECTION.                                               
086500                                                                          
086600     MOVE 'W6KVAA01 '         TO SSA1                                     
086700     MOVE '  ' TO GODK-STATUSKODER                                        
086800     CALL CBLTDLI USING ISRT W6H5-PCB DLI-IO-AREA SSA1                    
086900     MOVE W6H5-STATUS-CODE TO STATUS-WS                                   
087000     PERFORM IMS-STATUSKONTROLL                                           
087100     .                                                                    
087200     SKIP2                                                                
087300 IMS-REPL-W6KVAA01 SECTION.                                               
087400                                                                          
087500     MOVE '  ' TO GODK-STATUSKODER                                        
087600     CALL CBLTDLI USING REPL W6H5-PCB DLI-IO-AREA                         
087700     MOVE W6H5-STATUS-CODE TO STATUS-WS                                   
087800     PERFORM IMS-STATUSKONTROLL                                           
087900     .                                                                    
088000     SKIP2                                                                
088100 IMS-DLET-W6KVAA01 SECTION.                                               
088200                                                                          
088300     MOVE '  ' TO GODK-STATUSKODER                                        
088400     CALL CBLTDLI USING DLET W6H5-PCB DLI-IO-AREA                         
088500     MOVE W6H5-STATUS-CODE TO STATUS-WS                                   
088600     PERFORM IMS-STATUSKONTROLL                                           
088700     .                                                                    
088800     EJECT                                                                
088900 IMS-STATUSKONTROLL SECTION.                                              
089000                                                                          
089100     SET STATUS-IX TO 1                                                   
089200     SEARCH GODK-STATUS                                                   
089300       AT END CALL FELLOG                                                 
089400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
089500     END-SEARCH                                                           
089600     .                                                                    
