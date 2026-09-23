000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4260400.                                        
000400 AUTHOR.                 INGER NILSSON.                                   
000500 DATE-WRITTEN.           APRIL 1990.                                      
000600                                                                          
000700     REMARKS.                                                             
000800                                                                          
000900*    FUNKTION:                                                            
001000*        FILER FÖR UTSKRIFT AV LISTOR.                                    
001100*                                                                         
001200*        INDATA  WDR1 ( IDHTYP 4821  FELKOD )                             
001300*                WDG2 ( IDHTYP 4823  LISTBEST.)                           
001400*                W6H5, W6H6, WDK6, WDK7, WDD3                             
001500*                                                                         
001600*        UTDATA  FILER FÖR UTSKRIFT AV KONTROLLISTA OCH FELLISTA          
001700*                                                                         
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500     SELECT W4260401 ASSIGN TO W42604D1.                                  
002600     SELECT W4260402 ASSIGN TO W42604D2.                                  
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900                                                                          
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W4260401                                                             
003300     LABEL RECORD STANDARD                                                
003400     RECORDING F                                                          
003500     BLOCK CONTAINS 0.                                                    
003600*01  W4260401-POST -COPY W4260401   -L                                    
003700     SKIP2                                                                
003800 FD  W4260402                                                             
003900     LABEL RECORD STANDARD                                                
004000     RECORDING F                                                          
004100     BLOCK CONTAINS 0.                                                    
004200*01  W4260402-POST -COPY W4260402   -L                                    
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004600                                                                          
004700*    -- CHECKED BY WY2000                                                 
004800 77  IDPGM                   PIC X(08) VALUE 'W4260400'.                  
004900*    ---- ARBETSVARIABLER                                                 
005000*                                                                         
005100 77  W-BEKVAOMR              PIC X(10) VALUE SPACE.                       
005200*      --- VALID IDDC CODES                                               
005300*                                                                         
005400*01    -COPY WWDC99                                                       
005500       EJECT                                                              
005600*                                                                         
005700*    ---- INDEXFÄLT                                                       
005800                                                                          
005900 77  OMRIX                   PIC S9(3)   VALUE +0   COMP SYNC.            
006000 77  DCIX                    PIC S9(3)   VALUE +0   COMP SYNC.            
006100                                                                          
006200     EJECT                                                                
006300*    -COPY WWKVAOMR                                                       
006400     EJECT                                                                
006500*    ---- FIL FÖR UTSKRIFT AV KONTROLLISTA                                
006600                                                                          
006700*    -COPY W4260401         -PRE UT-KTR-                                  
006800     EJECT                                                                
006900*    ---- FIL FÖR UTSKRIFT AV FELLISTA                                    
007000                                                                          
007100*    -COPY W4260402         -PRE UT-FEL-                                  
007200     EJECT                                                                
007300*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
007400     SKIP3                                                                
007500 01  DYNAMISKA-SUBPROGRAM.                                                
007600   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
007700   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
007800   03  DATKORT               PIC X(8)    VALUE 'DATKORT '.                
007900   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
008000   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
008100     EJECT                                                                
008200*    ----  PARAMETRAR TILL DATUMKORT                                      
008300                                                                          
008400 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
008500     SKIP3                                                                
008600*    -COPY WDATAREA                                                       
008700     EJECT                                                                
008800*    -COPY WDATKORT                                                       
008900     EJECT                                                                
009000*    ---- POST-AREOR OCH IMS KOMMUNIKATIONS-AREOR                         
009100     SKIP3                                                                
009200*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
009300                                                                          
009400 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
009500     SKIP3                                                                
009600*    ---- STATUSKOD FRÅN IMS                                              
009700                                                                          
009800 01  STATUS-WS               PIC XX.                                      
009900     88  SEGMENT-FINNS                    VALUE '  '.                     
010000     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
010100     88  SEGMENT-SLUT                     VALUE 'GB'.                     
010200     88  IMS-EJ-OK                        VALUE 'XD'.                     
010300     SKIP3                                                                
010400 01  GODK-STATUSKODER.                                                    
010500   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
010600     SKIP3                                                                
010700 01  SSA1                    PIC X(99).                                   
010800 01  SSA2                    PIC X(64).                                   
010900 01  SSA3                    PIC X(64).                                   
011000     EJECT                                                                
011100*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
011200                                                                          
011300 01  FILLER                  PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.          
011400 01  NYCKLAR-TILL-DLI.                                                    
011500                                                                          
011600   03  W-WDGX-4821-KEY-X.                                                 
011700     05  W-IDHTYP-4821       PIC X(04)  VALUE '4821'.                     
011800     05  FILLER              PIC X(26)  VALUE LOW-VALUE.                  
011900                                                                          
012000   03  W-WDGX-4822-KEY-X.                                                 
012100*                         FELTEXTREGISTER                                 
012200     05  W-IDKVAFEL          PIC X(02)  VALUE SPACE.                      
012300     05  FILLER              PIC X(03)  VALUE LOW-VALUE.                  
012400                                                                          
012500   03  W-WDGX-4823-KEY-X.                                                 
012600*                         LISTBESTÄLLNINGSREGISTER                        
012700     05  W-IDHTYP-4823       PIC X(04)  VALUE '4823'.                     
012800     05  FILLER              PIC X(26)  VALUE LOW-VALUE.                  
012900     SKIP3                                                                
013000   03  W6-W6H601KY-X.                                                     
013100     05  W6-IDDC             PIC  XX    VALUE SPACE.                      
013200     05  W6-IDKVAOMR         PIC  X     VALUE SPACE.                      
013300     05  W6-IDKVAGRP         PIC  999.                                    
013400     05  W6-DAREGDAT         PIC  9(08).                                  
013500     SKIP3                                                                
013600   03  W-W6H501KY-MIN-X.                                                  
013700      05  W-IDDC-MIN         PIC  XX     VALUE SPACE.                     
013800      05  W-IDKVAOMR-MIN     PIC  X      VALUE SPACE.                     
013900      05  W-IDKVATRG-MIN     PIC  99     VALUE ZERO.                      
014000      05  W-IDKVAGRP-MIN     PIC  999    VALUE ZERO.                      
014100      05  W-ADLAGOMR-FOM-MIN PIC S999    VALUE +0    COMP-3.              
014200      05  W-ADGANG-FOM-MIN   PIC S999    VALUE +0    COMP-3.              
014300      05  W-ADPLATS-FOM-MIN  PIC S9(5)   VALUE +0    COMP-3.              
014400     SKIP3                                                                
014500   03  W-W6H501KY-MAX-X.                                                  
014600      05  W-IDDC-MAX         PIC XX      VALUE HIGH-VALUE.                
014700      05  W-IDKVAOMR-MAX     PIC  X(01)  VALUE HIGH-VALUE.                
014800      05  W-IDKVATRG-MAX     PIC  9(02)  VALUE 99.                        
014900      05  W-IDKVAGRP-MAX     PIC  9(03)  VALUE 999.                       
015000      05  W-ADLAGOMR-FOM-MAX PIC S9(03)  VALUE +999  COMP-3.              
015100      05  W-ADGANG-FOM-MAX   PIC S9(03)  VALUE +999  COMP-3.              
015200      05  W-ADPLATS-FOM-MAX  PIC S9(05)  VALUE +99999 COMP-3.             
015300     SKIP3                                                                
015400   03  W-IDDC-X.                                                          
015500     05  W-IDDC              PIC XX     VALUE SPACE.                      
015600                                                                          
015700   03  W-IDARTNR-X.                                                       
015800     05  W-IDARTNR           PIC S9(09) COMP-3.                           
015900                                                                          
016000   03  W-KDSEGKEY-X.                                                      
016100     05  W-KDSEGKEY          PIC X(1)   VALUE '1'.                        
016200                                                                          
016300   03  W-IDSKYLT-X.                                                       
016400     05  W-IDSKYLT           PIC X(03).                                   
016500                                                                          
016600   03  W-IDKVAGRP-X.                                                      
016700     05  W-IDKVAGRP          PIC 9(03).                                   
016800     EJECT                                                                
016900*01  -COPY W0003                                                          
017000     EJECT                                                                
017100 01  FILLER                       PIC X(16) VALUE 'DLI-IO-AREA1'.         
017200 01  DLI-IO-AREA1.                                                        
017300   03  IO-AREA1         PIC X(30).                                        
017400     SKIP3                                                                
017500*    03 W6H501 -COPY W6H501         -RED IO-AREA1.                        
017600     EJECT                                                                
017700 01  FILLER                       PIC X(16) VALUE 'DLI-IO-AREA2'.         
017800     SKIP3                                                                
017900 01  DLI-IO-AREA2.                                                        
018000   03  IO-AREA2         PIC X(27).                                        
018100     SKIP3                                                                
018200*    03 W6H601 -COPY W6H601         -RED IO-AREA2.                        
018300     EJECT                                                                
018400 01  FILLER                       PIC X(16) VALUE 'DLI-IO-AREA3'.         
018500     SKIP3                                                                
018600 01  DLI-IO-AREA3.                                                        
018700   03  IO-AREA3         PIC X(10).                                        
018800     SKIP3                                                                
018900*    03 W6H601 -COPY W6H611         -RED IO-AREA3.                        
019000     EJECT                                                                
019100 01  FILLER                       PIC X(16) VALUE 'DLI-IO-AREA4'.         
019200     SKIP3                                                                
019300 01  DLI-IO-AREA4.                                                        
019500*    03 WDK611 -COPY WDK611                                               
019600     EJECT                                                                
019700 01  FILLER                       PIC X(16) VALUE 'DLI-IO-AREA5'.         
019800     SKIP3                                                                
019900 01  DLI-IO-AREA5.                                                        
020000   03  IO-AREA5         PIC X(120).                                       
020100*    03 WDD311 -COPY WDD311           -RED IO-AREA5.                      
020200     EJECT                                                                
020300 01  FILLER                       PIC X(16) VALUE 'DLI-IO-AREA6'.         
020400     SKIP3                                                                
020500 01  DLI-IO-AREA6.                                                        
020600   03  IO-AREA6         PIC X(400).                                       
020700*    03 WLXXJY11 -COPY WDGX4822         -RED IO-AREA6.                    
020800     EJECT                                                                
020900 01  FILLER                       PIC X(16) VALUE 'DLI-IO-AREA7'.         
021000     SKIP3                                                                
021100 01  DLI-IO-AREA7.                                                        
021200   03  IO-AREA7         PIC X(30).                                        
021300*    03 WLXXJZ01 -COPY WDGX01           -RED IO-AREA7.                    
021400     EJECT                                                                
021500*    03 WLXXJZ11 -COPY WDGX4824         -RED IO-AREA7.                    
021600     EJECT                                                                
021700 01  FILLER                       PIC X(16) VALUE 'DLI-IO-AREA8'.         
021800 01  DLI-IO-AREA8.                                                        
021900   03  IO-AREA8         PIC X(500).                                       
022000*    03 WDK711   -COPY WDK711                     -RED IO-AREA8.          
022100     EJECT                                                                
022200 LINKAGE SECTION.                                                         
022300     SKIP2                                                                
022400*01  -COPY W0009      -PRE  MSG-                                          
022500     EJECT                                                                
022600*01  -COPY W0008 -PRE  W6H5-                                              
022700       05  FILLER                PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008 -PRE  W6H6-                                              
023000       05  FILLER                PIC X.                                   
023100     EJECT                                                                
023200*01  -COPY W0008 -PRE  ARTC-                                              
023300       05  FILLER                PIC X.                                   
023400     EJECT                                                                
023500*01  -COPY W0008 -PRE  ARTS-                                              
023600       05  FILLER                PIC X.                                   
023700     EJECT                                                                
023800*01  -COPY W0008 -PRE  BENA-                                              
023900       05  FILLER                PIC X.                                   
024000     EJECT                                                                
024100*01  -COPY W0008 -PRE  XXJY-                                              
024200       05  FILLER                PIC X.                                   
024300     EJECT                                                                
024400*01  -COPY W0008 -PRE  XXJZ-                                              
024500       05  FILLER                PIC X.                                   
024600     EJECT                                                                
024700 PROCEDURE DIVISION  USING MSG-PCB W6H5-PCB W6H6-PCB ARTC-PCB             
024800                          ARTS-PCB BENA-PCB XXJY-PCB XXJZ-PCB.            
024900     ENTRY 'DLITCBL' USING MSG-PCB W6H5-PCB W6H6-PCB ARTC-PCB             
025000                          ARTS-PCB BENA-PCB XXJY-PCB XXJZ-PCB.            
025100                                                                          
025200     PERFORM A-INIT                                                       
025300                                                                          
025400     PERFORM IMS-GU-WLXXJZ01                                              
025500                                                                          
025600     PERFORM IMS-GNP-WLXXJZ11                                             
025700                                                                          
025800     PERFORM UNTIL SEGMENT-SAKNAS                                         
025900       PERFORM B-LAES-BEKVAOMR                                            
026000                                                                          
026100       MOVE 4824-IDDC      TO W6-IDDC                                     
026200       MOVE 4824-IDKVAOMR  TO W6-IDKVAOMR                                 
026300       MOVE 4824-IDKVAGRP  TO W6-IDKVAGRP                                 
026400       MOVE 4824-TIREGDAT  TO W6-DAREGDAT                                 
026500       IF 4824-TIREGDAT NOT = ZERO                                        
026600         IF 4824-TIREGDAT < 500000                                        
026700           MOVE 20         TO W6-DAREGDAT (1:2)                           
026800         ELSE                                                             
026900           IF 4824-TIREGDAT < 999999                                      
027000             MOVE 19       TO W6-DAREGDAT (1:2)                           
027100           ELSE                                                           
027200             MOVE 99999999 TO W6-DAREGDAT                                 
027300           END-IF                                                         
027400         END-IF                                                           
027500       END-IF                                                             
027600                                                                          
027700       PERFORM IMS-GU-W6KVAB01                                            
027800       MOVE 'AAMMDD'       TO DAT-KDDATFORM                               
027900       MOVE OMR-TIKVAKON   TO DAT-I-TIDATUM                               
028000       CALL WDATKONV USING    DAT-KDDATFORM  DAT-I-TIDATUM                
028100                              DAT-O-TIDATUM  DAT-KDSVAR                   
028200                                                                          
028300       MOVE OMR-IDDC       TO W-IDDC                                      
028400                                                                          
028500       IF OMR-KDKVASTA = '1'                                              
028600          PERFORM C-SKRIV-KONTROLLIST-FIL                                 
028700       ELSE                                                               
028800          IF OMR-KDKVASTA = '2'                                           
028900             PERFORM IMS-GNP-W6KVAB11                                     
029000             PERFORM UNTIL SEGMENT-SAKNAS                                 
029100               PERFORM D-SKRIV-FELLIST-FIL                                
029200               PERFORM IMS-GNP-W6KVAB11                                   
029300             END-PERFORM                                                  
029400          END-IF                                                          
029500       END-IF                                                             
029600       PERFORM IMS-GNP-WLXXJZ11                                           
029700     END-PERFORM                                                          
029800                                                                          
029900     PERFORM IMS-GHU-WLXXJZ01                                             
030000                                                                          
030100     PERFORM IMS-DLET-WLXXJZ01                                            
030200                                                                          
030300     PERFORM IMS-ISRT-WLXXJZ01                                            
030400                                                                          
030500     PERFORM Z-FINIT                                                      
030600                                                                          
030700     MOVE ZERO TO RETURN-CODE                                             
030800     GOBACK                                                               
030900     .                                                                    
031000     EJECT                                                                
031100 A-INIT SECTION.                                                          
031200     SKIP2                                                                
031300     OPEN OUTPUT W4260401 W4260402                                        
031400     .                                                                    
031500     EJECT                                                                
031600 B-LAES-BEKVAOMR SECTION.                                                 
031700     SKIP2                                                                
031800     MOVE SPACE TO W-BEKVAOMR                                             
031900                                                                          
032000     IF  4824-IDDC   =   W6-IDDC                                          
032100       IF  4824-IDKVAOMR  =  W6-IDKVAOMR                                  
032200         CONTINUE                                                         
032300       ELSE                                                               
032400         PERFORM   BA-HAEMTA-BEKVAOMR                                     
032500       END-IF                                                             
032600     ELSE                                                                 
032700       MOVE 4824-IDDC         TO WS-IDDC                                  
032800       EVALUATE TRUE                                                      
032900         WHEN CDC-SE MOVE +1  TO DCIX                                     
033000         WHEN SDC-NL MOVE +2  TO DCIX                                     
033100*        WHEN DC-FRA MOVE +3  TO DCIX                                     
033200         WHEN SDC-GB MOVE +4  TO DCIX                                     
033300         WHEN SDC-ES MOVE +5  TO DCIX                                     
033400         WHEN SDC-IT MOVE +6  TO DCIX                                     
033500         WHEN SDC-AT MOVE +7  TO DCIX                                     
033600         WHEN OTHER  MOVE +99 TO DCIX                                     
033700       END-EVALUATE                                                       
033800                                                                          
033900       IF DCIX < +99                                                      
034000         PERFORM BA-HAEMTA-BEKVAOMR                                       
034100       END-IF                                                             
034200     END-IF                                                               
034300     .                                                                    
034400     EJECT                                                                
034500 BA-HAEMTA-BEKVAOMR SECTION.                                              
034600     SKIP2                                                                
034700     MOVE +1    TO OMRIX                                                  
034800     PERFORM UNTIL OMRIX > +6 OR W-BEKVAOMR NOT = SPACE                   
034900       IF KVA-IDKVAOMR (DCIX OMRIX) = 4824-IDKVAOMR                       
035000         IF KVA-BEKVAOMR-DC (DCIX OMRIX) NOT = SPACE                      
035100           MOVE KVA-BEKVAOMR-DC (DCIX OMRIX) TO W-BEKVAOMR                
035200         ELSE                                                             
035300           IF KVA-BEKVAOMR-GB (DCIX OMRIX) NOT = SPACE                    
035400             MOVE KVA-BEKVAOMR-GB (DCIX OMRIX) TO W-BEKVAOMR              
035500           END-IF                                                         
035600         END-IF                                                           
035700       END-IF                                                             
035800       ADD +1  TO OMRIX                                                   
035900     END-PERFORM                                                          
036000     .                                                                    
036100     EJECT                                                                
036200 C-SKRIV-KONTROLLIST-FIL SECTION.                                         
036300     SKIP2                                                                
036400     MOVE OMR-IDDC           TO W-IDDC-MIN                                
036500                                W-IDDC-MAX                                
036600     MOVE OMR-IDKVAOMR       TO W-IDKVAOMR-MIN                            
036700                                W-IDKVAOMR-MAX                            
036800     MOVE OMR-IDKVAGRP       TO W-IDKVAGRP                                
036900                                                                          
037000     PERFORM IMS-GU-W6KVAA01-MIN-MAX                                      
037100                                                                          
037200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
037300                                                                          
037400       MOVE OMR-IDDC         TO UT-KTR-IDDC                               
037500       MOVE OMR-IDKVAOMR     TO UT-KTR-IDKVAOMR                           
037600       MOVE W-BEKVAOMR       TO UT-KTR-BEKVAOMR                           
037700       MOVE OMR-IDKVAGRP     TO UT-KTR-IDKVAGRP                           
037800       MOVE OMR-DAREGDAT (3:6)   TO UT-KTR-TIREGDAT                       
037900       MOVE GRP-ADLAGOMR-FOM TO UT-KTR-ADLAGOMR-FOM                       
038000       MOVE GRP-ADGANG-FOM   TO UT-KTR-ADGANG-FOM                         
038100       MOVE GRP-ADPLATS-FOM  TO UT-KTR-ADPLATS-FOM                        
038200       MOVE GRP-ADLAGOMR-TOM TO UT-KTR-ADLAGOMR-TOM                       
038300       MOVE GRP-ADGANG-TOM   TO UT-KTR-ADGANG-TOM                         
038400       MOVE GRP-ADPLATS-TOM  TO UT-KTR-ADPLATS-TOM                        
038500                                                                          
038600       WRITE W4260401-POST FROM UT-KTR-W4260401                           
038700                                                                          
038800       PERFORM IMS-GN-W6KVAA01-MIN-MAX                                    
038900     END-PERFORM                                                          
039000     .                                                                    
039100     EJECT                                                                
039200 D-SKRIV-FELLIST-FIL SECTION.                                             
039300     SKIP2                                                                
039400     MOVE OMR-IDDC              TO UT-FEL-IDDC                            
039500     MOVE OMR-IDKVAOMR          TO UT-FEL-IDKVAOMR                        
039600     MOVE W-BEKVAOMR            TO UT-FEL-BEKVAOMR                        
039700     MOVE OMR-IDKVAGRP          TO UT-FEL-IDKVAGRP                        
039800     MOVE DAT-TIAARP            TO UT-FEL-TIAARP                          
039900                                                                          
040000     MOVE ART-IDKVAFEL          TO W-IDKVAFEL                             
040100     PERFORM IMS-GU-WLXXJY11                                              
040200     MOVE 4822-IDKVAFEL         TO UT-FEL-IDKVAFEL                        
040300     MOVE 4822-KVKVAFPO         TO UT-FEL-KVKVAFPO                        
040400                                                                          
040500     MOVE ART-IDARTNR           TO UT-FEL-IDARTNR                         
040600                                   W-IDARTNR                              
040700     MOVE W-IDDC              TO WS-IDDC                                  
040800     EVALUATE TRUE                                                        
040900       WHEN CDC-SE MOVE 'S  ' TO W-IDSKYLT                                
041000           PERFORM DA-HAEMTA-LAGER-VAERDEN                                
041100           MOVE 4822-BEKVAFEL (1) TO UT-FEL-BEKVAFEL                      
041200           MOVE 4822-BEKVAFGR (1) TO UT-FEL-BEKVAFGR                      
041300                                                                          
041400       WHEN SDC-NL MOVE 'NL ' TO W-IDSKYLT                                
041500           PERFORM DA-HAEMTA-LAGER-VAERDEN                                
041600           MOVE 4822-BEKVAFEL (2) TO UT-FEL-BEKVAFEL                      
041700           MOVE 4822-BEKVAFGR (2) TO UT-FEL-BEKVAFGR                      
041800                                                                          
041900*      WHEN DC-FRA MOVE 'F  ' TO W-IDSKYLT                                
042000*          PERFORM DA-HAEMTA-LAGER-VAERDEN                                
042100*          MOVE 4822-BEKVAFEL (3) TO UT-FEL-BEKVAFEL                      
042200*          MOVE 4822-BEKVAFGR (3) TO UT-FEL-BEKVAFGR                      
042300                                                                          
042400       WHEN SDC-GB MOVE 'GB ' TO W-IDSKYLT                                
042500           PERFORM DA-HAEMTA-LAGER-VAERDEN                                
042600           MOVE 4822-BEKVAFEL (3) TO UT-FEL-BEKVAFEL                      
042700           MOVE 4822-BEKVAFGR (3) TO UT-FEL-BEKVAFGR                      
042800                                                                          
042900       WHEN SDC-ES MOVE 'E  ' TO W-IDSKYLT                                
043000           PERFORM DA-HAEMTA-LAGER-VAERDEN                                
043100           MOVE 4822-BEKVAFEL (3) TO UT-FEL-BEKVAFEL                      
043200           MOVE 4822-BEKVAFGR (3) TO UT-FEL-BEKVAFGR                      
043300                                                                          
043400       WHEN SDC-IT MOVE 'I  ' TO W-IDSKYLT                                
043500           PERFORM DA-HAEMTA-LAGER-VAERDEN                                
043600           MOVE 4822-BEKVAFEL (3) TO UT-FEL-BEKVAFEL                      
043700           MOVE 4822-BEKVAFGR (3) TO UT-FEL-BEKVAFGR                      
043800                                                                          
043900       WHEN SDC-AT MOVE 'A  ' TO W-IDSKYLT                                
044000           PERFORM DA-HAEMTA-LAGER-VAERDEN                                
044100           MOVE 4822-BEKVAFEL (3) TO UT-FEL-BEKVAFEL                      
044200           MOVE 4822-BEKVAFGR (3) TO UT-FEL-BEKVAFGR                      
044300                                                                          
044400       WHEN OTHER  MOVE 'GB ' TO W-IDSKYLT                                
044500           PERFORM DA-HAEMTA-LAGER-VAERDEN                                
044600           MOVE 4822-BEKVAFEL (3) TO UT-FEL-BEKVAFEL                      
044700           MOVE 4822-BEKVAFGR (3) TO UT-FEL-BEKVAFGR                      
044800     END-EVALUATE                                                         
044900                                                                          
045000     WRITE W4260402-POST FROM UT-FEL-W4260402                             
045100     .                                                                    
045200     EJECT                                                                
045300 DA-HAEMTA-LAGER-VAERDEN   SECTION.                                       
045400     SKIP2                                                                
045500     PERFORM IMS-GU-BENA-TEXT                                             
045600                                                                          
045700     MOVE TEXT-BEART            TO UT-FEL-BEART                           
045800                                                                          
045900     MOVE W-IDDC                TO WS-IDDC                                
046000     IF CDC-SE                                                            
046100       PERFORM IMS-GU-WLARTC11                                            
046200       MOVE CLAG-ADLAGOMR       TO UT-FEL-ADLAGOMR                        
046300       MOVE CLAG-ADGANG         TO UT-FEL-ADGANG                          
046400       MOVE CLAG-ADPLATS        TO UT-FEL-ADPLATS                         
046500     ELSE                                                                 
046600       PERFORM IMS-GU-WLARTS11                                            
046700       MOVE SLAG-ADLAGOMR       TO UT-FEL-ADLAGOMR                        
046800       MOVE SLAG-ADGANG         TO UT-FEL-ADGANG                          
046900       MOVE SLAG-ADPLATS        TO UT-FEL-ADPLATS                         
047000     END-IF                                                               
047100     .                                                                    
047200     EJECT                                                                
047300 Z-FINIT SECTION.                                                         
047400     SKIP2                                                                
047500     CLOSE W4260401 W4260402                                              
047600     .                                                                    
047700     EJECT                                                                
047800*    ---- IMS SEKTIONER                                                   
047900                                                                          
048000 IMS-GU-WLXXJZ01 SECTION.                                                 
048100                                                                          
048200     STRING 'WLXXJZ01(WDGXKEY  =' W-WDGX-4823-KEY-X ')'                   
048300          DELIMITED BY SIZE INTO SSA1                                     
048400     MOVE '  '                  TO GODK-STATUSKODER                       
048500     CALL CBLTDLI USING GU XXJZ-PCB DLI-IO-AREA7 SSA1                     
048600     MOVE XXJZ-STATUS-CODE      TO STATUS-WS                              
048700     PERFORM IMS-STATUSKONTROLL                                           
048800     .                                                                    
048900     SKIP2                                                                
049000 IMS-GNP-WLXXJZ11 SECTION.                                                
049100                                                                          
049200     MOVE 'WLXXJZ11 '           TO SSA1                                   
049300     MOVE '  GE'                TO GODK-STATUSKODER                       
049400     CALL CBLTDLI USING GNP XXJZ-PCB DLI-IO-AREA7 SSA1                    
049500     MOVE XXJZ-STATUS-CODE      TO STATUS-WS                              
049600     PERFORM IMS-STATUSKONTROLL                                           
049700     .                                                                    
049800     EJECT                                                                
049900 IMS-GHU-WLXXJZ01 SECTION.                                                
050000                                                                          
050100     STRING 'WLXXJZ01(WDGXKEY  =' W-WDGX-4823-KEY-X ')'                   
050200          DELIMITED BY SIZE INTO SSA1                                     
050300     MOVE '  '                  TO GODK-STATUSKODER                       
050400     CALL CBLTDLI USING GHU XXJZ-PCB DLI-IO-AREA7 SSA1                    
050500     MOVE XXJZ-STATUS-CODE      TO STATUS-WS                              
050600     PERFORM IMS-STATUSKONTROLL                                           
050700     .                                                                    
050800     SKIP2                                                                
050900 IMS-DLET-WLXXJZ01 SECTION.                                               
051000                                                                          
051100     MOVE '  '                  TO GODK-STATUSKODER                       
051200     CALL CBLTDLI USING DLET XXJZ-PCB DLI-IO-AREA7                        
051300     MOVE XXJZ-STATUS-CODE      TO STATUS-WS                              
051400     PERFORM IMS-STATUSKONTROLL                                           
051500     .                                                                    
051600     SKIP2                                                                
051700 IMS-ISRT-WLXXJZ01 SECTION.                                               
051800                                                                          
051900     MOVE W-WDGX-4823-KEY-X     TO DLI-IO-AREA7                           
052000     MOVE 'WLXXJZ01 '           TO SSA1                                   
052100     MOVE '  '                  TO GODK-STATUSKODER                       
052200     CALL CBLTDLI USING ISRT XXJZ-PCB DLI-IO-AREA7 SSA1                   
052300     MOVE XXJZ-STATUS-CODE      TO STATUS-WS                              
052400     PERFORM IMS-STATUSKONTROLL                                           
052500     .                                                                    
052600     EJECT                                                                
052700 IMS-GU-W6KVAA01-MIN-MAX SECTION.                                         
052800                                                                          
052900     STRING 'W6KVAA01(W6H501KY=>' W-W6H501KY-MIN-X                        
053000                    '&W6H501KY=<' W-W6H501KY-MAX-X                        
053100                    '&IDKVAGRP= ' W-IDKVAGRP-X ')'                        
053200          DELIMITED BY SIZE INTO SSA1                                     
053300     MOVE '  GE'   TO GODK-STATUSKODER                                    
053400     CALL CBLTDLI USING GU W6H5-PCB DLI-IO-AREA1 SSA1                     
053500     MOVE W6H5-STATUS-CODE TO STATUS-WS                                   
053600     PERFORM IMS-STATUSKONTROLL                                           
053700     .                                                                    
053800     SKIP2                                                                
053900 IMS-GN-W6KVAA01-MIN-MAX SECTION.                                         
054000                                                                          
054100     STRING 'W6KVAA01(W6H501KY=>' W-W6H501KY-MIN-X                        
054200                    '&W6H501KY=<' W-W6H501KY-MAX-X                        
054300                    '&IDKVAGRP= ' W-IDKVAGRP-X ')'                        
054400          DELIMITED BY SIZE INTO SSA1                                     
054500     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
054600     CALL CBLTDLI USING GN W6H5-PCB DLI-IO-AREA1 SSA1                     
054700     MOVE W6H5-STATUS-CODE TO STATUS-WS                                   
054800     PERFORM IMS-STATUSKONTROLL                                           
054900     .                                                                    
055000     EJECT                                                                
055100 IMS-GU-W6KVAB01 SECTION.                                                 
055200                                                                          
055300     STRING 'W6KVAB01(W6H601KY =' W6-W6H601KY-X ')'                       
055400          DELIMITED BY SIZE INTO SSA1                                     
055500     MOVE '  '                  TO GODK-STATUSKODER                       
055600     CALL CBLTDLI USING GU W6H6-PCB DLI-IO-AREA2 SSA1                     
055700     MOVE W6H6-STATUS-CODE      TO STATUS-WS                              
055800     PERFORM IMS-STATUSKONTROLL                                           
055900     .                                                                    
056000     SKIP2                                                                
056100 IMS-GNP-W6KVAB11 SECTION.                                                
056200                                                                          
056300     MOVE 'W6KVAB11 '           TO SSA1                                   
056400     MOVE '  GE'                TO GODK-STATUSKODER                       
056500     CALL CBLTDLI USING GNP W6H6-PCB DLI-IO-AREA3 SSA1                    
056600     MOVE W6H6-STATUS-CODE      TO STATUS-WS                              
056700     PERFORM IMS-STATUSKONTROLL                                           
056800     .                                                                    
056900     EJECT                                                                
057000 IMS-GU-WLARTC11 SECTION.                                                 
057100                                                                          
057200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
057300          DELIMITED BY SIZE INTO SSA1                                     
057400     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
057500          DELIMITED BY SIZE INTO SSA2                                     
057600     MOVE '  '                  TO GODK-STATUSKODER                       
057700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA4 SSA1 SSA2                
057800     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
057900     PERFORM IMS-STATUSKONTROLL                                           
058000     .                                                                    
058100     SKIP2                                                                
058200 IMS-GU-WLARTS11 SECTION.                                                 
058300                                                                          
058400     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
058500          DELIMITED BY SIZE INTO SSA1                                     
058600     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
058700          DELIMITED BY SIZE INTO SSA2                                     
058800     MOVE '  '                  TO GODK-STATUSKODER                       
058900     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA8 SSA1 SSA2                
059000     MOVE ARTS-STATUS-CODE      TO STATUS-WS                              
059100     PERFORM IMS-STATUSKONTROLL                                           
059200     .                                                                    
059300     EJECT                                                                
059400 IMS-GU-BENA-TEXT SECTION.                                                
059500                                                                          
059600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
059700          DELIMITED BY SIZE INTO SSA1                                     
059800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
059900          DELIMITED BY SIZE INTO SSA2                                     
060000     MOVE '  '                  TO GODK-STATUSKODER                       
060100     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-AREA5 SSA1 SSA2               
060200     MOVE BENA-STATUS-CODE      TO STATUS-WS                              
060300     PERFORM IMS-STATUSKONTROLL                                           
060400     .                                                                    
060500     EJECT                                                                
060600 IMS-GU-WLXXJY11  SECTION.                                                
060700                                                                          
060800     STRING 'WLXXJY01(WDGXKEY  =' W-WDGX-4821-KEY-X ')'                   
060900          DELIMITED BY SIZE INTO SSA1                                     
061000     STRING 'WLXXJY11(WDGXKEY  =' W-WDGX-4822-KEY-X ')'                   
061100          DELIMITED BY SIZE INTO SSA2                                     
061200     MOVE '  '                  TO GODK-STATUSKODER                       
061300     CALL CBLTDLI USING GU XXJY-PCB DLI-IO-AREA6 SSA1 SSA2                
061400     MOVE XXJY-STATUS-CODE      TO STATUS-WS                              
061500     PERFORM IMS-STATUSKONTROLL                                           
061600     .                                                                    
061700     SKIP2                                                                
061800 IMS-STATUSKONTROLL SECTION.                                              
061900                                                                          
062000     SET STATUS-IX TO 1                                                   
062100     SEARCH GODK-STATUS                                                   
062200       AT END CALL FELLOG                                                 
062300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
062400         CONTINUE                                                         
062500     END-SEARCH                                                           
062600     .                                                                    
