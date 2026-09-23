000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4121300.                                                
000400*AUTHOR.         LASSI OLGRENER.                                          
000500*DATE-WRITTEN.   93/11/05.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER NER SISTA VECKANS VOR-KÖ.                                  
001100*        SKRIVER 4 UTFILER - W41213 TILL W4121500                         
001200*                          - W41214 TILL W4121400                         
001300*                          - W41216 TILL W4121600                         
001400*                          - W41217                                       
001500*                                                                         
001600*        PROGRAMMET LÄSER      WDR480 MED SB                              
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300                                                                          
002400*          --- UTFIL VECKANS KLASS-0:OR                                   
002500     SELECT W41213                     ASSIGN TO W41213D1.                
002600*          --- UTFIL RENSNINGSPOSTER                                      
002700     SELECT W41214                     ASSIGN TO W41213D2.                
002800*          --- UTFIL LEDTIDER                                             
002900     SELECT W41216                     ASSIGN TO W41213D3.                
003000*          --- UTFIL VECKANS EJ LÖSTA RADER                               
003100     SELECT W41217                     ASSIGN TO W41213D4.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400                                                                          
003500 FILE SECTION.                                                            
003600                                                                          
003700 FD  W41213                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000     SKIP2                                                                
004100 01  UTPOST -COPY W41211  -L.                                             
004200                                                                          
004300 FD  W41214                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600     SKIP2                                                                
004700 01  UTPOST1 -COPY W41214  -L.                                            
004800                                                                          
004900 FD  W41216                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200     SKIP2                                                                
005300 01  UTPOST2 -COPY W41216  -L.                                            
005400     EJECT                                                                
005500 FD  W41217                                                               
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800     SKIP2                                                                
005900 01  UTPOST17 -COPY W41217  -L.                                           
006000     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006200     SKIP2                                                                
006201                                                                          
006210*    -- CHECKED BY WY2000                                                 
006300 77  IDPGM                       PIC X(8)    VALUE 'W4121300'.            
006400 77  JA                          PIC X(8)    VALUE 'J'.                   
006500 77  NEJ                         PIC X(8)    VALUE 'N'.                   
006600 77  FELTEXT                     PIC X(32)   VALUE SPACE.                 
006700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
006800 77  W-AKT-VECKA                 PIC 9(4)    VALUE ZERO.                  
006900 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007000                                                                          
007100 01  RAD-SW                      PIC X.                                   
007200     88  RAD-OK                              VALUE 'J'.                   
007210*      --- VALID IDDC CODES                                               
007220*                                                                         
007230*01    -COPY WWDC99                                                       
007230*01    -COPY WWDCKONS                                                     
007240       EJECT                                                              
007300                                                                          
007400 01  DYNAMISKA-SUBPROGRAM.                                                
007500*                                                                         
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008000     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
008100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL WDATKONV                                         
008400*01  -COPY WDATAREA                                                       
008500     EJECT                                                                
008600*    --- PARAMETRAR TILL WORKDAY                                          
008700*01  -COPY WORKAREA                                                       
008800     EJECT                                                                
008900*    --- PARAMETRAR TILL POSTSUM                                          
009000*01  -COPY W0005   -PRE POSTSUM-                                          
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)   VALUE 'UT11-AREA'.           
009300 01  AREA -COPY W41211    -PRE UT11-                                      
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'UT14-AREA'.           
009600 01  AREA -COPY W41214    -PRE UT14-                                      
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'UT16-AREA'.           
009900 01  AREA -COPY W41216    -PRE UT16-                                      
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'UT17-AREA'.           
010200 01  AREA -COPY W41217    -PRE UT17-                                      
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010700                                                                          
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011200     88  BASEN-SLUT                          VALUE 'GB'.                  
011300     SKIP2                                                                
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     EJECT                                                                
011700*    --- IMS FUNKTIONSKODER                                               
011800*01  -COPY W0003                                                          
011900     EJECT                                                                
012000*    ---  DLI INPUT-OUTPUT AREA                                           
012100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012200                                                                          
012300 01  DLI-IO-AREA.                                                         
012400   03  -COPY WDGX4542                                                     
012500     EJECT                                                                
012600 LINKAGE SECTION.                                                         
012700 01  -COPY W0008  -PRE WDR4-                                              
012800     05  FILLER                  PIC X.                                   
012900     EJECT                                                                
013000 PROCEDURE DIVISION  USING WDR4-PCB.                                      
013100     ENTRY 'DLITCBL' USING WDR4-PCB.                                      
013200                                                                          
013300     PERFORM A-INIT                                                       
013400                                                                          
013500     PERFORM IMS-GN-WDR4                                                  
013600     PERFORM UNTIL BASEN-SLUT                                             
013700       EVALUATE WDR4-SEG-NAME-FB                                          
013800         WHEN 'WDGX4542'                                                  
013900           IF 4542-IDANSK NUMERIC                                         
014000              PERFORM B-KOLLA-REGDAT                                      
014100              IF RAD-OK                                                   
014200                PERFORM C-SKAPA-W41213                                    
014300              END-IF                                                      
014400              IF 4542-KDVORATG = '2'                                      
014500                PERFORM D-SKAPA-W41214                                    
014700              END-IF                                                      
014710              IF 4542-KDVORATG = '0'                                      
014730                PERFORM E-SKAPA-W41217                                    
014740              END-IF                                                      
014800              PERFORM F-SKAPA-W41216                                      
014900           END-IF                                                         
015000       END-EVALUATE                                                       
015100       PERFORM IMS-GN-WDR4                                                
015200     END-PERFORM                                                          
015300     PERFORM S03-SKRIV-W41216                                             
015400                                                                          
015500     CLOSE W41213                                                         
015600           W41214                                                         
015700           W41216                                                         
015800           W41217                                                         
015900     MOVE 'S' TO POSTSUM-OPKOD                                            
016000     CALL POSTSUM USING POSTSUM-PARM                                      
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     OPEN OUTPUT W41213                                                   
016800                 W41214                                                   
016900                 W41216                                                   
017000                 W41217                                                   
017100     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
017200                                                                          
017300     MOVE 'IDAG'          TO DAT-KDDATFORM                                
017400                                                                          
017500     CALL WDATKONV  USING DAT-KDDATFORM                                   
017600                          DAT-I-TIDATUM                                   
017700                          DAT-O-TIDATUM                                   
017800                          DAT-KDSVAR                                      
017900     IF DAT-KDSVAR-FEL                                                    
018000        MOVE 'FEL FRÅN WDATKONV I A-SECTION'                              
018100                          TO FELTEXT                                      
018200        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
018300     END-IF                                                               
018400     MOVE DAT-TIAAMMDD    TO DAGENS-DATUM                                 
018500     MOVE DAT-TIAAVV-GRP  TO W-AKT-VECKA                                  
018600     MOVE W-AKT-VECKA     TO UT11-TIAAVV                                  
018700                                                                          
018800     MOVE +0              TO UT16-IDDISTR                                 
018900     .                                                                    
019000     EJECT                                                                
019100 B-KOLLA-REGDAT SECTION.                                                  
019200                                                                          
019300     IF 4542-KDVORATG < '2'                                               
019400       MOVE 'AAMMDD'        TO DAT-KDDATFORM                              
019500       MOVE 4542-TIREGDAT   TO DAT-I-TIDATUM                              
019600       CALL WDATKONV  USING DAT-KDDATFORM                                 
019700                            DAT-I-TIDATUM                                 
019800                            DAT-O-TIDATUM                                 
019900                            DAT-KDSVAR                                    
020000       IF DAT-KDSVAR-FEL                                                  
020100          MOVE 'FEL FRÅN WDATKONV I B-SECTION'                            
020200                            TO FELTEXT                                    
020300          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
020400       END-IF                                                             
020500       IF DAT-TIAAVV-GRP = W-AKT-VECKA                                    
020600         MOVE JA            TO RAD-SW                                     
020700       ELSE                                                               
020800         MOVE NEJ           TO RAD-SW                                     
020900       END-IF                                                             
021000     ELSE                                                                 
021100       MOVE JA              TO RAD-SW                                     
021200     END-IF                                                               
021300     .                                                                    
021400     EJECT                                                                
021500 C-SKAPA-W41213 SECTION.                                                  
021600                                                                          
021700     MOVE 4542-IDDISTR   TO UT11-IDDISTR                                  
021800     MOVE 4542-IDARTNR   TO UT11-IDARTNR                                  
021900     MOVE 4542-KVBEART-Q TO UT11-KVBEART                                  
022000     MOVE 4542-KDVORATG  TO UT11-KDVORATG                                 
022100     PERFORM S01-SKRIV-W41213                                             
022200     .                                                                    
022300     EJECT                                                                
022400 D-SKAPA-W41214 SECTION.                                                  
022500                                                                          
022600     MOVE 4542-IDDISTR   TO UT14-IDDISTR                                  
022700     MOVE 4542-IDANSK    TO UT14-IDANSK                                   
022800     MOVE 4542-IDARTNR   TO UT14-IDARTNR                                  
022900     MOVE 4542-IDLOPNR   TO UT14-IDLOPNR                                  
023000     MOVE 4542-IDORDER   TO UT14-IDORDER                                  
023100     PERFORM S02-SKRIV-W41214                                             
023200     .                                                                    
023300     EJECT                                                                
023400 E-SKAPA-W41217 SECTION.                                                  
023500                                                                          
023600     MOVE W-AKT-VECKA    TO UT17-TIUPPFV                                  
023700     MOVE 4542-IDARTNR   TO UT17-IDARTNR                                  
023800     MOVE 4542-IDDISTR   TO UT17-IDDISTR                                  
023900     MOVE 4542-IDKUNDNR  TO UT17-IDKUNDNR                                 
024000     MOVE 4542-IDKUNDRF  TO UT17-IDKUNDRF                                 
024100     MOVE 4542-KVBEART-Q TO UT17-KVBEART                                  
024200     MOVE 4542-IDANSK    TO UT17-IDANSK                                   
024300     MOVE 4542-IDLEVNR   TO UT17-IDLEVNR                                  
024400     MOVE 4542-TIREGDAT  TO UT17-TIREGDAT                                 
024500     PERFORM S04-SKRIV-W41217                                             
024600     .                                                                    
024700     EJECT                                                                
024800 F-SKAPA-W41216 SECTION.                                                  
024900                                                                          
025000     MOVE 4542-TIREGDAT   TO WORK-TIAAMMDD-FOM                            
025100     IF 4542-KDVORATG = '2'                                               
025200       MOVE 4542-TIUPPDAT TO WORK-TIAAMMDD-TOM                            
025300     ELSE                                                                 
025400       MOVE DAGENS-DATUM  TO WORK-TIAAMMDD-TOM                            
025500     END-IF                                                               
025600     MOVE 001             TO WORK-KDCALL                                  
025610     MOVE 4542-IDDC       TO WORK-IDDC                                    
025611                             WS-IDDC                                      
025612*FIX IDDC 22 FINNS EJ I WORKDAY                                           
025620     IF WORK-IDDC = '22'                                                  
025630       MOVE WC-SDC-NL     TO WORK-IDDC                                    
025640     END-IF                                                               
025650     IF GOOD-DDC                                                          
025660       MOVE WC-CDC-SE     TO WORK-IDDC                                    
025670     END-IF                                                               
025700     CALL WORKDAY   USING WORK-KDCALL                                     
025800                          WORK-DATE-AREA                                  
025900                          WORK-KDSVAR                                     
026000     IF WORK-KDSVAR-FEL                                                   
026210        DISPLAY 'FEL FRÅN WORKDAY I F-SECTION '                           
026220          WORK-KDCALL ' ' WORK-TIAAMMDD-FOM ' ' WORK-TIAAMMDD-TOM         
026300        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
026400     END-IF                                                               
026500                                                                          
026600     IF 4542-IDDISTR NOT = UT16-IDDISTR                                   
026700       IF UT16-IDDISTR > +0                                               
026800         PERFORM S03-SKRIV-W41216                                         
026900       END-IF                                                             
027000       PERFORM FA-NOLLA-UT2-LEDTIDER                                      
027100       MOVE W-AKT-VECKA     TO UT16-TIAAVV                                
027200       MOVE 4542-IDDISTR    TO UT16-IDDISTR                               
027300     END-IF                                                               
027400                                                                          
027500     PERFORM FB-SKAPA-LEDTID-TAB                                          
027600     .                                                                    
027700     EJECT                                                                
027800 FA-NOLLA-UT2-LEDTIDER SECTION.                                           
027900                                                                          
028000     MOVE ZERO            TO UT16-KVARBDAG-KLAR-0                         
028100                             UT16-KVARBDAG-0                              
028200                             UT16-KVARBDAG-KLAR-1                         
028300                             UT16-KVARBDAG-1                              
028400                             UT16-KVARBDAG-KLAR-2                         
028500                             UT16-KVARBDAG-2                              
028600                             UT16-KVARBDAG-KLAR-3                         
028700                             UT16-KVARBDAG-3                              
028800                             UT16-KVARBDAG-KLAR-4                         
028900                             UT16-KVARBDAG-4                              
029000                             UT16-KVARBDAG-KLAR-5                         
029100                             UT16-KVARBDAG-5                              
029200                             UT16-KVARBDAG-KLAR-6                         
029300                             UT16-KVARBDAG-6                              
029400     .                                                                    
029500     EJECT                                                                
029600 FB-SKAPA-LEDTID-TAB SECTION.                                             
029700                                                                          
029800     EVALUATE WORK-KVWORKD                                                
029900       WHEN 1                                                             
030000         IF 4542-KDVORATG = '2'                                           
030100           ADD +1           TO UT16-KVARBDAG-KLAR-0                       
030200         ELSE                                                             
030300           ADD +1           TO UT16-KVARBDAG-0                            
030400         END-IF                                                           
030500       WHEN 2                                                             
030600         IF 4542-KDVORATG = '2'                                           
030700           ADD +1           TO UT16-KVARBDAG-KLAR-1                       
030800         ELSE                                                             
030900           ADD +1           TO UT16-KVARBDAG-1                            
031000         END-IF                                                           
031100       WHEN 3                                                             
031200         IF 4542-KDVORATG = '2'                                           
031300           ADD +1           TO UT16-KVARBDAG-KLAR-2                       
031400         ELSE                                                             
031500           ADD +1           TO UT16-KVARBDAG-2                            
031600         END-IF                                                           
031700       WHEN 4                                                             
031800         IF 4542-KDVORATG = '2'                                           
031900           ADD +1           TO UT16-KVARBDAG-KLAR-3                       
032000         ELSE                                                             
032100           ADD +1           TO UT16-KVARBDAG-3                            
032200         END-IF                                                           
032300       WHEN 5                                                             
032400         IF 4542-KDVORATG = '2'                                           
032500           ADD +1           TO UT16-KVARBDAG-KLAR-4                       
032600         ELSE                                                             
032700           ADD +1           TO UT16-KVARBDAG-4                            
032800         END-IF                                                           
032900       WHEN 6                                                             
033000         IF 4542-KDVORATG = '2'                                           
033100           ADD +1           TO UT16-KVARBDAG-KLAR-5                       
033200         ELSE                                                             
033300           ADD +1           TO UT16-KVARBDAG-5                            
033400         END-IF                                                           
033500       WHEN OTHER                                                         
033600         IF 4542-KDVORATG = '2'                                           
033700           ADD +1           TO UT16-KVARBDAG-KLAR-6                       
033800         ELSE                                                             
033900           ADD +1           TO UT16-KVARBDAG-6                            
034000         END-IF                                                           
034100     END-EVALUATE                                                         
034200     .                                                                    
034300     EJECT                                                                
034400 S01-SKRIV-W41213 SECTION.                                                
034500                                                                          
034600     WRITE UTPOST FROM UT11-AREA                                          
034700                                                                          
034800     MOVE 'W41213'   TO POSTSUM-FDNAMN                                    
034900     MOVE 'W41213D1' TO POSTSUM-DDNAMN2                                   
035000     MOVE 'VOR'      TO POSTSUM-TRANSTYP                                  
035100     CALL POSTSUM USING POSTSUM-PARM                                      
035200     .                                                                    
035300     EJECT                                                                
035400 S02-SKRIV-W41214 SECTION.                                                
035500                                                                          
035600     WRITE UTPOST1 FROM UT14-AREA                                         
035700                                                                          
035800     MOVE 'W41214'   TO POSTSUM-FDNAMN                                    
035900     MOVE 'W41213D2' TO POSTSUM-DDNAMN2                                   
036000     MOVE 'DEL'      TO POSTSUM-TRANSTYP                                  
036100     CALL POSTSUM USING POSTSUM-PARM                                      
036200     .                                                                    
036300     EJECT                                                                
036400 S03-SKRIV-W41216 SECTION.                                                
036500                                                                          
036600     WRITE UTPOST2 FROM UT16-AREA                                         
036700                                                                          
036800     MOVE 'W41216'   TO POSTSUM-FDNAMN                                    
036900     MOVE 'W41213D3' TO POSTSUM-DDNAMN2                                   
037000     MOVE 'LED'      TO POSTSUM-TRANSTYP                                  
037100     CALL POSTSUM USING POSTSUM-PARM                                      
037200     .                                                                    
037300     EJECT                                                                
037400 S04-SKRIV-W41217 SECTION.                                                
037500                                                                          
037600     WRITE UTPOST17 FROM UT17-AREA                                        
037700                                                                          
037800     MOVE 'W41217'   TO POSTSUM-FDNAMN                                    
037900     MOVE 'W41213D4' TO POSTSUM-DDNAMN2                                   
038000     MOVE 'ART'      TO POSTSUM-TRANSTYP                                  
038100     CALL POSTSUM USING POSTSUM-PARM                                      
038200     .                                                                    
038300     EJECT                                                                
038400 IMS-GN-WDR4 SECTION.                                                     
038500                                                                          
038600     CALL CBLTDLI USING GN WDR4-PCB DLI-IO-AREA                           
038700     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
038800     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
038900     PERFORM IMS-STATUSKONTROLL                                           
039000     .                                                                    
039100     SKIP3                                                                
039200 IMS-STATUSKONTROLL SECTION.                                              
039300                                                                          
039400     SET STATUS-IX TO 1                                                   
039500     SEARCH GODK-STATUS                                                   
039600       AT END                                                             
039700         CALL FELLOG                                                      
039800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
039900         CONTINUE                                                         
040000     END-SEARCH                                                           
040100     .                                                                    
