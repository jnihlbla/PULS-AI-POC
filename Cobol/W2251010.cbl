000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.                 W2251010.                                    
000500*              PROGRAM CONVERTED BY                                       
000600*              COBOL CONVERSION AID PO 5785-ABJ                           
000700*              CONVERSION DATE 05/25/91 19:46:12.                         
000800*AUTHOR.                     IDK, GÖTEBORG.                               
000900*DATE-WRITTEN.               SEP 1978.                                    
001000*REMARKS.                                                                 
001100*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W2251000                       
001200*        OCH SKÖTER SAMTLIGA IMS-CALL MOT DATABASERNA                     
001300*        WDK6, WDD9, WDD3 OCH WDK9                                        
001400*                                                                         
001500*                                                                         
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP3                                                                
001900 DATA DIVISION.                                                           
002000     EJECT                                                                
002100 WORKING-STORAGE SECTION.                                                 
002200                                                                          
002300*    -- CHECKED BY WY2000                                                 
002301                                                                          
002310*     -COPY WWDCKONS                                                      
002320                                                                          
002400 01  SUBPROGRAM.                                                          
002500     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
002600     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
002700     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
002800     SKIP3                                                                
002900 01  SPAR-AREA.                                                           
003000   03  SPAR-LEVBESK    OCCURS 3.                                          
003100     05  SPAR-KVAVIS-LEVBESK      PIC S9(7) COMP-3.                       
003200     05  SPAR-TIAVIDAT-LEVBESK    PIC S9(7) COMP-3.                       
003300   03  SPAR-KONV-TILEVBSK-INL1    PIC S9(5) COMP-3 VALUE ZERO.            
003400   03  SPAR-KONV-TILEVBSK-INL2    PIC S9(5) COMP-3 VALUE ZERO.            
003500   03  SPAR-KONV-TILEVBSK-AVS     PIC S9(5) COMP-3 VALUE ZERO.            
003600   03  SPAR-TIAAVVD.                                                      
003700     05  SPAR-TIAAVV              PIC 9(4) VALUE ZERO.                    
003800     05  FILLER                   PIC 9(1) VALUE ZERO.                    
003900 01  ARBETSAREOR.                                                         
004000     03  WS-DALEVBSK-AVS          PIC 9(8).                               
004100     03  FILLER  REDEFINES WS-DALEVBSK-AVS.                               
004200         05  WS-DALEVBSK-SS       PIC 9(2).                               
004300         05  WS-DALEVBSK-AAMMDD   PIC 9(6).                               
004400     EJECT                                                                
004500 01  FILLER           PIC X(16) VALUE 'WDATAREAC0      '.                 
004600*     -COPY WDATAREA.                                                     
004700*--------------------------------------- ARBETSAREOR TILL                 
004800*                                        IMS-SEKTIONERNA                  
004900 01  IMS-WS.                                                              
005000     03  FILLER              PIC X(8)    VALUE 'IMS-WS'.                  
005100     SKIP3                                                                
005200*--------------------------------------- STATUSKOD FRÅN IMS               
005300     SKIP1                                                                
005400     03  STATUS-WS           PIC X(2).                                    
005500         88  SEGMENT-FINNS               VALUE '  '.                      
005600         88  SEGMENT-SAKNAS              VALUE 'GE'.                      
005700     SKIP3                                                                
005800     03  SSA1                PIC X(40).                                   
005900     03  SSA2                PIC X(40).                                   
006000     03  SSA3                PIC X(40).                                   
006100     SKIP3                                                                
006200     03  KONSTANTER.                                                      
006300         05 JA               PIC X       VALUE 'J'.                       
006400         05  NEJ             PIC X       VALUE 'N'.                       
006500         05  IX              PIC S9(9) COMP SYNC.                         
006600     SKIP3                                                                
006700 01  W-IDARTNR-X.                                                         
006800     03  W-IDARTNR           PIC S9(9)               COMP-3.              
006810 01  W-WDD901KY-X.                                                        
006820     03  W-IDARTNR-D9        PIC S9(9)               COMP-3.              
006830     03  W-IDDC-D9           PIC X(2).                                    
006900 01  W-IDLEVNR-X.                                                         
007000     03  W-IDLEVNR           PIC X(5).                                    
007100 01  W-KDCLAGER-X.                                                        
007200     03  W-KDCLAGER          PIC S9                  COMP-3.              
007300 01  W-DALEVBSK-X.                                                        
007400     03  W-DALEVBSK          PIC  9(8).                                   
007500 01  W-KDERS-0-X.                                                         
007600     03  FILLER              PIC S9(3)  VALUE ZERO   COMP-3.              
007700 01  W-IDSKYLT-X.                                                         
007800     03  IDSKYLT             PIC  X(3)  VALUE 'S  '.                      
007900     SKIP3                                                                
008000 01  GODK-STATUSKODER.                                                    
008100     03  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC X(2).             
008200     SKIP3                                                                
008300*01  -COPY W0003.                                                         
008400     SKIP3                                                                
008500 01  DLI-IO-AREA.                                                         
008600     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
008700     SKIP3                                                                
008800     03  WLBENA01 REDEFINES IO-AREA.                                      
008900*        05  -COPY WDD311  -PRE BENA-                                     
009000      03  WLINLB11 REDEFINES IO-AREA.                                     
009100*        05  -COPY WDD902  -PRE INLB11-                                   
009200      03  WLINLB24 REDEFINES IO-AREA.                                     
009300*        05  -COPY WDD924  -PRE INLB24-                                   
009400      EJECT                                                               
009500      03  WLARTM01 REDEFINES IO-AREA.                                     
009600*        05  -COPY WDK901  -PRE ARTM01-                                   
009700      EJECT                                                               
009800                                                                          
009900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-AREA-01'.          
010000     SKIP3                                                                
010100 01  DLI-IO-AREA-01.                                                      
010200     03  IO-AREA-01               PIC X(150) VALUE SPACE.                 
010300     03  WLARTC01 REDEFINES IO-AREA-01.                                   
010400*        05  -COPY WDK601                                                 
010500     SKIP3                                                                
010600                                                                          
010700 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-11'.        
010800     SKIP3                                                                
010900 01  DLI-IO-AREA-11.                                                      
011000     03  IO-AREA-11               PIC X(900) VALUE SPACE.                 
011100     03  WLARTC11 REDEFINES IO-AREA-11.                                   
011200*        05  -COPY WDK611                                                 
011300     EJECT                                                                
011400 LINKAGE SECTION.                                                         
011500*01  AREA  -COPY W225L001   -PRE LINK-                                    
011600     EJECT                                                                
011700*01  AREA  -COPY W225L002   -PRE LINK2- -RED LINK-AREA                    
011800     EJECT                                                                
011900*01  -COPY W0008 -PRE ARTC-                                               
012000         05  FILLER                      PIC X.                           
012100     SKIP3                                                                
012200*01  -COPY W0008 -PRE BENA-                                               
012300         05  FILLER                      PIC X.                           
012400     EJECT                                                                
012500*01  -COPY W0008 -PRE ARTM-                                               
012600         05  FILLER                      PIC X.                           
012700     EJECT                                                                
012800*01  -COPY W0008 -PRE INLB-                                               
012900         05  FILLER                      PIC X.                           
013000     EJECT                                                                
013100 PROCEDURE DIVISION  USING LINK-AREA ARTC-PCB BENA-PCB                    
013200                           ARTM-PCB INLB-PCB.                             
013300     ENTRY 'DLITCBL' USING LINK-AREA ARTC-PCB BENA-PCB                    
013400                           ARTM-PCB INLB-PCB.                             
013500     SKIP2                                                                
013600     IF  LINK-LAS-ARTIKELINFO                                             
013700       PERFORM A-HAMTA-ARTIKELINFO                                        
013800       IF LINK-ANROP-OK                                                   
013900          PERFORM B-HAMTA-BENAMNING                                       
014000          PERFORM C-HAMTA-ORDER-ENTRY                                     
014100       END-IF                                                             
014200     ELSE                                                                 
014300       IF  LINK-LAS-LEVERANSINFO                                          
014400         PERFORM  D-HAMTA-LEVERANSINFO                                    
014500       END-IF                                                             
014600     END-IF                                                               
014700     MOVE ZERO               TO RETURN-CODE                               
014800     GOBACK                                                               
014900     .                                                                    
015000     EJECT                                                                
015100 A-HAMTA-ARTIKELINFO SECTION.                                             
015200     SKIP2                                                                
015300     MOVE LINK-IDARTNR       TO W-IDARTNR                                 
015400     PERFORM AA-NOLLSTALL-LINK                                            
015500     PERFORM IMS-GET-ARTC01                                               
015600     IF SEGMENT-FINNS                                                     
015700        IF ART-KDERS-UTG = 0                                              
015800           MOVE JA TO LINK-FLJANEJ-IDARTNR                                
015900           MOVE +1 TO LINK-KDCLPOST                                       
016000           PERFORM AB-FLYTTA-ARTC01-INFO                                  
016100           PERFORM IMS-GET-ARTC11                                         
016200           PERFORM AC-FLYTTA-CLAG                                         
016300        ELSE                                                              
016400           MOVE NEJ TO LINK-FLJANEJ-IDARTNR                               
016500        END-IF                                                            
016600     ELSE                                                                 
016700        MOVE NEJ TO LINK-FLJANEJ-IDARTNR                                  
016800     END-IF                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 AA-NOLLSTALL-LINK SECTION.                                               
017200     SKIP2                                                                
017300     MOVE ZERO TO LINK-KVLS                                               
017400                  LINK-KVRESS                                             
017500                  LINK-KVOKS-BULK                                         
017600                  LINK-KVOKS-DAG                                          
017700                  LINK-KVOKS-VOR                                          
017800                  LINK-KVAKS-CDC                                          
017900                  LINK-KVAKS-PAV                                          
018000                  LINK-KVAKS-T                                            
018100                  LINK-KVUTRS                                             
018200                  LINK-KVSPANT                                            
018300                  LINK-KVSLAGER                                           
018400                  LINK-TIAVIDAT-SEN                                       
018500                  LINK-KVAVIS                                             
018600                  LINK-KVPB-SEP                                           
018700                  LINK-KVPB-SATS                                          
018800                  LINK-KDERS                                              
018900                  LINK-TIINVDAT                                           
019000                  LINK-TIRODAT                                            
019100                  LINK-TIPBDAT                                            
019200     MOVE ZERO TO LINK-KDPROD                                             
019300                  LINK-KDHF                                               
019400                  LINK-KDGK                                               
019500                  LINK-KDPRODSL                                           
019600                  LINK-TIFINLV                                            
019700                  LINK-KDLTK                                              
019800                  LINK-FLLTKSP                                            
019900                  LINK-KDLEVSP                                            
020000                  LINK-IDANSK                                             
020100                  LINK-IDFKNGRP                                           
020200                  LINK-IDLKTO                                             
020300                  LINK-KDVVKL                                             
020400                  LINK-PRARTSTD                                           
020500     MOVE SPACE TO LINK-FLLSRDEL                                          
020600                   LINK-KDUART                                            
020700                   LINK-BEART-SVE                                         
020800                   LINK-FLTOPP                                            
020900                   LINK-IDPROJ                                            
021000                   LINK-IDLEVNR                                           
021100     .                                                                    
021200     EJECT                                                                
021300                                                                          
021400                                                                          
021500 AB-FLYTTA-ARTC01-INFO SECTION.                                           
021600                                                                          
021700     MOVE ART-TIFINLV          TO LINK-TIFINLV                            
021800     MOVE ART-IDLEVNR          TO LINK-IDLEVNR                            
021900     MOVE ART-IDFKNGRP         TO LINK-IDFKNGRP                           
022000     MOVE ART-KDPRODSL         TO LINK-KDPRODSL                           
022100     .                                                                    
022200     EJECT                                                                
022300                                                                          
022400                                                                          
022500 AC-FLYTTA-CLAG SECTION.                                                  
022600                                                                          
022700     MOVE CLAG-KDHF          TO LINK-KDHF                                 
022800     MOVE CLAG-IDANSK        TO LINK-IDANSK                               
022900     MOVE CLAG-KDVVKL        TO LINK-KDVVKL                               
023000     MOVE CLAG-TIAVIDAT-SEN  TO LINK-TIAVIDAT-SEN                         
023100     MOVE CLAG-KVAVIS-SEN    TO LINK-KVAVIS                               
023200     MOVE CLAG-KVPB-SEP      TO LINK-KVPB-SEP                             
023300     MOVE CLAG-KVPB-SATS     TO LINK-KVPB-SATS                            
023400     MOVE CLAG-TIPBDAT       TO LINK-TIPBDAT                              
023500                                                                          
023600     MOVE CLAG-IDLKTO        TO LINK-IDLKTO                               
023700     MOVE CLAG-PRARTSTD      TO LINK-PRARTSTD                             
023800     MOVE CLAG-TIINVDAT      TO LINK-TIINVDAT                             
023900     MOVE CLAG-FLTOPP        TO LINK-FLTOPP                               
024000     MOVE CLAG-KDGK          TO LINK-KDGK                                 
024100     MOVE CLAG-KDLTK         TO LINK-KDLTK                                
024200     MOVE CLAG-KDLEVSP       TO LINK-KDLEVSP                              
024300     MOVE CLAG-FLLSRDEL      TO LINK-FLLSRDEL                             
024400     MOVE CLAG-KDUART        TO LINK-KDUART                               
024500     MOVE CLAG-KDERS         TO LINK-KDERS                                
024600     MOVE CLAG-KVRESS        TO LINK-KVRESS                               
024700     MOVE CLAG-KVUTRS        TO LINK-KVUTRS                               
024800     MOVE CLAG-KVSPANT       TO LINK-KVSPANT                              
024900     MOVE CLAG-KVSLAGER      TO LINK-KVSLAGER                             
025000     MOVE CLAG-TIRODAT       TO LINK-TIRODAT                              
025100     MOVE CLAG-KVAKS-CDC     TO LINK-KVAKS-CDC                            
025200     MOVE CLAG-KVAKS-PAV     TO LINK-KVAKS-PAV                            
025300     MOVE CLAG-KVAKS-T       TO LINK-KVAKS-T                              
025400     MOVE CLAG-KVLS          TO LINK-KVLS                                 
025500     MOVE CLAG-IDPROJ        TO LINK-IDPROJ                               
025600     .                                                                    
025700     EJECT                                                                
025800 B-HAMTA-BENAMNING    SECTION.                                            
025900     SKIP2                                                                
026000     PERFORM IMS-GET-BENA11-BSEQ                                          
026100     MOVE BENA-TEXT-BEART  TO LINK-BEART-SVE                              
026200     .                                                                    
026300     EJECT                                                                
026400 C-HAMTA-ORDER-ENTRY SECTION.                                             
026500                                                                          
026600     PERFORM IMS-GET-ARTM01-KVAL                                          
026700     IF SEGMENT-FINNS                                                     
026800        PERFORM CA-FLYTTA-ARTM01-INFO                                     
026900     END-IF                                                               
027000     .                                                                    
027100     EJECT                                                                
027200                                                                          
027300                                                                          
027400 CA-FLYTTA-ARTM01-INFO SECTION.                                           
027500                                                                          
027600     MOVE ARTM01-ART-KVOKS-BULK   TO LINK-KVOKS-BULK                      
027700     MOVE ARTM01-ART-KVOKS-DAG    TO LINK-KVOKS-DAG                       
027800     MOVE ARTM01-ART-KVOKS-VOR    TO LINK-KVOKS-VOR                       
027900     .                                                                    
028000     EJECT                                                                
028100                                                                          
028200                                                                          
028300 D-HAMTA-LEVERANSINFO SECTION.                                            
028400     SKIP2                                                                
028500     MOVE LINK2-IDARTNR      TO W-IDARTNR                                 
028600     MOVE LINK2-IDLEVNR      TO W-IDLEVNR                                 
028700     MOVE LINK2-TILEVBSK     TO DAT-I-TIDATUM                             
028800     MOVE 'AAVV  '           TO DAT-KDDATFORM                             
028900     CALL WDATKONV USING DAT-KDDATFORM                                    
029000     DAT-I-TIDATUM                                                        
029100     DAT-O-TIDATUM                                                        
029200     DAT-KDSVAR                                                           
029300     IF DAT-KDSVAR-OK                                                     
029400       MOVE DAT-TIAAMMDD    TO WS-DALEVBSK-AAMMDD                         
029500       MOVE DAT-TISEKEL     TO WS-DALEVBSK-SS                             
029600       MOVE WS-DALEVBSK-AVS TO W-DALEVBSK                                 
029700     END-IF                                                               
029800     PERFORM DA-NOLLSTALL-LINK2-SPAR                                      
029900     MOVE +1 TO IX                                                        
030000     MOVE W-IDARTNR  TO W-IDARTNR-D9                                      
030010     MOVE WC-CDC-SE  TO W-IDDC-D9                                         
030100     PERFORM IMS-GET-INLB01                                               
030200     IF  SEGMENT-FINNS                                                    
030300       PERFORM IMS-GET-INLB11                                             
030400       IF SEGMENT-FINNS                                                   
030500         MOVE JA       TO LINK2-FLJANEJ-IDLEVNR                           
030600         PERFORM UNTIL SEGMENT-SAKNAS                                     
030700           MOVE INLB11-IDLEVNR   TO W-IDLEVNR                             
030800           IF INLB11-IDLEVNR = LINK2-IDLEVNR                              
030900             MOVE INLB11-KVBR TO LINK2-KVBR                               
031000             PERFORM IMS-GET-INLB24                                       
031100             PERFORM UNTIL SEGMENT-SAKNAS OR IX = 3                       
031200               PERFORM DB-KONV-AVSDATUM                                   
031300               IF IX = +1                                                 
031400                 PERFORM DC-KONV-INLEVDATUM                               
031500                 MOVE SPAR-KONV-TILEVBSK-INL1 TO                          
031600                 LINK2-TILEVBSK-INLC1                                     
031700                 MOVE SPAR-KONV-TILEVBSK-INL2 TO                          
031800                 LINK2-TILEVBSK-INLC2                                     
031900               END-IF                                                     
032000               IF INLB24-LEV-KVAVIS-BSKKVAR    > 0                        
032100               AND IX < 3                                                 
032200                 ADD +1 TO IX                                             
032300                 MOVE INLB24-LEV-KVAVIS-BSKKVAR    TO                     
032400                 LINK2-KVAVIS-LEVBESK(IX)                                 
032500                 MOVE SPAR-KONV-TILEVBSK-AVS TO                           
032600                 LINK2-TIAVIDAT-LEVBESK(IX)                               
032700               END-IF                                                     
032800               IF IX < +3                                                 
032900                 PERFORM IMS-GET-INLB24                                   
033000               END-IF                                                     
033100             END-PERFORM                                                  
033200           ELSE                                                           
033300             PERFORM IMS-GET-INLB24                                       
033400             PERFORM UNTIL SEGMENT-SAKNAS OR IX = 3                       
033500               PERFORM DB-KONV-AVSDATUM                                   
033600               IF IX = +1                                                 
033700                 PERFORM DC-KONV-INLEVDATUM                               
033800                 MOVE SPAR-KONV-TILEVBSK-INL1 TO                          
033900                 LINK2-TILEVBSK-INLC1                                     
034000                 MOVE SPAR-KONV-TILEVBSK-INL2 TO                          
034100                 LINK2-TILEVBSK-INLC2                                     
034200               END-IF                                                     
034300               IF INLB24-LEV-KVAVIS-BSKKVAR    > 0                        
034400               AND IX < 3                                                 
034500                 ADD +1 TO IX                                             
034600                 MOVE INLB24-LEV-KVAVIS-BSKKVAR                           
034700                 TO SPAR-KVAVIS-LEVBESK (IX)                              
034800                 MOVE SPAR-KONV-TILEVBSK-AVS                              
034900                 TO SPAR-TIAVIDAT-LEVBESK (IX)                            
035000               END-IF                                                     
035100               IF IX < +3                                                 
035200                 PERFORM IMS-GET-INLB24                                   
035300               END-IF                                                     
035400             END-PERFORM                                                  
035500           END-IF                                                         
035600           PERFORM IMS-GET-INLB11                                         
035700         END-PERFORM                                                      
035800         MOVE +1 TO IX                                                    
035900         IF LINK2-KVAVIS-LEVBESK(1) = ZERO                                
036000         AND LINK2-TIAVIDAT-LEVBESK(1) = ZERO                             
036100           MOVE SPAR-KVAVIS-LEVBESK (IX)                                  
036200           TO LINK2-KVAVIS-LEVBESK(1)                                     
036300           MOVE SPAR-TIAVIDAT-LEVBESK (IX)                                
036400           TO LINK2-TIAVIDAT-LEVBESK(1)                                   
036500           ADD +1 TO IX                                                   
036600         END-IF                                                           
036700         IF LINK2-KVAVIS-LEVBESK(2) = ZERO                                
036800         AND LINK2-TIAVIDAT-LEVBESK(2) = ZERO                             
036900           MOVE SPAR-KVAVIS-LEVBESK (IX)                                  
037000           TO LINK2-KVAVIS-LEVBESK(2)                                     
037100           MOVE SPAR-TIAVIDAT-LEVBESK (IX)                                
037200           TO LINK2-TIAVIDAT-LEVBESK(2)                                   
037300           ADD +1 TO IX                                                   
037400         END-IF                                                           
037500         IF LINK2-KVAVIS-LEVBESK(3) = ZERO                                
037600         AND LINK2-TIAVIDAT-LEVBESK(3) = ZERO                             
037700           MOVE SPAR-KVAVIS-LEVBESK (IX)                                  
037800           TO LINK2-KVAVIS-LEVBESK(3)                                     
037900           MOVE SPAR-TIAVIDAT-LEVBESK (IX)                                
038000           TO LINK2-TIAVIDAT-LEVBESK(3)                                   
038100           ADD +1 TO IX                                                   
038200         END-IF                                                           
038300       ELSE                                                               
038400         MOVE NEJ      TO LINK2-FLJANEJ-IDLEVNR                           
038500       END-IF                                                             
038600     ELSE                                                                 
038700       MOVE NEJ      TO LINK2-FLJANEJ-IDLEVNR                             
038800     END-IF                                                               
038900     .                                                                    
039000     EJECT                                                                
039100 DA-NOLLSTALL-LINK2-SPAR SECTION.                                         
039200     SKIP2                                                                
039300     MOVE ZERO               TO LINK2-KVBR                                
039400     LINK2-TILEVBSK-INLC1                                                 
039500     LINK2-TILEVBSK-INLC2                                                 
039600     SPAR-KONV-TILEVBSK-INL1                                              
039700     SPAR-KONV-TILEVBSK-INL2                                              
039800     SPAR-KONV-TILEVBSK-AVS                                               
039900     SPAR-TIAAVV                                                          
040000     MOVE +1 TO IX                                                        
040100     PERFORM UNTIL IX > +3                                                
040200       MOVE ZERO TO SPAR-KVAVIS-LEVBESK (IX)                              
040300                    SPAR-TIAVIDAT-LEVBESK (IX)                            
040400                    LINK2-KVAVIS-LEVBESK(IX)                              
040500                    LINK2-TIAVIDAT-LEVBESK(IX)                            
040600       ADD +1 TO IX                                                       
040700     END-PERFORM                                                          
040800     .                                                                    
040900     EJECT                                                                
041000 DB-KONV-AVSDATUM SECTION.                                                
041100     SKIP2                                                                
041200     MOVE INLB24-LEV-DALEVBSK-AVS TO WS-DALEVBSK-AVS                      
041300     MOVE WS-DALEVBSK-AAMMDD      TO DAT-I-TIDATUM                        
041400     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
041500     CALL WDATKONV USING DAT-KDDATFORM                                    
041600     DAT-I-TIDATUM                                                        
041700     DAT-O-TIDATUM                                                        
041800     DAT-KDSVAR                                                           
041900     IF DAT-KDSVAR-OK                                                     
042000       MOVE DAT-TIAAVVD TO SPAR-TIAAVVD                                   
042100       MOVE SPAR-TIAAVV TO SPAR-KONV-TILEVBSK-AVS                         
042200     END-IF                                                               
042300     .                                                                    
042400     EJECT                                                                
042500 DC-KONV-INLEVDATUM SECTION.                                              
042600     SKIP2                                                                
042700     IF INLB24-LEV-TILEVBSK-INL    > 0                                    
042800       MOVE INLB24-LEV-TILEVBSK-INL    TO DAT-I-TIDATUM                   
042900       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
043000       CALL WDATKONV USING DAT-KDDATFORM                                  
043100       DAT-I-TIDATUM                                                      
043200       DAT-O-TIDATUM                                                      
043300       DAT-KDSVAR                                                         
043400       IF DAT-KDSVAR-OK                                                   
043500         MOVE DAT-TIAAVVD TO SPAR-TIAAVVD                                 
043600         MOVE SPAR-TIAAVV TO SPAR-KONV-TILEVBSK-INL1                      
043700       END-IF                                                             
043800     END-IF                                                               
043900     .                                                                    
044000     EJECT                                                                
044100 IMS-GET-ARTC01 SECTION.                                                  
044200     SKIP2                                                                
044300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
044400     DELIMITED BY SIZE INTO SSA1                                          
044500     MOVE '  GE'         TO GODK-STATUSKODER                              
044600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
044700     MOVE ARTC-STATUS-CODE   TO STATUS-WS                                 
044800     PERFORM IMS-STATUSKONTROLL                                           
044900     .                                                                    
045000     EJECT                                                                
045100*                                           SPLIT-FIX -                   
045200                                                                          
045300 IMS-GET-ARTC11 SECTION.                                                  
045400     SKIP2                                                                
045500     MOVE 'WLARTC11'         TO SSA1                                      
045600     MOVE '  '               TO GODK-STATUSKODER                          
045700     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
045800     MOVE ARTC-STATUS-CODE   TO STATUS-WS                                 
045900     PERFORM IMS-STATUSKONTROLL                                           
046000     .                                                                    
046100     EJECT                                                                
046200 IMS-GET-INLB01 SECTION.                                                  
046300     SKIP2                                                                
046400     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
046500     DELIMITED BY SIZE INTO SSA1                                          
046600     MOVE '  GE'             TO GODK-STATUSKODER                          
046700     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1                      
046800     MOVE INLB-STATUS-CODE   TO STATUS-WS                                 
046900     PERFORM IMS-STATUSKONTROLL                                           
047000     .                                                                    
047100     EJECT                                                                
047200 IMS-GET-INLB11 SECTION.                                                  
047300     SKIP2                                                                
047400     MOVE 'WLINLB11' TO SSA1                                              
047500     MOVE '  GE'             TO GODK-STATUSKODER                          
047600     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
047700     MOVE INLB-STATUS-CODE   TO STATUS-WS                                 
047800     PERFORM IMS-STATUSKONTROLL                                           
047900     .                                                                    
048000     EJECT                                                                
048100 IMS-GET-INLB24 SECTION.                                                  
048200     SKIP2                                                                
048300     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
048400     DELIMITED BY SIZE INTO SSA1                                          
048500     MOVE 'WLINLB24' TO SSA2                                              
048600     MOVE '  GE'             TO GODK-STATUSKODER                          
048700     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1 SSA2                
048800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
048900     PERFORM IMS-STATUSKONTROLL                                           
049000     .                                                                    
049100     EJECT                                                                
049200 IMS-GET-BENA11-BSEQ SECTION.                                             
049300     SKIP2                                                                
049400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
049500     DELIMITED BY SIZE INTO SSA1                                          
049600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
049700     DELIMITED BY SIZE INTO SSA2                                          
049800     MOVE '  '             TO GODK-STATUSKODER                            
049900     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
050000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
050100     PERFORM IMS-STATUSKONTROLL                                           
050200     .                                                                    
050300     EJECT                                                                
050400 IMS-GET-ARTM01-KVAL SECTION.                                             
050500     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
050600            DELIMITED BY SIZE INTO SSA1                                   
050700     MOVE '  GE'                 TO GODK-STATUSKODER                      
050800     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA SSA1                      
050900     MOVE ARTM-STATUS-CODE       TO STATUS-WS                             
051000     PERFORM IMS-STATUSKONTROLL                                           
051100     SKIP3                                                                
051200     .                                                                    
051300 IMS-STATUSKONTROLL SECTION.                                              
051400     SKIP2                                                                
051500     SET STATUS-IX TO 1                                                   
051600     SEARCH  GODK-STATUS                                                  
051700        AT END CALL FELLOG                                                
051800        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                 
051900        CONTINUE                                                          
052000     END-SEARCH                                                           
052100     .                                                                    
