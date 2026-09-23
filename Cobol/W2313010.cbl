000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2313010.                                    
000300 AUTHOR.                     IDK, GÖTEBORG.                               
000400 DATE-WRITTEN                JULI 1979.                                   
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*    FUNKTION.                                                            
000800*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W2313000                       
000900*        SOM SKÖTER OM SAMTLIGA IMS-CALL ÅT DETSAMMA.                     
001000*                                                                         
001100*    SKIP3                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300 DATA DIVISION.                                                           
001400     EJECT                                                                
001500 WORKING-STORAGE SECTION.                                                 
001600     SKIP3                                                                
001700*    -COPY WY2000W1                                                       
001800     SKIP2                                                                
001900 01  FELTEXT.                                                             
002000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
002100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
002200     SKIP3                                                                
002300 01  IXKDC                   PIC S9(9)               COMP    SYNC.        
002400                                                                          
002500 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W2313010'.                
002600*--------------------------------------- ARBETSFÄLT                       
002700 01  ARBETSFAELT.                                                         
002800     03  WS-DAGENS-DATUM     PIC S9(6)   VALUE +0.                        
002900     03  WS-DATUM-GRP.                                                    
003000       05  WS-AAR            PIC S9(2)   VALUE +0.                        
003100       05  WS-MAANAD         PIC S9(2)   VALUE +0.                        
003200       05  WS-DAG            PIC S9(2)   VALUE +0.                        
003300     03  W-KDPRODSL          PIC 9(2).                                    
003400     03  FILLER  REDEFINES W-KDPRODSL.                                    
003500         05  FILLER          PIC 9.                                       
003600         05  W-IDPROD        PIC 9.                                       
003700 01  WS-TIAAVV               PIC 9(04)   VALUE ZERO.                      
003800*--------------------------------------- KONSTANTER                       
003900                                                                          
004000 01  KONSTANTER.                                                          
004100     05  JA                  PIC X       VALUE 'J'.                       
004200     05  NEJ                 PIC X       VALUE 'N'.                       
004300     05  LAES-ART-START      PIC S9(3)   VALUE +1.                        
004400     05  LAES-INVENTERING    PIC S9(3)   VALUE +2.                        
004500     05  LAES-GEMENSAMINFO   PIC S9(3)   VALUE +3.                        
004600     05  LAES-LEVPLAN        PIC S9(3)   VALUE +4.                        
004700     05  LAES-LEVINFO        PIC S9(3)   VALUE +5.                        
004800     05  LAES-INV-ROT        PIC S9(3)   VALUE +6.                        
004900                                                                          
005000*    -COPY WWDCKONS                                                       
005100                                                                          
005200     SKIP3                                                                
005300 01  SUBPROGRAM.                                                          
005400     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
005500     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
005600     03  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
005700     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
005800     EJECT                                                                
005900*01  -COPY WDATAREA                                                       
006000     EJECT                                                                
006100*--------------------------------------- PARAMETRAR TILL DATKORT          
006200 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
006300*--------------------------------------- ARBETSAREOR TILL                 
006400*                                        IMS SEKTIONERNA                  
006500 01  IMS-WS.                                                              
006600     05  FILLER              PIC X(8)    VALUE 'IMS-WS'.                  
006700     SKIP3                                                                
006800*--------------------------------------- STATUSKOD FRÅN IMS               
006900                                                                          
007000     05  STATUS-WS           PIC XX.                                      
007100         88  SEGMENT-FINNS   VALUE '  '.                                  
007200         88  SEGMENT-SAKNAS  VALUE 'GE'.                                  
007300     SKIP3                                                                
007400     05  SSA1                PIC X(60).                                   
007500     05  SSA2                PIC X(60).                                   
007600     SKIP3                                                                
007700     05  GODK-STATUSKODER.                                                
007800         10  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.           
007900     SKIP3                                                                
008000 01  W-KDERS-UTG-X.                                                       
008100     05  W-KDERS-UTG         PIC S9(3)   VALUE ZERO  COMP-3.              
008200     SKIP3                                                                
008300 01  W-IDARTNR-X.                                                         
008400     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
008500 01  W-WDD901KY-X.                                                        
008600     05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO  COMP-3.              
008700     05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                     
008800 01  W-IDDC-X.                                                            
008900     05  W-IDDC              PIC X(2)    VALUE '11'.                      
009000     SKIP3                                                                
009100*01     -COPY W0003                                                       
009200     EJECT                                                                
009300 01  DLI-IO-AREA-01.                                                      
009400                                                                          
009500*03  WLARTC01  -COPY WDK601                                               
009600     EJECT                                                                
009700 01  DLI-IO-AREA-11.                                                      
009800                                                                          
009900*03  WLARTC11  -COPY WDK611                                               
010000     EJECT                                                                
010100 01  FILLER.                                                              
010200 03  DLI-IO-AREA             PIC X(200)  VALUE SPACE.                     
010300                                                                          
010400*03  W221BB02  -COPY WDD902  -PRE WDD902- -RED DLI-IO-AREA.               
010500     EJECT                                                                
010800*03  W221BB05  -COPY WDD905  -PRE WDD905- -RED DLI-IO-AREA.               
010900     EJECT                                                                
011000*03  WLINVC11  -COPY WDH711               -RED DLI-IO-AREA.               
011100     EJECT                                                                
011200 LINKAGE SECTION.                                                         
011300*01  AREA  -COPY W231L001    -PRE LINK1-.                                 
011400     EJECT                                                                
011500*01  AREA  -COPY W231L002    -PRE LINK2- -RED LINK1-AREA.                 
011600     EJECT                                                                
011700*01  AREA  -COPY W231L003    -PRE LINK3- -RED LINK1-AREA.                 
011800     EJECT                                                                
011900*01  AREA  -COPY W231L004    -PRE LINK4- -RED LINK1-AREA.                 
012000     EJECT                                                                
012100*01  AREA  -COPY W231L005    -PRE LINK5- -RED LINK1-AREA.                 
012200     EJECT                                                                
012500*01  AREA  -COPY W231L007    -PRE LINK7- -RED LINK1-AREA.                 
012600     EJECT                                                                
012700*    -COPY WDATKORT                                                       
012800     EJECT                                                                
012900*01  -COPY W0008     -PRE ARTC-                                           
013000        05 FILLER                        PIC X.                           
013100     EJECT                                                                
013200*01  -COPY W0008     -PRE INLB-                                           
013300        05 FILLER                        PIC X.                           
013400     EJECT                                                                
013500*01  -COPY W0008     -PRE INVC-                                           
013600        05 FILLER                        PIC X.                           
013700     EJECT                                                                
013800 PROCEDURE DIVISION  USING LINK1-AREA DATUMKORT                           
013900                                      ARTC-PCB INLB-PCB INVC-PCB.         
014000 MAIN SECTION.                                                            
014100                                                                          
014200     EVALUATE LINK1-KDCALL                                                
014300         WHEN LAES-ART-START                                              
014400             MOVE JA TO LINK1-FLJANEJ-ARTIKEL                             
014500             PERFORM A-LAS-ARTIKEL                                        
014600         WHEN LAES-INVENTERING                                            
014700             MOVE JA TO LINK2-FLJANEJ-INVSEG                              
014800             PERFORM B-LAS-INVENTERING                                    
014900         WHEN LAES-GEMENSAMINFO                                           
015000             PERFORM C-LAS-GEMENSAMINFO                                   
015100         WHEN LAES-LEVPLAN                                                
015200             PERFORM D-LAS-LEVPLAN                                        
015300         WHEN LAES-LEVINFO                                                
015400             PERFORM E-LAS-LEVINFO                                        
015500         WHEN LAES-INV-ROT                                                
015600             MOVE JA TO LINK2-FLJANEJ-INVSEG                              
015700             PERFORM F-LAS-INV-ROT                                        
015800     END-EVALUATE                                                         
015900     SKIP3                                                                
016000     MOVE ZERO   TO RETURN-CODE                                           
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-LAS-ARTIKEL SECTION.                                                   
016500                                                                          
016600     MOVE LOW-VALUE TO LINK1-IO-AREA                                      
016700     PERFORM IMS-GET-ROT-ARTC                                             
016800                                                                          
016900     IF  SEGMENT-FINNS                                                    
017000         MOVE NEJ TO LINK1-FLJANEJ-C2                                     
017100         PERFORM AA-01-SEGM                                               
017200         PERFORM IMS-GET-BARN-ARTC11                                      
017300         PERFORM AB-30-SEGM                                               
017400         PERFORM AC-31-SEGM                                               
017500         PERFORM AC-50-SEGM                                               
017600     ELSE                                                                 
017700         MOVE NEJ TO LINK1-FLJANEJ-ARTIKEL                                
017800     END-IF                                                               
017900     .                                                                    
018000     EJECT                                                                
018100 AA-01-SEGM SECTION.                                                      
018200                                                                          
018300     MOVE ART-IDARTNR        TO LINK1-IDARTNR                             
018400     MOVE ART-TIFINLV        TO LINK1-TIFINLV                             
018500     MOVE ART-TIERSDAT       TO LINK1-TIERSDAT                            
018600     MOVE ART-IDLEVNR        TO LINK1-IDLEVNR                             
018700     MOVE ART-IDFKNGRP       TO LINK1-IDFKNGRP                            
018800     MOVE ART-IDFTG          TO LINK1-IDFTG                               
018900     MOVE ART-KDPRODSL       TO LINK1-KDPRODSL W-KDPRODSL                 
019000     MOVE W-IDPROD           TO LINK1-IDPROD                              
019100     .                                                                    
019200     EJECT                                                                
019300 AB-30-SEGM SECTION.                                                      
019400                                                                          
019500     MOVE CLAG-FLAVRART      TO LINK1-FLAVRART                            
019600     MOVE CLAG-IDANSK        TO LINK1-IDANSK                              
019700     MOVE CLAG-KDHF          TO LINK1-KDHF                                
019800     MOVE CLAG-KDVVKL        TO LINK1-KDVVKL                              
019900     MOVE CLAG-KVLAAN        TO LINK1-KVLAAN                              
020000     MOVE CLAG-KVQ           TO LINK1-KVQ                                 
020100     MOVE CLAG-KVOVERF       TO LINK1-KVOVERF                             
020200     IF CLAG-KVSLUTKP > +0                                                
020300       MOVE D-AAR    TO WS-AAR                                            
020400       MOVE D-MAANAD TO WS-MAANAD                                         
020500       MOVE D-DAG    TO WS-DAG                                            
020600       MOVE WS-DATUM-GRP TO WS-DAGENS-DATUM                               
020700       MOVE CLAG-TISLUTKP     TO TMP1-YYMMDD                              
020800       MOVE WS-DAGENS-DATUM   TO TMP2-YYMMDD                              
020900       PERFORM WY2000P1                                                   
021000       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
021100         MOVE +0 TO LINK1-KVSLUTKP                                        
021200       ELSE                                                               
021300         MOVE CLAG-KVSLUTKP TO LINK1-KVSLUTKP                             
021400       END-IF                                                             
021500     ELSE                                                                 
021600       MOVE CLAG-KVSLUTKP      TO LINK1-KVSLUTKP                          
021700     END-IF                                                               
021800     .                                                                    
021900     EJECT                                                                
022000 AC-31-SEGM SECTION.                                                      
022100                                                                          
022200     MOVE +1                 TO IXKDC                                     
022300                                                                          
022400     MOVE CLAG-KVMP          TO LINK1-KVMP (IXKDC)                        
022500     MOVE CLAG-KVPB-SATS     TO LINK1-KVPB-SATS (IXKDC)                   
022600     MOVE CLAG-KVPB-SEP      TO LINK1-KVPB-SEP (IXKDC)                    
022700     MOVE CLAG-REDIRLEV      TO LINK1-REDIRLEV (IXKDC)                    
022800     .                                                                    
022900     EJECT                                                                
023000 AC-50-SEGM SECTION.                                                      
023100                                                                          
023200     MOVE CLAG-IDLKTO        TO LINK1-IDLKTO                              
023300     MOVE CLAG-PRARTSTD      TO LINK1-PRARTSTD                            
023400     .                                                                    
023500     EJECT                                                                
023600 F-LAS-INV-ROT     SECTION.                                               
023700                                                                          
023800     MOVE LINK2-IDARTNR  TO W-IDARTNR                                     
023900                                                                          
024000     PERFORM IMS-GET-WLINVC01-WDH701                                      
024100                                                                          
024200     IF  SEGMENT-SAKNAS                                                   
024300         MOVE NEJ        TO LINK2-FLJANEJ-INVSEG                          
024400     END-IF                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 B-LAS-INVENTERING SECTION.                                               
024800                                                                          
024900        PERFORM IMS-GET-WLINVC11-WDH711                                   
025000                                                                          
025100        IF  SEGMENT-SAKNAS                                                
025200            MOVE NEJ     TO LINK2-FLJANEJ-INVSEG                          
025300        ELSE                                                              
025400           MOVE INVH-DAREGDAT-CLO(3:6) TO LINK2-TIJUSTDA                  
025500           MOVE INVH-KVJUSTKV          TO LINK2-KVJUSTKV                  
025600        END-IF                                                            
025700     .                                                                    
025800     EJECT                                                                
025900 C-LAS-GEMENSAMINFO SECTION.                                              
026000                                                                          
026100     MOVE CLAG-KDUART        TO  LINK3-KDUART                             
026200     MOVE CLAG-KDLTK         TO  LINK3-KDLTK                              
026300     MOVE CLAG-KDERS         TO  LINK3-KDERS                              
026400     MOVE CLAG-KVAKS-CDC     TO  LINK3-KVAKS (1)                          
026500     ADD  CLAG-KVAKS-PAV     TO  LINK3-KVAKS (1)                          
026600     ADD  CLAG-KVAKS-T       TO  LINK3-KVAKS (1)                          
026700     MOVE CLAG-KVLS          TO  LINK3-KVLS (1)                           
026800     MOVE CLAG-KVRESS        TO  LINK3-KVRESS (1)                         
026900     MOVE CLAG-KVROS         TO  LINK3-KVROS (1)                          
027000     MOVE CLAG-KVSLAGER      TO  LINK3-KVSLAGER (1)                       
027100     MOVE ZERO               TO  LINK3-KVAKS-E (1)                        
027200     MOVE ZERO               TO  LINK3-KVAKS-F (1)                        
027300     MOVE NEJ TO LINK3-FLJANEJ-C2                                         
027400     .                                                                    
027500     EJECT                                                                
027600 D-LAS-LEVPLAN SECTION.                                                   
027700                                                                          
027800     MOVE LINK4-IDARTNR      TO W-IDARTNR-D9                              
027900     MOVE WC-CDC-SE          TO W-IDDC-D9                                 
028000     PERFORM IMS-GET-ROT-INLB                                             
028100     IF  SEGMENT-FINNS                                                    
028200         MOVE JA             TO LINK4-FLJANEJ-LEVPLAN                     
028300     ELSE                                                                 
028400         MOVE NEJ            TO LINK4-FLJANEJ-LEVPLAN                     
028500     END-IF                                                               
028600     .                                                                    
028700     EJECT                                                                
028800 E-LAS-LEVINFO SECTION.                                                   
028900                                                                          
029000     PERFORM IMS-GET-BARN-INLB                                            
029100                                                                          
029200     IF  SEGMENT-FINNS                                                    
029300         MOVE JA  TO LINK5-FLJANEJ-LEVINFO                                
029400         IF  INLB-SEG-NAME-FB = 'WLINLB11'                                
029500             PERFORM EA-02-SEGM                                           
029600         ELSE                                                             
030000             IF  INLB-SEG-NAME-FB = 'WLINLB23'                            
030100                 PERFORM EC-05-SEGM                                       
030200             END-IF                                                       
030400         END-IF                                                           
030500     ELSE                                                                 
030600             MOVE NEJ TO LINK5-FLJANEJ-LEVINFO                            
030700     END-IF                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 EA-02-SEGM SECTION.                                                      
031100                                                                          
031200     MOVE WDD902-KVBR        TO LINK5-KVBR                                
031300     .                                                                    
031400     EJECT                                                                
032100 EC-05-SEGM SECTION.                                                      
032200                                                                          
032300     MOVE WDD905-KDAVROP         TO LINK7-KDAVROP                         
032400     MOVE WDD905-KVAVROP         TO LINK7-KVAVROP                         
032500     MOVE WDD905-TIAVRDAT-INL    TO DAT-I-TIDATUM                         
032600     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
032700     CALL WDATKONV USING            DAT-KDDATFORM                         
032800                                    DAT-I-TIDATUM                         
032900                                    DAT-O-TIDATUM                         
033000                                    DAT-KDSVAR                            
033100     IF DAT-KDSVAR-FEL                                                    
033200       MOVE 'FEL VID ANROP TILL DATKONV 2' TO FELTEXT-STR                 
033300       CALL FELLOG                                                        
033400     ELSE                                                                 
033500       MOVE DAT-TIAAVV-GRP       TO WS-TIAAVV                             
033600       MOVE WS-TIAAVV            TO LINK7-TIAVROP-INL                     
033700     END-IF                                                               
033800     MOVE WDD905-TIAVRDAT-DISP   TO DAT-I-TIDATUM                         
033900     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
034000     CALL WDATKONV USING            DAT-KDDATFORM                         
034100                                    DAT-I-TIDATUM                         
034200                                    DAT-O-TIDATUM                         
034300                                    DAT-KDSVAR                            
034400     IF DAT-KDSVAR-FEL                                                    
034500       MOVE 'FEL VID ANROP TILL DATKONV 3' TO FELTEXT-STR                 
034600       CALL FELLOG                                                        
034700     ELSE                                                                 
034800       MOVE DAT-TIAAVV-GRP       TO WS-TIAAVV                             
034900       MOVE WS-TIAAVV            TO LINK7-TIAVROP-DISP                    
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 IMS-GET-ROT-ARTC SECTION.                                                
035400                                                                          
035500     MOVE '  GB'             TO GODK-STATUSKODER                          
035600     STRING 'WLARTC01(KDERS   <=' W-KDERS-UTG-X ')'                       
035700     DELIMITED BY SIZE INTO SSA1                                          
035800     CALL CBLTDLI USING GN ARTC-PCB DLI-IO-AREA-01 SSA1                   
035900     MOVE ARTC-STATUS-CODE     TO STATUS-WS                               
036000     PERFORM IMS-STATUSKONTROLL                                           
036100     .                                                                    
036200     EJECT                                                                
036300 IMS-GET-BARN-ARTC11 SECTION.                                             
036400                                                                          
036500     MOVE '  GEGAGK'         TO GODK-STATUSKODER                          
036600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11                       
036700     MOVE ARTC-STATUS-CODE     TO STATUS-WS                               
036800     PERFORM IMS-STATUSKONTROLL                                           
036900     SKIP2                                                                
037000     IF STATUS-WS = 'GA' OR 'GK'                                          
037100         MOVE SPACE TO STATUS-WS                                          
037200     END-IF                                                               
037300     .                                                                    
037400     EJECT                                                                
037500 IMS-GET-WLINVC01-WDH701 SECTION.                                         
037600                                                                          
037700     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ')'                         
037800     DELIMITED BY SIZE INTO SSA1                                          
037900     MOVE '  GE'         TO GODK-STATUSKODER                              
038000     CALL CBLTDLI USING GU  INVC-PCB DLI-IO-AREA                          
038100                                                 SSA1                     
038200     MOVE INVC-STATUS-CODE     TO STATUS-WS                               
038300     PERFORM IMS-STATUSKONTROLL                                           
038400     .                                                                    
038500     SKIP3                                                                
038600 IMS-GET-WLINVC11-WDH711 SECTION.                                         
038700                                                                          
038800     STRING 'WLINVC11(IDDC     =' W-IDDC-X ')'                            
038900     DELIMITED BY SIZE INTO SSA1                                          
039000     MOVE '  GE'         TO GODK-STATUSKODER                              
039100     CALL CBLTDLI USING GNP INVC-PCB DLI-IO-AREA                          
039200                                                 SSA1                     
039300     MOVE INVC-STATUS-CODE     TO STATUS-WS                               
039400     PERFORM IMS-STATUSKONTROLL                                           
039500     .                                                                    
039600     EJECT                                                                
039700 IMS-GET-ROT-INLB SECTION.                                                
039800                                                                          
039900     MOVE '  GE'             TO GODK-STATUSKODER                          
040000     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
040100     DELIMITED BY SIZE INTO SSA1                                          
040200     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1                      
040300     MOVE INLB-STATUS-CODE     TO STATUS-WS                               
040400     PERFORM IMS-STATUSKONTROLL                                           
040500     .                                                                    
040600     EJECT                                                                
040700 IMS-GET-BARN-INLB SECTION.                                               
040800                                                                          
040900     MOVE '  GEGAGK'         TO GODK-STATUSKODER                          
041000     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA                          
041100     MOVE INLB-STATUS-CODE     TO STATUS-WS                               
041200     PERFORM IMS-STATUSKONTROLL                                           
041300     SKIP2                                                                
041400     IF STATUS-WS = 'GA' OR 'GK'                                          
041500         MOVE SPACE TO STATUS-WS                                          
041600     END-IF                                                               
041700     .                                                                    
041800     EJECT                                                                
041900 IMS-STATUSKONTROLL SECTION.                                              
042000                                                                          
042100     SET STATUS-IX           TO 1                                         
042200     SEARCH GODK-STATUS AT END DISPLAY IMS-WS CALL FELLOG                 
042300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
042400          CONTINUE                                                        
042500     END-SEARCH                                                           
042600     .                                                                    
042700     EJECT                                                                
042800*    -COPY WY2000P1                                                       
