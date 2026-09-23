000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2313000.                                    
000300 AUTHOR.                     IDK, GÖTEBORG.                               
000400 DATE-WRITTEN.               JUNI 1979.                                   
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*    FUNKTION.                                                            
000800*                                                                         
000900*        PROGRAMMET LÄSER W221BB I SEKVENS (WDK6 + WDD9) OCH              
001000*        SKAPAR ARTREGFIL, INVENTERINGSFIL OCH BER.OMS/LEVERANTÖR.        
001100*                                                                         
001200*    SUBPROGRAM.                                                          
001300*        W2313010            IMS SUBMODUL                                 
001400*        POSTSUM                                                          
001500*        DATKORT                                                          
001600*        W22222                                                           
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000 FILE-CONTROL.                                                            
002100     SKIP3                                                                
002200*-------------------- UTFILER                                             
002300     SKIP3                                                                
002400*--------------------------- ARTIKELFIL                                   
002500                                                                          
002600     SELECT  W23131          ASSIGN UT-S-W23130D2.                        
002700                                                                          
002800*--------------------------- INVENTERINGAR                                
002900                                                                          
003000     SELECT  W23133          ASSIGN UT-S-W23130D3.                        
003100                                                                          
003200*--------------------------- BER OMS/LEVERANTÖR VCC-ARTIKLAR              
003300                                                                          
003400     SELECT  W23182          ASSIGN UT-S-W23130D4.                        
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W23131                                                               
004000     RECORDING F                                                          
004100     BLOCK 0                                                              
004200     LABEL RECORDS STANDARD.                                              
004300*01  POST -COPY W231210       -PRE W23131- -L.                            
004400     SKIP3                                                                
004500 FD  W23133                                                               
004600     RECORDING F                                                          
004700     BLOCK 0                                                              
004800     LABEL RECORDS STANDARD.                                              
004900*01  POST -COPY W231229      -PRE W23133- -L.                             
005000     SKIP3                                                                
005100 FD  W23182                                                               
005200     RECORDING F                                                          
005300     BLOCK 0                                                              
005400     LABEL RECORDS STANDARD.                                              
005500*01  POST -COPY W231227       -PRE W23182- -L.                            
005600     SKIP3                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800     SKIP3                                                                
005900*    -COPY WY2000W3                                                       
006000     SKIP3                                                                
006100 77  EOF-KALENDER            PIC X       VALUE 'N'.                       
006200     SKIP3                                                                
006300 01  RKOD                    PIC S9(4)   VALUE +0    COMP    SYNC.        
006400     SKIP3                                                                
006500 01  KONSTANTER.                                                          
006600     03  JA                  PIC X       VALUE 'J'.                       
006700     03  NEJ                 PIC X       VALUE 'N'.                       
006800     SKIP3                                                                
006900 01  WS-DAGENS-AAVV          PIC 9(4).                                    
007000 01  FILLER REDEFINES WS-DAGENS-AAVV.                                     
007100     03 WS-DAGENS-AA         PIC 9(2).                                    
007200     03 WS-DAGENS-VV         PIC 9(2).                                    
007300 01  SPAR-WS-AARP            PIC 9(4).                                    
007400 01  WS-AARP                 PIC 9(4).                                    
007500 01  FILLER REDEFINES WS-AARP.                                            
007600     03 WS-AA                PIC 9(2).                                    
007700     03 WS-RP                PIC 9(2).                                    
007800 01  WS-KVVIPER              PIC 9.                                       
007900 01  WS-FORSTA-VECKAN        PIC 9(4).                                    
008000 01  FILLER REDEFINES WS-FORSTA-VECKAN.                                   
008100     03 WS-TIAA              PIC 9(2).                                    
008200     03 WS-TIVV              PIC 9(2).                                    
008300 01  WS-SISTA-VECKAN         PIC 9(4).                                    
008400*                                                                         
008500 01  W.                                                                   
008600     03  W-TIAAR             PIC 9(2).                                    
008700     03  W-TIPER             PIC 9(2).                                    
008800     03  W-TIERSDAT-X.                                                    
008900       05    W-TIERSDAT-AA   PIC 9(2).                                    
009000       05    W-TIERSDAT-VV   PIC 9(2).                                    
009100       05    W-TIERSDAT-D    PIC 9.                                       
009200     03  W-TIERSDAT  REDEFINES W-TIERSDAT-X PIC 9(5).                     
009300     03  W-TIERSDAT-AAVV-X REDEFINES W-TIERSDAT.                          
009400         05  W-TIERSDAT-AAVV PIC 9(4).                                    
009500         05  FILLER          PIC 9.                                       
009600                                                                          
009700     03  W-TIJUSTDA-X.                                                    
009800       05    W-TIJUSTDA-AAR  PIC 9(2).                                    
009900       05    W-TIJUSTDA-VKA  PIC 9(2).                                    
010000     03  W-TIJUSTDA  REDEFINES W-TIJUSTDA-X PIC 9(4).                     
010100     03  W-TIAVROP-DISP.                                                  
010200         05  W-TIAVROP-DISP-N PIC 9(4).                                   
010300     03      W-KORDAT-X.                                                  
010400         05  FILLER          PIC X.                                       
010500         05  W-KORDAT-AAR    PIC 9(2).                                    
010600         05  W-KORDAT-VKA    PIC 9(2).                                    
010700     03  W-KORDAT REDEFINES W-KORDAT-X  PIC S9(5).                        
010800     03  W-TOTLAGER          PIC S9(9)V9(2).                              
010900     03  W-BEHOV             PIC S9(9)V9(2).                              
011000     SKIP3                                                                
011100 01  IDEX.                                                                
011200     03  IXPER               PIC S9(9)               COMP SYNC.           
011300     03  IXPER-MAX           PIC S9(9)  VALUE +13 COMP-3.                 
011400     03  IXANT               PIC S9(9)               COMP SYNC.           
011500     03  IXKDC               PIC S9(9)               COMP SYNC.           
011600     03  IXVKA               PIC S9(9)               COMP SYNC.           
011700     03  IXVKATOT            PIC S9(9)               COMP SYNC.           
011800     03  IXVKAMAX            PIC S9(9)               COMP SYNC.           
011900     03  IX                  PIC S9(9)               COMP SYNC.           
012000     SKIP3                                                                
012100*--------------------------------------- PERIODTABELL                     
012200                                                                          
012300 01  PTAB.                                                                
012400     03  PTAB-INGANG         OCCURS 13.                                   
012500       04    PTAB-MIN        PIC 9(4).                                    
012600       04    PTAB-MIN-GRP REDEFINES PTAB-MIN.                             
012700         05  PTAB-MINAAR     PIC 9(2).                                    
012800         05  PTAB-MINVKA     PIC 9(2).                                    
012900       04    PTAB-MAX        PIC 9(4).                                    
013000       04    PTAB-MAX-GRP REDEFINES PTAB-MAX.                             
013100         05  PTAB-MAXAAR     PIC 9(2).                                    
013200         05  PTAB-MAXVKA     PIC 9(2).                                    
013300       04    PTAB-VKAANT     PIC 9(2).                                    
013400     EJECT                                                                
013500                                                                          
013600*01  -COPY WWPRODSL                                                       
013700                                                                          
013800 01  DYNAMISKA-SUBPROGRAM.                                                
013900     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
014000     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
014100     03  W2313010            PIC X(8)    VALUE 'W2313010'.                
014200     03  W22222              PIC X(8)    VALUE 'W22222'.                  
014300     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
014400     EJECT                                                                
014500 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W23130'.                  
014600                                                                          
014700*01  -COPY W0005             -PRE POSTSUM-.                               
014800     EJECT                                                                
014900 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
015000*01  -COPY WDATKORT                                                       
015100     EJECT                                                                
015200*03  -COPY WDATAREA                                                       
015300     EJECT                                                                
015400*01  AREA -COPY W231210     -PRE ART-.                                    
015500     EJECT                                                                
015600*01  AREA -COPY W231227     -PRE OMS-.                                    
015700     EJECT                                                                
015800*01  AREA  -COPY W231229    -PRE INV-.                                    
015900     SKIP3                                                                
016000 01  LINK-AREA               PIC X(100).                                  
016100     EJECT                                                                
016200*01  AREA    -COPY W231L001  -PRE LINK1- -RED LINK-AREA.                  
016300     EJECT                                                                
016400*01  AREA    -COPY W231L002  -PRE LINK2- -RED LINK-AREA.                  
016500     EJECT                                                                
016600*01  AREA    -COPY W231L003  -PRE LINK3- -RED LINK-AREA.                  
016700     EJECT                                                                
016800*01  AREA    -COPY W231L004  -PRE LINK4- -RED LINK-AREA.                  
016900     EJECT                                                                
017000*01  AREA    -COPY W231L005  -PRE LINK5- -RED LINK-AREA.                  
017100     EJECT                                                                
017200*01  AREA    -COPY W231L007  -PRE LINK7- -RED LINK-AREA.                  
017300     EJECT                                                                
017400*01  AREA    -COPY W222L222   -PRE W222-.                                 
017500     EJECT                                                                
017600 LINKAGE SECTION.                                                         
017700     SKIP3                                                                
017800 01  ARTC-PCB                PIC X.                                       
017900*01  -COPY W0008  -PRE  INLB-.                                            
018000        05  FILLER           PIC X.                                       
018100 01  WDK6-PCB                PIC X.                                       
018200 01  ARTM-PCB                PIC X.                                       
018300 01  WDK7-PCB                PIC X.                                       
018400 01  INVC-PCB                PIC X.                                       
018500 01  2501-PCB                PIC X.                                       
018600 01  WDB6R-PCB               PIC X.                                       
018700 01  WDK7R-PCB               PIC X.                                       
018800 01  WDB6-PCB                PIC X.                                       
018900 01  WDD7-PCB                PIC X.                                       
019000 01  WDK7E-PCB               PIC X.                                       
019100 01  W222-UTIL-WDK6-PCB      PIC X.                                       
019200 01  W222-UTIL-WDK7-PCB      PIC X.                                       
019300 01  W222-UTIL-WDB6-PCB      PIC X.                                       
019400 01  W222-UTUP-WDK7-PCB      PIC X.                                       
019500 01  W222-UTUP-WDB6-PCB      PIC X.                                       
019600 01  W222-UTUP-UTIL-WDK6-PCB PIC X.                                       
019700 01  W222-UTUP-UTIL-WDK7-PCB PIC X.                                       
019800 01  W222-UTUP-UTIL-WDB6-PCB PIC X.                                       
019900                                                                          
020000                                                                          
020100 PROCEDURE DIVISION  USING ARTC-PCB INLB-PCB WDK6-PCB                     
020200                           ARTM-PCB                                       
020300                           WDK7-PCB                                       
020400                           INVC-PCB                                       
020500                           2501-PCB                                       
020600                           WDB6R-PCB                                      
020700                           WDK7R-PCB                                      
020800                           WDB6-PCB                                       
020900                           WDD7-PCB                                       
021000                           WDK7E-PCB                                      
021100                           W222-UTIL-WDK6-PCB                             
021200                           W222-UTIL-WDK7-PCB                             
021300                           W222-UTIL-WDB6-PCB                             
021400                           W222-UTUP-WDK7-PCB                             
021500                           W222-UTUP-WDB6-PCB                             
021600                           W222-UTUP-UTIL-WDK6-PCB                        
021700                           W222-UTUP-UTIL-WDK7-PCB                        
021800                           W222-UTUP-UTIL-WDB6-PCB                        
021900                           .                                              
022000 MAIN SECTION.                                                            
022100     ENTRY 'DLITCBL' USING ARTC-PCB INLB-PCB WDK6-PCB                     
022200                           ARTM-PCB                                       
022300                           WDK7-PCB                                       
022400                           INVC-PCB                                       
022500                           2501-PCB                                       
022600                           WDB6R-PCB                                      
022700                           WDK7R-PCB                                      
022800                           WDB6-PCB                                       
022900                           WDD7-PCB                                       
023000                           WDK7E-PCB                                      
023100                           W222-UTIL-WDK6-PCB                             
023200                           W222-UTIL-WDK7-PCB                             
023300                           W222-UTIL-WDB6-PCB                             
023400                           W222-UTUP-WDK7-PCB                             
023500                           W222-UTUP-WDB6-PCB                             
023600                           W222-UTUP-UTIL-WDK6-PCB                        
023700                           W222-UTUP-UTIL-WDK7-PCB                        
023800                           W222-UTUP-UTIL-WDB6-PCB                        
023900                           .                                              
024000     PERFORM A-INITIERA                                                   
024100     PERFORM B-LAS-ARTIKEL                                                
024200     SKIP3                                                                
024300     PERFORM UNTIL NOT(                                                   
024400        LINK1-FLJANEJ-ARTIKEL = JA)                                       
024500         IF  LINK1-PRARTSTD NOT = ZERO                                    
024600             PERFORM C-BEHANDLA-ARTIKEL                                   
024700             PERFORM E-BEHANDLA-GEMENSAM-INFO                             
024800             PERFORM D-BEHANDLA-INVENTERING                               
024900             PERFORM F-BEHANDLA-LEVERANSPLANEINFO                         
025000             PERFORM G-BEHANDLA-BEHOV                                     
025100             PERFORM H-SKAPA-UTFILER                                      
025200         END-IF                                                           
025300         PERFORM B-LAS-ARTIKEL                                            
025400     END-PERFORM                                                          
025500     SKIP3                                                                
025600     PERFORM Z-AVSLUTA                                                    
025700     MOVE ZERO  TO RETURN-CODE                                            
025800     GOBACK                                                               
025900     .                                                                    
026000     EJECT                                                                
026100 A-INITIERA SECTION.                                                      
026200                                                                          
026300     OPEN OUTPUT W23131 W23133                                            
026400                 W23182                                                   
026500                                                                          
026600     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
026700                                                                          
026800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
026900     MOVE ZERO       TO W-KORDAT                                          
027000     MOVE D-AAR              TO  W-TIERSDAT-AA                            
027100                                 WS-DAGENS-AA                             
027200     MOVE D-VECKA            TO  W-TIERSDAT-VV                            
027300                                 WS-DAGENS-VV                             
027400     MOVE D-DAGNR            TO  W-TIERSDAT-D                             
027500     PERFORM AA-BEHANDLA-KALENDER                                         
027600     .                                                                    
027700     EJECT                                                                
027800 AA-BEHANDLA-KALENDER SECTION.                                            
027900                                                                          
028000****** DAGENS-PERIOD                                                      
028100     MOVE 'AAVV  '       TO DAT-KDDATFORM                                 
028200     MOVE WS-DAGENS-AAVV TO DAT-I-TIDATUM                                 
028300     CALL WDATKONV USING DAT-KDDATFORM                                    
028400                         DAT-I-TIDATUM                                    
028500                         DAT-O-TIDATUM                                    
028600                         DAT-KDSVAR                                       
028700     MOVE DAT-TIAARP     TO WS-AARP SPAR-WS-AARP                          
028800                                                                          
028900     MOVE +1 TO IX                                                        
029000     PERFORM UNTIL IX > IXPER-MAX                                         
029100                                                                          
029200        MOVE 'AARP  '       TO DAT-KDDATFORM                              
029300        MOVE WS-AARP        TO DAT-I-TIDATUM                              
029400        CALL WDATKONV USING DAT-KDDATFORM                                 
029500                            DAT-I-TIDATUM                                 
029600                            DAT-O-TIDATUM                                 
029700                            DAT-KDSVAR                                    
029800        MOVE DAT-TIAA       TO WS-TIAA                                    
029900        MOVE DAT-TIVV       TO WS-TIVV                                    
030000        MOVE DAT-KVVIPER    TO WS-KVVIPER                                 
030100                                                                          
030200        MOVE WS-FORSTA-VECKAN TO PTAB-MIN(IX)                             
030300        MOVE WS-KVVIPER       TO PTAB-VKAANT(IX)                          
030400        MOVE WS-FORSTA-VECKAN TO WS-SISTA-VECKAN                          
030500        ADD WS-KVVIPER        TO WS-SISTA-VECKAN                          
030600        ADD -1                TO WS-SISTA-VECKAN                          
030700        MOVE WS-SISTA-VECKAN  TO PTAB-MAX(IX)                             
030800        IF WS-RP < 12                                                     
030900           ADD +1 TO WS-RP                                                
031000        ELSE                                                              
031100           ADD +1  TO WS-AA                                               
031200           MOVE +1 TO WS-RP                                               
031300        END-IF                                                            
031400                                                                          
031500        ADD +1 TO IX                                                      
031600     END-PERFORM                                                          
031700                                                                          
031800     MOVE SPAR-WS-AARP TO WS-AARP                                         
031900     .                                                                    
032000     EJECT                                                                
032100 B-LAS-ARTIKEL SECTION.                                                   
032200                                                                          
032300     MOVE +1                 TO  LINK1-KDCALL                             
032400     CALL W2313010 USING LINK1-AREA DATUMKORT                             
032500                                    ARTC-PCB INLB-PCB                     
032600     .                                                                    
032700     EJECT                                                                
032800 C-BEHANDLA-ARTIKEL SECTION.                                              
032900                                                                          
033000     MOVE LOW-VALUE          TO  ART-AREA                                 
033100     MOVE ZERO TO  ART-KVLS (1)                                           
033200                   ART-KVLS (2)                                           
033300                   ART-KVRESS (1)                                         
033400                   ART-KVRESS (2)                                         
033500                   ART-KVAKS (1)                                          
033600                   ART-KVAKS (2)                                          
033700                   ART-KVAKS-E (1)                                        
033800                   ART-KVAKS-E (2)                                        
033900                   ART-KVBR                                               
034000                   ART-KVLAAN                                             
034100                   ART-KVMP     (2)                                       
034200                   ART-KVPB-SEP (2)                                       
034300                   ART-KVPB-SATS (2)                                      
034400                   ART-REDIRLEV (2)                                       
034500     MOVE '210'              TO  ART-IDPTYP                               
034600     MOVE LINK1-IDARTNR      TO  ART-IDARTNR                              
034700     MOVE LINK1-TIFINLV      TO  ART-TIFINLV                              
034800     MOVE LINK1-FLAVRART     TO  ART-FLAVRART                             
034900     MOVE LINK1-IDANSK       TO  ART-IDANSK                               
035000     MOVE LINK1-IDLEVNR      TO  ART-IDLEVNR                              
035100     MOVE LINK1-IDFKNGRP     TO  ART-IDFKNGRP                             
035200     MOVE LINK1-KDPRODSL     TO  ART-KDPRODSL                             
035300     MOVE LINK1-IDFTG        TO  ART-IDFTG                                
035400     MOVE LINK1-IDPROD       TO  ART-IDPROD                               
035500     MOVE LINK1-KDHF         TO  ART-KDHF                                 
035600     MOVE LINK1-KDVVKL       TO  ART-KDVVKL                               
035700     MOVE LINK1-KVLAAN       TO  ART-KVLAAN                               
035800     MOVE LINK1-KVQ          TO  ART-KVQ                                  
035900     MOVE LINK1-KVOVERF      TO  ART-KVOVERF                              
036000     MOVE LINK1-KVSLUTKP     TO  ART-KVSLUTKP                             
036100     MOVE LINK1-IDLKTO       TO  ART-IDLKTO                               
036200     MOVE LINK1-PRARTSTD     TO ART-PRARTSTD                              
036300     MOVE LINK1-KVMP (1)         TO  ART-KVMP (1)                         
036400     MOVE LINK1-KVPB-SATS (1)    TO  ART-KVPB-SATS (1)                    
036500     MOVE LINK1-KVPB-SEP (1)     TO  ART-KVPB-SEP (1)                     
036600     MOVE LINK1-REDIRLEV (1)     TO  ART-REDIRLEV (1)                     
036700     MOVE NEJ                    TO  ART-FLJANEJ-C2                       
036800                                                                          
036900     IF  LINK1-FLJANEJ-C2 = JA                                            
037000         MOVE JA                 TO  ART-FLJANEJ-C2                       
037100         MOVE LINK1-KVMP (2)       TO  ART-KVMP (2)                       
037200         MOVE LINK1-KVPB-SATS (2)  TO  ART-KVPB-SATS (2)                  
037300         MOVE LINK1-KVPB-SEP (2)   TO  ART-KVPB-SEP (2)                   
037400         MOVE LINK1-REDIRLEV (2)   TO  ART-REDIRLEV (2)                   
037500     END-IF                                                               
037600     SKIP3                                                                
037700     MOVE LOW-VALUE          TO OMS-AREA                                  
037800     MOVE '227'              TO OMS-IDPTYP                                
037900     MOVE LINK1-IDARTNR      TO OMS-IDARTNR                               
038000     MOVE LINK1-IDLEVNR      TO OMS-IDLEVNR                               
038100     ADD LINK1-KVPB-SATS (1) LINK1-KVPB-SEP (1)                           
038200                             GIVING OMS-KVPB-TOT                          
038300                                                                          
038400     IF  LINK1-FLJANEJ-C2 = JA                                            
038500         COMPUTE OMS-KVPB-TOT = OMS-KVPB-TOT                              
038600                              + LINK1-KVPB-SATS (2)                       
038700                              + LINK1-KVPB-SEP (2)                        
038800     END-IF                                                               
038900                                                                          
039000     MOVE LINK1-PRARTSTD     TO OMS-PRARTSTD                              
039100     MOVE LINK1-IDPROD TO OMS-IDPROD                                      
039200     SKIP3                                                                
039300     MOVE LOW-VALUE          TO INV-AREA                                  
039400     MOVE '229'              TO INV-IDPTYP                                
039500     MOVE LINK1-IDARTNR      TO INV-IDARTNR                               
039600     MOVE +1                 TO INV-KDRORELS                              
039700     .                                                                    
039800     EJECT                                                                
039900 D-BEHANDLA-INVENTERING SECTION.                                          
040000                                                                          
040100     MOVE +6  TO LINK2-KDCALL                                             
040200     MOVE ART-IDARTNR        TO LINK2-IDARTNR                             
040300     CALL W2313010 USING LINK2-AREA DATUMKORT                             
040400                                    ARTC-PCB INLB-PCB INVC-PCB            
040500                                                                          
040600     IF LINK2-FLJANEJ-INVSEG NOT = NEJ                                    
040700       MOVE +2  TO LINK2-KDCALL                                           
040800       CALL W2313010 USING LINK2-AREA DATUMKORT                           
040900                                      ARTC-PCB INLB-PCB INVC-PCB          
041000                                                                          
041100       PERFORM UNTIL LINK2-FLJANEJ-INVSEG = NEJ                           
041200           MOVE 'AAMMDD'       TO  DAT-KDDATFORM                          
041300           MOVE LINK2-TIJUSTDA TO DAT-I-TIDATUM                           
041400           CALL WDATKONV USING DAT-KDDATFORM                              
041500                               DAT-I-TIDATUM                              
041600                               DAT-O-TIDATUM                              
041700                               DAT-KDSVAR                                 
041800           IF DAT-KDSVAR-OK                                               
041900              MOVE DAT-TIAAVV-GRP TO W-TIJUSTDA-X                         
042000           ELSE                                                           
042100              MOVE ZERO        TO W-TIJUSTDA                              
042200           END-IF                                                         
042300                                                                          
042400           MOVE W-TIJUSTDA     TO TMP1-YYWW                               
042500           MOVE PTAB-MIN (1)   TO TMP2-YYWW                               
042600           MOVE PTAB-MAX (1)   TO TMP3-YYWW                               
042700           PERFORM WY2000Q3                                               
042800           IF  TMP1-YYWW >= TMP2-YYWW                                     
042900           AND TMP1-YYWW <= TMP3-YYWW                                     
043000               MOVE +1             TO INV-KDCLAGER                        
043100               IF  LINK2-KVJUSTKV < ZERO                                  
043200                   COMPUTE INV-KVANTAL = - LINK2-KVJUSTKV                 
043300                   MOVE '2'            TO INV-KDUPPD                      
043400               ELSE                                                       
043500                   MOVE LINK2-KVJUSTKV TO INV-KVANTAL                     
043600                   MOVE '1'            TO INV-KDUPPD                      
043700               END-IF                                                     
043800               IF INV-KVANTAL NOT = ZERO                                  
043900                   WRITE W23133-POST FROM INV-AREA                        
044000                                                                          
044100                   MOVE 'W23133'   TO POSTSUM-FDNAMN                      
044200                   MOVE 'W23130D3' TO POSTSUM-DDNAMN2                     
044300                   MOVE INV-IDPTYP TO POSTSUM-TRANSTYP                    
044400                   CALL POSTSUM USING POSTSUM-PARM                        
044500               END-IF                                                     
044600           END-IF                                                         
044700           MOVE +2             TO  LINK2-KDCALL                           
044800           CALL W2313010 USING LINK2-AREA DATUMKORT                       
044900                               ARTC-PCB INLB-PCB INVC-PCB                 
045000       END-PERFORM                                                        
045100     END-IF                                                               
045200     .                                                                    
045300     EJECT                                                                
045400 E-BEHANDLA-GEMENSAM-INFO SECTION.                                        
045500                                                                          
045600     MOVE +0 TO ART-KVAKS (2)                                             
045700                ART-KVAKS-E (2)                                           
045800                ART-KVAKS-F (2)                                           
045900                ART-KVLS (2)                                              
046000                ART-KVRESS (2)                                            
046100                ART-KVROS (2)                                             
046200                ART-KVSLAGER (2)                                          
046300     SKIP2                                                                
046400     MOVE +3                 TO LINK3-KDCALL                              
046500     CALL W2313010 USING LINK3-AREA DATUMKORT                             
046600                                    ARTC-PCB INLB-PCB                     
046700                                                                          
046800     MOVE LINK3-KDUART       TO ART-KDUART                                
046900     MOVE LINK3-KDLTK        TO ART-KDLTK                                 
047000     MOVE LINK3-KDERS        TO ART-KDERS                                 
047100                                                                          
047200     MOVE +1                 TO IXKDC                                     
047300                                                                          
047400     MOVE LINK3-KVAKS (IXKDC)     TO  ART-KVAKS (IXKDC)                   
047500     MOVE LINK3-KVAKS-E (IXKDC)   TO  ART-KVAKS-E (IXKDC)                 
047600     MOVE LINK3-KVAKS-F (IXKDC)   TO  ART-KVAKS-F (IXKDC)                 
047700     MOVE LINK3-KVLS (IXKDC)      TO  ART-KVLS (IXKDC)                    
047800     MOVE LINK3-KVRESS (IXKDC)    TO  ART-KVRESS (IXKDC)                  
047900     MOVE LINK3-KVROS (IXKDC)     TO  ART-KVROS (IXKDC)                   
048000     MOVE LINK3-KVSLAGER (IXKDC)  TO  ART-KVSLAGER (IXKDC)                
048100     .                                                                    
048200     EJECT                                                                
048300 F-BEHANDLA-LEVERANSPLANEINFO SECTION.                                    
048400                                                                          
048500     MOVE ZERO TO ART-KVBR                                                
048600     MOVE ZERO TO ART-KVAVROP-EFTERSLAP                                   
048700     MOVE +1  TO IXVKA                                                    
048800     PERFORM UNTIL IXVKA > 12                                             
048900         MOVE ZERO           TO ART-KVAVROP-PLANINL (IXVKA)               
049000         ADD +1              TO IXVKA                                     
049100     END-PERFORM                                                          
049200     SKIP2                                                                
049300     MOVE +4                 TO LINK4-KDCALL                              
049400     MOVE ART-IDARTNR        TO LINK4-IDARTNR                             
049500     CALL W2313010 USING LINK4-AREA DATUMKORT                             
049600                                    ARTC-PCB INLB-PCB                     
049700                                                                          
049800     IF  LINK4-FLJANEJ-LEVPLAN = JA                                       
049900         MOVE +5             TO LINK5-KDCALL                              
050000         CALL W2313010 USING LINK5-AREA DATUMKORT                         
050100                                        ARTC-PCB INLB-PCB                 
050200         PERFORM UNTIL NOT(                                               
050300            (INLB-SEG-NAME-FB = 'WLINLB11'                                
050400         OR  INLB-SEG-NAME-FB = 'WLINLB23')                               
050500         AND LINK5-FLJANEJ-LEVINFO = JA)                                  
050600             IF INLB-SEG-NAME-FB = 'WLINLB11'                             
050700                PERFORM FA-02-SEGM                                        
050800             ELSE                                                         
050900                IF INLB-SEG-NAME-FB = 'WLINLB23'                          
051000                   PERFORM FC-05-SEGM                                     
051100                END-IF                                                    
051200             END-IF                                                       
051300             MOVE +5     TO LINK5-KDCALL                                  
051400             CALL W2313010 USING LINK5-AREA DATUMKORT                     
051500                                 ARTC-PCB INLB-PCB                        
051600         END-PERFORM                                                      
051700     END-IF                                                               
051800     .                                                                    
051900     EJECT                                                                
052000 FA-02-SEGM SECTION.                                                      
052100                                                                          
052200     ADD LINK5-KVBR          TO ART-KVBR                                  
052300     .                                                                    
052400     EJECT                                                                
052500 FC-05-SEGM SECTION.                                                      
052600                                                                          
052700     IF  LINK7-KDAVROP = 2                                                
052800         MOVE LINK7-TIAVROP-INL   TO TMP1-YYWW                            
052900         MOVE W-TIERSDAT-AAVV     TO TMP2-YYWW                            
053000         PERFORM WY2000P3                                                 
053100         IF  TMP1-YYWW <= TMP2-YYWW                                       
053200             ADD LINK7-KVAVROP   TO ART-KVAVROP-EFTERSLAP                 
053300         END-IF                                                           
053400         MOVE LINK7-TIAVROP-DISP   TO TMP1-YYWW                           
053500         MOVE W-TIERSDAT-AAVV      TO TMP2-YYWW                           
053600         PERFORM WY2000P3                                                 
053700         IF  TMP1-YYWW > TMP2-YYWW                                        
053800             MOVE +2             TO IXVKA                                 
053900             MOVE LINK7-TIAVROP-DISP TO W-TIAVROP-DISP-N                  
054000             PERFORM UNTIL NOT(                                           
054100                IXVKA NOT > 13)                                           
054200                 MOVE W-TIAVROP-DISP-N TO TMP1-YYWW                       
054300                 MOVE PTAB-MIN (IXVKA) TO TMP2-YYWW                       
054400                 MOVE PTAB-MAX (IXVKA) TO TMP3-YYWW                       
054500                 PERFORM WY2000Q3                                         
054600                 IF  TMP1-YYWW >= TMP2-YYWW                               
054700                 AND TMP1-YYWW <= TMP3-YYWW                               
054800                     SUBTRACT +1 FROM IXVKA GIVING IX                     
054900                     ADD LINK7-KVAVROP  TO                                
055000                                 ART-KVAVROP-PLANINL (IX)                 
055100                     MOVE +14        TO IXVKA                             
055200                 END-IF                                                   
055300                 ADD +1  TO IXVKA                                         
055400             END-PERFORM                                                  
055500         END-IF                                                           
055600     END-IF                                                               
055700     .                                                                    
055800     EJECT                                                                
055900 G-BEHANDLA-BEHOV SECTION.                                                
056000                                                                          
056100     MOVE +1                 TO IXVKA                                     
056200     PERFORM UNTIL IXVKA > 12                                             
056300         MOVE ZERO   TO  ART-KVBEHOV-PERIOD (1, IXVKA)                    
056400         MOVE ZERO   TO  ART-KVBEHOV-PERIOD (2, IXVKA)                    
056500         ADD +1              TO IXVKA                                     
056600     END-PERFORM                                                          
056700                                                                          
056800     IF  ART-KDERS NOT > +10                                              
056900         PERFORM GA-UNDANTAG                                              
057000         MOVE +1         TO  IXKDC                                        
057100         PERFORM UNTIL NOT(                                               
057200            IXKDC NOT > +2)                                               
057300             MOVE ART-IDARTNR    TO W222-IDARTNR                          
057400             MOVE SPACE          TO W222-IDDC                             
057500             MOVE D-AAR          TO W-KORDAT-AAR                          
057600             MOVE D-VECKA        TO W-KORDAT-VKA                          
057700             MOVE W-KORDAT       TO W222-TIAAVV-AKTUELL                   
057800             IF  WS-RP = 12                                               
057900                 ADD +1 TO W-KORDAT-AAR                                   
058000                 MOVE +1 TO W-KORDAT-VKA                                  
058100                 MOVE W-KORDAT TO W222-TIBEHOV-START                      
058200             ELSE                                                         
058300                 ADD  W-KORDAT 1 GIVING W222-TIBEHOV-START                
058400             END-IF                                                       
058500*FIX V0253                                                                
058600             IF W222-TIBEHOV-START = 0253                                 
058700               MOVE 0252 TO W222-TIBEHOV-START                            
058800             END-IF                                                       
058900             IF W222-TIBEHOV-START = 0353                                 
059000               MOVE 0352 TO W222-TIBEHOV-START                            
059100             END-IF                                                       
059200*FIX-END                                                                  
059300             MOVE +52            TO W222-KVVECKOR-BEHOV                   
059400             MOVE '17'           TO W222-KDBEHOV                          
059500             MOVE +5             TO W222-TID-AKTUELL                      
059600             MOVE NEJ            TO W222-FLINKLDIRLEV                     
059700             CALL W22222 USING  W222-AREA                                 
059800                                WDK6-PCB                                  
059900                                WDK7-PCB                                  
060000                                ARTM-PCB                                  
060100                                2501-PCB                                  
060200                                WDB6R-PCB                                 
060300                                WDK7R-PCB                                 
060400                                WDB6-PCB                                  
060500                                WDD7-PCB                                  
060600                                WDK7E-PCB                                 
060700                                W222-UTIL-WDK6-PCB                        
060800                                W222-UTIL-WDK7-PCB                        
060900                                W222-UTIL-WDB6-PCB                        
061000                                W222-UTUP-WDK7-PCB                        
061100                                W222-UTUP-WDB6-PCB                        
061200                                W222-UTUP-UTIL-WDK6-PCB                   
061300                                W222-UTUP-UTIL-WDK7-PCB                   
061400                                W222-UTUP-UTIL-WDB6-PCB                   
061500                                                                          
061600             MOVE +1             TO IXPER                                 
061700             MOVE +1             TO IXVKATOT                              
061800             MOVE +1             TO IXVKA                                 
061900             PERFORM UNTIL NOT(                                           
062000                IXPER NOT > 12                                            
062100             AND IXVKATOT NOT > IXVKAMAX)                                 
062200                 ADD W222-KVBEHOV-VECKA (IXVKATOT)                        
062300                             TO  ART-KVBEHOV-PERIOD (IXKDC, IXPER)        
062400                 ADD +1  TO IXVKA                                         
062500                 ADD +1  TO IXVKATOT                                      
062600                 ADD IXPER 1 GIVING IX                                    
062700                 IF  IXVKA > PTAB-VKAANT (IX)                             
062800                     MOVE +1     TO IXVKA                                 
062900                     ADD  +1     TO IXPER                                 
063000                 END-IF                                                   
063100             END-PERFORM                                                  
063200                 ADD +1          TO IXKDC                                 
063300         END-PERFORM                                                      
063400     END-IF                                                               
063500     .                                                                    
063600     EJECT                                                                
063700 GA-UNDANTAG SECTION.                                                     
063800                                                                          
063900     MOVE +52                TO IXVKAMAX                                  
064000                                                                          
064100     IF  ART-KDERS > ZERO                                                 
064200         COMPUTE W-TOTLAGER  = ART-KVLS (1)                               
064300                             + ART-KVRESS (1)                             
064400                             + ART-KVAKS (1)                              
064500                             + ART-KVBR                                   
064600                             + ART-KVLAAN                                 
064700          IF  ART-FLJANEJ-C2 = JA                                         
064800              COMPUTE W-TOTLAGER = W-TOTLAGER                             
064900                                 + ART-KVLS (2)                           
065000                                 + ART-KVRESS (2)                         
065100                                 + ART-KVAKS (2)                          
065200          END-IF                                                          
065300         MOVE ART-IDARTNR    TO  W222-IDARTNR                             
065400         MOVE SPACE          TO  W222-IDDC                                
065500         MOVE D-AAR          TO W-KORDAT-AAR                              
065600         MOVE D-VECKA        TO W-KORDAT-VKA                              
065700         MOVE W-KORDAT       TO W222-TIAAVV-AKTUELL                       
065800         IF  WS-RP = 12                                                   
065900             ADD +1 TO W-KORDAT-AAR                                       
066000             MOVE +1 TO W-KORDAT-VKA                                      
066100             MOVE W-KORDAT TO W222-TIBEHOV-START                          
066200         ELSE                                                             
066300             ADD  W-KORDAT 1 GIVING W222-TIBEHOV-START                    
066400         END-IF                                                           
066500*FIX V0253                                                                
066600             IF W222-TIBEHOV-START = 0253                                 
066700               MOVE 0252 TO W222-TIBEHOV-START                            
066800             END-IF                                                       
066900             IF W222-TIBEHOV-START = 0353                                 
067000               MOVE 0352 TO W222-TIBEHOV-START                            
067100             END-IF                                                       
067200*FIX-END                                                                  
067300         MOVE +52            TO W222-KVVECKOR-BEHOV                       
067400         MOVE '17'           TO W222-KDBEHOV                              
067500         MOVE +5             TO W222-TID-AKTUELL                          
067600         MOVE NEJ            TO W222-FLINKLDIRLEV                         
067700         CALL W22222 USING  W222-AREA                                     
067800                            WDK6-PCB                                      
067900                            WDK7-PCB                                      
068000                            ARTM-PCB                                      
068100                            2501-PCB                                      
068200                            WDB6R-PCB                                     
068300                            WDK7R-PCB                                     
068400                            WDB6-PCB                                      
068500                            WDD7-PCB                                      
068600                            WDK7E-PCB                                     
068700                            W222-UTIL-WDK6-PCB                            
068800                            W222-UTIL-WDK7-PCB                            
068900                            W222-UTIL-WDB6-PCB                            
069000                            W222-UTUP-WDK7-PCB                            
069100                            W222-UTUP-WDB6-PCB                            
069200                            W222-UTUP-UTIL-WDK6-PCB                       
069300                            W222-UTUP-UTIL-WDK7-PCB                       
069400                            W222-UTUP-UTIL-WDB6-PCB                       
069500                                                                          
069600         MOVE +1             TO IXVKA                                     
069700         MOVE ZERO TO W-BEHOV                                             
069800         PERFORM UNTIL NOT(                                               
069900            W-BEHOV < W-TOTLAGER                                          
070000         AND  IXVKA NOT > +52)                                            
070100             ADD  W222-KVBEHOV-VECKA (IXVKA) TO W-BEHOV                   
070200             ADD +1          TO IXVKA                                     
070300         END-PERFORM                                                      
070400                                                                          
070500         IF  IXVKA < IXVKAMAX                                             
070600             MOVE IXVKA      TO IXVKAMAX                                  
070700         END-IF                                                           
070800     END-IF                                                               
070900     .                                                                    
071000     EJECT                                                                
071100 H-SKAPA-UTFILER SECTION.                                                 
071200                                                                          
071300     WRITE W23131-POST  FROM ART-AREA                                     
071400                                                                          
071500     MOVE 'W23131'   TO POSTSUM-FDNAMN                                    
071600     MOVE 'W23130D2' TO POSTSUM-DDNAMN2                                   
071700     MOVE ART-IDPTYP TO POSTSUM-TRANSTYP                                  
071800     CALL POSTSUM USING POSTSUM-PARM                                      
071900                                                                          
072000     MOVE ART-KDPRODSL TO TEST-KDPRODSL                                   
072100     IF KDPRODSL-VOLVO-ALL                                                
072200****** VCC-ARTIKEL  ELLER VCBV-ARTIKEL                                    
072300       WRITE W23182-POST FROM OMS-AREA                                    
072400       MOVE 'W23182'   TO POSTSUM-FDNAMN                                  
072500       MOVE 'W23130D4' TO POSTSUM-DDNAMN2                                 
072600       MOVE OMS-IDPTYP TO POSTSUM-TRANSTYP                                
072700       CALL POSTSUM USING POSTSUM-PARM                                    
072800     END-IF                                                               
072900     .                                                                    
073000     EJECT                                                                
073100 Z-AVSLUTA SECTION.                                                       
073200                                                                          
073300     CLOSE W23131 W23133                                                  
073400           W23182                                                         
073500                                                                          
073600     MOVE 'S' TO POSTSUM-OPKOD                                            
073700     CALL POSTSUM USING POSTSUM-PARM                                      
073800     .                                                                    
073900     EJECT                                                                
074000*    -COPY WY2000P3                                                       
074100     EJECT                                                                
074200*    -COPY WY2000Q3                                                       
