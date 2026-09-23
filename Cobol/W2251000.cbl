000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.                 W2251000.                                    
000500*              PROGRAM CONVERTED BY                                       
000600*              COBOL CONVERSION AID PO 5785-ABJ                           
000700*              CONVERSION DATE 05/25/91 19:45:43.                         
000800*AUTHOR.                     IDK, GÖTEBORG.                               
000900*DATE-WRITTEN.               SEP 1978.                                    
001000*    SKIP2                                                                
001100*REMARKS.                                                                 
001200*    FUNKTION.                                                            
001300*                                                                         
001400                                                                          
001500*    MED W22511 SOM INPUT HÄMTAS KOMPLETTERANDE INFO FRÅN                 
001600*    ARTIKELREGISTER (WDK6),BENÄMNINGSREGISTRET (WDD3)                    
001700*    OCH ORDER-ENTRYREGISTRET (WDK9)                                      
001800*    SAMT DÄREFTER FRÅN LEVERANSPLAN (WDD9).                              
001900                                                                          
002000*    FINNS INTE ARTIKEL FRÅN W22511 PÅ LEVERANSPLAN (WDD9),               
002100*    NOLLSTÄLLS/SPACE UTAREORNA.                                          
002200                                                                          
002300***  LEVERANSBESKED MED PASSERAD MOTTAGNINGSVECKA NOLLSTÄLL               
002400*    W22513-FILEN     *********.                                          
002500                                                                          
002600*    ALLA GODKÄNDA RECORD SKRIVES PÅ W22513, 15, 19 OCH 24                
002700*    FILERNA.                                                             
002800                                                                          
002900                                                                          
003000                                                                          
003100*    SUBPROGRAM.                                                          
003200                                                                          
003300*    W2251010    HANDHAR SAMTLIGA IMS-ANROP.                              
003400*    POSTSUM     BERÄKNAR OCH REDOVISAR ANTAL LÄSTA                       
003500*                OCH SKRIVNA POSTER.                                      
003600*                                                                         
003700*    SDC:                                                                 
003800*    FILCOPUTEXTER HAR EJ FÖRÄNDRATS. C2-FÄLT OCH UTGÅNGNA                
003900*    DATAELEMENT NOLLFYLLS.                                               
004000*    11/11-94 SK                                                          
004100*                                                                         
004200     EJECT                                                                
004300 ENVIRONMENT DIVISION.                                                    
004400 INPUT-OUTPUT SECTION.                                                    
004500 FILE-CONTROL.                                                            
004600     SKIP2                                                                
004700*--------------------------------------- INFILER                          
004800     SKIP1                                                                
004900*--------------------------------------- RESTORDER OCH LEVERANS-          
005000*                                        INFO                             
005100     SELECT W22511           ASSIGN UT-S-W22510D1.                        
005200     SKIP3                                                                
005300*--------------------------------------- UTFILER                          
005400     SKIP1                                                                
005500*-------------------------------------------------------                  
005600*                                        SERVICEGRAD PER                  
005700*                                        ANSKAFFARE                       
005800     SELECT W22513           ASSIGN UT-S-W22510D2.                        
005900     SKIP1                                                                
006000                                                                          
006100*-----------------------------------------------                          
006200*                                        RANKING                          
006300     SKIP1                                                                
006400     SELECT W22515           ASSIGN UT-S-W22510D3.                        
006500     SKIP1                                                                
006600                                                                          
006700                                                                          
006800*-------------------------------------------------------------            
006900*                                        ÅTERSTÅENDE ORDERRADER           
007000     SKIP1                                                                
007100     SELECT W22519           ASSIGN UT-S-W22510D5.                        
007200     SKIP1                                                                
007300                                                                          
007400*--------------------------------------------------------------           
007500*                                        FELLISTA                         
007600     SKIP1                                                                
007700     SELECT W22523           ASSIGN UT-S-W22510D7.                        
007800     SKIP1                                                                
007900*-------------------------------------------------                        
008000*                                       RESTORDER,RADER                   
008100     SELECT W22524           ASSIGN UT-S-W22510D6.                        
008200     SKIP1                                                                
008300     EJECT                                                                
008400 DATA DIVISION.                                                           
008500 FILE SECTION.                                                            
008600     SKIP2                                                                
008700 FD  W22511                                                               
008800     RECORDING F                                                          
008900     BLOCK 0.                                                             
009000*01  -COPY W225P234   -L                                                  
009100     SKIP2                                                                
009200                                                                          
009300                                                                          
009400 FD  W22513                                                               
009500     RECORDING F                                                          
009600     BLOCK 0.                                                             
009700*01  POST -COPY W225LI01   -PRE U13- -L                                   
009800     SKIP2                                                                
009900                                                                          
010000                                                                          
010100 FD  W22515                                                               
010200     RECORDING F                                                          
010300     BLOCK 0.                                                             
010400*01  POST -COPY W225LI02   -PRE U15- -L                                   
010500     EJECT                                                                
010600                                                                          
010700                                                                          
010800 FD  W22519                                                               
010900     RECORDING F                                                          
011000     BLOCK 0.                                                             
011100*01  POST -COPY W225LI04   -PRE U19- -L                                   
011200     SKIP2                                                                
011300                                                                          
011400                                                                          
011500 FD  W22523                                                               
011600     RECORDING F                                                          
011700     BLOCK 0.                                                             
011800*01  POST -COPY W225LI05   -PRE U23- -L                                   
011900     SKIP2                                                                
012000     EJECT                                                                
012100                                                                          
012200                                                                          
012300 FD  W22524                                                               
012400     RECORDING F                                                          
012500     BLOCK 0.                                                             
012600*01  POST -COPY W22524 -PRE U24- -L                                       
012700     EJECT                                                                
012800                                                                          
012900                                                                          
013000 WORKING-STORAGE SECTION.                                                 
013100*    -COPY WY2000W3                                                       
013200     SKIP3                                                                
013300 77  W22510-EOF-SW           PIC X       VALUE 'N'.                       
013400     88  END-OF-W22511                   VALUE 'J'.                       
013500 01  IX                      PIC S9(9)                COMP SYNC.          
013600 01  KONSTANTER.                                                          
013700     03  JA                  PIC X       VALUE 'J'.                       
013800     03  NEJ                 PIC X       VALUE 'N'.                       
013900 01  DYNAMISKA-SUBPROGRAM.                                                
014000     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
014100     03  W2251010            PIC X(8)    VALUE 'W2251010'.                
014200     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
014300 01  W-TILEVBESK.                                                         
014400     03  W-TILEVBESK-AAR     PIC 9(3).                                    
014500     03  W-TILEVBESK-VECKA   PIC 9(2).                                    
014600 01  W-TILEVBESK-N REDEFINES W-TILEVBESK PIC S9(5).                       
014700     EJECT                                                                
014800 01  W-TEST-DATUM                        PIC S9(5).                       
014900*                                                                         
015000*    -COPY WWPRODSL                                                       
015100*                                                                         
015200*--------------------------------------- PARAMETRAR TILL DATKORT          
015300     SKIP1                                                                
015400 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22510'.                  
015500 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
015600*    -COPY WDATKORTC0                                                     
015700     EJECT                                                                
015800*--------------------------------------- PARAMETRAR TILL POSTSUM          
015900     SKIP1                                                                
016000*01  -COPY W0005      -PRE POSTSUM-                                       
016100     EJECT                                                                
016200*------------------------------------------------------------             
016300*                                        AREA FÖR W22511-POST             
016400     SKIP1                                                                
016500*01  AREA -COPY W225P234   -PRE I11-                                      
016600     EJECT                                                                
016700*------------------------------------------------------------             
016800*                                        AREA FÖR W22513-POST             
016900     SKIP1                                                                
017000*01  AREA -COPY W225LI01   -PRE U13-                                      
017100     EJECT                                                                
017200*------------------------------------------------------------             
017300*                                        AREA FÖR W22515-POST             
017400     SKIP1                                                                
017500*01  AREA -COPY W225LI02   -PRE U15-                                      
017600     EJECT                                                                
017700*------------------------------------------------------------             
017800*                                        AREA FÖR W22519-POST             
017900     SKIP1                                                                
018000*01  AREA -COPY W225LI04   -PRE U19-                                      
018100     EJECT                                                                
018200*------------------------------------------------------------             
018300*                                        AREA FÖR W22523-POST             
018400*01  AREA -COPY W225LI05   -PRE U23-                                      
018500     EJECT                                                                
018600*------------------------------------------------------------             
018700*                                        AREA FÖR W22524-POST             
018800*01  AREA -COPY W22524     -PRE U24-                                      
018900     EJECT                                                                
019000*--------------------------------------- LÄNK-AREOR TILL SUB-             
019100*                                        PROGRAMMET W2251010              
019200*01  AREA -COPY W225L001   -PRE IMSART-                                   
019300     EJECT                                                                
019400*01  AREA -COPY W225L002   -PRE IMSLEV-                                   
019500     EJECT                                                                
019600 LINKAGE SECTION.                                                         
019700* PCB FÖR SUBPROGRAM W2251010                                             
019800                                                                          
019900 01  ARTC-PCB               PIC X.                                        
020000 01  BENA-PCB               PIC X.                                        
020100 01  ARTM-PCB               PIC X.                                        
020200 01  INLB-PCB               PIC X.                                        
020300     EJECT                                                                
020400 PROCEDURE DIVISION  USING ARTC-PCB BENA-PCB ARTM-PCB INLB-PCB.           
020500     ENTRY 'DLITCBL' USING ARTC-PCB BENA-PCB ARTM-PCB INLB-PCB.           
020600     SKIP2                                                                
020700     PERFORM A-INITIERA                                                   
020800     PERFORM S01-LAS-W22511                                               
020900     IF NOT END-OF-W22511                                                 
021000        MOVE I11-IDARTNR        TO IMSART-IDARTNR                         
021100        MOVE +1                 TO IMSART-KDCALL                          
021200        CALL W2251010 USING IMSART-W225L001 ARTC-PCB                      
021300                                            BENA-PCB                      
021400                                            ARTM-PCB                      
021500                                            INLB-PCB                      
021600        PERFORM UNTIL END-OF-W22511                                       
021700          MOVE IMSART-KDPRODSL   TO TEST-KDPRODSL                         
021800          IF IMSART-ANROP-OK      AND KDPRODSL-VOLVO-BIMA                 
021900             PERFORM B-SKAPA-W22513                                       
022000             PERFORM C-SKAPA-W22515                                       
022100             PERFORM E-SKAPA-W22519                                       
022200             PERFORM F-SKAPA-W22523                                       
022300             PERFORM G-SKAPA-W22524                                       
022400          END-IF                                                          
022500          PERFORM S01-LAS-W22511                                          
022600          MOVE +1             TO IMSART-KDCALL                            
022700          MOVE I11-IDARTNR    TO IMSART-IDARTNR                           
022800          CALL W2251010       USING IMSART-W225L001 ARTC-PCB              
022900                                                    BENA-PCB              
023000                                                    ARTM-PCB              
023100                                                    INLB-PCB              
023200        END-PERFORM                                                       
023300     END-IF                                                               
023400     PERFORM Z-AVSLUTA                                                    
023500     MOVE ZERO               TO RETURN-CODE                               
023600     GOBACK                                                               
023700     .                                                                    
023800     EJECT                                                                
023900                                                                          
024000                                                                          
024100 A-INITIERA SECTION.                                                      
024200     SKIP2                                                                
024300     OPEN INPUT W22511                                                    
024400     OUTPUT W22513 W22515 W22519 W22523 W22524                            
024500     MOVE PROGRAM-NAMN       TO POSTSUM-PROGNAMN                          
024600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
024700     MOVE D-AAR              TO W-TILEVBESK-AAR                           
024800     MOVE D-VECKA            TO W-TILEVBESK-VECKA                         
024900     .                                                                    
025000     EJECT                                                                
025100                                                                          
025200                                                                          
025300 B-SKAPA-W22513 SECTION.                                                  
025400     SKIP2                                                                
025500     PERFORM BA-NOLLSTALL-W22513-LEVINFO                                  
025600     MOVE W-TILEVBESK-N      TO IMSLEV-TILEVBSK                           
025700     MOVE I11-IDARTNR        TO IMSLEV-IDARTNR                            
025800     MOVE IMSART-IDLEVNR     TO IMSLEV-IDLEVNR                            
025900     MOVE +2                 TO IMSLEV-KDCALL                             
026000     CALL W2251010           USING IMSLEV-W225L002 ARTC-PCB               
026100                                                   BENA-PCB               
026200                                                   ARTM-PCB               
026300                                                   INLB-PCB               
026400     IF  IMSLEV-FLJANEJ-IDLEVNR = NEJ                                     
026500       PERFORM BA-NOLLSTALL-W22513-LEVINFO                                
026600       MOVE ZERO               TO  U13-KVBR                               
026700     ELSE                                                                 
026800       MOVE IMSLEV-KVBR                TO U13-KVBR                        
026900       IF IMSLEV-TILEVBSK-INLC2 > ZERO                                    
027000         MOVE IMSLEV-TILEVBSK-INLC1   TO TMP1-YYWW                        
027100         MOVE IMSLEV-TILEVBSK-INLC2   TO TMP2-YYWW                        
027200         PERFORM WY2000P3                                                 
027300         IF TMP1-YYWW > TMP2-YYWW                                         
027400           MOVE IMSLEV-TILEVBSK-INLC2 TO W-TEST-DATUM                     
027500         ELSE                                                             
027600           MOVE IMSLEV-TILEVBSK-INLC1 TO W-TEST-DATUM                     
027700         END-IF                                                           
027800         MOVE W-TILEVBESK-N   TO TMP1-YYWW                                
027900         MOVE W-TEST-DATUM    TO TMP2-YYWW                                
028000         PERFORM WY2000P3                                                 
028100         IF TMP1-YYWW > TMP2-YYWW                                         
028200           PERFORM BA-NOLLSTALL-W22513-LEVINFO                            
028300         ELSE                                                             
028400           PERFORM BB-FLYTTA-TILL-W22513-LEVINFO                          
028500         END-IF                                                           
028600       ELSE                                                               
028700         MOVE IMSLEV-TILEVBSK-INLC1 TO W-TEST-DATUM                       
028800         MOVE W-TILEVBESK-N   TO TMP1-YYWW                                
028900         MOVE W-TEST-DATUM    TO TMP2-YYWW                                
029000         PERFORM WY2000P3                                                 
029100         IF TMP1-YYWW > TMP2-YYWW                                         
029200           PERFORM BA-NOLLSTALL-W22513-LEVINFO                            
029300         ELSE                                                             
029400           PERFORM BB-FLYTTA-TILL-W22513-LEVINFO                          
029500         END-IF                                                           
029600       END-IF                                                             
029700     END-IF                                                               
029800     PERFORM BC-FLYTTA-TILL-W22513-ARTINFO                                
029900     PERFORM S02-SKRIV-W22513                                             
030000     .                                                                    
030100     EJECT                                                                
030200 BA-NOLLSTALL-W22513-LEVINFO SECTION.                                     
030300                                                                          
030400     INITIALIZE U13-W225LI01                                              
030500     .                                                                    
030600     EJECT                                                                
030700                                                                          
030800                                                                          
030900 BB-FLYTTA-TILL-W22513-LEVINFO SECTION.                                   
031000     SKIP2                                                                
031100     MOVE IMSLEV-KVAVIS-LEVBESK(1)   TO U13-KVAVIS-LEVBESK-1              
031200     MOVE IMSLEV-KVAVIS-LEVBESK(2)   TO U13-KVAVIS-LEVBESK-2              
031300     MOVE IMSLEV-KVAVIS-LEVBESK(3)   TO U13-KVAVIS-LEVBESK-3              
031400     MOVE IMSLEV-TIAVIDAT-LEVBESK(1) TO U13-TIAVIDAT-LEVBESK-1            
031500     MOVE IMSLEV-TIAVIDAT-LEVBESK(2) TO U13-TIAVIDAT-LEVBESK-2            
031600     MOVE IMSLEV-TIAVIDAT-LEVBESK(3) TO U13-TIAVIDAT-LEVBESK-3            
031700     .                                                                    
031800     EJECT                                                                
031900                                                                          
032000                                                                          
032100 BC-FLYTTA-TILL-W22513-ARTINFO SECTION.                                   
032200     SKIP2                                                                
032300     MOVE 1                          TO U13-IDLISTA                       
032400     MOVE IMSART-IDANSK              TO U13-IDANSK                        
032500     MOVE IMSART-IDLEVNR             TO U13-IDLEVNR                       
032600     MOVE I11-IDARTNR                TO U13-IDARTNR                       
032700     MOVE IMSART-FLTOPP              TO U13-FLTOPP-CDC                    
032800     MOVE I11-KVROS-CDC-1-2          TO U13-KVROS-CDC-1-2                 
032900     MOVE I11-KVROS-CDC-3-4          TO U13-KVROS-CDC-3-4                 
033000     COMPUTE U13-KVDISPL-CDC =                                            
033100                                (IMSART-KVLS -                            
033200                                (IMSART-KVRESS +                          
033300                                 IMSART-KVOKS-BULK +                      
033400                                 IMSART-KVOKS-DAG +                       
033500                                 IMSART-KVOKS-VOR))                       
033600                                                                          
033700     COMPUTE U13-KVAKS-TILLG-CDC =                                        
033800             IMSART-KVAKS-CDC +                                           
033900             IMSART-KVAKS-PAV +                                           
034000             IMSART-KVAKS-T                                               
034100     MOVE IMSART-KVUTRS              TO U13-KVUTRS-CDC                    
034200     MOVE IMSART-KVSPANT             TO U13-KVSPANT-CDC                   
034300     MOVE IMSART-KVSLAGER            TO U13-KVSLAGER-CDC                  
034400     MOVE IMSART-TIAVIDAT-SEN        TO U13-TIAVIDAT-CDC-SEN              
034500     MOVE IMSART-KVAVIS              TO U13-KVAVIS-CDC                    
034600     ADD IMSART-KVPB-SEP    IMSART-KVPB-SATS                              
034700     GIVING U13-KVPB-TOT-CDC                                              
034800     MOVE IMSART-TIPBDAT             TO U13-TIPBDAT-CDC                   
034900     MOVE IMSART-KDERS               TO U13-KDERS-CDC                     
035000     MOVE IMSART-TIINVDAT            TO U13-TIINVDAT-CDC                  
035100     MOVE IMSART-TIRODAT             TO U13-TIRODAT-CDC                   
035200     MOVE I11-TIRODAT-ORDER-CDC      TO U13-TIRODAT-ORDER-CDC             
035300     COMPUTE U13-SUROBEL-CDC =                                            
035400     (I11-KVROS-CDC-1-2 + I11-KVROS-CDC-3-4) * IMSART-PRARTSTD            
035500     MOVE I11-KVRORAD-CDC-TOT       TO U13-KVRORAD-CDC-TOT                
035600     MOVE I11-KVRORAD-CDC-1-2 (2)   TO U13-KVRORAD-CDC-1-2                
035700     MOVE I11-KVRORAD-CDC-3-4 (2)   TO U13-KVRORAD-CDC-3-4                
035800     MOVE I11-KVRORAD-CDC-1-2-VECKA TO U13-KVRORAD-CDC-1-2-VECKA          
035900     MOVE I11-KVRORAD-CDC-3-4-VECKA TO U13-KVRORAD-CDC-3-4-VECKA          
036000     MOVE I11-KVAVBRAD-CDC-1-2 (2)  TO U13-KVAVBRAD-CDC-1-2               
036100     MOVE I11-KVAVBRAD-CDC-3-4 (2)  TO U13-KVAVBRAD-CDC-3-4               
036200     MOVE I11-KVFYSAVV-CDC-1-2 (2)  TO U13-KVFYSAVV-CDC-1-2               
036300     MOVE I11-KVFYSAVV-CDC-3-4 (2)  TO U13-KVFYSAVV-CDC-3-4               
036400     MOVE I11-KVINORD-CDC-1-2 (2)   TO U13-KVINORD-CDC-1-2                
036500     MOVE I11-KVINORD-CDC-3-4 (2)   TO U13-KVINORD-CDC-3-4                
036600     MOVE I11-KVEJRO-CDC-1-2        TO U13-KVEJRO-CDC-1-2                 
036700     MOVE I11-KVEJRO-CDC-3-4        TO U13-KVEJRO-CDC-3-4                 
036800     MOVE IMSART-KDHF            TO U13-KDHF                              
036900     MOVE IMSART-KDPROD          TO U13-KDPROD                            
037000     MOVE IMSART-KDGK            TO U13-KDGK                              
037100     MOVE IMSART-KDPRODSL        TO U13-KDPRODSL                          
037200     MOVE IMSART-TIFINLV         TO U13-TIFINLV                           
037300     MOVE IMSART-KDCLPOST        TO U13-KDCLPOST                          
037400     MOVE IMSART-BEART-SVE       TO U13-BEART-SVE                         
037500     MOVE IMSART-IDLKTO          TO U13-IDLKTO                            
037600     MOVE IMSART-KDVVKL          TO U13-KDVVKL                            
037700     MOVE IMSART-IDFKNGRP        TO U13-IDFKNGRP                          
037800     MOVE IMSART-KDLTK           TO U13-KDLTK                             
037900     MOVE IMSART-KDUART          TO U13-KDUART                            
038000                                                                          
038100     IF  IMSART-KDLEVSP = 20                                              
038200       MOVE 2              TO U13-KDSPARR-CDC                             
038300     ELSE                                                                 
038400       IF  IMSART-KDLEVSP = 21                                            
038500         MOVE 2              TO U13-KDSPARR-CDC                           
038600       ELSE                                                               
038700         IF  IMSART-KDLEVSP = 22                                          
038800           MOVE ZERO           TO U13-KDSPARR-CDC                         
038900         ELSE                                                             
039000           IF  IMSART-FLLSRDEL = NEJ                                      
039100             MOVE 3              TO U13-KDSPARR-CDC                       
039200           ELSE                                                           
039300             IF  U13-KDUART = 'S'                                         
039400               MOVE 4              TO U13-KDSPARR-CDC                     
039500             ELSE                                                         
039600               MOVE ZERO           TO U13-KDSPARR-CDC                     
039700             END-IF                                                       
039800           END-IF                                                         
039900         END-IF                                                           
040000       END-IF                                                             
040100     END-IF                                                               
040200     MOVE IMSART-PRARTSTD    TO U13-PRARTSTD                              
040300     .                                                                    
040400     EJECT                                                                
040500                                                                          
040600                                                                          
040700 C-SKAPA-W22515 SECTION.                                                  
040800     SKIP2                                                                
040900     IF  (U13-KVDISPL-CDC                                                 
041000     - (I11-KVROS-CDC-1-2 + I11-KVROS-CDC-3-4)                            
041100     - (IMSART-KVSLAGER  / 2)) NOT > 0                                    
041200       IF  I11-KDCLPOST = +0                                              
041300       OR  I11-KDCLPOST = 1                                               
041400         PERFORM CA-FLYTTA-TILL-W22515-INFO                               
041500         IF  U15-SUROBEL NOT = ZERO                                       
041600         OR  U15-KVRORAD NOT = ZERO                                       
041700         OR  U15-TIRODAT-ORDER NOT = ZERO                                 
041800           PERFORM S03-SKRIV-W22515                                       
041900         END-IF                                                           
042000       END-IF                                                             
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400                                                                          
042500                                                                          
042600 CA-FLYTTA-TILL-W22515-INFO SECTION.                                      
042700     SKIP2                                                                
042800     MOVE I11-IDARTNR            TO U15-IDARTNR                           
042900     MOVE  1                     TO U15-KDCLAGER                          
043000     COMPUTE U15-SUROBEL = (I11-KVROS-CDC-1-2 + I11-KVROS-CDC-3-4)        
043100                          * IMSART-PRARTSTD                               
043200     MOVE I11-KVRORAD-CDC-TOT TO U15-KVRORAD                              
043300     MOVE I11-TIRODAT-ORDER-CDC  TO U15-TIRODAT-ORDER                     
043400     MOVE IMSART-IDLEVNR         TO U15-IDLEVNR                           
043500     MOVE IMSART-FLTOPP          TO U15-FLTOPP                            
043600     MOVE IMSART-IDANSK          TO U15-IDANSK                            
043700     ADD I11-KVROS-CDC-1-2 I11-KVROS-CDC-3-4 GIVING U15-KVROS             
043800     COMPUTE U15-SUAKBEL =                                                
043900                 (IMSART-KVAKS-CDC +                                      
044000                  IMSART-KVAKS-PAV +                                      
044100                   IMSART-KVAKS-T)                                        
044200                  * IMSART-PRARTSTD                                       
044300                                                                          
044400     MOVE IMSART-KVAKS-CDC       TO U15-KVAKS-CDC                         
044500     MOVE IMSART-KVAKS-PAV       TO U15-KVAKS-PAV                         
044600     MOVE IMSART-KVAKS-T         TO U15-KVAKS-T                           
044700     .                                                                    
044800     EJECT                                                                
044900                                                                          
045000                                                                          
045100 E-SKAPA-W22519 SECTION.                                                  
045200     SKIP2                                                                
045300     PERFORM EA-FLYTTA-TILL-W22519-ARTINFO                                
045400     PERFORM S05-SKRIV-W22519                                             
045500     .                                                                    
045600     EJECT                                                                
045700                                                                          
045800                                                                          
045900 EA-FLYTTA-TILL-W22519-ARTINFO SECTION.                                   
046000     SKIP2                                                                
046100                                                                          
046200     INITIALIZE U19-W225LI04                                              
046300                                                                          
046400     MOVE I11-IDARTNR        TO  U19-IDARTNR                              
046500     MOVE IMSART-IDLKTO      TO  U19-IDLKTO                               
046600     MOVE IMSART-IDPROJ      TO  U19-IDPROJ                               
046700     MOVE I11-KDCLPOST       TO  U19-KDCLPOST                             
046800     COMPUTE U19-SUROBEL-CDC = (I11-KVROS-CDC-1-2                         
046900                             + I11-KVROS-CDC-3-4)                         
047000                             * IMSART-PRARTSTD                            
047100                                                                          
047200     MOVE +1                 TO IX                                        
047300     PERFORM UNTIL IX > 3                                                 
047400       MOVE I11-KVRORAD-CDC-1-2 (IX) TO U19-KVRORAD-CDC-1-2 (IX)          
047500       MOVE I11-KVRORAD-CDC-3-4 (IX) TO U19-KVRORAD-CDC-3-4 (IX)          
047600       ADD I11-KVAVBRAD-CDC-1-2 (IX) I11-KVAVBRAD-CDC-3-4  (IX)           
047700                                GIVING U19-KVAVBRAD-CDC  (IX)             
047800       ADD I11-KVFYSAVV-CDC-1-2 (IX) I11-KVFYSAVV-CDC-3-4 (IX)            
047900                                GIVING U19-KVFYSAVV-CDC  (IX)             
048000       ADD I11-KVINORD-CDC-1-2  (IX) I11-KVINORD-CDC-3-4 (IX)             
048100                                GIVING U19-KVINORD-CDC   (IX)             
048200       ADD +1                   TO IX                                     
048300     END-PERFORM                                                          
048400     COMPUTE U19-SUROBEL-CDC = (I11-KVROS-CDC-1-2                         
048500                             + I11-KVROS-CDC-3-4)                         
048600                             * IMSART-PRARTSTD                            
048700                                                                          
048800     .                                                                    
048900     EJECT                                                                
049000                                                                          
049100                                                                          
049200 F-SKAPA-W22523 SECTION.                                                  
049300     SKIP2                                                                
049400     INITIALIZE U23-W225LI05                                              
049500                                                                          
049600     PERFORM FA-FLYTTA-TILL-W22523-CDC                                    
049700     IF  U23-KVRORAD-KVAR-P NOT = ZERO                                    
049800     OR  U23-KVINORD-KVAR-P NOT = ZERO                                    
049900       PERFORM S06-SKRIV-W22523                                           
050000     END-IF                                                               
050100     SKIP2                                                                
050200     .                                                                    
050300     EJECT                                                                
050400                                                                          
050500                                                                          
050600 FA-FLYTTA-TILL-W22523-CDC SECTION.                                       
050700     SKIP2                                                                
050800     MOVE +5                 TO U23-IDLISTA                               
050900     MOVE I11-IDARTNR        TO  U23-IDARTNR                              
051000     MOVE +1                 TO  U23-KDCLAGER                             
051100     MOVE IMSART-IDANSK     TO  U23-IDANSK                                
051200     MOVE I11-KVRORAD-KVAR-V1-CDC TO U23-KVRORAD-KVAR-V1                  
051300     MOVE I11-KVRORAD-KVAR-IV-CDC TO U23-KVRORAD-KVAR-IV                  
051400     MOVE I11-KVRORAD-KVAR-P-CDC TO U23-KVRORAD-KVAR-P                    
051500     ADD I11-KVINORD-CDC-1-2 (1) I11-KVINORD-CDC-3-4 (1) GIVING           
051600     U23-KVINORD-KVAR-V1                                                  
051700     ADD I11-KVINORD-CDC-1-2 (2) I11-KVINORD-CDC-3-4 (2) GIVING           
051800     U23-KVINORD-KVAR-IV                                                  
051900     ADD I11-KVINORD-CDC-1-2 (3) I11-KVINORD-CDC-3-4 (3) GIVING           
052000     U23-KVINORD-KVAR-P                                                   
052100     .                                                                    
052200     EJECT                                                                
052300                                                                          
052400                                                                          
052500 G-SKAPA-W22524 SECTION.                                                  
052600                                                                          
052700     IF (U13-KVDISPL-CDC - (I11-KVROS-CDC-1-2 + I11-KVROS-CDC-3-4)        
052800        - (IMSART-KVSLAGER / 2)) NOT > 0                                  
052900                                                                          
053000        PERFORM GA-FLYTTA-TILL-W22524                                     
053100        IF U24-SUROBEL       NOT = ZERO OR                                
053200           U24-KVRORAD       NOT = ZERO OR                                
053300           U24-TIRODAT-ORDER NOT = ZERO                                   
053400              PERFORM S07-SKRIV-W22524                                    
053500        END-IF                                                            
053600     END-IF                                                               
053700     .                                                                    
053800     EJECT                                                                
053900                                                                          
054000                                                                          
054100 GA-FLYTTA-TILL-W22524 SECTION.                                           
054200                                                                          
054300     MOVE I11-IDARTNR       TO U24-IDARTNR                                
054400     MOVE IMSART-IDLEVNR    TO U24-IDLEVNR                                
054500     MOVE IMSART-IDANSK     TO U24-IDANSK                                 
054600                                                                          
054700     COMPUTE U24-SUROBEL =                                                
054800     ((I11-KVROS-CDC-1-2 + I11-KVROS-CDC-3-4 +                            
054900       I11-KVROS-SDC-1-2 + I11-KVROS-SDC-3-4) * IMSART-PRARTSTD)          
055000                                                                          
055100     COMPUTE U24-KVRORAD =                                                
055200     I11-KVRORAD-CDC-TOT + I11-KVRORAD-SDC-TOT                            
055300                                                                          
055400     COMPUTE U24-KVROS =                                                  
055500     I11-KVROS-CDC-1-2 + I11-KVROS-CDC-3-4 +                              
055600     I11-KVROS-SDC-1-2 + I11-KVROS-SDC-3-4                                
055700                                                                          
055800     MOVE I11-TIRODAT-ORDER-CDC   TO U24-TIRODAT-ORDER                    
055900                                                                          
056000     COMPUTE U24-SUAKBEL =                                                
056100             (IMSART-KVAKS-CDC +                                          
056200              IMSART-KVAKS-PAV +                                          
056300              IMSART-KVAKS-T)                                             
056400             * IMSART-PRARTSTD                                            
056500                                                                          
056600     MOVE IMSART-KVAKS-CDC      TO U24-KVAKS-CDC                          
056700     MOVE IMSART-KVAKS-PAV      TO U24-KVAKS-PAV                          
056800     MOVE IMSART-KVAKS-T        TO U24-KVAKS-T                            
056900     MOVE U13-KVAVIS-LEVBESK-1     TO U24-KVAVIS-LEVBESK-1                
057000     MOVE U13-TIAVIDAT-LEVBESK-1   TO U24-TIAVIDAT-LEVBESK-1              
057100     MOVE U13-KVAVIS-LEVBESK-2     TO U24-KVAVIS-LEVBESK-2                
057200     MOVE U13-TIAVIDAT-LEVBESK-2   TO U24-TIAVIDAT-LEVBESK-2              
057300     MOVE U13-KVAVIS-LEVBESK-3     TO U24-KVAVIS-LEVBESK-3                
057400     MOVE U13-TIAVIDAT-LEVBESK-3   TO U24-TIAVIDAT-LEVBESK-3              
057500     .                                                                    
057600     EJECT                                                                
057700                                                                          
057800                                                                          
057900 Z-AVSLUTA SECTION.                                                       
058000     SKIP2                                                                
058100     CLOSE W22511 W22513 W22515 W22519 W22523 W22524                      
058200     MOVE 'S'                TO POSTSUM-OPKOD                             
058300     CALL POSTSUM USING POSTSUM-PARM                                      
058400     .                                                                    
058500     EJECT                                                                
058600                                                                          
058700                                                                          
058800 S01-LAS-W22511 SECTION.                                                  
058900     SKIP2                                                                
059000     READ W22511 INTO I11-AREA                                            
059100     AT END                                                               
059200        SET END-OF-W22511 TO TRUE                                         
059300     NOT AT END                                                           
059400        MOVE 'W22511'       TO POSTSUM-FDNAMN                             
059500        MOVE 'W22510D1'     TO POSTSUM-DDNAMN2                            
059600        MOVE I11-IDPTYP     TO POSTSUM-TRANSTYP                           
059700        CALL POSTSUM USING POSTSUM-PARM                                   
059800     END-READ                                                             
059900     .                                                                    
060000     EJECT                                                                
060100                                                                          
060200                                                                          
060300 S02-SKRIV-W22513 SECTION.                                                
060400     WRITE U13-POST FROM U13-AREA                                         
060500     MOVE 'W22513'           TO POSTSUM-FDNAMN                            
060600     MOVE 'W22510D2'         TO POSTSUM-DDNAMN2                           
060700     MOVE SPACE              TO POSTSUM-TRANSTYP                          
060800     CALL POSTSUM            USING POSTSUM-PARM                           
060900     .                                                                    
061000     EJECT                                                                
061100                                                                          
061200                                                                          
061300 S03-SKRIV-W22515 SECTION.                                                
061400     WRITE U15-POST FROM U15-AREA                                         
061500     MOVE 'W22515'           TO POSTSUM-FDNAMN                            
061600     MOVE 'W22510D3'         TO POSTSUM-DDNAMN2                           
061700     MOVE SPACE              TO POSTSUM-TRANSTYP                          
061800     CALL POSTSUM            USING POSTSUM-PARM                           
061900     .                                                                    
062000     EJECT                                                                
062100                                                                          
062200                                                                          
062300 S05-SKRIV-W22519 SECTION.                                                
062400     WRITE U19-POST FROM U19-AREA                                         
062500     MOVE 'W22519'           TO POSTSUM-FDNAMN                            
062600     MOVE 'W22510D5'         TO POSTSUM-DDNAMN2                           
062700     MOVE SPACE              TO POSTSUM-TRANSTYP                          
062800     CALL POSTSUM            USING POSTSUM-PARM                           
062900     .                                                                    
063000     EJECT                                                                
063100                                                                          
063200                                                                          
063300 S06-SKRIV-W22523 SECTION.                                                
063400     WRITE U23-POST FROM U23-AREA                                         
063500     MOVE 'W22523'           TO POSTSUM-FDNAMN                            
063600     MOVE 'W22510D7'         TO POSTSUM-DDNAMN2                           
063700     MOVE SPACE              TO POSTSUM-TRANSTYP                          
063800     CALL POSTSUM            USING POSTSUM-PARM                           
063900     .                                                                    
064000     EJECT                                                                
064100                                                                          
064200                                                                          
064300 S07-SKRIV-W22524 SECTION.                                                
064400     WRITE U24-POST FROM U24-AREA                                         
064500     MOVE 'W22524'           TO POSTSUM-FDNAMN                            
064600     MOVE 'W22510D6'         TO POSTSUM-DDNAMN2                           
064700     MOVE SPACE              TO POSTSUM-TRANSTYP                          
064800     CALL POSTSUM            USING POSTSUM-PARM                           
064900     .                                                                    
065000     EJECT                                                                
065100*    -COPY WY2000P3                                                       
