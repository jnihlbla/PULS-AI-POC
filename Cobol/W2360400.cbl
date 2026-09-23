000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300 ID  DIVISION.                                                            
000400 PROGRAM-ID.       W2360400.                                              
000500*              PROGRAM CONVERTED BY                                       
000600*              COBOL CONVERSION AID PO 5785-ABJ                           
000700*              CONVERSION DATE 05/25/91 20:01:24.                         
000800*AUTHOR.           THOMAS FALLENIUS.                                      
000900*INSTALLATION.                                                            
001000*DATE-WRITTEN.     DEC 1979.                                              
001100*    REMARKS.                                                             
001200*    FUNKTION.                                                            
001300*        PROGRAMMET UNDERSÖKER OM LEVERANSFÖRSENING FÖRELIGGER            
001400*        MOT GÄLLANDELEVERANSPLAN VID PERIODSLUT. PROGRAMMET              
001500*        AVGÖR OM BRISTSITUATIONEN ÄR AKUT,BRISTSITUATION                 
001600*        FÖRUTSES ELLER OM INTENSIVBEVAKNING AV HÖGFREKVENTA              
001700*        ARTIKLAR SKALL GÖRAS.                                            
001800*                                                                         
001900*    INDATA.                                                              
002000*             WDK6  ARTIKELREGISTER                                       
002100*             WDD9  LEVERANSPLAN-REGISTER                                 
002200*             WDF1  LEVERANTÖRS-REGISTER                                  
002300*             WDF5  CROSS-INDEX ART/LEV                                   
002400*             WDD3  BENÄMNINGSREG                                         
002500*                                                                         
002600*    SUBPROGRAM.                                                          
002700*        W2360410 IMS-PROGRAM                                             
002800*        POSTSUM                                                          
002900*        ABEND                                                            
003000     EJECT                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200 INPUT-OUTPUT SECTION.                                                    
003300 FILE-CONTROL.                                                            
003400*                                                                         
003500     SELECT  W23601     ASSIGN UT-S-W23604D1.                             
003600*                                                                         
003700     SELECT  W23603     ASSIGN UT-S-W23604D2.                             
003800*                                                                         
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W23601                                                               
004600     BLOCK CONTAINS 0                                                     
004700     RECORDING MODE F.                                                    
004800                                                                          
004900 01  IDARTNR     PIC S9(9) COMP-3.                                        
005000     SKIP3                                                                
005100 FD  W23603                                                               
005200     BLOCK CONTAINS 0                                                     
005300     RECORDING MODE F.                                                    
005400                                                                          
005500*01  POST      -COPY W236003   -L  -PRE W23603-                           
005600                                                                          
006700     EJECT                                                                
006800 WORKING-STORAGE SECTION.                                                 
006900                                                                          
007000*    -COPY WY2000W3                                                       
007100     SKIP3                                                                
007200 77  JA                  PIC X       VALUE 'J'.                           
007300 77  NEJ                 PIC X       VALUE 'N'.                           
007400 77  AKTUELLT-AVROP      PIC X       VALUE 'N'.                           
007500 77  ABEND-CODE          PIC S9(4)   COMP SYNC  VALUE +0.                 
007600 01  MER-LEV-PLANER      PIC X       VALUE 'N'.                           
007700 01  NY-IDARTNR          PIC X       VALUE 'Y'.                           
007800 01  WS-IDARTNR          PIC S9(9)   COMP-3.                              
007900*                                                                         
008000 01  LEV-BESK-SW         PIC X       VALUE ' '.                           
008100     88  NYTT-IDARTNR                VALUE 'Y'.                           
008200     SKIP2                                                                
008300 01  INDEX-FALT.                                                          
008400                                                                          
008500     03  IX                  PIC S9(9)  COMP SYNC.                        
008600     SKIP2                                                                
008700 01  KOMMANDON-TILL-IMSPROGRAMMET.                                        
008800                                                                          
008900     03  LAS-ART-INFO        PIC S9(3)  COMP-3  VALUE +101.               
009000     03  LAS-LEV-INFO        PIC S9(3)  COMP-3  VALUE +102.               
009100     03  LAS-AVROP           PIC S9(3)  COMP-3  VALUE +103.               
009200     03  LAS-INLEVERANS      PIC S9(3)  COMP-3  VALUE +104.               
009300     03  LAS-LEV-BESK        PIC S9(3)  COMP-3  VALUE +105.               
009400     03  LAS-LEVERANTORS-REG PIC S9(3)  COMP-3  VALUE +106.               
009500     03  LAS-BEN-REG         PIC S9(3)  COMP-3  VALUE +107.               
009600     03  LAS-CROSS-INDEX     PIC S9(3)  COMP-3  VALUE +108.               
009700     03  LAS-FRAM-TILL-LEVBSK PIC S9(3)  COMP-3  VALUE +109.              
009800     SKIP2                                                                
009900 01  SUBPROGRAM.                                                          
010000                                                                          
010100     03  GENERELLA.                                                       
010200         05  ABEND       PIC X(8)    VALUE 'ABEND'.                       
010300         05  DATKORT     PIC X(8)    VALUE 'DATKORT'.                     
010400         05  POSTSUM     PIC X(8)    VALUE 'POSTSUM'.                     
010500         05  WDATKONV    PIC X(8)    VALUE 'WDATKONV'.                    
010600     03  APPLIKATIONS.                                                    
010700         05  W2360410    PIC X(8)    VALUE 'W2360410'.                    
010800     EJECT                                                                
010900 01  WS-FALT.                                                             
011000     SKIP1                                                                
011100     03  WS-IDLOPNR          PIC S9(9).                                   
011200     03  FILLER   REDEFINES WS-IDLOPNR.                                   
011300         05  INLEV-AAVV      PIC  9(4).                                   
011400         05  INLEV-TID       PIC  9(1).                                   
011410         05  FILLER          PIC  9(4).                                   
011420     03  FILLER   REDEFINES WS-IDLOPNR.                                   
011430         05  INLEV-AAVVD     PIC  9(5).                                   
011450         05  FILLER          PIC  9(4).                                   
011500                                                                          
011600     03  WS-SUMAVROP-FORTIDIGT   PIC S9(7)  VALUE ZERO  COMP-3.           
011700     03  WS-SUMAVROP-I-TID       PIC S9(7)  VALUE ZERO  COMP-3.           
011800     03  WS-SUMAVROP-FORSENT     PIC S9(7)  VALUE ZERO  COMP-3.           
011900     03  WS-SUMAVROP             PIC S9(7)  VALUE ZERO  COMP-3.           
012000                                                                          
012100     03  WS-TIAVROP-FORTIDIGT    PIC S9(5)   COMP-3.                      
012200     03  WS-TIAVROP-FORSENT      PIC S9(5)   COMP-3.                      
012300     03  WS-TIAVROP-AAVVD        PIC  9(6).                               
012320     03  FILLER REDEFINES WS-TIAVROP-AAVVD.                               
012330         05  WS-TIAVROP          PIC  9(5).                               
012400         05  FILLER   REDEFINES WS-TIAVROP.                               
012500             07  FILLER          PIC 9.                                   
012600             07  WS-TIAVROP-AA   PIC 99.                                  
012700             07  WS-TIAVROP-VV   PIC 99.                                  
012710         05  WS-TID              PIC  9.                                  
012800                                                                          
012900     03  WS-SUMKVAVROP-90           PIC S9(7)   COMP-3.                   
013000     03  WS-SUMKVAVROP-10           PIC S9(7)   COMP-3.                   
013100     03  WS-KVLAGER              PIC S9(7)   COMP-3.                      
013200                                                                          
013300     03  WS-AAVV                 PIC 9(4)   VALUE ZERO.                   
013400     03  FILLER    REDEFINES WS-AAVV.                                     
013600         05  AA                  PIC  9(2).                               
013700         05  VV                  PIC  9(2).                               
013800                                                                          
013900     03  PERIODSLUT              PIC S9(5)   COMP-3.                      
014000     03  PERIODSLUT-1VK          PIC S9(5)   COMP-3.                      
014100     03  PERIODSTART             PIC S9(5)   COMP-3.                      
014200     03  PERIODSTART-2VK         PIC S9(5)   COMP-3.                      
014300                                                                          
014400     03  WS-VARDE                PIC S9(9)V99 COMP-3.                     
014500     03  WS-TIAARP               PIC 9(4).                                
014600     03  FILLER    REDEFINES WS-TIAARP.                                   
014800         05  WS-AA               PIC 9(2).                                
014900         05  WS-RP               PIC 9(2).                                
015000                                                                          
015100 01  DATUM-KONV.                                                          
015200     05  DATUM-KONV-AA           PIC 9(02).                               
015300     05  DATUM-KONV-VV           PIC 9(02)  VALUE 53.                     
015400 01  DATUM-KONV-R  REDEFINES  DATUM-KONV PIC 9(04).                       
015500                                                                          
015600 01  VECKO-KONV-FALT.                                                     
015700     05  VECKO-ANT-INNEV-AAR     PIC 9(02).                               
015800     05  VECKO-ANT-FOREG-AAR     PIC 9(02).                               
015900     EJECT                                                                
016000*01    -COPY WDATAREA                                                     
016100                                                                          
016200     EJECT                                                                
016300*01    -COPY W236003    -PRE WS03-                                        
016400                                                                          
016500     EJECT                                                                
016600*    PARAMETRAR TILL DATKORT.                                             
016700 01  PROGRAM-NAMN        PIC X(6)    VALUE 'W23604'.                      
016800 01  DATUMKORT-ID        PIC X(6)    VALUE 'WDATUM'.                      
016900     SKIP3                                                                
017000*01    -COPY WDATKORT                                                     
017100                                                                          
017200     EJECT                                                                
017300*    PARAMETAR TILL POSTSUM.                                              
017400     SKIP3                                                                
017800*            -COPY W0005   -PRE W23603-                                   
017900                                                                          
018000     EJECT                                                                
018100*            -COPY W0005   -PRE W23601-                                   
018200                                                                          
018300     EJECT                                                                
019000*01  UTAREA  -COPY W236003   -PRE W23603-                                 
019100                                                                          
019200     EJECT                                                                
019300*  *  *   FÄLT SOM ANVÄNDS SOM  LÄNKAREOR  *  *  *                        
019400*  *  *         TILL IMS-PROGRAMMET        *  *  *                        
019500     SKIP2                                                                
019600 01  LINK-KDCALL         PIC S9(3)          COMP-3.                       
019700     SKIP2                                                                
019800*              -COPY W236L001C0    -PRE IMS1-.                            
019900                                                                          
020000     EJECT                                                                
020100*              -COPY W236L002C0    -PRE IMS2-.                            
020200                                                                          
020300     EJECT                                                                
020400*              -COPY W236L003C0    -PRE IMS3-.                            
020500                                                                          
020600     EJECT                                                                
020700*              -COPY W236L004C0    -PRE IMS4-.                            
020800                                                                          
020900     EJECT                                                                
021000*              -COPY W236L005C0    -PRE IMS5-.                            
021100                                                                          
021200     EJECT                                                                
021300 LINKAGE SECTION.                                                         
021400     SKIP1                                                                
021500*    -COPY W0008  -PRE WDF5-                                              
021600                                                                          
021700         05  FILLER      PIC X.                                           
021800     EJECT                                                                
021900*    -COPY W0008  -PRE LEVA-                                              
022000                                                                          
022100         05  FILLER      PIC X.                                           
022200     SKIP1                                                                
022300*    -COPY W0008  -PRE ARTC-                                              
022400                                                                          
022500         05  FILLER      PIC X.                                           
022600     SKIP2                                                                
022700*    -COPY W0008  -PRE INLB-                                              
022800                                                                          
022900         05  FILLER      PIC X.                                           
023000     SKIP2                                                                
023100*    -COPY W0008  -PRE INLB2-                                             
023200                                                                          
023300         05  FILLER      PIC X.                                           
023400     SKIP2                                                                
023500*    -COPY W0008  -PRE BENA-                                              
023600                                                                          
023700         05  FILLER      PIC X.                                           
023800     SKIP2                                                                
023900 PROCEDURE DIVISION USING WDF5-PCB LEVA-PCB ARTC-PCB                      
024000     INLB-PCB INLB2-PCB BENA-PCB.                                         
024100     ENTRY 'DLITCBL' USING WDF5-PCB LEVA-PCB ARTC-PCB                     
024200     INLB-PCB INLB2-PCB BENA-PCB.                                         
024300     SKIP2                                                                
024400     PERFORM A-INITIERA                                                   
024500     PERFORM S01-LAS-ART-INFO                                             
024600     PERFORM UNTIL                                                        
024700      NOT ( IMS1-ART-INFO-FINNS )                                         
024800       IF IMS1-KDHF = 0 AND IMS1-KDERS-UTG = ZERO                         
024900         MOVE NY-IDARTNR  TO  LEV-BESK-SW                                 
025000         PERFORM S02-LAS-LEV-INFO                                         
025100         PERFORM UNTIL                                                    
025200          NOT ( IMS1-LEV-INFO-FINNS AND                                   
025300                IMS1-IDLEVNR NOT = '1000 ')                               
025400           PERFORM S03-LAS-AVROP                                          
025500           MOVE IMS2-TIAVROP-INL     TO TMP1-YYWW                         
025600           MOVE PERIODSLUT           TO TMP2-YYWW                         
025700           PERFORM WY2000P3                                               
025800           PERFORM UNTIL                                                  
025900            NOT ( TMP1-YYWW NOT > TMP2-YYWW  AND                          
026000              (IMS2-KDAVROP = 2 OR IMS2-KDAVROP = 9) AND                  
026100              IMS2-AVROP-FINNS )                                          
026200             PERFORM C-BEHANDLA-INLEVERANS                                
026300             MOVE IMS2-TIAVROP-INL   TO TMP1-YYWW                         
026400             MOVE PERIODSTART-2VK    TO TMP2-YYWW                         
026500             MOVE PERIODSLUT-1VK     TO TMP3-YYWW                         
026600             PERFORM WY2000Q3                                             
026700             IF  TMP1-YYWW     >= TMP2-YYWW                               
026800             AND TMP1-YYWW     <= TMP3-YYWW                               
026900               PERFORM D-KONTROLL-LEV-AVIKELSE                            
027000             END-IF                                                       
027400             PERFORM S13-NOLLSTALL-AVROP                                  
027500             PERFORM S03-LAS-AVROP                                        
027600             MOVE IMS2-TIAVROP-INL   TO TMP1-YYWW                         
027700             MOVE PERIODSLUT         TO TMP2-YYWW                         
027800             PERFORM WY2000P3                                             
027900           END-PERFORM                                                    
028100           PERFORM G-SKRIV-W23603-POST                                    
028200           PERFORM S02-LAS-LEV-INFO                                       
028300         END-PERFORM                                                      
028400       END-IF                                                             
028500       PERFORM S01-LAS-ART-INFO                                           
028600     END-PERFORM                                                          
028700     PERFORM H-AVSLUTA                                                    
028800     MOVE 0 TO RETURN-CODE                                                
028900     GOBACK                                                               
029000     .                                                                    
029100     EJECT                                                                
029200 A-INITIERA SECTION.                                                      
029300     SKIP1                                                                
029400     OPEN OUTPUT W23603                                                   
029500           INPUT W23601                                                   
029600     MOVE 'W23602'   TO W23603-PROGNAMN                                   
029700     MOVE 'W23602D1' TO W23601-DDNAMN2                                    
029900     MOVE 'W23602D3' TO W23603-DDNAMN2                                    
030000     MOVE 'W23601' TO W23601-FDNAMN                                       
030200     MOVE 'W23603' TO W23603-FDNAMN                                       
030300                                                                          
030600     PERFORM S12-NOLLSTALL-W23603-UTAREA                                  
030700     PERFORM S13-NOLLSTALL-AVROP                                          
030800     PERFORM S14-NOLLSTALL-WS03-W23603                                    
030900                                                                          
031000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
031100     MOVE D-AAR       TO AA WS-AA                                         
031200     MOVE D-VECKA     TO VV                                               
031210                                                                          
031220     MOVE WS-AAVV TO PERIODSLUT                                           
031230     COMPUTE PERIODSLUT-1VK ROUNDED = WS-AAVV - 1                         
031240                                                                          
031300     MOVE WS-AAVV     TO DAT-I-TIDATUM                                    
031400     MOVE 'AAVV  '    TO DAT-KDDATFORM                                    
031600     CALL WDATKONV USING DAT-KDDATFORM                                    
031700                         DAT-I-TIDATUM                                    
031800                         DAT-O-TIDATUM                                    
031900                         DAT-KDSVAR                                       
031910     IF DAT-KDSVAR-OK                                                     
032000        MOVE DAT-TIRP    TO WS-RP                                         
032010     ELSE                                                                 
032011        DISPLAY ' FEL I DATKONV  TIRP AAVV ' WS-AAVV                      
032012        MOVE 20          TO ABEND-CODE                                    
032013        CALL ABEND USING ABEND-CODE                                       
032020     END-IF                                                               
032400                                                                          
032500*HUR MÅNGA VECKOR HAR INNEVARANDE ÅR?                                     
032600     MOVE D-AAR      TO DATUM-KONV-AA                                     
032700     MOVE DATUM-KONV-R TO DAT-I-TIDATUM                                   
032800     MOVE 'AAVV  '   TO DAT-KDDATFORM                                     
032900                                                                          
033000     CALL WDATKONV USING DAT-KDDATFORM                                    
033100                         DAT-I-TIDATUM                                    
033200                         DAT-O-TIDATUM                                    
033300                         DAT-KDSVAR                                       
033400                                                                          
033500     IF DAT-KDSVAR-OK                                                     
033600       MOVE 53         TO VECKO-ANT-INNEV-AAR                             
033700     ELSE                                                                 
033800       MOVE 52         TO VECKO-ANT-INNEV-AAR                             
033900     END-IF                                                               
034000     IF D-AAR = 00                                                        
034100        MOVE 99     TO D-AAR                                              
034200     ELSE                                                                 
034300       SUBTRACT 1 FROM D-AAR                                              
034400     END-IF                                                               
034500     MOVE D-AAR         TO DATUM-KONV-AA                                  
034600     ADD 1              TO D-AAR                                          
034700     MOVE DATUM-KONV-R  TO DAT-I-TIDATUM                                  
034800     MOVE 'AAVV  '      TO DAT-KDDATFORM                                  
034900                                                                          
035000     CALL WDATKONV USING DAT-KDDATFORM                                    
035100                         DAT-I-TIDATUM                                    
035200                         DAT-O-TIDATUM                                    
035300                         DAT-KDSVAR                                       
035400                                                                          
035500     IF DAT-KDSVAR-OK                                                     
035600       MOVE 53         TO VECKO-ANT-FOREG-AAR                             
035700     ELSE                                                                 
035800       MOVE 52         TO VECKO-ANT-FOREG-AAR                             
035900     END-IF                                                               
035910                                                                          
035920     MOVE WS-TIAARP   TO DAT-I-TIDATUM                                    
035930     MOVE 'AARP  '    TO DAT-KDDATFORM                                    
035940                                                                          
035950     CALL WDATKONV USING DAT-KDDATFORM                                    
035960                         DAT-I-TIDATUM                                    
035970                         DAT-O-TIDATUM                                    
035980                         DAT-KDSVAR                                       
035981     IF DAT-KDSVAR-OK                                                     
035990        MOVE DAT-TIVV TO VV                                               
035991     ELSE                                                                 
035992        DISPLAY ' FEL I DATKONV  PERIODSTART AARP ' WS-TIAARP             
035993        MOVE 20          TO ABEND-CODE                                    
035994        CALL ABEND USING ABEND-CODE                                       
035995     END-IF                                                               
035996                                                                          
036000     MOVE WS-AAVV     TO PERIODSTART                                      
036100     IF VV = 1                                                            
036200        SUBTRACT 1 FROM AA                                                
036300        MOVE VECKO-ANT-FOREG-AAR TO VV                                    
036400        SUBTRACT 1 FROM VV                                                
036410     ELSE                                                                 
036420        SUBTRACT 2 FROM VV                                                
036500     END-IF                                                               
036600     MOVE WS-AAVV     TO PERIODSTART-2VK                                  
038700     .                                                                    
038800     EJECT                                                                
038900 C-BEHANDLA-INLEVERANS SECTION.                                           
039000     SKIP2                                                                
039100     MOVE IMS2-TIAVROP-INL TO WS-TIAVROP                                  
039110     MOVE IMS2-TID         TO WS-TID                                      
039120                                                                          
039130     IF WS-TIAVROP-VV = 01                                                
039140       IF VECKO-ANT-FOREG-AAR = 52                                        
039150         MOVE 51       TO WS-TIAVROP-VV                                   
039160       ELSE                                                               
039170         MOVE 52       TO WS-TIAVROP-VV                                   
039180       END-IF                                                             
039190       IF WS-TIAVROP-AA = 00                                              
039200          MOVE 99      TO WS-TIAVROP-AA                                   
039300       ELSE                                                               
039400          SUBTRACT 1 FROM WS-TIAVROP-AA                                   
039500       END-IF                                                             
039600     ELSE                                                                 
039700       SUBTRACT 2 FROM WS-TIAVROP-VV                                      
039800     END-IF                                                               
039900     MOVE WS-TIAVROP     TO WS-TIAVROP-FORTIDIGT                          
040000                                                                          
040100     MOVE IMS2-TIAVROP-INL TO WS-TIAVROP                                  
040200     ADD 1                 TO WS-TIAVROP-VV                               
040300     IF WS-TIAVROP-VV = 53                                                
040400       IF WS-TIAVROP-AA = D-AAR                                           
040500         IF VECKO-ANT-INNEV-AAR = 52                                      
040600           ADD 1        TO WS-TIAVROP-AA                                  
040700           MOVE 01      TO WS-TIAVROP-VV                                  
040800         ELSE                                                             
040900           CONTINUE                                                       
041000         END-IF                                                           
041100       ELSE                                                               
041200         IF VECKO-ANT-FOREG-AAR = 52                                      
041300           ADD 1        TO WS-TIAVROP-AA                                  
041400           MOVE 01      TO WS-TIAVROP-VV                                  
041500         ELSE                                                             
041600           CONTINUE                                                       
041700         END-IF                                                           
041800       END-IF                                                             
041900     ELSE                                                                 
042000       IF WS-TIAVROP-VV = 54                                              
042100         ADD 1           TO WS-TIAVROP-AA                                 
042200         MOVE 01         TO WS-TIAVROP-VV                                 
042300       ELSE                                                               
042400         CONTINUE                                                         
042500       END-IF                                                             
042600     END-IF                                                               
042700     MOVE WS-TIAVROP       TO WS-TIAVROP-FORSENT                          
043600                                                                          
043700     MOVE ZERO    TO IMS2-IDLOPNRM-PL                                     
043800                                                                          
043900     PERFORM S04-LAS-INLEVERANS                                           
044000     PERFORM UNTIL                                                        
044100      NOT ( IMS2-INLEVERANS-FINNS )                                       
044200       MOVE IMS2-IDLOPNRM-PL TO WS-IDLOPNR                                
044300                                                                          
044400       MOVE INLEV-AAVV             TO TMP1-YYWW                           
044500       MOVE WS-TIAVROP-FORTIDIGT   TO TMP2-YYWW                           
044600       PERFORM WY2000P3                                                   
044700       IF TMP1-YYWW < TMP2-YYWW                                           
044800         ADD IMS2-KVAVROP-AVB TO WS-SUMAVROP-FORTIDIGT                    
044900       ELSE                                                               
045000         MOVE INLEV-AAVV           TO TMP1-YYWW                           
045100         MOVE WS-TIAVROP-FORSENT   TO TMP2-YYWW                           
045200         PERFORM WY2000P3                                                 
045300         IF TMP1-YYWW <= TMP2-YYWW                                        
045400           ADD IMS2-KVAVROP-AVB TO WS-SUMAVROP-I-TID                      
045500         ELSE                                                             
045600           ADD IMS2-KVAVROP-AVB TO WS-SUMAVROP-FORSENT                    
045700         END-IF                                                           
045800       END-IF                                                             
045900       PERFORM S04-LAS-INLEVERANS                                         
046000     END-PERFORM                                                          
046100     MOVE IMS2-TIAVROP-INL   TO TMP1-YYWW                                 
046200     MOVE PERIODSLUT-1VK     TO TMP2-YYWW                                 
046300     PERFORM WY2000P3                                                     
046400     IF TMP1-YYWW <= TMP2-YYWW                                            
046500       ADD IMS2-KVAVROP TO WS-SUMAVROP-FORSENT                            
046600     ELSE                                                                 
046700       ADD IMS2-KVAVROP TO WS-SUMAVROP-I-TID                              
046800     END-IF                                                               
046900     COMPUTE WS-SUMAVROP ROUNDED =  WS-SUMAVROP-FORSENT +                 
047000                                   WS-SUMAVROP-FORTIDIGT +                
047100                                   WS-SUMAVROP-I-TID                      
047200     .                                                                    
047300     EJECT                                                                
047400 D-KONTROLL-LEV-AVIKELSE SECTION.                                         
047500     SKIP2                                                                
048200     MOVE IMS1-KDPRODSL TO W23603-KDPRODSL WS03-KDPRODSL                  
048500     MOVE IMS1-IDLEVNR  TO W23603-IDLEVNR  WS03-IDLEVNR                   
048600     MOVE WS-TIAARP     TO W23603-TIAARP   WS03-TIAARP                    
048700                                                                          
048800     MULTIPLY 0.9 BY WS-SUMAVROP GIVING WS-SUMKVAVROP-90                  
048900     MULTIPLY 0.1 BY WS-SUMAVROP GIVING WS-SUMKVAVROP-10                  
049000                                                                          
049100     IF WS-SUMAVROP-FORSENT NOT < WS-SUMKVAVROP-10 AND                    
049200        WS-SUMAVROP-FORSENT > 0                                           
049300       MOVE +1 TO W23603-TYP                                              
049400       MOVE +1 TO W23603-ANTAL                                            
049500       MULTIPLY WS-SUMAVROP-FORSENT BY                                    
049600                IMS1-PRARTSTD GIVING W23603-VARDE                         
049700       MOVE IMS1-IDARTNR     TO W23603-IDARTNR                            
049800       MOVE IMS2-TIAVROP-INL TO W23603-TIAVROP-INL                        
049900       MOVE IMS2-IDLOPNRM-PL TO W23603-IDLOPNRM-PL                        
050000       PERFORM S09-SKRIV-W23603-UTAREA                                    
050100       PERFORM S12-NOLLSTALL-W23603-UTAREA                                
050200     ELSE                                                                 
050300       IF WS-SUMAVROP-FORTIDIGT >= WS-SUMKVAVROP-90 AND                   
050400       WS-SUMAVROP-FORTIDIGT > 0                                          
050500         MOVE +2 TO W23603-TYP                                            
050600         MOVE +1 TO W23603-ANTAL                                          
050700         MULTIPLY WS-SUMAVROP-FORTIDIGT BY                                
050800                  IMS1-PRARTSTD GIVING W23603-VARDE                       
050900         MOVE IMS1-IDARTNR     TO W23603-IDARTNR                          
051000         MOVE IMS2-TIAVROP-INL TO W23603-TIAVROP-INL                      
051100         MOVE IMS2-IDLOPNRM-PL TO W23603-IDLOPNRM-PL                      
051200         PERFORM S09-SKRIV-W23603-UTAREA                                  
051300         PERFORM S12-NOLLSTALL-W23603-UTAREA                              
051400       END-IF                                                             
051500     END-IF                                                               
051600     IF WS-SUMAVROP > 0                                                   
051700       ADD +1 TO WS03-ANTAL                                               
051800       MULTIPLY WS-SUMAVROP BY                                            
051900                IMS1-PRARTSTD GIVING WS-VARDE                             
052000       ADD WS-VARDE TO WS03-VARDE                                         
052100     END-IF                                                               
052200     .                                                                    
052300     EJECT                                                                
064400 G-SKRIV-W23603-POST SECTION.                                             
064500     SKIP2                                                                
064600     IF WS03-ANTAL > 0                                                    
064700       MOVE WS03-KDPRODSL TO W23603-KDPRODSL                              
064800       MOVE WS03-IDLEVNR TO W23603-IDLEVNR                                
064900       MOVE WS03-TIAARP  TO W23603-TIAARP                                 
065000       MOVE +3           TO W23603-TYP                                    
065100       MOVE WS03-ANTAL   TO W23603-ANTAL                                  
065200       MOVE WS03-VARDE   TO W23603-VARDE                                  
065300       MOVE ZERO         TO W23603-IDARTNR                                
065400                            W23603-TIAVROP-INL                            
065500                            W23603-IDLOPNRM-PL                            
065600                                                                          
065700       PERFORM S09-SKRIV-W23603-UTAREA                                    
065800       PERFORM S12-NOLLSTALL-W23603-UTAREA                                
065900       PERFORM S14-NOLLSTALL-WS03-W23603                                  
066000     END-IF                                                               
066100     .                                                                    
066200     EJECT                                                                
066300 H-AVSLUTA SECTION.                                                       
066400     SKIP2                                                                
066500     CLOSE W23601 W23603                                                  
066600     SKIP1                                                                
066700     MOVE 'S' TO  W23603-OPKOD                                            
066800     CALL POSTSUM USING W23603-PARM                                       
066900     .                                                                    
067000     SKIP3                                                                
067100     SKIP3                                                                
067200 S01-LAS-ART-INFO SECTION.                                                
067300     SKIP2                                                                
067400     MOVE SPACE TO IMS1-KDSVAR                                            
067500     READ W23601 INTO IMS1-IDARTNR AT END MOVE NEJ TO                     
067600     IMS1-KDSVAR                                                          
067700     END-READ                                                             
067800     IF IMS1-ART-INFO-FINNS                                               
067900       MOVE 'W23601'  TO  W23601-TRANSTYP                                 
068000       CALL POSTSUM USING W23601-PARM                                     
068100     END-IF                                                               
068200     IF IMS1-ART-INFO-FINNS                                               
068300       MOVE LAS-ART-INFO TO LINK-KDCALL                                   
068400       CALL W2360410 USING LINK-KDCALL IMS1-W236L001                      
068500       WDF5-PCB LEVA-PCB ARTC-PCB INLB-PCB INLB2-PCB BENA-PCB             
068600     END-IF                                                               
068700     .                                                                    
068800     SKIP3                                                                
068900 S02-LAS-LEV-INFO SECTION.                                                
069000     SKIP1                                                                
069100     MOVE LAS-LEV-INFO TO LINK-KDCALL                                     
069200     CALL W2360410 USING LINK-KDCALL IMS1-W236L001                        
069300     WDF5-PCB LEVA-PCB ARTC-PCB INLB-PCB INLB2-PCB BENA-PCB               
069400     .                                                                    
069500     SKIP3                                                                
069600 S03-LAS-AVROP SECTION.                                                   
069700     SKIP1                                                                
069800     MOVE LAS-AVROP TO LINK-KDCALL                                        
069900     CALL W2360410 USING LINK-KDCALL IMS2-W236L002                        
070000     WDF5-PCB LEVA-PCB ARTC-PCB INLB-PCB INLB2-PCB BENA-PCB               
070100     .                                                                    
070200     SKIP3                                                                
070300 S04-LAS-INLEVERANS SECTION.                                              
070400     SKIP1                                                                
070500     MOVE LAS-INLEVERANS TO LINK-KDCALL                                   
070600     CALL W2360410 USING LINK-KDCALL IMS2-W236L002                        
070700     WDF5-PCB LEVA-PCB ARTC-PCB INLB-PCB INLB2-PCB BENA-PCB               
070800     .                                                                    
070900     EJECT                                                                
073800 S09-SKRIV-W23603-UTAREA SECTION.                                         
073900     SKIP2                                                                
074000     WRITE W23603-POST FROM W23603-UTAREA                                 
074100     SKIP1                                                                
074200     MOVE 'W23603' TO W23603-TRANSTYP                                     
074300     CALL POSTSUM USING W23603-PARM                                       
074400     .                                                                    
074500     EJECT                                                                
077400 S12-NOLLSTALL-W23603-UTAREA SECTION.                                     
077500     SKIP2                                                                
077600     MOVE ZERO TO W23603-KDPRODSL                                         
077800                  W23603-TIAARP                                           
077900                  W23603-TYP                                              
077910     MOVE SPACE   TO W23603-IDLEVNR                                       
078000     .                                                                    
078100                                                                          
078200     EJECT                                                                
078300 S13-NOLLSTALL-AVROP SECTION.                                             
078400     SKIP1                                                                
078500     MOVE ZERO TO WS-SUMAVROP-FORTIDIGT                                   
078600                  WS-SUMAVROP-FORSENT                                     
078700                  WS-SUMAVROP-I-TID                                       
078800                  WS-SUMAVROP                                             
078900                  WS-SUMKVAVROP-90                                        
079000                  WS-SUMKVAVROP-10                                        
079100                  WS-KVLAGER                                              
079200     .                                                                    
079300     SKIP3                                                                
079400 S14-NOLLSTALL-WS03-W23603 SECTION.                                       
079500     SKIP1                                                                
079600     MOVE ZERO TO WS03-KDPRODSL                                           
079800                  WS03-TIAARP                                             
079900                  WS03-TYP                                                
080000                  WS03-ANTAL                                              
080100                  WS03-VARDE                                              
080200     MOVE SPACE   TO WS03-IDLEVNR                                         
081100     .                                                                    
081200     EJECT                                                                
082000*    -COPY WY2000Q3                                                       
082100     EJECT                                                                
082200*    -COPY WY2000P3                                                       
