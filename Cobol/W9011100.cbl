000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W9011100.                                                
000500*              PROGRAM CONVERTED BY                                       
000600*              COBOL CONVERSION AID PO 5785-ABJ                           
000700*              CONVERSION DATE 05/25/91 17:31:48.                         
000800*AUTHOR.         MARGARETA GABRIELSSON.                                   
000900*DATE-COMPILED.                                                           
001000*DATE-WRITTEN.   JANUARI 1984.                                            
001100*REMARKS.                                                                 
001200*    FUNKTION.   FRÅGEPROGRAM FÖR TULLRESTITUTION.                        
001300*                UPPDATERING AV TULLRESTITUTIONSKOD.                      
001400                                                                          
001500                                                                          
001600     SKIP2                                                                
001700*    INDATA.                                                              
001800*        TRANSAKTION: W9T111 ELLER 'W9T111U'                              
001900*        MID:         W9I11101                                            
002000*    UTDATA.                                                              
002100*        MOD:         W9O11101                                            
002200*    SUBPROGRAM.                                                          
002300*        FELLOG                                                           
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
002910                                                                          
003000*    -- CHECKED BY WY2000                                                 
003500 77  IDARTNR-WS                  PIC X(9).                                
003600 77  MAX-MOD-LENGD               PIC S9(4)   VALUE +237 COMP SYNC.        
003700     SKIP2                                                                
003800 01  FILLER           PIC X(16)  VALUE 'KONSTANTER'.                      
003900 77  JA                      PIC X       VALUE 'J'.                       
004000 77  NEJ                     PIC X       VALUE 'N'.                       
004100 77  INGET-IFYLLT            PIC X       VALUE 'I'.                       
004200 77  FEL                     PIC X       VALUE 'F'.                       
004300 77  RETT                    PIC X       VALUE 'R'.                       
004400 77  ARTIKEL-RETT            PIC X.                                       
004500 77  ARTIKEL-FINNS           PIC X.                                       
004600     SKIP2                                                                
004700 01  FALT-AREA.                                                           
004800     03  FALT-KDTULLRE           PIC X       VALUE 'I'.                   
004900     EJECT                                                                
004910 01  DYNAMISKA-SUBPROGRAM.                                                
004920     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004921     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004922     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
004930                                                                          
004940     EJECT                                                                
004941*                    ****  PARAMETRAR TILL W005INIT                       
004950*01  -COPY WMSGINIT                                                       
004960                                                                          
005000 01  NYCKLAR-TILL-DLI.                                                    
005100     03  W-IDARTNR-X.                                                     
005200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.          
005300         SKIP2                                                            
005310     03  W-KDSEGKEY-X.                                                    
005320         05  W-KDSEGKEY          PIC  X(1)   VALUE '1'.                   
005600     03  W-IDSKYLT-X.                                                     
005700         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
005800     SKIP3                                                                
005900 01  RETT-MEDDELANDE.                                                     
006000     03  RETT-1                  PIC X(30)                                
006100         VALUE 'UPPLYST FÄLT HAR NU UPDATERATS'.                          
006200     SKIP2                                                                
006300 01  FELMEDDELANDE.                                                       
006400     03  FEL-1                   PIC X(26)                                
006500         VALUE 'ARTIKELNUMRET EJ NUMERISKT'.                              
006600     SKIP1                                                                
006700     03  FEL-2                   PIC X(15)                                
006800         VALUE 'ARTIKELN SAKNAS'.                                         
006900     SKIP1                                                                
007000     03  FEL-3                   PIC X(20)                                
007100         VALUE 'ARTIKELN ÄR UTGÅNGEN'.                                    
007200     SKIP1                                                                
007300     03  FEL-4                   PIC X(17)                                
007400         VALUE 'UPPLYSTA FÄLT FEL'.                                       
007500     SKIP1                                                                
007600     03  FEL-5                   PIC X(38)                                
007700         VALUE 'DU HAR FÖRSÖKT UPPDATERA FÖRE KONTROLL'.                  
007800     SKIP1                                                                
007900     03  FEL-6                   PIC X(33)                                
008000         VALUE 'UPPDATERINGSFÄLTET ÄR INTE IFYLLT'.                       
008100     EJECT                                                                
008200*                        ****    TP-AREOR                                 
008300 01  FILLER                      PIC X(16)   VALUE ' MFS-WS   '.          
008400     SKIP2                                                                
008500*01  MID -COPY W9I11101 -PRE MID-.                                        
008700     EJECT                                                                
008800*01  -COPY WMSGAREA                                                       
009000     EJECT                                                                
009200*    03  MOD -COPY W9O11101 -RED MSG-AREA.                                
009300     EJECT                                                                
009400*01  -COPY WMFSAREA.                                                      
009600     EJECT                                                                
009700******************************************************************        
009800*****                                                                     
009900*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010000*****                                                                     
010100 01  IMS-WS.                                                              
010200     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
010300     SKIP3                                                                
010400*****                    **** STATUS-KOD FRÅN IMS                         
010500     03  STATUS-WS               PIC X(2).                                
010600         88  SEGMENT-FINNS                   VALUE '  '.                  
010700         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
010800     SKIP3                                                                
010900     03  GODK-STATUSKODER.                                                
011000         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
011100     SKIP3                                                                
011200 01  SSA1                        PIC X(64).                               
011300 01  SSA2                        PIC X(64).                               
011400     EJECT                                                                
011500*                            IMS FUNKTIONSKODER                           
011600*01  -COPY W0003                                                          
011800     EJECT                                                                
011900*                            DLI INPUT-OUTPUT AREA                        
012000 01  DLI-IO-AREA.                                                         
012100     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
012200     SKIP3                                                                
013100*-------- WDK6-ARTIKELREG                                                 
013200     SKIP2                                                                
013300*    03  WLARTC01  -COPY WDK601             -RED IO-AREA.                 
013500     EJECT                                                                
013600*    03  WLARTC11  -COPY WDK611            -RED IO-AREA.                  
013800     EJECT                                                                
014800*------- WDD3-BENREG                                                      
014900     SKIP1                                                                
015300*    03  WLBENA11  -COPY WDD311 -PRE BEN-  -RED IO-AREA.                  
015500     EJECT                                                                
015600 LINKAGE SECTION.                                                         
015700     SKIP2                                                                
015800*01  -COPY W0009     -PRE MSG-                                            
016000     EJECT                                                                
016100*01  -COPY W0008     -PRE USEA-                                           
016200         05  FILLER              PIC X.                                   
016400     EJECT                                                                
016500*01  -COPY W0008     -PRE ARTC-.                                          
016700         05  FILLER              PIC X.                                   
016800     EJECT                                                                
016900*01  -COPY W0008     -PRE BEN-.                                           
017100         05  FILLER              PIC X.                                   
017200     EJECT                                                                
017300 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
017310                                  ARTC-PCB BEN-PCB.                       
017400     SKIP1                                                                
017500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
017510                                   ARTC-PCB BEN-PCB.                      
017600     SKIP2                                                                
017700     PERFORM IMS-GET-MSG                                                  
017800     IF SEGMENT-FINNS                                                     
017900       PERFORM A-KOLLA-NYCKLAR                                            
018000       IF ARTIKEL-RETT = JA                                               
018100         IF MFS-UPDATE                                                    
018200           IF MID-IDARTNR-IN = SPACE OR ALL '+'                           
018300             PERFORM D-KONTROLLERA-INDATA                                 
018400             IF FALT-KDTULLRE = RETT                                      
018500               PERFORM E-UPPDATERA-TULLREST                               
018600             END-IF                                                       
018700           ELSE                                                           
018800             MOVE FEL-5 TO MOD-MESSAGE-RAD1                               
018900             MOVE MFS-RENSA-FAELT TO MOD-KDTULLRE-NY                      
019000*                                                                         
019100             PERFORM B-FINNS-ARTIKEL                                      
019200             IF ARTIKEL-FINNS = JA                                        
019300               PERFORM C-INFORMATIONSBILD                                 
019400             END-IF                                                       
019500           END-IF                                                         
019600         ELSE                                                             
019700           PERFORM B-FINNS-ARTIKEL                                        
019800           IF ARTIKEL-FINNS = JA                                          
019900             PERFORM C-INFORMATIONSBILD                                   
020000           END-IF                                                         
020100         END-IF                                                           
020200       END-IF                                                             
020300       MOVE MAX-MOD-LENGD TO MSG-KVLL                                     
020400       PERFORM IMS-INSERT-MSG                                             
020500     END-IF                                                               
020600     MOVE ZERO TO RETURN-CODE                                             
020700     GOBACK                                                               
020800     .                                                                    
020900     EJECT                                                                
021000****************************************************************          
021100*    NYCKEL = ARTIKELNR                                                   
021200*                                                                         
021300 A-KOLLA-NYCKLAR   SECTION.                                               
021400     SKIP2                                                                
021500     IF MSG-DUBBLA-TRANSKODER                                             
021600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W9I11101                 
021700       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
021800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
021900       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
022000     ELSE                                                                 
022100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I11101                  
022200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
022300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022400       MOVE ' ' TO MFS-KDTRTYP                                            
022500     END-IF                                                               
022600     SKIP2                                                                
022610     MOVE ALL '+' TO MSGI-WMSGINIT                                        
022620     MOVE '001'             TO MSGI-KDCALL                                
022630     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
022631     MOVE '9111'            TO MSGI-IDTRANS                               
022632     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
022640     IF MFS-IDTRANS = '9111'                                              
022650     OR (MID-IDARTNR-IN NUMERIC                                           
022651     AND MID-IDARTNR-IN > ZERO)                                           
022660         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
022670     END-IF                                                               
022680     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022690     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
022691     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
022692                                                                          
023300     SKIP2                                                                
023400     IF MFS-IDTRANS NOT = '9111'                                          
023500       MOVE ' ' TO MFS-KDTRTYP                                            
023600     END-IF                                                               
023700     SKIP2                                                                
023800     MOVE LOW-VALUE TO MOD-W9O11101                                       
023900     MOVE MFS-RENSA-FAELT TO MOD-MESSAGE-RAD1                             
024000                             MOD-MESSAGE-RAD23                            
024100     SKIP1                                                                
024200     MOVE JA TO ARTIKEL-RETT                                              
024300     IF IDARTNR-WS NOT NUMERIC                                            
024400       MOVE NEJ TO ARTIKEL-RETT                                           
024500       MOVE FEL-1 TO MOD-MESSAGE-RAD1                                     
024600     END-IF                                                               
024700     EJECT                                                                
024800     MOVE 'W9O11101' TO MFS-IDMOD                                         
024900     MOVE '9111' TO MOD-IDTRANS                                           
025000     SKIP1                                                                
025100     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
025200     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
025300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
025400     SKIP1                                                                
025500     IF MFS-UPDATE                                                        
025600       MOVE MFS-ROER-EJ-FAELT TO MOD-BEART-SVE                            
025700                               MOD-BEART-ENG                              
025800                               MOD-KDSRA                                  
025900                               MOD-KDARTURS                               
026000                               MOD-KDTULLRE                               
026100                               MOD-KDTULLRE-NY                            
026200                               MOD-KDIART                                 
026300                               MOD-KDHF                                   
026400                               MOD-IDANSK                                 
026500                               MOD-IDLEVNR                                
026600                               MOD-IDINK                                  
026700                               MOD-KVPB-TOT                               
026800                               MOD-PRARTSJK                               
026900                               MOD-KVDISP                                 
027000     END-IF                                                               
027100     .                                                                    
027200     EJECT                                                                
027300******************************************************************        
027400*    KONTROLL ATT ARTIKEL FINNS PÅ WDK6                                   
027500*                                                                         
027600*                                                                         
027700 B-FINNS-ARTIKEL SECTION.                                                 
027800     SKIP2                                                                
027900     MOVE NEJ TO ARTIKEL-FINNS                                            
028000     MOVE IDARTNR-WS TO W-IDARTNR                                         
028100     SKIP2                                                                
028200*                                 WLARTC01-WDK601                         
028300     SKIP1                                                                
028400     PERFORM IMS-LAES-ARTIKELINFO                                         
028500     IF SEGMENT-FINNS                                                     
028600       MOVE JA TO ARTIKEL-FINNS                                           
028700       IF ART-FLIART = JA                                                 
028800         MOVE JA TO MOD-KDIART                                            
028900       ELSE                                                               
029000         MOVE NEJ TO MOD-KDIART                                           
029100       END-IF                                                             
029200     ELSE                                                                 
029300       MOVE FEL-2 TO MOD-MESSAGE-RAD1                                     
029400       PERFORM S01-RENSA-FAELT                                            
029500     END-IF                                                               
029600     .                                                                    
029700     EJECT                                                                
029800*****************************************************************         
029900*    INFORMATIONSVÄRDEN PÅ ÖNSKAT ARTIKELNR LÄGGS UT                      
030000*    INFORMATIONEN HÄMTAS FRÅN  WDK6 OCH WDD3                             
030100*                                                                         
030200 C-INFORMATIONSBILD SECTION.                                              
030300     SKIP1                                                                
030400*                                 WLARTC11-WDK611                         
030500     SKIP1                                                                
030600     MOVE ART-IDLEVNR TO MOD-IDLEVNR                                      
030700     PERFORM IMS-GET-ARTC11                                               
030800     IF SEGMENT-FINNS                                                     
030900       MOVE CLAG-IDANSK TO MOD-IDANSK                                     
031000       MOVE CLAG-IDINK TO MOD-IDINK                                       
031100       MOVE CLAG-KDHF TO MOD-KDHF                                         
031101       MOVE CLAG-KDSRA TO MOD-KDSRA                                       
031102       MOVE CLAG-KDARTURS TO MOD-KDARTURS                                 
031103       MOVE CLAG-KDTULLRE TO MOD-KDTULLRE                                 
031104       MOVE CLAG-PRARTSJK TO MOD-PRARTSJK                                 
031105                                                                          
031106       SUBTRACT CLAG-KVRESS FROM CLAG-KVLS                                
031107       GIVING MOD-KVDISP                                                  
031108                                                                          
031110       ADD CLAG-KVPB-SEP CLAG-KVPB-SATS                                   
031120       GIVING MOD-KVPB-TOT                                                
031121     END-IF                                                               
031122                                                                          
032970     EJECT                                                                
032980                                                                          
035000     MOVE 'S  ' TO W-IDSKYLT                                              
035100     PERFORM IMS-GU-BEN-SEQ                                               
035200                                                                          
035300     IF SEGMENT-FINNS                                                     
035400       MOVE BEN-TEXT-BEART TO MOD-BEART-SVE                               
035500     END-IF                                                               
035600     MOVE 'GB ' TO W-IDSKYLT                                              
035700     PERFORM IMS-GU-BEN-SEQ                                               
035800                                                                          
035900     IF SEGMENT-FINNS                                                     
036000       MOVE BEN-TEXT-BEART TO MOD-BEART-ENG                               
036100     END-IF                                                               
036200     .                                                                    
036300     EJECT                                                                
036400 D-KONTROLLERA-INDATA SECTION.                                            
036500     SKIP2                                                                
036600     IF MID-KDTULLRE-NY = ALL '+'                                         
036700       MOVE FEL-6 TO MOD-MESSAGE-RAD1                                     
036800       MOVE INGET-IFYLLT TO FALT-KDTULLRE                                 
036900     ELSE                                                                 
037000       EVALUATE TRUE                                                      
037100       WHEN MID-KDTULLRE-NY = 0 OR 2                                      
037200         MOVE RETT TO FALT-KDTULLRE                                       
037300        WHEN OTHER                                                        
037400         MOVE MFS-NUM-FAELT-FEL TO MOD-KDTULLRE-NY-ATTR                   
037500         MOVE FEL TO FALT-KDTULLRE                                        
037600         MOVE FEL-4 TO MOD-MESSAGE-RAD1                                   
037700       END-EVALUATE                                                       
037800     END-IF                                                               
037900     .                                                                    
038000     EJECT                                                                
038100 E-UPPDATERA-TULLREST SECTION.                                            
038200     SKIP2                                                                
038300     MOVE IDARTNR-WS TO W-IDARTNR                                         
040210     PERFORM IMS-LAES-HOLD-ARTC11                                         
040211     IF SEGMENT-FINNS                                                     
040212        MOVE MID-KDTULLRE-NY      TO CLAG-KDTULLRE                        
040213        PERFORM IMS-REPLACE-WLARTC                                        
040214*                                                                         
040215       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDTULLRE-ATTR                    
040216       MOVE MID-KDTULLRE-NY       TO MOD-KDTULLRE                         
040217*                                                                         
040218       MOVE MFS-FORMATETS-ATTR TO MOD-KDTULLRE-NY-ATTR                    
040219       MOVE MFS-RENSA-FAELT    TO MOD-KDTULLRE-NY                         
040220*                                                                         
040221       MOVE RETT-1 TO MOD-MESSAGE-RAD23                                   
040222     ELSE                                                                 
040223       MOVE FEL-2 TO MOD-MESSAGE-RAD1                                     
040224       PERFORM S01-RENSA-FAELT                                            
040230     END-IF                                                               
040240     .                                                                    
040400     EJECT                                                                
040500 S01-RENSA-FAELT SECTION.                                                 
040600                                                                          
040700     MOVE MFS-RENSA-FAELT TO   MOD-BEART-SVE                              
040800                             MOD-BEART-ENG                                
040900                             MOD-KDSRA                                    
041000                             MOD-KDARTURS                                 
041100                             MOD-KDTULLRE                                 
041200                             MOD-KDTULLRE-NY                              
041300                             MOD-KDIART                                   
041400                             MOD-KDHF                                     
041500                             MOD-IDANSK                                   
041600                             MOD-IDLEVNR                                  
041700                             MOD-IDINK                                    
041800                             MOD-KVPB-TOT                                 
041900                             MOD-PRARTSJK                                 
042000                             MOD-KVDISP                                   
042100     .                                                                    
042200     EJECT                                                                
042300******************************************************************        
042400* IMS SEKTIONER                                                           
042500     SKIP3                                                                
042600 IMS-GET-MSG SECTION.                                                     
042700                                                                          
042800     MOVE '  QC' TO GODK-STATUSKODER                                      
042900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
043000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043100     PERFORM IMS-STATUSKONTROLL                                           
043200     .                                                                    
043300     SKIP3                                                                
043400 IMS-INSERT-MSG SECTION.                                                  
043500                                                                          
043510     IF MSGI-IDLAND-SPR = 'GB'                                            
043520        MOVE 'N' TO MFS-KDHUVOMR                                          
043530     END-IF                                                               
043600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
043700     MOVE SPACE TO GODK-STATUSKODER                                       
043800     CALL CBLTDLI USING ISRT MSG-PCB                                      
043900                          MSG-IO-AREA MFS-IDMOD                           
044000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
044100     PERFORM IMS-STATUSKONTROLL                                           
044200     .                                                                    
044300     EJECT                                                                
045700                                                                          
045710 IMS-LAES-ARTIKELINFO SECTION.                                            
045800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
045900            DELIMITED BY SIZE INTO SSA1                                   
046000     MOVE '  GE' TO GODK-STATUSKODER                                      
046100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
046200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
046300     PERFORM IMS-STATUSKONTROLL                                           
046400     .                                                                    
046500     SKIP3                                                                
046600 IMS-GET-ARTC11 SECTION.                                                  
046700                                                                          
046800     MOVE 'WLARTC11 ' TO SSA1                                             
046900     MOVE '  GE' TO GODK-STATUSKODER                                      
047000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
047100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
047200     PERFORM IMS-STATUSKONTROLL                                           
047300     .                                                                    
047400     EJECT                                                                
052510 IMS-LAES-HOLD-ARTC11 SECTION.                                            
052520                                                                          
052530     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
052540     DELIMITED BY SIZE INTO SSA1                                          
052550     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
052560     DELIMITED BY SIZE INTO SSA2                                          
052570     MOVE '  GE' TO GODK-STATUSKODER                                      
052580     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1 SSA2                
052590     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
052591     PERFORM IMS-STATUSKONTROLL                                           
052592     .                                                                    
052593     SKIP3                                                                
052594 IMS-REPLACE-WLARTC SECTION.                                              
052595                                                                          
052596     MOVE '  ' TO GODK-STATUSKODER                                        
052597     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
052598     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
052599     PERFORM IMS-STATUSKONTROLL                                           
052600     .                                                                    
052601     SKIP3                                                                
052610 IMS-GU-BEN-SEQ SECTION.                                                  
052700                                                                          
052800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
052900     DELIMITED BY SIZE INTO SSA1                                          
053000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
053100     DELIMITED BY SIZE INTO SSA2                                          
053200     MOVE '  GE' TO GODK-STATUSKODER                                      
053300     CALL CBLTDLI USING GU BEN-PCB DLI-IO-AREA SSA1 SSA2                  
053400     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
053500     PERFORM IMS-STATUSKONTROLL                                           
053600     .                                                                    
053700     EJECT                                                                
053800 IMS-STATUSKONTROLL SECTION.                                              
053900     SET STATUS-IX TO 1                                                   
054000     SEARCH GODK-STATUS AT END CALL FELLOG                                
054100     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
054200     END-SEARCH                                                           
054400     .                                                                    
