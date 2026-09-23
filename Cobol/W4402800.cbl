000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4402800.                                        
000400 AUTHOR.                 GUNNAR LARSSON, IDK.                             
000500 DATE-WRITTEN.           FEBR 1991.                                       
000600                                                                          
000700******************************************************************        
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*      ERSÄTTNINGSBEVAKNING RESTORDER.                                    
001200*                                                                         
001300*      PROGRAMMET UPPDATERAR ORDERBEKRÄFTELSE (WDQ1/WLORQM)               
001400*      FÖR ERSATTA OCH TILLKOMMANDE-FÖRÄNDRADE ARTIKLAR.                  
001500*                                                                         
001600*      PROGRAMMET KÖRS SOM BMP.                                           
001700*      HTR 4555 WLXXLO/WDR4 UPPDATERAS MED CHKP-RÄKNARE MM.               
001800*      VID ÅTERSTART IN-FILEN FRAM TILL SENASTE CHKP-LÄGE.                
001900*      CHKP TAS VID BRYTNING IDORDER OM MAX ANTAL UPPDAT PER              
002000*      CHKP UPPNÅTT.                                                      
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 - RETURKOD VID FEL ÅTERSTART                               
002400*                                                                         
002500******************************************************************        
002603*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
002604*    E-TRACKER:10254592  2015       DECOMISSION VOHF                      
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*    ---- INFIL:                                                          
003500     SELECT  INFIL         ASSIGN  W44028D1.                              
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800                                                                          
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  INFIL                                                                
004200     LABEL RECORD STANDARD                                                
004300     RECORDING  V                                                         
004400     BLOCK CONTAINS 0.                                                    
004500                                                                          
004600 01  FILLER                  PIC X(998).                                  
004700*01  -COPY W440001       -L.                                              
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                   PIC X(8)    VALUE 'W4402800'.                
005300     SKIP1                                                                
005400 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
005500     SKIP1                                                                
005600 77  JA                      PIC X       VALUE 'J'.                       
005700 77  NEJ                     PIC X       VALUE 'N'.                       
005800     SKIP1                                                                
005900 77  CHKP-ID                 PIC X(8)    VALUE 'W44028  '.                
006000 77  MSG-IO-AREA-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
006100 77  MSG-IO-AREA             PIC X(32)   VALUE SPACE.                     
006200 77  CHKP-AREA-1-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
006300 77  CHKP-AREA-1             PIC X(32)   VALUE SPACE.                     
006400     SKIP1                                                                
006500 77  INFIL-EOF               PIC X       VALUE 'N'.                       
006600     EJECT                                                                
006700 01  FILLER                  PIC X(16)   VALUE 'WS-FLD**********'.        
006800 01  WS-FLD.                                                              
006900     SKIP1                                                                
007000   03  WS-ANT-POST-IN        PIC S9(5)   VALUE ZERO      COMP-3.          
007100   03  WS-ANT-UPPD-CHKP      PIC S9(5)   VALUE ZERO      COMP-3.          
007200*    CHKP-GRÄNS 100 UPPD.                                                 
007300   03  WS-MAX-UPPD-CHKP      PIC S9(5)   VALUE +100      COMP-3.          
007400     SKIP1                                                                
007500*    RÄKNARE FÖR ID I ORDERBEKR.                                          
007600   03  WS-OBKR.                                                           
007700     05  WS-OBKR-IDLOPNR     PIC S9(3)   COMP-3.                          
007800     05  WS-OBKR-IDSEKVNR    PIC S9(3)   COMP-3.                          
007900     SKIP1                                                                
008000*    SPARAT FRÅN ERS-POST                                                 
008100   03  WS-ERS.                                                            
008200     05  WS-ERS-IDARTNR      PIC S9(9)   COMP-3.                          
008300     05  WS-ERS-REKSIFFR     PIC S9(1)   COMP-3.                          
008400     05  WS-ERS-KDORDBEK     PIC 9(2).                                    
008500     SKIP1                                                                
008600*    FRÅN AKTUELL INPOST                                                  
008700   03  WS-AKT.                                                            
008800     05  WS-AKT-KDRESTR      PIC S9(3)   COMP-3.                          
008900     05  WS-AKT-KDORDBEK     PIC 9(2).                                    
009000     SKIP1                                                                
009100*    BRYT-ID                                                              
009200   03  WS-BRYT.                                                           
009300     05  WS-BRYT-IDORDER     PIC S9(7)   COMP-3.                          
009400     05  WS-BRYT-IDARTNR-URS PIC S9(9)   COMP-3.                          
009500     05  WS-BRYT-IDLOPNRE    PIC S9(3)   COMP-3.                          
009600     SKIP1                                                                
009700   03  WS-TIAAMMDD           PIC 9(6).                                    
009800   03  WS-TIKLOCK            PIC 9(8).                                    
009900   03  FILLER                REDEFINES WS-TIKLOCK.                        
010000     05  WS-TIKLOCK-TTMMSS   PIC 9(6).                                    
010100     05  FILLER              PIC X(2).                                    
010200     SKIP1                                                                
010300*    INFO FRÅN SIST KONSUMERAD IN-POST FÖRE CHKP                          
010400   03  WS-CHKPID.                                                         
010500     05  WS-CHKPID-KVPOST    PIC S9(7)               COMP-3.              
010600     05  WS-CHKPID-IDDISTR   PIC S9(5)               COMP-3.              
010700     05  WS-CHKPID-IDKUNDNR  PIC S9(7)               COMP-3.              
010800     05  WS-CHKPID-IDKUNDRF  PIC X(10).                                   
010900     05  WS-CHKPID-IDLOPNRE  PIC S9(3)               COMP-3.              
011000     05  WS-CHKPID-IDKORTNR-ERS                                           
011100                             PIC S9(3)               COMP-3.              
011200     SKIP1                                                                
011300*    TIDS-STÄMPEL FÖR CHKP-HTR                                            
011400   03  WS-TS.                                                             
011500     05  WS-TS-TIAAMMDD      PIC 9(6).                                    
011600     05  WS-TS-TIKLOCK       PIC 9(8).                                    
011700     EJECT                                                                
011800 01  FILLER                  PIC X(16)   VALUE 'K-KONSTANTER****'.        
011900 01  K-KONSTANTER.                                                        
012000   03  K-9KOMPL              PIC S9(9)   VALUE +999999999 COMP-3.         
012100     SKIP1                                                                
012200 01  FILLER                  PIC X(16)   VALUE 'KOMPL.DATUM*****'.        
012300 01  WS-DAT-9KOMPL               PIC 9(8).                                
012400 01  FILLER REDEFINES WS-DAT-9KOMPL.                                      
012500     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
012600     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
012700     EJECT                                                                
012800 01  FILLER                  PIC X(16)   VALUE 'IN-AREA*********'.        
012900*01  IN-POST  -COPY W440001                                               
013000     EJECT                                                                
013100*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
013200     SKIP3                                                                
013300 01  DYNAMISKA-SUBPROGRAM.                                                
013400   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
013500   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
013600   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
013700   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
013800   03  DATKORT               PIC X(8)    VALUE 'DATKORT '.                
013900   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
014000     SKIP3                                                                
014100*    ----  PARAMETRAR TILL ABEND                                          
014200     SKIP1                                                                
014300 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16 COMP SYNC.               
014400 01  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +33 COMP SYNC.               
014500     EJECT                                                                
014600*    ----  PARAMETRAR TILL POSTSUM                                        
014700                                                                          
014800*01  -COPY W0005       -PRE POSTSUM-.                                     
014900     EJECT                                                                
015000*    ----  PARAMETRAR TILL DATUMKORT                                      
015100                                                                          
015200 01  DATUMKORT-ID            PIC X(6)   VALUE 'WDATUM'.                   
015300     SKIP3                                                                
015400*01  -COPY WDATKORT                                                       
015500     EJECT                                                                
015600*    ----  PARAMETRAR TILL WDATKONV                                       
015700                                                                          
015800 01  FILLER                  PIC X(16)  VALUE 'WDATAREA********'.         
015900     SKIP2                                                                
016000*01  -COPY WDATAREA                                                       
016100     EJECT                                                                
016200*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
016300                                                                          
016400 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
016500     SKIP3                                                                
016600*    ---- STATUSKOD FRÅN IMS                                              
016700                                                                          
016800 01  STATUS-WS               PIC XX.                                      
016900     88  SEGMENT-FINNS                    VALUE '  '.                     
017000     88  IMS-EJ-OK                        VALUE 'XD'.                     
017100     SKIP3                                                                
017200 01  GODK-STATUSKODER.                                                    
017300   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
017400     SKIP3                                                                
017500 01  SSA1                    PIC X(96).                                   
017600 01  SSA2                    PIC X(64).                                   
017700     EJECT                                                                
017800*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
017900                                                                          
018000 01  NYCKLAR-TILL-DLI.                                                    
018100                                                                          
018200   03  W-IDHTYP-4555-X.                                                   
018300     05  FILLER              PIC X(4)    VALUE '4555'.                    
018400     05  FILLER              PIC X(26)   VALUE LOW-VALUE.                 
018500                                                                          
018600   03  W-WDQ101KY-F-X.                                                    
018700     05  W-WDQ101KY-F-IDORDER  PIC S9(7)             COMP-3.              
018800     05  W-WDQ101KY-F-IDARTNR  PIC S9(9)             COMP-3.              
018900     05  FILLER                PIC X(8)  VALUE LOW-VALUE.                 
019000                                                                          
019100   03  W-WDQ101KY-T-X.                                                    
019200     05  W-WDQ101KY-T-IDORDER  PIC S9(7)             COMP-3.              
019300     05  W-WDQ101KY-T-IDARTNR  PIC S9(9)             COMP-3.              
019400     05  FILLER                PIC X(8)  VALUE HIGH-VALUE.                
019500                                                                          
019600     EJECT                                                                
019700*01  -COPY W0003                                                          
019800     EJECT                                                                
019900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
020000     SKIP3                                                                
020100 01  DLI-IO-AREA.                                                         
020200   03  IO-AREA               PIC X(400).                                  
020300     SKIP3                                                                
020400*03  WLORQM01 -COPY WDQ101      -PRE ORQM-     -RED IO-AREA               
020500     EJECT                                                                
020600*03  WLXXLO11 -COPY WDGX4556    -PRE XXLO-     -RED IO-AREA               
020700     EJECT                                                                
020800     EJECT                                                                
020900 LINKAGE SECTION.                                                         
021000     SKIP2                                                                
021100*01  -COPY W0009      -PRE  MSG-                                          
021200     EJECT                                                                
021300*01  -COPY W0008      -PRE  ORQM-                                         
021400       05  FILLER                PIC X.                                   
021500     EJECT                                                                
021600*01  -COPY W0008      -PRE  XXLO-                                         
021700       05  FILLER                PIC X.                                   
021800     EJECT                                                                
021900 PROCEDURE DIVISION  USING  MSG-PCB                                       
022000                            ORQM-PCB XXLO-PCB.                            
022100     ENTRY 'DLITCBL' USING  MSG-PCB                                       
022200                            ORQM-PCB XXLO-PCB.                            
022300     SKIP2                                                                
022400     PERFORM A-INIT                                                       
022500                                                                          
022600     PERFORM C-BEHANDLA-SORTERADE-POSTER.                                 
022700                                                                          
022800     PERFORM Z-FINIT                                                      
022900     MOVE ZERO TO RETURN-CODE                                             
023000     GOBACK                                                               
023100     .                                                                    
023200     EJECT                                                                
023300 A-INIT SECTION.                                                          
023400     SKIP2                                                                
023500     OPEN INPUT  INFIL                                                    
023600                                                                          
023700     ACCEPT WS-TIAAMMDD      FROM DATE                                    
023800     ACCEPT WS-TIKLOCK       FROM TIME                                    
023900                                                                          
024000     PERFORM IMS-RESTART                                                  
024100                                                                          
024200     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
024300                                                                          
024400     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
024500     .                                                                    
024600     EJECT                                                                
024700 C-BEHANDLA-SORTERADE-POSTER SECTION.                                     
024800     SKIP2                                                                
024900     MOVE NEJ                TO INFIL-EOF                                 
025000     MOVE ZERO               TO WS-ANT-POST-IN                            
025100     MOVE ZERO               TO WS-ANT-UPPD-CHKP                          
025200                                                                          
025300     PERFORM CA-INIT-BMP                                                  
025400                                                                          
025500     PERFORM S01-LAS-INFIL                                                
025600                                                                          
025700*----------------------------------------- ORDER/ARTNR-URS GRP            
025800     PERFORM UNTIL (INFIL-EOF = JA)                                       
025900                                                                          
026000       MOVE FOR-IDORDER      TO WS-BRYT-IDORDER                           
026100       MOVE FOR-IDARTNR-URS  TO WS-BRYT-IDARTNR-URS                       
026200                                                                          
026300       PERFORM CC-INIT-OBKR-ORD-ART                                       
026400                                                                          
026500*----------------------------------------- ERS.STRUKT/RO GRP              
026600       PERFORM UNTIL (INFIL-EOF = JA                                      
026700                  OR  FOR-IDORDER     NOT = WS-BRYT-IDORDER               
026800                  OR  FOR-IDARTNR-URS NOT = WS-BRYT-IDARTNR-URS)          
026900                                                                          
027000         MOVE FOR-IDLOPNRE   TO WS-BRYT-IDLOPNRE                          
027100                                                                          
027200         PERFORM CD-INIT-OBKR-ERS-RO                                      
027300                                                                          
027400         IF  FOR-KDRADERS = +1                                            
027500*          * ERSÄTTNING                                                   
027600                                                                          
027700           PERFORM S15-SPARA-ERS                                          
027800           PERFORM S10-RED-OBKR-GEM                                       
027900           PERFORM S11-RED-OBKR-ERS                                       
028000           PERFORM S02-SKRIV-OBKR                                         
028100                                                                          
028200           PERFORM S09-SPARA-CHKPID                                       
028300           PERFORM S01-LAS-INFIL                                          
028400                                                                          
028500*----------------------------------------- TILLK. GRP                     
028600           PERFORM UNTIL (INFIL-EOF = JA                                  
028700                      OR  FOR-IDLOPNRE NOT = WS-BRYT-IDLOPNRE)            
028800                                                                          
028900             IF  FOR-KDSTARAD = '1'                                       
029000             OR  FOR-KDSTARAD = '2'                                       
029100*              * ENDAST TPO & RO BEKRÄFTAS (EJ RESERVERING)               
029200                                                                          
029300               PERFORM S10-RED-OBKR-GEM                                   
029400               PERFORM S12-RED-OBKR-TIL                                   
029500               MOVE WS-ERS-KDORDBEK TO ORQM-OBKR-KDORDBEK                 
029600               PERFORM S02-SKRIV-OBKR                                     
029700                                                                          
029800               MOVE FOR-KDRESTR TO WS-AKT-KDRESTR                         
029900               PERFORM S16-KONV-RESTR-ORDBEK                              
030000                                                                          
030100               IF WS-AKT-KDORDBEK = ZERO                                  
030200               OR WS-AKT-KDORDBEK = WS-ERS-KDORDBEK                       
030300*                * KDORDBEK NOLL ELLER SAMMA SOM ERS-POST                 
030400                 CONTINUE                                                 
030500                                                                          
030600               ELSE                                                       
030700                 IF WS-AKT-KDORDBEK NOT = 47                              
030800*                   * KDORDBEK AVVIKANDE MOT ERS-POST,                    
030900*                   * EXTRA OBKR SKRIVS.                                  
031000                    PERFORM S10-RED-OBKR-GEM                              
031100                    PERFORM S12-RED-OBKR-TIL                              
031200                    MOVE WS-AKT-KDORDBEK TO ORQM-OBKR-KDORDBEK            
031300                    PERFORM S02-SKRIV-OBKR                                
031400                 END-IF                                                   
031500               END-IF                                                     
031600                                                                          
031700             ELSE                                                         
031800*              * KDSTARAD=3 (RESERVERING) ORDERBEKRÄFTAS EJ.              
031900               CONTINUE                                                   
032000             END-IF                                                       
032100                                                                          
032200             PERFORM S09-SPARA-CHKPID                                     
032300             PERFORM S01-LAS-INFIL                                        
032400           END-PERFORM                                                    
032500                                                                          
032600         ELSE                                                             
032700*          * LTK-FÖRÄNDRAD RAD                                            
032800                                                                          
032900           DISPLAY 'HÄR BORDE BARA LTK-ÄNDR HAMNA ?? LASSI'               
033000*          CALL ABEND USING  RKOD-ABEND-MED-DUMP                          
033100           IF  FOR-KDSTARAD = '1'                                         
033200           OR  FOR-KDSTARAD = '2'                                         
033300*            * ENDAST TPO & RO BEKRÄFTAS (EJ RESERVERING)                 
033400                                                                          
033500             MOVE FOR-KDRESTR   TO WS-AKT-KDRESTR                         
033600             PERFORM S16-KONV-RESTR-ORDBEK                                
033700                                                                          
033800             IF WS-AKT-KDORDBEK NOT = 47                                  
033900               PERFORM S10-RED-OBKR-GEM                                   
034000               PERFORM S13-RED-OBKR-LTK                                   
034100               MOVE WS-AKT-KDORDBEK TO ORQM-OBKR-KDORDBEK                 
034200               PERFORM S02-SKRIV-OBKR                                     
034300             END-IF                                                       
034400                                                                          
034500           ELSE                                                           
034600*            * KDSTARAD=3 (RESERVERING) ORDERBEKRÄFTAS EJ.                
034700             CONTINUE                                                     
034800           END-IF                                                         
034900                                                                          
035000           PERFORM S09-SPARA-CHKPID                                       
035100           PERFORM S01-LAS-INFIL                                          
035200         END-IF                                                           
035300                                                                          
035400*---------------------------- VID BRYTNING IDLOPNRE,                      
035500*                             OM ANTAL UPPD. UPPNÅTT MAX-GRÄNS:           
035600*                             I DET FALL IN-FILEN EJ ÄR SLUT;             
035700*                              TAS CHECKPOINT.                            
035800*                             VID EOF IN-FIL;                             
035900*                              TAS INGEN CHECKPOINT, EFTERSOM             
036000*                              NORMALT PGM-AVSLUT SNART SKALL SKE.        
036100         IF  WS-ANT-UPPD-CHKP >= WS-MAX-UPPD-CHKP                         
036200           IF  INFIL-EOF = NEJ                                            
036300             PERFORM CB-TAG-CHECKPOINT                                    
036400             MOVE ZERO   TO WS-ANT-UPPD-CHKP                              
036500           END-IF                                                         
036600         END-IF                                                           
036700                                                                          
036800       END-PERFORM                                                        
036900                                                                          
037000     END-PERFORM                                                          
037100     .                                                                    
037200     EJECT                                                                
037300 CA-INIT-BMP SECTION.                                                     
037400     SKIP2                                                                
037500     PERFORM IMS-GHU-XXLO-4556                                            
037600                                                                          
037700     IF SEGMENT-FINNS                                                     
037800                                                                          
037900       IF  XXLO-4556-KVPOST > ZERO                                        
038000                                                                          
038100*--------------------------- INFIL LÄSES                                  
038200*                            FRAM TILL CHECKPOINT-LÄGE                    
038300                                                                          
038400         PERFORM S01-LAS-INFIL                                            
038500                                                                          
038600         PERFORM UNTIL (INFIL-EOF = JA                                    
038700                    OR  WS-ANT-POST-IN >= XXLO-4556-KVPOST)               
038800           PERFORM S01-LAS-INFIL                                          
038900         END-PERFORM                                                      
039000                                                                          
039100         IF  INFIL-EOF      = JA                                          
039200         OR  WS-ANT-POST-IN   NOT = XXLO-4556-KVPOST                      
039300         OR  FOR-IDDISTR      NOT = XXLO-4556-IDDISTR                     
039400         OR  FOR-IDKUNDNR     NOT = XXLO-4556-IDKUNDNR                    
039500         OR  FOR-IDKUNDRF     NOT = XXLO-4556-IDKUNDRF                    
039600         OR  FOR-IDLOPNRE     NOT = XXLO-4556-IDLOPNRE                    
039700         OR  FOR-IDKORTNR-ERS NOT = XXLO-4556-IDKORTNR                    
039800           DISPLAY 'W4402800: FEL I ÅTERSTARTEN, IN-FIL'                  
039900           CALL ABEND USING  RKOD-ABEND-UTAN-DUMP                         
040000         END-IF                                                           
040100                                                                          
040200*------- NU ÄR DEN IN-POST INLÄST, SOM VID FÖREGÅENDE EXEKVERING          
040300*        "KONSUMERADES" NÄRMAST FÖRE SISTA CHECKPOINT.                    
040400                                                                          
040500       END-IF                                                             
040600     END-IF                                                               
040700     .                                                                    
040800     EJECT                                                                
040900 CB-TAG-CHECKPOINT SECTION.                                               
041000     SKIP2                                                                
041100     PERFORM IMS-GHU-XXLO-4556                                            
041200                                                                          
041300     MOVE SPACE              TO XXLO-4556-WDGX4556                        
041400     MOVE '1'                TO XXLO-4556-KDSEGKEY                        
041500                                                                          
041600     MOVE WS-CHKPID-KVPOST   TO XXLO-4556-KVPOST                          
041700     MOVE WS-CHKPID-IDDISTR  TO XXLO-4556-IDDISTR                         
041800     MOVE WS-CHKPID-IDKUNDNR TO XXLO-4556-IDKUNDNR                        
041900     MOVE WS-CHKPID-IDKUNDRF TO XXLO-4556-IDKUNDRF                        
042000     MOVE WS-CHKPID-IDLOPNRE TO XXLO-4556-IDLOPNRE                        
042100     MOVE WS-CHKPID-IDKORTNR-ERS                                          
042200                             TO XXLO-4556-IDKORTNR                        
042300                                                                          
042400     ACCEPT WS-TS-TIAAMMDD   FROM DATE                                    
042500     MOVE WS-TS-TIAAMMDD     TO XXLO-4556-TIUPPDAT                        
042600     ACCEPT WS-TS-TIKLOCK    FROM TIME                                    
042700     MOVE WS-TS-TIKLOCK      TO XXLO-4556-TIUPPTID                        
042800                                                                          
042900     IF  SEGMENT-FINNS                                                    
043000       PERFORM IMS-REPL-XXLO                                              
043100     ELSE                                                                 
043200       PERFORM IMS-ISRT-XXLO-4556                                         
043300     END-IF                                                               
043400                                                                          
043500     MOVE CHKP-ID            TO MSG-IO-AREA                               
043600     PERFORM IMS-CHECKPOINT                                               
043700     .                                                                    
043800     EJECT                                                                
043900 CC-INIT-OBKR-ORD-ART SECTION.                                            
044000*                                                                         
044100*    FÖR AKTUELL IDORDER - IDARTNR-URS.                                   
044200*                                                                         
044300*    IDLOPNR SÄTTS TILL NOLL ELLER TILL HÖGSTA BEFINTLIGA                 
044400*      IDLOPNR FÖR AKTUELL IDORDER - IDARTNR-URS.                         
044500*      ÖKAS SENARE MED 1 FÖR VARJE "ERSÄTTNINGS-STRUKTUR" PER RO.         
044600*                                                                         
044700     MOVE WS-BRYT-IDORDER    TO W-WDQ101KY-F-IDORDER                      
044800     MOVE WS-BRYT-IDARTNR-URS                                             
044900                             TO W-WDQ101KY-F-IDARTNR                      
045000     MOVE WS-BRYT-IDORDER    TO W-WDQ101KY-T-IDORDER                      
045100     MOVE WS-BRYT-IDARTNR-URS                                             
045200                             TO W-WDQ101KY-T-IDARTNR                      
045300     PERFORM IMS-GU-ORQM-OBKR                                             
045400                                                                          
045500     IF  SEGMENT-FINNS                                                    
045600                                                                          
045700       PERFORM UNTIL (NOT SEGMENT-FINNS)                                  
045800         PERFORM IMS-GN-ORQM-OBKR                                         
045900       END-PERFORM                                                        
046000                                                                          
046100       MOVE ORQM-OBKR-IDLOPNR                                             
046200                             TO WS-OBKR-IDLOPNR                           
046300     ELSE                                                                 
046400       MOVE ZERO             TO WS-OBKR-IDLOPNR                           
046500     END-IF                                                               
046600     .                                                                    
046700     EJECT                                                                
046800 CD-INIT-OBKR-ERS-RO SECTION.                                             
046900*                                                                         
047000*    FÖR AKTUELL "ERSÄTTNINGS-STRUKTUR" PER RO.                           
047100*                                                                         
047200*    IDLOPNR ÖKAS MED 1.                                                  
047300*    IDSEKVNR INITIERAS TILL NOLL,                                        
047400*      ÖKAS SENARE MED 1 FÖR VARJE SEGMENT INOM EN                        
047500*      "ERSÄTTNINGS-STRUKTUR" PER RO                                      
047600*      SOM LÄGGS UPP PÅ OBKR-BASEN.                                       
047700*                                                                         
047800     ADD +1                  TO WS-OBKR-IDLOPNR                           
047900     MOVE ZERO               TO WS-OBKR-IDSEKVNR                          
048000     .                                                                    
048100     EJECT                                                                
048200 Z-FINIT SECTION.                                                         
048300     SKIP2                                                                
048400     CLOSE  INFIL                                                         
048500                                                                          
048600     PERFORM ZA-FINIT-BMP                                                 
048700                                                                          
048800     MOVE 'S' TO POSTSUM-OPKOD                                            
048900     CALL POSTSUM USING POSTSUM-PARM                                      
049000     .                                                                    
049100     EJECT                                                                
049200 ZA-FINIT-BMP SECTION.                                                    
049300*                                                                         
049400*    HTR FÖR CHKP-INFO TAS BORT                                           
049500*                                                                         
049600     PERFORM IMS-GHU-XXLO-4556                                            
049700                                                                          
049800     IF  SEGMENT-FINNS                                                    
049900       PERFORM IMS-DLET-XXLO                                              
050000     END-IF                                                               
050100     .                                                                    
050200     EJECT                                                                
050300 S01-LAS-INFIL SECTION.                                                   
050400                                                                          
050500     READ INFIL INTO IN-POST                                              
050600          AT END MOVE JA TO INFIL-EOF                                     
050700     END-READ                                                             
050800     IF INFIL-EOF = NEJ                                                   
050900        MOVE 'W44028'    TO POSTSUM-FDNAMN                                
051000        MOVE 'W44028D1'  TO POSTSUM-DDNAMN2                               
051100        MOVE 'IN'        TO POSTSUM-TRANSTYP                              
051200        CALL POSTSUM USING POSTSUM-PARM                                   
051300        ADD +1           TO WS-ANT-POST-IN                                
051400     END-IF                                                               
051500     .                                                                    
051600     EJECT                                                                
051700 S02-SKRIV-OBKR SECTION.                                                  
051800*                                                                         
051900*    LÄGG UPP ORDERBEKR.                                                  
052000*                                                                         
052100     PERFORM IMS-ISRT-ORQM-OBKR                                           
052200                                                                          
052300     ADD +1              TO WS-ANT-UPPD-CHKP                              
052400     .                                                                    
052500     EJECT                                                                
052600 S09-SPARA-CHKPID SECTION.                                                
052700*                                                                         
052800*    HÄR SPARAS INFO FRÅN DEN SENAST KONSUMERADE IN-POSTEN,               
052900*    FÖR ATT KUNNA SPARA RÄTT INFO VID EV. CHKP.                          
053000*    DETTA GÖRS FÖRE LÄSNING AV NÄSTA IN-POST.                            
053100*                                                                         
053200     MOVE WS-ANT-POST-IN         TO WS-CHKPID-KVPOST                      
053300     MOVE FOR-IDDISTR            TO WS-CHKPID-IDDISTR                     
053400     MOVE FOR-IDKUNDNR           TO WS-CHKPID-IDKUNDNR                    
053500     MOVE FOR-IDKUNDRF           TO WS-CHKPID-IDKUNDRF                    
053600     MOVE FOR-IDLOPNRE           TO WS-CHKPID-IDLOPNRE                    
053700     MOVE FOR-IDKORTNR-ERS       TO WS-CHKPID-IDKORTNR-ERS                
053800     .                                                                    
053900     EJECT                                                                
054000 S10-RED-OBKR-GEM SECTION.                                                
054100*                                                                         
054200*    REDIGERA GEMENSAM DEL I ORDERBEKR.                                   
054300*                                                                         
054400     MOVE FOR-IDORDER            TO ORQM-OBKR-IDORDER                     
054500     MOVE WS-OBKR-IDLOPNR        TO ORQM-OBKR-IDLOPNR                     
054600     ADD +1                      TO WS-OBKR-IDSEKVNR                      
054700     MOVE WS-OBKR-IDSEKVNR       TO ORQM-OBKR-IDSEKVNR                    
054800*                                                                         
054900     MOVE FOR-BEERS              TO ORQM-OBKR-BEERS                       
055000     MOVE SPACE                  TO ORQM-OBKR-IDBIL                       
055100     MOVE IDPGM                  TO ORQM-OBKR-IDPGM                       
055200     MOVE FOR-BEKUNDRF           TO ORQM-OBKR-BEKUNDRF                    
055300     MOVE FOR-BERADREF           TO ORQM-OBKR-BERADREF                    
055400     MOVE FOR-BEVOLREF           TO ORQM-OBKR-BEVOLREF                    
055500     MOVE NEJ                    TO ORQM-OBKR-FLAKPLOC                    
055600     MOVE FOR-FLINVEST           TO ORQM-OBKR-FLINVEST                    
055700     MOVE JA                     TO ORQM-OBKR-FLOBOK                      
055800     MOVE NEJ                    TO ORQM-OBKR-FLOBTRAN                    
055900     MOVE NEJ                    TO ORQM-OBKR-FLOBPRT                     
056000     MOVE FOR-FLPRTILL           TO ORQM-OBKR-FLPRTILL                    
056100     MOVE JA                     TO ORQM-OBKR-FLRESTN                     
056200     MOVE NEJ                    TO ORQM-OBKR-FLSLATT                     
056300     MOVE NEJ                    TO ORQM-OBKR-FLTILLK                     
056400     MOVE FOR-IDDISTR            TO ORQM-OBKR-IDDISTR                     
056500     MOVE FOR-IDKUNDNR           TO ORQM-OBKR-IDKUNDNR                    
056600                                                                          
056700     MOVE '0000000   '           TO ORQM-OBKR-IDKUNDRF                    
056800     MOVE FOR-IDKUNDRF (1:5)     TO ORQM-OBKR-IDKUNDRF (3:5)              
056900                                                                          
057000     MOVE '0000000   '           TO ORQM-OBKR-IDKUNDRF-RO                 
057100     MOVE SPACE                  TO ORQM-OBKR-IDLEVNR                     
057200     MOVE FOR-IDLOPNR            TO ORQM-OBKR-IDLOPNR-RO                  
057300     MOVE FOR-IDSYSTEM           TO ORQM-OBKR-IDSYSTEM                    
057400     MOVE FOR-IDDC-RO            TO ORQM-OBKR-IDDC-RO                     
057500     MOVE FOR-KDOI               TO ORQM-OBKR-KDOI                        
057600     MOVE FOR-CLEARGROUP         TO ORQM-OBKR-CLEARGROUP                  
057700     MOVE FOR-KDDSP              TO ORQM-OBKR-KDDSP                       
057800     MOVE FOR-KDERS              TO ORQM-OBKR-KDERS                       
057900     MOVE FOR-KDKVBRYT           TO ORQM-OBKR-KDKVBRYT                    
058000     MOVE FOR-KDPRTYP            TO ORQM-OBKR-KDPRTYP                     
058100     MOVE FOR-KDTPOTYP           TO ORQM-OBKR-KDTPOTYP                    
058200     MOVE FOR-KDVRINFO           TO ORQM-OBKR-KDVRINFO                    
058300     MOVE ZERO                   TO ORQM-OBKR-KVANNANT                    
058400     MOVE ZERO                   TO ORQM-OBKR-KVAVBART                    
058500     MOVE ZERO                   TO ORQM-OBKR-KVPREAVB                    
058600     MOVE ZERO                   TO ORQM-OBKR-KVPRERO                     
058700     MOVE FOR-KVQPACK-1          TO ORQM-OBKR-KVQPACK                     
058800     MOVE FOR-KVRO               TO ORQM-OBKR-KVRO                        
058900     MOVE ZERO                   TO ORQM-OBKR-KVSLATT                     
059000     MOVE FOR-PRARTNTO           TO ORQM-OBKR-PRARTNTO                    
059010     MOVE FOR-PRAVCOST           TO ORQM-OBKR-PRAVCOST                    
059100     MOVE ZERO                   TO ORQM-OBKR-PRBPRIS                     
059200     MOVE ZERO                   TO ORQM-OBKR-RERF-RAD                    
059300     MOVE FOR-TIDISPIN           TO ORQM-OBKR-TIDISPIN                    
059400     MOVE FOR-TIORDREG           TO ORQM-OBKR-TIORDREG                    
059500     MOVE ZERO                   TO ORQM-OBKR-TIPRIS                      
059600     MOVE WS-TIAAMMDD            TO ORQM-OBKR-TIREGDAT                    
059700     MOVE WS-TIKLOCK-TTMMSS      TO ORQM-OBKR-TIREGTID                    
059800     MOVE FOR-TIRODAT            TO ORQM-OBKR-TIRODAT                     
059900                                                                          
060000     MOVE ORQM-OBKR-TIREGDAT     TO WS-AAMMDD-9KOMPL                      
060100     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
060200       MOVE 20                   TO WS-SEKEL-9KOMPL                       
060300     ELSE                                                                 
060400       MOVE 19                   TO WS-SEKEL-9KOMPL                       
060500     END-IF                                                               
060600     COMPUTE ORQM-OBKR-TITIREGD-9KOMPL                                    
060700        = K-9KOMPL - WS-DAT-9KOMPL                                        
060800     END-COMPUTE                                                          
060900                                                                          
061000     MOVE FOR-TITPO              TO ORQM-OBKR-TITPO                       
061100                                                                          
061200     MOVE ORQM-OBKR-TIORDREG     TO WS-AAMMDD-9KOMPL                      
061300     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
061400       MOVE 20                   TO WS-SEKEL-9KOMPL                       
061500     ELSE                                                                 
061600       MOVE 19                   TO WS-SEKEL-9KOMPL                       
061700     END-IF                                                               
061800     COMPUTE ORQM-OBKR-TITIORDD-9KOMPL                                    
061900        = K-9KOMPL - WS-DAT-9KOMPL                                        
062000     END-COMPUTE                                                          
062100                                                                          
062200     MOVE FOR-KDFRAKT            TO ORQM-OBKR-KDFRAKT                     
062300     MOVE FOR-KDORDKL            TO ORQM-OBKR-KDORDKL                     
062400     MOVE FOR-DEAL-PR-LINE       TO ORQM-OBKR-DEAL-PR-LINE                
062500                                                                          
062600     MOVE FOR-KDORDTYP-LDC       TO ORQM-OBKR-KDORDTYP-LDC                
062700     MOVE FOR-TIREPDAT           TO ORQM-OBKR-TIREPDAT                    
062800     MOVE FOR-IDKUNDRF-WIP       TO ORQM-OBKR-IDKUNDRF-WIP                
063000     MOVE ZERO                   TO ORQM-OBKR-TIDLEVDAT                   
063010     MOVE +0                     TO ORQM-OBKR-PRAVCOST                    
063100     .                                                                    
063200                                                                          
063300 S11-RED-OBKR-ERS SECTION.                                                
063400*                                                                         
063500*    REDIGERA ERSÄTTNINGS-SPECIFIK DEL I ORDERBEKR.                       
063600*                                                                         
063700     MOVE FOR-IDARTNR            TO ORQM-OBKR-IDARTNR                     
063800     MOVE FOR-IDDC               TO ORQM-OBKR-IDDC                        
063900     MOVE WS-ERS-KDORDBEK        TO ORQM-OBKR-KDORDBEK                    
064000*                                                                         
064100     MOVE FOR-IDKAMPRF           TO ORQM-OBKR-IDKAMPRF                    
064200     MOVE ZERO                   TO ORQM-OBKR-DIERS-KVOT                  
064300     MOVE ZERO                   TO ORQM-OBKR-IDARTNR-TILLK               
064400     MOVE FOR-KVART              TO ORQM-OBKR-KVBEART                     
064500     MOVE FOR-KVART              TO ORQM-OBKR-KVBEART-Q                   
064600     MOVE ZERO                   TO ORQM-OBKR-KVBEART-TILLK               
064700     MOVE FOR-REKSIFFR           TO ORQM-OBKR-REKSIFFR                    
064800     MOVE ZERO                   TO ORQM-OBKR-REKSIFFR-TILLK              
064900     .                                                                    
065000     EJECT                                                                
065100 S12-RED-OBKR-TIL SECTION.                                                
065200*                                                                         
065300*    REDIGERA TILLKOMMANDE-SPECIFIK DEL I ORDERBEKR.                      
065400*                                                                         
065500     MOVE WS-ERS-IDARTNR         TO ORQM-OBKR-IDARTNR                     
065600     MOVE FOR-IDDC               TO ORQM-OBKR-IDDC                        
065700*                                                                         
065800     IF  FOR-DIERS-ERS > ZERO                                             
065900       COMPUTE ORQM-OBKR-DIERS-KVOT ROUNDED                               
066000         = FOR-DIERS-TILLK                                                
066100         / FOR-DIERS-ERS                                                  
066200       END-COMPUTE                                                        
066300     ELSE                                                                 
066400       MOVE ZERO                 TO ORQM-OBKR-DIERS-KVOT                  
066500     END-IF                                                               
066600     MOVE FOR-IDARTNR            TO ORQM-OBKR-IDARTNR-TILLK               
066700     IF ORQM-OBKR-IDARTNR-TILLK > ZERO                                    
066800       MOVE JA                   TO ORQM-OBKR-FLTILLK                     
066900     END-IF                                                               
067000     MOVE FOR-KVART              TO ORQM-OBKR-KVBEART                     
067100     MOVE FOR-KVART              TO ORQM-OBKR-KVBEART-Q                   
067200     MOVE FOR-KVART              TO ORQM-OBKR-KVBEART-TILLK               
067300     MOVE WS-ERS-REKSIFFR        TO ORQM-OBKR-REKSIFFR                    
067400     MOVE FOR-REKSIFFR           TO ORQM-OBKR-REKSIFFR-TILLK              
067500     .                                                                    
067600     EJECT                                                                
067700 S13-RED-OBKR-LTK SECTION.                                                
067800*                                                                         
067900*    REDIGERA LTK-ÄNDRINGS-SPECIFIK DEL I ORDERBEKR.                      
068000*                                                                         
068100     MOVE FOR-IDARTNR            TO ORQM-OBKR-IDARTNR                     
068200     MOVE FOR-IDDC               TO ORQM-OBKR-IDDC                        
068300*                                                                         
068400     MOVE FOR-IDKAMPRF           TO ORQM-OBKR-IDKAMPRF                    
068500     MOVE ZERO                   TO ORQM-OBKR-DIERS-KVOT                  
068600     MOVE ZERO                   TO ORQM-OBKR-IDARTNR-TILLK               
068700     MOVE FOR-KVART              TO ORQM-OBKR-KVBEART                     
068800     MOVE FOR-KVART              TO ORQM-OBKR-KVBEART-Q                   
068900     MOVE ZERO                   TO ORQM-OBKR-KVBEART-TILLK               
069000     MOVE FOR-REKSIFFR           TO ORQM-OBKR-REKSIFFR                    
069100     MOVE ZERO                   TO ORQM-OBKR-REKSIFFR-TILLK              
069200     .                                                                    
069300     EJECT                                                                
069400 S15-SPARA-ERS SECTION.                                                   
069500*                                                                         
069600*    SPARA FÄLT FRÅN ERS-POST SOM BEHÖVS FÖR KOMMANDE TIL-POSTER          
069700*                                                                         
069800     MOVE FOR-IDARTNR            TO WS-ERS-IDARTNR                        
069900     MOVE FOR-REKSIFFR           TO WS-ERS-REKSIFFR                       
070000                                                                          
070100     MOVE FOR-KDRESTR            TO WS-AKT-KDRESTR                        
070200     PERFORM S16-KONV-RESTR-ORDBEK                                        
070300     MOVE WS-AKT-KDORDBEK        TO WS-ERS-KDORDBEK                       
070400     .                                                                    
070500     EJECT                                                                
070600 S16-KONV-RESTR-ORDBEK SECTION.                                           
070700*                                                                         
070800*    KONVERTERA KDRESTR TILL KDORDBEK.                                    
070900*                                                                         
071000     MOVE WS-AKT-KDRESTR         TO WS-AKT-KDORDBEK                       
071100     .                                                                    
071200     EJECT                                                                
071300*    ---- IMS SEKTIONER                                                   
071400                                                                          
071500 IMS-RESTART      SECTION.                                                
071600                                                                          
071700     MOVE SPACE TO MSG-IO-AREA                                            
071800     MOVE '  ' TO GODK-STATUSKODER                                        
071900     CALL CBLTDLI USING XRST                                              
072000                        MSG-PCB                                           
072100                        MSG-IO-AREA-LENGTH                                
072200                        MSG-IO-AREA                                       
072300                        CHKP-AREA-1-LENGTH                                
072400                        CHKP-AREA-1                                       
072500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072600     PERFORM IMS-STATUSKONTROLL                                           
072700     .                                                                    
072800     SKIP3                                                                
072900 IMS-CHECKPOINT   SECTION.                                                
073000                                                                          
073100     MOVE SPACE TO MSG-IO-AREA                                            
073200     MOVE '  XD' TO GODK-STATUSKODER                                      
073300     CALL CBLTDLI USING CHKP                                              
073400                        MSG-PCB                                           
073500                        MSG-IO-AREA-LENGTH                                
073600                        MSG-IO-AREA                                       
073700                        CHKP-AREA-1-LENGTH                                
073800                        CHKP-AREA-1                                       
073900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074000     PERFORM IMS-STATUSKONTROLL                                           
074100                                                                          
074200     IF  IMS-EJ-OK                                                        
074300       DISPLAY 'IMS-KONTROLLREGIONEN EJ TILLGÄNGLIG'                      
074400       CALL FELLOG                                                        
074500     END-IF                                                               
074600     .                                                                    
074700     EJECT                                                                
074800 IMS-ISRT-ORQM-OBKR SECTION.                                              
074900                                                                          
075000     MOVE 'WLORQM01 ' TO SSA1                                             
075100     MOVE '  ' TO GODK-STATUSKODER                                        
075200     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA SSA1                    
075300     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
075400     PERFORM IMS-STATUSKONTROLL                                           
075500     .                                                                    
075600     SKIP3                                                                
075700 IMS-GU-ORQM-OBKR SECTION.                                                
075800                                                                          
075900     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-F-X                          
076000                    '&WDQ101KY <' W-WDQ101KY-T-X ')'                      
076100            DELIMITED BY SIZE INTO SSA1                                   
076200     MOVE '  GE' TO GODK-STATUSKODER                                      
076300     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA SSA1                      
076400     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
076500     PERFORM IMS-STATUSKONTROLL                                           
076600     .                                                                    
076700     SKIP3                                                                
076800 IMS-GN-ORQM-OBKR SECTION.                                                
076900                                                                          
077000     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-F-X                          
077100                    '&WDQ101KY <' W-WDQ101KY-T-X ')'                      
077200            DELIMITED BY SIZE INTO SSA1                                   
077300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
077400     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-AREA SSA1                      
077500     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
077600     PERFORM IMS-STATUSKONTROLL                                           
077700     .                                                                    
077800     EJECT                                                                
077900 IMS-GHU-XXLO-4556 SECTION.                                               
078000                                                                          
078100     STRING 'WLXXLO01(WDGXKEY  =' W-IDHTYP-4555-X ')'                     
078200            DELIMITED BY SIZE INTO SSA1                                   
078300     STRING 'WLXXLO11(KDSEGKEY =' '1' ')'                                 
078400            DELIMITED BY SIZE INTO SSA2                                   
078500     MOVE '  GE' TO GODK-STATUSKODER                                      
078600     CALL CBLTDLI USING GHU  XXLO-PCB DLI-IO-AREA SSA1 SSA2               
078700     MOVE XXLO-STATUS-CODE TO STATUS-WS                                   
078800     PERFORM IMS-STATUSKONTROLL                                           
078900     .                                                                    
079000     SKIP3                                                                
079100 IMS-REPL-XXLO SECTION.                                                   
079200                                                                          
079300     MOVE '  ' TO GODK-STATUSKODER                                        
079400     CALL CBLTDLI USING REPL XXLO-PCB DLI-IO-AREA                         
079500     MOVE XXLO-STATUS-CODE TO STATUS-WS                                   
079600     PERFORM IMS-STATUSKONTROLL                                           
079700     .                                                                    
079800     EJECT                                                                
079900 IMS-DLET-XXLO SECTION.                                                   
080000                                                                          
080100     MOVE '  ' TO GODK-STATUSKODER                                        
080200     CALL CBLTDLI USING DLET XXLO-PCB DLI-IO-AREA                         
080300     MOVE XXLO-STATUS-CODE TO STATUS-WS                                   
080400     PERFORM IMS-STATUSKONTROLL                                           
080500     .                                                                    
080600     SKIP3                                                                
080700 IMS-ISRT-XXLO-4556 SECTION.                                              
080800                                                                          
080900     STRING 'WLXXLO01(WDGXKEY  =' W-IDHTYP-4555-X ')'                     
081000            DELIMITED BY SIZE INTO SSA1                                   
081100     MOVE 'WLXXLO11 ' TO SSA2                                             
081200     MOVE '  ' TO GODK-STATUSKODER                                        
081300     CALL CBLTDLI USING ISRT XXLO-PCB DLI-IO-AREA SSA1 SSA2               
081400     MOVE XXLO-STATUS-CODE TO STATUS-WS                                   
081500     PERFORM IMS-STATUSKONTROLL                                           
081600     .                                                                    
081700     EJECT                                                                
081800 IMS-STATUSKONTROLL SECTION.                                              
081900                                                                          
082000     SET STATUS-IX TO 1                                                   
082100     SEARCH GODK-STATUS                                                   
082200       AT END                                                             
082300         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
082400           DELIMITED BY SIZE INTO FELTEXT                                 
082500         CALL FELLOG                                                      
082600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
082700         CONTINUE                                                         
082800     END-SEARCH.                                                          
