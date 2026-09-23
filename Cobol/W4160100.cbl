000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4160100.                                                
000400 AUTHOR.         GUNILLA JOHANSSON.                                       
000500 DATE-WRITTEN.   90/11/08.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET SKAPAR SATSORDERHUVUD OCH SATSORDERRADER              
001100*        UTGÅNGSPUNKT ÄR HÄNDELSEBAS FRÅN ANSKAFFNINGEN                   
001200*        CHECKPOINT ANVÄNDS I PROGRAMMET KRING VARJE 10 ORDER             
001300*        ÅTERRAPPORTERING TILL ANSKAFFNINGEN VIA HÄNDELSEBAS              
001400*                                                                         
001500*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001600*        PROGRAMMET LÄSER      WLSATB (WDJ1)                              
001700*        PROGRAMMET UPPDATERAR WLSATG (WDJ2)                              
001800*        PROGRAMMET UPPDATERAR WLXXKP (WDR1)                              
001900*        PROGRAMMET UPPDATERAR WLXXCN (WDR5)                              
002000*        PROGRAMMET UPPDATERAR WLXXCO (WDR5)                              
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600*    RÄTTELSER OCH KOMPLETTERINGAR:                                       
002700*        TA HAND OM FELAKTIGA BEFT, GENOM ATT                             
002800*        EJ BEHANDLA DEM.                                                 
002900*        EFFEKTIVARE DATABASLÄSNINGAR MOT WDR5.                           
003000*        CHECKPOINT EFTER 10 SATSORDER ISTF 1.                            
003100*        GJN 920321.                                                      
003200*        I SAMBAND MED SPLIT ÄNDRAS KONTROLL AV                           
003300*        ANVÄNT ORDERNR.                                                  
003400*        PRC-TABELLER, KONTO OCH KOSTNADSSTÄLLE                           
003500*        ÄR OFÖRÄNDRADE FÖR PV.                                           
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     SKIP2                                                                
003900 INPUT-OUTPUT SECTION.                                                    
004000                                                                          
004100 FILE-CONTROL.                                                            
004200     SKIP3                                                                
004300 DATA DIVISION.                                                           
004400     SKIP3                                                                
004500 FILE SECTION.                                                            
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800     SKIP2                                                                
004900*    -COPY WY2000W1                                                       
005000     SKIP3                                                                
005100 77  IDPGM                       PIC X(8)    VALUE 'W4160100'.            
005200 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
005300 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
005400 77  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005500 77  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800     SKIP3                                                                
005900*    --- DIVERSE ARBETSVARIABLER                                          
006000 77  WS-EN-RAD-FUNNEN-FLAGGA     PIC X       VALUE 'N'.                   
006100 77  WS-ORDER-FUNNEN-FLAGGA      PIC X       VALUE 'J'.                   
006200 77  WS-ORDERNR-OK-FLAGGA        PIC X       VALUE 'N'.                   
006300 77  WS-VKART                PIC S9(7)      COMP-3  VALUE ZERO.           
006400 77  WS-VLARTNTO             PIC S9(8)V9(1) COMP-3  VALUE ZERO.           
006500 77  WS-VLORDNTO-CM          PIC S9(8)V9(1) COMP-3  VALUE ZERO.           
006600 77  WS-VLORDNTO-M           PIC S9(4)V9(3) COMP-3  VALUE ZERO.           
006700 77  WS-VKORDNTO             PIC S9(6)V9(1) COMP-3  VALUE ZERO.           
006800 77  WS-MAX-SUFFIX           PIC S9(1)      COMP-3  VALUE +9.             
006900 77  WS-SUFFIX               PIC S9(3)      COMP-3  VALUE ZERO.           
007000 77  RAKNARE                     PIC 9(4)   COMP-3.                       
007100*                                                                         
007200 01  WS-IDKONTO                  PIC 9(10).                               
007300     SKIP2                                                                
007400 01  FILLER REDEFINES WS-IDKONTO.                                         
007500     03 WS-IDKONTO-DEL1          PIC 9(4).                                
007600     03 WS-IDKONTO-DEL2          PIC 9(4).                                
007700     03 WS-IDKONTO-DEL3          PIC 9(2).                                
007800     SKIP3                                                                
007900                                                                          
008000 01  DAGENS-DATUM.                                                        
008100     03  DAGENS-DATUM-AAR        PIC X(2)    VALUE SPACE.                 
008200     03  DAGENS-DATUM-MAANAD     PIC X(2)    VALUE SPACE.                 
008300     03  DAGENS-DATUM-DAG        PIC X(2)    VALUE SPACE.                 
008400     SKIP3                                                                
008500 01  DAGENS-DATUM-NUM REDEFINES DAGENS-DATUM PIC 9(6).                    
008600     SKIP3                                                                
008700 01  DAGENS-DATUM-Y2K            PIC 9(8).                                
008800     SKIP3                                                                
008900 01  FILLER                      PIC X(10)   VALUE 'SWITCHAR  '.          
009000 01  SWITCHAR.                                                            
009100     03 RAETT-BEFT-SW            PIC X       VALUE 'J'.                   
009200     SKIP3                                                                
009300 01  WS-TIBEHOV                  PIC 9(4).                                
009400     SKIP2                                                                
009500 01  FILLER REDEFINES WS-TIBEHOV.                                         
009600     03  WS-DAT-TIAA-VECKA       PIC 9(2).                                
009700     03  WS-DAT-TIVV             PIC 9(2).                                
009800     EJECT                                                                
009900 01  FILLER                      PIC X(10)   VALUE 'PRC-TABELL'.          
010000     SKIP2                                                                
010100 01  WS-TAB-BEFT-PRC.                                                     
010200     03  FILLER          PIC X(03) VALUE '009'.                           
010300     03  FILLER          PIC X(03) VALUE '079'.                           
010400     03  FILLER          PIC X(03) VALUE '15T'.                           
010500     03  FILLER          PIC X(03) VALUE '100'.                           
010610     03  FILLER          PIC X(03) VALUE '11K'.                           
010700     03  FILLER          PIC X(03) VALUE '120'.                           
010800     03  FILLER          PIC X(03) VALUE '139'.                           
010900     03  FILLER          PIC X(03) VALUE '14E'.                           
011000     03  FILLER          PIC X(03) VALUE '16Q'.                           
011110     03  FILLER          PIC X(03) VALUE '17P'.                           
011200     03  FILLER          PIC X(03) VALUE '181'.                           
011300     03  FILLER          PIC X(03) VALUE '19F'.                           
011310     03  FILLER          PIC X(03) VALUE '90R'.                           
011400     03  FILLER          PIC X(03) VALUE '939'.                           
011500     03  FILLER          PIC X(03) VALUE '96R'.                           
011600     03  FILLER          PIC X(03) VALUE '979'.                           
011700     03  FILLER          PIC X(03) VALUE '999'.                           
011800     SKIP2                                                                
011900 01  FILLER REDEFINES WS-TAB-BEFT-PRC.                                    
012000     03  WS-TAB-RAD-BEFT-PRC OCCURS 17                                    
012100                         INDEXED BY TAB-IDX.                              
012200         05  WS-TAB-RAD-BEFT     PIC 9(2).                                
012300         05  WS-TAB-RAD-PRCVAR   PIC X.                                   
012400     EJECT                                                                
012500 01  DYNAMISKA-SUBPROGRAM.                                                
012600*                                                                         
012700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013100     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
013200     SKIP2                                                                
013300 01  ABEND-KODER.                                                         
013400     03  RKOD-ABEND-MED-DUMP  PIC S9(4) COMP SYNC VALUE +1000.            
013500     03  RKOD-ABEND-UTAN-DUMP PIC S9(4) COMP SYNC VALUE   +16.            
013600     EJECT                                                                
013700*    --- PARAMETRAR TILL SUBPROGRAM                                       
013800 01  FILLER                   PIC X(16) VALUE 'DATUMKONV-START'.          
013900*01  -COPY WDATAREA                                                       
014000     EJECT                                                                
014100 01  FILLER                   PIC X(16) VALUE 'W411ORDNR-START'.          
014200*01  -COPY W411ORDN                                                       
014300     EJECT                                                                
014400 01  TEST-IDDISTR        PIC 9(5)    COMP-3.                              
014500*01  FILLER  -COPY WWDIST19  -RED TEST-IDDISTR                            
014600     EJECT                                                                
014700*    --- ARBETSAREA FÖR SATSORDERHUVUD                                    
014800     SKIP2                                                                
014900 01  WS-SHUV.                                                             
015000*    03  WLSATG01  -COPY WDJ201  -PRE WS-                                 
015100     EJECT                                                                
015200*    --- ARBETSAREA FÖR SATSORDERRAD                                      
015300     SKIP2                                                                
015400 01  WS-SRAD.                                                             
015500*    03  WLSATG11  -COPY WDJ211  -PRE WS-                                 
015600     EJECT                                                                
015700*    --- ARBETSAREOR FÖR IMS-SEKTIONERNA                                  
015800     SKIP2                                                                
015900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016000     SKIP3                                                                
016100 01  NYCKLAR-TILL-DLI.                                                    
016200     03  W-ARTC01-IDARTNR-X.                                              
016300         05  W-ARTC01-IDARTNR    PIC S9(9)   VALUE ZERO COMP-3.           
016400*                                                                         
016500     03  W-STR-IDARTNR-X.                                                 
016600         05  W-STR-IDARTNR       PIC S9(9)   VALUE ZERO COMP-3.           
016700*                                                                         
016800     03  W-RAD-WDJ111KY-X.                                                
016900         05  W-RAD-KDSTRRAD      PIC X(1)    VALUE SPACE.                 
017000         05  W-RAD-IDRADNR       PIC S9(5)   VALUE ZERO COMP-3.           
017100*                                                                         
017200     03  W-SHUV-IDORDNST-X.                                               
017300         05  W-SHUV-IDORDNSB     PIC S9(5)   VALUE ZERO COMP-3.           
017400         05  W-SHUV-IDORDNSS     PIC S9(1)   VALUE ZERO COMP-3.           
017500*                                                                         
017600     03  W-SRAD-IDARTNR-X.                                                
017700         05  W-SRAD-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.           
017800*                                                                         
017900     03  W-ART-IDARTNR-X.                                                 
018000         05  W-ART-IDARTNR       PIC S9(9)   VALUE ZERO COMP-3.           
018100*                                                                         
018200     03  W-WDGXKEY-4525-X.                                                
018300         05  FILLER              PIC X(4)    VALUE '4525'.                
018400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
018500*                                                                         
018600     03  W-WDGXKEY-4526-X.                                                
018700         05  FILLER              PIC X(1)    VALUE '1'.                   
018800         05  FILLER              PIC X(4)    VALUE LOW-VALUE.             
018900*                                                                         
019000     03  W-WDGXKEY-2235-X.                                                
019100         05  FILLER              PIC X(4)    VALUE '2235'.                
019200         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
019300*                                                                         
019400     03  W-WDGXKEY-2236-X.                                                
019500         05  W-2236-IDARTNR      PIC S9(9)   COMP-3.                      
019600         05  W-2236-TIBEHOV      PIC S9(5)   COMP-3.                      
019700         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
019800*                                                                         
019900     03  W-WDGXKEY-2237-X.                                                
020000         05  FILLER              PIC X(4)    VALUE '2237'.                
020100         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
020200*                                                                         
020300     03  W-WDGXKEY-2238-X.                                                
020400         05  W-2238-IDARTNR      PIC S9(9)   COMP-3.                      
020500         05  W-2238-TIBEHOV      PIC S9(5)   COMP-3.                      
020600         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
020700     EJECT                                                                
020800*    --- STATUS-KODER FRÅN IMS                                            
020900 01  STATUS-WS                   PIC XX.                                  
021000     88  SEGMENT-FINNS                       VALUE '  '.                  
021100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
021400     88  IMS-EJ-OK                           VALUE 'XD'.                  
021500     SKIP2                                                                
021600 01  GODK-STATUSKODER.                                                    
021700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021800     SKIP3                                                                
021900 01  SSA1                        PIC X(64).                               
022000 01  SSA2                        PIC X(64).                               
022100 01  SSA3                        PIC X(64).                               
022200     EJECT                                                                
022300*    --- IMS FUNKTIONSKODER                                               
022400*01  -COPY W0003                                                          
022500     EJECT                                                                
022600*    ---  DLI INPUT-OUTPUT AREA 1                                         
022700*    ---  DLI-IO-AREA                                                     
022800     SKIP2                                                                
022900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC01'.            
023000 01  DLI-IO-ARTC01.                                                       
023100*    03 WLARTC01  -COPY WDK601                                            
023200     EJECT                                                                
023300                                                                          
023400 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC11'.            
023500 01  DLI-IO-ARTC11.                                                       
023600*    05 WLARTC11  -COPY WDK611                                            
023700     EJECT                                                                
023800                                                                          
023900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-SATB01'.            
024000 01  DLI-IO-SATB01.                                                       
024100*    03 WLSATB01  -COPY WDJ101       -PRE SATB01-                         
024200     EJECT                                                                
024300                                                                          
024400 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-SATB11'.            
024500 01  DLI-IO-SATB11.                                                       
024600*    03 WLSATB11  -COPY WDJ111       -PRE SATB11-                         
024700     EJECT                                                                
024800                                                                          
024900 01  FILLER                PIC X(16)   VALUE 'DLI-IO-SATG01'.             
025000 01  DLI-IO-SATG01.                                                       
025100*    03 WLSATG01  -COPY WDJ201      -PRE SATG01-                          
025200     EJECT                                                                
025300                                                                          
025400 01  FILLER                PIC X(16)   VALUE 'DLI-IO-SATG11'.             
025500 01  DLI-IO-SATG11.                                                       
025600*    03 WLSATG11  -COPY WDJ211      -PRE SATG11-                          
025700     EJECT                                                                
025800                                                                          
025900 01  FILLER                PIC X(16)   VALUE 'DLI-IO-XXCN11'.             
026000 01  DLI-IO-XXCN11.                                                       
026100*    03 WLXXCN11  -COPY WDGX2236    -PRE XXCN11-                          
026200     EJECT                                                                
026300                                                                          
026400 01  FILLER                PIC X(16)   VALUE 'DLI-IO-XXCO11'.             
026500 01  DLI-IO-XXCO11.                                                       
026600*    03 WLXXCO11  -COPY WDGX2238    -PRE XXCO11-                          
026700     EJECT                                                                
026800 LINKAGE SECTION.                                                         
026900     SKIP2                                                                
027000*01       -COPY W0009      -PRE MSG-.                                     
027100     EJECT                                                                
027200*01       -COPY W0008      -PRE ARTC-.                                    
027300     05  FILLER                  PIC X.                                   
027400     EJECT                                                                
027500*01       -COPY W0008      -PRE SATB-.                                    
027600     05  FILLER                  PIC X.                                   
027700     EJECT                                                                
027800 01       -COPY W0008      -PRE SATG-.                                    
027900     05  FILLER                  PIC X.                                   
028000     EJECT                                                                
028100*01       -COPY W0008      -PRE XXKP-.                                    
028200     05  FILLER                  PIC X.                                   
028300     EJECT                                                                
028400*01       -COPY W0008      -PRE XXCN-.                                    
028500     05  FILLER                  PIC X.                                   
028600     EJECT                                                                
028700*01       -COPY W0008      -PRE XXCO-.                                    
028800     05  FILLER                  PIC X.                                   
028900     EJECT                                                                
029000 PROCEDURE DIVISION  USING MSG-PCB  ARTC-PCB SATB-PCB                     
029100                           SATG-PCB XXKP-PCB XXCN-PCB                     
029200                           XXCO-PCB.                                      
029300                                                                          
029400     ENTRY 'DLITCBL' USING MSG-PCB  ARTC-PCB SATB-PCB                     
029500                           SATG-PCB XXKP-PCB XXCN-PCB                     
029600                           XXCO-PCB.                                      
029700                                                                          
029800     PERFORM A-INIT                                                       
029900     PERFORM IMS-GU-XXCN01                                                
030000     IF SEGMENT-FINNS                                                     
030100       PERFORM IMS-GHNP-XXCN11                                            
030200       PERFORM UNTIL SEGMENT-SAKNAS                                       
030300************* FIX 971106 BL                                               
030400         IF XXCN11-2236-IDARTNR = 030813054                               
030500         OR XXCN11-2236-IDARTNR = 000272383                               
030600*FIX SK 011207                                                            
030700*****    OR XXCN11-2236-IDARTNR = 000274303                               
030800*        OR XXCN11-2236-IDARTNR = 030755197                               
031000            CONTINUE                                                      
031100**************************                                                
031200         ELSE                                                             
031300            PERFORM B-SKAPA-ORDERHUVUD                                    
031400            IF RAETT-BEFT-SW = JA                                         
031500               PERFORM C-SKAPA-ORDERRAD                                   
031600               PERFORM D-SKAPA-TRANS-T-ANSK                               
031700               PERFORM E-TABORT-TRANS-FR-ANSK                             
031800               ADD 1 TO RAKNARE                                           
031900               IF RAKNARE > 10                                            
032000                 MOVE ZERO TO RAKNARE                                     
032100                 PERFORM IMS-CHECKPOINT                                   
032200                 PERFORM IMS-GU-XXCN01                                    
032300               END-IF                                                     
032400            END-IF                                                        
032500         END-IF                                                           
032600         IF SEGMENT-FINNS                                                 
032700            PERFORM IMS-GHNP-XXCN11                                       
032800         END-IF                                                           
032900       END-PERFORM                                                        
033000     END-IF                                                               
033100                                                                          
033200     MOVE ZERO TO RETURN-CODE                                             
033300                                                                          
033400     GOBACK                                                               
033500     .                                                                    
033600     EJECT                                                                
033700 A-INIT SECTION.                                                          
033800                                                                          
033900     PERFORM IMS-RESTART                                                  
034000                                                                          
034100     ACCEPT DAGENS-DATUM FROM DATE                                        
034200     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
034300     MOVE 1 TO RAKNARE                                                    
034400     .                                                                    
034500     EJECT                                                                
034600 B-SKAPA-ORDERHUVUD SECTION.                                              
034700                                                                          
034800     PERFORM S07-NOLLST-ORDERHUVUD                                        
034900                                                                          
035000     PERFORM BA-HAEMTA-FRAN-WDGX2236                                      
035100     PERFORM BB-KONTROLLERA-STRUKTUR                                      
035200     PERFORM BD-LAES-HAEMTA-FRAN-WDK6                                     
035300     PERFORM BF-FINN-PRC                                                  
035400                                                                          
035500     IF RAETT-BEFT-SW = JA                                                
035600        PERFORM BG-TAG-UT-ORDERNR                                         
035700        PERFORM BH-FASTA-VARDEN                                           
035800        PERFORM BI-TAG-UT-KONTO-KST                                       
035900                                                                          
036000        MOVE    WS-SHUV-WDJ201 TO SATG01-SHUV-WDJ201                      
036100        PERFORM IMS-ISRT-SATG01                                           
036200     END-IF                                                               
036300     .                                                                    
036400     EJECT                                                                
036500 BA-HAEMTA-FRAN-WDGX2236 SECTION.                                         
036600                                                                          
036700     MOVE XXCN11-2236-TIBEHOV    TO DAT-I-TIDATUM                         
036800     MOVE 'AAVV'                 TO DAT-KDDATFORM                         
036900                                                                          
037000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
037100                         DAT-O-TIDATUM DAT-KDSVAR                         
037200     END-CALL                                                             
037300                                                                          
037400     IF DAT-KDSVAR-OK                                                     
037500        MOVE DAT-TIAAMMDD        TO WS-SHUV-TIBEGPAC                      
037600     ELSE                                                                 
037700        DISPLAY '*** W41601 - FEL I DATKONV ***'                          
037800        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
037900     END-IF                                                               
038000                                                                          
038100                                                                          
038200     MOVE XXCN11-2236-IDARTNR    TO WS-SHUV-IDARTNR                       
038300     MOVE XXCN11-2236-IDDISTR    TO WS-SHUV-IDDISTR                       
038400     MOVE XXCN11-2236-IDANSK     TO WS-SHUV-IDANSK                        
038500     MOVE XXCN11-2236-KVBEART    TO WS-SHUV-KVBEART                       
038600     MOVE XXCN11-2236-IDLEVNR    TO WS-SHUV-IDLEVNR                       
038700     MOVE XXCN11-2236-IDDISTR    TO TEST-IDDISTR                          
038800     .                                                                    
038900     EJECT                                                                
039000 BB-KONTROLLERA-STRUKTUR SECTION.                                         
039100                                                                          
039200*    --- ALLA SATSER SKALL VARA AKTUELLA. AKTUELL SATS HAR                
039300*    --- TIBORT = +0                                                      
039400     MOVE WS-SHUV-IDARTNR   TO W-STR-IDARTNR                              
039500     PERFORM IMS-GET-SATB01                                               
039600     IF SATB01-STR-TIBORT = +0                                            
039700       MOVE DAGENS-DATUM-NUM TO WS-SHUV-TIUPPDAT                          
039800     ELSE                                                                 
039900       DISPLAY 'SATSNR =' SATB01-STR-IDARTNR                              
040000       DISPLAY '*** W41601 - EJ AKTUELL SATS *'                           
040100       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
040200     END-IF                                                               
040300     .                                                                    
040400     EJECT                                                                
040500 BD-LAES-HAEMTA-FRAN-WDK6 SECTION.                                        
040600                                                                          
040700     MOVE    WS-SHUV-IDARTNR TO W-ARTC01-IDARTNR                          
040800     PERFORM IMS-GET-ARTC01                                               
040900     MOVE    ART-REKSIFFR    TO WS-SHUV-REKSIFFR                          
041000     MOVE    ART-KDPRODSL    TO WS-SHUV-KDPRODSL                          
041100                                                                          
041200     PERFORM IMS-GNP-ARTC11                                               
041300     MOVE    CLAG-PRARTSTD   TO WS-SHUV-PRARTSTD                          
041400     MOVE    CLAG-BEFT       TO WS-SHUV-BEFT                              
041500     .                                                                    
041600     EJECT                                                                
041700 BF-FINN-PRC SECTION.                                                     
041800                                                                          
041900*    --- PRC 9980 TILL 998Z ÄR RESERVERADE FÖR SATSER                     
042000*    --- PRODUKTIONSKANALEN STYRS AV FÖRPACKNINGSTYPEN                    
042100                                                                          
042200     SET TAB-IDX TO +1                                                    
042300     PERFORM UNTIL                                                        
042400        TAB-IDX        > 17  OR                                           
042500        WS-SHUV-BEFT   = WS-TAB-RAD-BEFT (TAB-IDX)                        
042600        SET TAB-IDX UP BY +1                                              
042700     END-PERFORM                                                          
042800                                                                          
042900     IF TAB-IDX < +18                                                     
043000        IF WS-SHUV-BEFT   = WS-TAB-RAD-BEFT (TAB-IDX)                     
043100           MOVE WS-TAB-RAD-PRCVAR (TAB-IDX) TO WS-SHUV-IDPRCVAR           
043200           MOVE '998' TO WS-SHUV-IDPRCBAS                                 
043300        ELSE                                                              
043400*          DISPLAY '*** W41601 - FEL BEFT/PTC  ***'                       
043500*          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
043600           MOVE NEJ TO RAETT-BEFT-SW                                      
043700        END-IF                                                            
043800     ELSE                                                                 
043900*       DISPLAY '*** W41601 - FEL BEFT/PTC  ***'                          
044000*       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
044100        MOVE NEJ TO RAETT-BEFT-SW                                         
044200     END-IF                                                               
044300     .                                                                    
044400     EJECT                                                                
044500 BG-TAG-UT-ORDERNR SECTION.                                               
044600                                                                          
044700*    --- KONTROLLERA ATT INTE IDORDNSB ÄR I BRUK I WDJ201.                
044800*    --- BASEN LÄSES MED NYCKEL LIKA MED IDORDNST, DVS                    
044900*    --- IDORDNSB + SUFFIX (0-9)                                          
045000*                                                                         
045100     MOVE NEJ  TO WS-ORDER-FUNNEN-FLAGGA                                  
045200     MOVE NEJ  TO WS-ORDERNR-OK-FLAGGA                                    
045300     PERFORM S01-CALL-W411ORDN                                            
045400     MOVE +0          TO WS-SUFFIX                                        
045500     MOVE WS-SUFFIX   TO W-SHUV-IDORDNSS                                  
045600     PERFORM IMS-GET-SATG01                                               
045700                                                                          
045800     PERFORM UNTIL WS-ORDERNR-OK-FLAGGA = JA                              
045900        PERFORM UNTIL (WS-SUFFIX > WS-MAX-SUFFIX) OR                      
046000                WS-ORDER-FUNNEN-FLAGGA = JA                               
046100           IF SEGMENT-FINNS                                               
046200              MOVE JA TO WS-ORDER-FUNNEN-FLAGGA                           
046300           ELSE                                                           
046400              ADD +1 TO WS-SUFFIX                                         
046500              IF WS-SUFFIX < 10                                           
046600                 MOVE WS-SUFFIX TO W-SHUV-IDORDNSS                        
046700                 PERFORM IMS-GET-SATG01                                   
046800              END-IF                                                      
046900           END-IF                                                         
047000        END-PERFORM                                                       
047100                                                                          
047200        IF WS-ORDER-FUNNEN-FLAGGA = NEJ                                   
047300           MOVE ORDN-IDORDNSB-UT TO WS-SHUV-IDORDNSB                      
047400           MOVE +0               TO WS-SHUV-IDORDNSS                      
047500           MOVE JA               TO WS-ORDERNR-OK-FLAGGA                  
047600        ELSE                                                              
047700           PERFORM S01-CALL-W411ORDN                                      
047800           MOVE +0          TO WS-SUFFIX                                  
047900           MOVE WS-SUFFIX   TO W-SHUV-IDORDNSS                            
048000           PERFORM IMS-GET-SATG01                                         
048100           MOVE NEJ         TO WS-ORDER-FUNNEN-FLAGGA                     
048200        END-IF                                                            
048300     END-PERFORM                                                          
048400     .                                                                    
048500     EJECT                                                                
048600 BH-FASTA-VARDEN SECTION.                                                 
048700                                                                          
048800     MOVE NEJ    TO WS-SHUV-FLBYGGB                                       
048900     MOVE NEJ    TO WS-SHUV-FLSATNOL                                      
049000     MOVE NEJ    TO WS-SHUV-FLSATSPR                                      
049100     MOVE JA     TO WS-SHUV-FLSATNYO                                      
049200     MOVE +0     TO WS-SHUV-IDKUNDNR                                      
049300     MOVE SPACE  TO WS-SHUV-IDUSER                                        
049400     MOVE ZERO   TO WS-SHUV-IDPRODNR                                      
049500     MOVE 1      TO WS-SHUV-KDCLAGER                                      
049600     MOVE 'N'    TO WS-SHUV-KDFAKTYP                                      
049700     MOVE +5     TO WS-SHUV-KDORDKL                                       
049800     MOVE 'A'    TO WS-SHUV-KDSATKMB                                      
049900     MOVE SPACE  TO WS-SHUV-KDSATPLK                                      
050000     MOVE 'R'    TO WS-SHUV-KDSATSTA                                      
050100     MOVE +0     TO WS-SHUV-KVBYGGB                                       
050200     MOVE +0     TO WS-SHUV-SUSATPTI                                      
050300     MOVE DAGENS-DATUM-Y2K TO WS-SHUV-DAREGDAT                            
050400     MOVE +0     TO WS-SHUV-VKORDNTO                                      
050500     MOVE +0     TO WS-SHUV-VLORDNTO                                      
050600     MOVE +0     TO WS-SHUV-KVRORAD-9KOMPL                                
050700     MOVE +0     TO WS-SHUV-RELSKVOT-L                                    
050800     MOVE NEJ    TO WS-SHUV-FLSATPRI                                      
050900     MOVE +0     TO WS-SHUV-RELSKVOT-S                                    
051000     .                                                                    
051100     EJECT                                                                
051200 BI-TAG-UT-KONTO-KST SECTION.                                             
051300*                                                                         
051400*    ANALYSNUMRET LÄGGS I IDKONTO                                         
051500*                                                                         
051600     IF DIST19-SATS                                                       
051700        MOVE   5780 TO WS-IDKONTO-DEL1                                    
051800        MOVE +57000 TO WS-SHUV-IDKST                                      
051900     ELSE                                                                 
052000        DISPLAY '*** W41601 - FEL DISTRIKT  ***'                          
052100        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
052200     END-IF                                                               
052300                                                                          
052400     MOVE WS-SHUV-IDORDNSB TO WS-IDKONTO-DEL2                             
052500     MOVE 00               TO WS-IDKONTO-DEL3                             
052600     MOVE WS-IDKONTO       TO WS-SHUV-IDKONTO                             
052700     .                                                                    
052800     EJECT                                                                
052900 C-SKAPA-ORDERRAD SECTION.                                                
053000                                                                          
053100     MOVE NEJ    TO WS-EN-RAD-FUNNEN-FLAGGA                               
053200     PERFORM S08-NOLLST-ORDERRAD                                          
053300     PERFORM IMS-GET-SATB01                                               
053400     PERFORM IMS-GNP-FIRST-SATB11                                         
053500                                                                          
053600     IF SEGMENT-FINNS                                                     
053700        PERFORM UNTIL WS-EN-RAD-FUNNEN-FLAGGA = JA                        
053800        OR SEGMENT-SAKNAS                                                 
053900           MOVE SATB11-RAD-TISTODAT   TO TMP1-YYMMDD                      
054000           MOVE DAGENS-DATUM-NUM      TO TMP2-YYMMDD                      
054100           MOVE SATB11-RAD-TISTADAT   TO TMP3-YYMMDD                      
054200           PERFORM WY2000Q1                                               
054300           IF  TMP3-YYMMDD <= TMP2-YYMMDD                                 
054400           AND TMP1-YYMMDD >  TMP2-YYMMDD                                 
054500              MOVE JA TO WS-EN-RAD-FUNNEN-FLAGGA                          
054600              PERFORM CA-HAEMTA-FRAN-WDJ1-SHUV                            
054700              PERFORM CB-HAEMTA-FRAN-WDK6                                 
054800              PERFORM CC-FASTA-VARDEN                                     
054900              PERFORM CD-SKRIV-FORSTA-RAD                                 
055000           END-IF                                                         
055100           PERFORM IMS-GNP-SATB11                                         
055200        END-PERFORM                                                       
055300     END-IF                                                               
055400                                                                          
055500     IF WS-EN-RAD-FUNNEN-FLAGGA = JA                                      
055600        PERFORM S08-NOLLST-ORDERRAD                                       
055700        PERFORM UNTIL SEGMENT-SAKNAS                                      
055800           MOVE SATB11-RAD-TISTODAT   TO TMP1-YYMMDD                      
055900           MOVE DAGENS-DATUM-NUM      TO TMP2-YYMMDD                      
056000           MOVE SATB11-RAD-TISTADAT   TO TMP3-YYMMDD                      
056100           PERFORM WY2000Q1                                               
056200           IF  TMP3-YYMMDD <= TMP2-YYMMDD                                 
056300           AND TMP1-YYMMDD >  TMP2-YYMMDD                                 
056400              PERFORM CA-HAEMTA-FRAN-WDJ1-SHUV                            
056500              PERFORM CB-HAEMTA-FRAN-WDK6                                 
056600              PERFORM CC-FASTA-VARDEN                                     
056700              PERFORM CE-SKRIV-RAD                                        
056800              IF SEGMENT-FINNS-REDAN                                      
056900                 PERFORM CF-SUMMERA-OCH-SKRIV-RAD                         
057000              END-IF                                                      
057100           END-IF                                                         
057200           PERFORM S08-NOLLST-ORDERRAD                                    
057300           PERFORM IMS-GNP-SATB11                                         
057400        END-PERFORM                                                       
057500     ELSE                                                                 
057600        DISPLAY '** W41601 - INGA ORDERRADER **'                          
057700        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
057800     END-IF                                                               
057900     EJECT                                                                
058000     .                                                                    
058100 CA-HAEMTA-FRAN-WDJ1-SHUV SECTION.                                        
058200                                                                          
058300     MOVE SATB11-RAD-IDARTNR  TO WS-SRAD-IDARTNR                          
058400     MOVE SATB11-RAD-REANTPSA TO WS-SRAD-REANTPSA                         
058500     MOVE SATB11-RAD-KDSTRRAD TO WS-SRAD-KDSTRRAD                         
058600     MOVE WS-SHUV-IDKONTO     TO WS-SRAD-IDKONTO                          
058700     MOVE WS-SHUV-IDKST       TO WS-SRAD-IDKST                            
058800     .                                                                    
058900     EJECT                                                                
059000 CB-HAEMTA-FRAN-WDK6 SECTION.                                             
059100                                                                          
059200     MOVE    WS-SRAD-IDARTNR  TO W-ARTC01-IDARTNR                         
059300     PERFORM IMS-GET-ARTC01                                               
059400                                                                          
059500     MOVE    ART-REKSIFFR     TO WS-SRAD-REKSIFFR                         
059600     MOVE    ART-KDSORT       TO WS-SRAD-KDSORT                           
059700     MOVE    ART-KDPRODSL     TO WS-SRAD-KDPRODSL                         
059800                                                                          
059900     PERFORM IMS-GNP-ARTC11                                               
060000     MOVE    CLAG-VKART       TO WS-VKART                                 
060100     MOVE    CLAG-VLARTNTO    TO WS-SRAD-VLARTNTO                         
060200                                                                          
060300     COMPUTE WS-SRAD-VKARTNTO  = WS-VKART / 1000                          
060400     PERFORM S02-BERAEKNA-ANTAL                                           
060500     .                                                                    
060600     EJECT                                                                
060700 CC-FASTA-VARDEN SECTION.                                                 
060800                                                                          
060900     MOVE NEJ                 TO WS-SRAD-FLSATRAS                         
061000     MOVE NEJ                 TO WS-SRAD-FLSATSPR                         
061100     MOVE NEJ                 TO WS-SRAD-FLSATUTS                         
061200     MOVE +1                  TO WS-SRAD-KDCLAGER                         
061300     MOVE SPACE               TO WS-SRAD-KDSATAND                         
061400     MOVE SPACE               TO WS-SRAD-KDSATKMB                         
061500     MOVE +0                  TO WS-SRAD-KVSATRES                         
061600     MOVE +0                  TO WS-SRAD-KVSATROS                         
061700     MOVE +0                  TO WS-SRAD-PRARTSTD                         
061800     .                                                                    
061900     EJECT                                                                
062000 CD-SKRIV-FORSTA-RAD SECTION.                                             
062100                                                                          
062200     MOVE WS-SRAD-WDJ211   TO SATG11-SRAD-WDJ211                          
062300     MOVE WS-SHUV-IDORDNSB TO W-SHUV-IDORDNSB                             
062400     MOVE WS-SHUV-IDORDNSS TO W-SHUV-IDORDNSS                             
062500     PERFORM IMS-ISRT-FIRST-SATG11                                        
062600     .                                                                    
062700     EJECT                                                                
062800 CE-SKRIV-RAD SECTION.                                                    
062900                                                                          
063000     MOVE WS-SRAD-WDJ211 TO SATG11-SRAD-WDJ211                            
063100     PERFORM IMS-ISRT-SATG11                                              
063200     .                                                                    
063300     EJECT                                                                
063400 CF-SUMMERA-OCH-SKRIV-RAD  SECTION.                                       
063500                                                                          
063600     MOVE WS-SRAD-IDARTNR  TO W-SRAD-IDARTNR                              
063700     PERFORM IMS-GHU-SATG11                                               
063800     IF SEGMENT-FINNS                                                     
063900        COMPUTE WS-SRAD-REANTPSA =                                        
064000        WS-SRAD-REANTPSA + SATG11-SRAD-REANTPSA                           
064100        PERFORM S02-BERAEKNA-ANTAL                                        
064200        MOVE JA TO WS-SRAD-FLSATRAS                                       
064300        MOVE WS-SRAD-WDJ211 TO SATG11-SRAD-WDJ211                         
064400        PERFORM IMS-REPL-SATG11                                           
064500     END-IF                                                               
064600     .                                                                    
064700     EJECT                                                                
064800 D-SKAPA-TRANS-T-ANSK SECTION.                                            
064900                                                                          
065000     MOVE WS-SHUV-IDARTNR     TO XXCO11-2238-IDARTNR                      
065100     MOVE WS-SHUV-IDORDNSB    TO XXCO11-2238-IDORDNSB                     
065200     MOVE LOW-VALUE           TO XXCO11-2238-LOW-VALUE                    
065300     MOVE SPACE               TO XXCO11-2238-FILLER                       
065400                                                                          
065500     MOVE WS-SHUV-TIBEGPAC    TO DAT-I-TIDATUM                            
065600     MOVE 'AAMMDD'            TO DAT-KDDATFORM                            
065700                                                                          
065800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
065900                         DAT-O-TIDATUM DAT-KDSVAR                         
066000     END-CALL                                                             
066100                                                                          
066200     IF DAT-KDSVAR-OK                                                     
066300        MOVE DAT-TIAA-VECKA   TO WS-DAT-TIAA-VECKA                        
066400        MOVE DAT-TIVV         TO WS-DAT-TIVV                              
066500        MOVE WS-TIBEHOV       TO XXCO11-2238-TIBEHOV                      
066600     ELSE                                                                 
066700        DISPLAY '*** W41601 - FEL I DATKONV ***'                          
066800        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
066900     END-IF                                                               
067000     PERFORM IMS-ISRT-XXCO11                                              
067100     .                                                                    
067200     EJECT                                                                
067300 E-TABORT-TRANS-FR-ANSK SECTION.                                          
067400                                                                          
067500     PERFORM IMS-DLET-XXCN11                                              
067600     .                                                                    
067700     EJECT                                                                
067800 S01-CALL-W411ORDN SECTION.                                               
067900                                                                          
068000     MOVE 'SATS'        TO ORDN-IDSYSTEM                                  
068100     CALL W411ORDN USING ORDN-W411ORDN XXKP-PCB                           
068200     MOVE ORDN-IDORDNSB-UT TO W-SHUV-IDORDNSB                             
068300     .                                                                    
068400     EJECT                                                                
068500 S02-BERAEKNA-ANTAL SECTION.                                              
068600                                                                          
068700     COMPUTE WS-SRAD-REBEART ROUNDED  =                                   
068800             ( WS-SRAD-REANTPSA * WS-SHUV-KVBEART ) + 0.499               
068900     .                                                                    
069000     EJECT                                                                
069100 S07-NOLLST-ORDERHUVUD SECTION.                                           
069200*    --- NOLLSTÄLL ARBETSAREOR FÖR ORDERHUVUDET                           
069300                                                                          
069400     MOVE SPACE TO WS-SHUV-WDJ201                                         
069500     MOVE JA    TO RAETT-BEFT-SW                                          
069600     MOVE ZERO  TO WS-VLORDNTO-CM                                         
069700                   WS-VLORDNTO-M                                          
069800                   WS-VKORDNTO                                            
069900     .                                                                    
070000     EJECT                                                                
070100 S08-NOLLST-ORDERRAD SECTION.                                             
070200*    --- NOLLSTÄLL ARBETSAREOR FÖR ORDERRADERNA                           
070300                                                                          
070400     MOVE SPACE TO WS-SRAD-WDJ211                                         
070500     MOVE ZERO  TO WS-VLARTNTO                                            
070600                   WS-VKART                                               
070700     .                                                                    
070800     EJECT                                                                
070900 IMS-GET-ARTC01 SECTION.                                                  
071000                                                                          
071100     STRING 'WLARTC01(IDARTNR  =' W-ARTC01-IDARTNR-X ')'                  
071200          DELIMITED BY SIZE INTO SSA1                                     
071300     MOVE '  ' TO GODK-STATUSKODER                                        
071400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC01 SSA1                    
071500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
071600     PERFORM IMS-STATUSKONTROLL                                           
071700     .                                                                    
071800     SKIP3                                                                
071900 IMS-GNP-ARTC11 SECTION.                                                  
072000                                                                          
072100     MOVE 'WLARTC11 ' TO SSA1                                             
072200     MOVE '  ' TO GODK-STATUSKODER                                        
072300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC11 SSA1                   
072400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
072500     PERFORM IMS-STATUSKONTROLL                                           
072600     .                                                                    
072700     EJECT                                                                
072800 IMS-GET-SATB01 SECTION.                                                  
072900                                                                          
073000     STRING 'WLSATB01(IDARTNR  =' W-STR-IDARTNR-X ')'                     
073100          DELIMITED BY SIZE INTO SSA1                                     
073200     MOVE '  ' TO GODK-STATUSKODER                                        
073300     CALL CBLTDLI USING GU SATB-PCB DLI-IO-SATB01 SSA1                    
073400     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
073500     PERFORM IMS-STATUSKONTROLL                                           
073600     .                                                                    
073700     SKIP3                                                                
073800 IMS-GNP-FIRST-SATB11 SECTION.                                            
073900                                                                          
074000     MOVE 'WLSATB11*F ' TO SSA1                                           
074100     MOVE '  ' TO GODK-STATUSKODER                                        
074200     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-SATB11 SSA1                   
074300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
074400     PERFORM IMS-STATUSKONTROLL                                           
074500     .                                                                    
074600     SKIP3                                                                
074700 IMS-GNP-SATB11 SECTION.                                                  
074800                                                                          
074900     MOVE 'WLSATB11 ' TO SSA1                                             
075000     MOVE '  GE' TO GODK-STATUSKODER                                      
075100     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-SATB11 SSA1                   
075200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
075300     PERFORM IMS-STATUSKONTROLL                                           
075400     .                                                                    
075500     EJECT                                                                
075600 IMS-GET-SATG01 SECTION.                                                  
075700                                                                          
075800     STRING 'WLSATG01(IDORDNST =' W-SHUV-IDORDNST-X ')'                   
075900          DELIMITED BY SIZE INTO SSA1                                     
076000     MOVE '  GE' TO GODK-STATUSKODER                                      
076100     CALL CBLTDLI USING GU SATG-PCB DLI-IO-SATG01 SSA1                    
076200     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
076300     PERFORM IMS-STATUSKONTROLL                                           
076400     .                                                                    
076500     SKIP3                                                                
076600 IMS-ISRT-SATG01 SECTION.                                                 
076700                                                                          
076800     MOVE 'WLSATG01 ' TO SSA1                                             
076900     MOVE '  ' TO GODK-STATUSKODER                                        
077000     CALL CBLTDLI USING ISRT SATG-PCB DLI-IO-SATG01 SSA1                  
077100     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
077200     PERFORM IMS-STATUSKONTROLL                                           
077300     .                                                                    
077400     EJECT                                                                
077500 IMS-GHU-SATG11  SECTION.                                                 
077600                                                                          
077700     STRING 'WLSATG01(IDORDNST =' W-SHUV-IDORDNST-X ')'                   
077800          DELIMITED BY SIZE INTO SSA1                                     
077900     STRING 'WLSATG11(IDARTNR  =' W-SRAD-IDARTNR-X ')'                    
078000          DELIMITED BY SIZE INTO SSA2                                     
078100     MOVE '    ' TO GODK-STATUSKODER                                      
078200     CALL CBLTDLI USING GHU SATG-PCB DLI-IO-SATG11 SSA1 SSA2              
078300     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
078400     PERFORM IMS-STATUSKONTROLL                                           
078500     .                                                                    
078600     SKIP3                                                                
078700 IMS-REPL-SATG11 SECTION.                                                 
078800                                                                          
078900     MOVE '  ' TO GODK-STATUSKODER                                        
079000     CALL CBLTDLI USING REPL SATG-PCB DLI-IO-SATG11                       
079100     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
079200     PERFORM IMS-STATUSKONTROLL                                           
079300     .                                                                    
079400     EJECT                                                                
079500 IMS-ISRT-FIRST-SATG11 SECTION.                                           
079600                                                                          
079700     STRING 'WLSATG01(IDORDNST =' W-SHUV-IDORDNST-X ')'                   
079800          DELIMITED BY SIZE INTO SSA1                                     
079900     MOVE 'WLSATG11 ' TO SSA2                                             
080000     MOVE '  ' TO GODK-STATUSKODER                                        
080100     CALL CBLTDLI USING ISRT SATG-PCB DLI-IO-SATG11 SSA1 SSA2             
080200     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
080300     PERFORM IMS-STATUSKONTROLL                                           
080400     .                                                                    
080500     SKIP3                                                                
080600 IMS-ISRT-SATG11 SECTION.                                                 
080700                                                                          
080800     STRING 'WLSATG01(IDORDNST =' W-SHUV-IDORDNST-X ')'                   
080900          DELIMITED BY SIZE INTO SSA1                                     
081000     MOVE 'WLSATG11 ' TO SSA2                                             
081100     MOVE '  II' TO GODK-STATUSKODER                                      
081200     CALL CBLTDLI USING ISRT SATG-PCB DLI-IO-SATG11 SSA1 SSA2             
081300     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
081400     PERFORM IMS-STATUSKONTROLL                                           
081500     .                                                                    
081600     EJECT                                                                
081700 IMS-GU-XXCN01 SECTION.                                                   
081800                                                                          
081900     STRING 'WLXXCN01(WDGXKEY  =' W-WDGXKEY-2235-X ')'                    
082000          DELIMITED BY SIZE INTO SSA1                                     
082100     MOVE '  GE' TO GODK-STATUSKODER                                      
082200     CALL CBLTDLI USING GU  XXCN-PCB DLI-IO-XXCN11 SSA1                   
082300     MOVE XXCN-STATUS-CODE TO STATUS-WS                                   
082400     PERFORM IMS-STATUSKONTROLL                                           
082500     .                                                                    
082600     SKIP2                                                                
082700 IMS-GHNP-XXCN11 SECTION.                                                 
082800                                                                          
082900     MOVE 'WLXXCN11 ' TO SSA1                                             
083000     MOVE '  GE' TO GODK-STATUSKODER                                      
083100     CALL CBLTDLI USING GHNP XXCN-PCB DLI-IO-XXCN11 SSA1                  
083200     MOVE XXCN-STATUS-CODE TO STATUS-WS                                   
083300     PERFORM IMS-STATUSKONTROLL                                           
083400     .                                                                    
083500     SKIP2                                                                
083600 IMS-DLET-XXCN11 SECTION.                                                 
083700                                                                          
083800     MOVE '  ' TO GODK-STATUSKODER                                        
083900     CALL CBLTDLI USING DLET XXCN-PCB DLI-IO-XXCN11                       
084000     MOVE XXCN-STATUS-CODE TO STATUS-WS                                   
084100     PERFORM IMS-STATUSKONTROLL                                           
084200     .                                                                    
084300     EJECT                                                                
084400 IMS-ISRT-XXCO11 SECTION.                                                 
084500                                                                          
084600     STRING 'WLXXCO01(WDGXKEY  =' W-WDGXKEY-2237-X ')'                    
084700          DELIMITED BY SIZE INTO SSA1                                     
084800     MOVE 'WLXXCO11 ' TO SSA2                                             
084900     MOVE '  ' TO GODK-STATUSKODER                                        
085000     CALL CBLTDLI USING ISRT XXCO-PCB DLI-IO-XXCO11 SSA1 SSA2             
085100     MOVE XXCO-STATUS-CODE TO STATUS-WS                                   
085200     PERFORM IMS-STATUSKONTROLL                                           
085300     .                                                                    
085400     EJECT                                                                
085500 IMS-RESTART SECTION.                                                     
085600                                                                          
085700     MOVE SPACE TO MSG-IO-AREA                                            
085800     MOVE '  ' TO GODK-STATUSKODER                                        
085900     CALL CBLTDLI USING XRST MSG-PCB                                      
086000                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
086100                        CHKP-AREA-LENGTH CHKP-AREA                        
086200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
086300     PERFORM IMS-STATUSKONTROLL                                           
086400     SKIP2                                                                
086500     IF IMS-EJ-OK                                                         
086600     CALL FELLOG                                                          
086700     END-IF                                                               
086800     .                                                                    
086900     EJECT                                                                
087000 IMS-CHECKPOINT SECTION.                                                  
087100                                                                          
087200     MOVE IDPGM TO MSG-IO-AREA                                            
087300     MOVE '  XD' TO GODK-STATUSKODER                                      
087400     CALL CBLTDLI USING CHKP MSG-PCB                                      
087500                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
087600                        CHKP-AREA-LENGTH CHKP-AREA                        
087700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
087800     PERFORM IMS-STATUSKONTROLL                                           
087900                                                                          
088000     IF IMS-EJ-OK                                                         
088100       CALL FELLOG                                                        
088200     END-IF                                                               
088300     .                                                                    
088400     EJECT                                                                
088500 IMS-STATUSKONTROLL SECTION.                                              
088600                                                                          
088700     SET    STATUS-IX TO 1                                                
088800     SEARCH GODK-STATUS                                                   
088900       AT   END CALL FELLOG                                               
089000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
089100     END-SEARCH                                                           
089200     .                                                                    
089300     EJECT                                                                
089400*    -COPY WY2000Q1                                                       
