000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4010600.                                                
000300 AUTHOR.         KENETH GOUDE.                                            
000400 DATE-WRITTEN.   NOV 1979.                                                
000500*                                                                         
000600*    FUNCTION.                                                            
000700*         TP-FRÅGE-PROGRAM BALANCE INFO SDC.                              
000800*                                                                         
000900*    INDATA.                                                              
001000*        TRANSAKTION: W4T106                                              
001100*        MID:         W4I10601                                            
001200*    UTDATA.                                                              
001300*        MOD:         W4O10601                                            
001400*    SUBPROGRAM.                                                          
001500*        FELLOG                                                           
001600     SKIP2                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 DATA DIVISION.                                                           
002000     EJECT                                                                
002100 WORKING-STORAGE SECTION.                                                 
002200                                                                          
002300*    -- CHECKED BY WY2000                                                 
002400 77  IDARTNR-WS          PIC X(9).                                        
002500 77  IDDC-WS             PIC X(2).                                        
002600 77  MAX-MOD-LENGD       PIC S9(4)   COMP SYNC VALUE +354.                
002700 77  IX                  PIC S9(9)   COMP SYNC.                           
002800 77  W-KVBR              PIC S9(7)   COMP-3.                              
002900 77  JA                  PIC X       VALUE 'J'.                           
003000 77  NEJ                 PIC X       VALUE 'N'.                           
003100     SKIP2                                                                
003200 77  NYCKEL-SW           PIC X.                                           
003300     88  NYCKLAR-OK                  VALUE 'J'.                           
003400 77  WS-IDTRANS                  PIC X(4).                                
003500     88  WS-GODKAEND-BILD      VALUE '4101' '4102' '4103' '4104'          
003600                                     '4105' '4106' '4107' '4108'.         
003700                                                                          
003800 01  ARBETSAREOR.                                                         
003900*     -- FÖR REDIGERING AV IDAVINR FRÅN IDFS                              
004000  02     WS-IDAVINR              PIC 9(7)    VALUE ZERO.                  
004100  02     FILLER                  REDEFINES WS-IDAVINR.                    
004200   03    WS-IDAVINR-TKN          OCCURS 7                                 
004300                                 PIC 9(1).                                
004400*     -- FÖR REDIGERING AV IDAVINR FRÅN IDFS                              
004500  02     WS-IDFS                 PIC X(8)    VALUE SPACE.                 
004600  02     FILLER                  REDEFINES WS-IDFS.                       
004700   03    WS-IDFS-TKN             OCCURS 8                                 
004800                                 PIC 9(1).                                
004900*     -- FÄLTLÄNGD IDFS                                                   
005000  02     K-IDFS-LNG              PIC S9(9)   VALUE +8   COMP SYNC.        
005100*     -- FÄLTLÄNGD IDAVINR                                                
005200  02     K-IDAVINR-LNG           PIC S9(9)   VALUE +7   COMP SYNC.        
005300                                                                          
005400 01      IX-INDEXVARIABLER.                                               
005500                                                                          
005600*     -- TECKEN I WS-IDFS                                                 
005700  02     IX-IDFS                 PIC S9(9)   VALUE ZERO COMP SYNC.        
005800*     -- TECKEN I WS-IDAVINR                                              
005900  02     IX-IDAVINR              PIC S9(9)   VALUE ZERO COMP SYNC.        
006000                                                                          
006100     EJECT                                                                
006200 01  WS-FAELT.                                                            
006300     03  WS-SUTPO-TOT   OCCURS 2 PIC 9(7)  VALUE ZERO.                    
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500     03  CBLTDLI                 PIC X(8)  VALUE 'CBLTDLI '.              
006600     03  FELLOG                  PIC X(8)  VALUE 'FELLOG  '.              
006700     03  W005INIT                PIC X(8)  VALUE 'W005INIT'.              
006800     EJECT                                                                
006900 01  NYCKLAR-TILL-DLI.                                                    
007000     03  W-IDARTNR-X.                                                     
007100         05  W-IDARTNR   PIC S9(9)   VALUE ZERO COMP-3.                   
007200     03  W-WDD901KY-X.                                                    
007300         05  W-IDARTNR-D9 PIC S9(9)   VALUE ZERO COMP-3.                  
007400         05  W-IDDC-D9    PIC X(2)    VALUE SPACE.                        
007500     03  W-IDDC-X.                                                        
007600         05  W-IDDC      PIC X(2)    VALUE SPACE.                         
007700     03  W-IDDC-B6-X.                                                     
007800         05 W-IDDC-B6                  PIC X(2).                          
007900     EJECT                                                                
008000 01  MEDDELANDEN.                                                         
008100     03  FEL-1           PIC X(35)                                        
008200         VALUE 'THIS ARTICLE IS NOT IN THE DATABASE'.                     
008300     03  FEL-2           PIC X(26)                                        
008400         VALUE 'PART NUMBER IS NOT NUMERIC'.                              
008500     03  FEL-3           PIC X(23)                                        
008600         VALUE 'THIS ARTICLE IS DELETED'.                                 
008700     03  FEL-4           PIC X(24)                                        
008800         VALUE 'ONLY DC 21 TO 92 ALLOWED'.                                
008900     EJECT                                                                
009000*                        ****    PARAMETRAR TILL W005INIT                 
009100*01      -COPY WMSGINIT                                                   
009200     EJECT                                                                
009300*                        ****    TP-AREOR                                 
009400 01  FILLER              PIC X(16)   VALUE '    TP-AREAOR   '.            
009500     SKIP2                                                                
009600*01      MID -COPY W4I10601 -PRE MID-.                                    
009700     EJECT                                                                
009800*01      -COPY WMSGAREA.                                                  
009900     EJECT                                                                
010000*  03    MOD -COPY W4O10601 -PRE MOD- -RED MSG-AREA.                      
010100     EJECT                                                                
010200*01      -COPY WMFSAREA.                                                  
010300     EJECT                                                                
010400******************************************************************        
010500*****                                                                     
010600*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010700*****                                                                     
010800 01  IMS-WS.                                                              
010900     03  FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
011000     SKIP2                                                                
011100*****                    **** STATUS-KOD FRÅN IMS                         
011200     03  STATUS-WS       PIC XX.                                          
011300         88  SEGMENT-FINNS           VALUE '  '.                          
011400         88  SEGMENT-SAKNAS          VALUE 'GE'.                          
011500     SKIP2                                                                
011600     03  GODK-STATUSKODER.                                                
011700         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
011800     SKIP2                                                                
011900 01  SSA1                PIC X(32).                                       
012000 01  SSA2                PIC X(32).                                       
012100 01  SSA3                PIC X(32).                                       
012200     EJECT                                                                
012300*                        *** IMS FUNKTIONSKODER ***                       
012400*01          -COPY   W0003                                                
012500     EJECT                                                                
012600*            *** DLI INPUT-OUTPUT AREA ***                                
012700 01  DLI-IO-AREA-K601.                                                    
012800*    03  -COPY WDK601                                                     
012900     EJECT                                                                
013000 01  DLI-IO-AREA-K611.                                                    
013100*    03  -COPY WDK611                                                     
013200     EJECT                                                                
013300 01  DLI-IO-AREA-K701.                                                    
013400*    03  -COPY WDK701                                                     
013500     EJECT                                                                
013600 01  DLI-IO-AREA-K711.                                                    
013700*    03  -COPY WDK711                                                     
013800     EJECT                                                                
013810 01  DLI-IO-AREA-K722.                                                    
013820*    03  -COPY WDK722                                                     
013830     EJECT                                                                
013900 01  DLI-IO-AREA-D901.                                                    
014000*    03  -COPY WDD901 -PRE WDD9-                                          
014100     EJECT                                                                
014110 01  DLI-IO-AREA-D902.                                                    
014120*    03  -COPY WDD902 -PRE WDD9-                                          
014130     EJECT                                                                
014200 01  DLI-IO-AREA-K901.                                                    
014300*    03  -COPY WDK901 -PRE WDK9-                                          
014400                                                                          
014500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014600 01   DLI-IO-AREA-B601.                                                   
014700*     03  -COPY WDB601                                                    
014800     EJECT                                                                
014900 LINKAGE SECTION.                                                         
015000*01               -COPY W0009  -PRE MSG-                                  
015100     EJECT                                                                
015200*01               -COPY W0008  -PRE USEA-.                                
015300         05  FILLER      PIC X.                                           
015400     EJECT                                                                
015500*01               -COPY W0008  -PRE WDK6-.                                
015600         05  FILLER      PIC X.                                           
015700     EJECT                                                                
015800*01               -COPY W0008  -PRE WDK7-.                                
015900         05  FILLER      PIC X.                                           
016000     EJECT                                                                
016100*01               -COPY W0008  -PRE WDD9-.                                
016200         05  FILLER      PIC X.                                           
016300     EJECT                                                                
016400*01               -COPY W0008  -PRE WDK9-.                                
016500         05  FILLER      PIC X.                                           
016600*01               -COPY W0008  -PRE WDB6-.                                
016700         05  FILLER      PIC X.                                           
016800     EJECT                                                                
016900 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
017000                                  WDK6-PCB                                
017100                                  WDK7-PCB                                
017200                                  WDD9-PCB                                
017300                                  WDK9-PCB                                
017400                                  WDB6-PCB.                               
017500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
017600                                   WDK6-PCB                               
017700                                   WDK7-PCB                               
017800                                   WDD9-PCB                               
017900                                   WDK9-PCB                               
018000                                   WDB6-PCB.                              
018100                                                                          
018200*******************************                                           
018300**   ARTIKELREGISTER   WDK6  **                                           
018400**   ARTIKELREGISTER   WDK7  **                                           
018500**   LEV PLANREGISTER  WDD9  **                                           
018600**   ART.REG. SALDO    WDK9  **                                           
018700*******************************                                           
018800                                                                          
018900     PERFORM IMS-GET-MSG                                                  
019000     IF SEGMENT-FINNS                                                     
019100       PERFORM A-KOLLA-NYCKLAR                                            
019200       IF NYCKLAR-OK                                                      
019300         PERFORM IMS-GU-WDK601                                            
019400         IF SEGMENT-FINNS                                                 
019500           PERFORM B-REDIGERA-WDK6-INFO                                   
019600         ELSE                                                             
019700           MOVE FEL-1 TO MOD-MESSAGE-RAD1                                 
019800         END-IF                                                           
019900     ELSE                                                                 
020000        IF NOT WS-GODKAEND-BILD                                           
020100         PERFORM C-RENSA-NYCKLAR                                          
020200        END-IF                                                            
020300       END-IF                                                             
020400       PERFORM IMS-INSERT-MSG                                             
020500     END-IF                                                               
020600     MOVE ZERO TO RETURN-CODE                                             
020700     GOBACK.                                                              
020800     EJECT                                                                
020900 A-KOLLA-NYCKLAR   SECTION.                                               
021000     SKIP2                                                                
021100     MOVE JA TO NYCKEL-SW                                                 
021200                                                                          
021300     IF MSG-DUBBLA-TRANSKODER                                             
021400         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I10601               
021500         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
021600         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
021700     ELSE                                                                 
021800         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I10601                
021900         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
022000         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
022100     END-IF                                                               
022200     MOVE MFS-IDTRANS         TO WS-IDTRANS                               
022300                                                                          
022400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
022500     MOVE '001'             TO MSGI-KDCALL                                
022600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
022700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
022800     MOVE '4106'            TO MSGI-IDTRANS                               
022900                                                                          
023000     IF MFS-IDTRANS = '4106'                                              
023100     OR (MID-IDARTNR-IN NUMERIC                                           
023200     AND MID-IDARTNR-IN > ZERO)                                           
023300         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
023400     END-IF                                                               
023500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023600     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
023700     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
023800     IF MID-IDDC-IN NOT = ALL '+'                                         
023900     AND MFS-IDTRANS = '4106'                                             
024000       MOVE MID-IDDC-IN      TO IDDC-WS                                   
024100     ELSE                                                                 
024200       MOVE MSGI-IDDC        TO IDDC-WS                                   
024300     END-IF                                                               
024400                                                                          
024500     MOVE LOW-VALUE TO MOD-W4O10601                                       
024600     MOVE 'W4O106N1' TO MFS-IDMOD                                         
024700     IF ENGLISH-TEXT                                                      
024800         MOVE 'N' TO MFS-KDHUVOMR                                         
024900     END-IF                                                               
025000                                                                          
025100     MOVE '4106' TO MOD-IDTRANS                                           
025200                                                                          
025300     MOVE IDDC-WS    TO MOD-IDDC-UT                                       
025400     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
025500     MOVE '-' TO MOD-STRECK                                               
025600     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
025700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
025800                             MOD-IDDC-IN                                  
025900                                                                          
026000     IF IDARTNR-WS NOT NUMERIC                                            
026100         MOVE NEJ TO NYCKEL-SW                                            
026200         MOVE FEL-2 TO MOD-MESSAGE-RAD1                                   
026300     ELSE                                                                 
026400       MOVE IDDC-WS TO W-IDDC-B6                                          
026500       PERFORM IMS-GU-WDB601                                              
026600       IF DCS-KDDC = SPACE OR DCS-CDC OR DCS-CDC-TR OR DCS-DDC            
026700         MOVE NEJ TO NYCKEL-SW                                            
026800         MOVE FEL-4 TO MOD-MESSAGE-RAD1                                   
026900       ELSE                                                               
027000         MOVE IDARTNR-WS TO W-IDARTNR                                     
027100         MOVE IDDC-WS    TO W-IDDC                                        
027200       END-IF                                                             
027300     END-IF                                                               
027400                                                                          
027500     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
027600     .                                                                    
027700     EJECT                                                                
027800 B-REDIGERA-WDK6-INFO     SECTION.                                        
027900     SKIP2                                                                
028000***  SEGMENT 01 (ARTIKEL)                                                 
028100                                                                          
028200     MOVE ART-REKSIFFR TO MOD-REKSIFFR                                    
028300     SKIP2                                                                
028400                                                                          
028500     IF ART-KDERS-UTG > ZERO                                              
028600       CONTINUE                                                           
028700     ELSE                                                                 
028800***    SEGMENT 11 (CDC-INFORMATION)                                       
028900                                                                          
029000       PERFORM IMS-GNP-WDK611                                             
029100       MOVE 1              TO IX                                          
029200       MOVE CLAG-KVMP      TO MOD-KVMP                                    
029300       MOVE CLAG-KVPB-SEP  TO MOD-KVPB-SEP                                
029400       MOVE CLAG-KVPB-SATS TO MOD-KVPB-SATS                               
029500       MOVE CLAG-KVAVIS-SEN TO MOD-KVAVIS-SEN                             
029600                                                                          
029700*    -- REDIGERA IDAVINR                                                  
029800       MOVE CLAG-IDFS-SEN          TO WS-IDFS                             
029900       MOVE ZERO                   TO WS-IDAVINR                          
030000       MOVE K-IDFS-LNG             TO IX-IDFS                             
030100       MOVE K-IDAVINR-LNG          TO IX-IDAVINR                          
030200       PERFORM UNTIL (IX-IDFS      = ZERO                                 
030300                  OR  IX-IDAVINR   = ZERO)                                
030400         IF  WS-IDFS-TKN (IX-IDFS) NUMERIC                                
030500           MOVE WS-IDFS-TKN (IX-IDFS)                                     
030600                                   TO WS-IDAVINR-TKN (IX-IDAVINR)         
030700           SUBTRACT 1              FROM IX-IDAVINR                        
030800         END-IF                                                           
030900         SUBTRACT 1                FROM IX-IDFS                           
031000       END-PERFORM                                                        
031100       MOVE WS-IDAVINR     TO MOD-IDAVINR-SEN                             
031200                                                                          
031300       MOVE CLAG-KVLS      TO MOD-KVLS (IX)                               
031400       MOVE CLAG-KVRESS    TO MOD-KVRESS                                  
031500       MOVE CLAG-KVSLAGER  TO MOD-KVSLAGER                                
031600       MOVE CLAG-KVAKS-T   TO MOD-KVAKS-T                                 
031700       MOVE CLAG-KVAKS-CDC TO MOD-KVAKS-DC(IX)                            
031800       MOVE CLAG-KVAKS-PAV TO MOD-KVAKS-PAV(IX)                           
031900       MOVE CLAG-KVEFRS    TO MOD-KVEFRS (IX)                             
032000       MOVE CLAG-KVUTRS    TO MOD-KVUTRS (IX)                             
032100       MOVE CLAG-KVROS     TO MOD-KVROS                                   
032200       MOVE CLAG-ADLAGOMR  TO MOD-ADLAGOMR (IX)                           
032300       MOVE CLAG-ADGANG    TO MOD-ADGANG (IX)                             
032400       MOVE CLAG-ADPLATS   TO MOD-ADPLATS (IX)                            
032500       EJECT                                                              
032600***    SEGMENT K901 (TPO-INFORMATION)                                     
032700                                                                          
032800       PERFORM IMS-GU-WDK901                                              
032900       IF SEGMENT-FINNS                                                   
033000         MOVE WDK9-ART-SUTPO-TOT  TO MOD-SUTPO-TOT                        
033100       END-IF                                                             
033200                                                                          
033300***    SEGMENT K711 (SDC-INFORMATION)                                     
033400                                                                          
033500       MOVE 2              TO IX                                          
033600                                                                          
033700       MOVE ZERO           TO MOD-KVLS (IX)                               
033800                                MOD-KVAKS-DC(IX)                          
033900                                MOD-KVAKS-PAV(IX)                         
034000                                MOD-KVEFRS (IX)                           
034100                                MOD-KVUTRS (IX)                           
034200                                MOD-ADLAGOMR (IX)                         
034300                                MOD-ADGANG (IX)                           
034400                                MOD-ADPLATS (IX)                          
034500                                MOD-KVSKROT                               
034600                                MOD-TISKROT                               
034700                                                                          
034800       PERFORM IMS-GU-WDK701                                              
034900       IF SEGMENT-FINNS                                                   
035000         PERFORM IMS-GNP-WDK711                                           
035100         IF SEGMENT-FINNS                                                 
035200           MOVE SLAG-KVLS      TO MOD-KVLS (IX)                           
035300           MOVE SLAG-KVAKS-SDC TO MOD-KVAKS-DC(IX)                        
035400           MOVE SLAG-KVAKS-PAV TO MOD-KVAKS-PAV(IX)                       
035500           MOVE SLAG-KVEFRS    TO MOD-KVEFRS (IX)                         
035600           MOVE SLAG-KVUTRS    TO MOD-KVUTRS (IX)                         
035700           MOVE SLAG-ADLAGOMR  TO MOD-ADLAGOMR (IX)                       
035800           MOVE SLAG-ADGANG    TO MOD-ADGANG (IX)                         
035900           MOVE SLAG-ADPLATS   TO MOD-ADPLATS (IX)                        
036000           MOVE SLAG-KVSKROT   TO MOD-KVSKROT                             
036100           MOVE SLAG-TISKROT   TO MOD-TISKROT                             
036200         END-IF                                                           
036300       END-IF                                                             
036400***   SEGMENT WDK722 (ANSKAFFAR-INFO)                                     
036410***   OM SEGMENTET FINNS HÄMTAS KVSLAGER HÄRIFRÅN                         
036420***   OM SEGMENTET SAKNAS GÄLLER FORTFARANDE K6-VÄRDET                    
036421                                                                          
036430      PERFORM IMS-GU-WDK722                                               
036440      IF SEGMENT-FINNS                                                    
036450         MOVE XLAG-KVSLAGER    TO MOD-KVSLAGER                            
036460       END-IF                                                             
036500                                                                          
036510***   SEGMENT WDD902 (LEVERANSPLAN-INFO)                                  
036520                                                                          
036600       MOVE ZERO         TO W-KVBR                                        
036700       MOVE IDARTNR-WS   TO W-IDARTNR-D9                                  
036710       MOVE IDDC-WS      TO W-IDDC-D9                                     
036800       PERFORM IMS-GU-WDD901                                              
036900       IF SEGMENT-FINNS                                                   
037000         PERFORM IMS-GNP-WDD902                                           
037100         PERFORM UNTIL SEGMENT-SAKNAS                                     
037200           ADD WDD9-KVBR TO W-KVBR                                        
037300           PERFORM IMS-GNP-WDD902                                         
037400         END-PERFORM                                                      
037500         MOVE W-KVBR TO MOD-KVBR                                          
037600       END-IF                                                             
037700     END-IF                                                               
037800     .                                                                    
037900     EJECT                                                                
038000 C-RENSA-NYCKLAR SECTION.                                                 
038100     MOVE MFS-RENSA-FAELT           TO  MOD-IDARTNR-UT                    
038200     .                                                                    
038300     EJECT                                                                
038400 IMS-GET-MSG SECTION.                                                     
038500     MOVE '  QC' TO GODK-STATUSKODER                                      
038600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
038700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038800     PERFORM IMS-STATUS-KONTROLL                                          
038900     .                                                                    
039000     SKIP2                                                                
039100 IMS-INSERT-MSG SECTION.                                                  
039200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
039300     MOVE SPACE TO GODK-STATUSKODER                                       
039400     CALL CBLTDLI USING ISRT MSG-PCB                                      
039500                          MSG-IO-AREA MFS-IDMOD                           
039600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039700     PERFORM IMS-STATUS-KONTROLL                                          
039800     .                                                                    
039900     EJECT                                                                
040000 IMS-GU-WDK601        SECTION.                                            
040100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
040200            DELIMITED BY SIZE INTO SSA1                                   
040300     MOVE '  GE' TO GODK-STATUSKODER                                      
040400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K601 SSA1                 
040500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
040600     PERFORM IMS-STATUS-KONTROLL                                          
040700     .                                                                    
040800     SKIP2                                                                
040900 IMS-GNP-WDK611   SECTION.                                                
041000     MOVE 'WDK611  '   TO SSA1                                            
041100     MOVE '  ' TO GODK-STATUSKODER                                        
041200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-K611 SSA1                
041300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
041400     PERFORM IMS-STATUS-KONTROLL                                          
041500     .                                                                    
041600     EJECT                                                                
041700 IMS-GU-WDK701         SECTION.                                           
041800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
041900            DELIMITED BY SIZE INTO SSA1                                   
042000     MOVE '  GE' TO GODK-STATUSKODER                                      
042100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-K701 SSA1                 
042200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
042300     PERFORM IMS-STATUS-KONTROLL                                          
042400     .                                                                    
042500     SKIP2                                                                
042600 IMS-GNP-WDK711   SECTION.                                                
042700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
042800            DELIMITED BY SIZE INTO SSA1                                   
042900     MOVE '  GE' TO GODK-STATUSKODER                                      
043000     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-K711 SSA1                
043100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
043200     PERFORM IMS-STATUS-KONTROLL                                          
043300     .                                                                    
043400     EJECT                                                                
043410 IMS-GU-WDK722   SECTION.                                                 
043411     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
043412            DELIMITED BY SIZE INTO SSA1                                   
043420     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
043430            DELIMITED BY SIZE INTO SSA2                                   
043431     STRING 'WDK722  (KDSEGKEY =1)'                                       
043432            DELIMITED BY SIZE INTO SSA3                                   
043440     MOVE '  GE' TO GODK-STATUSKODER                                      
043450     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-AREA-K722                     
043451                            SSA1 SSA2 SSA3                                
043460     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
043470     PERFORM IMS-STATUS-KONTROLL                                          
043480     .                                                                    
043490     EJECT                                                                
043500 IMS-GU-WDD901                 SECTION.                                   
043600     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
043700            DELIMITED BY SIZE INTO SSA1                                   
043800     MOVE '  GE' TO GODK-STATUSKODER                                      
043900     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-AREA-D901 SSA1                 
044000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
044100     PERFORM IMS-STATUS-KONTROLL                                          
044200     .                                                                    
044300     SKIP2                                                                
044400 IMS-GNP-WDD902                SECTION.                                   
044500     MOVE 'WDD902  ' TO SSA1                                              
044600     MOVE '  GE' TO GODK-STATUSKODER                                      
044700     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-AREA-D902 SSA1                
044800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
044900     PERFORM IMS-STATUS-KONTROLL                                          
045000     .                                                                    
045100     SKIP2                                                                
045200 IMS-GU-WDK901                 SECTION.                                   
045300     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
045400            DELIMITED BY SIZE INTO SSA1                                   
045500     MOVE '  GE' TO GODK-STATUSKODER                                      
045600     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-AREA-K901 SSA1                 
045700     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
045800     PERFORM IMS-STATUS-KONTROLL                                          
045900     .                                                                    
046000     SKIP2                                                                
046100 IMS-GU-WDB601    SECTION.                                                
046200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
046300          DELIMITED BY SIZE INTO SSA1                                     
046400     MOVE '  GE' TO GODK-STATUSKODER                                      
046500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
046600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
046700     PERFORM IMS-STATUS-KONTROLL                                          
046800     IF SEGMENT-SAKNAS                                                    
046900         MOVE SPACE TO DCS-KDDC                                           
047000     END-IF                                                               
047100     .                                                                    
047200 IMS-STATUS-KONTROLL SECTION.                                             
047300     SET STATUS-IX TO 1                                                   
047400     SEARCH GODK-STATUS AT END CALL FELLOG                                
047500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
047600     END-SEARCH.                                                          
