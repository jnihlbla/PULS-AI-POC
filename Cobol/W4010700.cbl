000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4010700.                                                
000300 AUTHOR.         KENETH GOUDE.                                            
000400 DATE-WRITTEN.   JAN 1980.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION.                                                            
000800*         TP-FRÅGE-PROGRAM TREATMENT INFO SDC GRUNDBILD                   
000900*                                                                         
001000*    INDATA.                                                              
001100*        TRANSAKTION: W4T107                                              
001200*        MID:         W4I10701                                            
001300*    UTDATA.                                                              
001400*        MOD:         W4O10701                                            
001500*    SUBPROGRAM.                                                          
001600*        FELLOG                                                           
001700*    SKIP2                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 DATA DIVISION.                                                           
002100     EJECT                                                                
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400*    -- CHECKED BY WY2000                                                 
002500 77  IDARTNR-WS          PIC X(9).                                        
002600 77  IDDC-WS             PIC X(2).                                        
002700 77  MAX-MOD-LENGD       PIC S9(4)   COMP SYNC VALUE +436.                
002800 77  IX                  PIC S9(9)   COMP SYNC.                           
002900 77  IND                 PIC S9(9)   COMP SYNC.                           
003000 77  ANTAL-IDKAT-PA-BILDEN    PIC S9(3) COMP-3 VALUE +16.                 
003100 77  JA                  PIC X       VALUE 'J'.                           
003200 77  NEJ                 PIC X       VALUE 'N'.                           
003300                                                                          
003400 77  NYCKEL-SW           PIC X.                                           
003500     88  NYCKLAR-OK                  VALUE 'J'.                           
003600 77  WS-IDTRANS                  PIC X(4).                                
003700     88  WS-GODKAEND-BILD      VALUE '4101' '4102' '4103' '4104'          
003800                                     '4105' '4106' '4107' '4108'.         
003810*01  -COPY WWDCLAND                                                       
003811*01  -COPY WWOMVAND                                                       
003820                                                                          
003900                                                                          
004000 01  GENERELLA-SUBPROGRAM.                                                
004100   03  CBLTDLI           PIC X(8)    VALUE 'CBLTDLI '.                    
004200   03  FELLOG            PIC X(8)    VALUE 'FELLOG  '.                    
004300   03  W005INIT          PIC X(8)    VALUE 'W005INIT'.                    
004400                                                                          
004500 01  NYCKLAR-TILL-DLI.                                                    
004600     03  W-IDARTNR-X.                                                     
004700         05  W-IDARTNR   PIC S9(9)   VALUE ZERO COMP-3.                   
004800     03  W-IDDC-X.                                                        
004900         05  W-IDDC      PIC X(2)    VALUE SPACE.                         
004910     03  W-IDLAND-X.                                                      
004920         05  W-IDLAND    PIC X(2)    VALUE SPACE.                         
005000     03  W-IDSKYLT-X.                                                     
005100         05  W-IDSKYLT   PIC X(3).                                        
005200     03  W-IDLEVNR-X.                                                     
005300         05  W-IDLEVNR   PIC  X(5)   VALUE SPACE.                         
005400     03  W-IDDC-B6-X.                                                     
005500         05 W-IDDC-B6                  PIC X(2).                          
005600     EJECT                                                                
005700 01  MEDDELANDEN.                                                         
005800     03  FEL-1           PIC X(35)                                        
005900         VALUE 'THIS ARTICLE IS NOT IN THE DATABASE'.                     
006000     03  FEL-2           PIC X(26)                                        
006100         VALUE 'PART NUMBER IS NOT NUMERIC'.                              
006200     03  FEL-3           PIC X(23)                                        
006300         VALUE 'THIS ARTICLE IS DELETED'.                                 
006400     03  FEL-4           PIC X(24)                                        
006500         VALUE 'ONLY DC 21 TO 92 ALLOWED'.                                
006600     EJECT                                                                
006700*                        ****    PARAMETRAR TILL W005INIT                 
006800*01      -COPY WMSGINIT                                                   
006900     EJECT                                                                
007000*                        ****    TP-AREOR                                 
007100 01  FILLER              PIC X(16)   VALUE '    TP-AREAOR   '.            
007200     SKIP2                                                                
007300*01      MID -COPY W4I10701 -PRE MID-.                                    
007400     EJECT                                                                
007500*01      -COPY WMSGAREA.                                                  
007600     EJECT                                                                
007700*  03    MOD -COPY W4O10701 -PRE MOD- -RED MSG-AREA.                      
007800     EJECT                                                                
007900*01      -COPY WMFSAREA.                                                  
008000     EJECT                                                                
008100******************************************************************        
008200*****                                                                     
008300*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008400*****                                                                     
008500 01  IMS-WS.                                                              
008600     03  FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
008700     SKIP2                                                                
008800*****                    **** STATUS-KOD FRÅN IMS                         
008900     03  STATUS-WS       PIC XX.                                          
009000         88  SEGMENT-FINNS           VALUE '  '.                          
009100         88  SEGMENT-SAKNAS          VALUE 'GE'.                          
009200     SKIP2                                                                
009300     03  GODK-STATUSKODER.                                                
009400         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
009500     SKIP2                                                                
009600 01  SSA1                PIC X(32).                                       
009700 01  SSA2                PIC X(32).                                       
009800     EJECT                                                                
009900*                        *** IMS FUNKTIONSKODER ***                       
010000*01          -COPY   W0003                                                
010100     EJECT                                                                
010200*            *** DLI INPUT-OUTPUT AREA ***                                
010300 01  DLI-IO-AREA-601.                                                     
010400*    03  -COPY WDK601                                                     
010500                                                                          
010600 01  DLI-IO-AREA-611.                                                     
010700*    03  -COPY WDK611                                                     
010710                                                                          
010720 01  DLI-IO-AREA-712.                                                     
010730*    03  -COPY WDK712                                                     
010800                                                                          
010900 01  DLI-IO-AREA-301.                                                     
011000*    03  -COPY WDD301 -PRE WDD301-                                        
011010                                                                          
011020 01  DLI-IO-AREA-311.                                                     
011030*    03  -COPY WDD311 -PRE WDD311-                                        
011100                                                                          
011200 01  DLI-IO-AREA-502.                                                     
011300*    03  -COPY WDF502                                                     
011400                                                                          
011500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
011600 01   DLI-IO-AREA-B601.                                                   
011700*     03  -COPY WDB601                                                    
011800     EJECT                                                                
011900 LINKAGE SECTION.                                                         
012000*01               -COPY W0009  -PRE MSG-                                  
012100                                                                          
012200*01               -COPY W0008  -PRE USEA-.                                
012300         05  FILLER      PIC X.                                           
012400                                                                          
012500*01               -COPY W0008  -PRE WDD3-.                                
012600         05  FILLER      PIC X.                                           
012700                                                                          
012800*01               -COPY W0008  -PRE WDK6-.                                
012900         05  FILLER      PIC X.                                           
013000                                                                          
013010*01               -COPY W0008  -PRE WDK7-.                                
013020         05  FILLER      PIC X.                                           
013030                                                                          
013100*01               -COPY W0008  -PRE WDF5-.                                
013200         05  FILLER      PIC X.                                           
013300                                                                          
013400*01               -COPY W0008  -PRE WDB6-.                                
013500         05  FILLER      PIC X.                                           
013600                                                                          
013700 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
013800                                  WDD3-PCB WDK6-PCB WDK7-PCB              
013900                                  WDF5-PCB WDB6-PCB.                      
014000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
014100                                   WDD3-PCB WDK6-PCB WDK7-PCB             
014200                                   WDF5-PCB WDB6-PCB.                     
014300                                                                          
014400*********************************                                         
014500**   BENÄMNINGSREGISTER  WDD3  **                                         
014600**   ARTIKELREGISTER     WDK6  **                                         
014610**   ARTIKELREGISTER     WDK7  **                                         
014700**   CROSS-INDEX         WDF5  **                                         
014800*********************************                                         
014900                                                                          
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FINNS                                                     
015200         PERFORM A-KOLLA-NYCKLAR                                          
015300         IF NYCKLAR-OK                                                    
015400           PERFORM IMS-GU-WDK601                                          
015500           IF SEGMENT-FINNS                                               
015600               PERFORM B-REDIGERA-WDK6-WDK7-INFO                          
015700               PERFORM D-REDIGERA-WDD3-INFO                               
015800               PERFORM E-REDIGERA-WDF5-INFO                               
015900           ELSE                                                           
016000               MOVE FEL-1 TO MOD-MESSAGE-RAD1                             
016100           END-IF                                                         
016200         ELSE                                                             
016300            IF NOT WS-GODKAEND-BILD                                       
016400               PERFORM F-RENSA-NYCKLAR                                    
016500            END-IF                                                        
016600         END-IF                                                           
016700         PERFORM IMS-INSERT-MSG                                           
016800     END-IF                                                               
016900     MOVE ZERO TO RETURN-CODE                                             
017000     GOBACK                                                               
017100     .                                                                    
017200     EJECT                                                                
017300 A-KOLLA-NYCKLAR   SECTION.                                               
017400     SKIP2                                                                
017500     MOVE JA TO NYCKEL-SW                                                 
017600     SKIP2                                                                
017700     IF MSG-DUBBLA-TRANSKODER                                             
017800         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I10701               
017900         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
018000         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
018100         MOVE ZERO TO MID-IDKAT-SKIP                                      
018200     ELSE                                                                 
018300         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I10701                
018400         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
018500         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
018600     END-IF                                                               
018700     MOVE MFS-IDTRANS        TO WS-IDTRANS                                
018800                                                                          
018900     MOVE ALL '+' TO MSGI-WMSGINIT                                        
019000     MOVE '001'             TO MSGI-KDCALL                                
019100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
019200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
019300     MOVE '4107'            TO MSGI-IDTRANS                               
019400                                                                          
019500     IF MFS-IDTRANS = '4107'                                              
019600     OR (MID-IDARTNR-IN NUMERIC                                           
019700     AND MID-IDARTNR-IN > ZERO)                                           
019800         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
019900     END-IF                                                               
020000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020100     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
020200     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
020300     IF MID-IDDC-IN NOT = ALL '+'                                         
020400     AND MFS-IDTRANS = '4107'                                             
020500       MOVE MID-IDDC-IN      TO IDDC-WS                                   
020600     ELSE                                                                 
020700       MOVE MSGI-IDDC        TO IDDC-WS                                   
020800     END-IF                                                               
020900                                                                          
021000     MOVE LOW-VALUE TO MOD-W4O10701                                       
021100     MOVE 'W4O107N1' TO MFS-IDMOD                                         
021200     IF ENGLISH-TEXT                                                      
021300         MOVE 'N' TO MFS-KDHUVOMR                                         
021400     END-IF                                                               
021500                                                                          
021600     MOVE '4107' TO MOD-IDTRANS                                           
021700     MOVE ZERO TO MOD-IDKAT-SKIP                                          
021800                                                                          
021900     MOVE IDDC-WS TO MOD-IDDC-UT                                          
022000     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
022100     MOVE '-' TO MOD-STRECK                                               
022200     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
022300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
022400                                                                          
022500     IF IDARTNR-WS NOT NUMERIC                                            
022600         MOVE NEJ TO NYCKEL-SW                                            
022700         MOVE FEL-2 TO MOD-MESSAGE-RAD1                                   
022800     ELSE                                                                 
022900       MOVE IDDC-WS TO W-IDDC-B6                                          
023000       PERFORM IMS-GU-WDB601                                              
023100       IF DCS-KDDC = SPACE OR DCS-CDC OR DCS-CDC-TR OR DCS-DDC            
023200         MOVE NEJ TO NYCKEL-SW                                            
023300         MOVE FEL-4 TO MOD-MESSAGE-RAD1                                   
023400       ELSE                                                               
023500         MOVE IDARTNR-WS TO W-IDARTNR                                     
023600         MOVE IDDC-WS    TO W-IDDC                                        
023700       END-IF                                                             
023800     END-IF                                                               
023900                                                                          
024000     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
024100     .                                                                    
024200     EJECT                                                                
024300 B-REDIGERA-WDK6-WDK7-INFO   SECTION.                                     
024400     SKIP2                                                                
024500***  SEGMENT 01 (ARTIKEL-INFORMATION)                                     
024600                                                                          
024700     MOVE ART-REKSIFFR    TO MOD-REKSIFFR                                 
024800     MOVE ART-IDLEVNR     TO W-IDLEVNR                                    
024900     MOVE ART-KDPRODSL    TO MOD-KDPRODSL                                 
025000                                                                          
025100     MOVE +1 TO IX                                                        
025200     PERFORM UNTIL IX NOT < +6                                            
025300         MOVE ART-IDAO(IX) TO MOD-IDAO (IX)                               
025400         ADD +1 TO IX                                                     
025500     END-PERFORM                                                          
025600                                                                          
025700     IF ART-KDERS-UTG > ZERO                                              
025800       MOVE FEL-3         TO MOD-MESSAGE-RAD1                             
025900     ELSE                                                                 
026000       PERFORM BA-REDIGERA-WDK611-INFO                                    
026010       PERFORM BB-REDIGERA-WDK712-INFO                                    
026100     END-IF                                                               
026200     .                                                                    
026300     EJECT                                                                
026400 BA-REDIGERA-WDK611-INFO SECTION.                                         
026500                                                                          
026600*** SEGMENT 11   (CDC-INFORMATION)                                        
026700                                                                          
026800     PERFORM IMS-GNP-WDK611                                               
026900     MOVE +1                  TO IX                                       
027000     MOVE +1                  TO IND                                      
027100     PERFORM UNTIL IND NOT < +4                                           
027200        MOVE CLAG-IDKAT (IND) TO MOD-IDKAT(IX)                            
027300        ADD +1 TO IX                                                      
027400        ADD +1 TO IND                                                     
027500     END-PERFORM                                                          
027600                                                                          
027700     MOVE CLAG-KVQPACK-0       TO MOD-KVQPACK-0                           
027800     MOVE CLAG-KVQPACK-1       TO MOD-KVQPACK-1                           
027900     MOVE CLAG-KVQPACK-2       TO MOD-KVQPACK-2                           
028000     MOVE CLAG-KVQPACK-3       TO MOD-KVQPACK-3                           
028100     MOVE CLAG-KVQPACK-4       TO MOD-KVQPACK-4                           
028200     MOVE CLAG-BEFT            TO MOD-BEFT                                
028300     MOVE CLAG-IDARTNR-EMBQ0   TO MOD-IDARTNR-EMBQ0                       
028400     MOVE CLAG-IDARTNR-EMBQ1   TO MOD-IDARTNR-EMBQ1                       
028500     MOVE CLAG-IDARTNR-EMBQ2   TO MOD-IDARTNR-EMBQ2                       
028600     MOVE CLAG-IDARTNR-EMBQ3   TO MOD-IDARTNR-EMBQ3                       
028700     MOVE CLAG-IDARTNR-EMBQ4   TO MOD-IDARTNR-EMBQ4                       
028800     MOVE CLAG-KDARTURS        TO MOD-CDC-KDARTURS                        
059700       IF MSGI-KDMATT = 'U'                                               
059800         COMPUTE MOD-CDC-VKART ROUNDED =                                  
059900                                   CLAG-VKART * CONV-GR-TO-OZ             
060000         END-COMPUTE                                                      
060100         COMPUTE MOD-CDC-VLARTNTO ROUNDED =                               
060200                                   CLAG-VLARTNTO * CONV-CM3-TO-IN3        
060300         END-COMPUTE                                                      
060400         MOVE '    OZ'               TO MOD-BESORT-VKART                  
060500         MOVE 'CU.IN.'               TO MOD-BESORT-VLARTNTO               
060700       ELSE                                                               
060911         MOVE CLAG-VKART             TO MOD-CDC-VKART                     
060912         MOVE CLAG-VLARTNTO          TO MOD-CDC-VLARTNTO                  
061000         MOVE '     G'               TO MOD-BESORT-VKART                  
061100         MOVE '   CM3'               TO MOD-BESORT-VLARTNTO               
061300       END-IF                                                             
061301                                                                          
061302     .                                                                    
061303                                                                          
061304 BB-REDIGERA-WDK712-INFO SECTION.                                         
061305                                                                          
061306*** SEGMENT 12   (AVVIKANDE INFO FÖR GIVET LAND)                          
061307                                                                          
061308     SEARCH ALL DC-LAND                                                   
061309        AT END                                                            
061310           MOVE SPACE          TO W-IDLAND                                
061311        WHEN DCLAND-IDDC (DCLAND-IX) = W-IDDC                             
061312           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
061313                               TO W-IDLAND                                
061314     END-SEARCH                                                           
061315     PERFORM IMS-GU-WDK712                                                
061316                                                                          
061317     IF SEGMENT-FINNS                                                     
061318       IF LART-BEFT > 0                                                   
061319         MOVE LART-BEFT          TO MOD-BEFT                              
061320       END-IF                                                             
061321       IF LART-IDARTNR-EMBQ0 > 0                                          
061322         MOVE LART-IDARTNR-EMBQ0 TO MOD-IDARTNR-EMBQ0                     
061323       END-IF                                                             
061324       IF LART-IDARTNR-EMBQ0 > 0                                          
061325         MOVE LART-IDARTNR-EMBQ1 TO MOD-IDARTNR-EMBQ1                     
061326       END-IF                                                             
061327       IF LART-IDARTNR-EMBQ0 > 0                                          
061328         MOVE LART-IDARTNR-EMBQ2 TO MOD-IDARTNR-EMBQ2                     
061329       END-IF                                                             
061330                                                                          
061331       IF LART-KVQPACK-3 > 0                                              
061332         MOVE LART-KVQPACK-3 TO MOD-KVQPACK-3                             
061333       END-IF                                                             
061334                                                                          
061335       MOVE LART-KDARTURS      TO MOD-DC-KDARTURS                         
061336       IF LART-VKART > 0                                                  
061337         IF MSGI-KDMATT = 'U'                                             
061338           COMPUTE MOD-DC-VKART ROUNDED =                                 
061339                                 LART-VKART * CONV-GR-TO-OZ               
061340         ELSE                                                             
061350           MOVE LART-VKART         TO MOD-DC-VKART                        
061360         END-IF                                                           
061370       END-IF                                                             
061380       IF LART-VLARTNTO > 0                                               
061390         IF MSGI-KDMATT = 'U'                                             
061400           COMPUTE MOD-DC-VLARTNTO ROUNDED =                              
061500                                 LART-VLARTNTO * CONV-CM3-TO-IN3          
061600         ELSE                                                             
061700           MOVE LART-VLARTNTO      TO MOD-DC-VLARTNTO                     
061800         END-IF                                                           
061801       END-IF                                                             
061802     END-IF                                                               
061803     .                                                                    
061804     EJECT                                                                
061805 D-REDIGERA-WDD3-INFO SECTION.                                            
061806     PERFORM IMS-GU-WDD301                                                
061807     IF ENGLISH-TEXT                                                      
061808         MOVE 'GB ' TO        W-IDSKYLT                                   
061809         PERFORM IMS-GNP-WDD311                                           
061810         IF SEGMENT-FINNS                                                 
061811             MOVE WDD311-TEXT-BEART TO MOD-BEART-SVE-ENG                  
061812         END-IF                                                           
061813     ELSE                                                                 
061814         MOVE 'S  ' TO        W-IDSKYLT                                   
061815         PERFORM IMS-GNP-WDD311                                           
061816         IF SEGMENT-FINNS                                                 
061817             MOVE WDD311-TEXT-BEART TO MOD-BEART-SVE-ENG                  
061818         ELSE                                                             
061819             MOVE SPACE TO MOD-BEART-SVE-ENG                              
061820         END-IF                                                           
061821     END-IF                                                               
061822     .                                                                    
061823     EJECT                                                                
061824 E-REDIGERA-WDF5-INFO   SECTION.                                          
061825*** SEGMENT 03                                                            
061826     SKIP2                                                                
061827*                                                                         
061828     PERFORM IMS-GU-WDF502                                                
061829     IF  SEGMENT-FINNS                                                    
061830         MOVE XLEV-BELEVART TO MOD-BELEV                                  
061831     END-IF                                                               
061832     .                                                                    
061833     EJECT                                                                
061834 F-RENSA-NYCKLAR SECTION.                                                 
061835     MOVE MFS-RENSA-FAELT           TO  MOD-IDARTNR-UT                    
061836                                        MOD-IDDC-UT                       
061837     .                                                                    
061838     EJECT                                                                
061839 IMS-GET-MSG SECTION.                                                     
061840     MOVE '  QC' TO GODK-STATUSKODER                                      
061841     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
061842     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061843     PERFORM IMS-STATUS-KONTROLL                                          
061844     SKIP2                                                                
061845     .                                                                    
061846 IMS-INSERT-MSG SECTION.                                                  
061847     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
061848     MOVE SPACE TO GODK-STATUSKODER                                       
061849     CALL CBLTDLI USING ISRT MSG-PCB                                      
061850                          MSG-IO-AREA MFS-IDMOD                           
061851     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061852     PERFORM IMS-STATUS-KONTROLL                                          
061853     .                                                                    
061854     EJECT                                                                
061855 IMS-GU-WDD301           SECTION.                                         
061856     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
061857             DELIMITED BY SIZE INTO SSA1                                  
061858     MOVE '  ' TO GODK-STATUSKODER                                        
061859     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA-301 SSA1                  
061860     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
061861     PERFORM IMS-STATUS-KONTROLL                                          
061862     SKIP2                                                                
061863     .                                                                    
061864 IMS-GNP-WDD311            SECTION.                                       
061865     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
061866             DELIMITED BY SIZE INTO SSA1                                  
061867     MOVE '  GE' TO GODK-STATUSKODER                                      
061868     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-AREA-311 SSA1                 
061869     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
061870     PERFORM IMS-STATUS-KONTROLL                                          
061871     .                                                                    
061872     EJECT                                                                
061873 IMS-GU-WDK601           SECTION.                                         
061874     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
061875            DELIMITED BY SIZE INTO SSA1                                   
061876     MOVE '  GE' TO GODK-STATUSKODER                                      
061877     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-601 SSA1                  
061878     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
061879     PERFORM IMS-STATUS-KONTROLL                                          
061880     .                                                                    
061881                                                                          
061882 IMS-GNP-WDK611      SECTION.                                             
061883     MOVE 'WDK611  ' TO SSA1                                              
061884     MOVE '  ' TO GODK-STATUSKODER                                        
061885     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-611 SSA1                 
061886     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
061887     PERFORM IMS-STATUS-KONTROLL                                          
061888     .                                                                    
061889                                                                          
061890 IMS-GU-WDK712           SECTION.                                         
061891     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
061892            DELIMITED BY SIZE INTO SSA1                                   
061893     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
061894            DELIMITED BY SIZE INTO SSA2                                   
061895     MOVE '  GE' TO GODK-STATUSKODER                                      
061896     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-712 SSA1 SSA2             
061897     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
061898     PERFORM IMS-STATUS-KONTROLL                                          
061899     .                                                                    
061900                                                                          
061901 IMS-GU-WDF502                 SECTION.                                   
061902     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
061903            DELIMITED BY SIZE INTO SSA1                                   
061904     STRING 'WDF502  *L(IDLEVNR  =' W-IDLEVNR-X ')'                       
061905            DELIMITED BY SIZE INTO SSA2                                   
061906     MOVE '  GE' TO GODK-STATUSKODER                                      
061907     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA-502 SSA1 SSA2             
061908     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
061909     PERFORM IMS-STATUS-KONTROLL                                          
061910     SKIP2                                                                
061911     .                                                                    
061912 IMS-GU-WDB601    SECTION.                                                
061913     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
061914          DELIMITED BY SIZE INTO SSA1                                     
061915     MOVE '  GE' TO GODK-STATUSKODER                                      
061916     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
061917     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
061918     PERFORM IMS-STATUS-KONTROLL                                          
061919     IF SEGMENT-SAKNAS                                                    
061920         MOVE SPACE TO DCS-KDDC                                           
061921     END-IF                                                               
061922     .                                                                    
061923 IMS-STATUS-KONTROLL SECTION.                                             
061924     SET STATUS-IX TO 1                                                   
061925     SEARCH GODK-STATUS                                                   
061926       AT END                                                             
061927         CALL FELLOG                                                      
061928       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
061929         CONTINUE                                                         
061930     END-SEARCH                                                           
061940     .                                                                    
