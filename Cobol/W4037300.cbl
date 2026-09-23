000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4037300.                                                
000400 AUTHOR.         ROGER OLSSON.                                            
000500 DATE-WRITTEN.   90/12/11.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        LÄSER ANGIVEN SATSORDER (WLSATG01), KONTROLLERAR ATT             
001100*        STATUS = 3.                                                      
001200*        ENDAST RADER SOM HAR KDSATLI = 1 SKALL BEHANDLAS.                
001300*        VARJE RAD (WLSATG12) SOM BEHANDLAS DELETAS EFTERÅT.              
001400*        NÄR SATSORDERN ÄR FÄRDIGBEHANDLAD SÄTTS STATUS TILL 4.           
001500*        PROGRAMMET LÄGGER UPP SEGMENT PÅ :                               
001600*             - WDE4   - WDE401 (ORDERHUVUD)                              
001700*                      - WDE411 (ORDERRADER)                              
001800*                                                                         
001900*             - WDE6   - WDE601 (KOLLI)                                   
002000*                                                                         
002100*        VID ABEND-PROBLEM MED RC=NI MOT WDE4, SÖK PÅ FIX.                
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W4T373X.                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        UPPDATERING AV WDE4, WDE6.                                       
002800*                                                                         
002900*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
003000*    E-TRACKER: 10254592 2015       DECOMISSION VOHF                      
003100                                                                          
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600*    -- CHECKED BY WY2000                                                 
003700     SKIP3                                                                
003800 77  IDPGM                       PIC X(08)   VALUE 'W4037300'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  WS-DAGENS-DATUM             PIC 9(6).                                
004600 77  WS-IDKONTO-ALFA             PIC X(11)   VALUE SPACE.                 
004700                                                                          
004800 77  IDPURAD                     PIC S9(9)   VALUE +0   COMP SYNC.        
004900 77  W-IDARTNR-NUM               PIC  9(8)   VALUE ZERO.                  
005000 77  W-KVBEART-NUM               PIC  9(6)   VALUE ZERO.                  
005100 77  WDE411-INDX                 PIC S9(6)   VALUE +0.                    
005200                                                                          
005300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005400     88  ALLT-OK                             VALUE 'J'.                   
005500     88  ALLT-FEL                            VALUE 'N'.                   
005600                                                                          
005700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005800     88  GODK-MID                            VALUE '4372'.                
005900                                                                          
006000 01  WS-KLOCKAN.                                                          
006100     03 WS-TIHHMMSS              PIC 9(6).                                
006200     03 FILLER                   PIC X(2).                                
006300*      --- VALID IDDC CODES                                               
006400*                                                                         
006500*01    -COPY WWDCKONS                                                     
006600       EJECT                                                              
006700                                                                          
006800 01  WS-MSG-AREA.                                                         
006900     03 WS-IDORDNSB              PIC X(5).                                
007000     03 WS-IDORDNSS              PIC X(1).                                
007100     EJECT                                                                
007200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007300 01  GENERELLA-SUBPROGRAM.                                                
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     EJECT                                                                
007700*    --- AREOR FÖR MSG HANTERING                                          
007800*                                                                         
007900 01  FILLER                     PIC X(16)   VALUE 'MSG-AREA'.             
008000*                                                                         
008100*01  -COPY WMSGAREA                                                       
008200     EJECT                                                                
008300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008600     SKIP3                                                                
008700 01  NYCKLAR-TILL-DLI.                                                    
008800                                                                          
008900*----> DIREKTNYCKEL TILL SATSORDER.                                       
009000                                                                          
009100     03  W-WDJ2-IDORDNST-X.                                               
009200         05 W-WDJ2-IDORDNSB      PIC S9(5) COMP-3.                        
009300         05 W-WDJ2-IDORDNSS      PIC S9(1) COMP-3.                        
009400                                                                          
009500     03  W-WDJ2-KDSATLI-X        PIC  X(1) VALUE '1'.                     
009600                                                                          
009700     03  W-WDJ2-IDARTNR-X.                                                
009800         05 W-WDJ2-IDARTNR       PIC S9(9) COMP-3.                        
009900                                                                          
010000*----> DIREKTNYCKEL TILL KUNDORDERREGISTER.                               
010100                                                                          
010200     03  W-WDE4-WDE4KEY-X.                                                
010300         05  W-WDE4-IDDISTR      PIC S9(5) COMP-3.                        
010400         05  W-WDE4-IDKUNDNR     PIC S9(7) COMP-3.                        
010500         05  W-WDE4-IDKUNDRF     PIC X(10).                               
010600         05  W-WDE4-IDPRODNR     PIC S9(7) COMP-3.                        
010700         05  W-WDE4-IDPLKLST     PIC S9(3) COMP-3.                        
010800                                                                          
010900*------- STATUS-KOD FRÅN IMS                                              
011000                                                                          
011100 01  STATUS-WS                   PIC X(2).                                
011200     88  SEGMENT-FINNS                       VALUE '  '.                  
011300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011400     88  END-OF-DATA                         VALUE 'GB'.                  
011500     SKIP2                                                                
011600 01  GODK-STATUSKODER.                                                    
011700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011800     SKIP3                                                                
011900 01  SSA1                        PIC X(96).                               
012000 01  SSA2                        PIC X(96).                               
012100     EJECT                                                                
012200**************************                                                
012300*  ARBETSAREA RYI-TRANS  *                                                
012400**************************                                                
012500                                                                          
012600 01  W-RYIPOST.                                                           
012700     03 -COPY WDGZRYI -PRE W-                                             
012800     EJECT                                                                
012900*    --- IMS FUNKTIONSKODER                                               
013000*01  -COPY W0003                                                          
013100     EJECT                                                                
013200*    ---  DLI INPUT-OUTPUT AREA                                           
013300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013400     SKIP3                                                                
013500 01  DLI-IO-AREA1.                                                        
013600     03  WLSATG01.                                                        
013700*        05  -COPY WDJ201                                                 
013800     EJECT                                                                
013900 01  DLI-IO-AREA2.                                                        
014000     03  WLSATG11.                                                        
014100*        05  -COPY WDJ211                                                 
014200     EJECT                                                                
014300 01  DLI-IO-AREA3.                                                        
014400     03  WLSATG12.                                                        
014500*        05  -COPY WDJ212                                                 
014600     EJECT                                                                
014700 01  FILLER        PIC X(16) VALUE 'DLI-IO-WDE401'.                       
014800 01  DLI-IO-WDE401.                                                       
014900*    03  -COPY WDE401                                                     
015000     EJECT                                                                
015100 01  FILLER        PIC X(16) VALUE 'DLI-IO-WDE411'.                       
015200 01  DLI-IO-WDE411.                                                       
015300*    03  -COPY WDE411                                                     
015400     EJECT                                                                
015500 01  FILLER        PIC X(16) VALUE 'DLI-IO-WDE601'.                       
015600 01  DLI-IO-WDE601.                                                       
015700*    03  -COPY WDE601                                                     
015800     EJECT                                                                
015900 01  DLI-IO-AREA6.                                                        
016000     03  WLZZAC01.                                                        
016100*        05  -COPY WDGZ01  -PRE ZZAC-                                     
016200     EJECT                                                                
016300 LINKAGE SECTION.                                                         
016400                                                                          
016500*01  -COPY W0009      -PRE MSG-                                           
016600     EJECT                                                                
016700*01  -COPY W0008      -PRE SATG1-                                         
016800     05  FILLER                  PIC X.                                   
016900     EJECT                                                                
017000*01  -COPY W0008      -PRE SATG2-                                         
017100     05  FILLER                  PIC X.                                   
017200     EJECT                                                                
017300*01  -COPY W0008      -PRE WDE4-                                          
017400     05  FILLER                  PIC X.                                   
017500     EJECT                                                                
017600*01  -COPY W0008      -PRE WDE6-                                          
017700     05  FILLER                  PIC X.                                   
017800     EJECT                                                                
017900*01  -COPY W0008      -PRE ZZAC-                                          
018000     05  FILLER                  PIC X.                                   
018100     EJECT                                                                
018200 PROCEDURE DIVISION  USING MSG-PCB                                        
018300                           SATG1-PCB                                      
018400                           SATG2-PCB                                      
018500                           WDE4-PCB                                       
018600                           WDE6-PCB                                       
018700                           ZZAC-PCB.                                      
018800                                                                          
018900     ENTRY 'DLITCBL' USING MSG-PCB                                        
019000                           SATG1-PCB                                      
019100                           SATG2-PCB                                      
019200                           WDE4-PCB                                       
019300                           WDE6-PCB                                       
019400                           ZZAC-PCB.                                      
019500                                                                          
019600     PERFORM IMS-GET-MSG                                                  
019700     IF SEGMENT-FINNS                                                     
019800        PERFORM A-INIT                                                    
019900        IF ALLT-OK                                                        
020000           PERFORM B-LAES-SATSORDER                                       
020100           IF ALLT-OK                                                     
020200              PERFORM C-UPPDAT-WDE4-WDE6                                  
020300              PERFORM D-UPPDAT-SATSORDER                                  
020400           END-IF                                                         
020500        END-IF                                                            
020600     END-IF                                                               
020700                                                                          
020800     MOVE ZERO TO RETURN-CODE                                             
020900     GOBACK                                                               
021000     .                                                                    
021100     EJECT                                                                
021200 A-INIT SECTION.                                                          
021300                                                                          
021400     IF MSG-KDTRANS-1  NOT = 'W4T373X '                                   
021500        MOVE NEJ TO ALLT-SW                                               
021600     END-IF                                                               
021700                                                                          
021800     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
021900     IF NOT GODK-MID                                                      
022000        MOVE NEJ TO ALLT-SW                                               
022100     END-IF                                                               
022200                                                                          
022300     IF ALLT-OK                                                           
022400        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO WS-MSG-AREA                   
022500        MOVE WS-IDORDNSB                 TO W-WDJ2-IDORDNSB               
022600        MOVE WS-IDORDNSS                 TO W-WDJ2-IDORDNSS               
022700        ACCEPT WS-DAGENS-DATUM         FROM DATE                          
022800        ACCEPT WS-KLOCKAN              FROM TIME                          
022900     END-IF                                                               
023000     .                                                                    
023100     EJECT                                                                
023200 B-LAES-SATSORDER SECTION.                                                
023300                                                                          
023400     PERFORM IMS-GHU-WDJ2-WLSATG01                                        
023500                                                                          
023600     IF SEGMENT-FINNS                                                     
023700        IF SHUV-KDSATPLK NOT = 3                                          
023800           MOVE NEJ TO ALLT-SW                                            
023900        END-IF                                                            
024000     ELSE                                                                 
024100        MOVE NEJ TO ALLT-SW                                               
024200     END-IF                                                               
024300     .                                                                    
024400     EJECT                                                                
024500 C-UPPDAT-WDE4-WDE6 SECTION.                                              
024600                                                                          
024700     MOVE +0  TO WDE411-INDX                                              
024800     PERFORM CA-SKAPA-WDE401                                              
024900     PERFORM CB-SKAPA-WDE401-NYCKEL                                       
025000                                                                          
025100     PERFORM IMS-GHNP-WDJ2-WLSATG12                                       
025200                                                                          
025300     PERFORM UNTIL SEGMENT-SAKNAS                                         
025400        ADD 1 TO IDPURAD                                                  
025500        ADD 1 TO WDE411-INDX                                              
025600        PERFORM CC-SKAPA-WDE411                                           
025700        PERFORM CH-SKAPA-RYITRANS                                         
025800        PERFORM IMS-DLET-WDJ2-WLSATG12                                    
025900        PERFORM IMS-GHNP-WDJ2-WLSATG12                                    
026000     END-PERFORM                                                          
026100                                                                          
026200     IF WDE411-INDX = 0                                                   
026202       MOVE KORD-IDDISTR       TO W-WDE4-IDDISTR                          
026203       MOVE KORD-IDKUNDNR      TO W-WDE4-IDKUNDNR                         
026204       MOVE KORD-IDKUNDRF      TO W-WDE4-IDKUNDRF                         
026205       MOVE KORD-IDPRODNR      TO W-WDE4-IDPRODNR                         
026206       MOVE KORD-IDPLKLST      TO W-WDE4-IDPLKLST                         
026220       PERFORM IMS-GHU-WDE401                                             
026230                                                                          
026300       PERFORM IMS-DLET-WDE401                                            
026400     END-IF                                                               
026500                                                                          
026600     PERFORM CE-SKAPA-WDE601                                              
026610     IF WDE411-INDX > 0                                                   
026700       PERFORM CG-UPPDAT-WDE401                                           
026701     END-IF                                                               
026710     MOVE +0  TO WDE411-INDX                                              
026800     .                                                                    
026900     EJECT                                                                
027000 CA-SKAPA-WDE401 SECTION.                                                 
027100                                                                          
027200*    FIX FÖR ATT KLARA DUBBLA PRODNR. OBS SE ÄVEN FIX I D-UPPDAT-         
027300*    ABEND-PROBLEM MED RC=NI VID ISRT MOT WDE4. BEROENDE PÅ ATT           
027400*    PRODNR-SERIEN HAR GÅTT RUNT ETT VARV SEDAN SATS-ORDERN GICK          
027500*    I REST.                                                              
027600*    OBS! MEDDELA ÄVEN DET NYA PRODNUMMRET TILL DEN SOM SKREV UT          
027700*    ORDERN. DET BEHÖVS FÖR VID PACKRAPPORTERINGEN.                       
027800*    KOMIHÅG ATT ÖPPNA FIX I D-UPPDAT SECTION OXÅ.                        
027900*    IF SHUV-IDPRODNR          = +135388                                  
028000*       MOVE +654324           TO SHUV-IDPRODNR                           
028100*    END-IF                                                               
028200                                                                          
028300     MOVE SHUV-IDDISTR         TO  KORD-IDDISTR                           
028400     MOVE SHUV-IDKUNDNR        TO  KORD-IDKUNDNR                          
028500     MOVE SPACE                TO  KORD-IDKUNDRF                          
028600     MOVE WS-IDORDNSB (2:4)    TO  KORD-IDKUNDRF (1:4)                    
028700     MOVE WS-IDORDNSS          TO  KORD-IDKUNDRF (5:1)                    
028800     MOVE SHUV-IDPRODNR        TO  KORD-IDPRODNR                          
028900     MOVE 1                    TO  KORD-IDPLKLST                          
029000     MOVE ZERO                 TO  KORD-IDORDER                           
029100                                   KORD-KDFAKPAP                          
029200                                   KORD-KDFDKRAV                          
029300                                   KORD-KDPAKOLL                          
029400                                   KORD-KVORDRAD                          
029500                                   KORD-KVORDRAD-LEVPL                    
029600                                   KORD-KVORDRAD-PACK                     
029700                                   KORD-KVORDRAD-VO                       
029800                                   KORD-KVORDRAD-VO-LEVPL                 
029900                                   KORD-SUORDV-LEVPL                      
030000                                   KORD-SUORDV-LOC                        
030100                                   KORD-SUORDV-LOCPREL                    
030200                                   KORD-SUORDV-LEVPL-LOC                  
030300                                   KORD-SUORDV-LEVPL-LOCPREL              
030400     MOVE SPACE                TO  KORD-KDVALISO                          
030500     MOVE SHUV-IDUSER          TO  KORD-IDUSER                            
030600     MOVE WC-CDC-SE            TO  KORD-IDDC                              
030700     MOVE JA                   TO  KORD-FLLSBOK                           
030800     MOVE NEJ                  TO  KORD-FLOVRLEV                          
030900                                   KORD-FLORDSPE                          
031000                                   KORD-FLPAFEL                           
031100     MOVE SHUV-KDFAKTYP        TO  KORD-KDFAKTYP                          
031200     MOVE +68                  TO  KORD-KDFRAKT                           
031300     MOVE SHUV-KDORDKL         TO  KORD-KDORDKL                           
031400     MOVE SHUV-DAREGDAT (3:6)  TO  KORD-TIORDREG                          
031500     MOVE SPACE                TO  KORD-KDROPACK                          
031600     MOVE SHUV-IDANSK          TO  KORD-KDPERSON                          
031700     COMPUTE KORD-SUORDV       = SHUV-PRARTSTD * SHUV-KVBEART             
031800     MOVE SHUV-TIBEGPAC        TO  KORD-TIBEGPAC                          
031900     MOVE WS-DAGENS-DATUM      TO  KORD-TIUTSKR                           
032000     MOVE SHUV-VKORDNTO        TO  KORD-VKORDNTO                          
032100     MOVE SHUV-VLORDNTO        TO  KORD-VLORDNTO                          
032200                                                                          
032300     MOVE SHUV-IDARTNR         TO  W-IDARTNR-NUM                          
032400     MOVE W-IDARTNR-NUM        TO  KORD-IDARTNR-SATS                      
032500     MOVE SHUV-KVBEART         TO  W-KVBEART-NUM                          
032600     MOVE W-KVBEART-NUM        TO  KORD-KVBEART-SATS                      
032700     MOVE ZERO                 TO  KORD-SUORDV-EXP                        
032800     MOVE SPACE                TO  KORD-KDVALISO-EXP                      
032900                                                                          
033000     PERFORM IMS-ISRT-WDE401                                              
033100     .                                                                    
033200     EJECT                                                                
033300 CB-SKAPA-WDE401-NYCKEL SECTION.                                          
033400                                                                          
033500     MOVE KORD-IDDISTR         TO W-WDE4-IDDISTR                          
033600     MOVE KORD-IDKUNDNR        TO W-WDE4-IDKUNDNR                         
033700     MOVE KORD-IDKUNDRF        TO W-WDE4-IDKUNDRF                         
033800     MOVE KORD-IDPRODNR        TO W-WDE4-IDPRODNR                         
033900     MOVE KORD-IDPLKLST        TO W-WDE4-IDPLKLST                         
034000     .                                                                    
034100     EJECT                                                                
034200 CC-SKAPA-WDE411 SECTION.                                                 
034300                                                                          
034400     MOVE URAD-IDARTNR TO W-WDJ2-IDARTNR                                  
034500     PERFORM IMS-GHU-WDJ2-WLSATG11                                        
034600     MOVE JA TO SRAD-FLSATUTS                                             
034700     PERFORM IMS-REPL-WDJ2-WLSATG11                                       
034800                                                                          
034900     MOVE IDPURAD                 TO ORAD-IDPURAD                         
035000     MOVE URAD-IDARTNR            TO ORAD-IDARTNR                         
035100     MOVE URAD-REKSIFFR           TO ORAD-REKSIFFR                        
035200     MOVE URAD-ADLAGOMR           TO ORAD-ADLAGOMR                        
035300     MOVE WC-CDC-SE               TO ORAD-IDDC-RO                         
035400     MOVE ZERO                    TO ORAD-ADLEVPL                         
035500                                     ORAD-KDORDING                        
035600                                     ORAD-KDVRINFO                        
035700                                     ORAD-IDKUNDRF-RO (1:5)               
035800                                     ORAD-KDDSP                           
035900                                     ORAD-KDFARLIG                        
036000                                     ORAD-KDKVBRYT                        
036100                                     ORAD-KDOFFERT                        
036200                                     ORAD-KDOI                            
036300                                     ORAD-KDORDTYP                        
036400                                     ORAD-KDQPACK                         
036500                                     ORAD-KVANNANT                        
036600                                     ORAD-KVFLAMP                         
036700                                     ORAD-KVLEVART                        
036800                                     ORAD-KVSLATT                         
036900                                     ORAD-PRARTNTO                        
037000                                     ORAD-PRARTULL                        
037100                                     ORAD-TIPRIS                          
037200                                     ORAD-TIRODAT                         
037300                                     ORAD-IDKAMPRF                        
037400                                     ORAD-IDPSN                           
037500                                     ORAD-VKART-FG                        
037600                                     ORAD-VLFG                            
037700                                     ORAD-SUEQFG                          
037800                                     ORAD-IDLOPNR-RO                      
037900                                                                          
038000* OBS - HÄR INITIERAS NYA DDGS-FÄLT RENT GENERELLT                        
038100* OBS - KONTROLLERA OM DETTA ÄR KORREKT!!!                                
038200                                     ORAD-KDANNULL                        
038300                                     ORAD-TISLULEV                        
038400                                                                          
038500     MOVE SPACE                   TO ORAD-KDPRTYP                         
038600                                     ORAD-KDARTURS                        
038700                                     ORAD-BERADREF                        
038800                                     ORAD-IDKUNDRF-RO                     
038900                                     ORAD-BEVOLREF                        
039000                                     ORAD-IDLEVNR                         
039100                                     ORAD-CLEARGROUP                      
039200     MOVE NEJ                     TO ORAD-FLNOLLJ                         
039300                                     ORAD-FLDIRLEV                        
039400                                     ORAD-FLRESTN                         
039500     MOVE SHUV-IDPRODNR           TO ORAD-IDPRODNR                        
039600     MOVE URAD-IDKONTO            TO WS-IDKONTO-ALFA                      
039700     MOVE WS-IDKONTO-ALFA (2:1)   TO ORAD-KDFTG                           
039800     MOVE +68                     TO ORAD-KDFRAKT                         
039900     MOVE SHUV-KDORDKL            TO ORAD-KDORDKL                         
040000     MOVE URAD-KDPRODSL           TO ORAD-KDPRODSL                        
040100     MOVE 3                       TO ORAD-KDRADSTA                        
040200     MOVE URAD-IDKONTO            TO ORAD-IDKONTO                         
040300     MOVE URAD-IDANALYS           TO ORAD-IDANALYS                        
040400     MOVE URAD-IDKST              TO ORAD-IDKST                           
040500     MOVE URAD-REBEART            TO ORAD-KVAVBART                        
040600                                     ORAD-KVBEART                         
040700     MOVE WS-DAGENS-DATUM         TO ORAD-TIUTSKR                         
040800     MOVE URAD-VKARTNTO           TO ORAD-VKARTNTO                        
040900                                     ORAD-VKART-NTO-KG                    
041000     MOVE URAD-VLARTNTO           TO ORAD-VLARTNTO                        
041100     MOVE URAD-BEART              TO ORAD-BEART                           
041200     MOVE NEJ                     TO ORAD-FLFYSAVV                        
041300                                     ORAD-FLINVEST                        
041400                                     ORAD-FLPRTILL                        
041500                                     ORAD-FLTILLK                         
041600                                     ORAD-FLSDCLEV                        
041700     MOVE 'SATS'                  TO ORAD-IDSYSTEM                        
041800                                                                          
041900     MOVE SPACE                   TO ORAD-IDKUNDRF-WIP                    
042000     MOVE ZERO                    TO ORAD-PRAVCOST                        
042100     MOVE SPACE                   TO ORAD-KDVALISO-EXP                    
042200                                                                          
042300     INITIALIZE                    ORAD-DEAL-PR-LINE                      
042400                                                                          
042500     PERFORM CCA-HAMTA-BELGISK-MOMS                                       
042600                                                                          
042700     PERFORM IMS-ISRT-WDE411                                              
042800     .                                                                    
042900     EJECT                                                                
043000 CCA-HAMTA-BELGISK-MOMS SECTION.                                          
043100                                                                          
043200     MOVE +20      TO ORAD-KDTVA                                          
043300     .                                                                    
043400     EJECT                                                                
043500 CE-SKAPA-WDE601 SECTION.                                                 
043600                                                                          
043700     MOVE SHUV-IDPRODNR             TO VORD-IDPRODNR                      
043800     MOVE SHUV-IDDISTR              TO VORD-IDDISTR                       
043900     MOVE SHUV-IDKUNDNR             TO VORD-IDKUNDNR                      
044000     MOVE SHUV-KDORDKL              TO VORD-KDORDKL                       
044100     MOVE SHUV-IDANSK               TO VORD-KDPERSON                      
044200     MOVE NEJ                       TO VORD-FLCONTL                       
044300                                       VORD-FLDIRLEV                      
044400                                       VORD-FLSPARR                       
044500                                       VORD-FLMANORD                      
044600     MOVE WC-CDC-SE                 TO VORD-IDDC                          
044700     MOVE SHUV-KDFAKTYP             TO VORD-KDFAKTYP                      
044800     MOVE +68                       TO VORD-KDFRAKT                       
044900     MOVE 2                         TO VORD-KDMETOD                       
045000     MOVE 1                         TO VORD-KDORDSTA                      
045100     MOVE ZERO                      TO VORD-ADLEVPL                       
045200                                       VORD-IDLOTNR                       
045300                                       VORD-KVKOLLI                       
045400                                       VORD-KVKOLLI-FAKT                  
045500                                       VORD-KVKOLLI-LAST                  
045600                                       VORD-KVKOLLI-FL                    
045700                                       VORD-KVKOLPAC                      
045800                                       VORD-KVORDRAD-PACK                 
045900                                       VORD-SUORDV-FL                     
046000                                       VORD-SUORDV-PACK                   
046100                                       VORD-TIREGTID                      
046200                                       VORD-TIFAKT-SK                     
046300                                       VORD-TILASTN-SK                    
046400                                       VORD-TIPACKN-SK                    
046500                                       VORD-VKORDBTO                      
046600                                       VORD-VKORDBTO-FL                   
046700                                       VORD-VLORDBTO                      
046800                                       VORD-VLORDBTO-FL                   
046900                                       VORD-IDPRODNR-SAMP                 
047000                                       VORD-DARFS                         
047100                                                                          
047200* OBS - HÄR INITIERAS NYA DDGS-FÄLT RENT GENERELLT                        
047300* OBS - KONTROLLERA OM DETTA ÄR KORREKT!!!                                
047400     MOVE SPACE                     TO VORD-KDVIA                         
047500                                       VORD-IDLEVNR                       
047600                                                                          
047700     MOVE SHUV-TIBEGPAC             TO VORD-DABEGPAC                      
047800     IF SHUV-TIBEGPAC NOT = ZERO                                          
047900       IF SHUV-TIBEGPAC < 500000                                          
048000         MOVE 20                    TO VORD-DABEGPAC (1:2)                
048100       ELSE                                                               
048200         IF SHUV-TIBEGPAC < 999999                                        
048300           MOVE 19                  TO VORD-DABEGPAC (1:2)                
048400         ELSE                                                             
048500           MOVE 99999999            TO VORD-DABEGPAC                      
048600         END-IF                                                           
048700       END-IF                                                             
048800     END-IF                                                               
048900     MOVE WS-TIHHMMSS               TO VORD-TIUTSTID                      
049000     MOVE WS-DAGENS-DATUM           TO VORD-TIUTSKR                       
049100     MOVE IDPURAD                   TO VORD-KVORDRAD                      
049200     COMPUTE VORD-SUORDV = SHUV-PRARTSTD * SHUV-KVBEART                   
049300     MOVE SHUV-VKORDNTO             TO VORD-VKORDNTO                      
049400     MOVE SHUV-VLORDNTO             TO VORD-VLORDNTO                      
049500     MOVE JA                        TO VORD-FLAUTFAK                      
049600     MOVE NEJ                       TO VORD-FLFRAKTS                      
049700     MOVE SPACE                     TO VORD-KDORDLOT                      
049800     MOVE ZERO                      TO VORD-SUORDV-EXP                    
049900     MOVE SPACE                     TO VORD-KDVALISO-EXP                  
050000     MOVE SPACE                     TO VORD-IDDC-EXP                      
050100                                                                          
050200     INITIALIZE                  VORD-DEAL-PR-SUM                         
050300     MOVE   ZERO                    TO VORD-SUORDV-PACK-LOC               
050400                                       VORD-SUORDV-PACK-LOCPREL           
050500                                       VORD-SUORDV-FL-LOC                 
050600                                       VORD-SUORDV-FL-LOCPREL             
050700                                                                          
050800     PERFORM IMS-ISRT-WDE601                                              
050900     .                                                                    
051000     EJECT                                                                
051100 CG-UPPDAT-WDE401 SECTION.                                                
051200                                                                          
051300     PERFORM IMS-GHU-WDE401                                               
051400                                                                          
051500     MOVE IDPURAD    TO KORD-KVORDRAD                                     
051600                        KORD-KVORDRAD-VO                                  
051700                                                                          
051800     PERFORM IMS-REPL-WDE401                                              
051900     .                                                                    
052000     EJECT                                                                
052100 CH-SKAPA-RYITRANS SECTION.                                               
052200                                                                          
052300     MOVE 'RYI'           TO W-RYI-IDPTYP                                 
052400     MOVE WC-CDC-SE       TO W-RYI-IDDC                                   
052500     MOVE SRAD-IDARTNR    TO W-RYI-IDARTNR                                
052600     MOVE SHUV-IDDISTR    TO W-RYI-IDDISTR                                
052700     MOVE SHUV-KDORDKL    TO W-RYI-KDORDKL                                
052800     MOVE SRAD-REBEART    TO W-RYI-KVAVBART                               
052900     MOVE '2'             TO W-RYI-KDUPPD                                 
053000     MOVE 8               TO W-RYI-KDRORELS                               
053100     MOVE WS-DAGENS-DATUM TO W-RYI-TIUTSKR                                
053200                                                                          
053300     MOVE W-RYIPOST       TO ZZAC-LOGGPOST                                
053400     MOVE SPACE           TO ZZAC-SORTPOST                                
053500     MOVE WS-DAGENS-DATUM TO ZZAC-TIAAMMDD                                
053600     ACCEPT ZZAC-TIKLOCK FROM TIME                                        
053700     MOVE 1               TO ZZAC-IDLOGLOP                                
053800     PERFORM IMS-ISRT-ZZAC-WLZZAC01                                       
053900     PERFORM UNTIL SEGMENT-FINNS                                          
054000        ADD 1 TO ZZAC-IDLOGLOP                                            
054100        PERFORM IMS-ISRT-ZZAC-WLZZAC01                                    
054200        IF ZZAC-IDLOGLOP = 9                                              
054300           ADD 1 TO ZZAC-TIKLOCK                                          
054400        END-IF                                                            
054500     END-PERFORM                                                          
054600     .                                                                    
054700     EJECT                                                                
054800 D-UPPDAT-SATSORDER SECTION.                                              
054900                                                                          
055000     PERFORM IMS-GHU-WDJ2-WLSATG01                                        
055100                                                                          
055200     IF SEGMENT-FINNS                                                     
055300*    FIX FÖR ATT KLARA DUBBLA PRODNR. OBS SE ÄVEN FIX I CA-SKAPA-         
055400                                                                          
055500*       IF SHUV-IDPRODNR       = +135388                                  
055600*          MOVE +654324        TO SHUV-IDPRODNR                           
055700*       END-IF                                                            
055800                                                                          
055900        MOVE '4' TO SHUV-KDSATPLK                                         
056000        PERFORM IMS-REPL-WDJ2-WLSATG01                                    
056100     END-IF                                                               
056200     .                                                                    
056300     EJECT                                                                
056400* --- IMS SEKTIONER ---                                                   
056500     SKIP3                                                                
056600 IMS-GET-MSG SECTION.                                                     
056700                                                                          
056800     MOVE '  QC' TO GODK-STATUSKODER                                      
056900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
057000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057100     PERFORM IMS-STATUSKONTROLL                                           
057200     .                                                                    
057300     EJECT                                                                
057400 IMS-GHU-WDJ2-WLSATG01 SECTION.                                           
057500                                                                          
057600     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
057700          DELIMITED BY SIZE INTO SSA1                                     
057800     MOVE '  GE' TO GODK-STATUSKODER                                      
057900     CALL CBLTDLI USING GHU SATG1-PCB DLI-IO-AREA1 SSA1                   
058000     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
058100     PERFORM IMS-STATUSKONTROLL                                           
058200     .                                                                    
058300                                                                          
058400                                                                          
058500 IMS-REPL-WDJ2-WLSATG01 SECTION.                                          
058600                                                                          
058700     MOVE '    ' TO GODK-STATUSKODER                                      
058800     CALL CBLTDLI USING REPL SATG1-PCB DLI-IO-AREA1                       
058900     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
059000     PERFORM IMS-STATUSKONTROLL                                           
059100     .                                                                    
059200     EJECT                                                                
059300 IMS-GHU-WDJ2-WLSATG11 SECTION.                                           
059400                                                                          
059500     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
059600          DELIMITED BY SIZE INTO SSA1                                     
059700     STRING 'WLSATG11(IDARTNR  =' W-WDJ2-IDARTNR-X ')'                    
059800          DELIMITED BY SIZE INTO SSA2                                     
059900     MOVE '    ' TO GODK-STATUSKODER                                      
060000     CALL CBLTDLI USING GHU SATG2-PCB DLI-IO-AREA2 SSA1 SSA2              
060100     MOVE SATG2-STATUS-CODE TO STATUS-WS                                  
060200     PERFORM IMS-STATUSKONTROLL                                           
060300     .                                                                    
060400                                                                          
060500                                                                          
060600 IMS-REPL-WDJ2-WLSATG11 SECTION.                                          
060700                                                                          
060800     MOVE '    ' TO GODK-STATUSKODER                                      
060900     CALL CBLTDLI USING REPL SATG2-PCB DLI-IO-AREA2                       
061000     MOVE SATG2-STATUS-CODE TO STATUS-WS                                  
061100     PERFORM IMS-STATUSKONTROLL                                           
061200     .                                                                    
061300     EJECT                                                                
061400 IMS-GHNP-WDJ2-WLSATG12 SECTION.                                          
061500                                                                          
061600     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
061700          DELIMITED BY SIZE INTO SSA1                                     
061800     STRING 'WLSATG12(KDSATLI  =' W-WDJ2-KDSATLI-X ')'                    
061900          DELIMITED BY SIZE INTO SSA2                                     
062000     MOVE '  GE' TO GODK-STATUSKODER                                      
062100     CALL CBLTDLI USING GHNP SATG1-PCB DLI-IO-AREA3 SSA1 SSA2             
062200     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
062300     PERFORM IMS-STATUSKONTROLL                                           
062400     .                                                                    
062500                                                                          
062600                                                                          
062700 IMS-DLET-WDJ2-WLSATG12 SECTION.                                          
062800                                                                          
062900     MOVE '    ' TO GODK-STATUSKODER                                      
063000     CALL CBLTDLI USING DLET SATG1-PCB DLI-IO-AREA3                       
063100     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
063200     PERFORM IMS-STATUSKONTROLL                                           
063300     .                                                                    
063400     EJECT                                                                
063500 IMS-GHU-WDE401 SECTION.                                                  
063600                                                                          
063700     STRING 'WDE401  (WDE401KY =' W-WDE4-WDE4KEY-X ')'                    
063800          DELIMITED BY SIZE INTO SSA1                                     
063900     MOVE '  ' TO GODK-STATUSKODER                                        
064000     CALL CBLTDLI USING GHU WDE4-PCB DLI-IO-WDE401 SSA1                   
064100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
064200     PERFORM IMS-STATUSKONTROLL                                           
064300     .                                                                    
064400                                                                          
064500                                                                          
064600 IMS-REPL-WDE401 SECTION.                                                 
064700                                                                          
064800     MOVE '  ' TO GODK-STATUSKODER                                        
064900     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE401                       
065000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
065100     PERFORM IMS-STATUSKONTROLL                                           
065200     .                                                                    
065300                                                                          
065400 IMS-DLET-WDE401 SECTION.                                                 
065500                                                                          
065600     MOVE '  ' TO GODK-STATUSKODER                                        
065700     CALL CBLTDLI USING DLET WDE4-PCB DLI-IO-WDE401                       
065800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
065900     PERFORM IMS-STATUSKONTROLL                                           
066000     .                                                                    
066100                                                                          
066200                                                                          
066300 IMS-ISRT-WDE401 SECTION.                                                 
066400                                                                          
066500     MOVE 'WDE401   ' TO SSA1                                             
066600     MOVE '    ' TO GODK-STATUSKODER                                      
066700     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-WDE401 SSA1                  
066800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
066900     PERFORM IMS-STATUSKONTROLL                                           
067000     .                                                                    
067100     EJECT                                                                
067200 IMS-ISRT-WDE411 SECTION.                                                 
067300                                                                          
067400     STRING 'WDE401  (WDE401KY =' W-WDE4-WDE4KEY-X ')'                    
067500          DELIMITED BY SIZE INTO SSA1                                     
067600     MOVE 'WDE411   ' TO SSA2                                             
067700     MOVE '  ' TO GODK-STATUSKODER                                        
067800     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-WDE411 SSA1 SSA2             
067900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
068000     PERFORM IMS-STATUSKONTROLL                                           
068100     .                                                                    
068200     EJECT                                                                
068300 IMS-ISRT-WDE601 SECTION.                                                 
068400                                                                          
068500     MOVE 'WDE601   ' TO SSA1                                             
068600     MOVE '  ' TO GODK-STATUSKODER                                        
068700     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE601 SSA1                  
068800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
068900     PERFORM IMS-STATUSKONTROLL                                           
069000     .                                                                    
069100     EJECT                                                                
069200 IMS-ISRT-ZZAC-WLZZAC01 SECTION.                                          
069300                                                                          
069400     MOVE 'WLZZAC01 ' TO SSA1                                             
069500     MOVE '  II' TO GODK-STATUSKODER                                      
069600     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA6 SSA1                   
069700     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
069800     PERFORM IMS-STATUSKONTROLL                                           
069900     .                                                                    
070000     EJECT                                                                
070100 IMS-STATUSKONTROLL SECTION.                                              
070200                                                                          
070300     SET STATUS-IX TO 1                                                   
070400     SEARCH GODK-STATUS                                                   
070500       AT END                                                             
070600          STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS               
070700               DELIMITED BY SIZE INTO FELTEXT                             
070800          CALL FELLOG                                                     
070900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
071000     END-SEARCH                                                           
080000     .                                                                    
