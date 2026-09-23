000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4037900.                                                
000400 AUTHOR.         ROGER OLSSON.                                            
000500 DATE-WRITTEN.   91/02/28.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        DETTA PROGRAMMET UPPDATERAR WDE4 OCH WDE6, BEROENDE PÅ           
001100*        VAD SOM HAR BLIVIT ANGETT I AVVIKELSERAPPORTERINGEN.             
001200*                                                                         
001300*        LÄSER ANGIVEN SATSORDER (WLSATG01), KONTROLLERAR ATT             
001400*        STATUS = 6 ELLER 7                                               
001500*        RADER SOM HAR KDSATLI = 1 OCH 2 SKALL BEHANDLAS.                 
001600*        VARJE RAD (WLSATG12) SOM BEHANDLAS DELETAS EFTERÅT.              
001700*        NÄR SATSORDERN ÄR FÄRDIGBEHANDLAD SÄTTS STATUS TILL 8.           
001800*        PROGRAMMET JUSTERAR SEGMENT PÅ :                                 
001900*             - WDE4   - WDE401 (ORDERHUVUD)                              
002000*                      - WDE411 (ORDERRADER)                              
002100*                      - WDE430 (PRODNR-KOPPL WDE6)                       
002200*                                                                         
002300*             - WDE6   - WDE601 (KOLLI)                                   
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W4T379X.                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        UPPDATERING AV WDE4, WDE6.                                       
003000                                                                          
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 CONFIGURATION SECTION.                                                   
003400*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800*    -- CHECKED BY WY2000                                                 
003900     SKIP3                                                                
004000 77  IDPGM                       PIC X(08)   VALUE 'W4037900'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700                                                                          
004800 77  IDPURAD                     PIC S9(9)   VALUE +0   COMP SYNC.        
004900 77  WS-DELETE-SW                PIC 9       VALUE 0.                     
005000 77  WS-DAGENS-DATUM             PIC 9(6).                                
005100 77  WS-IDKONTO-ALFA             PIC X(11)   VALUE SPACE.                 
005200 77  W-KVORDRAD                  PIC 9(3)    VALUE 0.                     
005300 77  W-KVBEART-NUM               PIC 9(6)    VALUE 0.                     
005400                                                                          
005500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005600     88  ALLT-OK                             VALUE 'J'.                   
005700     88  ALLT-FEL                            VALUE 'N'.                   
005800                                                                          
005900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006000     88  GODK-MID                            VALUE '4374'.                
006100                                                                          
006200 01  WS-KLOCKAN.                                                          
006300     03 WS-TIHHMMSS              PIC 9(6).                                
006400     03 FILLER                   PIC X(2).                                
006500*      --- VALID IDDC CODES                                               
006600*                                                                         
006700*01    -COPY WWDCKONS                                                     
006800       EJECT                                                              
006900 01  FILLER                      PIC X(11)   VALUE 'WS-MSG-AREA'.         
007000 01  WS-MSG-AREA.                                                         
007100     03 WS-IDORDNSB              PIC X(5).                                
007200     03 WS-IDORDNSS              PIC X(1).                                
007300     EJECT                                                                
007400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007500 01  GENERELLA-SUBPROGRAM.                                                
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     EJECT                                                                
007900*    --- AREOR FÖR MSG HANTERING                                          
008000*                                                                         
008100 01  FILLER                     PIC X(16)   VALUE 'MSG-AREA'.             
008200*                                                                         
008300*01  -COPY WMSGAREA                                                       
008400     EJECT                                                                
008500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008600*                                                                         
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
008900 01  FILLER                      PIC X(11)    VALUE 'NYCKLAR-DL1'.        
009000 01  NYCKLAR-TILL-DLI.                                                    
009100                                                                          
009200*----> DIREKTNYCKEL TILL SATSORDER.                                       
009300                                                                          
009400     03  W-WDJ2-IDORDNST-X.                                               
009500         05 W-WDJ2-IDORDNSB      PIC S9(5) COMP-3.                        
009600         05 W-WDJ2-IDORDNSS      PIC S9(1) COMP-3.                        
009700                                                                          
009800     03  W-WDJ2-KDSATLI-X        PIC  X(1).                               
009900                                                                          
010000     03  W-WDJ2-IDARTNR-X.                                                
010100         05 W-WDJ2-IDARTNR       PIC S9(9) COMP-3.                        
010200                                                                          
010300*----> DIREKTNYCKEL TILL KUNDORDERREGISTER.                               
010400                                                                          
010500     03  W-WDE4-WDE4KEY-X.                                                
010600         05  W-WDE4-IDDISTR      PIC S9(5) COMP-3.                        
010700         05  W-WDE4-IDKUNDNR     PIC S9(7) COMP-3.                        
010800         05  W-WDE4-IDKUNDRF     PIC X(10).                               
010900         05  W-WDE4-IDPRODNR     PIC S9(7) COMP-3.                        
011000         05  W-WDE4-IDPLKLST     PIC S9(3) COMP-3.                        
011100                                                                          
011200*----> DIREKTNYCKEL TILL WDE411                                           
011300     03  W-WDE4-IDPURAD-X.                                                
011400         05  W-WDE4-IDPURAD      PIC S9(5) COMP-3.                        
011500                                                                          
011600*----> DIREKTNYCKEL TILL WDE601                                           
011700     03  W-WDE6-IDPRODNR-X.                                               
011800         05  W-WDE6-IDPRODNR     PIC S9(7) COMP-3.                        
011900                                                                          
012000*------- STATUS-KOD FRÅN IMS                                              
012100                                                                          
012200 01  STATUS-WS                   PIC X(2).                                
012300     88  SEGMENT-FINNS                       VALUE '  '.                  
012400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012500     88  END-OF-DATA                         VALUE 'GB'.                  
012600     SKIP2                                                                
012700 01  GODK-STATUSKODER.                                                    
012800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012900     SKIP3                                                                
013000 01  SSA1                        PIC X(96).                               
013100 01  SSA2                        PIC X(96).                               
013200     EJECT                                                                
013300*    --- IMS FUNKTIONSKODER                                               
013400*01  -COPY W0003                                                          
013500     EJECT                                                                
013600*    ---  DLI INPUT-OUTPUT AREA                                           
013700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013800     SKIP3                                                                
013900 01  DLI-IO-AREA1.                                                        
014000     03  WLSATG01.                                                        
014100*        05  -COPY WDJ201                                                 
014200     EJECT                                                                
014300 01  DLI-IO-AREA2.                                                        
014400     03  WLSATG11.                                                        
014500*        05  -COPY WDJ211                                                 
014600     EJECT                                                                
014700 01  DLI-IO-AREA3.                                                        
014800     03  WLSATG12.                                                        
014900*        05  -COPY WDJ212                                                 
015000     EJECT                                                                
015100 01  FILLER                     PIC X(16) VALUE 'IO-E401'.                
015200 01  DLI-IO-E401.                                                         
015300*    03  -COPY WDE401                                                     
015400     EJECT                                                                
015500 01  FILLER                     PIC X(16) VALUE 'IO-E411'.                
015600 01  DLI-IO-E411.                                                         
015700*    03  -COPY WDE411                                                     
015800     EJECT                                                                
015900 01  FILLER                     PIC X(16) VALUE 'IO-E601'.                
016000 01  DLI-IO-E601.                                                         
016100*    05  -COPY WDE601                                                     
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
017900 PROCEDURE DIVISION  USING MSG-PCB                                        
018000                           SATG1-PCB                                      
018100                           SATG2-PCB                                      
018200                           WDE4-PCB                                       
018300                           WDE6-PCB.                                      
018400                                                                          
018500     ENTRY 'DLITCBL' USING MSG-PCB                                        
018600                           SATG1-PCB                                      
018700                           SATG2-PCB                                      
018800                           WDE4-PCB                                       
018900                           WDE6-PCB.                                      
019000                                                                          
019100     PERFORM IMS-GET-MSG                                                  
019200     IF SEGMENT-FINNS                                                     
019300        PERFORM A-INIT                                                    
019400        IF ALLT-OK                                                        
019500           PERFORM B-LAES-SATSORDER                                       
019600           IF ALLT-OK                                                     
019700              PERFORM C-BEHANDLA-WDE4-WDE6                                
019800              PERFORM D-UPPDAT-SATSORDER                                  
019900           END-IF                                                         
020000        END-IF                                                            
020100     END-IF                                                               
020200                                                                          
020300     MOVE ZERO TO RETURN-CODE                                             
020400     GOBACK                                                               
020500     .                                                                    
020600     EJECT                                                                
020700 A-INIT SECTION.                                                          
020800                                                                          
020900     IF MSG-KDTRANS-1  NOT = 'W4T379X '                                   
021000        MOVE NEJ TO ALLT-SW                                               
021100     END-IF                                                               
021200                                                                          
021300     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
021400     IF NOT GODK-MID                                                      
021500        MOVE NEJ TO ALLT-SW                                               
021600     END-IF                                                               
021700                                                                          
021800     IF ALLT-OK                                                           
021900        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO WS-MSG-AREA                   
022000        MOVE WS-IDORDNSB                 TO W-WDJ2-IDORDNSB               
022100        MOVE WS-IDORDNSS                 TO W-WDJ2-IDORDNSS               
022200        ACCEPT WS-KLOCKAN                FROM TIME                        
022300        ACCEPT WS-DAGENS-DATUM           FROM DATE                        
022400     END-IF                                                               
022500     .                                                                    
022600     EJECT                                                                
022700 B-LAES-SATSORDER SECTION.                                                
022800                                                                          
022900     PERFORM IMS-GHU-WDJ2-WLSATG01                                        
023000                                                                          
023100     IF SHUV-KDSATPLK NOT = '7'                                           
023200        MOVE NEJ TO ALLT-SW                                               
023300     END-IF                                                               
023400     .                                                                    
023500     EJECT                                                                
023600 C-BEHANDLA-WDE4-WDE6 SECTION.                                            
023700                                                                          
023800     IF SHUV-FLSATNOL = NEJ                                               
023900        PERFORM CA-UPPDATERA-WDE4-WDE6                                    
024000     ELSE                                                                 
024100        PERFORM CB-TABORT-WDE4-WDE6                                       
024200     END-IF                                                               
024300     .                                                                    
024400     EJECT                                                                
024500                                                                          
024600****************************************************************          
024700*  VID PLOCKNING HAR INTE ALLA INGÅENDE ARTIKLAR HITTATS,      *          
024800*  WDE4 OCH WDE6 MÅSTE DÄRFÖR UPPDATERAS MED VAD SOM BLIR      *          
024900*  LEVERERAT.                                                  *          
025000****************************************************************          
025100 CA-UPPDATERA-WDE4-WDE6 SECTION.                                          
025200                                                                          
025300     PERFORM CAA-JUSTERA-WDE401                                           
025400                                                                          
025500     PERFORM CAB-TA-BORT-WDE411                                           
025600                                                                          
025700     MOVE '1'                  TO W-WDJ2-KDSATLI-X                        
025800     PERFORM IMS-GHNP-WDJ2-WLSATG12                                       
025900                                                                          
026000     PERFORM UNTIL SEGMENT-SAKNAS                                         
026100        ADD 1 TO IDPURAD                                                  
026200                 W-KVORDRAD                                               
026300        PERFORM CAE-SKAPA-WDE411                                          
026400        PERFORM IMS-DLET-WDJ2-WLSATG12                                    
026500        PERFORM IMS-GHNP-WDJ2-WLSATG12                                    
026600     END-PERFORM                                                          
026700                                                                          
026800     PERFORM CAC-JUSTERA-WDE401-DEL2                                      
026900                                                                          
027000     PERFORM IMS-GHU-WDJ2-WLSATG01                                        
027100     MOVE '2'                  TO W-WDJ2-KDSATLI-X                        
027200     PERFORM IMS-GHNP-WDJ2-WLSATG12                                       
027300                                                                          
027400     PERFORM UNTIL SEGMENT-SAKNAS                                         
027500        PERFORM IMS-DLET-WDJ2-WLSATG12                                    
027600        PERFORM IMS-GHNP-WDJ2-WLSATG12                                    
027700     END-PERFORM                                                          
027800                                                                          
027900     PERFORM CAD-JUSTERA-WDE601                                           
028000     .                                                                    
028100     EJECT                                                                
028200 CAA-JUSTERA-WDE401 SECTION.                                              
028300                                                                          
028400     MOVE SHUV-IDDISTR         TO  W-WDE4-IDDISTR                         
028500     MOVE SHUV-IDKUNDNR        TO  W-WDE4-IDKUNDNR                        
028600     MOVE SPACE                TO  W-WDE4-IDKUNDRF                        
028700     MOVE WS-IDORDNSB (2:4)    TO  W-WDE4-IDKUNDRF (1:4)                  
028800     MOVE WS-IDORDNSS          TO  W-WDE4-IDKUNDRF (5:1)                  
028900     MOVE SHUV-IDPRODNR        TO  W-WDE4-IDPRODNR                        
029000     MOVE 1                    TO  W-WDE4-IDPLKLST                        
029100     PERFORM IMS-GHU-WDE401                                               
029200                                                                          
029300     COMPUTE KORD-SUORDV        = SHUV-PRARTSTD * SHUV-KVBEART            
029400     MOVE SHUV-VKORDNTO        TO  KORD-VKORDNTO                          
029500     MOVE SHUV-VLORDNTO        TO  KORD-VLORDNTO                          
029600                                                                          
029700     PERFORM IMS-REPL-WDE401                                              
029800     .                                                                    
029900     EJECT                                                                
030000 CAB-TA-BORT-WDE411 SECTION.                                              
030100                                                                          
030200     PERFORM IMS-GHNP-WDE411                                              
030300                                                                          
030400     PERFORM UNTIL SEGMENT-SAKNAS                                         
030500                                                                          
030600        PERFORM IMS-DLET-WDE411                                           
030700        PERFORM IMS-GHNP-WDE411                                           
030800                                                                          
030900     END-PERFORM                                                          
031000     .                                                                    
031100     EJECT                                                                
031200 CAC-JUSTERA-WDE401-DEL2 SECTION.                                         
031300                                                                          
031400     PERFORM IMS-GHU-WDE401                                               
031500                                                                          
031600* W-KVORDRAD TILL KORD-KVORDRAD PGA ATT INNAN E410 TOGS BORT SÅ           
031700* LADES W-KVORDRAD I KORDT-KVORDRAD.                                      
031800       MOVE W-KVORDRAD         TO KORD-KVORDRAD                           
031900       MOVE SHUV-KVBEART       TO W-KVBEART-NUM                           
032000       MOVE W-KVBEART-NUM      TO KORD-KVBEART-SATS                       
032100*                                 WS-KORD-KVBEART-SATS                    
032200                                                                          
032300     PERFORM IMS-REPL-WDE401                                              
032400     .                                                                    
032500     EJECT                                                                
032600 CAD-JUSTERA-WDE601 SECTION.                                              
032700                                                                          
032800     MOVE SHUV-IDPRODNR             TO W-WDE6-IDPRODNR                    
032900     PERFORM IMS-GHU-WDE601                                               
033000                                                                          
033100     COMPUTE VORD-SUORDV = SHUV-PRARTSTD * SHUV-KVBEART                   
033200     MOVE SHUV-VKORDNTO             TO VORD-VKORDNTO                      
033300     MOVE SHUV-VLORDNTO             TO VORD-VLORDNTO                      
033400                                                                          
033500     PERFORM IMS-REPL-WDE601                                              
033600     .                                                                    
033700     EJECT                                                                
033800 CAE-SKAPA-WDE411 SECTION.                                                
033900                                                                          
034000     MOVE IDPURAD                 TO ORAD-IDPURAD                         
034100     MOVE URAD-IDARTNR            TO ORAD-IDARTNR                         
034200     MOVE URAD-REKSIFFR           TO ORAD-REKSIFFR                        
034300     MOVE URAD-ADLAGOMR           TO ORAD-ADLAGOMR                        
034400     MOVE WC-CDC-SE               TO ORAD-IDDC-RO                         
034500     MOVE SPACE                   TO ORAD-BERADREF                        
034600                                     ORAD-IDKUNDRF-RO                     
034700                                     ORAD-BEVOLREF                        
034800                                     ORAD-KDARTURS                        
034900                                     ORAD-KDPRTYP                         
035000                                     ORAD-IDBIL                           
035100                                     ORAD-IDKLIENT                        
035200                                     ORAD-IDARBREF                        
035300                                     ORAD-IDVIN                           
035400                                     ORAD-IDLEVNR                         
035500     MOVE NEJ                     TO ORAD-FLNOLLJ                         
035600                                     ORAD-FLDIRLEV                        
035700                                     ORAD-FLRESTN                         
035800                                     ORAD-FLFYSAVV                        
035900                                     ORAD-FLINVEST                        
036000                                     ORAD-FLPRTILL                        
036100                                     ORAD-FLTILLK                         
036200     MOVE 'N'                     TO ORAD-FLSDCLEV                        
036300     MOVE SHUV-IDPRODNR           TO ORAD-IDPRODNR                        
036400     MOVE ZERO                    TO ORAD-KDORDING                        
036500                                     ORAD-ADLEVPL                         
036600                                     ORAD-KDVRINFO                        
036700                                     ORAD-IDKUNDRF-RO (1:5)               
036800                                     ORAD-KDDSP                           
036900                                     ORAD-KDFARLIG                        
037000                                     ORAD-KDKVBRYT                        
037100                                     ORAD-KDOFFERT                        
037200                                     ORAD-KDOI                            
037300                                     ORAD-KDORDTYP                        
037400                                     ORAD-KDQPACK                         
037500                                     ORAD-KVANNANT                        
037600                                     ORAD-KVFLAMP                         
037700                                     ORAD-KVLEVART                        
037800                                     ORAD-KVSLATT                         
037900                                     ORAD-PRARTNTO                        
038000                                     ORAD-PRARTULL                        
038100                                     ORAD-TIPRIS                          
038200                                     ORAD-TIRODAT                         
038300                                     ORAD-IDKAMPRF                        
038400                                     ORAD-KDANNULL                        
038500                                     ORAD-TISLULEV                        
038600     MOVE URAD-IDKONTO            TO WS-IDKONTO-ALFA                      
038700     MOVE WS-IDKONTO-ALFA (2:1)   TO ORAD-KDFTG                           
038800     MOVE +68                     TO ORAD-KDFRAKT                         
038900     MOVE SHUV-KDORDKL            TO ORAD-KDORDKL                         
039000     MOVE URAD-KDPRODSL           TO ORAD-KDPRODSL                        
039100     MOVE 3                       TO ORAD-KDRADSTA                        
039200     MOVE URAD-IDKONTO            TO ORAD-IDKONTO                         
039300     MOVE URAD-IDANALYS           TO ORAD-IDANALYS                        
039400     MOVE URAD-IDKST              TO ORAD-IDKST                           
039500     MOVE URAD-REBEART            TO ORAD-KVAVBART                        
039600                                     ORAD-KVBEART                         
039700                                     ORAD-KVLEVART                        
039800     MOVE WS-DAGENS-DATUM         TO ORAD-TIUTSKR                         
039900     MOVE URAD-VKARTNTO           TO ORAD-VKARTNTO                        
039910                                     ORAD-VKART-NTO-KG                    
040000     MOVE URAD-VLARTNTO           TO ORAD-VLARTNTO                        
040100     MOVE URAD-BEART              TO ORAD-BEART                           
040200     MOVE 1                       TO ORAD-IDLOPNR-RO                      
040300     MOVE 'SATS'                  TO ORAD-IDSYSTEM                        
040400     MOVE +20                     TO ORAD-KDTVA                           
040500     INITIALIZE                      ORAD-DEAL-PR-LINE                    
040600                                                                          
040700     MOVE SPACE                   TO ORAD-IDKUNDRF-WIP                    
040800     MOVE +0                      TO ORAD-PRAVCOST                        
040810     MOVE SPACE                   TO ORAD-KDVALISO-EXP                    
040900     MOVE SPACE                   TO ORAD-CLEARGROUP                      
041000                                                                          
041100     PERFORM IMS-ISRT-WDE411                                              
041200     .                                                                    
041300     EJECT                                                                
041400                                                                          
041500****************************************************************          
041600*  VID PLOCKNING ÄR DET MINST 1 ST INGÅENDE ARTIKEL SOM SAKNAS *          
041700*  HELT. HELA ORDERN HAR LAGTS TILLBAKA SOM "EJ BYGGBAR".      *          
041800*  WDE4 OCH WDE6 BLIR DÄRFÖR BORTTAGNA FÖR ORDERN.             *          
041900****************************************************************          
042000 CB-TABORT-WDE4-WDE6 SECTION.                                             
042100                                                                          
042200     MOVE ZERO TO WS-DELETE-SW                                            
042300     MOVE '2'                  TO W-WDJ2-KDSATLI-X                        
042400     PERFORM IMS-GHNP-WDJ2-WLSATG12                                       
042500                                                                          
042600     PERFORM UNTIL SEGMENT-SAKNAS                                         
042700                                                                          
042800        IF WS-DELETE-SW = ZERO                                            
042900           MOVE SHUV-IDPRODNR     TO W-WDE6-IDPRODNR                      
043000           PERFORM IMS-GHU-WDE601                                         
043100           PERFORM IMS-DLET-WDE601                                        
043200                                                                          
043300           MOVE SHUV-IDDISTR      TO  W-WDE4-IDDISTR                      
043400           MOVE SHUV-IDKUNDNR     TO  W-WDE4-IDKUNDNR                     
043500           MOVE SPACE             TO  W-WDE4-IDKUNDRF                     
043600           MOVE WS-IDORDNSB (2:4) TO  W-WDE4-IDKUNDRF (1:4)               
043700           MOVE WS-IDORDNSS       TO  W-WDE4-IDKUNDRF (5:1)               
043800           MOVE SHUV-IDPRODNR     TO  W-WDE4-IDPRODNR                     
043900           MOVE 1                 TO  W-WDE4-IDPLKLST                     
044000           PERFORM IMS-GHU-WDE401                                         
044100           PERFORM IMS-DLET-WDE401                                        
044200           MOVE 1                 TO WS-DELETE-SW                         
044300        END-IF                                                            
044400                                                                          
044500        PERFORM IMS-DLET-WDJ2-WLSATG12                                    
044600        PERFORM IMS-GHNP-WDJ2-WLSATG12                                    
044700                                                                          
044800     END-PERFORM                                                          
044900     .                                                                    
045000     EJECT                                                                
045100                                                                          
045200 D-UPPDAT-SATSORDER SECTION.                                              
045300                                                                          
045400     PERFORM IMS-GHU-WDJ2-WLSATG01                                        
045500                                                                          
045600     IF SEGMENT-FINNS                                                     
045700        MOVE '8' TO SHUV-KDSATPLK                                         
045800        MOVE NEJ TO SHUV-FLSATNOL                                         
045900        PERFORM IMS-REPL-WDJ2-WLSATG01                                    
046000     END-IF                                                               
046100     .                                                                    
046200     EJECT                                                                
046300*                                                                         
046400*                                                                         
046500* --- IMS SEKTIONER ---                                                   
046600     SKIP3                                                                
046700 IMS-GET-MSG SECTION.                                                     
046800                                                                          
046900     MOVE '  QC' TO GODK-STATUSKODER                                      
047000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
047100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
047200     PERFORM IMS-STATUSKONTROLL                                           
047300     .                                                                    
047400     EJECT                                                                
047500 IMS-GHU-WDJ2-WLSATG01 SECTION.                                           
047600                                                                          
047700     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
047800          DELIMITED BY SIZE INTO SSA1                                     
047900     MOVE '    ' TO GODK-STATUSKODER                                      
048000     CALL CBLTDLI USING GHU SATG1-PCB DLI-IO-AREA1 SSA1                   
048100     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
048200     PERFORM IMS-STATUSKONTROLL                                           
048300     .                                                                    
048400                                                                          
048500                                                                          
048600 IMS-REPL-WDJ2-WLSATG01 SECTION.                                          
048700                                                                          
048800     MOVE '    ' TO GODK-STATUSKODER                                      
048900     CALL CBLTDLI USING REPL SATG1-PCB DLI-IO-AREA1                       
049000     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
049100     PERFORM IMS-STATUSKONTROLL                                           
049200     .                                                                    
049300     EJECT                                                                
049400                                                                          
049500                                                                          
049600 IMS-GHNP-WDJ2-WLSATG12 SECTION.                                          
049700                                                                          
049800     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
049900          DELIMITED BY SIZE INTO SSA1                                     
050000     STRING 'WLSATG12(KDSATLI  =' W-WDJ2-KDSATLI-X ')'                    
050100          DELIMITED BY SIZE INTO SSA2                                     
050200     MOVE '  GE' TO GODK-STATUSKODER                                      
050300     CALL CBLTDLI USING GHNP SATG1-PCB DLI-IO-AREA3 SSA1 SSA2             
050400     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
050500     PERFORM IMS-STATUSKONTROLL                                           
050600     .                                                                    
050700                                                                          
050800                                                                          
050900 IMS-DLET-WDJ2-WLSATG12 SECTION.                                          
051000                                                                          
051100     MOVE '    ' TO GODK-STATUSKODER                                      
051200     CALL CBLTDLI USING DLET SATG1-PCB DLI-IO-AREA3                       
051300     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
051400     PERFORM IMS-STATUSKONTROLL                                           
051500     .                                                                    
051600     EJECT                                                                
051700                                                                          
051800                                                                          
051900 IMS-GHU-WDE401 SECTION.                                                  
052000                                                                          
052100     STRING 'WDE401  (WDE401KY =' W-WDE4-WDE4KEY-X ')'                    
052200          DELIMITED BY SIZE INTO SSA1                                     
052300     MOVE '  ' TO GODK-STATUSKODER                                        
052400     CALL CBLTDLI USING GHU WDE4-PCB DLI-IO-E401 SSA1                     
052500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
052600     PERFORM IMS-STATUSKONTROLL                                           
052700     .                                                                    
052800                                                                          
052900 IMS-GHNP-WDE411 SECTION.                                                 
053000                                                                          
053100     MOVE   'WDE411   '       TO SSA1                                     
053200     MOVE '  GE' TO GODK-STATUSKODER                                      
053300     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-E411 SSA1                    
053400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
053500     PERFORM IMS-STATUSKONTROLL                                           
053600     .                                                                    
053700                                                                          
053800 IMS-ISRT-WDE411 SECTION.                                                 
053900                                                                          
054000     STRING 'WDE401  (WDE401KY =' W-WDE4-WDE4KEY-X ')'                    
054100          DELIMITED BY SIZE INTO SSA1                                     
054200     MOVE 'WDE411   ' TO SSA2                                             
054300     MOVE '  ' TO GODK-STATUSKODER                                        
054400     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-E411 SSA1 SSA2               
054500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
054600     PERFORM IMS-STATUSKONTROLL                                           
054700     .                                                                    
054800     EJECT                                                                
054900                                                                          
055000 IMS-DLET-WDE401 SECTION.                                                 
055100                                                                          
055200     MOVE '  ' TO GODK-STATUSKODER                                        
055300     CALL CBLTDLI USING DLET WDE4-PCB DLI-IO-E401                         
055400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
055500     PERFORM IMS-STATUSKONTROLL                                           
055600     .                                                                    
055700                                                                          
055800                                                                          
055900 IMS-DLET-WDE411 SECTION.                                                 
056000                                                                          
056100     MOVE '  ' TO GODK-STATUSKODER                                        
056200     CALL CBLTDLI USING DLET WDE4-PCB DLI-IO-E411                         
056300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
056400     PERFORM IMS-STATUSKONTROLL                                           
056500     .                                                                    
056600                                                                          
056700 IMS-REPL-WDE401 SECTION.                                                 
056800                                                                          
056900     MOVE '  ' TO GODK-STATUSKODER                                        
057000     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E401                         
057100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
057200     PERFORM IMS-STATUSKONTROLL                                           
057300     .                                                                    
057400                                                                          
057500 IMS-GHU-WDE601 SECTION.                                                  
057600                                                                          
057700     STRING 'WDE601  (IDPRODNR =' W-WDE6-IDPRODNR-X ')'                   
057800          DELIMITED BY SIZE INTO SSA1                                     
057900     MOVE '  ' TO GODK-STATUSKODER                                        
058000     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E601 SSA1                     
058100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
058200     PERFORM IMS-STATUSKONTROLL                                           
058300     .                                                                    
058400                                                                          
058500 IMS-DLET-WDE601 SECTION.                                                 
058600                                                                          
058700     MOVE 'WDE601   ' TO SSA1                                             
058800     MOVE '  ' TO GODK-STATUSKODER                                        
058900     CALL CBLTDLI USING DLET WDE6-PCB DLI-IO-E601 SSA1                    
059000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
059100     PERFORM IMS-STATUSKONTROLL                                           
059200     .                                                                    
059300     EJECT                                                                
059400                                                                          
059500 IMS-REPL-WDE601 SECTION.                                                 
059600                                                                          
059700     MOVE '  ' TO GODK-STATUSKODER                                        
059800     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
059900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
060000     PERFORM IMS-STATUSKONTROLL                                           
060100     .                                                                    
060200     EJECT                                                                
060300 IMS-STATUSKONTROLL SECTION.                                              
060400                                                                          
060500     SET STATUS-IX TO 1                                                   
060600     SEARCH GODK-STATUS                                                   
060700       AT END                                                             
060800          STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS               
060900               DELIMITED BY SIZE INTO FELTEXT                             
061000          CALL FELLOG                                                     
061100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
061200     END-SEARCH                                                           
061300     .                                                                    
061400                                                                          
