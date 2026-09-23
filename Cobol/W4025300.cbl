000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4025300.                                                
000400 AUTHOR.         STEFAN KIHLBERG                                          
000500 DATE-WRITTEN.   SEPT-98.                                                 
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR EN BAKGRUNDSTRANS FÖR UTVALDA ORDERBE-             
001100*        KRÄFTELSER FRÅN VIPS.                                            
001200*        ORDERBEKRÄFTELSERNA SKALL ANVÄNDAS TILL DELIVERY                 
001300*        NOTE FÖR NORDAMERIKA. PROGRAMMET HÄMTAR ORDERBE-                 
001400*        KRÄFTELSER FRÅN DISBATCHERN. OM KUNDORDERN REDAN FINNS           
001500*        PÅ Q5 SKICKAS ORDERBEKRÄFTELSERNA DIT VIA W411DNOT,              
001600*        ANNARS LÄGGS DE UPP PÅ EN HÄNDELSEBAS.                           
001700*                                                                         
002300*                                                                         
002400*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
002500*        PROGRAMMET LÄSER              WDQ5A  DEL NOTES                   
002600*        PROGRAMMET UPPDATERAR WL4013 (WDR4)  VIPSORDERBEKR               
002700*                              WL4014                                     
004300*                                                                         
004400*    INDATA.                                                              
004500*        TRANSAKTION: W4T253X                                             
004600*        MID:         W4I25301                                            
004700*                     WMSGKOM                                             
004800*    UTDATA.                                                              
004900*        MOD:         WMSGKOM    FEL/KLAR MED TILL DISPATHER              
005100     EJECT                                                                
005200 ENVIRONMENT DIVISION.                                                    
005300                                                                          
005400 DATA DIVISION.                                                           
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700*    -- CHECKED BY WY2000                                                 
005800     SKIP3                                                                
005900 77  IDPGM                       PIC X(08)   VALUE 'W4025300'.            
006000 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
006100 77  YES                         PIC X(1)   VALUE 'Y'.                    
006200 77  JA                          PIC X(1)   VALUE 'J'.                    
006300 77  NEJ                         PIC X(1)   VALUE 'N'.                    
006500 77  MID-IX                      PIC S9(9)   COMP-3 VALUE ZERO.           
006600 77  MAX-MID-IX                  PIC S9(9)   COMP-3 VALUE +14.            
007600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007700                                                                          
007800     EJECT                                                                
009300 77  ALLT-SW                       PIC X       VALUE 'J'.                 
009400     88  ALLT-OK                               VALUE 'J'.                 
009500                                                                          
012800 01  WS-ALFA-1.                                                           
012900     03  WS-NUM-1                  PIC 9(1).                              
013000 01  WS-ALFA-6.                                                           
013100     03  WS-NUM-6                  PIC 9(6).                              
013200                                                                          
013210 01  ARBETSFALT.                                                          
013300     03  WS-IDARTNR                PIC  9(11) VALUE ZERO.                 
013700                                                                          
013800     03  WS-TITIREGD-9KOMPL        PIC  9(8).                             
013900     03  FILLER REDEFINES WS-TITIREGD-9KOMPL.                             
014000       05  WS-SEKEL-9KOMPL         PIC  9(2).                             
014100       05  WS-AAMMDD-9KOMPL        PIC  9(6).                             
014200                                                                          
014201                                                                          
014228     03 DAGENS-DATUM-SSAAMMDD      PIC   X(08) VALUE ZERO.                
014229                                                                          
014230     EJECT                                                                
014300                                                                          
014310 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
014320*01  FILLER   -COPY WWDIST07    -RED TEST-IDDISTR.                        
014330     EJECT                                                                
014340                                                                          
014400 01  MESSAGE-CODES.                                                       
014530     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
014540                                                                          
014600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014700 01  GENERELLA-SUBPROGRAM.                                                
014800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015200 01  RKOD-ABEND-33               PIC S9(4) COMP VALUE +33.                
015300*                                                                         
015400     EJECT                                                                
015500*    --- PARAMETRAR TILL GENERELLA SUBPROGRAM                             
015600*                                                                         
016000 01  GEMENSAMMA-SUBPROGRAM.                                               
016100     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
020100     EJECT                                                                
020200                                                                          
020900*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
021000*                                                                         
021100 01 FILLER                       PIC X(8) VALUE 'W411DNOT'.               
021200*   -COPY W411DNOT                                                        
021300     EJECT                                                                
021400 01  MESSAGE-CODES.                                                       
021500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
021600     EJECT                                                                
027100*    --- AREOR FÖR MID                                                    
027200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
027300     SKIP3                                                                
027400 01  MID-AREA.                                                            
027500*03  MID -COPY W4I25301                                                   
027600     EJECT                                                                
027700 01  FILLER                      PIC X(16)  VALUE 'MSG-IO-AREA'.          
027800     SKIP3                                                                
027900*01  -COPY WMSGAREA                                                       
028000     EJECT                                                                
028300 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
028400     SKIP3                                                                
028500 01  KOM-IO-AREA.                                                         
028600*03  -COPY WMSGKOM                                                        
028700     EJECT                                                                
028800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028900*                                                                         
029000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
029100                                                                          
029200 01  NYCKLAR-TILL-DLI.                                                    
029300                                                                          
029400     03 W-WDQ5A1KY-MIN-X.                                                 
029410       05  W-IDGMTREF-MIN.                                                
029420         07 W-IDDISTR-MIN          PIC S9(05)  VALUE ZERO COMP-3.         
029430         07 W-IDKUNDNR-MIN         PIC S9(07)  VALUE ZERO COMP-3.         
029440         07 W-IDKUNDRF-GRP-MIN.                                           
029450           09 W-IDKUNDRF-MIN       PIC  X(10).                            
029460           09 W-IDORDNR5 REDEFINES W-IDKUNDRF-MIN.                        
029470             11 W-IDORDNR5-MIN     PIC  9(05).                            
029471             11 FILLER             PIC  X(05).                            
029480           09 W-IDORDNR7 REDEFINES W-IDKUNDRF-MIN.                        
029490             11 W-IDORDNR7-MIN     PIC  9(07).                            
029491             11 FILLER             PIC  X(03).                            
029530       05  FILLER                  PIC  X(17)   VALUE LOW-VALUE.          
030000                                                                          
030001                                                                          
030002     03 W-WDQ5A1KY-MAX-X.                                                 
030003       05  W-IDGMTREF-MAX.                                                
030004         07 W-IDDISTR-MAX          PIC S9(05)   VALUE ZERO                
030005                                                COMP-3.                   
030006         07 W-IDKUNDNR-MAX         PIC S9(07)   VALUE ZERO                
030007                                                COMP-3.                   
030008         07 W-IDKUNDRF-GRP-MAX.                                           
030009           09 W-IDKUNDRF-MAX       PIC  X(10).                            
030010           09 W-IDORDNR5 REDEFINES W-IDKUNDRF-MAX.                        
030011             11 W-IDORDNR5-MAX     PIC  9(05).                            
030012             11 FILLER             PIC  X(05).                            
030013           09 W-IDORDNR7 REDEFINES W-IDKUNDRF-MAX.                        
030014             11 W-IDORDNR7-MAX     PIC  9(07).                            
030015             11 FILLER             PIC  X(03).                            
030016       05  FILLER                  PIC  X(17)   VALUE HIGH-VALUE.         
030025                                                                          
030026                                                                          
030027     03  W-4013-WDGXKEY-X.                                                
030028         05  W-4013-IDHTYP       PIC  X(04) VALUE '4013'.                 
030029         05  W-4013-LOW-VALUE    PIC  X(26) VALUE LOW-VALUE.              
030030                                                                          
030031     03  W-4014-IDGMTREF-X.                                               
030032         05  W-4014-IDDISTR      PIC S9(05) VALUE ZERO COMP-3.            
030033         05  W-4014-IDKUNDNR     PIC S9(07) VALUE ZERO COMP-3.            
030034         05  W-4014-IDKUNDRF.                                             
030035            07  W-4014-IDORDNR7  PIC  9(07) VALUE ZERO.                   
030036            07  FILLER           PIC  X(03) VALUE SPACE.                  
030037*                                                                         
030038     03  W-KY4016-X.                                                      
030039         05  W-4016-IDARTNR      PIC S9(09) VALUE ZERO COMP-3.            
030040         05  W-4016-IDLOPNR      PIC S9(03) VALUE ZERO COMP-3.            
030041         05  W-4016-IDSEKVNR     PIC S9(03) VALUE ZERO COMP-3.            
030050         05  W-4016-KDORDBEK     PIC  9(02) VALUE ZERO.                   
030900                                                                          
032300     EJECT                                                                
032400                                                                          
032500*    --- STATUS-KOD FRÅN IMS                                              
032600 01  STATUS-WS                   PIC XX.                                  
032700     88  SEGMENT-FINNS                       VALUE '  '.                  
032800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
033000     88  BASEN-SLUT                          VALUE 'GB'.                  
033100     SKIP2                                                                
033200 01  GODK-STATUSKODER.                                                    
033300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
033400                                                                          
033500 01  SSA1                        PIC X(160).                              
033600 01  SSA2                        PIC X(96).                               
033700 01  SSA3                        PIC X(96).                               
033800 01  SSA4                        PIC X(96).                               
033900     EJECT                                                                
034000                                                                          
034100*    --- IMS FUNKTIONSKODER                                               
034200*01  -COPY W0003                                                          
034300     EJECT                                                                
034400*    ---  DLI INPUT-OUTPUT AREA                                           
034500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
034600     SKIP3                                                                
034700 01  FILLER                      PIC X(16)   VALUE 'WDQ5A1-AREA'.         
034800 01  DLI-IO-WDQ5A.                                                        
035000*    03  -COPY WDQ5A1                                                     
035100     EJECT                                                                
035110                                                                          
035200 01  FILLER                      PIC X(16) VALUE 'WL401301-AREA'.         
035300 01  DLI-IO-401301.                                                       
035400     03  WL401301.                                                        
035500*        05  -COPY WDGX01                                                 
035501                                                                          
035510 01  FILLER                      PIC X(16) VALUE 'WL401311-AREA'.         
035520 01  DLI-IO-401311.                                                       
035530     03  WL401311.                                                        
035540*        05  -COPY WDGX4014                                               
035541                                                                          
035550 01  FILLER                      PIC X(16) VALUE 'WL401321-AREA'.         
035560 01  DLI-IO-401321.                                                       
035570     03  WL401321.                                                        
035580*        05  -COPY WDGX4016                                               
035590                                                                          
035600     EJECT                                                                
038400 LINKAGE SECTION.                                                         
038500*01  -COPY W0009   -PRE MSG-                                              
038600*01  -COPY W0009   -PRE DISP-                                             
038700     EJECT                                                                
039100*01  -COPY W0008   -PRE WDQ5A-                                            
039200     05  FILLER                  PIC X.                                   
039300     SKIP2                                                                
039400*01  -COPY W0008   -PRE 4013-                                             
039500     05  FILLER                  PIC X.                                   
040500     EJECT                                                                
040600                                                                          
041720 01  DNOT-ORQP-PCB               PIC X.                                   
041730 01  DNOT-ORQP2-PCB              PIC X.                                   
041731 01  DNOT-ORQP3-PCB              PIC X.                                   
041740 01  DNOT-4013-PCB               PIC X.                                   
041741 01  DNOT-BENA-PCB               PIC X.                                   
041750                                                                          
041800                                                                          
046700     EJECT                                                                
046710                                                                          
046800 PROCEDURE DIVISION  USING MSG-PCB                                        
046900                           DISP-PCB                                       
047000                           WDQ5A-PCB                                      
047100                           4013-PCB                                       
047200                           DNOT-ORQP-PCB                                  
047400                           DNOT-ORQP2-PCB                                 
047410                           DNOT-ORQP3-PCB                                 
047500                           DNOT-4013-PCB                                  
047600                           DNOT-BENA-PCB.                                 
049100                                                                          
049200     EJECT                                                                
049300                                                                          
049400     ENTRY 'DLITCBL' USING MSG-PCB                                        
049410                           DISP-PCB                                       
049420                           WDQ5A-PCB                                      
049430                           4013-PCB                                       
049440                           DNOT-ORQP-PCB                                  
049450                           DNOT-ORQP2-PCB                                 
049451                           DNOT-ORQP3-PCB                                 
049460                           DNOT-4013-PCB                                  
049470                           DNOT-BENA-PCB.                                 
051700     EJECT                                                                
051800                                                                          
051900     PERFORM IMS-GET-MSG                                                  
052000                                                                          
052100     IF SEGMENT-FINNS                                                     
052200        PERFORM IMS-GN-MSG                                                
052300                                                                          
052410        PERFORM A-INIT                                                    
052420                                                                          
052430        MOVE MID-IDDISTR             TO TEST-IDDISTR                      
052440        IF DIST07-USA-RETAILER-DNOTE                                      
052450        OR DIST07-CAN-RETAILER                                            
052460           PERFORM B-KONTROLL-ATT-ORDER-FINNS                             
052470           IF ALLT-OK                                                     
052480              PERFORM IMS-GU-WDQ5A1-MIN-MAX                               
052490              IF SEGMENT-FINNS AND                                        
052491                 SEQA-IDARTNR = ZERO                                      
052492                 PERFORM C-OBKR-TILL-WDQ5                                 
052493              ELSE                                                        
052494                 PERFORM D-OBKR-TILL-WL4013                               
052495              END-IF                                                      
052496           END-IF                                                         
052497        END-IF                                                            
052498        PERFORM Z-FINIT                                                   
054400     END-IF                                                               
054500     MOVE +0 TO RETURN-CODE                                               
054600     GOBACK                                                               
054700     .                                                                    
054800     EJECT                                                                
054810                                                                          
054820                                                                          
054900 A-INIT SECTION.                                                          
055000                                                                          
055010     MOVE 'STA-A   '    TO FELTEXT                                        
055100     MOVE MSG-INDATA-MINUS-1-TRANSKOD                                     
055200                               TO MID-AREA                                
055300     MOVE MSG-IDTRANS-1        TO W-IDTRANS                               
055400                                                                          
055500     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
055600                                                                          
055610     MOVE FUNCTION CURRENT-DATE (1:8)                                     
055620                               TO DAGENS-DATUM-SSAAMMDD                   
055630                                                                          
055700     MOVE JA                   TO ALLT-SW                                 
056100     .                                                                    
056200     EJECT                                                                
056300                                                                          
056400                                                                          
059600 B-KONTROLL-ATT-ORDER-FINNS SECTION.                                      
059700                                                                          
059701     MOVE 'STA-B   '    TO FELTEXT                                        
059730                                                                          
059800     IF MID-IDDISTR NUMERIC AND MID-IDDISTR > ZERO                        
059900        MOVE MID-IDDISTR          TO W-IDDISTR-MIN                        
059910                                     W-IDDISTR-MAX                        
060000     ELSE                                                                 
060100        MOVE NEJ                  TO ALLT-SW                              
060200     END-IF                                                               
060300                                                                          
060400     IF MID-IDKUNDNR = SPACE                                              
060500        MOVE ZERO                 TO W-IDKUNDNR-MIN                       
060510                                     W-IDKUNDNR-MAX                       
060600     ELSE                                                                 
060700        IF MID-IDKUNDNR NUMERIC                                           
060800           MOVE MID-IDKUNDNR      TO W-IDKUNDNR-MIN                       
060810                                     W-IDKUNDNR-MAX                       
060900        ELSE                                                              
061000           MOVE NEJ               TO ALLT-SW                              
061100        END-IF                                                            
061200     END-IF                                                               
061300                                                                          
061400     IF MID-IDORDNR NUMERIC AND MID-IDORDNR > ZERO                        
061500        MOVE MID-IDORDNR          TO W-IDORDNR7-MIN                       
061510                                     W-IDORDNR7-MAX                       
061600     ELSE                                                                 
061700        MOVE NEJ                  TO ALLT-SW                              
061800     END-IF                                                               
061900                                                                          
062000     IF NOT ALLT-OK                                                       
062100        MOVE ERR-WRONG-KEY        TO MSG-KOM-IDMFSMED                     
062200        MOVE 'R'                  TO MSG-KOM-KDSVAR                       
062300        CALL ABEND USING RKOD-ABEND-33                                    
062400     END-IF                                                               
066800     .                                                                    
066900     EJECT                                                                
067000                                                                          
067010                                                                          
067100 C-OBKR-TILL-WDQ5 SECTION.                                                
067110                                                                          
067111     MOVE 'STA-C   '    TO FELTEXT                                        
067120     MOVE +1                       TO MID-IX                              
067130     PERFORM UNTIL MID-IX > 14                                            
067131        IF MID-IDARTNR(MID-IX) = SPACE                                    
067132        OR MID-IDARTNR(MID-IX) = ZERO                                     
067133           CONTINUE                                                       
067134        ELSE                                                              
067140           PERFORM CA-FLYTTA-TILL-W411DNOT                                
067151           CALL W411DNOT USING DNOT-W411DNOT                              
067152                               DNOT-ORQP-PCB                              
067153                               DNOT-ORQP2-PCB                             
067154                               DNOT-ORQP3-PCB                             
067155                               DNOT-4013-PCB                              
067156                               DNOT-BENA-PCB                              
067158        END-IF                                                            
067159        ADD +1                     TO MID-IX                              
067160     END-PERFORM                                                          
067200     .                                                                    
067210     EJECT                                                                
067220                                                                          
067230                                                                          
067240  CA-FLYTTA-TILL-W411DNOT SECTION.                                        
067241                                                                          
067242     MOVE 'STA-CA  '    TO FELTEXT                                        
067243     MOVE IDPGM                    TO DNOT-IDPGM                          
067244     MOVE SEQA-IDORDER             TO DNOT-IDORDER                        
067246     MOVE SEQA-IDDC                TO DNOT-IDDC                           
067247     MOVE MID-IDDISTR              TO DNOT-IDDISTR                        
067248     MOVE MID-IDKUNDNR             TO DNOT-IDKUNDNR                       
067249     MOVE SPACE                    TO DNOT-IDKUNDRF                       
067250     MOVE MID-IDORDNR              TO DNOT-IDORDNR7                       
067251     MOVE MID-KDORDKL              TO DNOT-KDORDKL                        
067252     MOVE MID-IDARTNR(MID-IX)      TO DNOT-IDARTNR                        
067253     MOVE MID-IDLOPNR(MID-IX)      TO DNOT-IDLOPNR                        
067254     MOVE MID-IDSEKVNR(MID-IX)     TO DNOT-IDSEKVNR                       
067255     MOVE MID-KDORDBEK(MID-IX)     TO DNOT-KDORDBEK                       
067256     MOVE MID-BEART-USA(MID-IX)    TO DNOT-BEART-USA                      
067257     MOVE MID-KVBEART(MID-IX)      TO DNOT-KVBEART                        
067258     .                                                                    
067260     EJECT                                                                
067270                                                                          
067280                                                                          
067300 D-OBKR-TILL-WL4013 SECTION.                                              
067301     MOVE 'STA-D   '    TO FELTEXT                                        
067310                                                                          
067311     PERFORM IMS-GU-WL401301                                              
067312     IF SEGMENT-FINNS                                                     
067320        MOVE MID-IDDISTR        TO W-4014-IDDISTR                         
067321        MOVE MID-IDKUNDNR       TO W-4014-IDKUNDNR                        
067322        MOVE SPACE              TO W-4014-IDKUNDRF                        
067323        MOVE MID-IDORDNR        TO W-4014-IDORDNR7                        
067325                                                                          
067326        PERFORM IMS-GNP-WL401311                                          
067327        IF SEGMENT-SAKNAS                                                 
067328           PERFORM DA-FLYTTA-TILL-WL4014                                  
067329           PERFORM IMS-ISRT-WL401311                                      
067331        END-IF                                                            
067333        MOVE +1                       TO MID-IX                           
067334        PERFORM UNTIL MID-IX > 14                                         
067335           IF MID-IDARTNR(MID-IX) = SPACE                                 
067336              CONTINUE                                                    
067337           ELSE                                                           
067340              PERFORM DB-FLYTTA-TILL-WL4016                               
067343              PERFORM IMS-ISRT-WL401321                                   
067359           END-IF                                                         
067360           ADD +1               TO MID-IX                                 
067370        END-PERFORM                                                       
067380     END-IF                                                               
067400     .                                                                    
067500     EJECT                                                                
067600                                                                          
067700                                                                          
067800 DA-FLYTTA-TILL-WL4014 SECTION.                                           
067801                                                                          
067802     MOVE 'STA-DA  '    TO FELTEXT                                        
067841     MOVE MID-IDDISTR              TO 4014-IDDISTR                        
067850     MOVE MID-IDKUNDNR             TO 4014-IDKUNDNR                       
067860     MOVE SPACE                    TO 4014-IDKUNDRF                       
067870     MOVE MID-IDORDNR              TO 4014-IDORDNR7                       
067900     .                                                                    
068000     EJECT                                                                
068100                                                                          
068200                                                                          
068300 DB-FLYTTA-TILL-WL4016 SECTION.                                           
068301                                                                          
068302     MOVE 'STA-DB  '    TO FELTEXT                                        
068303     MOVE MID-IDARTNR(MID-IX)      TO W-4016-IDARTNR                      
068304     MOVE MID-IDLOPNR(MID-IX)      TO W-4016-IDLOPNR                      
068305     MOVE MID-IDSEKVNR(MID-IX)     TO W-4016-IDSEKVNR                     
068306     MOVE MID-KDORDBEK(MID-IX)     TO W-4016-KDORDBEK                     
068307                                                                          
068310     MOVE MID-IDARTNR(MID-IX)      TO 4016-IDARTNR                        
068320     MOVE MID-IDLOPNR(MID-IX)      TO 4016-IDLOPNR                        
068330     MOVE MID-IDSEKVNR(MID-IX)     TO 4016-IDSEKVNR                       
068350     MOVE MID-KDORDBEK(MID-IX)     TO 4016-KDORDBEK                       
068360     MOVE MID-BEART-USA(MID-IX)    TO 4016-BEART-USA                      
068370     MOVE MID-KVBEART(MID-IX)      TO 4016-KVBEART                        
068380     MOVE MID-KDORDKL              TO 4016-KDORDKL                        
068390     MOVE DAGENS-DATUM-SSAAMMDD    TO 4016-DAREGDAT                       
068400     .                                                                    
068500     EJECT                                                                
068600                                                                          
068700                                                                          
250100 Z-FINIT  SECTION.                                                        
250200                                                                          
250210     MOVE 'STA-Z   '    TO FELTEXT                                        
250300*    SKRIV MED TILL MPP DISPATCHERN                                       
250400     IF MSG-KOM-IDMFSMED = SPACE                                          
250500        MOVE OK-BEHANDLAD      TO MSG-KOM-IDMFSMED                        
250600     END-IF                                                               
250700     PERFORM IMS-INSERT-DISP-MSG                                          
250800     .                                                                    
250900     EJECT                                                                
251000                                                                          
251100                                                                          
283400* --- IMS SEKTIONER ---                                                   
283500                                                                          
283600 IMS-GET-MSG SECTION.                                                     
283700                                                                          
283800     MOVE '  QC' TO GODK-STATUSKODER                                      
283900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
284000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
284100     PERFORM IMS-STATUSKONTROLL                                           
284200     .                                                                    
284300     SKIP2                                                                
284310                                                                          
284320                                                                          
284400 IMS-GN-MSG SECTION.                                                      
284500                                                                          
284600     MOVE '  '   TO GODK-STATUSKODER                                      
284700     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
284800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
284900     PERFORM IMS-STATUSKONTROLL                                           
285000     .                                                                    
285100     SKIP2                                                                
285110                                                                          
285120                                                                          
285200 IMS-INSERT-DISP-MSG SECTION.                                             
285300                                                                          
285400     MOVE SPACE TO GODK-STATUSKODER                                       
285500     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
285600     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
285700     PERFORM IMS-STATUSKONTROLL                                           
285800     .                                                                    
285900     EJECT                                                                
286000                                                                          
286100                                                                          
288110 IMS-GU-WDQ5A1-MIN-MAX SECTION.                                           
288200                                                                          
288310     STRING 'WDQ5A1  (WDQ5A1KY=>' W-WDQ5A1KY-MIN-X                        
288320                    '&WDQ5A1KY<=' W-WDQ5A1KY-MAX-X ')'                    
288400          DELIMITED BY SIZE INTO SSA1                                     
288500     MOVE '  GE'               TO GODK-STATUSKODER                        
288600     CALL CBLTDLI USING GU WDQ5A-PCB DLI-IO-WDQ5A SSA1                    
288700     MOVE WDQ5A-STATUS-CODE    TO STATUS-WS                               
288800     PERFORM IMS-STATUSKONTROLL                                           
288900     .                                                                    
289000     SKIP2                                                                
289100                                                                          
289101                                                                          
289110 IMS-GU-WL401301  SECTION.                                                
289200                                                                          
289300     STRING 'WL401301(WDGXKEY  =' W-4013-WDGXKEY-X ')'                    
289400          DELIMITED BY SIZE INTO SSA1                                     
289500     MOVE '  GE'               TO GODK-STATUSKODER                        
289600     CALL CBLTDLI USING GU 4013-PCB DLI-IO-401301 SSA1                    
289700     MOVE 4013-STATUS-CODE     TO STATUS-WS                               
289800     PERFORM IMS-STATUSKONTROLL                                           
289900     .                                                                    
290000     SKIP2                                                                
290100                                                                          
290200                                                                          
290300 IMS-GNP-WL401311 SECTION.                                                
290400                                                                          
290500     STRING 'WL401311(IDGMTREF =' W-4014-IDGMTREF-X ')'                   
290600          DELIMITED BY SIZE INTO SSA1                                     
290700     MOVE '  GE'               TO GODK-STATUSKODER                        
290800     CALL CBLTDLI USING GNP 4013-PCB DLI-IO-401311 SSA1                   
290900     MOVE 4013-STATUS-CODE     TO STATUS-WS                               
291000     PERFORM IMS-STATUSKONTROLL                                           
291100     .                                                                    
291200     SKIP2                                                                
291300                                                                          
295010 IMS-ISRT-WL401311 SECTION.                                               
295100                                                                          
295110     STRING 'WL401301(WDGXKEY  =' W-4013-WDGXKEY-X ')'                    
295120          DELIMITED BY SIZE INTO SSA1                                     
295200     MOVE 'WL401311 '         TO SSA2                                     
295300     MOVE '  II'              TO GODK-STATUSKODER                         
295400     CALL CBLTDLI USING ISRT 4013-PCB DLI-IO-401311 SSA1 SSA2             
295500     MOVE 4013-STATUS-CODE    TO STATUS-WS                                
295600     PERFORM IMS-STATUSKONTROLL                                           
295700     .                                                                    
295800     EJECT                                                                
295810                                                                          
295811                                                                          
295826 IMS-ISRT-WL401321 SECTION.                                               
295827                                                                          
295828     STRING 'WL401301(WDGXKEY  =' W-4013-WDGXKEY-X ')'                    
295829          DELIMITED BY SIZE INTO SSA1                                     
295830     STRING 'WL401311(IDGMTREF =' W-4014-IDGMTREF-X ')'                   
295831          DELIMITED BY SIZE INTO SSA2                                     
295832     MOVE 'WL401321 '         TO SSA3                                     
295840     MOVE '  II'              TO GODK-STATUSKODER                         
295850     CALL CBLTDLI USING ISRT 4013-PCB DLI-IO-401321                       
295851                                      SSA1 SSA2 SSA3                      
295860     MOVE 4013-STATUS-CODE    TO STATUS-WS                                
295870     PERFORM IMS-STATUSKONTROLL                                           
295880     .                                                                    
295890     EJECT                                                                
295891                                                                          
295899                                                                          
295900 IMS-STATUSKONTROLL SECTION.                                              
296000                                                                          
296100     SET STATUS-IX TO 1                                                   
296200     SEARCH GODK-STATUS                                                   
296300       AT END CALL FELLOG                                                 
296400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
296500     END-SEARCH                                                           
296600     .                                                                    
296700     EJECT                                                                
