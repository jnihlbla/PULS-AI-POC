000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4037400.                                                
000400 AUTHOR.         KERSTIN MATTIASSON.                                      
000500 DATE-WRITTEN.   91/02/26.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        DETTA PROGRAM SKRIVER UT EN INLÄGGNINGSLISTA FÖR DE              
001100*        INGÅENDE ARTIKLAR SOM SKALL LÄGGAS TILLBAKS I LAGRET.            
001200*        TILL DETTA PROGRAMMET KOMMER ANTINGEN IFRÅN                      
001300*        PGM W4030400, ELLER VIA PGM W4037200.                            
001400*                                                                         
001500*        ENDAST RADER SOM HAR KDSATLI = 2 SKALL SKRIVAS.                  
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W4T304U                                             
001900*        TRANSAKTION: W4T372X                                             
002000*                                                                         
002100*    UTDATA.                                                              
002200*        INLÄGGNINSLISTA                                                  
002300*        TRANSAKTION: W4T379X (JUSTERING AV WDE4, WDE6)                   
002400                                                                          
002500                                                                          
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 CONFIGURATION SECTION.                                                   
002900*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003201                                                                          
003210*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W4037400'.            
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
003910 77  IDDC-SATS                   PIC X(02)   VALUE '11'.                  
004000                                                                          
004100 77  IDPURAD                     PIC S9(9)  VALUE +0    COMP SYNC.        
004200 77  RAD-IX                      PIC S9(9)  VALUE +99   COMP SYNC.        
004300 77  SID-IX                      PIC S9(9)  VALUE +0    COMP SYNC.        
004400                                                                          
004500 77  SPAR-ADLAGOMR               PIC S9(3)  VALUE +0    COMP-3.           
004600                                                                          
004700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004800     88  ALLT-OK                             VALUE 'J'.                   
004900     88  ALLT-FEL                            VALUE 'N'.                   
005000                                                                          
005100 77  PLOCK-SW                    PIC X       VALUE 'N'.                   
005200     88  PLOCKSATS-KLAR                      VALUE 'J'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88 GODK-MID                             VALUE '4304' '4372'.         
005610                                                                          
005700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005800 01  GENERELLA-SUBPROGRAM.                                                
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
006200     EJECT                                                                
006300*    --- AREA FÖR SUBPROGRAM W006PRR1                                     
006400*                                                                         
006500 01  FILLER                      PIC X(16)  VALUE 'PRINTPGM'.             
006600                                                                          
006700*01  -COPY W006PRAR                                                       
006800                                                                          
006900 01  WS-BERAEKN-AREA.                                                     
007000     03  WS-VLORDNTO-CM          PIC S9(8)V9 VALUE ZERO COMP-3.           
007100     03  WS-VLORDNTO-M           PIC S9(4)V9(3) VALUE ZERO COMP-3.        
007200     03  WS-VKORDNTO-1DEC        PIC S9(6)V9 VALUE ZERO COMP-3.           
007300     03  WS-VKORDNTO-3DEC        PIC S9(6)V9(3) VALUE ZERO COMP-3.        
007400     03  WS-VKARTNTO-TOT         PIC S9(4)V999 VALUE ZERO COMP-3.         
007500     03  WS-VLARTNTO-TOT         PIC S9(8)V9 VALUE ZERO COMP-3.           
007600                                                                          
007700 01  WS-INL-AREA.                                                         
007710     03 WS-IDPRTLST-PU.                                                   
007720        05 WS-SYSTDEL-PU         PIC X(1).                                
007730        05 WS-LISTTYP-PU         PIC X(2).                                
007750        05 WS-KDPRT-PU           PIC X(3).                                
007760        05 FILLER                PIC X(2)    VALUE SPACE.                 
007900     03 WS-INL-LISTID.                                                    
008000        05 FILLER                PIC X(5)  VALUE 'SATS'.                  
008100        05 WS-INL-IDORDNSB       PIC 9(4).                                
008200        05 WS-INL-IDORDNSS       PIC 9(1).                                
008300     03 WS-INL-LISTRAD.                                                   
008400        05 FILLER                PIC X(2)  VALUE SPACE.                   
008500        05 WS-INL-RAD            PIC X(78).                               
008600     03 WS-INL-DUMMY             PIC X(1).                                
008700                                                                          
008800 01  WS-MSG-AREA.                                                         
008900     03 WS-IDORDNSB              PIC X(5).                                
009000     03 WS-IDORDNSS              PIC X(1).                                
009100     EJECT                                                                
009200*    --- AREOR FÖR MSG-HANTERING                                          
009300*                                                                         
009400 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
009500                                                                          
009600*01  -COPY WMSGAREA                                                       
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
009900 01  P-TO-P-SW.                                                           
010000     03  PTOP-LL                 PIC S9(4)   VALUE 23 COMP SYNC.          
010100     03  PTOP-Z1                 PIC  X(1)   VALUE LOW-VALUE.             
010200     03  PTOP-Z2                 PIC  X(1)   VALUE LOW-VALUE.             
010300     03  PTOP-TRANSKOD           PIC  X(7)   VALUE 'W4T379X'.             
010400     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
010500     03  FILLER                  PIC  X(4)   VALUE '4374'.                
010600     03  PTOP-KDMFSFOR           PIC  X(1).                               
010700     03  PTOP-IDORDNSB           PIC  X(5).                               
010800     03  PTOP-IDORDNSS           PIC  X(1).                               
010900                                                                          
011000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011300                                                                          
011400 01  NYCKLAR-TILL-DLI.                                                    
011500                                                                          
011600*----> SATSORDERKÖN.                                                      
011700                                                                          
011800     03  W-WDJ2-IDORDNST-X.                                               
011900         05 W-WDJ2-IDORDNSB               PIC S9(5) COMP-3.               
012000         05 W-WDJ2-IDORDNSS               PIC S9(1) COMP-3.               
012100                                                                          
012200     03 W-WDJ2-KDSATLI-X                  PIC X(1)  VALUE '2'.            
012300                                                                          
012400*----> BENÄMNINGSREGISTER WDD3.                                           
012500                                                                          
012600     03  W-WDD3BSEQ-X.                                                    
012700         05 W-WDD3BSEQ-IDARTNR            PIC S9(9) COMP-3.               
012800                                                                          
012900     03  W-WDD3-IDSKYLT-X                 PIC X(3).                       
013000                                                                          
013100*----> PRC-PRINTERTABELL WDR1.                                            
013200                                                                          
013300     03  W-WDR1-WDGXKEY-4453-X.                                           
013400         05  W-4453-IDHTYP       PIC  X(04) VALUE '4453'.                 
013500         05  W-4453-IDDC         PIC  X(02) VALUE '11'.                   
013600         05  W-4453-IDPRC        PIC  X(04).                              
013700         05  W-4447-LOW-VALUE    PIC  X(20) VALUE LOW-VALUE.              
013800                                                                          
013900     03  W-WDR1-KDSEGKEY-4454-X  PIC  X(01) VALUE '1'.                    
014000                                                                          
014100*    --- STATUS-KOD FRÅN IMS                                              
014200                                                                          
014300 01  STATUS-WS                   PIC  X(02).                              
014400     88  SEGMENT-FINNS                       VALUE '  '.                  
014500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014600     88  END-OF-DATA                         VALUE 'GB'.                  
014700     SKIP2                                                                
014800 01  GODK-STATUSKODER.                                                    
014900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015000                                                                          
015100 01  SSA1                        PIC X(96).                               
015200 01  SSA2                        PIC X(96).                               
015300     EJECT                                                                
015400**************************************                                    
015500*  PRINTRADER FÖR INLÄGGNINGSLISTAN  *                                    
015600**************************************                                    
015700 01  INL-HRAD1X.                                                          
015800     03   FILLER                  PIC X(29) VALUE SPACE.                  
015900     03   FILLER                  PIC X(22)                               
016000                           VALUE 'INLÄGGNINGSLISTA-SATS '.                
016100                                                                          
016200 01  INL-HRAD1.                                                           
016300     03   FILLER                  PIC X(74) VALUE SPACE.                  
016400     03   INL-HRAD1-IDSID         PIC ZZ9.                                
016500                                                                          
016600 01  INL-HRAD2.                                                           
016700     03   FILLER                  PIC X(2)  VALUE SPACE.                  
016800     03   INL-HRAD2-IDDISTR       PIC Z(4)9.                              
016900     03   FILLER                  PIC X(18) VALUE SPACE.                  
017000     03   INL-HRAD2-IDORDNSB      PIC Z(4).                               
017100     03   INL-HRAD2-IDORDNSS      PIC 9(1).                               
017200     03   FILLER                  PIC X(7)  VALUE SPACE.                  
017300     03   INL-HRAD2-KDORDKL       PIC X(1).                               
017400     03   FILLER                  PIC X(9)  VALUE SPACE.                  
017500     03   INL-HRAD2-IDUSER        PIC X(8).                               
017600     03   FILLER                  PIC X(8)  VALUE SPACE.                  
017700     03   INL-HRAD2-IDPRODNR      PIC Z(6)9.                              
017800                                                                          
017900 01  INL-HRAD3.                                                           
018000     03   FILLER                  PIC X(12) VALUE 'SATSART.NR: '.         
018100     03   INL-HRAD3-IDARTNR       PIC Z(8)9.                              
018200     03   FILLER                  PIC X(2)  VALUE SPACE.                  
018300     03   INL-HRAD3-BEART         PIC X(32).                              
018400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
018500     03   FILLER                  PIC X(16)                               
018600                             VALUE 'UTSKRIFTSDATUM: '.                    
018700     03   INL-HRAD3-DATUM         PIC 9(6).                               
018800                                                                          
018900 01  INL-HRAD4.                                                           
019000     03   FILLER                  PIC X(12) VALUE 'PRIORITET:  '.         
019100     03   INL-HRAD4-PRIO          PIC X(12).                              
019200     03   FILLER                  PIC XX    VALUE SPACE.                  
019300     03   FILLER                  PIC X(7)  VALUE 'ANTAL: '.              
019400     03   INL-HRAD4-KVBYGGB       PIC Z(6)9.                              
019500     03   FILLER                  PIC XX    VALUE SPACE.                  
019600     03   FILLER                  PIC X(9)  VALUE 'FÖRPTYP: '.            
019700     03   INL-HRAD4-BEFT          PIC Z(3).                               
019800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
019900     03   FILLER                  PIC X(16)                               
020000                             VALUE 'REGISTRERINGSD: '.                    
020100     03   INL-HRAD4-TIREGDAT      PIC 9(6).                               
020200                                                                          
020300 01  INL-HRAD5.                                                           
020400                                                                          
020500     03   FILLER                  PIC X(12) VALUE 'ANSKAFFARE: '.         
020600     03   INL-HRAD5-IDANSK        PIC Z(3).                               
020700     03   FILLER                  PIC X(41) VALUE SPACE.                  
020800     03   FILLER                  PIC X(16)                               
020900                             VALUE 'BEGÄRD PACKAD:  '.                    
021000     03   INL-HRAD5-TIBEGPAC      PIC 9(6).                               
021100                                                                          
021200 01  INL-HRAD6.                                                           
021300     03   FILLER                  PIC X(7)  VALUE 'BRI    '.              
021400     03   FILLER                  PIC X(11) VALUE 'ADRESS     '.          
021500     03   FILLER                  PIC X(7)  VALUE 'RAD    '.              
021600     03   FILLER                  PIC X(8)  VALUE 'ART.NR  '.             
021700     03   FILLER                  PIC X(9)  VALUE 'BENÄMNING'.            
021800     03   FILLER                  PIC X(11) VALUE SPACE.                  
021900     03   FILLER                  PIC X(9)  VALUE 'ANT/SATS '.            
022000     03   FILLER                  PIC X(5)  VALUE 'ENH  '.                
022100     03   FILLER                  PIC X(5)  VALUE 'ANTAL'.                
022200                                                                          
022300 01  INL-RAD.                                                             
022400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
022500     03   INL-RAD-BRIST           PIC X(1).                               
022600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
022700     03   INL-RAD-KDSATKMB        PIC X(1).                               
022800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
022900     03   INL-RAD-ADLAGOMR        PIC Z(2)9.                              
023000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
023100     03   INL-RAD-ADGANG          PIC Z(1)9.                              
023200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
023300     03   INL-RAD-ADPLATS         PIC Z(4)9.                              
023400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
023500     03   INL-RAD-IDPURAD         PIC Z(3).                               
023600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
023700     03   INL-RAD-IDARTNR         PIC Z(8)9.                              
023800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
023900     03   INL-RAD-BEART           PIC X(20).                              
024000     03   FILLER                  PIC X(2)  VALUE SPACE.                  
024100     03   INL-RAD-REANTPSA        PIC Z9.999.                             
024200     03   FILLER                  PIC X(2)  VALUE SPACE.                  
024300     03   INL-RAD-KDSORT          PIC X(2).                               
024400     03   INL-RAD-REBEART         PIC Z(7).                               
024500                                                                          
024600 01  INL-TOTRAD1.                                                         
024700     03   FILLER                  PIC X(11) VALUE 'TOTALVIKT: '.          
024800     03   INL-TOTRAD1-VKORDNTO    PIC Z(5)9.9(1).                         
024900     03   FILLER                  PIC X(6)  VALUE ' KG   '.               
025000     03   FILLER                  PIC X(7)  VALUE 'VOLYM: '.              
025100     03   INL-TOTRAD1-VLORDNTO    PIC Z(3)9.9(3).                         
025200     03   FILLER                  PIC X(6)  VALUE ' M3   '.               
025300     03   FILLER                  PIC X(16)                               
025400                             VALUE 'ANTAL ARTIKLAR: '.                    
025500     03   INL-TOTRAD1-KVRADER     PIC Z(7)9.                              
025600     03   FILLER                  PIC X(3)  VALUE ' ST'.                  
025700     EJECT                                                                
025800*    --- IMS FUNKTIONSKODER                                               
025900*01  -COPY W0003                                                          
026000     EJECT                                                                
026100*    ---  DLI INPUT-OUTPUT AREA                                           
026200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
026300                                                                          
026400 01  DLI-IO-AREA1.                                                        
026500     03  WLSATG01.                                                        
026600*        05  -COPY WDJ201                                                 
026700     EJECT                                                                
026800 01  DLI-IO-AREA2.                                                        
026900     03  WLSATG12.                                                        
027000*        05  -COPY WDJ212                                                 
027100     EJECT                                                                
027200 01  DLI-IO-AREA3.                                                        
027300     03  WLBENA11.                                                        
027400*        05  -COPY WDD311                                                 
027500     EJECT                                                                
027600 01  DLI-IO-AREA4.                                                        
027700     03  WLXXKL11.                                                        
027800*        05  -COPY WDGX4454                                               
027900     EJECT                                                                
028000 LINKAGE SECTION.                                                         
028100                                                                          
028200*01  -COPY W0009      -PRE MSG-                                           
028300     EJECT                                                                
028400*01  -COPY W0009      -PRE ALT1-                                          
028500     EJECT                                                                
028600*01  -COPY W0009      -PRE ALT2-                                          
028700     EJECT                                                                
028800*01  -COPY W0008      -PRE LISB-                                          
028900     05  FILLER                  PIC X.                                   
029000     EJECT                                                                
029100*01  -COPY W0008      -PRE SATG-                                          
029200     05  FILLER                  PIC X.                                   
029300     EJECT                                                                
029400*01  -COPY W0008      -PRE BENA-                                          
029500     05  FILLER                  PIC X.                                   
029600     EJECT                                                                
029700*01  -COPY W0008      -PRE XXKL-                                          
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000 PROCEDURE DIVISION  USING MSG-PCB                                        
030100                           ALT1-PCB                                       
030200                           ALT2-PCB                                       
030300                           LISB-PCB                                       
030400                           SATG-PCB                                       
030500                           BENA-PCB                                       
030600                           XXKL-PCB.                                      
030700                                                                          
030800     ENTRY 'DLITCBL' USING MSG-PCB                                        
030900                           ALT1-PCB                                       
031000                           ALT2-PCB                                       
031100                           LISB-PCB                                       
031200                           SATG-PCB                                       
031300                           BENA-PCB                                       
031400                           XXKL-PCB.                                      
031500                                                                          
031600     PERFORM IMS-GU-MSG                                                   
031700     IF SEGMENT-FINNS                                                     
031800        PERFORM A-INIT                                                    
031900        IF ALLT-OK                                                        
032000           PERFORM B-LAES-SATSORDER                                       
032100           IF ALLT-OK                                                     
032200              PERFORM C-LAES-PRC                                          
032300              IF ALLT-OK                                                  
032400                 PERFORM D-SKRIV-INLLISTA                                 
032500                 PERFORM E-UPPDAT-SATSORDER                               
032600                 PERFORM F-SKICKA-IMSTRANS                                
032700              END-IF                                                      
032800           END-IF                                                         
032900        END-IF                                                            
033000     END-IF                                                               
033100                                                                          
033200     MOVE ZERO TO RETURN-CODE                                             
033300     GOBACK                                                               
033400     .                                                                    
033500     EJECT                                                                
033600 A-INIT SECTION.                                                          
033700                                                                          
033800     IF MSG-KDTRANS-1  NOT = 'W4T374X'                                    
033900        MOVE NEJ TO ALLT-SW                                               
034000     END-IF                                                               
034100                                                                          
034200     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
034300     IF NOT GODK-MID                                                      
034400        MOVE NEJ TO ALLT-SW                                               
034500     END-IF                                                               
034600                                                                          
034700     IF ALLT-OK                                                           
034800        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO WS-MSG-AREA                   
034900        MOVE WS-IDORDNSB                 TO W-WDJ2-IDORDNSB               
035000        MOVE WS-IDORDNSS                 TO W-WDJ2-IDORDNSS               
035100        ACCEPT INL-HRAD3-DATUM           FROM DATE                        
035200     END-IF                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 B-LAES-SATSORDER SECTION.                                                
035600                                                                          
035700     PERFORM IMS-GHU-WDJ2-WLSATG01                                        
035800                                                                          
035900     IF SEGMENT-FINNS                                                     
036000        AND                                                               
036100       (SHUV-KDSATPLK = 5 OR 6)                                           
036200        MOVE SHUV-IDORDNSB TO WS-INL-IDORDNSB                             
036300        MOVE SHUV-IDORDNSS TO WS-INL-IDORDNSS                             
036400        PERFORM BA-HAMTA-BENAMNING                                        
036500        PERFORM BB-REDIGERA-INLHUVUD                                      
036600        PERFORM BC-NOLLSTAELL-BERAEKN                                     
036700     ELSE                                                                 
036800        MOVE NEJ TO ALLT-SW                                               
036900     END-IF                                                               
037000     .                                                                    
037100     EJECT                                                                
037200 BA-HAMTA-BENAMNING SECTION.                                              
037300                                                                          
037400     MOVE SHUV-IDARTNR TO W-WDD3BSEQ-IDARTNR                              
037500                                                                          
037700     MOVE 'S  ' TO W-WDD3-IDSKYLT-X                                       
038100                                                                          
038200     PERFORM IMS-GU-WDD3-WLBENA11                                         
038300     IF SEGMENT-FINNS                                                     
038400        MOVE TEXT-BEART       TO INL-HRAD3-BEART                          
038500     ELSE                                                                 
038600        MOVE 'BENÄMN. SAKNAS' TO INL-HRAD3-BEART                          
038700     END-IF                                                               
038800     .                                                                    
038900     EJECT                                                                
039000 BB-REDIGERA-INLHUVUD SECTION.                                            
039100                                                                          
039200     MOVE SHUV-IDDISTR         TO INL-HRAD2-IDDISTR                       
039300     MOVE SHUV-IDORDNSB        TO INL-HRAD2-IDORDNSB                      
039400     MOVE SHUV-IDORDNSS        TO INL-HRAD2-IDORDNSS                      
039500     MOVE SHUV-IDPRODNR        TO INL-HRAD2-IDPRODNR                      
039600     MOVE SHUV-KDORDKL         TO INL-HRAD2-KDORDKL                       
039700     IF SHUV-FLSATPRI = JA                                                
039800        MOVE 'PRIO'            TO INL-HRAD4-PRIO                          
039900     ELSE                                                                 
040000        MOVE SPACE             TO INL-HRAD4-PRIO                          
040100     END-IF                                                               
040200     MOVE SHUV-DAREGDAT (3:6)  TO INL-HRAD4-TIREGDAT                      
040300                                                                          
040400     MOVE SHUV-IDARTNR         TO INL-HRAD3-IDARTNR                       
040500     MOVE SHUV-KVBYGGB         TO INL-HRAD4-KVBYGGB                       
040600                                                                          
040700     MOVE SHUV-BEFT            TO INL-HRAD4-BEFT                          
040800     MOVE SHUV-IDANSK          TO INL-HRAD5-IDANSK                        
040900     MOVE SHUV-TIBEGPAC        TO INL-HRAD5-TIBEGPAC                      
041000                                                                          
041100     MOVE SHUV-IDUSER (4:5)    TO INL-HRAD2-IDUSER                        
041200     .                                                                    
041300     EJECT                                                                
041400 BC-NOLLSTAELL-BERAEKN  SECTION.                                          
041500     MOVE ZERO     TO WS-VLORDNTO-CM                                      
041600                      WS-VLORDNTO-M                                       
041700                      WS-VKORDNTO-1DEC                                    
041800                      WS-VKORDNTO-3DEC                                    
041900     .                                                                    
042000     EJECT                                                                
042100 C-LAES-PRC SECTION.                                                      
042200                                                                          
042310     MOVE IDDC-SATS     TO W-4453-IDDC                                    
042400     MOVE SHUV-IDPRC    TO W-4453-IDPRC                                   
042500                                                                          
042600     PERFORM IMS-GU-WDR1-WLXXKL11                                         
042700                                                                          
042800     IF SEGMENT-SAKNAS                                                    
042900        MOVE NEJ TO ALLT-SW                                               
043000     END-IF                                                               
043100                                                                          
043200     IF ALLT-OK                                                           
043210       MOVE '4'               TO WS-SYSTDEL-PU                            
043240       MOVE 'PU'              TO WS-LISTTYP-PU                            
043292       MOVE 4454-KDPRTGEN-PU  TO WS-KDPRT-PU                              
043400     END-IF                                                               
043500     .                                                                    
043600     EJECT                                                                
043700 D-SKRIV-INLLISTA SECTION.                                                
043800                                                                          
043900     PERFORM DA-OPEN-PRINTRAR                                             
044000                                                                          
044100     PERFORM DB-LAES-SATSORDERRAD                                         
044200                                                                          
044300     PERFORM UNTIL PLOCKSATS-KLAR                                         
044400        PERFORM DF-BERAEKNA-VIKT-VOLYM-RAD                                
044500        PERFORM DC-SKRIV-INLLISTA                                         
044600        MOVE URAD-ADLAGOMR TO SPAR-ADLAGOMR                               
044700        PERFORM DB-LAES-SATSORDERRAD                                      
044800     END-PERFORM                                                          
044900                                                                          
045000     PERFORM DE-SKRIV-TOTAL-INL                                           
045100     PERFORM DG-CLOSE-PRINTRAR                                            
045200     .                                                                    
045300     EJECT                                                                
045400 DA-OPEN-PRINTRAR SECTION.                                                
045500                                                                          
045600     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
045700                         PRT-OPEN                                         
045800                         WS-IDPRTLST-PU                                   
045900                         ALT2-PCB                                         
046000                         LISB-PCB                                         
046100                         WS-INL-LISTID                                    
046200                         WS-INL-DUMMY                                     
046300                         WS-INL-DUMMY                                     
046400     .                                                                    
046500     EJECT                                                                
046600 DB-LAES-SATSORDERRAD SECTION.                                            
046700                                                                          
046800     PERFORM IMS-GNP-WDJ2-WLSATG12                                        
046900                                                                          
047000     IF SEGMENT-SAKNAS                                                    
047100        MOVE JA TO PLOCK-SW                                               
047200     END-IF                                                               
047300     .                                                                    
047400     EJECT                                                                
047500 DC-SKRIV-INLLISTA SECTION.                                               
047600                                                                          
047700     IF RAD-IX > 61                                                       
047800        PERFORM S01-SKRIV-INLHUVUD                                        
047900     END-IF                                                               
048000                                                                          
048100     PERFORM DCA-REDIGERA-INLRAD                                          
048200                                                                          
048300     MOVE PRT-AFTER-2 TO PRT-RADSKIP                                      
048400     MOVE INL-RAD TO WS-INL-RAD                                           
048500     PERFORM S02-SKRIV-INLRAD                                             
048600                                                                          
048700     ADD 2       TO RAD-IX                                                
048800     .                                                                    
048900     EJECT                                                                
049000 DCA-REDIGERA-INLRAD SECTION.                                             
049100                                                                          
049200     IF URAD-FLSATBRI = JA                                                
049300        MOVE '*'                TO INL-RAD-BRIST                          
049400     ELSE                                                                 
049500        MOVE SPACE              TO INL-RAD-BRIST                          
049600     END-IF                                                               
049700     MOVE URAD-KDSATKMB         TO INL-RAD-KDSATKMB                       
049800     MOVE URAD-ADLAGOMR         TO INL-RAD-ADLAGOMR                       
049900     MOVE URAD-ADGANG           TO INL-RAD-ADGANG                         
050000     MOVE URAD-ADPLATS          TO INL-RAD-ADPLATS                        
050100     MOVE IDPURAD               TO INL-RAD-IDPURAD                        
050200     MOVE URAD-IDARTNR          TO INL-RAD-IDARTNR                        
050300     MOVE URAD-BEART            TO INL-RAD-BEART                          
050400     MOVE URAD-REANTPSA         TO INL-RAD-REANTPSA                       
050500     MOVE URAD-KDSORT           TO INL-RAD-KDSORT                         
050600     MOVE URAD-REBEART          TO INL-RAD-REBEART                        
050700     .                                                                    
050800     EJECT                                                                
050900 DE-SKRIV-TOTAL-INL SECTION.                                              
051000                                                                          
051100     COMPUTE WS-VKORDNTO-1DEC ROUNDED =                                   
051200                                    WS-VKORDNTO-3DEC                      
051300     COMPUTE WS-VLORDNTO-M ROUNDED =                                      
051400                                    WS-VLORDNTO-CM / 1000000              
051500                                                                          
051600     MOVE WS-VKORDNTO-1DEC       TO INL-TOTRAD1-VKORDNTO                  
051700     MOVE WS-VLORDNTO-M          TO INL-TOTRAD1-VLORDNTO                  
051800     MOVE IDPURAD                TO INL-TOTRAD1-KVRADER                   
051900                                                                          
052000     MOVE INL-TOTRAD1            TO WS-INL-RAD                            
052100     MOVE PRT-AFTER-2            TO PRT-RADSKIP                           
052200     PERFORM S02-SKRIV-INLRAD                                             
052300     .                                                                    
052400     EJECT                                                                
052500 DF-BERAEKNA-VIKT-VOLYM-RAD SECTION.                                      
052600                                                                          
052700     MOVE +0                    TO WS-VKARTNTO-TOT                        
052800                                   WS-VLARTNTO-TOT                        
052900                                                                          
053000     COMPUTE WS-VKARTNTO-TOT    =                                         
053100             URAD-VKARTNTO * URAD-REBEART                                 
053200     ADD     WS-VKARTNTO-TOT    TO WS-VKORDNTO-3DEC                       
053300                                                                          
053400     COMPUTE WS-VLARTNTO-TOT    =                                         
053500             URAD-VLARTNTO * URAD-REBEART                                 
053600     ADD     WS-VLARTNTO-TOT    TO WS-VLORDNTO-CM                         
053700                                                                          
053800     ADD +1                     TO IDPURAD                                
053900     .                                                                    
054000     EJECT                                                                
054100 DG-CLOSE-PRINTRAR SECTION.                                               
054200                                                                          
054300     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
054400                         PRT-CLOSE                                        
054500                         WS-IDPRTLST-PU                                   
054600                         ALT2-PCB                                         
054700                         LISB-PCB                                         
054800                         WS-INL-LISTID                                    
054900                         WS-INL-DUMMY                                     
055000                         WS-INL-DUMMY                                     
055100     .                                                                    
055200     EJECT                                                                
055300 E-UPPDAT-SATSORDER SECTION.                                              
055400                                                                          
055500     PERFORM IMS-GHU-WDJ2-WLSATG01                                        
055600                                                                          
055700     IF SEGMENT-FINNS                                                     
055800        MOVE '7' TO SHUV-KDSATPLK                                         
055900        PERFORM IMS-REPL-WDJ2-WLSATG01                                    
056000     END-IF                                                               
056100     .                                                                    
056200     EJECT                                                                
056300 F-SKICKA-IMSTRANS SECTION.                                               
056400                                                                          
056500     MOVE SHUV-IDORDNSB TO PTOP-IDORDNSB                                  
056600     MOVE SHUV-IDORDNSS TO PTOP-IDORDNSS                                  
056700     MOVE '1'           TO PTOP-KDMFSFOR                                  
056800     PERFORM IMS-ISRT-MSG-ALT1                                            
056900     .                                                                    
057000     EJECT                                                                
057100 S01-SKRIV-INLHUVUD SECTION.                                              
057200                                                                          
057300     ADD 1             TO SID-IX                                          
057400     MOVE SID-IX       TO INL-HRAD1-IDSID                                 
057500     MOVE PRT-NYSIDA-RAD1   TO PRT-RADSKIP                                
057600     MOVE INL-HRAD1X   TO WS-INL-RAD                                      
057700     PERFORM S02-SKRIV-INLRAD                                             
057800                                                                          
057900     MOVE PRT-AFTER-2  TO PRT-RADSKIP                                     
058000     MOVE INL-HRAD1    TO WS-INL-RAD                                      
058100     PERFORM S02-SKRIV-INLRAD                                             
058200                                                                          
058300     MOVE PRT-AFTER-3  TO PRT-RADSKIP                                     
058400     MOVE INL-HRAD2    TO WS-INL-RAD                                      
058500     PERFORM S02-SKRIV-INLRAD                                             
058600                                                                          
058700     MOVE PRT-AFTER-2  TO PRT-RADSKIP                                     
058800     MOVE INL-HRAD3    TO WS-INL-RAD                                      
058900     PERFORM S02-SKRIV-INLRAD                                             
059000                                                                          
059100     MOVE PRT-AFTER-2  TO PRT-RADSKIP                                     
059200     MOVE INL-HRAD4    TO WS-INL-RAD                                      
059300     PERFORM S02-SKRIV-INLRAD                                             
059400                                                                          
059500     MOVE PRT-AFTER-2  TO PRT-RADSKIP                                     
059600     MOVE INL-HRAD5    TO WS-INL-RAD                                      
059700     PERFORM S02-SKRIV-INLRAD                                             
059800                                                                          
059900     MOVE PRT-AFTER-3  TO PRT-RADSKIP                                     
060000     MOVE INL-HRAD6    TO WS-INL-RAD                                      
060100     PERFORM S02-SKRIV-INLRAD                                             
060200                                                                          
060300     MOVE 15           TO RAD-IX                                          
060400     .                                                                    
060500     EJECT                                                                
060600 S02-SKRIV-INLRAD SECTION.                                                
060700                                                                          
060800     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
060900                         PRT-WRITE                                        
061000                         WS-IDPRTLST-PU                                   
061100                         ALT2-PCB                                         
061200                         LISB-PCB                                         
061300                         WS-INL-LISTID                                    
061400                         PRT-RADSKIP                                      
061500                         WS-INL-LISTRAD                                   
061600     .                                                                    
061700     EJECT                                                                
061800* --- IMS SEKTIONER ---                                                   
061900                                                                          
062000 IMS-GU-MSG SECTION.                                                      
062100                                                                          
062200     MOVE '  QC' TO GODK-STATUSKODER                                      
062300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
062400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062500     PERFORM IMS-STATUSKONTROLL                                           
062600     .                                                                    
062700                                                                          
062800 IMS-ISRT-MSG-ALT1 SECTION.                                               
062900                                                                          
063000     MOVE SPACE TO GODK-STATUSKODER                                       
063100     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW                           
063200     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
063300     PERFORM IMS-STATUSKONTROLL                                           
063400     .                                                                    
063500                                                                          
063600 IMS-GHU-WDJ2-WLSATG01 SECTION.                                           
063700                                                                          
063800     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
063900          DELIMITED BY SIZE INTO SSA1                                     
064000     MOVE '  GE' TO GODK-STATUSKODER                                      
064100     CALL CBLTDLI USING GHU SATG-PCB DLI-IO-AREA1 SSA1                    
064200     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
064300     PERFORM IMS-STATUSKONTROLL                                           
064400     .                                                                    
064500                                                                          
064600                                                                          
064700 IMS-REPL-WDJ2-WLSATG01 SECTION.                                          
064800                                                                          
064900     MOVE '    ' TO GODK-STATUSKODER                                      
065000     CALL CBLTDLI USING REPL SATG-PCB DLI-IO-AREA1                        
065100     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
065200     PERFORM IMS-STATUSKONTROLL                                           
065300     .                                                                    
065400     EJECT                                                                
065500 IMS-GNP-WDJ2-WLSATG12 SECTION.                                           
065600                                                                          
065700     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
065800          DELIMITED BY SIZE INTO SSA1                                     
065900     STRING 'WLSATG12(KDSATLI  =' W-WDJ2-KDSATLI-X ')'                    
066000          DELIMITED BY SIZE INTO SSA2                                     
066100     MOVE '  GE' TO GODK-STATUSKODER                                      
066200     CALL CBLTDLI USING GNP SATG-PCB DLI-IO-AREA2 SSA1 SSA2               
066300     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
066400     PERFORM IMS-STATUSKONTROLL                                           
066500     .                                                                    
066600     EJECT                                                                
066700 IMS-GU-WDR1-WLXXKL11 SECTION.                                            
066800                                                                          
066900     STRING 'WLXXKL01(WDGXKEY  =' W-WDR1-WDGXKEY-4453-X ')'               
067000          DELIMITED BY SIZE INTO SSA1                                     
067100     STRING 'WLXXKL11(KDSEGKEY =' W-WDR1-KDSEGKEY-4454-X ')'              
067200          DELIMITED BY SIZE INTO SSA2                                     
067300     MOVE '  GE' TO GODK-STATUSKODER                                      
067400     CALL CBLTDLI USING GU XXKL-PCB DLI-IO-AREA4 SSA1 SSA2                
067500     MOVE XXKL-STATUS-CODE TO STATUS-WS                                   
067600     PERFORM IMS-STATUSKONTROLL                                           
067700     .                                                                    
067800     EJECT                                                                
067900 IMS-GU-WDD3-WLBENA11 SECTION.                                            
068000                                                                          
068100     STRING 'WLBENA01(WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
068200          DELIMITED BY SIZE INTO SSA1                                     
068300     STRING 'WLBENA11(IDSKYLT  =' W-WDD3-IDSKYLT-X ')'                    
068400          DELIMITED BY SIZE INTO SSA2                                     
068500     MOVE '  GE' TO GODK-STATUSKODER                                      
068600     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA3 SSA1 SSA2                
068700     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
068800     PERFORM IMS-STATUSKONTROLL                                           
068900     .                                                                    
069000     EJECT                                                                
069100 IMS-STATUSKONTROLL SECTION.                                              
069200                                                                          
069300     SET STATUS-IX TO 1                                                   
069400     SEARCH GODK-STATUS                                                   
069500       AT END                                                             
069600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
069700              DELIMITED BY SIZE INTO FELTEXT                              
069800         CALL FELLOG                                                      
069900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
070000     END-SEARCH                                                           
070100     .                                                                    
