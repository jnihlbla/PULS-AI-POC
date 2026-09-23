000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W4120200.                                        
000300 AUTHOR.                 KERSTIN JOHANSSON  GUIDE DATAKONSULT AB          
000400 DATE-WRITTEN.           OKT 1990.                                        
000500                                                                          
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*            BMP KONVERTERING AV ORDERTTANSAKTIONER FRÅN VR OCH           
001000*            ÖVRIGA TILL KOMMUNIKATIONS DB (TRANSAKTION 4251 OCH          
001100*            4252).                                                       
001200*            SEKVENSFILEN W41202 (GODKÄNDA POSTER SKAPAD AV               
001300*            PGM W41201) ÄR SORTERAD                                      
001400*            IDDISTR, IDKUNDNR, IDORDNR, IDPTYP, TIFILDAT, TIKLOCK        
001500*            INNAN KONVERTERINGEN STARTAR.                                
001600*            FÖR VARJE POST MED POSTTYP R50 SKAPAS EN                     
001700*            ORDERHUVUDTRANSAKTION.                                       
001800*            ORDERRADPOSTERNA MED POSTTYP R55 GRUPPERAS                   
001900*            MED 6 RADER I VARJE ORDERRADTRANSAKTION.                     
002000*            OM FLER RADER FINNS PÅ ORDERN SÄTTS FLSLUT TILL N            
002100*            ANNARS SÄTTS FLSLUT TILL J PÅ ORDERRADTRANSAKTIONEN.         
002200*            ORDERTRANSAKTIONERNA SKRIVS PÅ KOMMUNIKATIONS DB VIA         
002300*            W006KOM-SUBMODULEN                                           
002400*            CHECKPOINT TAGES FÖR VARJE NY ORDER.                         
002500*                                                                         
002600*    DATABASER: UPPDATERAR   KOMMUNIKATIONS DB                            
002700*                            WLKOMA-(WDP8)                                
002800*               UPPDATERAR   HÄNDELSE REGISTER (CHKPOINT)                 
002900*                            WLXXLT-(WDGX)                                
003000*                                                                         
003100*    ABENDKODER:                                                          
003200*            U0999      - FELLOG                                          
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500                                                                          
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900                                                                          
004000*                  INFIL: GODKÄNDA POSTER                                 
004100     SELECT  W41201                   ASSIGN TO    W41202D1.              
004200     SKIP3                                                                
004300 DATA DIVISION.                                                           
004400                                                                          
004500 FILE SECTION.                                                            
004600                                                                          
004700 FD  W41201                                                               
004800     LABEL RECORD STANDARD                                                
004900     RECORDING      V                                                     
005000     BLOCK CONTAINS 0.                                                    
005100                                                                          
005200*01  -COPY W412500 -L.                                                    
005300                                                                          
005400*01  -COPY W412550 -L.                                                    
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800*    -- CHECKED BY WY2000                                                 
005900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4120200'.            
006000 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
006100 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
006200 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
006300 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
006400 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
006500 77  DRAD-IX                     PIC S9(4)   COMP SYNC VALUE ZERO.        
006600 77  DRAD-IX-MAX                 PIC S9(4)   COMP SYNC VALUE +5.          
006700 77  RFS-IX                      PIC S9(4)   COMP SYNC VALUE ZERO.        
006800 77  MAX-RFS-IX                  PIC S9(4)   COMP SYNC VALUE +4.          
006900 77  W-ANT-ORDER-IN              PIC S9(7)   COMP-3.                      
007000 77  W-ANT-UPPD-CHKP             PIC S9(7)   COMP-3.                      
007100 77  JA                          PIC X       VALUE 'J'.                   
007200 77  NEJ                         PIC X       VALUE 'N'.                   
007300 77  W41201-EOF                  PIC X       VALUE 'N'.                   
007400 77  W-IDORDNR-DUBLET            PIC 9(7)    VALUE ZERO.                  
007500 77  SPAR-TIFILDAT               PIC X(6).                                
007600 77  SPAR-TIKLOCK                PIC X(8).                                
007700 77  SPAR-IDDISTR                PIC X(4).                                
007800 77  SPAR-IDKUNDNR               PIC X(6).                                
007900 77  SPAR-IDORDNR                PIC X(7).                                
008000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
008100 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008200     EJECT                                                                
008300                                                                          
008400 77  LDC-IDSYSTEM-SW             PIC X       VALUE 'N'.                   
008500     88  LDC-TILL-OVR                        VALUE 'J'.                   
008600                                                                          
008703 77  TACDIS-CUST-INFO-SW         PIC X       VALUE 'N'.                   
008803     88  TACDIS-CUST-INFO                    VALUE 'J'.                   
008903                                                                          
010000*    ---- AREA FÖR INFIL W41201                                           
010100 01  FILLER                      PIC X(8)    VALUE 'IN-AREA'.             
010200 01  IN-AREA                     PIC X(440).                              
010300*01  FILLER -COPY W412500 -PRE IN- -RED IN-AREA.                          
010400     EJECT                                                                
010500*01  FILLER -COPY W412550 -PRE IN- -RED IN-AREA.                          
010600     EJECT                                                                
010700*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
010800 01  DYNAMISKA-SUBPROGRAM.                                                
010900   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
011000   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
011100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
011200   03  W006KOM                   PIC X(8)    VALUE 'W006KOM '.            
011300   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
011400   03  WORKDAY                   PIC X(8)    VALUE 'WORKDAY '.            
011500     EJECT                                                                
011600*01  FILLER -COPY W0005       -PRE POSTSUM-.                              
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'WORKAREA   '.         
011900*   -COPY WORKAREA                                                        
012000     EJECT                                                                
012100*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
012200*                                                                         
012300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012400     SKIP3                                                                
012500 01  NYCKLAR-TILL-DLI.                                                    
012600     03  W-WDQ2CSEQ-X.                                                    
012700         05  W-IDDISTR           PIC S9(5)   VALUE +0 COMP-3.             
012800         05  W-IDKUNDNR          PIC S9(7)   VALUE +0 COMP-3.             
012900         05  W-IDKUNDRF.                                                  
013000           07  W-IDORDNR         PIC 9(7)    VALUE ZERO.                  
013100           07  FILLER            PIC X(3)    VALUE SPACE.                 
013200     SKIP3                                                                
013300     03  W-IDGMT-X.                                                       
013400         05  W-IDDISTR-GMT       PIC S9(5)   VALUE ZERO COMP-3.           
013500         05  W-IDKUNDNR-GMT      PIC S9(7)   VALUE ZERO COMP-3.           
013600                                                                          
013700     03  W-IDDC-B6-X.                                                     
013800         05 W-IDDC-B6                  PIC X(2).                          
013900                                                                          
014000*01  -COPY WDGX01                                                         
014100     EJECT                                                                
014200*    --- STATUS-KOD FRÅN IMS                                              
014300 01  STATUS-WS                   PIC XX.                                  
014400     88  SEGMENT-FINNS                       VALUE '  '.                  
014500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014800     88  IMS-EJ-OK                           VALUE 'XD'.                  
014900     SKIP2                                                                
015000 01  GODK-STATUSKODER.                                                    
015100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015200     SKIP3                                                                
015300 01  SSA1                        PIC X(64).                               
015400 01  SSA2                        PIC X(64).                               
015500     EJECT                                                                
015600*    IMS FUNKTIONSKODER                                                   
015700*    -COPY W0003                                                          
015800     EJECT                                                                
015900*    ---  DLI INPUT-OUTPUT AREA                                           
016000 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
016100 01  DLI-IO-AREA-OHUV.                                                    
016200     03  WLORQI01.                                                        
016300*        05  -COPY WDQ201                                                 
016400     EJECT                                                                
016500 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
016600 01  DLI-IO-AREA-GMTA.                                                    
016700     03  WLGMTA01.                                                        
016800*        05  -COPY WDB201                                                 
016900     EJECT                                                                
017000 01  FILLER                      PIC X(16)   VALUE 'WDI201-AREA'.         
017100 01  DLI-IO-WDI201.                                                       
017200*        03  -COPY WDI201                                                 
017300     EJECT                                                                
017400 01  FILLER                  PIC X(16)   VALUE 'XXLT-IO-AREA'.            
017500 01  XXLT-IO-AREA.                                                        
017600*03  FILLER  -COPY WDGX4566                                               
017700     EJECT                                                                
017800*                                                                         
017900 01  FILLER                  PIC X(16)   VALUE 'MSG-KOM-AREA'.            
018000*01  -COPY WMSGKOM                                                        
018100     EJECT                                                                
018200                                                                          
018300 01  FILLER                  PIC X(16)   VALUE 'MSG-IO-AREA'.             
018400     SKIP3                                                                
018500*01  -COPY WMSGAREA                                                       
018600     EJECT                                                                
018700*                                                                         
018800*    --- AREOR FÖR W006KOM SUBMODUL                                       
018900*                                                                         
019000 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
019100 01  KOM-IO-AREA.                                                         
019200   03  KOM-AREA                     PIC X(2400) VALUE SPACE.              
019300*03  FILLER  -COPY W4I25101 -PRE DHUV-  -RED KOM-AREA.                    
019400     EJECT                                                                
019500*03  FILLER  -COPY W4I25201 -PRE DRAD-  -RED KOM-AREA.                    
019600     EJECT                                                                
019700                                                                          
019800 LINKAGE SECTION.                                                         
019900*01  -COPY W0009         -PRE MSG-                                        
020000     SKIP3                                                                
020100 01  DISP-PCB                PIC X.                                       
020200 01  KOMA-PCB                PIC X.                                       
020300     EJECT                                                                
020400*01  -COPY W0008         -PRE XXLT-                                       
020500     05  FILLER              PIC X.                                       
020600     EJECT                                                                
020700*01  -COPY W0008         -PRE ORQL-                                       
020800     05  FILLER              PIC X.                                       
020900     EJECT                                                                
021000*01  -COPY W0008         -PRE GMTA-                                       
021100     05  FILLER              PIC X.                                       
021200     EJECT                                                                
021300*01  -COPY W0008         -PRE WDI2-                                       
021400     05  FILLER              PIC X.                                       
021500     EJECT                                                                
021600                                                                          
021700                                                                          
021800 PROCEDURE DIVISION  USING                                                
021900                     MSG-PCB                                              
022000                     DISP-PCB                                             
022100                     KOMA-PCB                                             
022200                     XXLT-PCB                                             
022300                     ORQL-PCB                                             
022400                     GMTA-PCB                                             
022500                     WDI2-PCB.                                            
022600 MAIN SECTION.                                                            
022700     ENTRY 'DLITCBL' USING                                                
022800                     MSG-PCB                                              
022900                     DISP-PCB                                             
023000                     KOMA-PCB                                             
023100                     XXLT-PCB                                             
023200                     ORQL-PCB                                             
023300                     GMTA-PCB                                             
023400                     WDI2-PCB.                                            
023500                                                                          
023600     PERFORM A-INITIERA                                                   
023700                                                                          
023800     PERFORM IMS-RESTART                                                  
023900     PERFORM IMS-LAS-ATERSTART                                            
024000     IF 4566-KVPOST > +0                                                  
024100        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
024200     ELSE                                                                 
024300        PERFORM S01-LAS-W41201                                            
024400     END-IF                                                               
024500     IF W41201-EOF = NEJ                                                  
024600        PERFORM S02-SPARA-ORDERIDENT                                      
024700        PERFORM S03-SKAPA-MSG-KOM-AREA                                    
024800     END-IF                                                               
024900                                                                          
025000     PERFORM UNTIL W41201-EOF = JA                                        
025100                                                                          
025200        PERFORM C-BEARBETA                                                
025300        PERFORM S01-LAS-W41201                                            
025400     END-PERFORM                                                          
025500                                                                          
025600     PERFORM Z-FINIT                                                      
025700     MOVE ZERO TO RETURN-CODE                                             
025800     GOBACK                                                               
025900     .                                                                    
026000     EJECT                                                                
026100                                                                          
026200 A-INITIERA SECTION.                                                      
026300     SKIP2                                                                
026400     OPEN INPUT W41201                                                    
026500                                                                          
026600     MOVE +0                   TO W-ANT-ORDER-IN                          
026700                                  W-ANT-UPPD-CHKP                         
026800                                  DRAD-IX                                 
026900     MOVE SPACE                TO MSG-AREA                                
027000                                                                          
027100     ACCEPT DAGENS-DATUM     FROM DATE                                    
027200                                                                          
027300     MOVE PROGRAM-NAMN         TO POSTSUM-PROGNAMN                        
027400     .                                                                    
027500     EJECT                                                                
027600 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
027700                                                                          
027800     PERFORM S01-LAS-W41201                                               
027900     PERFORM S02-SPARA-ORDERIDENT                                         
028000                                                                          
028100     PERFORM UNTIL W41201-EOF = JA  OR                                    
028200                     W-ANT-ORDER-IN = 4566-KVPOST                         
028300        PERFORM S01-LAS-W41201                                            
028400        IF SPAR-IDDISTR  = IN-OHUV-IDDISTR   AND                          
028500           SPAR-TIFILDAT = IN-OHUV-TIFILDAT AND                           
028600           SPAR-TIKLOCK  = IN-OHUV-TIKLOCK AND                            
028700           SPAR-IDKUNDNR = IN-OHUV-IDKUNDNR AND                           
028800           SPAR-IDORDNR  = IN-OHUV-IDORDNR                                
028900           CONTINUE                                                       
029000        ELSE                                                              
029100           ADD +1        TO W-ANT-ORDER-IN                                
029200           PERFORM S02-SPARA-ORDERIDENT                                   
029300        END-IF                                                            
029400     END-PERFORM                                                          
029500     IF W41201-EOF = JA                                                   
029600        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
029700                      TO FELTEXT                                          
029800        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
029900     END-IF                                                               
030000     .                                                                    
030100     EJECT                                                                
030200                                                                          
030300 C-BEARBETA SECTION.                                                      
030400                                                                          
030500*    UNDERSÖK OM NY ORDER                                                 
030600     IF SPAR-IDDISTR  = IN-OHUV-IDDISTR   AND                             
030700        SPAR-TIFILDAT = IN-OHUV-TIFILDAT AND                              
030800        SPAR-TIKLOCK  = IN-OHUV-TIKLOCK AND                               
030900        SPAR-IDKUNDNR = IN-OHUV-IDKUNDNR AND                              
031000        SPAR-IDORDNR  = IN-OHUV-IDORDNR                                   
031100        CONTINUE                                                          
031200     ELSE                                                                 
031300        PERFORM S05-AVSLUTA-ORDERRADTRANS                                 
031400        PERFORM CA-TAG-CHECKPOINT                                         
031500                                                                          
031600        PERFORM S02-SPARA-ORDERIDENT                                      
031700        PERFORM S03-SKAPA-MSG-KOM-AREA                                    
031800     END-IF                                                               
031900                                                                          
032000     IF IN-OHUV-IDPTYP = 'R50'                                            
032100        PERFORM CB-SKAPA-ORDERHUVUDTRANS                                  
032200     ELSE                                                                 
032300        IF IN-ORAD-IDPTYP = 'R55'                                         
032400           PERFORM CC-SKAPA-ORDERRADTRANS                                 
032500        ELSE                                                              
032600           MOVE 'FELAKTIG POSTTTYP PÅ INFILEN W41201'                     
032700                         TO FELTEXT                                       
032800           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
032900        END-IF                                                            
033000     END-IF                                                               
033100     .                                                                    
033200     EJECT                                                                
033300                                                                          
033400 CA-TAG-CHECKPOINT SECTION.                                               
033500     SKIP2                                                                
033600*    UPPDATERA ÅTERSTARTREGISTRET                                         
033700     PERFORM IMS-LAS-ATERSTART                                            
033800     ADD +1                    TO 4566-KVPOST                             
033900     ACCEPT 4566-TIUPPDAT FROM DATE                                       
034000     ACCEPT 4566-TIUPPTID FROM TIME                                       
034100                                                                          
034200     PERFORM IMS-REPL-ATERSTART                                           
034300                                                                          
034400*    TAG CHECKPOINT                                                       
034500     PERFORM IMS-CHECKPOINT                                               
034600     .                                                                    
034700     EJECT                                                                
034800                                                                          
034900 CB-SKAPA-ORDERHUVUDTRANS SECTION.                                        
035000     SKIP2                                                                
035100                                                                          
035200     MOVE ZERO                   TO W-IDORDNR-DUBLET                      
035500                                                                          
035600     IF IN-OHUV-IDSYSTEM = 'SOFT'                                         
035700                                                                          
035800*    FÖR SOFTVAROR BEHÖVS EN TRANS PER RAD EFTERSOM                       
035900*    IDARBREF OCH IDVIN LIGGER UTANFÖR GRUPPEN AV FEM RADER               
036000*    PÅ DET HÄR SÄTTET BEHÖVER VI INTE BYGGA OM MID-EN                    
036100                                                                          
036200        MOVE 1                   TO DRAD-IX-MAX                           
036300     ELSE                                                                 
036400        MOVE 5                   TO DRAD-IX-MAX                           
036500     END-IF                                                               
036600                                                                          
036700*DÖSKALLAR KAN KROCKA MED SOFTWARE                                        
036800     IF IN-OHUV-IDSYSTEM = 'OVR ' AND                                     
036900        IN-OHUV-BEKUNDRF = 'STOCK BO       '                              
037000       MOVE IN-OHUV-IDDISTR      TO W-IDDISTR                             
037100       IF IN-OHUV-IDKUNDNR = SPACE                                        
037200         MOVE ZERO               TO W-IDKUNDNR                            
037300       ELSE                                                               
037400         MOVE IN-OHUV-IDKUNDNR   TO W-IDKUNDNR                            
037500       END-IF                                                             
037600       MOVE IN-OHUV-IDORDNR      TO W-IDORDNR                             
037700       PERFORM IMS-GU-ORQL-WDQ201                                         
037800       IF SEGMENT-FINNS                                                   
037900         PERFORM UNTIL SEGMENT-SAKNAS                                     
038000           ADD +1   TO W-IDORDNR                                          
038100           IF W-IDORDNR > +9999                                           
038200             MOVE +1     TO W-IDORDNR                                     
038300           END-IF                                                         
038400           PERFORM IMS-GU-ORQL-WDQ201                                     
038500         END-PERFORM                                                      
038600         MOVE W-IDORDNR          TO W-IDORDNR-DUBLET                      
038700       END-IF                                                             
038800     END-IF                                                               
038900                                                                          
039000     IF IN-OHUV-IDSYSTEM = 'LDC ' AND IN-OHUV-CT-IDPTYP = 'TS1'           
039100       MOVE IN-OHUV-IDDISTR      TO W-IDDISTR                             
039200       IF IN-OHUV-IDKUNDNR = SPACE                                        
039300         MOVE ZERO               TO W-IDKUNDNR                            
039400       ELSE                                                               
039500         MOVE IN-OHUV-IDKUNDNR   TO W-IDKUNDNR                            
039600       END-IF                                                             
039700       MOVE IN-OHUV-IDORDNR      TO W-IDORDNR                             
039800       PERFORM IMS-GU-ORQL-WDQ201                                         
039900       IF SEGMENT-FINNS                                                   
040000           PERFORM UNTIL SEGMENT-SAKNAS                                   
040100             ADD +100 TO W-IDORDNR                                        
040200             IF W-IDORDNR > +94999                                        
040300               MOVE +50000 TO W-IDORDNR                                   
040400             END-IF                                                       
040500             PERFORM IMS-GU-ORQL-WDQ201                                   
040600           END-PERFORM                                                    
040700           MOVE W-IDORDNR        TO W-IDORDNR-DUBLET                      
040800       END-IF                                                             
040900     END-IF                                                               
041000                                                                          
042800                                                                          
042900     MOVE SPACE                  TO KOM-AREA                              
043000                                                                          
043100     COMPUTE MSG-KVLL = LENGTH OF DHUV-MID-W4I25101 + 17                  
043200                                                                          
043300     MOVE LOW-VALUE              TO MSG-KDZ1                              
043400     MOVE LOW-VALUE              TO MSG-KDZ2                              
043500     MOVE 'W4T251X '             TO MSG-KDTRANS-1                         
043600     MOVE '4251'                 TO MSG-IDTRANS-1                         
043700     MOVE '1'                    TO MSG-KDMFSFOR-1                        
043800                                                                          
043900     MOVE IN-OHUV-IDSYSTEM       TO DHUV-MID-IDSYSTEM                     
044000                                                                          
044100     MOVE IN-OHUV-IDDISTR        TO DHUV-MID-IDDISTR                      
044200     MOVE IN-OHUV-IDKUNDNR       TO DHUV-MID-IDKUNDNR                     
044300                                                                          
044400     IF IN-OHUV-IDSYSTEM = 'SOFT' OR 'LDC ' OR 'OVR '                     
044500       IF W-IDORDNR-DUBLET > ZERO                                         
044600         MOVE W-IDORDNR-DUBLET   TO DHUV-MID-IDORDNR                      
044700       ELSE                                                               
044800         MOVE IN-OHUV-IDORDNR    TO DHUV-MID-IDORDNR                      
044900       END-IF                                                             
045000     ELSE                                                                 
045100       MOVE IN-OHUV-IDORDNR      TO DHUV-MID-IDORDNR                      
045200     END-IF                                                               
045300                                                                          
045400     MOVE IN-OHUV-IDDISTR        TO W-IDDISTR-GMT                         
045500     MOVE IN-OHUV-IDKUNDNR       TO W-IDKUNDNR-GMT                        
045600     PERFORM IMS-GU-GMTA-WDB201                                           
045700                                                                          
045800*** BERÄKNA RFS  UTIFRÅN REP.DATUM OM DET FINNS                           
045900     IF (IN-OHUV-IDSYSTEM = 'LDC ' OR 'TACD')                             
046000     AND IN-OHUV-TIRFS > ZERO                                             
046100       IF SEGMENT-FINNS                                                   
046200         IF GMT-FLLDCKND = JA                                             
046300                                                                          
046400*TACDIS SKAPA INFO PÅ WDI201                                              
046500                                                                          
046600           PERFORM CBA-SKAPA-TACDIS-INFO                                  
046700                                                                          
046800           PERFORM CBB-CALC-RFS                                           
046900                                                                          
047000         ELSE                                                             
047100           MOVE IN-OHUV-TIRFS    TO DHUV-MID-TIRFS                        
047200           MOVE 'OVR '           TO DHUV-MID-IDSYSTEM                     
047300           MOVE JA               TO LDC-IDSYSTEM-SW                       
047400         END-IF                                                           
047500       ELSE                                                               
047600         MOVE IN-OHUV-TIRFS      TO DHUV-MID-TIRFS                        
047700         MOVE 'OVR '             TO DHUV-MID-IDSYSTEM                     
047800         MOVE JA                 TO LDC-IDSYSTEM-SW                       
047900       END-IF                                                             
048000     ELSE                                                                 
048100       IF (IN-OHUV-IDSYSTEM = 'LDC ' OR 'TACD')                           
048200*WDB201                                                                   
048300         IF SEGMENT-FINNS                                                 
048400           IF GMT-FLLDCKND = JA                                           
048500                                                                          
048600*TACDIS   SKAPA INFO PÅ WDI201                                            
048700             PERFORM CBA-SKAPA-TACDIS-INFO                                
048800           END-IF                                                         
048900         END-IF                                                           
049000       END-IF                                                             
049100       MOVE IN-OHUV-TIRFS        TO DHUV-MID-TIRFS                        
049200       MOVE IN-OHUV-KDORDTYP-LDC TO DHUV-MID-KDORDTYP-LDC                 
049300     END-IF                                                               
049400                                                                          
049500     MOVE IN-OHUV-KDORDKL        TO DHUV-MID-KDORDKL                      
049600     MOVE IN-OHUV-KDFRAKT        TO DHUV-MID-KDFRAKT                      
049700     MOVE IN-OHUV-BEKUNDRF       TO DHUV-MID-BEKUNDRF                     
049800     MOVE IN-OHUV-KDFAKTYP       TO DHUV-MID-KDFAKTYP                     
049900     MOVE IN-OHUV-FLRESTN        TO DHUV-MID-FLRESTN                      
050000     MOVE IN-OHUV-KDTPOTYP       TO DHUV-MID-KDTPOTYP                     
050100     MOVE IN-OHUV-TITPO          TO DHUV-MID-TITPO                        
050200     MOVE IN-OHUV-BELAGINS       TO DHUV-MID-BELAGINS                     
050303                                                                          
050403     MOVE NEJ                    TO TACDIS-CUST-INFO-SW                   
050503*    *IF TACDIS AND IGROSS=0 THERE CAN BE CUSTOMER INFO                   
050603*    *IN ADDRESS FIELDS. THIS CUSTOMER INFO SHALL NOT                     
050703*    *BE UPDATED IN ORDERHEAD ONLY IN WDI2. (TAKF-)                       
050803*    *W4025100 WILL UPDATE ORDERHEAD WITH ADDRESS FROM                    
050903*    *CUSTOMER FILE IN THIS CASE.                                         
051003     IF (IN-OHUV-IDSYSTEM = 'LDC ' OR 'TACD') AND                         
051103       GMT-FLLDCKND = JA AND IN-OHUV-IDGROSS = ZERO                       
051203         MOVE JA                 TO TACDIS-CUST-INFO-SW                   
051303     END-IF                                                               
051403                                                                          
051803     IF TACDIS-CUST-INFO                                                  
051903        MOVE SPACE                TO DHUV-MID-BEGMT-RAD1                  
052003     ELSE                                                                 
052103        MOVE IN-OHUV-BEGODSM-RAD1 TO DHUV-MID-BEGMT-RAD1                  
052203     END-IF                                                               
052703     IF TACDIS-CUST-INFO                                                  
052803        MOVE SPACE                TO DHUV-MID-BEGMT-RAD2                  
052903     ELSE                                                                 
053003        MOVE IN-OHUV-BEGODSM-RAD2 TO DHUV-MID-BEGMT-RAD2                  
053103     END-IF                                                               
053303     MOVE IN-OHUV-IDGROSS         TO DHUV-MID-IDGROSS                     
053703     IF TACDIS-CUST-INFO                                                  
053803        MOVE SPACE                TO DHUV-MID-ADGMT-GATA                  
053903     ELSE                                                                 
054003        MOVE IN-OHUV-ADGODSM-RAD1 TO DHUV-MID-ADGMT-GATA                  
054103     END-IF                                                               
054603     IF TACDIS-CUST-INFO                                                  
054703        MOVE SPACE                TO DHUV-MID-ADGMT-PADR                  
054803     ELSE                                                                 
054903        MOVE IN-OHUV-ADGODSM-RAD2 TO DHUV-MID-ADGMT-PADR                  
055003     END-IF                                                               
055203     MOVE IN-OHUV-KDROPACK        TO DHUV-MID-KDROPACK                    
055303     MOVE IN-OHUV-IDKONTO         TO DHUV-MID-IDKONTO                     
055403     MOVE IN-OHUV-IDANALYS        TO DHUV-MID-IDANALYS                    
055503     MOVE IN-OHUV-IDKST           TO DHUV-MID-IDKST                       
055603                                                                          
055703     IF IN-OHUV-IDSYSTEM = 'SOFT'                                         
055803       IF W-IDORDNR-DUBLET > ZERO                                         
055903         MOVE 'VADIS'            TO DHUV-MID-BEVARREF(1:5)                
056003         MOVE IN-OHUV-IDORDNR(3:5) TO DHUV-MID-BEVARREF(6:5)              
056103       ELSE                                                               
056203         MOVE IN-OHUV-BEVARREF   TO DHUV-MID-BEVARREF                     
056303       END-IF                                                             
056403     ELSE                                                                 
056503       MOVE IN-OHUV-BEVARREF     TO DHUV-MID-BEVARREF                     
056603     END-IF                                                               
056703                                                                          
056803     MOVE IN-OHUV-KDTULLVE       TO DHUV-MID-KDTULLVE                     
056903     MOVE IN-OHUV-KDNOTES        TO DHUV-MID-KDNOTES                      
057003     MOVE IN-OHUV-FLAUTFAK       TO DHUV-MID-FLAUTFAK                     
057103     MOVE IN-OHUV-FLAUTPAC       TO DHUV-MID-FLAUTPAC                     
057203     MOVE IN-OHUV-FLEMBORD       TO DHUV-MID-FLEMBORD                     
057303     MOVE IN-OHUV-FLOVRLEV       TO DHUV-MID-FLOVRLEV                     
057403     MOVE IN-OHUV-IDFTG          TO DHUV-MID-IDFTG                        
057503     MOVE IN-OHUV-FLLSBOK        TO DHUV-MID-FLLSBOK                      
057603     MOVE IN-OHUV-IDDC           TO DHUV-MID-IDDC                         
057703* IDDEPT FINNS FÖR TACDIS-ORDER                                           
057803     MOVE IN-OHUV-IDDEPT         TO DHUV-MID-IDDEPT                       
057903     MOVE SPACE                  TO DHUV-MID-ADBET                        
058003     MOVE SPACE                  TO DHUV-MID-BEBET                        
058103     MOVE SPACE                  TO DHUV-MID-IDSKYLT                      
058203                                    DHUV-MID-IDKAMPRF                     
058303     MOVE NEJ                    TO DHUV-MID-FLFORBI                      
058403                                    DHUV-MID-FLORDTIL                     
058603     MOVE IN-OHUV-IDBILREG       TO DHUV-MID-IDBILREG                     
058703     MOVE IN-OHUV-IDVIN          TO DHUV-MID-IDVIN                        
058803     MOVE IN-OHUV-IDCISNR        TO DHUV-MID-IDCISNR                      
058903                                                                          
059003     MOVE KOM-AREA               TO MSG-INDATA-MINUS-1-TRANSKOD           
059103     CALL W006KOM USING MSG-PCB                                           
059203                        DISP-PCB                                          
059303                        KOMA-PCB                                          
059403                        MSG-KOM-WMSGKOM                                   
059503                        MSG-IO-AREA                                       
059603     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
059703*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                         
059803*       DUBBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA                 
059903        MOVE ' FELAKTIG DATUM,TID PÅ INPUTFIL W41201A'                    
060003                      TO FELTEXT                                          
060103        DISPLAY ' FELAKTIG DATUM,TID INPUTFIL W41201A'                    
060203        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
060303     END-IF                                                               
060403                                                                          
060503     MOVE SPACE       TO KOM-AREA                                         
060603     .                                                                    
060703     EJECT                                                                
060803                                                                          
060903 CBA-SKAPA-TACDIS-INFO    SECTION.                                        
061003                                                                          
061103     IF IN-OHUV-IDDISTR NUMERIC AND IN-OHUV-IDDISTR > ZERO                
061203     AND IN-OHUV-IDKUNDNR NUMERIC                                         
061303                                                                          
061403*TACDIS TEST GMT-FLLDCKND I CB-SKAPA-ORDERHUVUDTRANS                      
061503       IF (IN-OHUV-IDSYSTEM = 'LDC ' OR 'TACD')                           
061603                                                                          
061703         MOVE IN-OHUV-IDDISTR    TO TAKF-IDDISTR                          
061803         MOVE IN-OHUV-IDKUNDNR   TO TAKF-IDKUNDNR                         
061903         MOVE SPACE              TO TAKF-IDKUNDRF                         
062003         MOVE IN-OHUV-IDORDNR    TO TAKF-IDORDNR7                         
062103         MOVE IN-OHUV-BEMEKAN    TO TAKF-BEMEKAN                          
062203         MOVE IN-OHUV-BETELNR-TACD TO TAKF-BETELNR-TACD                   
062303         MOVE IN-OHUV-FLFPLOCK   TO TAKF-FLFPLOCK                         
062403         MOVE IN-OHUV-IDBILREG   TO TAKF-IDBILREG                         
062503         MOVE IN-OHUV-IDGROSS    TO TAKF-IDGROSS                          
062603         MOVE IN-OHUV-TETACDBO   TO TAKF-TETACDBO                         
062703         MOVE IN-OHUV-TIHHMM     TO TAKF-TIHHMM                           
062803         MOVE IN-OHUV-BEGODSM-RAD1 TO TAKF-BEGMT-RAD1                     
062903         MOVE IN-OHUV-BEGODSM-RAD2 TO TAKF-BEGMT-RAD2                     
062904         MOVE IN-OHUV-ADGODSM-RAD1 TO TAKF-ADGMT-GATA                     
063103         MOVE IN-OHUV-ADGODSM-RAD2 TO TAKF-ADGMT-PADR                     
063203                                                                          
063303         ACCEPT TAKF-TIREGDAT    FROM DATE                                
063403                                                                          
063503         PERFORM IMS-ISRT-WDI201                                          
063603       END-IF                                                             
063703     END-IF                                                               
063803     .                                                                    
063903     EJECT                                                                
064003                                                                          
064103 CBB-CALC-RFS SECTION.                                                    
064203                                                                          
064303     MOVE GMT-IDDC-BULK(1)       TO WORK-IDDC                             
064403     MOVE +002                   TO WORK-KDCALL                           
064503     MOVE +001                   TO WORK-KVWORKD                          
064603     IF IN-OHUV-TIREPDAT = ZERO                                           
064703       MOVE DAGENS-DATUM         TO WORK-TIAAMMDD-FOM                     
064803     ELSE                                                                 
064903       MOVE IN-OHUV-TIREPDAT     TO WORK-TIAAMMDD-FOM                     
065003     END-IF                                                               
065103     CALL WORKDAY             USING WORK-KDCALL                           
065203                                    WORK-DATE-AREA                        
065303                                    WORK-KDSVAR                           
065403     IF WORK-KDSVAR-FEL                                                   
065503       MOVE 'SECT CBB-1, DATUM SAKNAS I WORKDAY'                          
065603                                 TO FELTEXT                               
065703       CALL ABEND             USING RKOD-ABEND-UTAN-DUMP                  
065803     ELSE                                                                 
065903       MOVE +003                 TO WORK-KDCALL                           
066003       MOVE GMT-KVDAGAR-RFS-DEF  TO WORK-KVWORKD                          
066103       PERFORM                                                            
066203       VARYING RFS-IX FROM 1 BY 1                                         
066303         UNTIL RFS-IX > MAX-RFS-IX                                        
066403         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
066503           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
066603                                 TO WORK-KVWORKD                          
066703         END-IF                                                           
066803       END-PERFORM                                                        
066903       ADD +1                    TO WORK-KVWORKD                          
067003*      +1 FÖR ATT VARIABELN SKALL KUNNA INNEHÅLLA                         
067103*      ANTAL DAGAR FÖRE RFS.                                              
067203*      0 GER DÅ SAMMA DAG, 1 GER FÖRSTA ARBETSDAG FÖRE OSV...             
067303*      OM VI INTE ADDERAR +1 SKULLE VARIABELN SÄTTAS SÅ                   
067403*      1 GER SAMMA DAG, 2 FÖRSTA ARBETSDAG FÖRE OSV...                    
067503*                                                                         
067603       CALL WORKDAY           USING WORK-KDCALL                           
067703                                    WORK-DATE-AREA                        
067803                                    WORK-KDSVAR                           
067903       IF WORK-KDSVAR-FEL                                                 
068003         MOVE 'SECT CBB-2, DATUM SAKNAS I WORKDAY'                        
068103                                 TO FELTEXT                               
068203         CALL ABEND           USING RKOD-ABEND-UTAN-DUMP                  
068303       ELSE                                                               
068403         IF WORK-TIAAMMDD-FOM < DAGENS-DATUM                              
068503           MOVE +002             TO WORK-KDCALL                           
068603           MOVE +001             TO WORK-KVWORKD                          
068703           MOVE DAGENS-DATUM     TO WORK-TIAAMMDD-FOM                     
068803           CALL WORKDAY       USING WORK-KDCALL                           
068903                                    WORK-DATE-AREA                        
069003                                    WORK-KDSVAR                           
069103           IF WORK-KDSVAR-FEL                                             
069203             MOVE 'SECT CBB-3, DATUM SAKNAS I WORKDAY'                    
069303                                 TO FELTEXT                               
069403             CALL ABEND       USING RKOD-ABEND-UTAN-DUMP                  
069503           ELSE                                                           
069603             MOVE WORK-TIAAMMDD-TOM                                       
069703                                 TO DHUV-MID-TIRFS                        
069803           END-IF                                                         
069903         ELSE                                                             
070003           MOVE WORK-TIAAMMDD-FOM                                         
070103                                 TO DHUV-MID-TIRFS                        
070203         END-IF                                                           
070303       END-IF                                                             
070403     END-IF                                                               
070503                                                                          
070603     MOVE IN-OHUV-TIREPDAT       TO DHUV-MID-TIREPDAT                     
070703     MOVE IN-OHUV-KDORDTYP-LDC   TO DHUV-MID-KDORDTYP-LDC                 
070803     .                                                                    
070903     EJECT                                                                
071003 CC-SKAPA-ORDERRADTRANS SECTION.                                          
071103                                                                          
071203     ADD +1         TO DRAD-IX                                            
071303     IF DRAD-IX > DRAD-IX-MAX                                             
071403        PERFORM S05-AVSLUTA-ORDERRADTRANS                                 
071503        ADD +1      TO DRAD-IX                                            
071603     END-IF                                                               
071703     IF DRAD-IX = 1                                                       
071803        MOVE SPACE               TO KOM-AREA                              
071903     END-IF                                                               
072003                                                                          
072103     COMPUTE MSG-KVLL = LENGTH OF DRAD-MID-W4I25201 + 17                  
072203                                                                          
072303     MOVE LOW-VALUE              TO MSG-KDZ1                              
072403     MOVE LOW-VALUE              TO MSG-KDZ2                              
072503     MOVE 'W4T252X '             TO MSG-KDTRANS-1                         
072603     MOVE '4252'                 TO MSG-IDTRANS-1                         
072703     MOVE '1'                    TO MSG-KDMFSFOR-1                        
072803     MOVE IN-ORAD-IDSYSTEM       TO DRAD-MID-IDSYSTEM                     
072903     MOVE IN-ORAD-IDDISTR        TO DRAD-MID-IDDISTR                      
073003     MOVE IN-ORAD-IDKUNDNR       TO DRAD-MID-IDKUNDNR                     
073103                                                                          
073203     IF IN-ORAD-IDSYSTEM = 'SOFT' OR IN-ORAD-IDSYSTEM = 'LDC '            
073303                                  OR IN-ORAD-IDSYSTEM = 'OVR '            
073403       IF W-IDORDNR-DUBLET > ZERO                                         
073503         MOVE W-IDORDNR-DUBLET   TO DRAD-MID-IDORDNR                      
073603       ELSE                                                               
073703         MOVE IN-ORAD-IDORDNR    TO DRAD-MID-IDORDNR                      
073803       END-IF                                                             
073903     ELSE                                                                 
074003         MOVE IN-ORAD-IDORDNR    TO DRAD-MID-IDORDNR                      
074103     END-IF                                                               
074203                                                                          
074303     IF IN-ORAD-IDSYSTEM = 'LDC ' AND                                     
074403        LDC-TILL-OVR                                                      
074503       MOVE 'OVR '               TO DRAD-MID-IDSYSTEM                     
074603     END-IF                                                               
074703                                                                          
074803     MOVE IN-ORAD-BEVOLREF       TO DRAD-MID-BEVOLREF                     
074903     MOVE SPACE                  TO DRAD-MID-IDKUNDRF-RO                  
075003     MOVE IN-ORAD-IDKLIENT       TO DRAD-MID-IDKLIENT                     
075103     MOVE IN-ORAD-IDARBREF       TO DRAD-MID-IDARBREF                     
075203     MOVE IN-ORAD-IDVIN          TO DRAD-MID-IDVIN                        
075303     MOVE 'N'                    TO DRAD-MID-FLSLUT                       
075403     MOVE IN-ORAD-IDARTNR        TO DRAD-MID-IDARTNR   (DRAD-IX)          
075503     MOVE IN-ORAD-REKSIFFR       TO DRAD-MID-REKSIFFR  (DRAD-IX)          
075603     MOVE IN-ORAD-KVBEART        TO DRAD-MID-KVBEART   (DRAD-IX)          
075703     MOVE IN-ORAD-PRARTNTO       TO DRAD-MID-PRARTNTO  (DRAD-IX)          
075803     MOVE IN-ORAD-TITPO          TO DRAD-MID-TITPO     (DRAD-IX)          
075903     MOVE IN-ORAD-FLRESTN        TO DRAD-MID-FLRESTN   (DRAD-IX)          
076003     MOVE IN-ORAD-KDKVBRYT       TO DRAD-MID-KDKVBRYT  (DRAD-IX)          
076103     MOVE IN-ORAD-FLINVEST       TO DRAD-MID-FLINVEST  (DRAD-IX)          
076203     MOVE IN-ORAD-KDVRINFO       TO DRAD-MID-KDVRINFO  (DRAD-IX)          
076303     MOVE ZERO                   TO DRAD-MID-IDKONTO   (DRAD-IX)          
076403     MOVE SPACE                  TO DRAD-MID-IDKST     (DRAD-IX)          
076503     MOVE IN-ORAD-BERADREF       TO DRAD-MID-BERADREF  (DRAD-IX)          
076603     MOVE IN-ORAD-KDDSP          TO DRAD-MID-KDDSP     (DRAD-IX)          
076703     MOVE IN-ORAD-FLSLATT        TO DRAD-MID-FLSLATT   (DRAD-IX)          
076803     MOVE IN-ORAD-IDBIL          TO DRAD-MID-IDBIL     (DRAD-IX)          
076903     MOVE IN-ORAD-PRARTNTO-LOC  TO DRAD-MID-PRARTNTO-LOC (DRAD-IX)        
077003     MOVE IN-ORAD-PRARTBTO-LOC  TO DRAD-MID-PRARTBTO-LOC (DRAD-IX)        
077103     MOVE IN-ORAD-KDVALISO      TO DRAD-MID-KDVALISO     (DRAD-IX)        
077203     MOVE IN-ORAD-KDVAT         TO DRAD-MID-KDVAT        (DRAD-IX)        
077303     MOVE IN-ORAD-RERAB         TO DRAD-MID-RERAB        (DRAD-IX)        
077403     MOVE IN-ORAD-KDRAB         TO DRAD-MID-KDRAB        (DRAD-IX)        
077503     MOVE IN-ORAD-BEART-VIPS    TO DRAD-MID-BEART-VIPS   (DRAD-IX)        
077603     MOVE IN-ORAD-ADLAGOMR-CD    TO DRAD-MID-ADLAGOMR-CD (DRAD-IX)        
077703     MOVE IN-ORAD-ADGANG-CD      TO DRAD-MID-ADGANG-CD  (DRAD-IX)         
077803     MOVE IN-ORAD-ADPLATS-CD     TO DRAD-MID-ADPLATS-CD (DRAD-IX)         
077903     MOVE IN-ORAD-IDKUNDRF-WIP   TO DRAD-MID-IDKUNDRF-WIP(DRAD-IX)        
078003     .                                                                    
078103     EJECT                                                                
078203                                                                          
078303 Z-FINIT    SECTION.                                                      
078403     SKIP2                                                                
078503     PERFORM S05-AVSLUTA-ORDERRADTRANS                                    
078603                                                                          
078703     CLOSE  W41201                                                        
078803                                                                          
078903*    NOLLA ÅTERSTARTINFORMATIONEN                                         
079003     PERFORM IMS-LAS-ATERSTART                                            
079103     MOVE +0                   TO 4566-KVPOST                             
079203     ACCEPT 4566-TIUPPDAT FROM DATE                                       
079303     ACCEPT 4566-TIUPPTID FROM TIME                                       
079403                                                                          
079503     PERFORM IMS-REPL-ATERSTART                                           
079603                                                                          
079703     MOVE 'S'      TO POSTSUM-OPKOD                                       
079803     CALL POSTSUM USING POSTSUM-PARM                                      
079903     .                                                                    
080003     EJECT                                                                
080103                                                                          
080203 S01-LAS-W41201 SECTION.                                                  
080303     SKIP2                                                                
080403     READ W41201 INTO IN-AREA                                             
080503       AT END                                                             
080603          MOVE JA TO W41201-EOF                                           
080703     END-READ                                                             
080803                                                                          
080903     IF W41201-EOF = NEJ                                                  
081003        MOVE 'W41201'       TO POSTSUM-FDNAMN                             
081103        MOVE 'W41202D1'     TO POSTSUM-DDNAMN2                            
081203        MOVE IN-OHUV-IDPTYP TO POSTSUM-TRANSTYP                           
081303        CALL POSTSUM USING POSTSUM-PARM                                   
081403     END-IF                                                               
081503     .                                                                    
081603     EJECT                                                                
081703                                                                          
081803 S02-SPARA-ORDERIDENT SECTION.                                            
081903     SKIP2                                                                
082003     MOVE NEJ                       TO LDC-IDSYSTEM-SW                    
082103     MOVE IN-OHUV-TIFILDAT          TO SPAR-TIFILDAT                      
082203     MOVE IN-OHUV-TIKLOCK           TO SPAR-TIKLOCK                       
082303     MOVE IN-OHUV-IDDISTR           TO SPAR-IDDISTR                       
082403     MOVE IN-OHUV-IDKUNDNR          TO SPAR-IDKUNDNR                      
082503     MOVE IN-OHUV-IDORDNR           TO SPAR-IDORDNR                       
082603     .                                                                    
082703     EJECT                                                                
082803                                                                          
082903 S03-SKAPA-MSG-KOM-AREA SECTION.                                          
083003     SKIP2                                                                
083103     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
083203     MOVE +54                    TO MSG-KOM-KVLL                          
083303     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
083403     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
083503     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
083603     MOVE IN-OHUV-IDCPYTXT       TO MSG-KOM-IDCPYTXT                      
083703     MOVE IN-OHUV-IDSYSTEM       TO MSG-KOM-IDSNDNOD                      
083803     MOVE IN-OHUV-IDDISTR        TO MSG-KOM-IDSNDNOD (5:4)                
083903     MOVE 'W4120200'             TO MSG-KOM-IDSNDJOB                      
084003     MOVE IN-OHUV-TIFILDAT       TO MSG-KOM-TIREGDAT                      
084103     MOVE IN-OHUV-TIKLOCK        TO MSG-KOM-TIKLOCK                       
084203     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
084303     .                                                                    
084403     EJECT                                                                
084503                                                                          
084603 S05-AVSLUTA-ORDERRADTRANS SECTION.                                       
084703     SKIP2                                                                
084803                                                                          
084903     IF DRAD-IX > 0                                                       
085003        IF DRAD-IX NOT > DRAD-IX-MAX                                      
085103           MOVE 'J'              TO DRAD-MID-FLSLUT                       
085203        END-IF                                                            
085303                                                                          
085403        MOVE KOM-AREA            TO MSG-INDATA-MINUS-1-TRANSKOD           
085503        CALL W006KOM USING MSG-PCB                                        
085603                           DISP-PCB                                       
085703                           KOMA-PCB                                       
085803                           MSG-KOM-WMSGKOM                                
085903                           MSG-IO-AREA                                    
086003        IF MSG-KOM-IDMFSMED NOT = SPACE                                   
086103*          FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                      
086203*          DUBBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA              
086303           MOVE ' FELAKTIG DATUM,TID PÅ INPUTFIL W41201B'                 
086403                         TO FELTEXT                                       
086503           DISPLAY ' FELAKTIG DATUM,TID INPUTFIL W41201B'                 
086603           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
086703        END-IF                                                            
086803                                                                          
086903     END-IF                                                               
087003                                                                          
087103     MOVE ZERO     TO DRAD-IX                                             
087203     MOVE SPACE    TO KOM-AREA                                            
087303     .                                                                    
087403     EJECT                                                                
087503                                                                          
087603* IMS SECTIONER                                                           
087703     SKIP3                                                                
087803                                                                          
087903 IMS-RESTART SECTION.                                                     
088003     SKIP2                                                                
088103     MOVE SPACE TO MSG-IO-AREA-1                                          
088203     MOVE '  ' TO GODK-STATUSKODER                                        
088303     CALL CBLTDLI USING XRST MSG-PCB                                      
088403                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
088503                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
088603     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
088703     PERFORM IMS-STATUSKONTROLL                                           
088803     .                                                                    
088903                                                                          
089003 IMS-CHECKPOINT SECTION.                                                  
089103     MOVE PROGRAM-NAMN TO MSG-IO-AREA-1                                   
089203     MOVE '  XD'       TO GODK-STATUSKODER                                
089303     CALL CBLTDLI USING CHKP MSG-PCB                                      
089403                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
089503                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
089603     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
089703     PERFORM IMS-STATUSKONTROLL                                           
089803     IF IMS-EJ-OK                                                         
089903       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
090003       MOVE ' IMS-KONTROLLREGION EJ TILLGÄNGLIG '                         
090103                            TO FELTEXT                                    
090203       CALL FELLOG                                                        
090303     END-IF                                                               
090403     .                                                                    
090503     EJECT                                                                
090603 IMS-GU-ORQL-WDQ201 SECTION.                                              
090703                                                                          
090803     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
090903          DELIMITED BY SIZE INTO SSA1                                     
091003     MOVE '  GE'               TO GODK-STATUSKODER                        
091103     CALL CBLTDLI USING GU ORQL-PCB DLI-IO-AREA-OHUV SSA1                 
091203     MOVE ORQL-STATUS-CODE    TO STATUS-WS                                
091303     PERFORM IMS-STATUSKONTROLL                                           
091403     .                                                                    
091503     SKIP2                                                                
091603 IMS-GU-GMTA-WDB201 SECTION.                                              
091703                                                                          
091803     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
091903          DELIMITED BY SIZE INTO SSA1                                     
092003     MOVE '  GE' TO GODK-STATUSKODER                                      
092103     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-GMTA SSA1                 
092203     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
092303     PERFORM IMS-STATUSKONTROLL                                           
092403     .                                                                    
092503     SKIP3                                                                
092603                                                                          
092703 IMS-ISRT-WDI201      SECTION.                                            
092803                                                                          
092903     MOVE   'WDI201'        TO SSA1                                       
093003     MOVE '  II' TO GODK-STATUSKODER                                      
093103     CALL CBLTDLI USING ISRT WDI2-PCB DLI-IO-WDI201 SSA1                  
093203     MOVE WDI2-STATUS-CODE TO STATUS-WS                                   
093303     PERFORM IMS-STATUSKONTROLL                                           
093403     SKIP3                                                                
093503     .                                                                    
093603                                                                          
093703 IMS-LAS-ATERSTART SECTION.                                               
093803                                                                          
093903*    MOVE 'WLXXLT11 '    TO SSA2                                          
094003     MOVE '4565'         TO IDHTYP                                        
094103     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
094203     STRING 'WLXXLT01(WDGXKEY  =' WDGX01 ')'                              
094303                    DELIMITED BY SIZE INTO SSA1                           
094403     STRING 'WLXXLT11(KDSEGKEY =' '1' ')'                                 
094503            DELIMITED BY SIZE INTO SSA2                                   
094603     MOVE '  '           TO GODK-STATUSKODER                              
094703     CALL CBLTDLI USING GHU XXLT-PCB XXLT-IO-AREA SSA1 SSA2               
094803     MOVE XXLT-STATUS-CODE TO STATUS-WS                                   
094903     PERFORM IMS-STATUSKONTROLL                                           
095003     .                                                                    
095103                                                                          
095203 IMS-REPL-ATERSTART SECTION.                                              
095303     SKIP2                                                                
095403     MOVE '  '             TO GODK-STATUSKODER                            
095503     CALL CBLTDLI USING REPL XXLT-PCB XXLT-IO-AREA                        
095603     MOVE XXLT-STATUS-CODE TO STATUS-WS                                   
095703     PERFORM IMS-STATUSKONTROLL                                           
095803     .                                                                    
095903                                                                          
096003 IMS-STATUSKONTROLL SECTION.                                              
096103     SET STATUS-IX TO 1                                                   
096203     SEARCH GODK-STATUS                                                   
096303       AT END                                                             
096403         MOVE ' STATUSKOD FRÅN IMS EJ TILLÅTEN '                          
096503                            TO FELTEXT                                    
096603         CALL FELLOG                                                      
096703       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
096803         CONTINUE                                                         
096903     END-SEARCH                                                           
097001     .                                                                    
