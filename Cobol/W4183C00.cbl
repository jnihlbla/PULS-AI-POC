000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4183C00.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   08/08/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000800*    FUNKTION:                                                            
000900*        BMP SOM TAR EMOT EN TRANS MED FAKTURERADE RADER FRÅN             
001000*        BILL-IT AV HANDLING FEE.SKAPAR EN UT-FIL FÖR ATT UPPDAT-         
001100*        ERA TABELL TP8LRET, EN FIL MED DATA SOM SKALL SKICKAS            
001200*        TILL EKONOMI OCH BOKA I SAP OCH EN UT-FIL MED DATA SOM           
001300*        SKALL SKICKAS TILL VIPS. VIPSBITEN SKALL ENBART FÖRBE-           
001400*        REDAS I NULÄGET TILLS VIPS KAN BYGGA EN MOTTAGNING.              
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4183CT                                             
001800*        MID:         WF2112I1 (VIA WZ01)                                 
001900*                                                                         
002000*    E-TRACKER 880053  DATUM 20080829                                     
002010*    E-TRACKER 8258527 DATUM 20100205                                     
002100*                                                                         
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- UPPDAT.POSTER TILL TP8LRET                                 
003100     SELECT W418C1                     ASSIGN TO W4183CD1.                
003200     SKIP2                                                                
003300*          --- EKONOMIFIL TILL RUTIN W510D2                               
003400     SELECT W418C2                     ASSIGN TO W4183CD2.                
003500     SKIP2                                                                
003600*          --- BILLIT-RADER MED FAKTURAINFO TILL VIPS                     
003700     SELECT W418C3                     ASSIGN TO W4183CD3.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W418C1                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY W418C1 -PRE  UT1-  -L.                                    
004800     SKIP3                                                                
004900 FD  W418C2                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  POST -COPY W51060 -PRE  UT2-  -L.                                    
005400     SKIP3                                                                
005500 FD  W418C3                                                               
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800                                                                          
005900*01  POST -COPY W4183C -PRE  UT3-  -L.                                    
006000     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006200                                                                          
006300 77  IDPGM                       PIC X(8)    VALUE 'W4183C00'.            
006400 77  JA                          PIC X       VALUE 'J'.                   
006500 77  NEJ                         PIC X       VALUE 'N'.                   
006610 77  SPAR-IDFINDOC               PIC X(9)    VALUE SPACE.                 
006620 77  WS-IDFINDOC                 PIC X(9)    VALUE SPACE.                 
006700 77  W-ANT                       PIC S9(3)   VALUE ZERO COMP-3.           
006800 77  WS-IDDISTR                  PIC 9(5)    VALUE ZERO.                  
006900 77  WS-IDKUNDNR                 PIC 9(7)    VALUE ZERO.                  
007000                                                                          
007100 77  FOERSTA-POST-SW             PIC X       VALUE 'J'.                   
007200     88 FOERSTA-POST                         VALUE 'J'.                   
007300                                                                          
007400     SKIP2                                                                
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
007900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008000 01  FILLER REDEFINES DAGENS-DATUM.                                       
008100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008400     EJECT                                                                
008500 01  DYNAMISKA-SUBPROGRAM.                                                
008600*                                                                         
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009000     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
009100     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
009200     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
009300     EJECT                                                                
009400*    --- PARAMETRAR TILL POSTSUM                                          
009500*                                                                         
009600*01  -COPY W0005   -PRE  POSTSUM-                                         
009700     EJECT                                                                
009800*    --- PARAMETRAR TILL W009CIA                                          
009900*01  -COPY W009CIA                                                        
010000                                                                          
010100     EJECT                                                                
010200*    --- AREOR FÖR KOMMUNIKATION                                          
010300 01  FILLER                      PIC X(16)   VALUE 'RECEIVE-AREA'.        
010400*01  -COPY WZ01RECV                                                       
010500     SKIP3                                                                
010600 01  RECV-DATA.                                                           
010700*03  -COPY  WZ01REQU                                                      
010800*03  -COPY  WF2112I1  -PRE MID-                                           
010900                                                                          
011000 01  KDRC-DISPLAY                PIC Z(5).                                
011100                                                                          
011200     SKIP3                                                                
011300 01  DECAREA.                                                             
011400* 03  WDECAREA   -COPY WDECAREA                                           
011500     EJECT                                                                
011600 01  UT1-AREA-START              PIC X(24)   VALUE                        
011700                                             'UT1-AREA-START'.            
011800     SKIP2                                                                
011900                                                                          
012000*01  AREA -COPY W418C1     -PRE UT1-                                      
012100     EJECT                                                                
012200 01  UT2-AREA-START              PIC X(24)   VALUE                        
012300                                             'UT2-AREA-START'.            
012400     SKIP2                                                                
012500                                                                          
012600*01  UT2-AREA -COPY W51060                                                
012700     EJECT                                                                
012800 01  UT3-AREA-START              PIC X(24)   VALUE                        
012900                                             'UT3-AREA-START'.            
013000     SKIP2                                                                
013100                                                                          
013200*01  AREA -COPY W4183C     -PRE UT3-                                      
013300*                                                                         
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013600     EJECT                                                                
013700 PROCEDURE DIVISION.                                                      
013800 MAIN SECTION.                                                            
013900                                                                          
014000     SKIP2                                                                
014100     PERFORM S01-RECV-OPEN                                                
014200     PERFORM S02-RECV-MESSAGE                                             
014300     PERFORM A-INIT                                                       
014400     PERFORM UNTIL RECV-KDRC > ZERO                                       
014500       PERFORM B-BEHANDLA-POSTER                                          
014600       PERFORM S02-RECV-MESSAGE                                           
014700     END-PERFORM                                                          
014800     PERFORM S03-RECV-CLOSE                                               
014900                                                                          
015000                                                                          
015100     PERFORM Z-FINIT                                                      
015200                                                                          
015300     MOVE ZERO TO RETURN-CODE                                             
015400     GOBACK                                                               
015500     .                                                                    
015600     EJECT                                                                
015700 A-INIT SECTION.                                                          
015800     SKIP2                                                                
015900                                                                          
016000     OPEN OUTPUT W418C1                                                   
016100                                                                          
016200                 W418C2                                                   
016300                                                                          
016400                 W418C3                                                   
016500                                                                          
016600                                                                          
016700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016800     MOVE JA    TO FOERSTA-POST-SW                                        
016900     .                                                                    
017000     EJECT                                                                
017100 Z-FINIT SECTION.                                                         
017200                                                                          
017300                                                                          
017400     CLOSE W418C1                                                         
017500                                                                          
017600           W418C2                                                         
017700                                                                          
017800           W418C3                                                         
017900     SKIP2                                                                
018000     MOVE 'S' TO POSTSUM-OPKOD                                            
018100     CALL POSTSUM USING POSTSUM-PARM                                      
018200     .                                                                    
018300     EJECT                                                                
018400 B-BEHANDLA-POSTER SECTION.                                               
018500     MOVE MID-IDFINDOC TO WS-IDFINDOC                                     
018600     IF FOERSTA-POST                                                      
018710       MOVE MID-IDFINDOC  TO SPAR-IDFINDOC                                
018800       PERFORM BA-SKAPA-HUVUD-EKOFIL                                      
018900       PERFORM BB-SKAPA-TKOST-EKOFIL                                      
019000       MOVE NEJ           TO FOERSTA-POST-SW                              
019100     END-IF                                                               
019200                                                                          
019300     IF WS-IDFINDOC NOT = SPAR-IDFINDOC                                   
019410       MOVE MID-IDFINDOC  TO SPAR-IDFINDOC                                
019500       PERFORM BA-SKAPA-HUVUD-EKOFIL                                      
019600       PERFORM BB-SKAPA-TKOST-EKOFIL                                      
019700     END-IF                                                               
019800                                                                          
019900     PERFORM BC-SKAPA-RADER-UPDATE                                        
020000     PERFORM BD-SKAPA-RADER-EKOFIL                                        
020100                                                                          
020200*- 20080902 FIL SKALL FÖRBEREDAS MEN AVVAKTA MED VIDARE ARBETE            
020300*- TILLS VIPS HAR BYGGT RUTINER FÖR ATT KUNNA TA EMOT DATAT.SS/AH.        
020400     PERFORM BE-SKAPA-RADER-VIPS                                          
020500                                                                          
020600     .                                                                    
020700     EJECT                                                                
020800 BA-SKAPA-HUVUD-EKOFIL  SECTION.                                          
020900                                                                          
021000     MOVE 'W4183C00'              TO EKHT-IDPGM                           
021100     MOVE FUNCTION CURRENT-DATE (1:8) TO EKHT-DAREGDAT                    
021200     MOVE FUNCTION CURRENT-DATE (9:8) TO EKHT-TIKLOCK                     
021300     MOVE 1                       TO EKHT-IDSEKVNR                        
021400     MOVE 'W510EKHA'              TO EKHT-IDCPYTXT                        
021500     MOVE MID-KDVAT               TO EKHT-BEVAT                           
021600     MOVE MID-DAFINDOC            TO EKHT-DAVERDAT                        
021700     MOVE ' '                     TO EKHT-FLLSBOK                         
021800     MOVE SPACE                   TO EKHT-IDANALYS                        
021900     MOVE 0                       TO EKHT-IDARTNR                         
022000     MOVE '11'                    TO EKHT-IDDC-SEND                       
022100     MOVE '  '                    TO EKHT-IDDC-REC                        
022200                                                                          
022300     IF MID-IDEXCUST-1 = SPACE                                            
022400       MOVE ZERO                  TO EKHT-IDDISTR                         
022500     ELSE                                                                 
022600       MOVE +0         TO W-ANT                                           
022700       INSPECT MID-IDEXCUST-1 TALLYING W-ANT FOR CHARACTERS               
022800               BEFORE INITIAL ' '                                         
022900       MOVE MID-IDEXCUST-1(1:W-ANT) TO WS-IDDISTR                         
023000       MOVE WS-IDDISTR              TO EKHT-IDDISTR                       
023100     END-IF                                                               
023200                                                                          
023300     MOVE ZERO                    TO EKHT-IDKONTO                         
023400     MOVE SPACE                   TO EKHT-IDKST                           
023500                                                                          
023600     IF MID-IDEXCUST-2 = SPACE                                            
023700       MOVE ZERO                  TO EKHT-IDKUNDNR                        
023800     ELSE                                                                 
023900       MOVE +0         TO W-ANT                                           
024000       INSPECT MID-IDEXCUST-2 TALLYING W-ANT FOR CHARACTERS               
024100               BEFORE INITIAL ' '                                         
024200       MOVE MID-IDEXCUST-2(1:W-ANT) TO WS-IDKUNDNR                        
024300       MOVE WS-IDKUNDNR             TO EKHT-IDKUNDNR                      
024400     END-IF                                                               
024500                                                                          
024600     MOVE ' '                     TO EKHT-IDTRANS                         
024700                                                                          
024800     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
024900     MOVE MID-IDFINDOC         TO CIA-IDARTBET-IN                         
025000     CALL W009CIA USING           CIA-W009CIA                             
025100     MOVE CIA-IDARTBET-UT      TO EKHT-IDVERGL                            
025200                                                                          
025300     MOVE SPACE                   TO EKHT-KDANMORS                        
025400     MOVE '204'                   TO EKHT-KDEKHHT                         
025500     MOVE '204'                   TO EKHT-KDEKSHT                         
025600     MOVE 'SUM'                   TO EKHT-KDEKNIVA                        
025700     MOVE ZERO                    TO EKHT-KDFRAKT                         
025800     MOVE 0                       TO EKHT-KDPRODSL                        
025900     MOVE 0                       TO EKHT-KDPSLLOC                        
026000     MOVE MID-KDVALISO            TO EKHT-KDVALISO                        
026100     MOVE 0                       TO EKHT-KVANTAL                         
026200     MOVE 0                       TO EKHT-PRARTNTO                        
026300     MOVE 0                       TO EKHT-PRARTSJK                        
026400     MOVE 0                       TO EKHT-PRARTSTD                        
026500     MOVE 0                       TO EKHT-PRDIRLON                        
026600     MOVE 0                       TO EKHT-PRDMTRL                         
026700     MOVE 0                       TO EKHT-PRINK                           
026800     MOVE MID-PRKURS              TO EKHT-PRKURS                          
026900     MOVE 0                       TO EKHT-PRLANDCO                        
027000     MOVE 0                       TO EKHT-PROVRPAL                        
027100     MOVE MID-SUBTO-TOT           TO EKHT-SUBEL                           
027200     MOVE MID-SUVAT-BILLIT-TOT    TO EKHT-SUVAT                           
027300     MOVE ZERO                    TO EKHT-DAAVIDAT                        
027400                                     EKHT-IDAVINR                         
027500     MOVE SPACE                   TO EKHT-IDLEVNR                         
027600     MOVE ZERO                    TO EKHT-KDAVVTYP                        
027700                                     EKHT-KDRT                            
027800                                     EKHT-KVANTMOT                        
027900                                     EKHT-KVAVIS                          
028000     MOVE SPACE                   TO EKHT-KDSORT                          
028100     MOVE MID-KDTRADP             TO EKHT-KDTRADP                         
028200     MOVE SPACE                   TO EKHT-FLOVRLEV                        
028300     MOVE ZERO                    TO EKHT-IDORDNR5                        
028400     MOVE ZERO                    TO EKHT-PRHEMTAG                        
028500     MOVE 'W4183C00'              TO EKHT-IDUSER                          
028600     MOVE SPACE                   TO EKHT-FLDCET                          
028610     MOVE SPACE                   TO EKHT-IDKUNDRF                        
028610     MOVE SPACE                   TO EKHT-IDFAKT-EXP                      
028700                                                                          
028800     PERFORM S12-SKRIV-W418C2                                             
028900                                                                          
029000     .                                                                    
029100     EJECT                                                                
029200 BB-SKAPA-TKOST-EKOFIL  SECTION.                                          
029300                                                                          
029400     IF MID-SUVAT-BILLIT-TOT > ZERO                                       
029500       MOVE 'MOMS'                  TO EKHT-KDEKNIVA                      
029600       MOVE MID-SUVAT-BILLIT-TOT    TO EKHT-SUBEL                         
029700                                                                          
029800       MOVE 'W4183C00'              TO EKHT-IDPGM                         
029900       MOVE FUNCTION CURRENT-DATE (1:8) TO EKHT-DAREGDAT                  
030000       MOVE FUNCTION CURRENT-DATE (9:8) TO EKHT-TIKLOCK                   
030100       MOVE 1                       TO EKHT-IDSEKVNR                      
030200       MOVE 'W510EKHA'              TO EKHT-IDCPYTXT                      
030300       MOVE 'W4183C00'              TO EKHT-IDUSER                        
030400       MOVE MID-KDVAT               TO EKHT-BEVAT                         
030500       MOVE MID-DAFINDOC            TO EKHT-DAVERDAT                      
030600                                                                          
030700       MOVE ' '                     TO EKHT-FLLSBOK                       
030800       MOVE SPACE                   TO EKHT-IDANALYS                      
030900       MOVE 0                       TO EKHT-IDARTNR                       
031000       MOVE '11'                    TO EKHT-IDDC-SEND                     
031100       MOVE '  '                    TO EKHT-IDDC-REC                      
031200                                                                          
031300       IF MID-IDEXCUST-1 = SPACE                                          
031400         MOVE ZERO                    TO EKHT-IDDISTR                     
031500       ELSE                                                               
031600         MOVE +0         TO W-ANT                                         
031700         INSPECT MID-IDEXCUST-1 TALLYING W-ANT FOR CHARACTERS             
031800                 BEFORE INITIAL ' '                                       
031900         MOVE MID-IDEXCUST-1(1:W-ANT) TO WS-IDDISTR                       
032000         MOVE WS-IDDISTR              TO EKHT-IDDISTR                     
032100       END-IF                                                             
032200                                                                          
032300       MOVE ZERO                    TO EKHT-IDKONTO                       
032400       MOVE SPACE                   TO EKHT-IDKST                         
032500                                                                          
032600       IF MID-IDEXCUST-2 = SPACE                                          
032700         MOVE ZERO                    TO EKHT-IDKUNDNR                    
032800       ELSE                                                               
032900         MOVE +0         TO W-ANT                                         
033000         INSPECT MID-IDEXCUST-2 TALLYING W-ANT FOR CHARACTERS             
033100                 BEFORE INITIAL ' '                                       
033200         MOVE MID-IDEXCUST-2(1:W-ANT) TO WS-IDKUNDNR                      
033300         MOVE WS-IDKUNDNR             TO EKHT-IDKUNDNR                    
033400       END-IF                                                             
033500                                                                          
033600       MOVE ' '                     TO EKHT-IDTRANS                       
033700                                                                          
033800       MOVE 'VO'                 TO CIA-IDARTPRE-IN                       
033900       MOVE MID-IDFINDOC         TO CIA-IDARTBET-IN                       
034000       CALL W009CIA USING           CIA-W009CIA                           
034100       MOVE CIA-IDARTBET-UT      TO EKHT-IDVERGL                          
034200                                                                          
034300       MOVE SPACE                   TO EKHT-KDANMORS                      
034400       MOVE '204'                   TO EKHT-KDEKHHT                       
034500       MOVE '204'                   TO EKHT-KDEKSHT                       
034600       MOVE 0                       TO EKHT-KDFRAKT                       
034700       MOVE 0                       TO EKHT-KDPRODSL                      
034800       MOVE 0                       TO EKHT-KDPSLLOC                      
034900       MOVE MID-KDVALISO            TO EKHT-KDVALISO                      
035000       MOVE 0                       TO EKHT-KVANTAL                       
035100       MOVE 0                       TO EKHT-PRARTNTO                      
035200       MOVE 0                       TO EKHT-PRARTSJK                      
035300       MOVE 0                       TO EKHT-PRARTSTD                      
035400       MOVE 0                       TO EKHT-PRDIRLON                      
035500       MOVE 0                       TO EKHT-PRDMTRL                       
035600       MOVE 0                       TO EKHT-PRINK                         
035700       MOVE MID-PRKURS              TO EKHT-PRKURS                        
035800       MOVE 0                       TO EKHT-PRLANDCO                      
035900       MOVE 0                       TO EKHT-PROVRPAL                      
036000       MOVE ZERO                    TO EKHT-SUVAT                         
036100                                       EKHT-DAAVIDAT                      
036200                                       EKHT-IDAVINR                       
036300       MOVE SPACE                   TO EKHT-IDLEVNR                       
036400       MOVE ZERO                    TO EKHT-KDAVVTYP                      
036500                                       EKHT-KDRT                          
036600                                       EKHT-KVANTMOT                      
036700                                       EKHT-KVAVIS                        
036800       MOVE SPACE                   TO EKHT-KDSORT                        
036900       MOVE MID-KDTRADP             TO EKHT-KDTRADP                       
037000       MOVE SPACE                   TO EKHT-FLOVRLEV                      
037100       MOVE ZERO                    TO EKHT-IDORDNR5                      
037200       MOVE 'W4183C00'              TO EKHT-IDUSER                        
037300       MOVE ZERO                    TO EKHT-PRHEMTAG                      
037400       MOVE SPACE                   TO EKHT-FLDCET                        
037410       MOVE SPACE                   TO EKHT-IDKUNDRF                      
037410       MOVE SPACE                   TO EKHT-IDFAKT-EXP                    
037500                                                                          
037600                                                                          
037700       PERFORM S12-SKRIV-W418C2                                           
037800     END-IF                                                               
037900     .                                                                    
038000     EJECT                                                                
038100 BC-SKAPA-RADER-UPDATE   SECTION.                                         
038200                                                                          
038300     MOVE MID-IDPARTNR      TO UT1-IDPARTNR                               
038400     MOVE MID-DAREGDAT      TO UT1-DAREGDAT                               
038500     MOVE MID-IDREF         TO UT1-IDREF                                  
038600     MOVE MID-IDEXCUST-1    TO UT1-IDEXCUST-1                             
038700     MOVE MID-IDEXCUST-2    TO UT1-IDEXCUST-2                             
038800     MOVE MID-DAFINDOC      TO UT1-DAFINDOC                               
038900     MOVE MID-IDFINDOC      TO UT1-IDFINDOC                               
039000     MOVE MID-PRARTNTO      TO UT1-PRARTBTO                               
039100     MOVE 'SF'              TO UT1-KDRAPPSTA                              
039200                                                                          
039300     IF MID-BEART = 'HANDL.FEE RETURN-O-RETURN'                           
039400       MOVE 'RR'            TO UT1-KDANMORS                               
039500     END-IF                                                               
039600     IF MID-BEART = 'HANDLING FEE FOR CODE 72 '                           
039700       MOVE '72'            TO UT1-KDANMORS                               
039800     END-IF                                                               
039900     IF MID-BEART = 'HANDLING FEE FOR CODE 98 '                           
040000       MOVE '98'            TO UT1-KDANMORS                               
040100     END-IF                                                               
040200                                                                          
040300     PERFORM S11-SKRIV-W418C1                                             
040400                                                                          
040500     .                                                                    
040600     EJECT                                                                
040700 BD-SKAPA-RADER-EKOFIL  SECTION.                                          
040800                                                                          
040900     MOVE '204'                   TO EKHT-KDEKHHT                         
041000     MOVE '204'                   TO EKHT-KDEKSHT                         
041100     MOVE 'EXP'                   TO EKHT-KDEKNIVA                        
041200     MOVE FUNCTION CURRENT-DATE (1:8) TO EKHT-DAREGDAT                    
041300     MOVE FUNCTION CURRENT-DATE (9:8) TO EKHT-TIKLOCK                     
041400     MOVE 1                       TO EKHT-IDSEKVNR                        
041500     MOVE '11'                    TO EKHT-IDDC-SEND                       
041600     MOVE '  '                    TO EKHT-IDDC-REC                        
041700                                                                          
041800     IF MID-IDEXCUST-1 = SPACE                                            
041900       MOVE ZERO                    TO EKHT-IDDISTR                       
042000     ELSE                                                                 
042100       MOVE +0         TO W-ANT                                           
042200       INSPECT MID-IDEXCUST-1 TALLYING W-ANT FOR CHARACTERS               
042300               BEFORE INITIAL ' '                                         
042400       MOVE MID-IDEXCUST-1(1:W-ANT) TO WS-IDDISTR                         
042500       MOVE WS-IDDISTR              TO EKHT-IDDISTR                       
042600     END-IF                                                               
042700                                                                          
042800     IF MID-IDEXCUST-2 = SPACE                                            
042900       MOVE ZERO                    TO EKHT-IDKUNDNR                      
043000     ELSE                                                                 
043100       MOVE +0         TO W-ANT                                           
043200       INSPECT MID-IDEXCUST-2 TALLYING W-ANT FOR CHARACTERS               
043300               BEFORE INITIAL ' '                                         
043400       MOVE MID-IDEXCUST-2(1:W-ANT) TO WS-IDKUNDNR                        
043500       MOVE WS-IDKUNDNR             TO EKHT-IDKUNDNR                      
043600     END-IF                                                               
043700                                                                          
043800     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
043900     MOVE MID-IDFINDOC         TO CIA-IDARTBET-IN                         
044000     CALL W009CIA USING           CIA-W009CIA                             
044100     MOVE CIA-IDARTBET-UT      TO EKHT-IDVERGL                            
044200                                                                          
044300     MOVE MID-DAFINDOC            TO EKHT-DAVERDAT                        
044400     MOVE 0                       TO EKHT-KDPRODSL                        
044500     MOVE 0                       TO EKHT-KDPSLLOC                        
044600     MOVE 0                       TO EKHT-IDARTNR                         
044700     MOVE ' '                     TO EKHT-FLLSBOK                         
044800     MOVE MID-KDVALISO            TO EKHT-KDVALISO                        
044900     MOVE MID-PRARTNTO            TO EKHT-PRARTNTO                        
045000     MOVE 0                       TO EKHT-PRARTSJK                        
045100     MOVE 0                       TO EKHT-PRARTSTD                        
045200     MOVE 0                       TO EKHT-PRLANDCO                        
045300     MOVE 0                       TO EKHT-PRINK                           
045400     MOVE MID-PRKURS              TO EKHT-PRKURS                          
045500     MOVE 0                       TO EKHT-PRDIRLON                        
045600     MOVE 0                       TO EKHT-PRDMTRL                         
045700     MOVE 0                       TO EKHT-PROVRPAL                        
045800     MOVE 0                       TO EKHT-PRHEMTAG                        
045900     MOVE MID-KVLEVART            TO EKHT-KVANTAL                         
046000     MOVE 0                       TO EKHT-SUBEL                           
046100     MOVE 'W4183C00'              TO EKHT-IDPGM                           
046200     MOVE SPACE                   TO EKHT-IDTRANS                         
046300     MOVE 'W510EKHA'              TO EKHT-IDCPYTXT                        
046400                                                                          
046500     IF MID-BEART = 'HANDL.FEE RETURN-O-RETURN'                           
046600       MOVE 'RR'            TO EKHT-KDANMORS                              
046700     END-IF                                                               
046800     IF MID-BEART = 'HANDLING FEE FOR CODE 72 '                           
046900       MOVE '72'            TO EKHT-KDANMORS                              
047000     END-IF                                                               
047100     IF MID-BEART = 'HANDLING FEE FOR CODE 98 '                           
047200       MOVE '98'            TO EKHT-KDANMORS                              
047300     END-IF                                                               
047400                                                                          
047500     MOVE SPACE                   TO EKHT-IDANALYS                        
047600     MOVE ZERO                    TO EKHT-IDKONTO                         
047800                                     EKHT-KDFRAKT                         
047900                                     EKHT-SUVAT                           
048000                                     EKHT-IDORDNR5                        
048100     MOVE MID-KDVAT               TO EKHT-BEVAT                           
048200     MOVE ZERO                    TO EKHT-DAAVIDAT                        
048300                                     EKHT-IDAVINR                         
048400                                     EKHT-KDAVVTYP                        
048500                                     EKHT-KDRT                            
048600                                     EKHT-KVANTMOT                        
048700                                     EKHT-KVAVIS                          
048800     MOVE SPACE                   TO EKHT-KDSORT                          
048900                                     EKHT-IDLEVNR                         
049000     MOVE SPACE                   TO EKHT-KDTRADP                         
049100                                     EKHT-FLOVRLEV                        
049200                                     EKHT-IDUSER                          
049210                                     EKHT-IDKST                           
049300                                     EKHT-FLDCET                          
049310                                     EKHT-IDKUNDRF                        
049310                                     EKHT-IDFAKT-EXP                      
049400                                                                          
049500     PERFORM S12-SKRIV-W418C2                                             
049600     .                                                                    
049700     EJECT                                                                
049710                                                                          
049800 BE-SKAPA-RADER-VIPS  SECTION.                                            
050000     MOVE '003'                 TO UT3-IDPTYP                             
050100     MOVE MID-IDFINDOC          TO UT3-IDFINDOC                           
050110     MOVE MID-DAFINDOC          TO UT3-DAFINDOC                           
050200     MOVE MID-IDPARTNR          TO UT3-IDPARTNR                           
050210     MOVE MID-IDEXCUST-1        TO UT3-IDDISTR                            
050211     IF MID-IDEXCUST-2(1:1) = SPACE                                       
050212       MOVE SPACE               TO UT3-IDKUNDNR                           
050213     ELSE                                                                 
050214       IF MID-IDEXCUST-2(2:1) = SPACE                                     
050215         MOVE '00000'                TO UT3-IDKUNDNR(1:5)                 
050216         MOVE MID-IDEXCUST-2(1:1)    TO UT3-IDKUNDNR(6:1)                 
050217       ELSE                                                               
050218       IF MID-IDEXCUST-2(3:1) = SPACE                                     
050219         MOVE '0000'                 TO UT3-IDKUNDNR(1:4)                 
050220         MOVE MID-IDEXCUST-2(1:2)    TO UT3-IDKUNDNR(5:2)                 
050221       ELSE                                                               
050222       IF MID-IDEXCUST-2(4:1) = SPACE                                     
050223         MOVE '000'                  TO UT3-IDKUNDNR(1:3)                 
050224         MOVE MID-IDEXCUST-2(1:3)    TO UT3-IDKUNDNR(4:3)                 
050225       ELSE                                                               
050226       IF MID-IDEXCUST-2(5:1) = SPACE                                     
050227         MOVE '00'                   TO UT3-IDKUNDNR(1:2)                 
050228         MOVE MID-IDEXCUST-2(1:4)    TO UT3-IDKUNDNR(3:4)                 
050229       ELSE                                                               
050230       IF MID-IDEXCUST-2(6:1) = SPACE                                     
050231         MOVE '0'                    TO UT3-IDKUNDNR(1:1)                 
050232         MOVE MID-IDEXCUST-2(1:5)    TO UT3-IDKUNDNR(2:5)                 
050233       ELSE                                                               
050240         MOVE MID-IDEXCUST-2        TO UT3-IDKUNDNR                       
050250       END-IF                                                             
050260       END-IF                                                             
050270       END-IF                                                             
050280       END-IF                                                             
050290       END-IF                                                             
050300     END-IF                                                               
050400     IF MID-BEART = 'HANDL.FEE RETURN-O-RETURN'                           
050500       MOVE 'RHF RR DR'         TO UT3-BEART                              
050600     END-IF                                                               
050700     IF MID-BEART = 'HANDLING FEE FOR CODE 72 '                           
050800       MOVE 'RHF 72 DR'         TO UT3-BEART                              
050810     END-IF                                                               
050820     IF MID-BEART = 'HANDLING FEE FOR CODE 98 '                           
050900       MOVE 'RHF 98 DR'         TO UT3-BEART                              
051000     END-IF                                                               
051001**** SO THAT VIPS CAN HANDLE THE DATA WE SEND SOME DATA TWO TIMES         
051010     MOVE MID-IDREF(1:8)        TO UT3-BEART(10:8)                        
051020                                                                          
051100     MOVE MID-KVLEVART          TO UT3-KVLEVART-VIPS                      
051200     MOVE MID-PRARTNTO          TO UT3-PRARTNTO-VIPS                      
051300     MOVE MID-SUNTO             TO UT3-SUNTO-VIPS                         
051310     MOVE MID-SUVAT-BILLIT      TO UT3-SUVAT-VIPS                         
051320     MOVE MID-SUBTO             TO UT3-SUBTO-VIPS                         
051500     MOVE MID-KDVAT             TO UT3-KDVAT                              
051600     MOVE MID-KDVALISO-BET      TO UT3-KDVALISO-LOC                       
051700     MOVE MID-PRKURS-BET        TO UT3-PRKURS                             
051810     MOVE MID-IDREF             TO UT3-IDREF                              
051900                                                                          
052000     PERFORM S13-SKRIV-W418C3                                             
052100     .                                                                    
052200     EJECT                                                                
052300 S01-RECV-OPEN SECTION.                                                   
052400     MOVE 'OPEN' TO RECV-KDFUNC                                           
052500     MOVE 'CARPARTS.PULS.FBINVKRE' TO RECV-ADDISPABS                      
052600                                                                          
052700     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
052800                                   RECV-OPEN-AREA                         
052900********              ...FELHANTERING...                                  
053000     IF RECV-KDRC > 0                                                     
053100       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
053200       STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                     
053300       DELIMITED BY SIZE INTO FELTEXT-STR                                 
053400       DISPLAY FELTEXT                                                    
053500       CALL FELLOG                                                        
053600     END-IF                                                               
053700     .                                                                    
053800     EJECT                                                                
053900 S02-RECV-MESSAGE SECTION.                                                
054000                                                                          
054100     MOVE 'GET' TO RECV-KDFUNC                                            
054200     MOVE LENGTH OF RECV-DATA TO RECV-KVDLEN                              
054300     CALL WZ01RECV USING RECV-CONTROL-AREA                                
054400                         RECV-KVDLEN                                      
054500                         RECV-DATA                                        
054600**FELHANTERING...                                                         
054700     IF RECV-KDRC > 1                                                     
054800       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
054900       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
055000       DELIMITED BY SIZE INTO FELTEXT-STR                                 
055100       DISPLAY FELTEXT                                                    
055200       CALL FELLOG                                                        
055300     END-IF                                                               
055400     .                                                                    
055500     EJECT                                                                
055600 S03-RECV-CLOSE SECTION.                                                  
055700                                                                          
055800     MOVE 'CLOSE' TO RECV-KDFUNC                                          
055900     CALL WZ01RECV USING RECV-CONTROL-AREA                                
056000**FELHANTERING...                                                         
056100     IF RECV-KDRC > 0                                                     
056200      MOVE RECV-KDRC TO KDRC-DISPLAY                                      
056300      STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                     
056400       DELIMITED BY SIZE INTO FELTEXT-STR                                 
056500       DISPLAY FELTEXT                                                    
056600       CALL FELLOG                                                        
056700     END-IF                                                               
056800     .                                                                    
056900     EJECT                                                                
057000 S11-SKRIV-W418C1 SECTION.                                                
057100     SKIP2                                                                
057200     WRITE UT1-POST FROM UT1-AREA                                         
057300                                                                          
057400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
057500     MOVE 'W418C1 ' TO POSTSUM-FDNAMN                                     
057600     MOVE 'W4183CD1' TO POSTSUM-DDNAMN2                                   
057700     CALL POSTSUM USING POSTSUM-PARM                                      
057800     .                                                                    
057900     EJECT                                                                
058000 S12-SKRIV-W418C2 SECTION.                                                
058100     SKIP2                                                                
058200     WRITE UT2-POST FROM UT2-AREA                                         
058300                                                                          
058400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
058500     MOVE 'W418C2 ' TO POSTSUM-FDNAMN                                     
058600     MOVE 'W4183CD2' TO POSTSUM-DDNAMN2                                   
058700     CALL POSTSUM USING POSTSUM-PARM                                      
058800     .                                                                    
058900     EJECT                                                                
059000 S13-SKRIV-W418C3 SECTION.                                                
059100     SKIP2                                                                
059200     WRITE UT3-POST FROM UT3-AREA                                         
059300                                                                          
059400     MOVE UT3-IDPTYP TO POSTSUM-TRANSTYP                                  
059500     MOVE 'W418C3 ' TO POSTSUM-FDNAMN                                     
059600     MOVE 'W4183CD3' TO POSTSUM-DDNAMN2                                   
059700     CALL POSTSUM USING POSTSUM-PARM                                      
059800     .                                                                    
059900     EJECT                                                                
