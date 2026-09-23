000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5101400.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL                                          
000400 DATE-WRITTEN.   98/10/30.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERING AV WDH3 MED TRANSAR FRÅN SAP.                        
001000*        WDH3 INNEHÅLLER UPPGIFTER OM GILTIGA KOSTNADSSTÄLLEN,            
001100*        KONTON OCH ANALYSNUMMER.                                         
001200*        TRANSFILEN LÄSES.                                                
001300*        FÖR ALLA TRÄFFAR PÅ RÄTT DATUM, SKER UPPDATERING,                
001400*        NYUPPLÄGG ELLER BORTTAG.                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- UPPDATERINGSTRANSAR                                        
002400     SELECT W51014                     ASSIGN TO W51014D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W51014                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400 01  IN-POST        PIC X(44).                                            
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'W5101400'.            
003900 01  CHKP-VAR.                                                            
004000 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004100 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004200 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004300 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004400 03  CHKP-ANT                    PIC S9(3)   VALUE +0   COMP-3.           
004500 03  CHKP-MAX                    PIC S9(3)   VALUE +500 COMP-3.           
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800     SKIP2                                                                
004910 01  W-DAGENS-DATUM              PIC X(8)    VALUE SPACE.                 
005000 01  W-IDKST                     PIC X(10).                               
005100 01  W-IDKONTO                   PIC S9(11) COMP-3.                       
005200                                                                          
005300 77  W51014-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W51014                       VALUE 'J'.                   
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
006200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006300     EJECT                                                                
006400*    --- PARAMETRAR TILL ABEND                                            
006500                                                                          
006600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL POSTSUM                                          
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007800                                                                          
007900*01  -COPY WDATKORT                                                       
008000     EJECT                                                                
008100 01  IN-AREA-START               PIC X(24)  VALUE 'IN-AREA-START'.        
008200                                                                          
008300 01  IN-AREA.                                                             
008400     03  IN-RECTYP              PIC X(3).                                 
008500     03  IN-KDSEGKEY            PIC X(1).                                 
008600     03  IN-KDTRADP             PIC X(4).                                 
008700     03  IN-KONTO-INFO          PIC X(36).                                
008800     03  IN-M10-AREA REDEFINES IN-KONTO-INFO.                             
008900       05  IN-IDKONTO             PIC X(10).                              
009000       05  IN-FL-COST-CENTER      PIC X(1).                               
009100       05  IN-FL-ANALYS           PIC X(1).                               
009200       05  IN-M10-DAREGDAT        PIC X(8).                               
009300       05  IN-M10-DAUPPDAT        PIC X(8).                               
009400       05  IN-M10-DADELDAT        PIC X(8).                               
009500     03 IN-M11-AREA REDEFINES IN-KONTO-INFO.                              
009600       05  IN-COST-CENTER         PIC X(10).                              
009700       05  IN-M11-DAREGDAT        PIC X(8).                               
009800       05  IN-M11-DADELDAT        PIC X(8).                               
009900     03 IN-M13-AREA REDEFINES IN-KONTO-INFO.                              
010000       05  IN-IDANALYSNR          PIC X(12).                              
010100       05  IN-M13-DAREGDAT        PIC X(8).                               
010200       05  IN-M13-DADELDAT        PIC X(8).                               
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010500     SKIP3                                                                
010600 01  NYCKLAR-TILL-DLI.                                                    
010700     03  W-WDH301KY-X.                                                    
010800         05  W-KDSEGKEY          PIC X        VALUE SPACE.                
010900         05  W-KDTRADP           PIC X(4)     VALUE SPACE.                
011000         05  W-WDH301KY-12       PIC X(12)    VALUE SPACE.                
011100     03  W-WDH301KY-X-2.                                                  
011200         05  W-KDSEGKEY-2        PIC X        VALUE SPACE.                
011300         05  W-KDTRADP-2         PIC X(4)     VALUE SPACE.                
011400         05  W-IDKONTO-2         PIC S9(11)   VALUE ZERO  COMP-3.         
011500         05  W-FILLER-2          PIC X(6)     VALUE SPACE.                
011600     SKIP2                                                                
011700*    --- STATUS-KOD FRÅN IMS                                              
011800 01  STATUS-WS                   PIC XX.                                  
011900     88  SEGMENT-FINNS                       VALUE '  '.                  
012000     88  INSERT-OK                           VALUE '  '.                  
012100     88  REPL-OK                             VALUE '  '.                  
012200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012500     88  IMS-EJ-OK                           VALUE 'XD'.                  
012600     SKIP2                                                                
012700 01  GODK-STATUSKODER.                                                    
012800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012900     SKIP3                                                                
013000 01  SSA1                        PIC X(64).                               
013100 01  SSA2                        PIC X(64).                               
013200     EJECT                                                                
013300*    --- IMS FUNKTIONSKODER                                               
013400*01  -COPY W0003                                                          
013500     EJECT                                                                
013600*    ---  DLI INPUT AREA                                                  
013700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013800     SKIP3                                                                
013900 01  DLI-IO-AREA.                                                         
014000*    03  -COPY WDH301                                                     
014100     EJECT                                                                
014200 LINKAGE SECTION.                                                         
014300                                                                          
014400*01  -COPY W0009   -PRE MSG-                                              
014500     SKIP2                                                                
014600*01  -COPY W0008   -PRE WDH3-                                             
014700     05  FILLER                  PIC X.                                   
014800     EJECT                                                                
014900 PROCEDURE DIVISION  USING MSG-PCB WDH3-PCB.                              
015000 MAIN SECTION.                                                            
015100     ENTRY 'DLITCBL' USING MSG-PCB WDH3-PCB.                              
015200                                                                          
015300                                                                          
015400     PERFORM A-INIT                                                       
015500                                                                          
015600     PERFORM S01-LAES-W51014                                              
015700     PERFORM UNTIL END-OF-W51014                                          
015800                                                                          
015900       EVALUATE IN-RECTYP                                                 
016000         WHEN 'M10'                                                       
016100            PERFORM B-UPPDATERA-M10-IDKONTO                               
016200         WHEN 'M11'                                                       
016300            PERFORM C-UPPDATERA-M11-COST-CENTER                           
016400         WHEN 'M13'                                                       
016500            PERFORM D-UPPDATERA-M13-IDANALYSNR                            
016600       END-EVALUATE                                                       
016700                                                                          
016800       IF CHKP-ANT > CHKP-MAX                                             
016900         PERFORM X-TAG-CHECKPOINT                                         
017000       END-IF                                                             
017100                                                                          
017200       PERFORM S01-LAES-W51014                                            
017300     END-PERFORM                                                          
017400                                                                          
017500     PERFORM Z-FINIT                                                      
017600                                                                          
017700     MOVE ZERO TO RETURN-CODE                                             
017800     GOBACK                                                               
017900     .                                                                    
018000     EJECT                                                                
018100                                                                          
018200 A-INIT SECTION.                                                          
018300                                                                          
018400     PERFORM IMS-RESTART                                                  
018500                                                                          
018600     OPEN INPUT W51014                                                    
018700                                                                          
018800     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
018900     MOVE '20'      TO  W-DAGENS-DATUM(1:2)                               
019000     MOVE D-AAR     TO  W-DAGENS-DATUM(3:2)                               
019100     MOVE D-MAANAD  TO  W-DAGENS-DATUM(5:2)                               
019200     MOVE D-DAG     TO  W-DAGENS-DATUM(7:2)                               
019300                                                                          
019400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019500                                                                          
019600     DISPLAY 'DAGENS DATUM ' W-DAGENS-DATUM                               
019700     .                                                                    
019800     EJECT                                                                
019900 B-UPPDATERA-M10-IDKONTO SECTION.                                         
020000                                                                          
020100     MOVE IN-KDSEGKEY             TO W-KDSEGKEY-2                         
020200     MOVE IN-KDTRADP              TO W-KDTRADP-2                          
020300     MOVE SPACE                   TO W-FILLER-2                           
020400     MOVE IN-IDKONTO(1:6)         TO W-IDKONTO                            
020500     MOVE W-IDKONTO               TO W-IDKONTO-2                          
020600     MOVE W-WDH301KY-X-2          TO W-WDH301KY-X                         
020700                                                                          
020800     IF IN-M10-DADELDAT = W-DAGENS-DATUM                                  
020900       PERFORM IMS-GETU-WDH301                                            
021000       IF SEGMENT-FINNS                                                   
021100         PERFORM IMS-DLET-WDH301                                          
021200         ADD 1 TO CHKP-ANT                                                
021300       END-IF                                                             
021400     ELSE                                                                 
021500       IF IN-M10-DAUPPDAT = W-DAGENS-DATUM                                
021600         PERFORM IMS-GETU-WDH301                                          
021700         IF SEGMENT-FINNS                                                 
021800           MOVE W-IDKONTO         TO SAP-IDKONTO                          
021900           MOVE IN-FL-COST-CENTER TO SAP-FLKST                            
022000           MOVE IN-FL-ANALYS      TO SAP-FLANALYS                         
022100           PERFORM IMS-REPL-WDH301                                        
022200           ADD 1 TO CHKP-ANT                                              
022300         END-IF                                                           
022400       ELSE                                                               
022500         IF IN-M10-DAREGDAT = W-DAGENS-DATUM                              
022600           MOVE SPACE             TO SAP-WDH301                           
022700           MOVE IN-KDSEGKEY       TO SAP-KDSEGKEY                         
022800           MOVE IN-KDTRADP        TO SAP-KDTRADP                          
022900           MOVE W-IDKONTO         TO SAP-IDKONTO                          
023000           MOVE IN-FL-COST-CENTER TO SAP-FLKST                            
023100           MOVE IN-FL-ANALYS      TO SAP-FLANALYS                         
023200           PERFORM IMS-ISRT-WDH301                                        
023300           ADD 1 TO CHKP-ANT                                              
023400         END-IF                                                           
023500       END-IF                                                             
023600     END-IF                                                               
023700     .                                                                    
023800     EJECT                                                                
023900 C-UPPDATERA-M11-COST-CENTER SECTION.                                     
024000                                                                          
024100     MOVE IN-KDSEGKEY             TO W-KDSEGKEY                           
024200     MOVE IN-KDTRADP              TO W-KDTRADP                            
024300*    MOVE IN-COST-CENTER(1:5)     TO W-WDH301KY-12(1:5)                   
024310     MOVE IN-COST-CENTER          TO W-WDH301KY-12(1:10)                  
024400                                                                          
024500     IF IN-M11-DADELDAT =  W-DAGENS-DATUM                                 
024600       PERFORM IMS-GETU-WDH301                                            
024700       IF SEGMENT-FINNS                                                   
024800         PERFORM IMS-DLET-WDH301                                          
024900         ADD 1 TO CHKP-ANT                                                
025000       END-IF                                                             
025100     ELSE                                                                 
025200       IF IN-M11-DAREGDAT = W-DAGENS-DATUM                                
025300         MOVE SPACE               TO SAP-WDH301                           
025400         MOVE IN-KDSEGKEY         TO SAP-KDSEGKEY                         
025500         MOVE IN-KDTRADP          TO SAP-KDTRADP                          
025600         MOVE IN-FL-COST-CENTER   TO SAP-FLKST                            
025700         IF IN-KDTRADP = 'CN05' OR 'IN07'                                 
025800           MOVE IN-COST-CENTER(3:5)   TO W-IDKST                          
025807         ELSE                                                             
025812           MOVE IN-COST-CENTER        TO W-IDKST                          
025814         END-IF                                                           
025815         MOVE W-IDKST                 TO SAP-IDKST                        
025900         PERFORM IMS-ISRT-WDH301                                          
026000         ADD 1 TO CHKP-ANT                                                
026100       END-IF                                                             
026200     END-IF                                                               
026300     .                                                                    
026400     EJECT                                                                
026500 D-UPPDATERA-M13-IDANALYSNR SECTION.                                      
026600                                                                          
026700     MOVE IN-KDSEGKEY             TO W-KDSEGKEY                           
026800     MOVE IN-KDTRADP              TO W-KDTRADP                            
026900     MOVE IN-IDANALYSNR           TO W-WDH301KY-12                        
027000                                                                          
027100     IF IN-M13-DADELDAT = W-DAGENS-DATUM                                  
027200       PERFORM IMS-GETU-WDH301                                            
027300       IF SEGMENT-FINNS                                                   
027400         PERFORM IMS-DLET-WDH301                                          
027500         ADD 1 TO CHKP-ANT                                                
027600       END-IF                                                             
027700     ELSE                                                                 
027800       MOVE SPACE              TO SAP-WDH301                              
027900       MOVE IN-KDSEGKEY        TO SAP-KDSEGKEY                            
028000       MOVE IN-KDTRADP         TO SAP-KDTRADP                             
028100       MOVE IN-IDANALYSNR      TO SAP-IDANALYS                            
028200       IF IN-M13-DAREGDAT = W-DAGENS-DATUM                                
028300         PERFORM IMS-ISRT-WDH301                                          
028400         ADD 1 TO CHKP-ANT                                                
028500       END-IF                                                             
028600     END-IF                                                               
028700     .                                                                    
028800     EJECT                                                                
028900                                                                          
029000 Z-FINIT SECTION.                                                         
029100                                                                          
029200     CLOSE W51014                                                         
029300                                                                          
029400     MOVE 'S' TO POSTSUM-OPKOD                                            
029500     CALL POSTSUM USING POSTSUM-PARM                                      
029600     .                                                                    
029700     EJECT                                                                
029800                                                                          
029900 S01-LAES-W51014  SECTION.                                                
030000                                                                          
030100     READ W51014 INTO IN-AREA                                             
030200     AT END                                                               
030300        SET END-OF-W51014 TO TRUE                                         
030400                                                                          
030500     NOT AT END                                                           
030600        MOVE 'W51014'         TO POSTSUM-FDNAMN                           
030700        MOVE 'W51014D1'       TO POSTSUM-DDNAMN2                          
030800        MOVE 'V'              TO POSTSUM-TRANSTYP(1:1)                    
030900        MOVE IN-RECTYP        TO POSTSUM-TRANSTYP(2:3)                    
031000        CALL POSTSUM USING POSTSUM-PARM                                   
031100     END-READ                                                             
031200     .                                                                    
031300     EJECT                                                                
031400                                                                          
031500 X-TAG-CHECKPOINT   SECTION.                                              
031600                                                                          
031700     PERFORM IMS-CHECKPOINT                                               
031800     MOVE ZERO TO CHKP-ANT                                                
031900     .                                                                    
032000     EJECT                                                                
032100                                                                          
032200* --- IMS SEKTIONER ---                                                   
032300                                                                          
032400 IMS-GETU-WDH301 SECTION.                                                 
032500                                                                          
032600     STRING 'WDH301  (WDH301KY =' W-WDH301KY-X ')'                        
032700          DELIMITED BY SIZE INTO SSA1                                     
032800     MOVE '  GE' TO GODK-STATUSKODER                                      
032900     CALL CBLTDLI USING GHU WDH3-PCB DLI-IO-AREA SSA1                     
033000     MOVE WDH3-STATUS-CODE TO STATUS-WS                                   
033100     PERFORM IMS-STATUSKONTROLL                                           
033200     .                                                                    
033300                                                                          
033400 IMS-ISRT-WDH301 SECTION.                                                 
033500                                                                          
033600     MOVE 'WDH301   ' TO SSA1                                             
033700     MOVE '  II' TO GODK-STATUSKODER                                      
033800     CALL CBLTDLI USING ISRT WDH3-PCB DLI-IO-AREA SSA1                    
033900     MOVE WDH3-STATUS-CODE TO STATUS-WS                                   
034000     PERFORM IMS-STATUSKONTROLL                                           
034100                                                                          
034200     IF INSERT-OK                                                         
034300       MOVE 'ISRT'           TO POSTSUM-FDNAMN                            
034400       MOVE 'WDH3V'          TO POSTSUM-DDNAMN2                           
034500       MOVE IN-RECTYP        TO POSTSUM-TRANSTYP                          
034600       CALL POSTSUM USING POSTSUM-PARM                                    
034700     END-IF                                                               
034800     .                                                                    
034900                                                                          
035000 IMS-REPL-WDH301 SECTION.                                                 
035100                                                                          
035200     MOVE 'WDH301   ' TO SSA1                                             
035300     MOVE '  ' TO GODK-STATUSKODER                                        
035400     CALL CBLTDLI USING REPL WDH3-PCB DLI-IO-AREA SSA1                    
035500     MOVE WDH3-STATUS-CODE TO STATUS-WS                                   
035600     PERFORM IMS-STATUSKONTROLL                                           
035700                                                                          
035800     IF REPL-OK                                                           
035900       MOVE 'REPL'           TO POSTSUM-FDNAMN                            
036000       MOVE 'WDH3V'          TO POSTSUM-DDNAMN2                           
036100       MOVE IN-RECTYP        TO POSTSUM-TRANSTYP                          
036200       CALL POSTSUM USING POSTSUM-PARM                                    
036300     END-IF                                                               
036400     .                                                                    
036500                                                                          
036600 IMS-DLET-WDH301 SECTION.                                                 
036700                                                                          
036800     MOVE '  ' TO GODK-STATUSKODER                                        
036900     CALL CBLTDLI USING DLET WDH3-PCB DLI-IO-AREA                         
037000     MOVE WDH3-STATUS-CODE TO STATUS-WS                                   
037100     PERFORM IMS-STATUSKONTROLL                                           
037200                                                                          
037300     MOVE 'DLET'           TO POSTSUM-FDNAMN                              
037400     MOVE 'WDH3V'          TO POSTSUM-DDNAMN2                             
037500     MOVE IN-RECTYP        TO POSTSUM-TRANSTYP                            
037600     CALL POSTSUM USING POSTSUM-PARM                                      
037700     .                                                                    
037800     EJECT                                                                
037900                                                                          
038000 IMS-RESTART SECTION.                                                     
038100                                                                          
038200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
038300     MOVE '  ' TO GODK-STATUSKODER                                        
038400     CALL CBLTDLI USING XRST MSG-PCB                                      
038500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
038600                        CHKP-AREA-LENGTH CHKP-AREA                        
038700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038800     PERFORM IMS-STATUSKONTROLL                                           
038900     .                                                                    
039000     EJECT                                                                
039100                                                                          
039200 IMS-CHECKPOINT SECTION.                                                  
039300                                                                          
039400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
039500     MOVE '  XD' TO GODK-STATUSKODER                                      
039600     CALL CBLTDLI USING CHKP MSG-PCB                                      
039700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
039800                        CHKP-AREA-LENGTH CHKP-AREA                        
039900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040000     PERFORM IMS-STATUSKONTROLL                                           
040100                                                                          
040200     IF IMS-EJ-OK                                                         
040300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
040400       DISPLAY FELTEXT                                                    
040500       CALL FELLOG                                                        
040600     END-IF                                                               
040700     .                                                                    
040800     EJECT                                                                
040900                                                                          
041000 IMS-STATUSKONTROLL SECTION.                                              
041100     SET STATUS-IX TO 1                                                   
041200     SEARCH GODK-STATUS                                                   
041300       AT END                                                             
041400         MOVE 'S' TO POSTSUM-OPKOD                                        
041500         CALL POSTSUM USING POSTSUM-PARM                                  
041600                                                                          
041700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
041800           DELIMITED BY SIZE INTO FELTEXT                                 
041900         DISPLAY FELTEXT                                                  
042000         CALL FELLOG                                                      
042100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
042200         CONTINUE                                                         
042300     END-SEARCH                                                           
042400     .                                                                    
