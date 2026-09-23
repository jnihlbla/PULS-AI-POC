000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5109100.                                                
000300 AUTHOR.         RANDI BERG.                                              
000400 DATE-WRITTEN.   98/02/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*  FUNKTION:                                                              
000800*    PROGRAMMET RÄTTAR EKONOMISALDON PÅ WDK6 OCH WDK7,                    
000900*    SAMTIDIGT SKER EN UPPDATERING I W555 (SOL).                          
001000*    EKONOMISALDON = KVAKS, KVAKS-PAV, KVEFRS OCH KVLS                    
001010*    ÖVRIGA SALDON = KVBEART (EJ CDC)                                     
001100*                                                                         
001200*  GÖR SÅ HÄR FÖR ATT UPPDATERA WDK6/WDK7:                                
001300*   -SKAPA FIL MED UTSEENDE ENLIGT COPYTEXTEN W51091,                     
001400*    W510.W510B3.W51091.                                                  
001500*    EN POST FÖR VARJE ARTIKEL OCH DC SOM SKALL UPPDATERAS.               
001600*   -FYLL I ARTIKELNUMMER OCH DC SAMT ETT ELLER FLERA AV                  
001700*    DE SALDON SOM SKALL UPPDATERAS.                                      
001800*   -KÖR RUTIN W510B3 VIA SOP                                             
001900*   -SKAPA TOMFIL W510.W510B3.W5101, SÅ ATT NY UPPDATERING EJ             
002000*    UTFÖRS AV MISSTAG.                                                   
002100*                                                                         
002200*    PROGRAMMET UPPDATERAR WLARTC/WLARTS (WDK6 OCH/ELLER WDK7)            
002300*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- INFIL FRÅN EFR-AVSTÄMNING                                  
003200     SELECT INFIL                      ASSIGN TO W51091D1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  INFIL                                                                
003900     LABEL RECORD STANDARD                                                
004000     RECORDING  F                                                         
004100     BLOCK CONTAINS 0.                                                    
004200                                                                          
004300*    -COPY W51091        -L.                                              
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700                                                                          
004800*    -- CHECKED BY WY2000                                                 
004900 77  IDPGM                       PIC X(8)    VALUE 'W5109100'.            
005000 01  CHKP-VAR.                                                            
005100  03 CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005200  03 CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005300  03 CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005400  03 CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005500  03 CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005600  03 CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900                                                                          
006000 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
006100     88  END-OF-INFIL                        VALUE 'J'.                   
006200     EJECT                                                                
006300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006400 01  FILLER REDEFINES DAGENS-DATUM.                                       
006500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006800     EJECT                                                                
006900 01  DATUM-TID-LOGG.                                                      
007000     03  TRANS-TID                   PIC 9(9).                            
007100     03  LOG-DAGENS-DATUM            PIC 9(8).                            
007200     EJECT                                                                
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400*                                                                         
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007900     SKIP2                                                                
008000*    --- PARAMETRAR TILL ABEND                                            
008100                                                                          
008200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008500     SKIP2                                                                
008600 01  FELTEXT.                                                             
008700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008900     EJECT                                                                
009000*    --- VALID IDDC CODES                                                 
009100*                                                                         
009200*01  -COPY WWDC99                                                         
009300*    --- PARAMETRAR TILL POSTSUM                                          
009400*                                                                         
009500*01  -COPY W0005   -PRE  POSTSUM-                                         
009600     EJECT                                                                
009700 01  IN-AREA-START               PIC X(24)   VALUE                        
009800                                 'IN-AREA-START  '.                       
009900 01  IN-AREA.                                                             
010000*    03  -COPY W51091    -PRE IN-                                         
010100     SKIP2                                                                
010200     EJECT                                                                
010300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010400*                                                                         
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010700     SKIP3                                                                
010800 01  NYCKLAR-TILL-DLI.                                                    
010900     03  W-IDARTNR-X.                                                     
011000         05  W-IDARTNR           PIC S9(9)  COMP-3  VALUE ZERO.           
011100     03  W-IDDC-X.                                                        
011200         05  W-IDDC              PIC X(2)           VALUE SPACE.          
011300                                                                          
011400 01  W-KVART-SALDO               PIC S9(7)   COMP-3.                      
011500                                                                          
011600     SKIP2                                                                
011700*    --- STATUS-KOD FRÅN IMS                                              
011800 01  STATUS-WS                   PIC XX.                                  
011900     88  SEGMENT-FINNS                       VALUE '  '.                  
012000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012200     88  IMS-EJ-OK                           VALUE 'XD'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300                                                                          
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500 01  FILLER         PIC X(16) VALUE 'DLI-IO-ARTC01'.                      
013600 01  DLI-IO-ARTC01.                                                       
013700*    03 WLARTC01 -COPY WDK601                                             
013800 01  FILLER         PIC X(16) VALUE 'DLI-IO-ARTC11'.                      
013900 01  DLI-IO-ARTC11.                                                       
014000*    03 WLARTC11 -COPY WDK611                                             
014100 01  FILLER         PIC X(16) VALUE 'DLI-IO-ARTS01'.                      
014200 01  DLI-IO-ARTS01.                                                       
014300*    03 WLARTS01 -COPY WDK701                                             
014400     EJECT                                                                
014500 01  FILLER         PIC X(16) VALUE 'DLI-IO-ARTS11'.                      
014600 01  DLI-IO-ARTS11.                                                       
014700*    03 WLARTS11 -COPY WDK711                                             
014800     EJECT                                                                
014900 01  FILLER         PIC X(16) VALUE 'WLLOGA01'.                           
015000*01  WLLOGA01 -COPY WDL901                                                
015100     EJECT                                                                
015200                                                                          
015300 LINKAGE SECTION.                                                         
015400                                                                          
015500*01  -COPY W0009  -PRE MSG-                                               
015600     EJECT                                                                
015700*01  -COPY W0008  -PRE ARTC-                                              
015800     05  FILLER                  PIC X.                                   
015900     EJECT                                                                
016000*01  -COPY W0008  -PRE ARTS-                                              
016100     05  FILLER                  PIC X.                                   
016200     EJECT                                                                
016300*01  -COPY W0008  -PRE LOGA-                                              
016400     05  FILLER                  PIC X.                                   
016500     EJECT                                                                
016600 PROCEDURE DIVISION  USING MSG-PCB ARTC-PCB ARTS-PCB LOGA-PCB.            
016700 MAIN SECTION.                                                            
016800     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB ARTS-PCB LOGA-PCB.            
016900                                                                          
017000     PERFORM A-INIT                                                       
017100                                                                          
017200     PERFORM S01-LAES-INFIL                                               
017300     PERFORM UNTIL END-OF-INFIL                                           
017400       PERFORM B-UPPDATERA                                                
017500       PERFORM S01-LAES-INFIL                                             
017600     END-PERFORM                                                          
017700                                                                          
017800     PERFORM Z-FINIT                                                      
017900                                                                          
018000     MOVE ZERO TO RETURN-CODE                                             
018100     GOBACK                                                               
018200     .                                                                    
018300     EJECT                                                                
018400 A-INIT SECTION.                                                          
018500                                                                          
018600     PERFORM IMS-RESTART                                                  
018700                                                                          
018800     OPEN INPUT  INFIL                                                    
018900                                                                          
019000     ACCEPT DAGENS-DATUM  FROM DATE                                       
019100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019200     .                                                                    
019300     EJECT                                                                
019400 B-UPPDATERA SECTION.                                                     
019500                                                                          
019600     MOVE IN-IDARTNR           TO W-IDARTNR                               
019700     MOVE IN-IDDC              TO W-IDDC                                  
019800                                  WS-IDDC                                 
019900     IF CDC-SE                                                            
020000      PERFORM IMS-GHU-ARTC11-CLAGSEG                                      
020010      IF SEGMENT-FINNS                                                    
020100       IF IN-KVAKS NOT = ZERO                                             
020200          COMPUTE CLAG-KVAKS-CDC = CLAG-KVAKS-CDC + IN-KVAKS              
020300                                                                          
020400          MOVE SPACE          TO LOGG-WDL901                              
020500          IF IN-KVAKS > +0                                                
020600            MOVE '+'          TO LOGG-IDTECKEN-KVAKS                      
020700            MOVE IN-KVAKS     TO W-KVART-SALDO                            
020800          ELSE                                                            
020900            MOVE '-'          TO LOGG-IDTECKEN-KVAKS                      
021000            COMPUTE W-KVART-SALDO = IN-KVAKS * -1                         
021100          END-IF                                                          
021200          PERFORM BB-SKAPA-LOGG-FRAN-WDK6                                 
021300          PERFORM BC-UPPDATERA-LOGG-WDL9                                  
021400          ADD +2              TO CHKP-ANT                                 
021500       END-IF                                                             
021600       IF IN-KVAKS-PAV NOT = ZERO                                         
021700          COMPUTE CLAG-KVAKS-PAV = CLAG-KVAKS-PAV + IN-KVAKS-PAV          
021800                                                                          
021900          MOVE SPACE          TO LOGG-WDL901                              
022000          IF IN-KVAKS-PAV > +0                                            
022100            MOVE '+'          TO LOGG-IDTECKEN-KVAKS-PAV                  
022200            MOVE IN-KVAKS-PAV TO W-KVART-SALDO                            
022300          ELSE                                                            
022400            MOVE '-'          TO LOGG-IDTECKEN-KVAKS-PAV                  
022500            COMPUTE W-KVART-SALDO = IN-KVAKS-PAV * -1                     
022600          END-IF                                                          
022700          PERFORM BB-SKAPA-LOGG-FRAN-WDK6                                 
022800          PERFORM BC-UPPDATERA-LOGG-WDL9                                  
022900          ADD +2              TO CHKP-ANT                                 
023000                                                                          
023100       END-IF                                                             
023200       IF IN-KVEFRS NOT = ZERO                                            
023300          COMPUTE CLAG-KVEFRS = CLAG-KVEFRS + IN-KVEFRS                   
023400                                                                          
023500          MOVE SPACE          TO LOGG-WDL901                              
023600          IF IN-KVEFRS > +0                                               
023700            MOVE '+'          TO LOGG-IDTECKEN-KVEFRS                     
023800            MOVE IN-KVEFRS    TO W-KVART-SALDO                            
023900          ELSE                                                            
024000            MOVE '-'          TO LOGG-IDTECKEN-KVEFRS                     
024100            COMPUTE W-KVART-SALDO = IN-KVEFRS * -1                        
024200          END-IF                                                          
024300          PERFORM BB-SKAPA-LOGG-FRAN-WDK6                                 
024400          PERFORM BC-UPPDATERA-LOGG-WDL9                                  
024500          ADD +2              TO CHKP-ANT                                 
024600       END-IF                                                             
024700       IF IN-KVLS NOT = ZERO                                              
024800          COMPUTE CLAG-KVLS = CLAG-KVLS + IN-KVLS                         
024900                                                                          
025000          MOVE SPACE          TO LOGG-WDL901                              
025100          IF IN-KVLS > +0                                                 
025200            MOVE '+'          TO LOGG-IDTECKEN-KVLS                       
025300            MOVE IN-KVLS      TO W-KVART-SALDO                            
025400          ELSE                                                            
025500            MOVE '-'          TO LOGG-IDTECKEN-KVLS                       
025600            COMPUTE W-KVART-SALDO = IN-KVLS * -1                          
025700          END-IF                                                          
025800          PERFORM BB-SKAPA-LOGG-FRAN-WDK6                                 
025900          PERFORM BC-UPPDATERA-LOGG-WDL9                                  
026000          ADD +2              TO CHKP-ANT                                 
026100       END-IF                                                             
026200       PERFORM IMS-REPL-ARTC11-CLAGSEG                                    
026210      ELSE                                                                
026220        DISPLAY ' ARTIKEL SAKNAS ' IN-IDARTNR ' ' IN-IDDC                 
026230      END-IF                                                              
026300     ELSE                                                                 
026400      PERFORM IMS-GHU-ARTS11-SLAGSEG                                      
026410      IF SEGMENT-FINNS                                                    
026500       IF IN-KVEFRS NOT = ZERO                                            
026600          COMPUTE SLAG-KVEFRS = SLAG-KVEFRS + IN-KVEFRS                   
026700                                                                          
026800          MOVE SPACE          TO LOGG-WDL901                              
026900          IF IN-KVEFRS > +0                                               
027000            MOVE '+'          TO LOGG-IDTECKEN-KVEFRS                     
027100            MOVE IN-KVEFRS    TO W-KVART-SALDO                            
027200          ELSE                                                            
027300            MOVE '-'          TO LOGG-IDTECKEN-KVEFRS                     
027400            COMPUTE W-KVART-SALDO = IN-KVEFRS * -1                        
027500          END-IF                                                          
027600          PERFORM BA-SKAPA-LOGG-FRAN-WDK7                                 
027700          PERFORM BC-UPPDATERA-LOGG-WDL9                                  
027800          ADD +2              TO CHKP-ANT                                 
027900       END-IF                                                             
028000       IF IN-KVAKS NOT = ZERO                                             
028100          COMPUTE SLAG-KVAKS-SDC = SLAG-KVAKS-SDC + IN-KVAKS              
028200                                                                          
028300          MOVE SPACE          TO LOGG-WDL901                              
028400          IF IN-KVAKS > +0                                                
028500            MOVE '+'          TO LOGG-IDTECKEN-KVAKS                      
028600            MOVE IN-KVAKS     TO W-KVART-SALDO                            
028700          ELSE                                                            
028800            MOVE '-'          TO LOGG-IDTECKEN-KVAKS                      
028900            COMPUTE W-KVART-SALDO = IN-KVAKS * -1                         
029000          END-IF                                                          
029100          PERFORM BA-SKAPA-LOGG-FRAN-WDK7                                 
029200          PERFORM BC-UPPDATERA-LOGG-WDL9                                  
029300          ADD +2              TO CHKP-ANT                                 
029400       END-IF                                                             
029500       IF IN-KVAKS-PAV NOT = ZERO                                         
029600          COMPUTE SLAG-KVAKS-PAV = SLAG-KVAKS-PAV + IN-KVAKS-PAV          
029700                                                                          
029800          MOVE SPACE          TO LOGG-WDL901                              
029900          IF IN-KVAKS-PAV > +0                                            
030000            MOVE '+'          TO LOGG-IDTECKEN-KVAKS-PAV                  
030100            MOVE IN-KVAKS-PAV TO W-KVART-SALDO                            
030200          ELSE                                                            
030300            MOVE '-'          TO LOGG-IDTECKEN-KVAKS-PAV                  
030400            COMPUTE W-KVART-SALDO = IN-KVAKS-PAV * -1                     
030500          END-IF                                                          
030600          PERFORM BA-SKAPA-LOGG-FRAN-WDK7                                 
030700          PERFORM BC-UPPDATERA-LOGG-WDL9                                  
030800          ADD +2              TO CHKP-ANT                                 
030900       END-IF                                                             
031000       IF IN-KVLS NOT = ZERO                                              
031100          COMPUTE SLAG-KVLS = SLAG-KVLS + IN-KVLS                         
031200                                                                          
031300          MOVE SPACE          TO LOGG-WDL901                              
031400          IF IN-KVLS > +0                                                 
031500            MOVE '+'          TO LOGG-IDTECKEN-KVLS                       
031600            MOVE IN-KVLS      TO W-KVART-SALDO                            
031700          ELSE                                                            
031800            MOVE '-'          TO LOGG-IDTECKEN-KVLS                       
031900            COMPUTE W-KVART-SALDO = IN-KVLS * -1                          
032000          END-IF                                                          
032100          PERFORM BA-SKAPA-LOGG-FRAN-WDK7                                 
032200          PERFORM BC-UPPDATERA-LOGG-WDL9                                  
032300          ADD +2              TO CHKP-ANT                                 
032400       END-IF                                                             
032410       IF IN-KVBEART NOT = ZERO                                           
032420          COMPUTE SLAG-KVBEART = SLAG-KVBEART + IN-KVBEART                
032430                                                                          
032431          ADD +1              TO CHKP-ANT                                 
032440       END-IF                                                             
032500       PERFORM IMS-REPL-ARTS11-SLAGSEG                                    
032510      ELSE                                                                
032520        DISPLAY ' ARTIKEL SAKNAS ' IN-IDARTNR ' ' IN-IDDC                 
032530      END-IF                                                              
032700     END-IF                                                               
032800     IF CHKP-ANT > CHKP-MAX                                               
032900        PERFORM X-TAG-CHECKPOINT                                          
033000     END-IF                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 BA-SKAPA-LOGG-FRAN-WDK7 SECTION.                                         
033310                                                                          
033400* LÄGGER UPP LOGG PÅ WDL9                                                 
033500     MOVE W-IDARTNR          TO LOGG-IDARTNR                              
033600     MOVE FUNCTION CURRENT-DATE (1:8) TO LOG-DAGENS-DATUM                 
033700     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - LOG-DAGENS-DATUM           
033800     ACCEPT TRANS-TID FROM TIME                                           
033900     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
034000     MOVE 9                       TO LOGG-IDSEKVNR                        
034100     MOVE W-IDDC                  TO LOGG-IDDC                            
034200     MOVE 'MISC'                  TO LOGG-IDHUVTYP                        
034300     MOVE 'R05'                   TO LOGG-IDSUBTYP                        
034400     MOVE IDPGM                   TO LOGG-IDPGM                           
034500     MOVE SPACE                   TO LOGG-IDTRANS                         
034600     MOVE IDPGM                   TO LOGG-IDUSER                          
034700     MOVE SPACE                   TO LOGG-REF                             
034800     MOVE W-KVART-SALDO           TO LOGG-KVART-SALDO                     
034900     MOVE SLAG-KVAKS-SDC          TO LOGG-KVAKS                           
035000     MOVE SLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                       
035100     MOVE SLAG-KVEFRS             TO LOGG-KVEFRS                          
035200     MOVE SLAG-KVLS               TO LOGG-KVLS                            
035300     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
035400     .                                                                    
035500     EJECT                                                                
035600 BB-SKAPA-LOGG-FRAN-WDK6 SECTION.                                         
035610                                                                          
035700* LÄGGER UPP LOGG PÅ WDL9                                                 
035800     MOVE W-IDARTNR          TO LOGG-IDARTNR                              
035900     MOVE FUNCTION CURRENT-DATE (1:8) TO LOG-DAGENS-DATUM                 
036000     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - LOG-DAGENS-DATUM           
036100     ACCEPT TRANS-TID FROM TIME                                           
036200     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
036300     MOVE 9                       TO LOGG-IDSEKVNR                        
036400     MOVE W-IDDC                  TO LOGG-IDDC                            
036500     MOVE 'MISC'                  TO LOGG-IDHUVTYP                        
036600     MOVE 'R05'                   TO LOGG-IDSUBTYP                        
036700     MOVE IDPGM                   TO LOGG-IDPGM                           
036800     MOVE SPACE                   TO LOGG-IDTRANS                         
036900     MOVE IDPGM                   TO LOGG-IDUSER                          
037000     MOVE SPACE                   TO LOGG-REF                             
037100     MOVE W-KVART-SALDO           TO LOGG-KVART-SALDO                     
037200     MOVE CLAG-KVAKS-CDC          TO LOGG-KVAKS                           
037300     MOVE CLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                       
037400     MOVE CLAG-KVEFRS             TO LOGG-KVEFRS                          
037500     MOVE CLAG-KVLS               TO LOGG-KVLS                            
037600     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
037700     .                                                                    
037800     EJECT                                                                
037900 BC-UPPDATERA-LOGG-WDL9 SECTION.                                          
037910                                                                          
038000     PERFORM IMS-ISRT-WDL901                                              
038100     IF SEGMENT-FINNS-REDAN                                               
038200        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
038300          ADD -1 TO LOGG-IDSEKVNR                                         
038400          PERFORM IMS-ISRT-WDL901                                         
038500        END-PERFORM                                                       
038600     END-IF                                                               
038700     .                                                                    
038800     EJECT                                                                
038900 Z-FINIT SECTION.                                                         
038910                                                                          
039000     CLOSE INFIL                                                          
039100     SKIP2                                                                
039200     MOVE 'S' TO POSTSUM-OPKOD                                            
039300     CALL POSTSUM USING POSTSUM-PARM                                      
039400     .                                                                    
039500     EJECT                                                                
039600 S01-LAES-INFIL   SECTION.                                                
039700     READ INFIL INTO IN-AREA                                              
039800     AT END                                                               
039900        MOVE HIGH-VALUE TO IN-AREA                                        
040000        SET END-OF-INFIL TO TRUE                                          
040100                                                                          
040200     NOT AT END                                                           
040300        MOVE 'INFIL' TO POSTSUM-FDNAMN                                    
040400        MOVE 'W51091RU' TO POSTSUM-DDNAMN2                                
040500        MOVE SPACE TO POSTSUM-TRANSTYP                                    
040600        CALL POSTSUM USING POSTSUM-PARM                                   
040700     END-READ                                                             
040800     .                                                                    
040900     EJECT                                                                
041000* --- IMS SEKTIONER ---                                                   
041100                                                                          
041200 X-TAG-CHECKPOINT   SECTION.                                              
041300                                                                          
041400     PERFORM IMS-CHECKPOINT                                               
041500     MOVE ZERO TO CHKP-ANT                                                
041600     .                                                                    
041700                                                                          
041800 IMS-RESTART SECTION.                                                     
041900                                                                          
042000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
042100     MOVE '  ' TO GODK-STATUSKODER                                        
042200     CALL CBLTDLI USING XRST MSG-PCB                                      
042300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
042400                        CHKP-AREA-LENGTH CHKP-AREA                        
042500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
042600     PERFORM IMS-STATUSKONTROLL                                           
042700     .                                                                    
042800     SKIP3                                                                
042900 IMS-CHECKPOINT SECTION.                                                  
043000                                                                          
043100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
043200     MOVE '  XD' TO GODK-STATUSKODER                                      
043300     CALL CBLTDLI USING CHKP MSG-PCB                                      
043400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
043500                        CHKP-AREA-LENGTH CHKP-AREA                        
043600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043700     PERFORM IMS-STATUSKONTROLL                                           
043800                                                                          
043900     IF IMS-EJ-OK                                                         
044000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
044100       DISPLAY FELTEXT                                                    
044200       CALL FELLOG                                                        
044300     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044600 IMS-GHU-ARTC11-CLAGSEG SECTION.                                          
044700                                                                          
044800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
044900          DELIMITED BY SIZE INTO SSA1                                     
045000     MOVE 'WLARTC11' TO SSA2                                              
045100     MOVE '  GE'       TO GODK-STATUSKODER                                
045200     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2              
045300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
045400     PERFORM IMS-STATUSKONTROLL                                           
045500     .                                                                    
045600     EJECT                                                                
045700 IMS-REPL-ARTC11-CLAGSEG SECTION.                                         
045800                                                                          
045900     MOVE '  '              TO GODK-STATUSKODER                           
046000     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-ARTC11                       
046100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
046200     PERFORM IMS-STATUSKONTROLL                                           
046300     .                                                                    
046400     EJECT                                                                
046500 IMS-GHU-ARTS11-SLAGSEG SECTION.                                          
046600                                                                          
046700     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
046800          DELIMITED BY SIZE INTO SSA1                                     
046900     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
047000          DELIMITED BY SIZE INTO SSA2                                     
047100     MOVE '  GE'       TO GODK-STATUSKODER                                
047200     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-ARTS11 SSA1 SSA2              
047300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
047400     PERFORM IMS-STATUSKONTROLL                                           
047500     .                                                                    
047600     EJECT                                                                
047700 IMS-REPL-ARTS11-SLAGSEG SECTION.                                         
047800                                                                          
047900     MOVE '  '              TO GODK-STATUSKODER                           
048000     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-ARTS11                       
048100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
048200     PERFORM IMS-STATUSKONTROLL                                           
048300     .                                                                    
048400     EJECT                                                                
048500 IMS-ISRT-WDL901 SECTION.                                                 
048600                                                                          
048700     MOVE 'WLLOGA01 ' TO SSA1                                             
048800     MOVE '  II' TO GODK-STATUSKODER                                      
048900     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
049000     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
049100     PERFORM IMS-STATUSKONTROLL                                           
049200     .                                                                    
049300     EJECT                                                                
049400 IMS-STATUSKONTROLL SECTION.                                              
049500                                                                          
049600     SET STATUS-IX TO 1                                                   
049700     SEARCH GODK-STATUS                                                   
049800       AT END                                                             
049900         MOVE 'FEL VID DL1 ANROP' TO FELTEXT-STR                          
050000         DISPLAY FELTEXT                                                  
050100         CALL FELLOG                                                      
050200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
050300     END-SEARCH                                                           
050400     .                                                                    
