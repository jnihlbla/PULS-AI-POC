000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5537400.                                                
000300 AUTHOR.         ANDERS HENRIKSSON                                        
000400 DATE-WRITTEN.   2012-07-24                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000900*        BMP                                                              
001100*        LÄSER FIL INNEHÅLLANDE NYTT BESTÄLLNINGSPRIS                     
001200*                                                                         
001300*        FILEN KOMMER FRÅN SYSTEM PV-INKÖP(A317) VIA KONTROLL-            
001400*        PROGRAM W5537300 SOM SÅLLAR BORT FELAKTIGA POSTER.               
001500*                                                                         
001600*        PROGRAMMET SKAPAR EN TRANS PER ARTIKEL PÅ INFILEN OCH            
001700*        SKICKAR TRANSEN TILL KOMMUNIKATIONSDATABAS WLKOMA (WDP8)         
001800*                                                                         
001900*        TRANSEN TAS SEDAN EMOT AV                                        
002000*        - PGM W5020600 OM DET ÄR EN HUVUDLEVERANTÖR                      
002100*                                                                         
002200*        PROGRAMMET TAR CHECKPOINT FÖR VART 99:E RECORD                   
002300*                                                                         
002400*    INDATA :                                                             
002600*        FIL W55373                                                       
002700*                                                                         
002800*    UTDATA :                                                             
003000*        TRANSAKTION          : W5I20601 + WMSGKOM                        
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003600 INPUT-OUTPUT SECTION.                                                    
003800 FILE-CONTROL.                                                            
004000*          --- NYA BESTÄLLNINGSPRISER FRÅN PV-INKÖP                       
004100                                                                          
004200     SELECT W55373                     ASSIGN TO W55374D1.                
004300     SKIP3                                                                
004400 DATA DIVISION.                                                           
004500                                                                          
004600 FILE SECTION.                                                            
004700     SKIP2                                                                
004800 FD  W55373                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100     SKIP2                                                                
005200*01  -COPY W55373        -L.                                              
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600 77  IDPGM                       PIC X(8)    VALUE 'W5537400'.            
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900 77  W-CHKP-MAX                  PIC S9(3)   VALUE +99   COMP-3.          
006000 77  W-CHKP-RAEKNARE             PIC S9(3)   VALUE +0    COMP-3.          
006100 77  W-MSG-IO-AREA-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
006200 77  W-MSG-IO-AREA               PIC X(32)   VALUE SPACE.                 
006300 77  W-CHKP-AREA-1-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
006400 77  W-CHKP-AREA-1               PIC X(32)   VALUE SPACE.                 
006500                                                                          
006600 01  FELTEXT.                                                             
006700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006900                                                                          
007000 77  W55373-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-W55373                       VALUE 'J'.                   
007200                                                                          
007300 01  WS-TIPRLIST-6               PIC  9(6).                               
007400 01  WS-TIUPPDAT                 PIC S9(7)   COMP-3.                      
007500 01  WS-TIUPPTID                 PIC S9(9)   COMP-3.                      
007600 01  WS-PRARTBEL                 PIC 9(13).                               
007800                                                                          
007900 01  DYNAMISKA-SUBPROGRAM.                                                
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005 -PRE  POSTSUM-                                           
008800     EJECT                                                                
008900 01  W73-AREA-START          PIC X(24)   VALUE 'W73-AREA-START'.          
009000     SKIP2                                                                
009100*01  AREA -COPY W55373     -PRE W73-                                      
009200*                                                                         
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009500     SKIP3                                                                
009600*    --- STATUS-KOD FRÅN IMS                                              
009700 01  STATUS-WS                   PIC XX.                                  
009800     88  IMS-EJ-OK                           VALUE 'XD'.                  
009900                                                                          
010000 01  GODK-STATUSKODER.                                                    
010100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010200     EJECT                                                                
010300*    --- IMS FUNKTIONSKODER                                               
010400*01  -COPY W0003                                                          
010500     EJECT                                                                
010600*    ---  MSG INPUT-OUTPUT AREA                                           
010700 01  FILLER                      PIC X(16)   VALUE 'MSG-IO-AREA'.         
010800                                                                          
010900*01  -COPY WMSGAREA                                                       
011000     EJECT                                                                
011100*    MID-AREA FÖR W5I20601                                       *        
011200*01  -COPY W5I20601 -PRE 5206-                                            
011300                                                                          
011400     EJECT                                                                
011500*    ---  AREA FÖR W006KOM SUBMODUL                                       
011600 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
011700                                                                          
011800 01  KOM-IO-AREA.                                                         
011900*    03  -COPY WMSGKOM                                                    
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200*01  -COPY W0009 -PRE MSG-                                                
012300                                                                          
012400*01  -COPY W0009 -PRE ALT-                                                
012500     EJECT                                                                
012600*01  -COPY W0008 -PRE KOMA-                                               
012700     05  FILLER                  PIC X.                                   
012800                                                                          
012900     EJECT                                                                
013000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB.                      
013100 MAIN SECTION.                                                            
013200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB.                      
013300                                                                          
013400     PERFORM A-INIT                                                       
013500     PERFORM S01-LAES-W55373                                              
013600                                                                          
013700     PERFORM UNTIL END-OF-W55373                                          
013800       PERFORM B-SKAPA-TRANS-5206                                         
013900       PERFORM C-SKICKA-TRANS                                             
014000                                                                          
014100       PERFORM S01-LAES-W55373                                            
014200     END-PERFORM                                                          
014300                                                                          
014400     PERFORM Z-FINIT                                                      
014500                                                                          
014600     MOVE ZERO TO RETURN-CODE                                             
014700     GOBACK                                                               
014800     .                                                                    
014900     EJECT                                                                
015000 A-INIT SECTION.                                                          
015100                                                                          
015200     OPEN INPUT W55373                                                    
015300                                                                          
015400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     PERFORM IMS-RESTART                                                  
015700                                                                          
015800*                          INITIERA WMSGKOM                               
016000     MOVE   SPACE                 TO   MSG-KOM-WMSGKOM                    
016100     MOVE   +54                   TO   MSG-KOM-KVLL                       
016200     MOVE   LOW-VALUE             TO   MSG-KOM-KDZ1                       
016300     MOVE   LOW-VALUE             TO   MSG-KOM-KDZ2                       
016400     MOVE   SPACE                 TO   MSG-KOM-KDTRANS                    
016500     MOVE   'W73'                 TO   MSG-KOM-IDSNDNOD                   
016600     MOVE   IDPGM                 TO   MSG-KOM-IDSNDJOB                   
016700     MOVE   'W5I20601'            TO   MSG-KOM-IDCPYTXT                   
016800                                                                          
016900     ACCEPT WS-TIUPPDAT           FROM DATE                               
017000     ACCEPT WS-TIUPPTID           FROM TIME                               
017100                                                                          
017200     MOVE   WS-TIUPPDAT           TO   MSG-KOM-TIREGDAT                   
017300     MOVE   WS-TIUPPTID           TO   MSG-KOM-TIKLOCK                    
017400                                                                          
017500     MOVE   SPACE                 TO   MSG-KOM-IDMFSMED                   
017600                                       MSG-KOM-KDSVAR                     
017700     .                                                                    
017800                                                                          
017900     EJECT                                                                
018000 B-SKAPA-TRANS-5206 SECTION.                                              
018100                                                                          
018200     COMPUTE MSG-KVLL = LENGTH OF 5206-W5I20601 + 17                      
018300     MOVE LOW-VALUE               TO MSG-KDZ1                             
018400     MOVE LOW-VALUE               TO MSG-KDZ2                             
018500     MOVE 'W5T206X'               TO MSG-KDTRANS-1                        
018600     MOVE '5206'                  TO MSG-IDTRANS-1                        
018700     MOVE '1'                     TO MSG-KDMFSFOR-1                       
018800                                                                          
018900     MOVE W73-IDARTNR             TO 5206-IDARTNR-IN                      
019000                                                                          
019100*******************DETTA ÄR INLAGT BARA FÖR ATT FYLLA MIDDEN              
019210     MOVE '+++++'                 TO 5206-IDLEVNR-IN                      
019300     MOVE '+'                     TO 5206-KDPRBEH-IN                      
019400     MOVE '++++++'                TO 5206-REAENDR-IN                      
019500                                                                          
019510     MOVE '+++++++++'             TO 5206-IDARTNR-UT                      
019600     MOVE SPACE                   TO 5206-IDLEVNR-UT                      
019700                                     5206-REAENDR-UT                      
019800     MOVE SPACE                   TO 5206-KDPRBEH-UT                      
019900************************************************************              
019910     MOVE W73-IDDC                TO 5206-IDDC                            
020000                                                                          
020100     IF W73-IDUSER = 'W55371  '                                           
020200       MOVE 'S'                   TO 5206-KDPRURSP-U                      
020300     ELSE                                                                 
020400       MOVE 'M'                   TO 5206-KDPRURSP-U                      
020500     END-IF                                                               
020600     MOVE W73-IDUSER              TO 5206-IDUSER                          
020700                                                                          
020800     MOVE W73-TIREGDAT            TO WS-TIPRLIST-6                        
020900     MOVE WS-TIPRLIST-6           TO 5206-TIPRLIST-U                      
021000                                                                          
021100     COMPUTE WS-PRARTBEL = 100000 * W73-PRARTBEL-PR                       
021200     MOVE WS-PRARTBEL(1:8)        TO 5206-PRARTBEL-U(1:8)                 
021300     MOVE '.'                     TO 5206-PRARTBEL-U(9:1)                 
021400     MOVE WS-PRARTBEL(9:5)        TO 5206-PRARTBEL-U(10:5)                
021910     MOVE W73-RETULF              TO 5206-RETULF                          
022100     MOVE W73-KDVALISO            TO 5206-KDVALISO-U                      
022300     MOVE W73-IDLEVNR             TO 5206-IDLEVNR-U                       
022310                                     5206-IDLEVNR-IN                      
022400     MOVE W73-KDFPKPRI            TO 5206-KDFPKPRI-U                      
022500                                                                          
022600     MOVE SPACE                   TO 5206-FLPRIBES-U                      
022800                                                                          
022900     MOVE 5206-W5I20601           TO MSG-INDATA-MINUS-1-TRANSKOD          
023000     .                                                                    
023100     EJECT                                                                
023200 C-SKICKA-TRANS SECTION.                                                  
023300                                                                          
023400     CALL W006KOM USING MSG-PCB                                           
023500                        ALT-PCB                                           
023600                        KOMA-PCB                                          
023700                        MSG-KOM-WMSGKOM                                   
023800                        MSG-IO-AREA                                       
023900                                                                          
024000     MOVE 'W55373'       TO POSTSUM-FDNAMN                                
024100     MOVE MSG-KDTRANS-1  TO POSTSUM-DDNAMN2                               
024200     MOVE 'W73'          TO POSTSUM-TRANSTYP                              
024300     CALL POSTSUM USING POSTSUM-PARM                                      
024400                                                                          
024500     ADD +1 TO W-CHKP-RAEKNARE                                            
024600                                                                          
024700     IF W-CHKP-RAEKNARE > W-CHKP-MAX                                      
024800       PERFORM IMS-CHECKPOINT                                             
024900       MOVE +0 TO W-CHKP-RAEKNARE                                         
025000       ADD  1  TO MSG-KOM-TIKLOCK                                         
025100     END-IF                                                               
025200     .                                                                    
025300                                                                          
025400     EJECT                                                                
025500 Z-FINIT SECTION.                                                         
025600                                                                          
025700     CLOSE W55373                                                         
025800                                                                          
025900     MOVE 'S' TO POSTSUM-OPKOD                                            
026000     CALL POSTSUM USING POSTSUM-PARM                                      
026100     .                                                                    
026200     SKIP3                                                                
026300 S01-LAES-W55373  SECTION.                                                
026400                                                                          
026500     READ W55373 INTO W73-AREA                                            
026600     AT END                                                               
026700        SET END-OF-W55373 TO TRUE                                         
026800                                                                          
026900     NOT AT END                                                           
027000        MOVE 'W55373'       TO POSTSUM-FDNAMN                             
027100        MOVE 'W55374D1'     TO POSTSUM-DDNAMN2                            
027200        MOVE 'W73'          TO POSTSUM-TRANSTYP                           
027300        CALL POSTSUM USING POSTSUM-PARM                                   
027400     END-READ                                                             
027500     .                                                                    
027600     EJECT                                                                
027700* IMS SECTIONER                                                           
027800                                                                          
027900 IMS-RESTART SECTION.                                                     
028000                                                                          
028100     MOVE SPACE TO W-MSG-IO-AREA                                          
028200     MOVE '  ' TO GODK-STATUSKODER                                        
028300     CALL CBLTDLI USING XRST MSG-PCB                                      
028400                             W-MSG-IO-AREA-LENGTH                         
028500                             W-MSG-IO-AREA                                
028600                             W-CHKP-AREA-1-LENGTH                         
028700                             W-CHKP-AREA-1                                
028800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028900     PERFORM IMS-STATUSKONTROLL                                           
029000     .                                                                    
029100     SKIP3                                                                
029200 IMS-CHECKPOINT SECTION.                                                  
029300                                                                          
029400     MOVE IDPGM TO W-MSG-IO-AREA                                          
029500     MOVE '  XD' TO GODK-STATUSKODER                                      
029600     CALL CBLTDLI USING CHKP MSG-PCB                                      
029700                             W-MSG-IO-AREA-LENGTH                         
029800                             W-MSG-IO-AREA                                
029900                             W-CHKP-AREA-1-LENGTH                         
030000                             W-CHKP-AREA-1                                
030100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030200     PERFORM IMS-STATUSKONTROLL                                           
030300     IF IMS-EJ-OK                                                         
030400       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
030500       CALL FELLOG                                                        
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 IMS-STATUSKONTROLL SECTION.                                              
031000                                                                          
031100     SET STATUS-IX TO 1                                                   
031200     SEARCH GODK-STATUS                                                   
031300       AT END                                                             
031400         MOVE 'IMS RETURKOD : ' TO FELTEXT-STR                            
031500         DISPLAY FELTEXT STATUS-WS                                        
031600         CALL FELLOG                                                      
031700       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
031800     END-SEARCH                                                           
031900     .                                                                    
