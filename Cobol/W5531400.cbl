000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5531400.                                                
000300 AUTHOR.         KARL JOHAN HANSSON                                       
000400 DATE-WRITTEN.   93/10/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        BMP                                                              
001000*                                                                         
001100*        LÄSER FIL INNEHÅLLANDE NYTT BESTÄLLNINGSPRIS                     
001200*                                                                         
001300*        FILEN KOMMER FRÅN SYSTEM PV-INKÖP(A317) VIA KONTROLL-            
001400*        PROGRAM W5531200 SOM SÅLLAR BORT FELAKTIGA POSTER.               
001500*                                                                         
001600*        PROGRAMMET SKAPAR EN TRANS PER ARTIKEL PÅ INFILEN OCH            
001700*        SKICKAR TRANSEN TILL KOMMUNIKATIONSDATABAS WLKOMA (WDP8)         
001800*                                                                         
001900*        TRANSEN TAS SEDAN EMOT AV                                        
002000*        - PGM W5011100 OM DET ÄR EN HUVUDLEVERANTÖR                      
002100*                                                                         
002200*        PROGRAMMET TAR CHECKPOINT FÖR VART 99:E RECORD                   
002300*                                                                         
002400*    INDATA :                                                             
002500*                                                                         
002600*        FIL W55312                                                       
002700*                                                                         
002800*    UTDATA :                                                             
002900*                                                                         
003000*        TRANSAKTION          : W5I11101 + WMSGKOM                        
003100*                                                                         
003200*  E-TRACKER 1286763, MÄRKNING AV PRISÄNDRING PÅ ONDEMANDLISTOR           
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900                                                                          
004000*          --- NYA BESTÄLLNINGSPRISER FRÅN PV-INKÖP                       
004100                                                                          
004200     SELECT W55312                     ASSIGN TO W55314D1.                
004300     SKIP3                                                                
004400 DATA DIVISION.                                                           
004500                                                                          
004600 FILE SECTION.                                                            
004700     SKIP2                                                                
004800 FD  W55312                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100     SKIP2                                                                
005200*01  -COPY W55312        -L.                                              
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600 77  IDPGM                       PIC X(8)    VALUE 'W5531400'.            
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
007000 77  W55312-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-W55312                       VALUE 'J'.                   
007200                                                                          
007300 01  WS-TIPRLIST-6               PIC  9(6).                               
007400 01  WS-TIUPPDAT                 PIC S9(7)   COMP-3.                      
007500 01  WS-TIUPPTID                 PIC S9(9)   COMP-3.                      
007600 01  WS-PRARTBEL                 PIC 9(13).                               
007700 01  WS-RETULF                   PIC 9(7).                                
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
008900 01  R25-AREA-START          PIC X(24)   VALUE 'R25-AREA-START'.          
009000     SKIP2                                                                
009100*01  AREA -COPY W55312     -PRE R25-                                      
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
011100*    MID-AREA FÖR W5I11101                                       *        
011200*01  -COPY W5I11101 -PRE 5111-                                            
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
013500     PERFORM S01-LAES-W55312                                              
013600                                                                          
013700     PERFORM UNTIL END-OF-W55312                                          
013800       PERFORM B-SKAPA-TRANS-5111                                         
013900       PERFORM C-SKICKA-TRANS                                             
014000                                                                          
014100       PERFORM S01-LAES-W55312                                            
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
015200     OPEN INPUT W55312                                                    
015300                                                                          
015400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015500                                                                          
015600     PERFORM IMS-RESTART                                                  
015700                                                                          
015800*                          INITIERA WMSGKOM                               
015900                                                                          
016000     MOVE   SPACE                 TO   MSG-KOM-WMSGKOM                    
016100     MOVE   +54                   TO   MSG-KOM-KVLL                       
016200     MOVE   LOW-VALUE             TO   MSG-KOM-KDZ1                       
016300     MOVE   LOW-VALUE             TO   MSG-KOM-KDZ2                       
016400     MOVE   SPACE                 TO   MSG-KOM-KDTRANS                    
016500     MOVE   'R25'                 TO   MSG-KOM-IDSNDNOD                   
016600     MOVE   IDPGM                 TO   MSG-KOM-IDSNDJOB                   
016700     MOVE   'W5I11101'            TO   MSG-KOM-IDCPYTXT                   
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
018000 B-SKAPA-TRANS-5111 SECTION.                                              
018100                                                                          
018200     COMPUTE MSG-KVLL = LENGTH OF 5111-W5I11101 + 17                      
018300     MOVE LOW-VALUE               TO MSG-KDZ1                             
018400     MOVE LOW-VALUE               TO MSG-KDZ2                             
018500     MOVE 'W5T111X'               TO MSG-KDTRANS-1                        
018600     MOVE '5111'                  TO MSG-IDTRANS-1                        
018700     MOVE '1'                     TO MSG-KDMFSFOR-1                       
018800                                                                          
018900     MOVE R25-IDARTNR             TO 5111-IDARTNR-IN                      
019000                                                                          
019100****************** DETTA ÄR INLAGT BARA FÖR ATT FYLLA MIDDEN              
019200     MOVE '+++++'                 TO 5111-IDLEVNR-IN                      
019300     MOVE '+'                     TO 5111-KDPRBEH-IN                      
019400     MOVE '++++++'                TO 5111-REAENDR-IN                      
019500                                                                          
019600     MOVE SPACE                   TO 5111-IDLEVNR-UT                      
019700                                     5111-REAENDR-UT                      
019800     MOVE SPACE                   TO 5111-KDPRBEH-UT                      
019900************************************************************              
020000                                                                          
020100     IF R25-IDUSER = 'W55316  '                                           
020200       MOVE 'S'                   TO 5111-KDPRURSP-U                      
020300     ELSE                                                                 
020400       MOVE 'M'                   TO 5111-KDPRURSP-U                      
020500     END-IF                                                               
020600     MOVE R25-IDUSER              TO 5111-IDUSER                          
020700                                                                          
020800     MOVE R25-TIREGDAT            TO WS-TIPRLIST-6                        
020900     MOVE WS-TIPRLIST-6           TO 5111-TIPRLIST-U                      
021000                                                                          
021100     COMPUTE WS-PRARTBEL = 100000 * R25-PRARTBEL-PR                       
021200     MOVE WS-PRARTBEL(1:8)        TO 5111-PRARTBEL-U(1:8)                 
021300     MOVE '.'                     TO 5111-PRARTBEL-U(9:1)                 
021400     MOVE WS-PRARTBEL(9:5)        TO 5111-PRARTBEL-U(10:5)                
021500                                                                          
021600     COMPUTE WS-RETULF = 10000 * R25-RETULF                               
021700                                                                          
021800     MOVE R25-KDVALISO            TO 5111-KDVALISO-U                      
021900*    MOVE '+++++'                 TO 5111-IDLEVNR-U                       
022000     MOVE R25-IDLEVNR             TO 5111-IDLEVNR-U                       
022100     MOVE R25-KDFPKPRI            TO 5111-KDFPKPRI-U                      
022200                                                                          
022300     MOVE '+'                     TO 5111-FLPRIBES-U                      
022400                                     5111-FLPRIGO-U                       
022500                                                                          
022600     MOVE 5111-W5I11101           TO MSG-INDATA-MINUS-1-TRANSKOD          
022700     .                                                                    
022800     EJECT                                                                
022900 C-SKICKA-TRANS SECTION.                                                  
023000                                                                          
023100     CALL W006KOM USING MSG-PCB                                           
023200                        ALT-PCB                                           
023300                        KOMA-PCB                                          
023400                        MSG-KOM-WMSGKOM                                   
023500                        MSG-IO-AREA                                       
023600                                                                          
023700     MOVE 'W55312'       TO POSTSUM-FDNAMN                                
023800     MOVE MSG-KDTRANS-1  TO POSTSUM-DDNAMN2                               
023900     MOVE 'R25'          TO POSTSUM-TRANSTYP                              
024000     CALL POSTSUM USING POSTSUM-PARM                                      
024100                                                                          
024200     ADD +1 TO W-CHKP-RAEKNARE                                            
024300                                                                          
024400     IF W-CHKP-RAEKNARE > W-CHKP-MAX                                      
024500       PERFORM IMS-CHECKPOINT                                             
024600       MOVE +0 TO W-CHKP-RAEKNARE                                         
024700       ADD  1  TO MSG-KOM-TIKLOCK                                         
024800     END-IF                                                               
024900     .                                                                    
025000                                                                          
025100     EJECT                                                                
025200 Z-FINIT SECTION.                                                         
025300                                                                          
025400     CLOSE W55312                                                         
025500                                                                          
025600     MOVE 'S' TO POSTSUM-OPKOD                                            
025700     CALL POSTSUM USING POSTSUM-PARM                                      
025800     .                                                                    
025900     SKIP3                                                                
026000 S01-LAES-W55312  SECTION.                                                
026100                                                                          
026200     READ W55312 INTO R25-AREA                                            
026300     AT END                                                               
026400        SET END-OF-W55312 TO TRUE                                         
026500                                                                          
026600     NOT AT END                                                           
026700        MOVE 'W55312'       TO POSTSUM-FDNAMN                             
026800        MOVE 'W55314D1'     TO POSTSUM-DDNAMN2                            
026900        MOVE 'R25'          TO POSTSUM-TRANSTYP                           
027000        CALL POSTSUM USING POSTSUM-PARM                                   
027100     END-READ                                                             
027200     .                                                                    
027300     EJECT                                                                
027400* IMS SECTIONER                                                           
027500                                                                          
027600 IMS-RESTART SECTION.                                                     
027700                                                                          
027800     MOVE SPACE TO W-MSG-IO-AREA                                          
027900     MOVE '  ' TO GODK-STATUSKODER                                        
028000     CALL CBLTDLI USING XRST MSG-PCB                                      
028100                             W-MSG-IO-AREA-LENGTH                         
028200                             W-MSG-IO-AREA                                
028300                             W-CHKP-AREA-1-LENGTH                         
028400                             W-CHKP-AREA-1                                
028500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028600     PERFORM IMS-STATUSKONTROLL                                           
028700     .                                                                    
028800     SKIP3                                                                
028900 IMS-CHECKPOINT SECTION.                                                  
029000                                                                          
029100     MOVE IDPGM TO W-MSG-IO-AREA                                          
029200     MOVE '  XD' TO GODK-STATUSKODER                                      
029300     CALL CBLTDLI USING CHKP MSG-PCB                                      
029400                             W-MSG-IO-AREA-LENGTH                         
029500                             W-MSG-IO-AREA                                
029600                             W-CHKP-AREA-1-LENGTH                         
029700                             W-CHKP-AREA-1                                
029800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029900     PERFORM IMS-STATUSKONTROLL                                           
030000     IF IMS-EJ-OK                                                         
030100       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
030200       CALL FELLOG                                                        
030300     END-IF                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 IMS-STATUSKONTROLL SECTION.                                              
030700                                                                          
030800     SET STATUS-IX TO 1                                                   
030900     SEARCH GODK-STATUS                                                   
031000       AT END                                                             
031100         MOVE 'IMS RETURKOD : ' TO FELTEXT-STR                            
031200         DISPLAY FELTEXT STATUS-WS                                        
031300         CALL FELLOG                                                      
031400       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
031500     END-SEARCH                                                           
031600     .                                                                    
