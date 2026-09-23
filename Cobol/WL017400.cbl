000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL017400.                                                
000300 AUTHOR.         GÖRAN KJELLSON   GUIDE.                                  
000400 DATE-WRITTEN.   NOVEMBER 2004                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    NAME:       CARPARTS.LDC.PRINVENTORYDOC2BG                           
000900*    WEB-LDC: WL017400 PROGRAM IS A REPLICA OF W5039200 PROGRAM           
001000*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001100*                                                                         
001200*    FUNCTION:                                                            
001300*        UTSKRIFT AV INVENTERINGSANMODAN                                  
001400*        UTSKRIFT KAN VÄLJAS PÅ FÖLJANDE SÄTT:                            
001500*        OM ANTAL KOMBINERAS MED OMRÅDE LÄSES BASEN FRÅN BÖRJAN.          
001600*        PRIORITERADE ARTIKLAR (PRIORITET = 1) SKRIVS UT I FÖRSTA         
001700*        HAND OBEROENDE AV OMRÅDE.                                        
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: WL0174T                                             
002100*        REQUEST:     WL0174I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    WL01741                                             
002500*                     WL01742                                             
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'WL017400'.            
003800                                                                          
003900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004100 77  KDRC-DISPLAY                PIC Z(5).                                
004200 77  CURR-SECTION                PIC X(16) VALUE 'MAIN'.                  
004300 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
004400                                                                          
004500 77    HEADER-SW                 PIC X       VALUE 'N'.                   
004600   88  HEADER-OK                             VALUE 'J'.                   
004700                                                                          
004800 77    BUFFERT-SW                PIC X.                                   
004900   88  BUFFERT-PRINT                         VALUE 'J'.                   
005000                                                                          
005100 77  YES                         PIC X       VALUE 'J'.                   
005200 77  NOO                         PIC X       VALUE 'N'.                   
005300 77  IX                          PIC S9(9)   VALUE +0.                    
005400 77  INDX                        PIC S9(9)   VALUE +0.                    
005500 77  SALDO-IX                    PIC S9(9)   VALUE +0.                    
005600 77  MAX-IX-7                    PIC S9(9)   VALUE +7.                    
005700 77  MAX-IX-10                   PIC S9(9)   VALUE +10.                   
005800                                                                          
005900 77  WS-IDSKYLT-CHINESE          PIC X(3)  VALUE 'RCN'.                   
006000 77  WS-IDSKYLT-ENGLISH          PIC X(3)  VALUE 'GB '.                   
006100                                                                          
006200 01    W-IDPRTOMG-ALFA           PIC X.                                   
006300 01    WS-IDUSER                 PIC X(8) VALUE SPACE.                    
006400 01    WS-DAGENS-DATUM           PIC 9(8) VALUE ZERO.                     
006500                                                                          
006600 01    WS-IDPRTINV-NUM           PIC 9(6).                                
006700 01    WS-IDPRTINV.                                                       
006800    03 WS-IDPRTOMG               PIC S9     COMP-3.                       
006900    03 WS-IDLOPNR                PIC S9(5)  COMP-3.                       
007000                                                                          
007100 01    WS-IDLOPNR-5              PIC 9(5).                                
007200 01    WS-QTY-SDC                PIC S9(7)   VALUE  +0.                   
007300 01  WS-ANTAL-RADER-EFR          PIC 9(7) VALUE ZERO.                     
007400 01  WS-SDC-ANTAL                PIC S9(9)   VALUE  ZERO COMP-3.          
007500 01  WS-SDC-ANTAL-METOD          PIC S9(9)   VALUE  ZERO COMP-3.          
007600                                                                          
007700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007800 01  FILLER REDEFINES DAGENS-DATUM.                                       
007900     03  DAGENS-AA               PIC 9(2).                                
008000     03  DAGENS-MM               PIC 9(2).                                
008100     03  DAGENS-DD               PIC 9(2).                                
008200                                                                          
008300 01  WS-IDDC-LOCAL.                                                       
008400     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
008500     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
008600     03  FILLER                  PIC X(1)   VALUE SPACE.                  
008700                                                                          
008800 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
008900                                                                          
009000 EJECT.                                                                   
009100                                                                          
009200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009300 01  GENERAL-SUBPROGRAMS.                                                 
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
009700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
010000     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
010100                                                                          
010200*    --- PARAMETERS TO ABEND                                              
010300                                                                          
010400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010700                                                                          
010800 01  MESSAGE-CODES.                                                       
010900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
011000                                                                          
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011300*01  -COPY WZ01SUB                                                        
011400                                                                          
011500 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
011600*01  -COPY WZ01SEND                                                       
011700                                                                          
011800 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
011900*01  -COPY WTRAUTF8                                                       
012000                                                                          
012100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012200 01  REQU-AREA.                                                           
012300*    03  -COPY WZ01REQU                                                   
012400*    03  -COPY WL0174I1                                                   
012500                                                                          
012600*                                                                         
012700*01  -COPY WL01TIDZ                                                       
012800*   SPAR AREA WDH111                                                      
012900 01  FILLER                      PIC X(16)   VALUE 'SPAR-WDH111'.         
013000 01  SPAR-WDH111-AREA.                                                    
013100     03  WDH111.                                                          
013200*        05  -COPY WDH111 -PRE SPAR-                                      
013300                                                                          
013400 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
013500 01  HDR-AREA.                                                            
013600*    03  -COPY WZ01REQU  -PRE HDR-                                        
013700*    03  -COPY WZ04HDR                                                    
013800*                                                                         
013900 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
014000 01  DOC-AREA.                                                            
014100*    03  -COPY WL01741                                                    
014200*                                                                         
014300 01  FILLER                      PIC X(16)   VALUE 'DOC2-AREA'.           
014400 01  DOC2-AREA.                                                           
014500*    03  -COPY WL01742                                                    
014600*                                                                         
014700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014800*                                                                         
014900 01    IMS-WS.                                                            
015000   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
015100     SKIP3                                                                
015200*                        **** STATUS-KOD FRÅN IMS                         
015300   03    STATUS-WS       PIC XX.                                          
015400         88  SEGMENT-FOUND       VALUE '  '.                              
015500         88  SEGMENT-MISSING     VALUE 'GE'.                              
015600         88  SEGMENT-EXISTS      VALUE 'II'.                              
015700         88  INDEX-EXISTS        VALUE 'NI'.                              
015800         88  END-OF-DB           VALUE 'GB'.                              
015900                                                                          
016000   03    GOOD-STATUSCODES.                                                
016100     05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016200                                                                          
016300 01    SSA1                      PIC X(132).                              
016400 01    SSA2                      PIC X(132).                              
016500                                                                          
016600*                            IMS FUNKTIONSKODER                           
016700*01    -COPY W0003                                                        
016800                                                                          
016900 01  FILLER                    PIC X(16) VALUE 'NYC-TILL-DLI'.            
017000 01  NYCKLAR-TILL-DLI.                                                    
017100   03  W-IDARTNR-X.                                                       
017200     05  W-IDARTNR               PIC S9(9)   COMP-3.                      
017300                                                                          
017400   03  W-IDPRODNR-X.                                                      
017500     05  W-IDPRODNR-E601         PIC S9(7)   COMP-3.                      
017600                                                                          
017700   03  W-IDDC                    PIC X(2)    VALUE SPACE.                 
017800                                                                          
017900   03  W-IDSKYLT-X.                                                       
018000     05  W-IDSKYLT               PIC X(3)    VALUE SPACE.                 
018100                                                                          
018200   03  W-ORDSTA-X.                                                        
018300     05  W-ORDSTA                PIC S9      COMP-3 VALUE +4.             
018400                                                                          
018500   03  W-WDG3KEY01-X.                                                     
018600     05  W-IDHTYP                PIC X(4)    VALUE '5101'.                
018700     05  W-FILLER                PIC X(26)   VALUE LOW-VALUE.             
018800                                                                          
018900   03  W-WDH111KY-MIN-X.                                                  
019000     05  W-IDDC-WDH1-MIN      PIC X(2)  VALUE SPACE.                      
019100     05  W-KDINVKAT-WDH1-MIN  PIC S9(3)                  COMP-3.          
019200     05  W-TISEGKEY-WDH1-MIN  PIC S9(9) VALUE ZERO       COMP-3.          
019300      05  W-DAREGDAT-SORT-MIN PIC 9(8) VALUE ZERO.                        
019400   03  W-WDH111KY-MAX-X.                                                  
019500     05  W-IDDC-WDH1-MAX      PIC X(2)  VALUE SPACE.                      
019600     05  W-KDINVKAT-WDH1-MAX  PIC S9(3)                  COMP-3.          
019700     05  W-TISEGKEY-WDH1-MAX  PIC S9(9) VALUE +999999999 COMP-3.          
019800     05  W-DAREGDAT-SORT-MAX PIC 9(8) VALUE 99999999.                     
019900                                                                          
020000     03  W-WDD8B1KY-MIN-X.                                                
020100       05 W-IDDC-D8-MIN        PIC X(2)           VALUE SPACE.            
020200       05 W-IDARTNR-D8-MIN     PIC S9(9)   COMP-3 VALUE ZERO.             
020300       05 FILLER               PIC X(15)   VALUE  LOW-VALUE.              
020400                                                                          
020500     03 W-WDD8B1KY-MAX-X.                                                 
020600       05 W-IDDC-D8-MAX        PIC X(2)           VALUE SPACE.            
020700       05 W-IDARTNR-D8-MAX     PIC S9(9)   COMP-3 VALUE ZERO.             
020800       05 FILLER               PIC X(15)   VALUE  HIGH-VALUE.             
020900                                                                          
021000     03  W-IDDC-B6-X.                                                     
021100         05 W-IDDC-B6            PIC X(2).                                
021200                                                                          
021300     03 W-WDE4C1KY-LOW.                                                   
021400       05 W-IDARTNR-LOW          PIC S9(9)   COMP-3.                      
021500       05 FILLER                 PIC X(7)    VALUE  LOW-VALUE.            
021600     03 W-WDE4C1KY-HIGH.                                                  
021700       05 W-IDARTNR-HIGH         PIC S9(9)   COMP-3.                      
021800       05 FILLER                 PIC X(7)    VALUE  HIGH-VALUE.           
021900     03 W-WDE4KEY-X.                                                      
022000       05 W-IDDISTR-WDE4         PIC S9(5)   COMP-3.                      
022100       05 W-IDKUNDNR             PIC S9(7)   COMP-3.                      
022200       05 W-IDKUNDRF             PIC X(10).                               
022300       05 W-IDPRODNR             PIC S9(7)   COMP-3.                      
022400       05 W-IDPLKLST             PIC S9(3)   COMP-3.                      
022500     03 W-IDPURAD-X.                                                      
022600       05 W-IDPURAD              PIC S9(5)   COMP-3.                      
022700     03  W-KDSEGKEY-X.                                                    
022800         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
022900                                                                          
023000 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-AREA'.            
023100 01    DLI-IO-AREA.                                                       
023200   03    IO-AREA                 PIC X(900)  VALUE SPACE.                 
023300*  03    -COPY WDK601              -RED IO-AREA.                          
023400*  03    -COPY WDK611              -RED IO-AREA.                          
023500*  03    -COPY WDD311 -PRE BEN-    -RED IO-AREA.                          
023600                                                                          
023700 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-AREA4'.           
023800 01    DLI-IO-AREA4.                                                      
023900   03  IO-AREA4                  PIC X(600)  VALUE SPACE.                 
024000*  03  -COPY WDK701               -RED IO-AREA4.                          
024100*  03  -COPY WDK711               -RED IO-AREA4.                          
024200                                                                          
024300 01  FILLER                      PIC X(16)   VALUE 'WDH101-AREA'.         
024400 01  DLI-IO-AREA-WDH101.                                                  
024500     03  WDH101.                                                          
024600*        05  -COPY WDH101                                                 
024700                                                                          
024800 01  FILLER                      PIC X(16)   VALUE 'WDH111-AREA'.         
024900 01  DLI-IO-AREA-WDH111.                                                  
025000     03  WDH111.                                                          
025100*        05  -COPY WDH111                                                 
025200                                                                          
025300 01  FILLER                      PIC X(16)   VALUE 'WDH121-AREA'.         
025400 01  DLI-IO-AREA-WDH121.                                                  
025500     03  WDH121.                                                          
025600*        05  -COPY WDH121                                                 
025700                                                                          
025800 01  FILLER                      PIC X(16)   VALUE 'WDGX5102AREA'.        
025900 01  DLI-IO-AREA-WDGX5102.                                                
026000     03  WDGX5102.                                                        
026100*        05  -COPY WDGX5102                                               
026200                                                                          
026300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
026400 01   DLI-IO-AREA-B601.                                                   
026500*     03  -COPY WDB601                                                    
026600     EJECT                                                                
026700                                                                          
026800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E4C1'.           
026900 01  DLI-IO-E4C1.                                                         
027000*  03  -COPY WDE4C1                                                       
027100     EJECT                                                                
027200                                                                          
027300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E401'.           
027400 01  DLI-IO-E401.                                                         
027500*  03  -COPY WDE401                                                       
027600     EJECT                                                                
027700                                                                          
027800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E411'.           
027900 01  DLI-IO-E411.                                                         
028000*  03  -COPY WDE411                                                       
028100     EJECT                                                                
028200                                                                          
028300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E601'.           
028400 01  DLI-IO-E601.                                                         
028500*  03  -COPY WDE601                                                       
028600     EJECT                                                                
028700                                                                          
028800     EJECT                                                                
028900 01  FILLER               PIC X(16)   VALUE 'WDD8B1 AREA'.                
029000 01   DLI-IO-AREA-D8B1.                                                   
029100*     03  -COPY WDD8B1                                                    
029200     EJECT                                                                
029300 LINKAGE SECTION.                                                         
029400                                                                          
029500*01    -COPY W0009     -PRE MSG-                                          
029600                                                                          
029700 01  DISTRDOC-PCB  PIC X.                                                 
029800                                                                          
029900*01    -COPY W0008     -PRE WDK6-                                         
030000   05  FILLER     PIC X.                                                  
030100*01    -COPY W0008     -PRE WDD8B-                                        
030200   05  FILLER     PIC X.                                                  
030300*01    -COPY W0008     -PRE WDE4C-                                        
030400   05  FILLER     PIC X.                                                  
030500*01    -COPY W0008     -PRE WDE4-                                         
030600   05  FILLER     PIC X.                                                  
030700*01    -COPY W0008     -PRE WDH1-                                         
030800   05  FILLER     PIC X.                                                  
030900*01    -COPY W0008     -PRE WDD3-                                         
031000   05  FILLER     PIC X.                                                  
031100*01  -COPY W0008       -PRE WDK7-.                                        
031200   05  FILLER     PIC X.                                                  
031300*01  -COPY W0008       -PRE WDG3-.                                        
031400   05  FILLER     PIC X.                                                  
031500*01  -COPY W0008       -PRE WDB6-.                                        
031600   05  FILLER     PIC X.                                                  
031700*01  -COPY W0008       -PRE WDE6-.                                        
031800   05  FILLER     PIC X.                                                  
031900     EJECT                                                                
032000 PROCEDURE DIVISION  USING   MSG-PCB DISTRDOC-PCB WDK6-PCB                
032100                  WDD8B-PCB WDE4C-PCB WDE4-PCB  WDH1-PCB                  
032200                   WDD3-PCB WDK7-PCB  WDG3-PCB                            
032300                   WDB6-PCB WDE6-PCB.                                     
032400                                                                          
032500 MAIN SECTION.                                                            
032600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
032700     IF SUB-KDRC = 0                                                      
032800        PERFORM S90-OPEN-DAP-SEND                                         
032900        PERFORM A-INIT                                                    
033000        PERFORM B-BEARBETNING                                             
033100        PERFORM S90-CLOSE-DAP-SEND                                        
033200     END-IF                                                               
033300                                                                          
033400     MOVE ZERO TO RETURN-CODE                                             
033500     GOBACK                                                               
033600     .                                                                    
033700                                                                          
033800 A-INIT SECTION.                                                          
033900     MOVE 'A-INIT          ' TO CURR-SECTION                              
034000                                                                          
034100     MOVE '1'                         TO DOC1-IDAFPRCD                    
034200***  MOVE '2'                         TO DOC2-IDAFPRCD                    
034300     MOVE REQU-IDDC-L174              TO DOC1-IDDC                        
034400     ACCEPT DAGENS-DATUM FROM DATE                                        
034500     ACCEPT DAGENS-TID   FROM TIME                                        
034600******** ADAPT DATE AND TIME FOR TIMEZONES                                
034700         MOVE REQU-IDDC-L174  TO W-IDDC-B6                                
034800         PERFORM IMS-GU-WDB601                                            
034900                                                                          
035000         MOVE '011'                TO MSGI-KDCALL                         
035100         MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                       
035100         MOVE DCS-IDDC             TO MSGI-IDDC                           
035200         MOVE DAGENS-DATUM         TO MSGI-TILOKDAT                       
035300         MOVE DAGENS-TID           TO MSGI-TILOKTID                       
035400         CALL WL01TIDZ USING          MSGI-WL01TIDZ                       
035500           MOVE MSGI-TILOKDAT(1:6) TO DAGENS-DATUM                        
035600           MOVE MSGI-TILOKTID(1:4) TO DAGENS-TID(1:4)                     
035700********                                                                  
035800     MOVE DAGENS-DATUM                TO DOC1-TIUTSKR                     
035900     MOVE DAGENS-TID(1:4)             TO DOC1-TIUTSTID                    
036000     MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-DAGENS-DATUM                  
036100     .                                                                    
036200 B-BEARBETNING SECTION.                                                   
036300     MOVE 'B-BEARBETNING   ' TO CURR-SECTION                              
036400                                                                          
036500     MOVE REQU-IDDC-L174             TO W-IDDC                            
036600                                        W-IDDC-WDH1-MIN                   
036700                                        W-IDDC-WDH1-MAX                   
036800                                                                          
036900     MOVE +1 TO IX                                                        
037000     PERFORM UNTIL IX > MAX-IX-10                                         
037100                OR REQU-ART-PRINT(IX) = ALL '+'                           
037200       PERFORM BA-NOLLA-LISTAN                                            
037300       MOVE REQU-IDARTNR-L174(IX)  TO W-IDARTNR                           
037400                                      W-IDARTNR-LOW                       
037500                                      W-IDARTNR-HIGH                      
037600                                      DOC1-IDARTNR                        
037700                                      W-IDARTNR-D8-MIN                    
037800                                      W-IDARTNR-D8-MAX                    
037900       MOVE REQU-KDINVKAT-L174(IX) TO W-KDINVKAT-WDH1-MIN                 
038000                                      W-KDINVKAT-WDH1-MAX                 
038100       PERFORM BB-HAEMTA-ANM-WDH1                                         
038200       IF SEGMENT-MISSING                                                 
038300          MOVE +99 TO IX                                                  
038400       ELSE                                                               
038500           IF NOT HEADER-OK                                               
038600****          MOVE 5102-IDLOPNR     TO HDR-IDLIST                         
038700              MOVE 001              TO HDR-REQU-IDMSGVER                  
038800              MOVE 'INVENTORY-DOC'  TO HDR-IDOUTTYPE                      
038900              MOVE SPACE            TO HDR-IDOUTREC                       
039000              MOVE REQU-IDDC-L174   TO HDR-IDOUTREC(1:2)                  
039100              MOVE REQU-IDUSER IN REQU-AREA   TO HDR-IDOUTREC(3:8)        
039200              MOVE DAGENS-DATUM                TO HDR-IDLIST              
039300                                                                          
039400              PERFORM S90-PUT-DAP-HEADER                                  
039500              MOVE YES TO HEADER-SW                                       
039600           END-IF                                                         
039700           PERFORM BC-HAEMTA-UPPG-WDK6-WDK7                               
039800           PERFORM BD-HAEMTA-UPPG-WDD8                                    
039900           PERFORM BE-HAEMTA-UPPG-WDD3                                    
040000           PERFORM BF-HAEMTA-WDE4-OVR-DISTR                               
040100           PERFORM S90-PUT-DOC                                            
040200           IF BUFFERT-PRINT                                               
040300              PERFORM BG-SKRIV-EXTRA-BUFFRADER                            
040400           END-IF                                                         
040500                                                                          
040600           ADD +1 TO IX                                                   
040700       END-IF                                                             
040800     END-PERFORM                                                          
040900     .                                                                    
041000 BA-NOLLA-LISTAN SECTION.                                                 
041100     MOVE 'BA-NOLLA-LISTAN ' TO CURR-SECTION                              
041200                                                                          
041300     MOVE ZERO TO  DOC1-IDARTNR                                           
041400                   DOC1-ADLAGOMR                                          
041500                   DOC1-ADGANG                                            
041600                   DOC1-ADPLATS                                           
041700                   DOC1-BUFF-ADLAGOMR                                     
041800                   DOC1-BUFF-ADGANG                                       
041900                   DOC1-BUFF-ADPLATS                                      
042000                   DOC1-BUFF-KVLS                                         
042100                   DOC1-KVLS                                              
042200                   DOC1-KVAKS                                             
042300                   DOC1-KVEFRS                                            
042400                   DOC2-ADLAGOMR                                          
042500                   DOC2-ADGANG                                            
042600                   DOC2-ADPLATS                                           
042700                   DOC2-KVLS                                              
042800                   DOC2-BUFF-ADLAGOMR                                     
042900                   DOC2-BUFF-ADGANG                                       
043000                   DOC2-BUFF-ADPLATS                                      
043100                   DOC2-BUFF-KVLS                                         
043200     MOVE SPACE TO DOC1-BEART                                             
043300                                                                          
043400     MOVE ZERO  TO  WS-QTY-SDC                                            
043500     .                                                                    
043600 BB-HAEMTA-ANM-WDH1 SECTION.                                              
043700     MOVE 'BB-HAEMTA-ANM-WD' TO CURR-SECTION                              
043800                                                                          
043900     PERFORM IMS-01-GHU-WDH101                                            
044000     IF SEGMENT-FOUND                                                     
044100        PERFORM IMS-02-GHNP-WDH111                                        
044200        IF SEGMENT-FOUND                                                  
044300           IF IX = 1                                                      
044400*--- EN DUBBLETT                                                          
044500            IF INV-IDLOPNR > 0                                            
044600              MOVE INV-IDPRTOMG    TO WS-IDPRTINV-NUM(1:1)                
044700                                      WS-IDPRTOMG                         
044800              MOVE INV-IDLOPNR     TO WS-IDLOPNR-5                        
044900                                      WS-IDLOPNR                          
045000                                      WS-IDPRTINV-NUM(2:5)                
045100            ELSE                                                          
045200             IF INV-IDPRTOMG = +0 OR +1 OR +2 OR +3                       
045300               EVALUATE INV-IDPRTOMG                                      
045400                 WHEN +0 MOVE 1    TO WS-IDPRTINV-NUM(1:1)                
045500                                      WS-IDPRTOMG                         
045600                 WHEN +1 MOVE 2    TO WS-IDPRTINV-NUM(1:1)                
045700                                      WS-IDPRTOMG                         
045800                 WHEN +2 MOVE 3    TO WS-IDPRTINV-NUM(1:1)                
045900                                      WS-IDPRTOMG                         
046000                 WHEN +3 MOVE 4    TO WS-IDPRTINV-NUM(1:1)                
046100                                      WS-IDPRTOMG                         
046200               END-EVALUATE                                               
046300****  HÄMTA NÄSTA INV-IDLOPNR FRÅN HÄNDELSEBASEN.                         
046400               PERFORM IMS-03-GHU-WDGX5102                                
046500               IF SEGMENT-MISSING                                         
046600                 PERFORM IMS-04-GU-WDG301                                 
046700                 MOVE +1           TO 5102-KDSEGKEY                       
046800                 MOVE +1           TO 5102-IDLOPNR                        
046900                 MOVE 5102-IDLOPNR TO WS-IDPRTINV-NUM(2:5)                
047000                 PERFORM IMS-05-ISRT-WDGX5102                             
047100               ELSE                                                       
047200                 ADD +1 TO 5102-IDLOPNR                                   
047300                 MOVE 5102-IDLOPNR TO WS-IDLOPNR-5                        
047400                                      WS-IDLOPNR                          
047500                 MOVE WS-IDLOPNR-5 TO WS-IDPRTINV-NUM(2:5)                
047600                 PERFORM IMS-06-REPL-WDGX5102                             
047700               END-IF                                                     
047800             ELSE                                                         
047900               MOVE ZERO           TO WS-IDPRTOMG                         
048000                                      WS-IDLOPNR                          
048100             END-IF                                                       
048200            END-IF                                                        
048300           END-IF                                                         
048400           MOVE WS-IDPRTINV-NUM    TO DOC1-IDPRINTINV                     
048500                                                                          
048600           IF INV-FLINVSKR = 'N'                                          
048700              MOVE 'J'             TO INV-FLINVSKR                        
048800           END-IF                                                         
048900        END-IF                                                            
049000     END-IF                                                               
049100     .                                                                    
049200 BC-HAEMTA-UPPG-WDK6-WDK7 SECTION.                                        
049300     MOVE 'BC-HAEMTA-UPPG-W' TO CURR-SECTION                              
049400     IF DCS-CDC                                                           
049500       PERFORM IMS-09-GET-ART-WDK6                                        
049600       MOVE CLAG-KVLS                        TO DOC1-KVLS                 
049700       MOVE CLAG-KVAKS-CDC                   TO DOC1-KVAKS                
049800       MOVE CLAG-ADLAGOMR                    TO DOC1-ADLAGOMR             
049900       MOVE CLAG-ADGANG                      TO DOC1-ADGANG               
050000       MOVE CLAG-ADPLATS                     TO DOC1-ADPLATS              
050100       MOVE CLAG-KVLS                        TO INV-KVLS-OLD              
050200       MOVE CLAG-KVAKS-CDC                   TO INV-KVAKS-OLD             
050300       MOVE WS-IDLOPNR                       TO INV-IDLOPNR               
050400     ELSE                                                                 
050500       PERFORM IMS-07-GET-ART-WDK7                                        
050600       IF SEGMENT-FOUND                                                   
050700         MOVE SLAG-KVLS                        TO DOC1-KVLS               
050800         MOVE SLAG-KVAKS-SDC                   TO DOC1-KVAKS              
050900         MOVE SLAG-ADLAGOMR                    TO DOC1-ADLAGOMR           
051000         MOVE SLAG-ADGANG                      TO DOC1-ADGANG             
051100         MOVE SLAG-ADPLATS                     TO DOC1-ADPLATS            
051200         MOVE SLAG-KVLS                        TO INV-KVLS-OLD            
051300         MOVE SLAG-KVAKS-SDC                   TO INV-KVAKS-OLD           
051400         MOVE WS-IDLOPNR                       TO INV-IDLOPNR             
051500       END-IF                                                             
051600     END-IF                                                               
051700     IF SEGMENT-FOUND                                                     
051800       IF WS-IDPRTOMG NOT = INV-IDPRTOMG                                  
051900         EVALUATE WS-IDPRTOMG                                             
052000           WHEN 1                                                         
052100*            MOVE MSG-SIGNON-USERID          TO INV-IDUSER-PR1            
052200             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR1          
052300           WHEN 2                                                         
052400*            MOVE MSG-SIGNON-USERID          TO INV-IDUSER-PR2            
052500             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR2          
052600           WHEN 3                                                         
052700*            MOVE MSG-SIGNON-USERID          TO INV-IDUSER-PR3            
052800             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR3          
052900         END-EVALUATE                                                     
053000       END-IF                                                             
053100       MOVE WS-IDPRTOMG                      TO INV-IDPRTOMG              
053200     END-IF                                                               
053300     .                                                                    
053400 BD-HAEMTA-UPPG-WDD8 SECTION.                                             
053500     MOVE 'BD-HAEMTA-UPPG-W' TO CURR-SECTION                              
053600                                                                          
053700     PERFORM BDA-NOLLSTAELL-SDC                                           
053800     MOVE NOO                   TO BUFFERT-SW                             
053900     MOVE REQU-IDDC-L174        TO W-IDDC-D8-MIN                          
054000                                   W-IDDC-D8-MAX                          
054100     MOVE +1                  TO SALDO-IX                                 
054200     PERFORM IMS-GU-WDD8B1                                                
054300     PERFORM UNTIL SEGMENT-MISSING OR SALDO-IX > +1                       
054400                                                                          
054500        MOVE SEQB-ADBUFFOMR  TO DOC1-BUFF-ADLAGOMR                        
054600        MOVE SEQB-ADBUFFGANG TO DOC1-BUFF-ADGANG                          
054700        MOVE SEQB-ADBUFFPL   TO DOC1-BUFF-ADPLATS                         
054800        COMPUTE WS-QTY-SDC = SEQB-KVBUFF-F +                              
054900                             SEQB-KVBUFF-OF                               
055000        MOVE WS-QTY-SDC       TO DOC1-BUFF-KVLS                           
055100                                                                          
055200        ADD +1                TO SALDO-IX                                 
055300        PERFORM IMS-GN-WDD8B1                                             
055400     END-PERFORM                                                          
055500     IF SEGMENT-FOUND                                                     
055600        MOVE YES              TO BUFFERT-SW                               
055700     END-IF                                                               
055800     .                                                                    
055900     EJECT                                                                
056000 BDA-NOLLSTAELL-SDC     SECTION.                                          
056100     MOVE 'BDA-NOLLSTAELL-S' TO CURR-SECTION                              
056200                                                                          
056300     MOVE +0 TO DOC1-BUFF-ADLAGOMR                                        
056400                DOC1-BUFF-ADGANG                                          
056500                DOC1-BUFF-ADPLATS                                         
056600                DOC1-BUFF-KVLS                                            
056700     .                                                                    
056800 BE-HAEMTA-UPPG-WDD3 SECTION.                                             
056900     MOVE 'BP-HAEMTA-UPPG-W' TO CURR-SECTION                              
057000                                                                          
057100     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
057200     IF DCS-UNICODE-IDSKYLT                                               
057300        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
057400     ELSE                                                                 
057500        MOVE '278 '             TO TRAUTF8-KDCP                           
057600     END-IF                                                               
057700     PERFORM IMS-10-GU-BEN-SEQ                                            
057800     IF SEGMENT-FOUND                                                     
057900        MOVE BEN-TEXT-BEART     TO TRAUTF8-TECONV-FROM                    
058000     ELSE                                                                 
058100        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
058200        MOVE '278'              TO TRAUTF8-KDCP                           
058300     END-IF                                                               
058400     IF TRAUTF8-TECONV-FROM = SPACES                                      
058500      MOVE 'GB'  TO W-IDSKYLT                                             
058600      MOVE '278' TO TRAUTF8-KDCP                                          
058700      PERFORM IMS-10-GU-BEN-SEQ                                           
058800      MOVE BEN-TEXT-BEART    TO TRAUTF8-TECONV-FROM                       
058900     END-IF                                                               
059000     MOVE 25                    TO TRAUTF8-KVMAXTL                        
059100     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
059200     MOVE TRAUTF8-TECONV-TO     TO DOC1-BEART                             
059300     .                                                                    
059400 BF-HAEMTA-WDE4-OVR-DISTR SECTION.                                        
059500     MOVE 'BF-HAEMTA-WDE4-O' TO CURR-SECTION                              
059600                                                                          
059700     MOVE +1 TO INDX                                                      
059800     MOVE +0 TO WS-ANTAL-RADER-EFR                                        
059900     MOVE +0 TO WS-SDC-ANTAL                                              
060000     MOVE +0 TO WS-SDC-ANTAL-METOD                                        
060100*9748228 START                                                            
060200*W-IDARTNR-*   SÄTTS I B-BEARBETNING SECTION.                             
060300                                                                          
060400     PERFORM IMS-GU-WDE4C1                                                
060500     PERFORM UNTIL SEGMENT-MISSING OR END-OF-DB                           
060600                OR INDX > MAX-IX-7                                        
060700                                                                          
060800       MOVE SEQC-IDDISTR              TO W-IDDISTR-WDE4                   
060900       MOVE SEQC-IDKUNDNR             TO W-IDKUNDNR                       
061000       MOVE SEQC-IDKUNDRF             TO W-IDKUNDRF                       
061100       MOVE SEQC-IDPRODNR             TO W-IDPRODNR                       
061200       MOVE SEQC-IDPLKLST             TO W-IDPLKLST                       
061300       MOVE SEQC-IDPURAD              TO W-IDPURAD                        
061400                                                                          
061500       PERFORM IMS-GU-WDE4-ROT                                            
061600       IF KORD-IDDC = W-IDDC                                              
061700         IF KORD-IDDISTR NOT = +98                                        
061800           MOVE KORD-IDPRODNR         TO W-IDPRODNR-E601                  
061900                                                                          
062000           PERFORM IMS-GNP-WDE411                                         
062100           IF (ORAD-FLDIRLEV NOT = 'J')                                   
062200                                                                          
062300             PERFORM IMS-GU-WDE601                                        
062400                                                                          
062500             IF VORD-KDMETOD = +3                                         
062600               IF ORAD-KVLEVART NOT = ORAD-KVAVBART                       
062700                 COMPUTE WS-SDC-ANTAL-METOD =                             
062800                         WS-SDC-ANTAL-METOD +                             
062900                   (ORAD-KVAVBART - ORAD-KVLEVART)                        
063000                 END-COMPUTE                                              
063100                 ADD  WS-SDC-ANTAL-METOD  TO WS-SDC-ANTAL                 
063200                 MOVE ZERO                TO WS-SDC-ANTAL-METOD           
063300               ELSE                                                       
063400                 ADD  ORAD-KVAVBART       TO WS-SDC-ANTAL                 
063500               END-IF                                                     
063600             ELSE                                                         
063700               COMPUTE WS-SDC-ANTAL = WS-SDC-ANTAL +                      
063800                 (ORAD-KVAVBART - ORAD-KVLEVART)                          
063900               END-COMPUTE                                                
064000             END-IF                                                       
064100             ADD +1 TO INDX                                               
064200             ADD +1 TO WS-ANTAL-RADER-EFR                                 
064300           END-IF                                                         
064400         END-IF                                                           
064500       END-IF                                                             
064600       PERFORM IMS-GN-WDE4C1                                              
064700     END-PERFORM                                                          
064800*9748228 END                                                              
064900* NU HAR VI ALLA SALDO, GÖR REPLACE PÅ WDH1 MED AKTUELLA VÄRDEN           
065000* OM EFR SKALL DELAS MED 2 LÄGG TILL EN RÄKNARE I SLINGAN OVAN            
065100* EFR SKALL INTE RÄKNAS OM ENL SUSSI 060703                               
065200*    IF WS-ANTAL-RADER-EFR < 2                                            
065300       MOVE WS-SDC-ANTAL                    TO INV-KVEFRS-OLD             
065400       MOVE WS-SDC-ANTAL                    TO DOC1-KVEFRS                
065500*    ELSE                                                                 
065600*      COMPUTE INV-KVEFRS-OLD = WS-SDC-ANTAL / 2                          
065700*      COMPUTE DOC1-KVEFRS    = WS-SDC-ANTAL / 2                          
065800*    END-IF                                                               
065900                                                                          
066000**** TA BORT POSTEN PÅ WDH1 OCH SKAPA SEDAN ETT NYTT MED                  
066100**** DAGENS DATUM I DAREGDAT-SORT OM DET ÄR FÖRSTA PRINTNINGEN            
066200     IF WS-IDPRTOMG = 1                                                   
066300       PERFORM IMS-GNP-WDH121                                             
066400       IF SEGMENT-FOUND                                                   
066500         IF INVL-KDSEGKEY = 0                                             
066600           MOVE INVL-IDUSER     TO WS-IDUSER                              
066700         END-IF                                                           
066800       END-IF                                                             
066900       PERFORM IMS-01-GHU-WDH101                                          
067000       MOVE DLI-IO-AREA-WDH111 TO SPAR-WDH111-AREA                        
067100       PERFORM IMS-02-GHNP-WDH111                                         
067200                                                                          
067300       PERFORM IMS-DELETE-WDH111                                          
067400       MOVE WS-DAGENS-DATUM TO SPAR-INV-DAREGDAT-SORT                     
067500                                                                          
067600       PERFORM IMS-01-GHU-WDH101                                          
067700       MOVE SPAR-WDH111-AREA TO DLI-IO-AREA-WDH111                        
067800       PERFORM IMS-INSERT-WDH111                                          
067900                                                                          
068000       IF SEGMENT-EXISTS                                                  
068100         PERFORM UNTIL SEGMENT-FOUND                                      
068200           IF SEGMENT-EXISTS OR INDEX-EXISTS                              
068300            ADD 1                TO INV-TISEGKEY                          
068400            PERFORM IMS-INSERT-WDH111                                     
068500           END-IF                                                         
068600         END-PERFORM                                                      
068700       END-IF                                                             
068800                                                                          
068900       MOVE '0'            TO INVL-KDSEGKEY                               
069000       MOVE WS-IDUSER      TO INVL-IDUSER                                 
069100       PERFORM IMS-INSERT-WDH121                                          
069200       MOVE '1'            TO INVL-KDSEGKEY                               
069300       MOVE REQU-IDUSER    TO INVL-IDUSER                                 
069400       PERFORM IMS-INSERT-WDH121                                          
069500       MOVE '2'            TO INVL-KDSEGKEY                               
069600       MOVE SPACE          TO INVL-IDUSER                                 
069700       PERFORM IMS-INSERT-WDH121                                          
069800       MOVE '3'            TO INVL-KDSEGKEY                               
069900       MOVE SPACE          TO INVL-IDUSER                                 
070000       PERFORM IMS-INSERT-WDH121                                          
070100     ELSE                                                                 
070200       PERFORM IMS-13-REPLACE                                             
070300       MOVE ZERO TO WS-SDC-ANTAL                                          
070400******* REPLACE PÅ WDH121 ****                                            
070500       MOVE WS-IDPRTOMG TO W-IDPRTOMG-ALFA                                
070600       PERFORM IMS-GNP-WDH121                                             
070700       IF SEGMENT-FOUND                                                   
070800         PERFORM UNTIL SEGMENT-MISSING                                    
070900           IF W-IDPRTOMG-ALFA =         INVL-KDSEGKEY                     
071000             MOVE REQU-IDUSER        TO INVL-IDUSER                       
071100             PERFORM IMS-REPLACE-WDH121                                   
071200           END-IF                                                         
071300           PERFORM IMS-GNP-WDH121                                         
071400         END-PERFORM                                                      
071500       END-IF                                                             
071600     END-IF                                                               
071700                                                                          
071800     .                                                                    
071900 BG-SKRIV-EXTRA-BUFFRADER SECTION.                                        
072000     MOVE 'BG-SKRIV-EXTRA-B' TO CURR-SECTION                              
072100     MOVE '2'  TO DOC2-IDAFPRCD                                           
072200     MOVE ZERO TO DOC2-ADLAGOMR                                           
072300                  DOC2-ADGANG                                             
072400                  DOC2-ADPLATS                                            
072500                  DOC2-KVLS                                               
072600                                                                          
072700     MOVE +1 TO SALDO-IX                                                  
072800     MOVE SPACE TO STATUS-WS                                              
072900     PERFORM UNTIL SEGMENT-MISSING                                        
073000                                                                          
073100       MOVE SEQB-ADBUFFOMR  TO DOC2-BUFF-ADLAGOMR                         
073200       MOVE SEQB-ADBUFFGANG TO DOC2-BUFF-ADGANG                           
073300       MOVE SEQB-ADBUFFPL   TO DOC2-BUFF-ADPLATS                          
073400       COMPUTE WS-QTY-SDC = SEQB-KVBUFF-F +                               
073500                            SEQB-KVBUFF-OF                                
073600       MOVE WS-QTY-SDC             TO DOC2-BUFF-KVLS                      
073700                                                                          
073800       PERFORM S91-PUT-DOC2                                               
073900       ADD +1 TO SALDO-IX                                                 
074000       PERFORM IMS-GN-WDD8B1                                              
074100     END-PERFORM                                                          
074200     .                                                                    
074300*    --- DISPATCHER SECTIONS                                              
074400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
074500                                                                          
074600     MOVE 'GETARG'               TO SUB-KDFUNC                            
074700     MOVE 'CARPARTS.LDC.PRINVENTORYDOC2BG' TO SUB-ADDISPABS               
074800     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
074900                                                                          
075000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
075100                                                                          
075200     IF SUB-KDRC > 0                                                      
075300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
075400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
075500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
075600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
075700     END-IF                                                               
075800     .                                                                    
075900     SKIP3                                                                
076000 S90-OPEN-DAP-SEND SECTION.                                               
076100     MOVE 'S90-OPEN-DAP-S' TO CURR-SECTION                                
076200                                                                          
076300     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
076400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
076500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
076600                         SEND-OPEN-AREA                                   
076700     IF SEND-KDRC > ZERO                                                  
076800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
076900       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
077000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
077100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
077200     END-IF                                                               
077300     .                                                                    
077400                                                                          
077500 S90-CLOSE-DAP-SEND SECTION.                                              
077600     MOVE 'S90-CLOSE-DAP-' TO CURR-SECTION                                
077700                                                                          
077800     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
077900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
078000                                                                          
078100     IF SEND-KDRC > 0                                                     
078200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
078300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
078400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
078500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
078600     END-IF                                                               
078700     .                                                                    
078800                                                                          
078900 S90-PUT-DAP-HEADER SECTION.                                              
079000     MOVE 'S90-PUT-DAP-HE' TO CURR-SECTION                                
079100                                                                          
079200     MOVE 'PUT'                           TO SEND-KDFUNC                  
079300     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
079400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
079500                         SEND-KVDLEN                                      
079600                         HDR-AREA                                         
079700     IF SEND-KDRC > ZERO                                                  
079800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
079900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
080000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
080100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
080200     END-IF                                                               
080300     .                                                                    
080400 S90-PUT-DOC      SECTION.                                                
080500     MOVE 'S90-PUT-DOC ' TO CURR-SECTION                                  
080600                                                                          
080700     MOVE 'PUT'                           TO SEND-KDFUNC                  
080800     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
080900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
081000                         SEND-KVDLEN                                      
081100                         DOC-AREA                                         
081200     IF SEND-KDRC > ZERO                                                  
081300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
081400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
081500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
081600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
081700     END-IF                                                               
081800     .                                                                    
081900 S91-PUT-DOC2     SECTION.                                                
082000     MOVE 'S91-PUT-DOC2' TO CURR-SECTION                                  
082100                                                                          
082200     MOVE 'PUT'                           TO SEND-KDFUNC                  
082300     MOVE LENGTH OF DOC2-AREA              TO SEND-KVDLEN                 
082400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
082500                         SEND-KVDLEN                                      
082600                         DOC2-AREA                                        
082700     IF SEND-KDRC > ZERO                                                  
082800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
082900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
083000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
083100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
083200     END-IF                                                               
083300     .                                                                    
083400 IMS-01-GHU-WDH101 SECTION.                                               
083500     MOVE 'IMS-01' TO CURR-IMS-SECTION                                    
083600                                                                          
083700     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
083800             DELIMITED BY SIZE INTO SSA1                                  
083900     MOVE '  GE'                 TO GOOD-STATUSCODES                      
084000     CALL CBLTDLI USING GU WDH1-PCB DLI-IO-AREA-WDH101 SSA1               
084100     MOVE WDH1-STATUS-CODE        TO STATUS-WS                            
084200     PERFORM IMS-STATUS-CHECK                                             
084300     .                                                                    
084400 IMS-02-GHNP-WDH111 SECTION.                                              
084500     MOVE 'IMS-02' TO CURR-IMS-SECTION                                    
084600                                                                          
084700     STRING 'WDH111  (WDH111KY>=' W-WDH111KY-MIN-X                        
084800                    '&WDH111KY<=' W-WDH111KY-MAX-X                        
084900                    '&FLINVBEH =' NOO ')'                                 
085000             DELIMITED BY SIZE INTO SSA1                                  
085100     MOVE '  GE'                 TO GOOD-STATUSCODES                      
085200     CALL CBLTDLI USING GHNP WDH1-PCB DLI-IO-AREA-WDH111 SSA1             
085300     MOVE WDH1-STATUS-CODE        TO STATUS-WS                            
085400     PERFORM IMS-STATUS-CHECK                                             
085500     .                                                                    
085600 IMS-DELETE-WDH111 SECTION.                                               
085700     MOVE 'IMS-DELETE-WDH111     ' TO CURR-IMS-SECTION                    
085800     MOVE 'WDH111 ' TO SSA1                                               
085900     MOVE '  ' TO GOOD-STATUSCODES                                        
086000     CALL CBLTDLI USING DLET WDH1-PCB DLI-IO-AREA-WDH111                  
086100     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
086200     PERFORM IMS-STATUS-CHECK                                             
086300     .                                                                    
086400     SKIP2                                                                
086500 IMS-INSERT-WDH111 SECTION.                                               
086600                                                                          
086700*    STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
086800*         DELIMITED BY SIZE INTO SSA1                                     
086900     MOVE 'WDH111 ' TO SSA1                                               
087000     MOVE '  IINI' TO GOOD-STATUSCODES                                    
087100     CALL CBLTDLI USING ISRT WDH1-PCB DLI-IO-AREA-WDH111 SSA1             
087200     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
087300     PERFORM IMS-STATUS-CHECK                                             
087400     .                                                                    
087500     SKIP3                                                                
087600 IMS-03-GHU-WDGX5102 SECTION.                                             
087700     MOVE 'IMS-03' TO CURR-IMS-SECTION                                    
087800                                                                          
087900     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY01-X ')'                       
088000            DELIMITED BY SIZE INTO SSA1                                   
088100     STRING 'WDGX5102 '                                                   
088200            DELIMITED BY SIZE INTO SSA2                                   
088300     MOVE '  GE'                TO GOOD-STATUSCODES                       
088400     CALL CBLTDLI USING GHU WDG3-PCB DLI-IO-AREA-WDGX5102                 
088500                                               SSA1 SSA2                  
088600     MOVE WDG3-STATUS-CODE      TO STATUS-WS                              
088700     PERFORM IMS-STATUS-CHECK                                             
088800     .                                                                    
088900 IMS-04-GU-WDG301 SECTION.                                                
089000     MOVE 'IMS-04' TO CURR-IMS-SECTION                                    
089100                                                                          
089200     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY01-X ')'                       
089300            DELIMITED BY SIZE INTO SSA1                                   
089400     MOVE '  '                  TO GOOD-STATUSCODES                       
089500     CALL CBLTDLI USING GU WDG3-PCB DLI-IO-AREA-WDGX5102 SSA1             
089600     MOVE WDG3-STATUS-CODE      TO STATUS-WS                              
089700     PERFORM IMS-STATUS-CHECK                                             
089800     .                                                                    
089900 IMS-05-ISRT-WDGX5102 SECTION.                                            
090000     MOVE 'IMS-05' TO CURR-IMS-SECTION                                    
090100                                                                          
090200     MOVE 'WDGX5102 '      TO SSA1                                        
090300     MOVE '  '             TO GOOD-STATUSCODES                            
090400     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-AREA-WDGX5102                
090500                                  SSA1                                    
090600     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
090700     PERFORM IMS-STATUS-CHECK                                             
090800     .                                                                    
090900 IMS-06-REPL-WDGX5102 SECTION.                                            
091000     MOVE 'IMS-06' TO CURR-IMS-SECTION                                    
091100                                                                          
091200     MOVE '  '             TO GOOD-STATUSCODES                            
091300     CALL CBLTDLI USING REPL WDG3-PCB DLI-IO-AREA-WDGX5102                
091400     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
091500     PERFORM IMS-STATUS-CHECK                                             
091600     .                                                                    
091700 IMS-07-GET-ART-WDK7 SECTION.                                             
091800     MOVE 'IMS-07' TO CURR-IMS-SECTION                                    
091900                                                                          
092000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
092100            DELIMITED BY SIZE INTO SSA1                                   
092200     STRING 'WDK711  (IDDC     =' W-IDDC ')'                              
092300            DELIMITED BY SIZE INTO SSA2                                   
092400     MOVE '  GE'                TO GOOD-STATUSCODES                       
092500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA4 SSA1 SSA2                
092600     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
092700     PERFORM IMS-STATUS-CHECK                                             
092800     .                                                                    
092900 IMS-09-GET-ART-WDK6 SECTION.                                             
093000     MOVE 'IMS-09'  TO CURR-IMS-SECTION                                   
093100                                                                          
093200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
093300            DELIMITED BY SIZE INTO SSA1                                   
093400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
093500            DELIMITED BY SIZE INTO SSA2                                   
093600     MOVE '  '                  TO GOOD-STATUSCODES                       
093700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA SSA1 SSA2                 
093800     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
093900     PERFORM IMS-STATUS-CHECK                                             
094000     .                                                                    
094100 IMS-GN-WDD8B1 SECTION.                                                   
094200                                                                          
094300     STRING 'WDD8B1  (WDD8B1KY=>' W-WDD8B1KY-MIN-X                        
094400                    '&WDD8B1KY=<' W-WDD8B1KY-MAX-X ')'                    
094500            DELIMITED BY SIZE INTO SSA1                                   
094600     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
094700     CALL CBLTDLI USING GN WDD8B-PCB DLI-IO-AREA-D8B1 SSA1                
094800     MOVE WDD8B-STATUS-CODE TO STATUS-WS                                  
094900     PERFORM IMS-STATUS-CHECK                                             
095000     .                                                                    
095100     EJECT                                                                
095200 IMS-GU-WDD8B1 SECTION.                                                   
095300                                                                          
095400     STRING 'WDD8B1  (WDD8B1KY=>' W-WDD8B1KY-MIN-X                        
095500                    '&WDD8B1KY=<' W-WDD8B1KY-MAX-X ')'                    
095600            DELIMITED BY SIZE INTO SSA1                                   
095700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
095800     CALL CBLTDLI USING GU WDD8B-PCB DLI-IO-AREA-D8B1 SSA1                
095900     MOVE WDD8B-STATUS-CODE TO STATUS-WS                                  
096000     PERFORM IMS-STATUS-CHECK                                             
096100     .                                                                    
096200     EJECT                                                                
096300 IMS-10-GU-BEN-SEQ SECTION.                                               
096400     MOVE 'IMS-10' TO CURR-IMS-SECTION                                    
096500                                                                          
096600     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
096700             DELIMITED BY SIZE INTO SSA1                                  
096800     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
096900             DELIMITED BY SIZE INTO SSA2                                  
097000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
097100     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA SSA1 SSA2                 
097200     MOVE WDD3-STATUS-CODE        TO STATUS-WS                            
097300     PERFORM IMS-STATUS-CHECK                                             
097400     .                                                                    
097500*9748228 START                                                            
097600 IMS-GU-WDE4C1   SECTION.                                                 
097700     MOVE 'IMS-GU-WDE4C1' TO CURR-IMS-SECTION                             
097800     STRING 'WDE4C1  (WDE4C1KY=>' W-WDE4C1KY-LOW                          
097900                    '&WDE4C1KY=<' W-WDE4C1KY-HIGH                         
098000                    '&KDRADSTA <' W-ORDSTA-X ')'                          
098100             DELIMITED BY SIZE INTO SSA1                                  
098200     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
098300     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-E4C1 SSA1                     
098400     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
098500     PERFORM IMS-STATUS-CHECK                                             
098600     .                                                                    
098700     SKIP2                                                                
098800 IMS-GN-WDE4C1   SECTION.                                                 
098900     MOVE 'IMS-GN-WDE4C1' TO CURR-IMS-SECTION                             
099000     STRING 'WDE4C1  (WDE4C1KY=>' W-WDE4C1KY-LOW                          
099100                    '&WDE4C1KY=<' W-WDE4C1KY-HIGH                         
099200                    '&KDRADSTA <' W-ORDSTA-X ')'                          
099300             DELIMITED BY SIZE INTO SSA1                                  
099400     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
099500     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-E4C1 SSA1                     
099600     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
099700     PERFORM IMS-STATUS-CHECK                                             
099800     .                                                                    
099900     SKIP2                                                                
100000 IMS-GU-WDE4-ROT SECTION.                                                 
100100                                                                          
100200     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
100300             DELIMITED BY SIZE INTO SSA1                                  
100400     MOVE '    ' TO GOOD-STATUSCODES                                      
100500     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401 SSA1                      
100600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
100700     PERFORM IMS-STATUS-CHECK                                             
100800     .                                                                    
100900     EJECT                                                                
101000 IMS-GNP-WDE411       SECTION.                                            
101100                                                                          
101200     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
101300             DELIMITED BY SIZE INTO SSA1                                  
101400     MOVE '  ' TO GOOD-STATUSCODES                                        
101500     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E411 SSA1                     
101600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
101700     PERFORM IMS-STATUS-CHECK                                             
101800     .                                                                    
101900     EJECT                                                                
102000*9748228 END                                                              
102100 IMS-13-REPLACE SECTION.                                                  
102200     MOVE 'IMS-13' TO CURR-IMS-SECTION                                    
102300                                                                          
102400     MOVE '  '            TO GOOD-STATUSCODES                             
102500     CALL CBLTDLI USING REPL WDH1-PCB DLI-IO-AREA-WDH111                  
102600     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
102700     PERFORM IMS-STATUS-CHECK                                             
102800     .                                                                    
102900 IMS-GNP-WDH121 SECTION.                                                  
103000     MOVE 'WDH121 ' TO SSA1                                               
103100     MOVE '  GE' TO GOOD-STATUSCODES                                      
103200     CALL CBLTDLI USING GHNP WDH1-PCB DLI-IO-AREA-WDH121 SSA1             
103300     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
103400     PERFORM IMS-STATUS-CHECK                                             
103500     .                                                                    
103600     SKIP2                                                                
103700 IMS-REPLACE-WDH121 SECTION.                                              
103800     MOVE '  ' TO GOOD-STATUSCODES                                        
103900     CALL CBLTDLI USING REPL WDH1-PCB DLI-IO-AREA-WDH121                  
104000     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
104100     PERFORM IMS-STATUS-CHECK                                             
104200     .                                                                    
104300     EJECT                                                                
104400 IMS-INSERT-WDH121 SECTION.                                               
104500     MOVE 'IMS-INSERT-WDH121     ' TO CURR-IMS-SECTION                    
104600     MOVE 'WDH121 ' TO SSA1                                               
104700     MOVE '  ' TO GOOD-STATUSCODES                                        
104800     CALL CBLTDLI USING ISRT WDH1-PCB DLI-IO-AREA-WDH121 SSA1             
104900     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
105000     PERFORM IMS-STATUS-CHECK                                             
105100     .                                                                    
105200     EJECT                                                                
105300 IMS-GU-WDB601    SECTION.                                                
105400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
105500          DELIMITED BY SIZE INTO SSA1                                     
105600     MOVE '  ' TO GOOD-STATUSCODES                                        
105700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
105800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
105900     PERFORM IMS-STATUS-CHECK                                             
106000     .                                                                    
106100     SKIP3                                                                
106200 IMS-GU-WDE601 SECTION.                                                   
106300                                                                          
106400     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
106500          DELIMITED BY SIZE INTO SSA1                                     
106600     MOVE '  ' TO GOOD-STATUSCODES                                        
106700     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
106800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
106900     PERFORM IMS-STATUS-CHECK                                             
107000     .                                                                    
107100     EJECT                                                                
107200                                                                          
107300                                                                          
107400 IMS-STATUS-CHECK   SECTION.                                              
107500                                                                          
107600     SET STATUS-IX TO 1                                                   
107700     SEARCH GOOD-STATUS                                                   
107800       AT END                                                             
107900         CALL FELLOG                                                      
108000     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
108100       CONTINUE                                                           
108200     END-SEARCH                                                           
108300     .                                                                    
