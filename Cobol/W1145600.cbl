000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1145600.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   13/01/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        CREATE FILES WITH CANCELATIONS TO BE SENT TO SI+                 
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDK7                                       
001100*        PROGRAMMET LÄSER      WDP3                                       
001200*        PROGRAMMET LÄSER      WDB6                                       
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400*          --- DEMANDS FOR CANCELATION                                    
002500     SELECT W1145A                     ASSIGN TO W11456D1.                
002600                                                                          
002700                                                                          
002800*          --- ANNULLATIONER DC71                                         
002900     SELECT W1145C                     ASSIGN TO W11456D2.                
003000                                                                          
003100*          --- ANNULLATIONER DC72                                         
003200     SELECT W1145D                     ASSIGN TO W11456D3.                
003300                                                                          
003400*          --- ANNULLATIONER DC73                                         
003500     SELECT W1145E                     ASSIGN TO W11456D4.                
003600                                                                          
003700*          --- ANNULLATIONER DC41                                         
003800     SELECT W1145F                     ASSIGN TO W11456D5.                
003900                                                                          
004000*          --- ANNULLATIONER DC43                                         
004100     SELECT W1145G                     ASSIGN TO W11456D6.                
004200                                                                          
004300*          --- ANNULLATIONER DC44                                         
004400     SELECT W1145H                     ASSIGN TO W11456D7.                
004500                                                                          
004600*          --- ANNULLATIONER DC45                                         
004700     SELECT W1145I                     ASSIGN TO W11456D8.                
004800                                                                          
004900*          --- ANNULLATIONER DC46                                         
005000     SELECT W1145J                     ASSIGN TO W11456D9.                
005100                                                                          
005200                                                                          
005300*          --- ANNULLATIONER DC47                                         
005400     SELECT W1145K                     ASSIGN TO W11456DA.                
005500                                                                          
005600                                                                          
005700 DATA DIVISION.                                                           
005800 FILE SECTION.                                                            
005900                                                                          
006000 FD  W1145A                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300                                                                          
006400*01  -COPY W1145A      -L.                                                
006500                                                                          
006600                                                                          
006700 FD  W1145C                                                               
006800     RECORDING       F                                                    
006900     BLOCK CONTAINS  0.                                                   
007000                                                                          
007100*01  POST -COPY T335R301 -PRE  DC71-  -L.                                 
007200                                                                          
007300                                                                          
007400 FD  W1145D                                                               
007500     RECORDING       F                                                    
007600     BLOCK CONTAINS  0.                                                   
007700                                                                          
007800*01  POST -COPY T335R301 -PRE  DC72-  -L.                                 
007900                                                                          
008000                                                                          
008100 FD  W1145E                                                               
008200     RECORDING       F                                                    
008300     BLOCK CONTAINS  0.                                                   
008400                                                                          
008500*01  POST -COPY T335R301 -PRE  DC73-  -L.                                 
008600                                                                          
008700 FD  W1145F                                                               
008800     RECORDING       F                                                    
008900     BLOCK CONTAINS  0.                                                   
009000                                                                          
009100*01  POST -COPY T335R301 -PRE  DC41-  -L.                                 
009200                                                                          
009300 FD  W1145G                                                               
009400     RECORDING       F                                                    
009500     BLOCK CONTAINS  0.                                                   
009600                                                                          
009700*01  POST -COPY T335R301 -PRE  DC43-  -L.                                 
009800                                                                          
009900 FD  W1145H                                                               
010000     RECORDING       F                                                    
010100     BLOCK CONTAINS  0.                                                   
010200                                                                          
010300*01  POST -COPY T335R301 -PRE  DC44-  -L.                                 
010400                                                                          
010500 FD  W1145I                                                               
010600     RECORDING       F                                                    
010700     BLOCK CONTAINS  0.                                                   
010800                                                                          
010900*01  POST -COPY T335R301 -PRE  DC45-  -L.                                 
011000                                                                          
011100 FD  W1145J                                                               
011200     RECORDING       F                                                    
011300     BLOCK CONTAINS  0.                                                   
011400                                                                          
011500*01  POST -COPY T335R301 -PRE  DC46-  -L.                                 
011600                                                                          
011700 FD  W1145K                                                               
011800     RECORDING       F                                                    
011900     BLOCK CONTAINS  0.                                                   
012000                                                                          
012100*01  POST -COPY T335R301 -PRE  DC47-  -L.                                 
012200                                                                          
012300                                                                          
012400                                                                          
012500 WORKING-STORAGE SECTION.                                                 
012600                                                                          
012700 77  IDPGM                       PIC X(8)    VALUE 'W1145600'.            
012800 77  JA                          PIC X       VALUE 'J'.                   
012900 77  NEJ                         PIC X       VALUE 'N'.                   
013000                                                                          
013100 77  IX                          PIC 9(3)    VALUE ZERO.                  
013200 77  IX-MAX                      PIC 9(3)    VALUE ZERO.                  
013300 77  KOLL-IX                     PIC 9(3)    VALUE ZERO.                  
013400                                                                          
013500 77  W-IDLEVNR-71                PIC X(5)    VALUE SPACE.                 
013600 77  W-IDLEVNR-72                PIC X(5)    VALUE SPACE.                 
013700 77  W-IDLEVNR-73                PIC X(5)    VALUE SPACE.                 
013800 77  W-IDLEVNR-41                PIC X(5)    VALUE SPACE.                 
013900 77  W-IDLEVNR-43                PIC X(5)    VALUE SPACE.                 
014000 77  W-IDLEVNR-44                PIC X(5)    VALUE SPACE.                 
014100 77  W-IDLEVNR-45                PIC X(5)    VALUE SPACE.                 
014200 77  W-IDLEVNR-46                PIC X(5)    VALUE SPACE.                 
014300 77  W-IDLEVNR-47                PIC X(5)    VALUE SPACE.                 
014400                                                                          
014500 01  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
014600 01  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
014700                                                                          
014800*01  -COPY WWDCKONS                                                       
014900                                                                          
015000 77  W1145A-EOF-SW               PIC X       VALUE 'N'.                   
015100     88  END-OF-W1145A                       VALUE 'J'.                   
015200                                                                          
015300 77  SW-TRAFF                    PIC X       VALUE 'N'.                   
015400                                                                          
015500 01  ARTIKELNR.                                                           
015600     03 BLANKA-X                 PIC X(11) VALUE SPACE.                   
015700     03 ARTNR-ALFA               PIC X(9).                                
015800     03 ARTNR-NUM REDEFINES ARTNR-ALFA PIC 9(9).                          
015900                                                                          
016000 01  WS-IDUSER.                                                           
016100     03 WS-IDMAIL                PIC X(9).                                
016200     03 FILLER                   PIC X(51).                               
016300                                                                          
016400 01  KOLL-IDUSER                 PIC X(9).                                
016500 01  FILLER REDEFINES KOLL-IDUSER.                                        
016600     03  KOLL-TKN                PIC X  OCCURS 9.                         
016700                                                                          
016800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
016900 01  FILLER REDEFINES DAGENS-DATUM.                                       
017000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
017100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
017200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
017300                                                                          
017400                                                                          
017500 01  DYNAMISKA-SUBPROGRAM.                                                
017600                                                                          
017700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
017800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
018100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018200                                                                          
018300                                                                          
018400*    --- PARAMETRAR TILL ABEND                                            
018500                                                                          
018600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
018700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
018800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
018900                                                                          
019000 01  FELTEXT.                                                             
019100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
019200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
019300                                                                          
019400                                                                          
019500*    --- PARAMETRAR TILL POSTSUM                                          
019600                                                                          
019700*01  -COPY W0005   -PRE  POSTSUM-                                         
019800                                                                          
019900                                                                          
020000*    --- PARAMETRAR TILL WDATKONV                                         
020100                                                                          
020200*01  -COPY WDATAREA.                                                      
020300                                                                          
020400 01  IN-AREA-START               PIC X(24)   VALUE                        
020500                                 'IN-AREA-START    '.                     
020600                                                                          
020700*01  AREA -COPY W1145A     -PRE IN-                                       
020800                                                                          
020900                                                                          
021000                                                                          
021100 01  UT-AREA-START               PIC X(24)   VALUE                        
021200                                 'UT71-AREA-START  '.                     
021300                                                                          
021400*01  AREA -COPY T335R301     -PRE UT-                                     
021500                                                                          
021600                                                                          
021700                                                                          
021800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021900                                                                          
022000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022100                                                                          
022200 01  NYCKLAR-TILL-DLI.                                                    
022300     03  W-IDARTNR-X.                                                     
022400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
022500     03  W-IDDC-X.                                                        
022600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
022700     03  W-KDARBTYP-X.                                                    
022800         05  W-KDARBTYP          PIC X(8)    VALUE SPACE.                 
022900     03  W-IDPERSON-X.                                                    
023000         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
023100                                                                          
023200                                                                          
023300*    --- STATUS-KOD FRÅN IMS                                              
023400 01  STATUS-WS                   PIC XX.                                  
023500     88  SEGMENT-FINNS                       VALUE '  '.                  
023600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023800                                                                          
023900 01  GODK-STATUSKODER.                                                    
024000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024100                                                                          
024200 01  SSA1                        PIC X(64).                               
024300 01  SSA2                        PIC X(64).                               
024400 01  SSA3                        PIC X(64).                               
024500                                                                          
024600*    --- IMS FUNKTIONSKODER                                               
024700*01  -COPY W0003                                                          
024800                                                                          
024900                                                                          
025000                                                                          
025100*    ---  DLI INPUT-OUTPUT AREA                                           
025200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
025300 01  DLI-IO-WDK711.                                                       
025400*    03  -COPY WDK711                                                     
025500                                                                          
025600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
025700 01  DLI-IO-WDK722.                                                       
025800*    03  -COPY WDK722                                                     
025900                                                                          
026000                                                                          
026100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
026200 01  DLI-IO-WDP311.                                                       
026300*    03  -COPY WDP311                                                     
026400                                                                          
026500                                                                          
026600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
026700 01  DLI-IO-WDB601.                                                       
026800*    03  -COPY WDB601                                                     
026900                                                                          
027000                                                                          
027100                                                                          
027200 LINKAGE SECTION.                                                         
027300                                                                          
027400*01  -COPY W0008  -PRE WDK7-                                              
027500     05  FILLER                  PIC X.                                   
027600                                                                          
027700*01  -COPY W0008  -PRE WDP3-                                              
027800     05  FILLER                  PIC X.                                   
027900                                                                          
028000*01  -COPY W0008  -PRE WDB6-                                              
028100     05  FILLER                  PIC X.                                   
028200                                                                          
028300                                                                          
028400                                                                          
028500 PROCEDURE DIVISION  USING WDK7-PCB WDP3-PCB WDB6-PCB.                    
028600 MAIN SECTION.                                                            
028700     ENTRY 'DLITCBL' USING WDK7-PCB WDP3-PCB  WDB6-PCB.                   
028800                                                                          
028900                                                                          
029000     PERFORM A-INIT                                                       
029100                                                                          
029200     PERFORM S01-LAES-W1145A                                              
029300     PERFORM UNTIL END-OF-W1145A                                          
029400                                                                          
029500        MOVE IN-IDARTNR  TO W-IDARTNR                                     
029600        MOVE IN-IDDC     TO W-IDDC                                        
029700        PERFORM IMS-GU-WDK711                                             
029800        IF SEGMENT-FINNS                                                  
029900                                                                          
030000           PERFORM IMS-GU-WDK722                                          
030100           IF SEGMENT-SAKNAS                                              
030200              MOVE ZERO  TO XLAG-IDANSK                                   
030300           END-IF                                                         
030400           PERFORM B-SKAPA-SKRIV-UTPOST                                   
030500                                                                          
030600        END-IF                                                            
030700        PERFORM S01-LAES-W1145A                                           
030800     END-PERFORM                                                          
030900                                                                          
031000                                                                          
031100     PERFORM Z-FINIT                                                      
031200                                                                          
031300     MOVE ZERO TO RETURN-CODE                                             
031400     GOBACK                                                               
031500     .                                                                    
031600                                                                          
031700                                                                          
031800                                                                          
031900 A-INIT SECTION.                                                          
032000     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
032100                                                                          
032200     OPEN INPUT  W1145A                                                   
032300                                                                          
032400     OPEN OUTPUT W1145C                                                   
032500                 W1145D                                                   
032600                 W1145E                                                   
032700                 W1145F                                                   
032800                 W1145G                                                   
032900                 W1145H                                                   
033000                 W1145I                                                   
033100                 W1145J                                                   
033200                 W1145K                                                   
033300                                                                          
033400     ACCEPT DAGENS-DATUM  FROM DATE                                       
033500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
033600                                                                          
033700     MOVE WC-NDC-CN-71 TO W-IDDC                                          
033800     PERFORM IMS-GU-WDB601                                                
033900     IF SEGMENT-FINNS                                                     
034000        MOVE DCS-IDLEVNR-DC TO W-IDLEVNR-71                               
034100     END-IF                                                               
034200                                                                          
034300     MOVE WC-NDC-CN-72 TO W-IDDC                                          
034400     PERFORM IMS-GU-WDB601                                                
034500     IF SEGMENT-FINNS                                                     
034600        MOVE DCS-IDLEVNR-DC TO W-IDLEVNR-72                               
034700     END-IF                                                               
034800                                                                          
034900     MOVE WC-NDC-CN-73 TO W-IDDC                                          
035000     PERFORM IMS-GU-WDB601                                                
035100     IF SEGMENT-FINNS                                                     
035200        MOVE DCS-IDLEVNR-DC TO W-IDLEVNR-73                               
035300     END-IF                                                               
035400                                                                          
035500*--  USA SKALL HA PARTNER NO/PLANTAN SOM LEVNR FRÅN SI+                   
035600     MOVE WC-NDC-US-RU TO W-IDDC                                          
035700     PERFORM IMS-GU-WDB601                                                
035800     IF SEGMENT-FINNS                                                     
035900        MOVE DCS-IDLEVNR-EMB TO W-IDLEVNR-41                              
036000     END-IF                                                               
036100                                                                          
036200     MOVE WC-NDC-US-LA TO W-IDDC                                          
036300     PERFORM IMS-GU-WDB601                                                
036400     IF SEGMENT-FINNS                                                     
036500        MOVE DCS-IDLEVNR-EMB TO W-IDLEVNR-43                              
036600     END-IF                                                               
036700                                                                          
036800     MOVE WC-NDC-US-SE TO W-IDDC                                          
036900     PERFORM IMS-GU-WDB601                                                
037000     IF SEGMENT-FINNS                                                     
037100        MOVE DCS-IDLEVNR-EMB TO W-IDLEVNR-44                              
037200     END-IF                                                               
037300                                                                          
037400     MOVE WC-NDC-US-CH TO W-IDDC                                          
037500     PERFORM IMS-GU-WDB601                                                
037600     IF SEGMENT-FINNS                                                     
037700        MOVE DCS-IDLEVNR-EMB TO W-IDLEVNR-45                              
037800     END-IF                                                               
037900                                                                          
038000     MOVE WC-NDC-US-JA TO W-IDDC                                          
038100     PERFORM IMS-GU-WDB601                                                
038200     IF SEGMENT-FINNS                                                     
038300        MOVE DCS-IDLEVNR-EMB TO W-IDLEVNR-46                              
038400     END-IF                                                               
038500                                                                          
038600     MOVE WC-NDC-US-DA TO W-IDDC                                          
038700     PERFORM IMS-GU-WDB601                                                
038800     IF SEGMENT-FINNS                                                     
038900        MOVE DCS-IDLEVNR-EMB TO W-IDLEVNR-47                              
039000     END-IF                                                               
039100                                                                          
039200     .                                                                    
039300                                                                          
039400 B-SKAPA-SKRIV-UTPOST SECTION.                                            
039500     MOVE 'B-SKAPA-SKRIV   ' TO CURRENT-SECTION                           
039600                                                                          
039700     MOVE '301'       TO UT-IDRT                                          
039800     MOVE SPACE       TO UT-COMMON-AREA                                   
039900                         UT-NP-AREA                                       
040000                         UT-CC-AREA                                       
040100                                                                          
040200     MOVE W-IDARTNR TO ARTNR-NUM                                          
040300     INSPECT ARTNR-ALFA REPLACING LEADING ZEROES BY SPACE                 
040400     MOVE ARTIKELNR   TO UT-IDPITEM                                       
040500     MOVE 'CC'        TO UT-CDTYPE-REQ                                    
040600     MOVE 'V'         TO UT-CD-IDPITEM                                    
040700     MOVE 'VCAS'      TO UT-IDPORG                                        
040800     MOVE 'Y'         TO UT-FLCOBL-ALLOWED                                
040900                                                                          
041000     IF XLAG-IDANSK > 0                                                   
041100        MOVE 'ANSK'           TO W-KDARBTYP                               
041200        MOVE XLAG-IDANSK      TO W-IDPERSON                               
041300        PERFORM IMS-GU-WDP311                                             
041400        IF SEGMENT-SAKNAS                                                 
041500           DISPLAY ' IDANSK SAKNAS PÅ P311: ' W-IDPERSON                  
041600        ELSE                                                              
041700           MOVE PERS-IDNAMN TO UT-NMHANDLR-ISSUER                         
041800           MOVE PERS-IDTFN  TO UT-IDPHONE-ISSUER                          
041900           MOVE PERS-IDAVD  TO UT-IDSECTN-ISSUER                          
042000           MOVE PERS-IDMAIL TO WS-IDUSER                                  
042100           PERFORM BA-KOLLA-IDUSER                                        
042200        END-IF                                                            
042300     END-IF                                                               
042400                                                                          
042500     MOVE 'N'            TO UT-CC-FLMTRL                                  
042600     MOVE 'Y'            TO UT-CC-FLTOTAL-CANCEL                          
042700     MOVE 'BL'           TO UT-CC-CDMODE-ORDER                            
042800                                                                          
042900*    MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
043000*    MOVE  IN-DATUM-UTSKR TO DAT-I-TIDATUM                                
043100*    CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
043200*                        DAT-O-TIDATUM DAT-KDSVAR                         
043300*    IF DAT-KDSVAR-OK                                                     
043400*      MOVE DAT-TIAAVV-GRP  TO  AAVV                                      
043500*    END-IF                                                               
043600*                                                                         
043700*    MOVE AAVV  TO UT-CC-TIEXIT-WEEK                                      
043800     MOVE 'IDAG'          TO DAT-KDDATFORM                                
043900     CALL WDATKONV USING     DAT-KDDATFORM                                
044000                             DAT-I-TIDATUM                                
044100                             DAT-O-TIDATUM                                
044200                             DAT-KDSVAR                                   
044300     MOVE DAT-TIAAVV-GRP  TO UT-CC-TIEXIT-WEEK                            
044400                                                                          
044500     MOVE ZERO  TO UT-CC-IDSUPPL                                          
044600                   UT-CC-QTPITEM-CANCEL                                   
044700                   UT-CC-QTPITEM-LAST-DEL                                 
044800                   UT-CC-QTPITEM-REMAIN                                   
044900                   UT-CC-QTPITEM-STOCK                                    
045000                   UT-CC-IDSUFFIX-ORDER                                   
045100                                                                          
045200     EVALUATE IN-IDDC                                                     
045300       WHEN WC-NDC-CN-71                                                  
045400          MOVE W-IDLEVNR-71 TO UT-CC-IDUSER                               
045500          PERFORM S11-SKRIV-W1145C                                        
045600       WHEN WC-NDC-CN-72                                                  
045700          MOVE W-IDLEVNR-72 TO UT-CC-IDUSER                               
045800          PERFORM S12-SKRIV-W1145D                                        
045900       WHEN WC-NDC-CN-73                                                  
046000          MOVE W-IDLEVNR-73 TO UT-CC-IDUSER                               
046100          PERFORM S13-SKRIV-W1145E                                        
046200       WHEN WC-NDC-US-RU                                                  
046300          MOVE W-IDLEVNR-41 TO UT-CC-IDUSER                               
046400          PERFORM S14-SKRIV-W1145F                                        
046500       WHEN WC-NDC-US-LA                                                  
046600          MOVE W-IDLEVNR-43 TO UT-CC-IDUSER                               
046700          PERFORM S15-SKRIV-W1145G                                        
046800       WHEN WC-NDC-US-SE                                                  
046900          MOVE W-IDLEVNR-44 TO UT-CC-IDUSER                               
047000          PERFORM S16-SKRIV-W1145H                                        
047100       WHEN WC-NDC-US-CH                                                  
047200          MOVE W-IDLEVNR-45 TO UT-CC-IDUSER                               
047300          PERFORM S17-SKRIV-W1145I                                        
047400       WHEN WC-NDC-US-JA                                                  
047500          MOVE W-IDLEVNR-46 TO UT-CC-IDUSER                               
047600          PERFORM S18-SKRIV-W1145J                                        
047700       WHEN WC-NDC-US-DA                                                  
047800          MOVE W-IDLEVNR-47 TO UT-CC-IDUSER                               
047900          PERFORM S19-SKRIV-W1145K                                        
048000     END-EVALUATE                                                         
048100     .                                                                    
048200     EJECT                                                                
048300 BA-KOLLA-IDUSER SECTION.                                                 
048400     MOVE 'BA-KOLLA-IDUSER ' TO CURRENT-SECTION                           
048500                                                                          
048600     MOVE NEJ       TO SW-TRAFF                                           
048700                                                                          
048800     MOVE WS-IDMAIL TO KOLL-IDUSER                                        
048900     MOVE 1 TO IX                                                         
049000     MOVE 9 TO IX-MAX                                                     
049100     PERFORM UNTIL IX > IX-MAX                                            
049200        IF KOLL-TKN(IX) = '@'                                             
049300           MOVE IX TO KOLL-IX                                             
049400           MOVE 10 TO IX                                                  
049500           MOVE JA TO SW-TRAFF                                            
049600        ELSE                                                              
049700           ADD 1 TO IX                                                    
049800        END-IF                                                            
049900     END-PERFORM                                                          
050000                                                                          
050100     IF SW-TRAFF = NEJ                                                    
050200        MOVE KOLL-IDUSER(1:8)      TO UT-IDUSERID-ISSUER                  
050300     ELSE                                                                 
050400       ADD -1 TO KOLL-IX                                                  
050500       MOVE KOLL-IDUSER(1:KOLL-IX) TO UT-IDUSERID-ISSUER                  
050600     END-IF                                                               
050700     .                                                                    
050800                                                                          
050900                                                                          
051000 Z-FINIT SECTION.                                                         
051100     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
051200                                                                          
051300     CLOSE W1145A                                                         
051400           W1145C                                                         
051500           W1145D                                                         
051600           W1145E                                                         
051700           W1145F                                                         
051800           W1145G                                                         
051900           W1145H                                                         
052000           W1145I                                                         
052100           W1145J                                                         
052200           W1145K                                                         
052300                                                                          
052400     SKIP2                                                                
052500     MOVE 'S' TO POSTSUM-OPKOD                                            
052600     CALL POSTSUM USING POSTSUM-PARM                                      
052700     .                                                                    
052800                                                                          
052900                                                                          
053000                                                                          
053100 S01-LAES-W1145A  SECTION.                                                
053200                                                                          
053300     READ W1145A INTO IN-AREA                                             
053400     AT END                                                               
053500        MOVE HIGH-VALUE   TO IN-AREA                                      
053600        SET END-OF-W1145A TO TRUE                                         
053700                                                                          
053800     NOT AT END                                                           
053900        MOVE 'W1145A'   TO POSTSUM-FDNAMN                                 
054000        MOVE 'W11456D1' TO POSTSUM-DDNAMN2                                
054100        MOVE SPACE      TO POSTSUM-TRANSTYP                               
054200        CALL POSTSUM USING POSTSUM-PARM                                   
054300     END-READ                                                             
054400     .                                                                    
054500                                                                          
054600                                                                          
054700 S11-SKRIV-W1145C SECTION.                                                
054800                                                                          
054900     WRITE DC71-POST FROM UT-AREA                                         
055000                                                                          
055100     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
055200     MOVE 'W1145C'   TO POSTSUM-FDNAMN                                    
055300     MOVE 'W11456D2' TO POSTSUM-DDNAMN2                                   
055400     CALL POSTSUM USING POSTSUM-PARM                                      
055500     .                                                                    
055600                                                                          
055700                                                                          
055800 S12-SKRIV-W1145D SECTION.                                                
055900                                                                          
056000     WRITE DC72-POST FROM UT-AREA                                         
056100                                                                          
056200     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
056300     MOVE 'W1145D'   TO POSTSUM-FDNAMN                                    
056400     MOVE 'W11456D3' TO POSTSUM-DDNAMN2                                   
056500     CALL POSTSUM USING POSTSUM-PARM                                      
056600     .                                                                    
056700                                                                          
056800                                                                          
056900 S13-SKRIV-W1145E SECTION.                                                
057000                                                                          
057100     WRITE DC73-POST FROM UT-AREA                                         
057200                                                                          
057300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
057400     MOVE 'W1145E'   TO POSTSUM-FDNAMN                                    
057500     MOVE 'W11456D4' TO POSTSUM-DDNAMN2                                   
057600     CALL POSTSUM USING POSTSUM-PARM                                      
057700     .                                                                    
057800                                                                          
057900 S14-SKRIV-W1145F SECTION.                                                
058000                                                                          
058100     WRITE DC41-POST FROM UT-AREA                                         
058200                                                                          
058300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
058400     MOVE 'W1145F'   TO POSTSUM-FDNAMN                                    
058500     MOVE 'W11456D5' TO POSTSUM-DDNAMN2                                   
058600     CALL POSTSUM USING POSTSUM-PARM                                      
058700     .                                                                    
058800                                                                          
058900 S15-SKRIV-W1145G SECTION.                                                
059000                                                                          
059100     WRITE DC43-POST FROM UT-AREA                                         
059200                                                                          
059300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
059400     MOVE 'W1145G'   TO POSTSUM-FDNAMN                                    
059500     MOVE 'W11456D6' TO POSTSUM-DDNAMN2                                   
059600     CALL POSTSUM USING POSTSUM-PARM                                      
059700     .                                                                    
059800                                                                          
059900 S16-SKRIV-W1145H SECTION.                                                
060000                                                                          
060100     WRITE DC44-POST FROM UT-AREA                                         
060200                                                                          
060300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
060400     MOVE 'W1145H'   TO POSTSUM-FDNAMN                                    
060500     MOVE 'W11456D7' TO POSTSUM-DDNAMN2                                   
060600     CALL POSTSUM USING POSTSUM-PARM                                      
060700     .                                                                    
060800                                                                          
060900 S17-SKRIV-W1145I SECTION.                                                
061000                                                                          
061100     WRITE DC45-POST FROM UT-AREA                                         
061200                                                                          
061300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
061400     MOVE 'W1145I'   TO POSTSUM-FDNAMN                                    
061500     MOVE 'W11456D8' TO POSTSUM-DDNAMN2                                   
061600     CALL POSTSUM USING POSTSUM-PARM                                      
061700     .                                                                    
061800                                                                          
061900 S18-SKRIV-W1145J SECTION.                                                
062000                                                                          
062100     WRITE DC46-POST FROM UT-AREA                                         
062200                                                                          
062300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
062400     MOVE 'W1145J'   TO POSTSUM-FDNAMN                                    
062500     MOVE 'W11456D9' TO POSTSUM-DDNAMN2                                   
062600     CALL POSTSUM USING POSTSUM-PARM                                      
062700     .                                                                    
062800                                                                          
062900 S19-SKRIV-W1145K SECTION.                                                
063000                                                                          
063100     WRITE DC47-POST FROM UT-AREA                                         
063200                                                                          
063300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
063400     MOVE 'W1145K'   TO POSTSUM-FDNAMN                                    
063500     MOVE 'W11456DA' TO POSTSUM-DDNAMN2                                   
063600     CALL POSTSUM USING POSTSUM-PARM                                      
063700     .                                                                    
063800                                                                          
063900                                                                          
064000                                                                          
064100* --- IMS SEKTIONER ---                                                   
064200                                                                          
064300 IMS-GU-WDK711 SECTION.                                                   
064400     MOVE 'IMS-GU-WDK711   ' TO CURRENT-IMS-SECTION                       
064500                                                                          
064600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
064700          DELIMITED BY SIZE INTO SSA1                                     
064800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
064900          DELIMITED BY SIZE INTO SSA2                                     
065000     MOVE '  GE'              TO GODK-STATUSKODER                         
065100     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
065200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
065300     PERFORM IMS-STATUSKONTROLL                                           
065400     .                                                                    
065500                                                                          
065600                                                                          
065700 IMS-GU-WDK722 SECTION.                                                   
065800     MOVE 'IMS-GU-WDK722   ' TO CURRENT-IMS-SECTION                       
065900                                                                          
066000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
066100          DELIMITED BY SIZE INTO SSA1                                     
066200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
066300          DELIMITED BY SIZE INTO SSA2                                     
066400     MOVE 'WDK722 '           TO SSA3                                     
066500     MOVE '  GE'              TO GODK-STATUSKODER                         
066600     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
066700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
066800     PERFORM IMS-STATUSKONTROLL                                           
066900     .                                                                    
067000                                                                          
067100                                                                          
067200 IMS-GU-WDP311 SECTION.                                                   
067300     MOVE 'IMS-GU-WDP311   ' TO CURRENT-IMS-SECTION                       
067400                                                                          
067500     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
067600          DELIMITED BY SIZE INTO SSA1                                     
067700     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
067800          DELIMITED BY SIZE INTO SSA2                                     
067900     MOVE '  GE'              TO GODK-STATUSKODER                         
068000     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
068100     MOVE WDP3-STATUS-CODE    TO STATUS-WS                                
068200     PERFORM IMS-STATUSKONTROLL                                           
068300     .                                                                    
068400                                                                          
068500                                                                          
068600 IMS-GU-WDB601 SECTION.                                                   
068700     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
068800                                                                          
068900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
069000          DELIMITED BY SIZE INTO SSA1                                     
069100     MOVE '  GE'              TO GODK-STATUSKODER                         
069200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
069300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
069400     PERFORM IMS-STATUSKONTROLL                                           
069500     .                                                                    
069600                                                                          
069700                                                                          
069800 IMS-STATUSKONTROLL SECTION.                                              
069900                                                                          
070000     SET STATUS-IX TO 1                                                   
070100     SEARCH GODK-STATUS                                                   
070200       AT END                                                             
070300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
070400           DELIMITED BY SIZE INTO FELTEXT                                 
070500         DISPLAY FELTEXT                                                  
070600         CALL FELLOG                                                      
070700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
070800         CONTINUE                                                         
070900     END-SEARCH                                                           
071000     .                                                                    
