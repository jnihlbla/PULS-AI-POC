000100                                                                          
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W4635500.                                                
000500 AUTHOR.         BO SVENSSON.                                             
000600 DATE-WRITTEN.   97/10/21.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*                                                                         
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        TAR EMOT EDIPOSTER FRÅN DIREKTLEVERANTÖR                         
001300*        SKAPAR POSTER PÅ W46356-FIL SOM SKALL TILL AUTOMATISK            
001400*        PACKNING.                                                        
001500*        SKRIVER ÄVEN UT EDI-LOGG FÖR VIDARE BEARBETNING.                 
001600*                                                                         
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- EDI-FIL FRåN DDGS-LEV                                      
003100     SELECT W46355                     ASSIGN TO W46355D1.                
003200     SKIP2                                                                
003300*          --- GNB-FIL FRåN EDI TILL AUTOMATPACKNING                      
003400     SELECT W46356                     ASSIGN TO W46355D2.                
003500     skip2                                                                
003600                                                                          
003700*          --- UTFIL TILL LOGG AV TYP DESADV                              
003800     SELECT W4635A                     ASSIGN TO W46355D3.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W46355                                                               
004500     RECORDING       V                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800 01  IN-POST         PIC X(1005).                                         
004900                                                                          
005000*01  -COPY WEDIUNB0      -L.                                              
005100                                                                          
005200*01  -COPY WEDIUNH0      -L.                                              
005300                                                                          
005400*01  -COPY WEDIBGM0      -L.                                              
005500                                                                          
005600*01  -COPY WEDIDTM0      -L.                                              
005700                                                                          
005800*01  -COPY WEDIRFF0      -L.                                              
005900                                                                          
006000*01  -COPY WEDINAD0      -L.                                              
006100                                                                          
006200*01  -COPY WEDILOC0      -L.                                              
006300                                                                          
006400*01  -COPY WEDICPS0      -L.                                              
006500                                                                          
006600*01  -COPY WEDIPAC0      -L.                                              
006700                                                                          
006800*01  -COPY WEDIQTY0      -L.                                              
006900                                                                          
007000*01  -COPY WEDIPCI0      -L.                                              
007100                                                                          
007200*01  -COPY WEDIGIR0      -L.                                              
007300                                                                          
007400*01  -COPY WEDILIN0      -L.                                              
007500                                                                          
007600*01  -COPY WEDIALI0      -L.                                              
007700                                                                          
007800*01  -COPY WEDIMEA0      -L.                                              
007900                                                                          
008000*01  -COPY WEDIUNT0      -L.                                              
008100     SKIP3                                                                
008200 FD  W46356                                                               
008300     RECORDING       F                                                    
008400     BLOCK CONTAINS  0.                                                   
008500                                                                          
008600*01  POST -COPY W46356 -PRE  UT-  -L.                                     
008700     EJECT                                                                
008800                                                                          
008900 FD  W4635A                                                               
009000     RECORDING       F                                                    
009100     BLOCK CONTAINS  0.                                                   
009200 01  UT-LOGG.                                                             
009300*    03 -COPY W46341  -L.                                                 
009400     EJECT                                                                
009500 WORKING-STORAGE SECTION.                                                 
009600                                                                          
009700*    -- CHECKED BY WY2000                                                 
009800 77  IDPGM                       PIC X(8)    VALUE 'W4635500'.            
009900 77  JA                          PIC X       VALUE 'J'.                   
010000 77  NEJ                         PIC X       VALUE 'N'.                   
010100     SKIP2                                                                
010200 01  FELTEXT.                                                             
010300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010500                                                                          
010600 77  W46355-EOF-SW               PIC X       VALUE 'N'.                   
010700     88  END-OF-W46355                       VALUE 'J'.                   
010800     EJECT                                                                
010900                                                                          
011000 77  FORM-FEL-SW                 PIC X       VALUE 'N'.                   
011100     88  FORM-FEL                            VALUE 'J'.                   
011200                                                                          
011300 77  SKRIV-RAD-SW                PIC X       VALUE 'N'.                   
011400     88  SKRIV-RAD                           VALUE 'J'.                   
011500     EJECT                                                                
011600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011700 01  FILLER REDEFINES DAGENS-DATUM.                                       
011800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
011900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012100                                                                          
012200 01  WS-DADAT-X.                                                          
012300     03  WS-SEKEL                PIC 9(2).                                
012400     03  WS-DAT                  PIC 9(6).                                
012500 01  WS-DADAT-N REDEFINES WS-DADAT-X                                      
012600                                 PIC 9(8).                                
012700 01  WS-NAD-PARTY-ID-X.                                                   
012800     03  WS-NAD-IDDISTR          PIC X(4).                                
012900     03  WS-NAD-IDKUNDNR         PIC X(6).                                
013000 01  WS-NAD-PARTY-ID-N REDEFINES WS-NAD-PARTY-ID-X                        
013100                                 PIC 9(10).                               
013200                                                                          
013300 01  WS-DAGENS-DATUM-8           PIC 9(8).                                
013400 01  WS-DAGENS-KLOCKA.                                                    
013500     03  WS-DAGENS-KLOCKA-1-6    PIC 9(6).                                
013600     03  WS-DAGENS-KLOCKA-7-9    PIC 9(3).                                
013700                                                                          
013800 01  WS-METER                    PIC S9(3)V9(2).                          
013900                                                                          
014000 01  WS-KOLLITAB-IX              PIC S9(3)   VALUE ZERO.                  
014100 01  WS-KOLLITAB-ANT             PIC S9(3)   VALUE ZERO.                  
014200 01  WS-KOLLITAB-MAX             PIC S9(3)   VALUE 250.                   
014300 01  WS-KOLLITAB.                                                         
014400     03  WS-RAPP-KOLLINR OCCURS 250                                       
014500                                 PIC 9(5).                                
014600                                                                          
014700     EJECT                                                                
014800 01  DYNAMISKA-SUBPROGRAM.                                                
014900*                                                                         
015000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
015100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015200     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
015300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015400     SKIP2                                                                
015500*    --- PARAMETRAR TILL ABEND                                            
015600                                                                          
015700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
015800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
015900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
016000     EJECT                                                                
016100 01  FILLER                     PIC X(10)   VALUE 'WDATAREA'.             
016200*01 -COPY WDATAREA                                                        
016300     EJECT                                                                
016400 01  FILLER                     PIC X(10)   VALUE 'WDECAREA'.             
016500*01 -COPY WDECAREA                                                        
016600     EJECT                                                                
016700*    --- PARAMETRAR TILL POSTSUM                                          
016800*                                                                         
016900*01  -COPY W0005   -PRE  POSTSUM-                                         
017000     EJECT                                                                
017100 01  IN-AREA-START               PIC X(24)   VALUE                        
017200                                 'IN-AREA-START  '.                       
017300     SKIP2                                                                
017400 01  IN-AREA.                                                             
017500     03  IN-AREA-0.                                                       
017600       05  IN-IDPTYP             PIC X(3).                                
017700       05  FILLER                PIC X(1002).                             
017800*   03  FILLER -COPY WEDIUNB0  -PRE IN-  -RED  IN-AREA-0                  
017900*   03  FILLER -COPY WEDIUNH0  -PRE IN-  -RED  IN-AREA-0                  
018000*   03  FILLER -COPY WEDIBGM0  -PRE IN-  -RED  IN-AREA-0                  
018100*   03  FILLER -COPY WEDIDTM0  -PRE IN-  -RED  IN-AREA-0                  
018200*   03  FILLER -COPY WEDIRFF0  -PRE IN-  -RED  IN-AREA-0                  
018300*   03  FILLER -COPY WEDINAD0  -PRE IN-  -RED  IN-AREA-0                  
018400*   03  FILLER -COPY WEDILOC0  -PRE IN-  -RED  IN-AREA-0                  
018500*   03  FILLER -COPY WEDICPS0  -PRE IN-  -RED  IN-AREA-0                  
018600*   03  FILLER -COPY WEDIPAC0  -PRE IN-  -RED  IN-AREA-0                  
018700*   03  FILLER -COPY WEDIQTY0  -PRE IN-  -RED  IN-AREA-0                  
018800*   03  FILLER -COPY WEDIPCI0  -PRE IN-  -RED  IN-AREA-0                  
018900*   03  FILLER -COPY WEDIGIR0  -PRE IN-  -RED  IN-AREA-0                  
019000*   03  FILLER -COPY WEDILIN0  -PRE IN-  -RED  IN-AREA-0                  
019100*   03  FILLER -COPY WEDIALI0  -PRE IN-  -RED  IN-AREA-0                  
019200*   03  FILLER -COPY WEDIMEA0  -PRE IN-  -RED  IN-AREA-0                  
019300*   03  FILLER -COPY WEDIUNT0  -PRE IN-  -RED  IN-AREA-0                  
019400     EJECT                                                                
019500 01  UT-AREA-START               PIC X(24)   VALUE                        
019600                                 'UT-AREA-START  '.                       
019700     SKIP2                                                                
019800                                                                          
019900*01  AREA -COPY W46356     -PRE UT-                                       
020000     EJECT                                                                
020100                                                                          
020200 01  UT-AREA-LOGG-START          PIC X(24)   VALUE                        
020300                                 'UT-AREA-LOGG-START  '.                  
020400     SKIP2                                                                
020500                                                                          
020600*01  AREA -COPY W46341     -PRE UTL-                                      
020700     EJECT                                                                
020800 PROCEDURE DIVISION.                                                      
020900 MAIN SECTION.                                                            
021000     SKIP2                                                                
021100                                                                          
021200     PERFORM A-INIT                                                       
021300     PERFORM S01-LAES-W46355                                              
021400     PERFORM UNTIL END-OF-W46355                                          
021500       EVALUATE IN-UNH-IDPTYP                                             
021600         WHEN 'UNB'                                                       
021700           PERFORM R-UNB                                                  
021800         WHEN 'UNH'                                                       
021900           PERFORM B-UNT                                                  
022000         WHEN 'BGM'                                                       
022100           PERFORM C-BGM                                                  
022200         WHEN 'DTM'                                                       
022300           PERFORM D-DTM                                                  
022400         WHEN 'RFF'                                                       
022500           PERFORM E-RFF                                                  
022600         WHEN 'NAD'                                                       
022700           PERFORM F-NAD                                                  
022800         WHEN 'LOC'                                                       
022900           PERFORM G-LOC                                                  
023000         WHEN 'CPS'                                                       
023100           PERFORM H-CPS                                                  
023200         WHEN 'PAC'                                                       
023300           PERFORM I-PAC                                                  
023400         WHEN 'MEA'                                                       
023500           PERFORM J-MEA                                                  
023600         WHEN 'QTY'                                                       
023700           PERFORM K-QTY                                                  
023800         WHEN 'PCI'                                                       
023900           PERFORM L-PCI                                                  
024000         WHEN 'GIR'                                                       
024100           PERFORM M-GIR                                                  
024200         WHEN 'LIN'                                                       
024300           PERFORM N-LIN                                                  
024400         WHEN 'ALI'                                                       
024500           PERFORM O-ALI                                                  
024600         WHEN 'UNT'                                                       
024700           PERFORM P-UNT                                                  
024800         WHEN 'UNZ'                                                       
024900           PERFORM Q-UNZ                                                  
025000       END-EVALUATE                                                       
025100                                                                          
025200       IF  FORM-FEL                                                       
025300           DISPLAY FELTEXT                                                
025400           PERFORM S99-ABEND                                              
025500       END-IF                                                             
025600                                                                          
025700       PERFORM S01-LAES-W46355                                            
025800     END-PERFORM                                                          
025900                                                                          
026000     PERFORM Z-FINIT                                                      
026100                                                                          
026200     MOVE ZERO TO RETURN-CODE                                             
026300     GOBACK                                                               
026400     .                                                                    
026500     EJECT                                                                
026600 A-INIT SECTION.                                                          
026700                                                                          
026800     OPEN INPUT  W46355                                                   
026900                                                                          
027000     OPEN OUTPUT W46356                                                   
027100                 W4635A                                                   
027200     SKIP2                                                                
027300     ACCEPT WS-DAGENS-KLOCKA         FROM TIME                            
027400     MOVE FUNCTION CURRENT-DATE(1:8) TO   WS-DAGENS-DATUM-8               
027500     ACCEPT DAGENS-DATUM             FROM DATE                            
027600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027700     MOVE SPACE TO UT-AREA                                                
027800     MOVE ZERO  TO UT-VKART                                               
027900     MOVE NEJ   TO FORM-FEL-SW                                            
028000     MOVE NEJ   TO SKRIV-RAD-SW                                           
028100     .                                                                    
028200     EJECT                                                                
028300 B-UNT SECTION.                                                           
028400                                                                          
028500     CONTINUE                                                             
028600     .                                                                    
028700 C-BGM SECTION.                                                           
028800                                                                          
028900     IF  SKRIV-RAD                                                        
029000         PERFORM S11-SKRIV-W46356                                         
029100         PERFORM S31-BYGG-LOGG                                            
029200         PERFORM S12-SKRIV-W4635A                                         
029300         MOVE NEJ   TO SKRIV-RAD-SW                                       
029400     END-IF                                                               
029500                                                                          
029600     PERFORM S21-NOLLA-BGM                                                
029700                                                                          
029800     UNSTRING IN-BGM-DOCNO DELIMITED BY SPACE                             
029900                              INTO UT-IDPRODNR                            
030000     IF  UT-IDPRODNR NOT NUMERIC                                          
030100         MOVE 'IDPRODNR EJ NUMERISKT'   TO FELTEXT-STR                    
030200         MOVE JA                        TO FORM-FEL-SW                    
030300     END-IF                                                               
030400     MOVE UT-IDPRODNR                   TO UTL-IDPRODNR                   
030500                                                                          
030600     MOVE ZERO                          TO WS-KOLLITAB-ANT                
030700                                           WS-KOLLITAB                    
030800     MOVE NEJ                           TO UT-FLFEL                       
030900     .                                                                    
031000 D-DTM SECTION.                                                           
031100                                                                          
031200     MOVE "AAMMDD" TO DAT-KDDATFORM                                       
031300     MOVE IN-DTM-DATE-TIME(3:6) TO DAT-I-TIDATUM                          
031400     CALL WDATKONV USING                                                  
031500          DAT-KDDATFORM,                                                  
031600          DAT-I-TIDATUM,                                                  
031700          DAT-O-TIDATUM,                                                  
031800          DAT-KDSVAR                                                      
031900                                                                          
032000     IF  DAT-KDSVAR-OK                                                    
032100         IF  DAT-TISEKEL = IN-DTM-DATE-TIME(1:2)                          
032200             MOVE IN-DTM-DATE-TIME(1:8) TO UT-DASUPREF                    
032300                                           UTL-DASUPREF                   
032400             IF  IN-DTM-DATE-TIME(9:4) NUMERIC                            
032500                 MOVE IN-DTM-DATE-TIME(9:4) TO UT-TISUPTID                
032600                                               UTL-TISUPTID               
032700             ELSE                                                         
032800                 MOVE 'TISUPTID FELAKTIG' TO FELTEXT-STR                  
032900                 MOVE JA                  TO FORM-FEL-SW                  
033000             END-IF                                                       
033100         ELSE                                                             
033200             MOVE 'DASUPREF  FELAKTIG' TO FELTEXT-STR                     
033300             MOVE JA                   TO FORM-FEL-SW                     
033400         END-IF                                                           
033500     ELSE                                                                 
033600         MOVE 'DASUPREF  FELAKTIG' TO FELTEXT-STR                         
033700         MOVE JA                   TO FORM-FEL-SW                         
033800     END-IF                                                               
033900     .                                                                    
034000 E-RFF SECTION.                                                           
034100                                                                          
034200     IF  IN-RFF-QUAL = 'AAS'                                              
034300         UNSTRING IN-RFF-REFNO DELIMITED BY SPACE                         
034400                              INTO UT-IDSUPREF                            
034500*        IF  UT-IDSUPREF NOT NUMERIC                                      
034600*            MOVE 'IDSUPREF EJ NUMERISKT' TO FELTEXT-STR                  
034700*            MOVE JA                      TO FORM-FEL-SW                  
034800*        END-IF                                                           
034900         MOVE UT-IDSUPREF                 TO UTL-IDSUPREF                 
035000     END-IF                                                               
035100                                                                          
035200     IF  IN-RFF-QUAL = 'IV '                                              
035300     AND (UT-IDLEVNR = '7500 '                                            
035400      OR  UT-IDLEVNR = 'BP7YA'                                            
035500      OR  UT-IDLEVNR = '10121'                                            
035600      OR  UT-IDLEVNR = 'BP3EA'                                            
035700      OR  UT-IDLEVNR = '14562'                                            
035800      OR  UT-IDLEVNR = 'BP8BA')                                           
035900                                                                          
036000         UNSTRING IN-RFF-REFNO DELIMITED BY SPACE                         
036100                              INTO UT-IDSUPREF                            
036200*        IF  UT-IDSUPREF NOT NUMERIC                                      
036300*            MOVE 'IDSUPREF EJ NUMERISKT' TO FELTEXT-STR                  
036400*            MOVE JA                      TO FORM-FEL-SW                  
036500*        END-IF                                                           
036600         MOVE UT-IDSUPREF                 TO UTL-IDSUPREF                 
036700     END-IF                                                               
036800                                                                          
036900                                                                          
037000     IF  IN-RFF-QUAL = 'ON '                                              
037100         UNSTRING IN-RFF-REFNO DELIMITED BY SPACE                         
037200                                  INTO UT-IDORDNR7                        
037300         IF  UT-IDORDNR7  NOT NUMERIC                                     
037400             MOVE 'IDORDNR7  EJ NUMERISKT' TO FELTEXT-STR                 
037500             MOVE JA                      TO FORM-FEL-SW                  
037600         END-IF                                                           
037700         MOVE UT-IDORDNR7                 TO UTL-IDORDNR7                 
037800     END-IF                                                               
037900     .                                                                    
038000     EJECT                                                                
038100 F-NAD SECTION.                                                           
038200                                                                          
038300     IF  IN-NAD-QUAL = 'CZ '                                              
038400         UNSTRING IN-NAD-PARTY-ID DELIMITED BY SPACE                      
038500                                  INTO UT-IDLEVNR                         
038600         MOVE UT-IDLEVNR                 TO UTL-IDLEVNR                   
038700     END-IF                                                               
038800                                                                          
038900     IF  IN-NAD-QUAL = 'CN '                                              
039000         UNSTRING IN-NAD-PARTY-ID DELIMITED BY SPACE                      
039100                                  INTO WS-NAD-PARTY-ID-N                  
039200         MOVE WS-NAD-IDDISTR      TO UT-IDDISTR                           
039300         IF  UT-IDDISTR  NOT NUMERIC                                      
039400             MOVE 'IDDISTR EJ NUMERISKT' TO FELTEXT-STR                   
039500             MOVE JA                     TO FORM-FEL-SW                   
039600         END-IF                                                           
039700         MOVE UT-IDDISTR                 TO UTL-IDDISTR                   
039800                                                                          
039900         MOVE WS-NAD-IDKUNDNR     TO UT-IDKUNDNR                          
040000         IF  UT-IDKUNDNR NOT NUMERIC                                      
040100             MOVE 'IDKUNDNR EJ NUMERISKT' TO FELTEXT-STR                  
040200             MOVE JA                      TO FORM-FEL-SW                  
040300         END-IF                                                           
040400         MOVE UT-IDKUNDNR                TO UTL-IDKUNDNR                  
040500     END-IF                                                               
040600     .                                                                    
040700     EJECT                                                                
040800 G-LOC SECTION.                                                           
040900                                                                          
041000     CONTINUE                                                             
041100     .                                                                    
041200     EJECT                                                                
041300 H-CPS SECTION.                                                           
041400                                                                          
041500     IF  SKRIV-RAD                                                        
041600         PERFORM S11-SKRIV-W46356                                         
041700         PERFORM S31-BYGG-LOGG                                            
041800         PERFORM S12-SKRIV-W4635A                                         
041900         MOVE NEJ   TO SKRIV-RAD-SW                                       
042000     END-IF                                                               
042100                                                                          
042200     PERFORM S22-NOLLA-PAC                                                
042300     .                                                                    
042400     EJECT                                                                
042500 I-PAC SECTION.                                                           
042600                                                                          
042700     UNSTRING IN-PAC-TYPE-OF-PACK-ID DELIMITED BY SPACE                   
042800                                 INTO UT-KDEMBTYP                         
042900     IF  UT-KDEMBTYP NOT NUMERIC                                          
043000         MOVE 'KDEMBTYP EJ NUMERISKT' TO FELTEXT-STR                      
043100         MOVE JA                      TO FORM-FEL-SW                      
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
043500 J-MEA SECTION.                                                           
043600                                                                          
043700     IF  IN-MEA-APPL-QUAL = 'PD'                                          
043800     AND IN-MEA-MEASUR-DETAIL = 'AAB'                                     
043900         MOVE IN-MEA-VALUE               TO DEC-IDFRIDATA                 
044000         MOVE 6                          TO DEC-KVHELTAL                  
044100         MOVE 2                          TO DEC-KVDECIMAL                 
044200         CALL WDECEDIT USING DEC-WDECAREA                                 
044300         IF   DEC-KDSVAR-OK                                               
044400             MOVE DEC-IDEDITDATA      TO UT-VKORDBTO-KOLLI                
044500             ADD 0.1                  TO UT-VKORDBTO-KOLLI                
044600*                                                                         
044700*        Due to problems with only one decimal when creating              
044800*        Shipping documents we have to add 0.1 to the weight              
044900*                                                                         
045000         ELSE                                                             
045100             MOVE 'WKORDBTO-KOLLI FEL'   TO FELTEXT-STR                   
045200             MOVE JA                     TO FORM-FEL-SW                   
045300         END-IF                                                           
045400     END-IF                                                               
045500                                                                          
045600     IF  IN-MEA-APPL-QUAL = 'PD'                                          
045700     AND IN-MEA-MEASUR-DETAIL = 'LN '                                     
045800         MOVE IN-MEA-VALUE               TO DEC-IDFRIDATA                 
045900         MOVE 1                          TO DEC-KVHELTAL                  
046000         MOVE 2                          TO DEC-KVDECIMAL                 
046100         CALL WDECEDIT USING DEC-WDECAREA                                 
046200         IF   DEC-KDSVAR-OK                                               
046300             MOVE DEC-IDEDITDATA      TO WS-METER                         
046400             COMPUTE UT-DIKOLLIL = WS-METER                               
046500                                 * 100                                    
046600             END-COMPUTE                                                  
046700         ELSE                                                             
046800             MOVE 'DIKOLLIL FEL'   TO FELTEXT-STR                         
046900             MOVE JA               TO FORM-FEL-SW                         
047000         END-IF                                                           
047100     END-IF                                                               
047200                                                                          
047300     IF  IN-MEA-APPL-QUAL = 'PD'                                          
047400     AND IN-MEA-MEASUR-DETAIL = 'WD '                                     
047500         MOVE IN-MEA-VALUE               TO DEC-IDFRIDATA                 
047600         MOVE 1                          TO DEC-KVHELTAL                  
047700         MOVE 2                          TO DEC-KVDECIMAL                 
047800         CALL WDECEDIT USING DEC-WDECAREA                                 
047900         IF   DEC-KDSVAR-OK                                               
048000             MOVE DEC-IDEDITDATA      TO WS-METER                         
048100             COMPUTE UT-DIKOLLIB = WS-METER                               
048200                                 * 100                                    
048300             END-COMPUTE                                                  
048400         ELSE                                                             
048500             MOVE 'DIKOLLIB FEL'   TO FELTEXT-STR                         
048600             MOVE JA               TO FORM-FEL-SW                         
048700         END-IF                                                           
048800     END-IF                                                               
048900                                                                          
049000     IF  IN-MEA-APPL-QUAL = 'PD'                                          
049100     AND IN-MEA-MEASUR-DETAIL = 'HT '                                     
049200         MOVE IN-MEA-VALUE               TO DEC-IDFRIDATA                 
049300         MOVE 1                          TO DEC-KVHELTAL                  
049400         MOVE 2                          TO DEC-KVDECIMAL                 
049500         CALL WDECEDIT USING DEC-WDECAREA                                 
049600         IF   DEC-KDSVAR-OK                                               
049700             MOVE DEC-IDEDITDATA      TO WS-METER                         
049800             COMPUTE UT-DIKOLLIH = WS-METER                               
049900                                 * 100                                    
050000             END-COMPUTE                                                  
050100         ELSE                                                             
050200             MOVE 'DIKOLLIH FEL'   TO FELTEXT-STR                         
050300             MOVE JA               TO FORM-FEL-SW                         
050400         END-IF                                                           
050500     END-IF                                                               
050600                                                                          
050700     IF  IN-MEA-APPL-QUAL = 'PD'                                          
050800     AND IN-MEA-MEASUR-DETAIL = 'ABJ'                                     
050900         MOVE IN-MEA-VALUE               TO DEC-IDFRIDATA                 
051000         MOVE 4                          TO DEC-KVHELTAL                  
051100         MOVE 3                          TO DEC-KVDECIMAL                 
051200         CALL WDECEDIT USING DEC-WDECAREA                                 
051300         IF   DEC-KDSVAR-OK                                               
051400             MOVE DEC-IDEDITDATA      TO UT-VLORDBTO-KOLLI                
051500         ELSE                                                             
051600             MOVE 'WLORDBTO-KOLLI FEL'   TO FELTEXT-STR                   
051700             MOVE JA                     TO FORM-FEL-SW                   
051800         END-IF                                                           
051900     END-IF                                                               
052000     .                                                                    
052100     EJECT                                                                
052200 K-QTY SECTION.                                                           
052300                                                                          
052400     IF  IN-QTY-QUANTITY-QUAL = '12 '                                     
052500         UNSTRING IN-QTY-QUANTITY DELIMITED BY SPACE                      
052600                                 INTO UT-KVLEVART                         
052700         IF  UT-KVLEVART NOT NUMERIC                                      
052800             MOVE 'KVLEVART EJ NUMERISKT' TO FELTEXT-STR                  
052900             MOVE JA                      TO FORM-FEL-SW                  
053000         END-IF                                                           
053100         MOVE UT-KVLEVART                 TO UTL-KVANTAL                  
053200     END-IF                                                               
053300     .                                                                    
053400     EJECT                                                                
053500 L-PCI SECTION.                                                           
053600                                                                          
053700     CONTINUE                                                             
053800     .                                                                    
053900     EJECT                                                                
054000 M-GIR SECTION.                                                           
054100                                                                          
054200     UNSTRING IN-GIR-IDNO-1 DELIMITED BY SPACE                            
054300                              INTO UT-IDKOLLI                             
054400     IF  UT-IDKOLLI NOT NUMERIC                                           
054500         MOVE 'IDKOLLI EJ NUMERISKT' TO FELTEXT-STR                       
054600         MOVE JA                     TO FORM-FEL-SW                       
054700     END-IF                                                               
054800     MOVE UT-IDKOLLI                 TO UTL-IDKOLLI                       
054900*                                                                         
055000     MOVE NEJ                    TO UT-FLFEL                              
055100     MOVE 1                      TO WS-KOLLITAB-IX                        
055200     PERFORM UNTIL WS-KOLLITAB-IX > WS-KOLLITAB-ANT                       
055300                OR UT-FLFEL = JA                                          
055400       IF WS-RAPP-KOLLINR(WS-KOLLITAB-IX) = UT-IDKOLLI                    
055500         MOVE JA                 TO UT-FLFEL                              
055600       ELSE                                                               
055700         ADD +1                  TO WS-KOLLITAB-IX                        
055800       END-IF                                                             
055900     END-PERFORM                                                          
056000                                                                          
056100     IF  WS-KOLLITAB-IX > WS-KOLLITAB-ANT                                 
056200       ADD +1                   TO WS-KOLLITAB-ANT                        
056300       IF  WS-KOLLITAB-ANT > WS-KOLLITAB-MAX                              
056400         MOVE 'UTÖKA KOLLITAB ' TO FELTEXT-STR                            
056500         MOVE JA                TO FORM-FEL-SW                            
056600       ELSE                                                               
056700         MOVE WS-KOLLITAB-ANT   TO WS-KOLLITAB-IX                         
056800         MOVE UT-IDKOLLI        TO WS-RAPP-KOLLINR(WS-KOLLITAB-IX)        
056900       END-IF                                                             
057000     END-IF                                                               
057100*                                                                         
057200     .                                                                    
057300     EJECT                                                                
057400 N-LIN SECTION.                                                           
057500                                                                          
057600     IF  SKRIV-RAD                                                        
057700         PERFORM S11-SKRIV-W46356                                         
057800         PERFORM S31-BYGG-LOGG                                            
057900         PERFORM S12-SKRIV-W4635A                                         
058000         PERFORM S23-NOLLA-LIN                                            
058100     END-IF                                                               
058200                                                                          
058300     MOVE JA                         TO SKRIV-RAD-SW                      
058400                                                                          
058500     UNSTRING IN-LIN-LINENO DELIMITED BY SPACE                            
058600                              INTO UT-IDRADNR                             
058700     IF  UT-IDRADNR NOT NUMERIC                                           
058800         MOVE 'IDRADNR EJ NUMERISKT' TO FELTEXT-STR                       
058900         MOVE JA                     TO FORM-FEL-SW                       
059000     END-IF                                                               
059100     MOVE UT-IDRADNR                 TO UTL-IDRADNR                       
059200                                                                          
059300     UNSTRING IN-LIN-ITEMNO DELIMITED BY SPACE                            
059400                              INTO UT-IDARTNR                             
059500     IF  UT-IDARTNR NOT NUMERIC                                           
059600         MOVE 'IDARTNR EJ NUMERISKT' TO FELTEXT-STR                       
059700         MOVE JA                     TO FORM-FEL-SW                       
059800     END-IF                                                               
059900     MOVE UT-IDARTNR                 TO UTL-IDARTNR                       
060000     .                                                                    
060100     EJECT                                                                
060200 O-ALI SECTION.                                                           
060300                                                                          
060400     UNSTRING IN-ALI-COUNTRY-OF-ORIG DELIMITED BY SPACE                   
060500                                     INTO UT-KDARTURS                     
060600     .                                                                    
060700     EJECT                                                                
060800 P-UNT SECTION.                                                           
060900                                                                          
061000     CONTINUE                                                             
061100     .                                                                    
061200     EJECT                                                                
061300 Q-UNZ SECTION.                                                           
061400                                                                          
061500     CONTINUE                                                             
061600     .                                                                    
061700     EJECT                                                                
061800 R-UNB SECTION.                                                           
061900                                                                          
062000     IF IN-UNB-DATE > 501231                                              
062100       COMPUTE UTL-DASNDDAT = 19000000 + IN-UNB-DATE                      
062200     ELSE                                                                 
062300       COMPUTE UTL-DASNDDAT = 20000000 + IN-UNB-DATE                      
062400     END-IF                                                               
062500                                                                          
062600     MOVE IN-UNB-TIME                TO UTL-TISNDTID                      
062700     .                                                                    
062800     EJECT                                                                
062900 Z-FINIT SECTION.                                                         
063000                                                                          
063100     IF  SKRIV-RAD                                                        
063200         PERFORM S11-SKRIV-W46356                                         
063300         PERFORM S31-BYGG-LOGG                                            
063400         PERFORM S12-SKRIV-W4635A                                         
063500     END-IF                                                               
063600                                                                          
063700     CLOSE W46355                                                         
063800           W46356                                                         
063900           W4635A                                                         
064000     SKIP2                                                                
064100     MOVE 'S' TO POSTSUM-OPKOD                                            
064200     CALL POSTSUM USING POSTSUM-PARM                                      
064300     .                                                                    
064400     EJECT                                                                
064500 S01-LAES-W46355  SECTION.                                                
064600     READ W46355 INTO IN-AREA                                             
064700     AT END                                                               
064800        MOVE HIGH-VALUE   TO IN-AREA                                      
064900        SET END-OF-W46355 TO TRUE                                         
065000                                                                          
065100     NOT AT END                                                           
065200        MOVE 'W46355'   TO POSTSUM-FDNAMN                                 
065300        MOVE 'W46355D1' TO POSTSUM-DDNAMN2                                
065400        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
065500        CALL POSTSUM USING POSTSUM-PARM                                   
065600     END-READ                                                             
065700     .                                                                    
065800     EJECT                                                                
065900 S11-SKRIV-W46356 SECTION.                                                
066000                                                                          
066100     WRITE UT-POST FROM UT-AREA                                           
066200                                                                          
066300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
066400     MOVE 'W46356'   TO POSTSUM-FDNAMN                                    
066500     MOVE 'W46355D2' TO POSTSUM-DDNAMN2                                   
066600     CALL POSTSUM USING POSTSUM-PARM                                      
066700     .                                                                    
066800     EJECT                                                                
066900 S12-SKRIV-W4635A SECTION.                                                
067000                                                                          
067100     WRITE UT-LOGG FROM UTL-W46341                                        
067200                                                                          
067300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
067400     MOVE 'W4635A'   TO POSTSUM-FDNAMN                                    
067500     MOVE 'W46355D3' TO POSTSUM-DDNAMN2                                   
067600     CALL POSTSUM USING POSTSUM-PARM                                      
067700     .                                                                    
067800     EJECT                                                                
067900 S21-NOLLA-BGM SECTION.                                                   
068000                                                                          
068100     MOVE ZERO              TO UT-IDPRODNR                                
068200                               UT-IDDISTR                                 
068300                               UT-IDKUNDNR                                
068400                               UT-IDORDNR7                                
068500                               UT-IDKOLLI                                 
068600                               UT-DASUPREF                                
068700                               UT-TISUPTID                                
068800                               UT-VKORDBTO-KOLLI                          
068900                               UT-KDEMBTYP                                
069000                               UT-DIKOLLIL                                
069100                               UT-DIKOLLIB                                
069200                               UT-DIKOLLIH                                
069300                               UT-IDRADNR                                 
069400                               UT-IDARTNR                                 
069500                               UT-KVLEVART                                
069600                               UT-VLORDBTO-KOLLI                          
069700     MOVE SPACE             TO UT-IDSUPREF                                
069800                               UT-KDARTURS                                
069900                               UT-IDLEVNR                                 
070000     .                                                                    
070100     EJECT                                                                
070200 S22-NOLLA-PAC SECTION.                                                   
070300                                                                          
070400     MOVE ZERO              TO UT-IDORDNR7                                
070500                               UT-IDKOLLI                                 
070600                               UT-VKORDBTO-KOLLI                          
070700                               UT-KDEMBTYP                                
070800                               UT-DIKOLLIL                                
070900                               UT-DIKOLLIB                                
071000                               UT-DIKOLLIH                                
071100                               UT-IDRADNR                                 
071200                               UT-IDARTNR                                 
071300                               UT-KVLEVART                                
071400                               UT-VLORDBTO-KOLLI                          
071500     MOVE SPACE             TO UT-KDARTURS                                
071600     .                                                                    
071700     EJECT                                                                
071800 S23-NOLLA-LIN SECTION.                                                   
071900                                                                          
072000     MOVE ZERO              TO UT-IDRADNR                                 
072100                               UT-IDARTNR                                 
072200                               UT-KVLEVART                                
072300                               UT-IDORDNR7                                
072400     MOVE SPACE             TO UT-KDARTURS                                
072500     .                                                                    
072600     EJECT                                                                
072700 S31-BYGG-LOGG SECTION.                                                   
072800     MOVE 'DES'                   TO UTL-IDPTYP                           
072900     MOVE WS-DAGENS-DATUM-8       TO UTL-DAREGDAT                         
073000     MOVE WS-DAGENS-KLOCKA-1-6    TO UTL-TIREGTID                         
073100     MOVE SPACE                   TO UTL-IDDC                             
073200                                     UTL-BERADREF                         
073300                                     UTL-KDVIA                            
073400     MOVE ZERO                    TO UTL-DABEKDAT                         
073500                                     UTL-DAFAKT                           
073600                                     UTL-DALEVDAT                         
073700                                     UTL-DAPACKN                          
073800                                     UTL-DASKEPPN                         
073900                                     UTL-TIBEKR                           
074000                                     UTL-TIPACTID                         
074100                                     UTL-KDORDBEK                         
074200                                     UTL-KDORDKL                          
074300     .                                                                    
074400     EJECT                                                                
074500 S99-ABEND SECTION.                                                       
074600                                                                          
074700     SKIP2                                                                
074800     MOVE 'S' TO POSTSUM-OPKOD                                            
074900     CALL POSTSUM USING POSTSUM-PARM                                      
075000     CALL ABEND USING RKOD-ABEND-MED-DUMP                                 
075100     .                                                                    
