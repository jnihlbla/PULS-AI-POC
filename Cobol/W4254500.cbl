000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4254500.                                                
000400 AUTHOR.         LARS THELL      (MG).                                    
000500 DATE-WRITTEN.   91/01/31.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        KOLLAR MOT WDB2 VILKA DISTRIKT SOM SKALL FÅ VR TRANSAR           
001100*        PROGRAMMET LÄSER     WLGMTA (WDB2)                               
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . . RETURKOD FRÅN SORTERING                         
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- VR TRANSAR IN                                              
002600     SELECT W42540                     ASSIGN TO W42545D1.                
002700     SKIP2                                                                
002800*          --- VR TRANSAR UT                                              
002900     SELECT W42545                     ASSIGN TO W42545D2.                
003000     SKIP2                                                                
003100*          --- SORTERINGSFIL                                              
003200     SELECT SORTFIL                    ASSIGN TO W42545DS.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W42540                                                               
003900     LABEL RECORD    STANDARD                                             
004000     RECORDING       V                                                    
004100     BLOCK CONTAINS  0.                                                   
004200     SKIP2                                                                
004300*01  -COPY W425687         -L.                                            
004400                                                                          
004500                                                                          
004600     SKIP3                                                                
004700 FD  W42545                                                               
004800     LABEL RECORD    STANDARD                                             
004900     RECORDING       V                                                    
005000     BLOCK CONTAINS  0.                                                   
005100     SKIP2                                                                
005200*01  POST -COPY W425687    -PRE  W42545-  -L.                             
005300                                                                          
005400     SKIP3                                                                
005500 SD  SORTFIL                                                              
005600     RECORDING       F.                                                   
005700     SKIP2                                                                
005800 01  SORT-POST.                                                           
005900    03   SORT-IDDISTR            PIC  9(5).                               
006000*03  -COPY W425687         -L.                                            
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300     SKIP2                                                                
006400                                                                          
006500*    -- CHECKED BY WY2000                                                 
006600 77  IDPGM                       PIC X(8)    VALUE 'W4254500'.            
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900 77  W-OLD-IDDISTR               PIC 9(5).                                
007000 77  W-OLD-IDKUNDNR              PIC 9(5).                                
007100                                                                          
007200 77  W42540-EOF-SW               PIC X       VALUE 'N'.                   
007300     88  END-OF-W42540                       VALUE 'J'.                   
007400                                                                          
007500 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
007600     88  END-OF-SORTFIL                      VALUE 'J'.                   
007700     EJECT                                                                
007800 01  DAGENS-DATUM.                                                        
007900     03  DAGENS-DATUM-AAR        PIC X(2)    VALUE SPACE.                 
008000     03  DAGENS-DATUM-MAANAD     PIC X(2)    VALUE SPACE.                 
008100     03  DAGENS-DATUM-DAG        PIC X(2)    VALUE SPACE.                 
008200     EJECT                                                                
008300****************************************************************          
008400*                DISTRIKTSTEST-COPYTEXT                                   
008500****************************************************************          
008600*                                                                         
008700 01  FILLER         PIC X(16)    VALUE 'DISTRIKTSAREA   '.                
008800     SKIP2                                                                
008900*01  -COPY WWDIS116                                                       
009000     EJECT                                                                
009100 01  TEST-IDDISTR   PIC 9(5)    COMP-3.                                   
009200     SKIP2                                                                
009300*01  FILLER        -COPY WWDIST59   -RED TEST-IDDISTR.                    
009400     EJECT                                                                
009500 01  DYNAMISKA-SUBPROGRAM.                                                
009600*                                                                         
009700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010100     SKIP2                                                                
010200*    --- PARAMETRAR TILL ABEND                                            
010300                                                                          
010400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL POSTSUM                                          
010800*                                                                         
010900*01  -COPY W0005      -PRE  POSTSUM-                                      
011000     EJECT                                                                
011100 01  W42540-AREA-START           PIC X(24)   VALUE                        
011200                                 'W42540-AREA-START  '.                   
011300     SKIP2                                                                
011400 01  W42540-AREA.                                                         
011500     03  W42540-IDPTYP           PIC X(3).                                
011600     03  FILLER                  PIC X(200).                              
011700     SKIP2                                                                
011800*01  -COPY W425685      -PRE W685-                                        
011900     EJECT                                                                
012000*01  -COPY W425686      -PRE W686-                                        
012100     EJECT                                                                
012200*01  -COPY W425687      -PRE W687-                                        
012300     EJECT                                                                
012400*01  -COPY W425688      -PRE W688-                                        
012500     EJECT                                                                
012600*01  -COPY W425SU1      -PRE SU1-                                         
012700     EJECT                                                                
012800*01  -COPY W425SU2      -PRE SU2-                                         
012900     EJECT                                                                
013000*01  -COPY W425SU3      -PRE SU3-                                         
013100     EJECT                                                                
013200*01  -COPY W425SU4      -PRE SU4-                                         
013300     EJECT                                                                
013400*01  -COPY W425SU5      -PRE SU5-                                         
013500     EJECT                                                                
013600*01  -COPY W425SUA      -PRE SUA-                                         
013700     EJECT                                                                
013800*01  -COPY W425SUC      -PRE SUC-                                         
013900     EJECT                                                                
014000*01  -COPY W425SUD      -PRE SUD-                                         
014100     EJECT                                                                
014200 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
014300                                  'SORTWS-AREA-START  '.                  
014400     SKIP2                                                                
014500 01  SORTWS-AREA.                                                         
014600     03  SORTWS-IDDISTR          PIC 9(5).                                
014700     03  SORTWS-IDKUNDNR         PIC 9(7).                                
014800     03  SORTWS-VRPOST.                                                   
014900      05 SORTWS-IDPTYP           PIC X(3).                                
015000      05 FILLER                  PIC X(204).                              
015100     EJECT                                                                
015200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015300*                                                                         
015400     EJECT                                                                
015500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015600     SKIP3                                                                
015700 01  NYCKLAR-TILL-DLI.                                                    
015800     03  W-IDGMT-X.                                                       
015900         05  W-IDDISTR    PIC S9(5)   VALUE ZERO COMP-3.                  
016000         05  W-IDKUNDNR   PIC S9(7)   VALUE ZERO COMP-3.                  
016100     SKIP2                                                                
016200*    --- STATUS-KOD FRÅN IMS                                              
016300 01  STATUS-WS                   PIC XX.                                  
016400     88  SEGMENT-FINNS                       VALUE '  '.                  
016500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016700     SKIP2                                                                
016800 01  GODK-STATUSKODER.                                                    
016900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017000     SKIP3                                                                
017100 01  SSA1                        PIC X(64).                               
017200 01  SSA2                        PIC X(64).                               
017300     EJECT                                                                
017400*    --- IMS FUNKTIONSKODER                                               
017500*01  -COPY W0003                                                          
017600     EJECT                                                                
017700*    ---  DLI INPUT-OUTPUT AREA                                           
017800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017900     SKIP3                                                                
018000 01  DLI-IO-AREA.                                                         
018100     03  WLGMTA01.                                                        
018200*        05  -COPY WDB201                                                 
018300     EJECT                                                                
018400 LINKAGE SECTION.                                                         
018500                                                                          
018600     EJECT                                                                
018700*01  -COPY W0008      -PRE GMTA-                                          
018800     05  FILLER                  PIC X.                                   
018900     EJECT                                                                
019000 PROCEDURE DIVISION  USING GMTA-PCB.                                      
019100     ENTRY 'DLITCBL' USING GMTA-PCB.                                      
019200                                                                          
019300     SKIP2                                                                
019400     PERFORM A-INIT                                                       
019500                                                                          
019600     MOVE -150000 TO SORT-CORE-SIZE                                       
019700     SORT SORTFIL ASCENDING KEY SORT-IDDISTR                              
019800                  INPUT PROCEDURE B-BEHANDLA-INFIL                        
019900                  OUTPUT PROCEDURE C-KOLLA-IDDISTR-SKRIV-UTFIL            
020000                                                                          
020100     IF SORT-RETURN NOT = 0                                               
020200       DISPLAY 'RETURKOD ' SORT-RETURN ' FRÅN SORT'                       
020300       PERFORM S99-ABEND                                                  
020400     ELSE                                                                 
020500       PERFORM Z-FINIT                                                    
020600                                                                          
020700       MOVE ZERO TO RETURN-CODE                                           
020800       GOBACK                                                             
020900     END-IF                                                               
021000                                                                          
021100     .                                                                    
021200     EJECT                                                                
021300 A-INIT SECTION.                                                          
021400                                                                          
021500     OPEN INPUT  W42540                                                   
021600                                                                          
021700     OPEN OUTPUT W42545                                                   
021800     SKIP2                                                                
021900     ACCEPT DAGENS-DATUM        FROM DATE                                 
022000     MOVE IDPGM                 TO POSTSUM-PROGNAMN                       
022100     .                                                                    
022200     EJECT                                                                
022300 B-BEHANDLA-INFIL   SECTION.                                              
022400                                                                          
022500     PERFORM S01-LAES-W42540                                              
022600     PERFORM UNTIL END-OF-W42540                                          
022700         PERFORM BA-KOLLA-IDPTYP                                          
022800         PERFORM BB-KONVERTERA-DISTRIKT                                   
022900         PERFORM S31-SORT-RELEASE                                         
023000         PERFORM S01-LAES-W42540                                          
023100     END-PERFORM                                                          
023200     .                                                                    
023300     EJECT                                                                
023400 BA-KOLLA-IDPTYP    SECTION.                                              
023500                                                                          
023600     EVALUATE TRUE                                                        
023700                                                                          
023800     WHEN W42540-IDPTYP        =   '685'                                  
023900         MOVE W42540-AREA      TO  W685-W425685                           
024000         MOVE W685-IDDISTR     TO  SORTWS-IDDISTR                         
024100         MOVE W685-IDKUNDNR    TO  SORTWS-IDKUNDNR                        
024200         MOVE W685-W425685     TO  SORTWS-VRPOST                          
024300                                                                          
024400     WHEN W42540-IDPTYP        =   '686'                                  
024500         MOVE W42540-AREA      TO  W686-W425686                           
024600         MOVE W686-IDDISTR     TO  SORTWS-IDDISTR                         
024700         MOVE W686-IDKUNDNR    TO  SORTWS-IDKUNDNR                        
024800         MOVE W686-W425686     TO  SORTWS-VRPOST                          
024900                                                                          
025000     WHEN W42540-IDPTYP        =   '687'                                  
025100         MOVE W42540-AREA      TO  W687-W425687                           
025200         MOVE W687-IDDISTR     TO  SORTWS-IDDISTR                         
025300         MOVE W687-IDKUNDNR    TO  SORTWS-IDKUNDNR                        
025400         MOVE W687-W425687     TO  SORTWS-VRPOST                          
025500                                                                          
025600     WHEN W42540-IDPTYP        =   '688'                                  
025700         MOVE W42540-AREA      TO  W688-W425688                           
025800         MOVE W688-IDDISTR     TO  SORTWS-IDDISTR                         
025900         MOVE W688-IDKUNDNR    TO  SORTWS-IDKUNDNR                        
026000         MOVE W688-W425688     TO  SORTWS-VRPOST                          
026100                                                                          
026200     WHEN W42540-IDPTYP        =   'SU1'                                  
026300         MOVE W42540-AREA      TO  SU1-W425SU1-CTX                        
026400         MOVE SU1-IDDISTR      TO  SORTWS-IDDISTR                         
026500         MOVE SU1-IDKUNDNR     TO  SORTWS-IDKUNDNR                        
026600         MOVE SU1-W425SU1-CTX  TO  SORTWS-VRPOST                          
026700                                                                          
026800     WHEN W42540-IDPTYP        =   'SU2'                                  
026900         MOVE W42540-AREA      TO  SU2-W425SU2-CTX                        
027000         MOVE SU2-IDDISTR      TO  SORTWS-IDDISTR                         
027100         MOVE SU2-IDKUNDNR     TO  SORTWS-IDKUNDNR                        
027200         MOVE SU2-W425SU2-CTX  TO  SORTWS-VRPOST                          
027300                                                                          
027400     WHEN W42540-IDPTYP        =   'SU3'                                  
027500         MOVE W42540-AREA      TO  SU3-W425SU3-CTX                        
027600         MOVE SU3-IDDISTR      TO  SORTWS-IDDISTR                         
027700         MOVE SU3-IDKUNDNR     TO  SORTWS-IDKUNDNR                        
027800         MOVE SU3-W425SU3-CTX  TO  SORTWS-VRPOST                          
027900                                                                          
028000     WHEN W42540-IDPTYP        =   'SU4'                                  
028100         MOVE W42540-AREA      TO  SU4-W425SU4-CTX                        
028200         MOVE SU4-IDDISTR      TO  SORTWS-IDDISTR                         
028300         MOVE SU4-IDKUNDNR     TO  SORTWS-IDKUNDNR                        
028400         MOVE SU4-W425SU4-CTX  TO  SORTWS-VRPOST                          
028500                                                                          
028600     WHEN W42540-IDPTYP        =   'SU5'                                  
028700         MOVE W42540-AREA      TO  SU5-W425SU5                            
028800         MOVE SU5-IDDISTR      TO  SORTWS-IDDISTR                         
028900         MOVE SU5-IDKUNDNR     TO  SORTWS-IDKUNDNR                        
029000         MOVE SU5-W425SU5      TO  SORTWS-VRPOST                          
029100                                                                          
029200     WHEN W42540-IDPTYP        =   'SUA'                                  
029300         MOVE W42540-AREA      TO  SUA-W425SUA-CTX                        
029400         MOVE SUA-IDDISTR      TO  SORTWS-IDDISTR                         
029500         MOVE SUA-IDKUNDNR     TO  SORTWS-IDKUNDNR                        
029600         MOVE SUA-W425SUA-CTX  TO  SORTWS-VRPOST                          
029700                                                                          
029800     WHEN W42540-IDPTYP        =   'SUC'                                  
029900         MOVE W42540-AREA      TO  SUC-W425SUC-CTX                        
030000         MOVE SUC-IDDISTR      TO  SORTWS-IDDISTR                         
030100         MOVE SUC-IDKUNDNR     TO  SORTWS-IDKUNDNR                        
030200         MOVE SUC-W425SUC-CTX  TO  SORTWS-VRPOST                          
030300                                                                          
030400     WHEN W42540-IDPTYP        =   'SUD'                                  
030500         MOVE W42540-AREA      TO  SUD-W425SUD-CTX                        
030600         MOVE SUD-IDDISTR      TO  SORTWS-IDDISTR                         
030700         MOVE SUD-IDKUNDNR     TO  SORTWS-IDKUNDNR                        
030800         MOVE SUD-W425SUD-CTX  TO  SORTWS-VRPOST                          
030900                                                                          
031000     END-EVALUATE                                                         
031100     .                                                                    
031200     EJECT                                                                
031300 BB-KONVERTERA-DISTRIKT       SECTION.                                    
031400                                                                          
031500     MOVE SORTWS-IDDISTR                  TO TEST-IDDISTR                 
031600     EVALUATE TRUE                                                        
031700                                                                          
031800     WHEN DIST59-DANMARK-VEST                                             
031900          MOVE DIS116-DISTR-DANM-VEST     TO SORTWS-IDDISTR               
032000                                                                          
032100     WHEN DIST59-DANMARK-OST                                              
032200          MOVE DIS116-DISTR-DANM-OST      TO SORTWS-IDDISTR               
032300                                                                          
032400     WHEN DIST59-NORGE                                                    
032500          MOVE DIS116-DISTR-NORGE         TO SORTWS-IDDISTR               
032600                                                                          
032700     WHEN DIST59-SVERIGE                                                  
032800          MOVE DIS116-DISTR-SVERIGE       TO SORTWS-IDDISTR               
032900                                                                          
033000     WHEN DIST59-PORTUGAL                                                 
033100          MOVE DIS116-DISTR-PORTUGAL      TO SORTWS-IDDISTR               
033200                                                                          
033300     WHEN DIST59-GREKLAND                                                 
033400          MOVE DIS116-DISTR-GREKLAND      TO SORTWS-IDDISTR               
033500                                                                          
033600     WHEN DIST59-TJECKIEN                                                 
033700          MOVE DIS116-DISTR-TJECKIEN      TO SORTWS-IDDISTR               
033800                                                                          
033900     WHEN DIST59-SLOVENIEN                                                
034000          MOVE DIS116-DISTR-SLOVENIEN     TO SORTWS-IDDISTR               
034100                                                                          
034200     WHEN DIST59-ITALIEN                                                  
034210          MOVE DIS116-DISTR-ITALIEN       TO SORTWS-IDDISTR               
034220                                                                          
034230     WHEN DIST59-SCHWEIZ                                                  
034240          MOVE DIS116-DISTR-SCHWEIZ       TO SORTWS-IDDISTR               
034250                                                                          
034260     WHEN DIST59-FRANKRIKE                                                
034270          MOVE DIS116-DISTR-FRANKRIKE     TO SORTWS-IDDISTR               
034280                                                                          
034290     WHEN DIST59-FINLAND                                                  
034291          MOVE DIS116-DISTR-FINLAND       TO SORTWS-IDDISTR               
034292                                                                          
034293     WHEN DIST59-ENGLAND                                                  
034294          MOVE DIS116-DISTR-ENGLAND       TO SORTWS-IDDISTR               
034295                                                                          
034296     WHEN DIST59-IRLAND                                                   
034297          MOVE DIS116-DISTR-IRLAND        TO SORTWS-IDDISTR               
034298                                                                          
           WHEN DIST59-BRAZIL                                                   
                MOVE DIS116-DISTR-BRAZIL        TO SORTWS-IDDISTR               
034298                                                                          
034300     END-EVALUATE                                                         
034400     .                                                                    
034500     EJECT                                                                
034600 C-KOLLA-IDDISTR-SKRIV-UTFIL  SECTION.                                    
034700                                                                          
034800     MOVE ZERO                 TO  W-OLD-IDDISTR                          
034900                                   W-OLD-IDKUNDNR                         
035000     PERFORM S32-SORT-RETURN                                              
035100     PERFORM UNTIL END-OF-SORTFIL                                         
035200         IF SORTWS-IDDISTR     =   W-OLD-IDDISTR                          
035300           AND SORTWS-IDKUNDNR =   W-OLD-IDKUNDNR                         
035400             CONTINUE                                                     
035500          ELSE                                                            
035600             MOVE SORTWS-IDDISTR  TO  W-IDDISTR                           
035700             MOVE SORTWS-IDKUNDNR TO  W-IDKUNDNR                          
035800             PERFORM IMS-GU-GMTA01                                        
035900             MOVE SORTWS-IDDISTR  TO  W-OLD-IDDISTR                       
036000             MOVE SORTWS-IDKUNDNR  TO  W-OLD-IDKUNDNR                     
036100         END-IF                                                           
036200                                                                          
036300         IF GMT-FLVR = JA                                                 
036400             PERFORM S11-SKRIV-W42545                                     
036500         END-IF                                                           
036600         PERFORM S32-SORT-RETURN                                          
036700     END-PERFORM                                                          
036800     .                                                                    
036900     EJECT                                                                
037000 Z-FINIT SECTION.                                                         
037100     CLOSE W42540                                                         
037200           W42545                                                         
037300     SKIP2                                                                
037400     MOVE 'S' TO POSTSUM-OPKOD                                            
037500     CALL POSTSUM USING POSTSUM-PARM                                      
037600     .                                                                    
037700     EJECT                                                                
037800 S01-LAES-W42540  SECTION.                                                
037900     SKIP2                                                                
038000     READ W42540 INTO W42540-AREA                                         
038100     AT END                                                               
038200        SET END-OF-W42540 TO TRUE                                         
038300                                                                          
038400     NOT AT END                                                           
038500        MOVE 'W42540'          TO POSTSUM-FDNAMN                          
038600        MOVE 'W42545D1'        TO POSTSUM-DDNAMN2                         
038700        MOVE W42540-IDPTYP     TO POSTSUM-TRANSTYP                        
038800        CALL POSTSUM           USING POSTSUM-PARM                         
038900     END-READ                                                             
039000     .                                                                    
039100     EJECT                                                                
039200 S11-SKRIV-W42545 SECTION.                                                
039300     SKIP2                                                                
039400     WRITE W42545-POST FROM SORTWS-VRPOST                                 
039500                                                                          
039600     MOVE SORTWS-IDPTYP        TO POSTSUM-TRANSTYP                        
039700     MOVE 'W42545'             TO POSTSUM-FDNAMN                          
039800     MOVE 'W42545D2'           TO POSTSUM-DDNAMN2                         
039900     CALL POSTSUM USING        POSTSUM-PARM                               
040000     .                                                                    
040100     EJECT                                                                
040200 S31-SORT-RELEASE  SECTION.                                               
040300     SKIP2                                                                
040400     RELEASE SORT-POST FROM SORTWS-AREA                                   
040500     .                                                                    
040600     EJECT                                                                
040700 S32-SORT-RETURN  SECTION.                                                
040800     SKIP2                                                                
040900     RETURN SORTFIL INTO SORTWS-AREA                                      
041000     AT END                                                               
041100         SET END-OF-SORTFIL TO TRUE                                       
041200     .                                                                    
041300     EJECT                                                                
041400 S99-ABEND SECTION.                                                       
041500     SKIP2                                                                
041600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
041700     .                                                                    
041800     EJECT                                                                
041900* --- IMS SEKTIONER ---                                                   
042000     SKIP3                                                                
042100     EJECT                                                                
042200 IMS-GU-GMTA01 SECTION.                                                   
042300     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-X ')'                           
042400          DELIMITED BY SIZE INTO SSA1                                     
042500     MOVE '  GE' TO GODK-STATUSKODER                                      
042600     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA SSA1                      
042700     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
042800     PERFORM IMS-STATUSKONTROLL                                           
042900     .                                                                    
043000     EJECT                                                                
043100 IMS-STATUSKONTROLL SECTION.                                              
043200     SKIP2                                                                
043300     SET STATUS-IX TO 1                                                   
043400     SEARCH GODK-STATUS                                                   
043500       AT END CALL FELLOG                                                 
043600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
043700     END-SEARCH                                                           
043800     .                                                                    
