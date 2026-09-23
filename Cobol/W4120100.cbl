000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W4120100.                                        
000300 AUTHOR.                 KERSTIN JOHANSSON  GUIDE DATAKONSULT AB          
000400 DATE-WRITTEN.           OKT 1990.                                        
000500                                                                          
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*            KONVERTERING AV VR OCH ÖVRIGA ORDERPOSTER.                   
001000*            INPUTPOSTERNA SORTERAS FÖRST PÅ IDDISTR, IDKUNDNR,           
001100*            IDORDNR, IDTYP.                                              
001200*            VARJE POST KONVERTERAS TILL W412 FORMAT FÖR ATT              
001300*            SENARE SKRIVAS PÅ KOMMUNIKATIONS DB I PGM W41202.            
001400*            POSTTYP R50, R51, R52, R53, R55, R75, VG4, RX3, RX5,         
001500*            RX7 (VR FRÅN VIPS),EXT, TS2 - TS6 OCH DR5 BEHANDLAS.         
001600*            ORDERHUVUD SKAPAS FÖR VR RX5 RX7 VG4 TS2 - TS5 RX3           
001700*            TPO:ER EX1 OCH DR5 POSTER ( ÖVERLEVERANSER ).                
001800*            DATUM OCH TID SÄTTS PÅ POSTERNA. TID ÄR TTMMSSTH.SSTH        
001900*            ANVÄNDS SOM EN RÄKNARE FÖR ATT GÖRA DATUM, TID UNIKT         
002000*            FÖR VARJE ORDER. SSTH RÄKNAS UPP FÖR VARJE NY ORDER.         
002100*            FELAKTIGA POSTER SKRIVS PÅ EN FELFIL                         
002200*                                                                         
002300*            NY FUNKTION: ANNULLATION AV ORDERRAD I TACDIS.               
002400*                                                                         
002500*                                                                         
002600*    INPUT:  TRANSAR FRÅN VR OCH ÖVRIGA                                   
002700*            W411500     POSTTYP R50            ORDERHUVUD                
002800*            W411510             R51            NAMN                      
002900*            W411520             R52            ADRESS                    
003000*            W411530             R53            DO / TPO                  
003100*            W411550             R55            ORDERRAD                  
003200*            W411575             R75            FLERA ORDERRADER          
003300*            W463VG4             VG4            RAD + HUVUD               
003400*            W412TS2             TS2-TS5        RAD + HUVUD               
003500*            W412TS2             TS6            ANNULLATIONSRAD           
003600*            W412EX1             EXT            RAD + HUVUD               
003700*            W412RX3             RX3            TPO RAD + HUVUD           
003800*            W412RX5             RX5            RAD + HUVUD               
003900*            W412RX7             RX7            RAD + HUVUD               
004000*            W418DR5             DR5            RAD + HUVUD               
004100*                                                                         
004200*    OUTPUT: ORDERPOSTER                                                  
004300*            W412500     POSTTYP R50            ORDERHUVUD                
004400*            W412550             R55            ORDERRAD                  
004500*                                                                         
004600*            ANNULLATIONSPOSTER                                           
004700*                                                                         
004800*            FELAKTIGA POSTER    POSTTYP SOM PÅ INPUTFIL                  
004900*            PROGRAMMET LÄSER             (WDB2)                          
005000*                                                                         
005100*    ABENDKODER:                                                          
005200*            U0999      - FELLOG                                          
005300     EJECT                                                                
005400 ENVIRONMENT DIVISION.                                                    
005500                                                                          
005600 INPUT-OUTPUT SECTION.                                                    
005700                                                                          
005800 FILE-CONTROL.                                                            
005900                                                                          
006000* INFIL:  POSTER FRÅN VR OCH ÖVR                                          
006100     SELECT  W41201                   ASSIGN TO    W41201D1.              
006200* UTFIL:  GODKÄNDA POSTER                                                 
006300     SELECT  W41202                   ASSIGN TO    W41201D2.              
006400* FELAKTIGA POSTER                                                        
006500     SELECT  W41203                   ASSIGN TO    W41201D3.              
006600* ANNULLATIONSPOSTER                                                      
006700     SELECT  W41209                   ASSIGN TO    W41201D4.              
006800* SORT                                                                    
006900     SELECT  SORTFIL                  ASSIGN TO    W41201DS.              
007000     EJECT                                                                
007100 DATA DIVISION.                                                           
007200                                                                          
007300 FILE SECTION.                                                            
007400                                                                          
007500 FD  W41201                                                               
007600     RECORDING      V                                                     
007700     BLOCK CONTAINS 0.                                                    
007800                                                                          
007900*01   -COPY W463VG4 -L                                                    
008000*01   -COPY W411500 -L                                                    
008100*01   -COPY W412RX5 -L                                                    
008200*01   -COPY W412RX7 -L                                                    
008300*01   -COPY W412RX3 -L                                                    
008400*01   -COPY W412TS2 -L                                                    
008500*01   -COPY W412EX1 -L                                                    
008600*01   -COPY W418DR5 -L                                                    
008700                                                                          
008800 FD  W41202                                                               
008900     LABEL RECORD STANDARD                                                
009000     RECORDING      V                                                     
009100     BLOCK CONTAINS 0.                                                    
009200                                                                          
009300*01  W412500   -COPY W412500 -PRE W41202-  -L.                            
009400                                                                          
009500*01  W412550   -COPY W412550 -PRE W41202-  -L.                            
009600                                                                          
009700 FD  W41203                                                               
009800     LABEL RECORD STANDARD                                                
009900     RECORDING      V                                                     
010000     BLOCK CONTAINS 0.                                                    
010100                                                                          
010200 01  FELPOST        PIC X(168).                                           
010300                                                                          
010400     EJECT                                                                
010500 FD  W41209                                                               
010600     LABEL RECORD STANDARD                                                
010700     RECORDING      V                                                     
010800     BLOCK CONTAINS 0.                                                    
010900                                                                          
011000*01  ANNUPOST  -COPY W41254    -L.                                        
011100                                                                          
011200     EJECT                                                                
011300                                                                          
011400 SD  SORTFIL.                                                             
011500                                                                          
011600 01  SORTPOST.                                                            
011700  03 SORT-SORTPOST               PIC X(334).                              
011800  03 SORT-IDDISTR                PIC X(4).                                
011900  03 SORT-IDKUNDNR               PIC X(6).                                
012000  03 SORT-IDORDNR                PIC X(7).                                
012100  03 SORT-IDTYP                  PIC X(3).                                
012200     EJECT                                                                
012300 WORKING-STORAGE SECTION.                                                 
012400 77  IDPGM                       PIC X(8)    VALUE 'W4120100'.            
012500 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
012600********* 77  FELTEXT                     PIC X(80)   VALUE SPACE.        
012700                                                                          
012800 01  WORK-AREA-START             PIC X(20)   VALUE 'WORK-START  '.        
012900 01  WORK-AREA.                                                           
013000  03 JA                          PIC X       VALUE 'J'.                   
013100  03 NEJ                         PIC X       VALUE 'N'.                   
013200  03 W41201-EOF                  PIC X       VALUE 'N'.                   
013300  03 SORTFIL-EOF                 PIC X       VALUE 'N'.                   
013400  03 WS-KVDAGAR-RFS-DEF          PIC S9(3)   COMP-3.                      
013500                                                                          
013600 77  RFS-IX                      PIC S9(4)   COMP SYNC VALUE ZERO.        
013700 77  MAX-RFS-IX                  PIC S9(4)   COMP SYNC VALUE +4.          
013800                                                                          
013900 01  SW-DOSKALLE                 PIC X       VALUE 'N'.                   
014000     88 DOSKALLE-JA              VALUE 'J'.                               
014100     88 DOSKALLE-NEJ             VALUE 'N'.                               
014200                                                                          
014300 01  PREV-HUVUD-SW               PIC X       VALUE 'J'.                   
014400     88 PREV-HUVUD-OK            VALUE 'J'.                               
014500     88 PREV-HUVUD-FEL           VALUE 'N'.                               
014600                                                                          
014700 01  W-IDORDNR7.                                                          
014800  03 W-IDORDNRPOS1-2             PIC X(2).                                
014900  03 W-IDORDNRPOS3-7.                                                     
015000   05 W-IDORDNRPOS3-5            PIC X(3).                                
015100   05 W-IDORDNRPOS6-7            PIC X(2).                                
015200 01  WDEF2-IDORDNR7              REDEFINES W-IDORDNR7.                    
015300  03 W-IDORDNRPOS1-4             PIC X(4).                                
015400  03 W-IDORDNRPOS5-6             PIC X(2).                                
015500  03 W-IDORDNRPOS7               PIC X(1).                                
015600                                                                          
015700                                                                          
015800 01  W-PRARTNTO.                                                          
015900  03 W-PRARTNTO-HEL              PIC X(7).                                
016000  03 FILLER                      PIC X(1)  VALUE '.'.                     
016100  03 W-PRARTNTO-DEC              PIC X(2).                                
016200                                                                          
016300 01  W-PRARTXXX-MED-PUNKT        PIC 9(7).9(2).                           
016400                                                                          
016500 01  W-IDKONTO                   PIC X(10).                               
016600 01  FILLER                      REDEFINES W-IDKONTO.                     
016700  03 W-IDKONTO-POS1              PIC X(1).                                
016800  03 W-IDKONTO-POS2-10           PIC X(9).                                
016900                                                                          
017000 01  W-IDKST                     PIC X(10).                               
017100 01  FILLER                      REDEFINES W-IDKST.                       
017200  03 W-IDKST2                    PIC X(2).                                
017300  03 FILLER                      PIC X(8).                                
017400                                                                          
017500 01  W-IDFTG                     PIC X(2).                                
017600 01  FILLER                      REDEFINES W-IDFTG.                       
017700  03 W-IDFTG1                    PIC X(1).                                
017800  03 W-IDFTG2                    PIC X(1).                                
017900                                                                          
018000 01  W-IDANALYS                  PIC X(12).                               
018100                                                                          
018200 01  WS-IDORDNR7-LDC             PIC 9(7).                                
018300                                                                          
018400 01  DATUM-AAMMDD                PIC 9(6).                                
018500 01  WS-TILOKDAT                 PIC 9(6).                                
018600                                                                          
018700 01  W-TIKLOCK                   PIC 9(8).                                
018800                                                                          
018900 01 DKONV-AAVVD.                                                          
019000   03  DKONV-AA.                                                          
019100    05 DKONV-A1                  PIC 9(1).                                
019200    05 DKONV-A2                  PIC 9(1).                                
019300   03  DKONV-VVD                 PIC 9(3).                                
019400                                                                          
019500 01 W-TIBEGPD.                                                            
019600   03  FILLER                    PIC 9(1)    VALUE ZERO.                  
019700   03  W-TIBEGPD-AA              PIC 9(2).                                
019800   03  W-TIBEGPD-VVD             PIC X(3).                                
019900                                                                          
020000 01 W-TIINLEV.                                                            
020100   03  FILLER                    PIC 9(2)    VALUE ZERO.                  
020200   03  W-TIINLEV-A1              PIC 9(1).                                
020300   03  W-TIINLEV-AVV             PIC 9(3).                                
020400   03  FILLER REDEFINES W-TIINLEV-AVV.                                    
020500    05 W-TIINLEV-A2              PIC 9(1).                                
020600    05 W-TIINLEV-VV              PIC 9(2).                                
020700                                                                          
020800 01  SPAR-JMF-AREA-START         PIC X(20)   VALUE 'SPAR-START  '.        
020900 01  SPAR-JMF-AREA.                                                       
021000   03  SPAR-IDDISTR              PIC 9(4).                                
021100   03  SPAR-IDKUNDNR             PIC 9(6).                                
021200   03  SPAR-IDORDNR7.                                                     
021300    05 SPAR-IDORDNRPOS1-2        PIC 9(2).                                
021400    05 SPAR-IDORDNR5             PIC 9(5).                                
021500                                                                          
021600 01  KONV-AREA-START             PIC X(20)   VALUE 'KONV-START '.         
021700 01  KONV-AREA.                                                           
021800   03  KONV-IDDISTR              PIC 9(4).                                
021900   03  KONV-IDKUNDNR             PIC 9(6).                                
022000   03  KONV-IDORDNR7.                                                     
022100    05 KONV-IDORDNRPOS1-2        PIC 9(2).                                
022200    05 KONV-IDORDNR5             PIC 9(5).                                
022300                                                                          
022400 01  CHAR-AREA-START             PIC X(20)   VALUE 'CHAR-START '.         
022500 01  CHAR-AREA.                                                           
022600   03  CHAR-IDDISTR              PIC X(4).                                
022700   03  CHAR-IDKUNDNR             PIC X(6).                                
022800   03  CHAR-IDORDNR7.                                                     
022900    05 CHAR-IDORDNRPOS1-2        PIC X(2).                                
023000    05 CHAR-IDORDNR5             PIC X(5).                                
023100                                                                          
023200 01  R50-AREA-X.                                                          
023300   03  FILLER                    PIC X(7).                                
023400   03  R50-IDKUNDNR-X            PIC X(6).                                
023500 01  R55-AREA-X.                                                          
023600   03  FILLER                    PIC X(7).                                
023700   03  R55-IDKUNDNR-X            PIC X(6).                                
023800 01  R75-AREA-X.                                                          
023900   03  FILLER                    PIC X(7).                                
024000   03  R75-IDKUNDNR-X            PIC X(6).                                
024100 01  RX3-AREA-X.                                                          
024200   03  FILLER                    PIC X(7).                                
024300   03  RX3-IDKUNDNR-X            PIC X(6).                                
024400     EJECT                                                                
024500*01 -COPY WWDIST35                                                        
024600                                                                          
024700 01  TEST-IDDISTR        PIC 9(5) COMP-3.                                 
024800*01  FILLER -COPY WWDIST20  -RED TEST-IDDISTR.                            
024900     EJECT                                                                
025000*01  FILLER -COPY WWDIST25  -RED TEST-IDDISTR.                            
025100     EJECT                                                                
025200*01  FILLER -COPY WWDIST23  -RED TEST-IDDISTR.                            
025300     EJECT                                                                
025400*    ---- AREA FÖR INFIL W41201                                           
025500 01  FILLER                      PIC X(8)    VALUE 'IN-AREA'.             
025600 01  IN-AREA                     PIC X(334).                              
025700*01  FILLER -COPY W463VG4 -PRE INVG4- -RED IN-AREA.                       
025800     EJECT                                                                
025900*01  FILLER -COPY W411500 -PRE IN-  -RED IN-AREA.                         
026000     EJECT                                                                
026100*01  FILLER -COPY W412RX5 -PRE INX5- -RED IN-AREA.                        
026200     EJECT                                                                
026300*01  FILLER -COPY W412RX7 -PRE INX7- -RED IN-AREA.                        
026400     EJECT                                                                
026500*01  FILLER -COPY W412RX3 -PRE INX3- -RED IN-AREA.                        
026600     EJECT                                                                
026700*01  FILLER -COPY W412TS2 -PRE INTS2-  -RED IN-AREA.                      
026800     EJECT                                                                
026900*01  FILLER -COPY W412EX1 -PRE INEX1-  -RED IN-AREA.                      
027000     EJECT                                                                
027100*01  FILLER -COPY W418DR5 -PRE INDR5-  -RED IN-AREA.                      
027200     EJECT                                                                
027300                                                                          
027400*    ---- AREA FÖR SORTFIL                                                
027500 01  FILLER                      PIC X(8)    VALUE 'SWS-AREA'.            
027600 01  SWS-AREA.                                                            
027700  03 SWS-SORT-POST               PIC X(334).                              
027800  03 SWS-SORT-IDDISTR            PIC X(4).                                
027900  03 SWS-SORT-IDKUNDNR           PIC X(6).                                
028000  03 SWS-SORT-IDORDNR            PIC X(7).                                
028100  03 SWS-SORT-IDTYP              PIC X(3).                                
028200*01  FILLER -COPY W411500 -PRE SWS- -RED SWS-AREA.                        
028300     EJECT                                                                
028400*    ---- AREA FÖR FELFIL                                                 
028500 01  FILLER                      PIC X(8)    VALUE 'FEL-AREA'.            
028600 01  FEL-AREA.                                                            
028700     03  FEL-POST                PIC X(108).                              
028800     03  FEL-POST-TEXT           PIC X(80).                               
028900     EJECT                                                                
029000*    ---- ARBETSAREOR FÖR INFIL W41201                                    
029100 01  FILLER                      PIC X(8)    VALUE 'R50-AREA'.            
029200 01  R50-AREA                    PIC X(180).                              
029300*01  FILLER -COPY W411500 -PRE R50- -RED R50-AREA.                        
029400     EJECT                                                                
029500 01  FILLER                      PIC X(8)    VALUE 'R51-AREA'.            
029600 01  R51-AREA                    PIC X(180).                              
029700*01  FILLER -COPY W411510 -PRE R51- -RED R51-AREA.                        
029800     EJECT                                                                
029900 01  FILLER                      PIC X(8)    VALUE 'R52-AREA'.            
030000 01  R52-AREA                    PIC X(180).                              
030100*01  FILLER -COPY W411520 -PRE R52- -RED R52-AREA.                        
030200     EJECT                                                                
030300 01  FILLER                      PIC X(8)    VALUE 'R53-AREA'.            
030400 01  R53-AREA                    PIC X(180).                              
030500*01  FILLER -COPY W411530 -PRE R53- -RED R53-AREA.                        
030600     EJECT                                                                
030700 01  FILLER                      PIC X(8)    VALUE 'R55-AREA'.            
030800 01  R55-AREA                    PIC X(180).                              
030900*01  FILLER -COPY W411550 -PRE R55- -RED R55-AREA.                        
031000     EJECT                                                                
031100 01  FILLER                      PIC X(8)    VALUE 'R75-AREA'.            
031200 01  R75-AREA                    PIC X(180).                              
031300*01  FILLER -COPY W411575 -PRE R75- -RED R75-AREA.                        
031400     EJECT                                                                
031500 01  FILLER                      PIC X(8)    VALUE 'RX5-AREA'.            
031600 01  RX5-AREA                    PIC X(180).                              
031700*01  FILLER -COPY W412RX5 -PRE RX5- -RED RX5-AREA.                        
031800     EJECT                                                                
031900 01  FILLER                      PIC X(8)    VALUE 'RX7-AREA'.            
032000 01  RX7-AREA                    PIC X(180).                              
032100*01  FILLER -COPY W412RX7 -PRE RX7- -RED RX7-AREA.                        
032200     EJECT                                                                
032300 01  FILLER                      PIC X(8)    VALUE 'RX3-AREA'.            
032400 01  RX3-AREA                    PIC X(180).                              
032500*01  FILLER -COPY W412RX3 -PRE RX3- -RED RX3-AREA.                        
032600     EJECT                                                                
032700 01  FILLER                      PIC X(8)    VALUE 'VG4-AREA'.            
032800 01  VG4-AREA                    PIC X(180).                              
032900*01  FILLER -COPY W463VG4 -PRE VG4- -RED VG4-AREA.                        
033000     EJECT                                                                
033100 01  FILLER                      PIC X(8)    VALUE 'TS2-AREA'.            
033200 01  TS2-AREA  -COPY W412TS2 -L.                                          
033300*01  FILLER -COPY W412TS2 -PRE TS2- -RED TS2-AREA.                        
033400     EJECT                                                                
033500 01  FILLER                      PIC X(8)    VALUE 'EX1-AREA'.            
033600 01  EX1-AREA                    PIC X(180).                              
033700*01  FILLER -COPY W412EX1 -PRE EX1- -RED EX1-AREA.                        
033800     EJECT                                                                
033900 01  FILLER                      PIC X(8)    VALUE 'DR5-AREA'.            
034000 01  DR5-AREA   -COPY W418DR5 -L.                                         
034100*01  FILLER -COPY W418DR5 -PRE DR5- -RED DR5-AREA.                        
034200     EJECT                                                                
034300*    ---- AREA FÖR UTFIL                                                  
034400 01  FILLER                      PIC X(16)   VALUE 'UT-OHUV-AREA'.        
034500 01  UT-OHUV-AREA                PIC X(440).                              
034600*01  FILLER -COPY W412500 -PRE UT- -RED UT-OHUV-AREA.                     
034700     EJECT                                                                
034800 01  FILLER                      PIC X(16)   VALUE 'UT-ORAD-AREA'.        
034900 01  UT-ORAD-AREA                PIC X(350).                              
035000*01  FILLER -COPY W412550 -PRE UT- -RED UT-ORAD-AREA.                     
035100     EJECT                                                                
035200*    ---- AREA FÖR ANNULLATIONS FIL                                       
035300 01  FILLER                      PIC X(16)   VALUE 'UT-ANNU-AREA'.        
035400 01  UT-ANNU-AREA                PIC X(350).                              
035500*01  FILLER -COPY W41254   -PRE UT- -RED UT-ANNU-AREA.                    
035600     EJECT                                                                
035700*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
035800 01  DYNAMISKA-SUBPROGRAM.                                                
035900   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
036000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
036100   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
036200   03  W009KSIF                  PIC X(8)    VALUE 'W009KSIF'.            
036300   03  W009CIA                   PIC X(8)    VALUE 'W009CIA '.            
036400   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
036500   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
036600   03  WZ20DAYS                  PIC X(8)    VALUE 'WZ20DAYS'.            
036700   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
036800     EJECT                                                                
036900                                                                          
037000 01  FELTEXT.                                                             
037100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
037200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
037300     EJECT                                                                
037400                                                                          
037500 01  RETURKOD-ABEND.                                                      
037600   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16 COMP SYNC.         
037700 01  KONTROLL-SIFFRA.                                                     
037800   03  REK-IDARTNR               PIC 9(9)    VALUE 0.                     
037900   03  REK-LNGD                  PIC 9(1)    VALUE 9.                     
038000   03  REK-REKSIFFR              PIC 9(1)    VALUE 0.                     
038100     EJECT                                                                
038200*01  FILLER -COPY WDATAREA.                                               
038300     EJECT                                                                
038400*01  FILLER -COPY W0005       -PRE POSTSUM-.                              
038500     EJECT                                                                
038600 01 FILLER                       PIC X(8) VALUE 'W005INIT'.               
038700*   -COPY WMSGINIT                                                        
038800     EJECT                                                                
038900 01  FILLER                      PIC X(16) VALUE 'CIA-AREA'.              
039000     SKIP3                                                                
039100*   -COPY W009CIA                                                         
039200     EJECT                                                                
039300 01  WZ20DAYS                    PIC X(8) VALUE 'WZ20DAYS'.               
039400     SKIP3                                                                
039500*    -COPY WZ20DAYS                                                       
039600     EJECT                                                                
039700                                                                          
039800 01  NYCKLAR-TILL-DLI.                                                    
039900                                                                          
040000    03 W-IDGMT-X.                                                         
040100       05 W-WDB2-IDDISTR       PIC S9(5)    VALUE ZERO COMP-3.            
040200       05 W-WDB2-IDKUNDNR      PIC S9(7)    VALUE ZERO COMP-3.            
040300                                                                          
040400     SKIP2                                                                
040500*    --- STATUS-KOD FRÅN IMS                                              
040600 01  STATUS-WS                   PIC XX.                                  
040700     88  SEGMENT-FINNS                       VALUE '  '.                  
040800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
040900     SKIP2                                                                
041000 01  GODK-STATUSKODER.                                                    
041100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
041200     SKIP3                                                                
041300 01  SSA1                        PIC X(64).                               
041400 01  SSA2                        PIC X(64).                               
041500     EJECT                                                                
041600*    --- IMS FUNKTIONSKODER                                               
041700*01  -COPY W0003                                                          
041800     EJECT                                                                
041900*    ---  DLI INPUT-OUTPUT AREA                                           
042000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA'.          
042100     SKIP3                                                                
042200                                                                          
042300 01  FILLER                      PIC X(16)  VALUE 'AREA FOR WDB2'.        
042400 01  DLI-IO-AREA-WDB2.                                                    
042500     03  DLI-IO-WDB201.                                                   
042600*        05  -COPY WDB201                                                 
042700     EJECT                                                                
042800                                                                          
042900 LINKAGE SECTION.                                                         
043000                                                                          
043100*01  -COPY W0008  -PRE WDB2-                                              
043200     05  FILLER                  PIC X.                                   
043300 01  WDP7-PCB                    PIC X.                                   
043400     EJECT                                                                
043500                                                                          
043600 PROCEDURE DIVISION   USING  WDB2-PCB WDP7-PCB.                           
043700     ENTRY 'DLITCBL'  USING  WDB2-PCB WDP7-PCB.                           
043800                                                                          
043900     PERFORM A-INITIERA                                                   
044000                                                                          
044100     SORT SORTFIL ON ASCENDING KEY SORT-IDDISTR                           
044200                                   SORT-IDKUNDNR                          
044300                                   SORT-IDORDNR                           
044400                                   SORT-IDTYP                             
044500     INPUT  PROCEDURE B-LAS-INPUT                                         
044600     OUTPUT PROCEDURE C-BEARBETA.                                         
044700                                                                          
044800     IF SORT-RETURN > 0                                                   
044900        DISPLAY '*** W41201  FEL VID SORTERING'                           
045000        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
045100     ELSE                                                                 
045200        PERFORM Z-FINIT                                                   
045300        MOVE ZERO TO RETURN-CODE                                          
045400        GOBACK                                                            
045500     END-IF                                                               
045600     .                                                                    
045700     EJECT                                                                
045800 A-INITIERA SECTION.                                                      
045900                                                                          
046000     ACCEPT DATUM-AAMMDD FROM DATE                                        
046100     ACCEPT W-TIKLOCK    FROM TIME                                        
046200                                                                          
046300*    KONVERTERA DAGENS DATUM TILL AAVVD FORMAT                            
046400     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
046500     MOVE DATUM-AAMMDD   TO DAT-I-TIDATUM                                 
046600     CALL WDATKONV USING DAT-KDDATFORM  DAT-I-TIDATUM                     
046700                         DAT-O-TIDATUM  DAT-KDSVAR                        
046800     IF DAT-KDSVAR NOT = SPACE                                            
046900        MOVE ' FELAKTIG RETURKOD FRÅN WDATKONV I A-INITIERA '             
047000                         TO FELTEXT                                       
047100        DISPLAY ' FELAKTIG RETURKOD FRÅN WDATKONV I A-INITIERA '          
047200        DISPLAY DAT-KDDATFORM                                             
047300        DISPLAY DAT-I-TIDATUM                                             
047400        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
047500     END-IF                                                               
047600     MOVE DAT-TIAAVVD    TO DKONV-AAVVD                                   
047700                                                                          
047800     MOVE IDPGM          TO POSTSUM-PROGNAMN                              
047900                                                                          
048000     OPEN INPUT  W41201                                                   
048100     OPEN OUTPUT W41202                                                   
048200                 W41203                                                   
048300                 W41209                                                   
048400                                                                          
048500     MOVE ZERO           TO SPAR-IDDISTR                                  
048600                            SPAR-IDKUNDNR                                 
048700                            SPAR-IDORDNR7                                 
048800     MOVE SPACE          TO UT-ORAD-AREA                                  
048900     INITIALIZE             UT-OHUV-AREA                                  
049000     MOVE ZERO           TO UT-OHUV-TIREPDAT                              
049100     MOVE ZERO           TO UT-ORAD-RERAB                                 
049200     .                                                                    
049300     EJECT                                                                
049400                                                                          
049500 B-LAS-INPUT      SECTION.                                                
049600     SKIP2                                                                
049700     PERFORM S01-LAS-W41201                                               
049800     PERFORM UNTIL W41201-EOF = JA                                        
049900        MOVE IN-AREA             TO SWS-AREA                              
050000        IF IN-IDTYP = 'RX5'                                               
050100           MOVE INX5-IDDISTR       TO SWS-SORT-IDDISTR                    
050200           MOVE INX5-IDKUNDNR      TO SWS-SORT-IDKUNDNR                   
050300           MOVE INX5-IDORDNR7      TO SWS-SORT-IDORDNR                    
050400           MOVE INX5-IDTYP         TO SWS-SORT-IDTYP                      
050500        ELSE                                                              
050600           IF IN-IDTYP = 'RX3'                                            
050700              MOVE INX3-IDDISTR    TO SWS-SORT-IDDISTR                    
050800              MOVE INX3-IDKUNDNR   TO SWS-SORT-IDKUNDNR                   
050900              MOVE INX3-IDORDNR7   TO SWS-SORT-IDORDNR                    
051000              MOVE INX3-IDTYP      TO SWS-SORT-IDTYP                      
051100           ELSE                                                           
051200             IF IN-IDTYP = 'VG4'                                          
051300                MOVE INVG4-IDDISTR  TO SWS-SORT-IDDISTR                   
051400                MOVE INVG4-IDKUNDNR TO SWS-SORT-IDKUNDNR                  
051500                MOVE INVG4-IDORDNR7 TO SWS-SORT-IDORDNR                   
051600                MOVE INVG4-IDPTYP   TO SWS-SORT-IDTYP                     
051700             ELSE                                                         
051800               IF IN-IDTYP = 'TS2' OR 'TS3' OR 'TS4' OR 'TS5'             
051900                          OR 'TS6'                                        
052000                  MOVE INTS2-IDDISTR  TO SWS-SORT-IDDISTR                 
052100                  MOVE INTS2-IDKUNDNR TO SWS-SORT-IDKUNDNR                
052200                  MOVE INTS2-IDORDNR7 TO SWS-SORT-IDORDNR                 
052300                  MOVE INTS2-IDPTYP   TO SWS-SORT-IDTYP                   
052400               ELSE                                                       
052500                 IF IN-IDTYP = 'EXT'                                      
052600                  MOVE INEX1-IDDISTR  TO SWS-SORT-IDDISTR                 
052700                  MOVE INEX1-IDKUNDNR TO SWS-SORT-IDKUNDNR                
052800                  MOVE INEX1-IDORDNR7 TO SWS-SORT-IDORDNR                 
052900                  MOVE INEX1-IDPTYP   TO SWS-SORT-IDTYP                   
053000                 ELSE                                                     
053100                   IF IN-IDTYP = 'RX7'                                    
053200                    MOVE INX7-IDDISTR       TO SWS-SORT-IDDISTR           
053300                    MOVE INX7-IDKUNDNR      TO SWS-SORT-IDKUNDNR          
053400                    MOVE INX7-IDORDNR7      TO SWS-SORT-IDORDNR           
053500                    MOVE INX7-IDTYP         TO SWS-SORT-IDTYP             
053600                   ELSE                                                   
053700                     IF IN-IDTYP = 'DR5'                                  
053800                      MOVE INDR5-IDDISTR  TO SWS-SORT-IDDISTR             
053900                      MOVE INDR5-IDKUNDNR TO SWS-SORT-IDKUNDNR            
054000                      MOVE ZERO           TO SWS-SORT-IDORDNR             
054100                      MOVE INDR5-IDORDNR5                                 
054200                                        TO SWS-SORT-IDORDNR(3:5)          
054300                      MOVE INDR5-IDPTYP TO SWS-SORT-IDTYP                 
054400                     ELSE                                                 
054500                       MOVE IN-IDDISTR  TO SWS-SORT-IDDISTR               
054600                       MOVE IN-IDKUNDNR TO SWS-SORT-IDKUNDNR              
054700                       MOVE IN-IDORDNR  TO SWS-SORT-IDORDNR               
054800                       MOVE IN-IDTYP    TO SWS-SORT-IDTYP                 
054900                     END-IF                                               
055000                   END-IF                                                 
055100                 END-IF                                                   
055200               END-IF                                                     
055300             END-IF                                                       
055400           END-IF                                                         
055500           IF SWS-SORT-IDKUNDNR = SPACE                                   
055600              MOVE ZERO         TO SWS-SORT-IDKUNDNR                      
055700           END-IF                                                         
055800        END-IF                                                            
055900        PERFORM S05-SORT-RELEASE                                          
056000        PERFORM S01-LAS-W41201                                            
056100     END-PERFORM                                                          
056200     .                                                                    
056300     EJECT                                                                
056400 C-BEARBETA SECTION.                                                      
056500                                                                          
056600     PERFORM S06-SORT-RETURN                                              
056700                                                                          
056800     PERFORM UNTIL SORTFIL-EOF = JA                                       
056900                                                                          
057000*       KONTROLLERA ATT POSTTYP ÄR KORREKT                                
057100        PERFORM S17-KONTROLL-POSTTYP                                      
057200                                                                          
057300*       SATSORDER ÄR EJ GODKÄNDA                                          
057400        IF SWS-IDTYP = 'R50'  AND  SWS-KDORDKL = 5 AND                    
057500           (SWS-KDMASK = 'H' OR 'S')                                      
057600           PERFORM CA-SKIP-SATSORDER                                      
057700        ELSE                                                              
057800                                                                          
057900*       KONTROLL OM POSTTYP R50/500/RG0 SAKNAS                            
058000           IF SWS-IDTYP = 'R51' OR 'R52' OR 'R53'                         
058100              PERFORM CB-ORDERHUV-SAKNAS                                  
058200           ELSE                                                           
058300              IF SWS-IDTYP = 'R50'                                        
058400                 PERFORM CC-SKAPA-ORDERHUVUD                              
058500              END-IF                                                      
058600              IF SWS-IDTYP = 'R55'                                        
058700                 PERFORM CD-SKAPA-ORDERRAD                                
058800              END-IF                                                      
058900              IF SWS-IDTYP = 'R75'                                        
059000                 PERFORM CE-SKAPA-ORDERRADER                              
059100              END-IF                                                      
059200              IF SWS-IDTYP = 'RX5'                                        
059300                 PERFORM CF-SKAPA-ORDERRAD-RX5                            
059400              END-IF                                                      
059500              IF SWS-IDTYP = 'RX3'                                        
059600                 PERFORM CG-SKAPA-ORDERRAD-RX3                            
059700              END-IF                                                      
059800              IF SWS-IDTYP = 'VG4'                                        
059900                 PERFORM CH-SKAPA-ORDERRAD-VG4                            
060000              END-IF                                                      
060100              IF SWS-IDTYP = 'DR5'                                        
060200                 PERFORM CI-SKAPA-ORDERRAD-DR5                            
060300              END-IF                                                      
060400              IF SWS-IDTYP = 'EXT'                                        
060500                 PERFORM CK-SKAPA-ORDERRAD-EX1                            
060600              END-IF                                                      
060700              IF SWS-IDTYP = 'RX7'                                        
060800                 PERFORM CL-SKAPA-ORDERRAD-RX7                            
060900              END-IF                                                      
061000              IF SWS-IDTYP = 'TS2' OR 'TS3' OR 'TS4' OR 'TS5'             
061100                          OR 'TS6'                                        
061200                 PERFORM CM-SKAPA-ORDERRAD-TS2                            
061300              END-IF                                                      
061400           END-IF                                                         
061500        END-IF                                                            
061600                                                                          
061700     END-PERFORM                                                          
061800                                                                          
061900*    SKRIV SISTA ORDERHUVUDPOST                                           
062000     IF UT-OHUV-IDPTYP NOT = SPACE                                        
062100        PERFORM S02-SKRIV-W41202                                          
062200     END-IF                                                               
062300                                                                          
062400     .                                                                    
062500     EJECT                                                                
062600 CA-SKIP-SATSORDER SECTION.                                               
062700     SKIP2                                                                
062800*    EN SATSORDER FINNS PÅ INPUTFILEN - SKRIV UT ALLA POSTER              
062900*    FÖR ORDERN PÅ FELLISTAN                                              
063000     PERFORM S09-SPARA-ORDERIDENT                                         
063100     PERFORM UNTIL SORTFIL-EOF = JA OR                                    
063200             SWS-IDTYP = 'RX5' OR SWS-IDTYP = 'RX3' OR                    
063300             SWS-IDTYP = 'RX7' OR                                         
063400             SPAR-IDDISTR  NOT = SWS-IDDISTR  OR                          
063500             SPAR-IDKUNDNR NOT = SWS-IDKUNDNR OR                          
063600             SPAR-IDORDNR5 NOT = SWS-IDORDNR                              
063700        MOVE SWS-SORT-POST    TO FEL-POST                                 
063800        MOVE 'SATSORDER BEHANDLAS EJ '                                    
063900                           TO FEL-POST-TEXT                               
064000        PERFORM S04-SKRIV-W41203                                          
064100        PERFORM S06-SORT-RETURN                                           
064200        IF SORTFIL-EOF = NEJ                                              
064300           PERFORM S17-KONTROLL-POSTTYP                                   
064400        END-IF                                                            
064500     END-PERFORM                                                          
064600     .                                                                    
064700     EJECT                                                                
064800                                                                          
064900 CB-ORDERHUV-SAKNAS SECTION.                                              
065000     SKIP2                                                                
065100*    EN FORTSÄTTNING PÅ ORDERHUVUD FINNS MEN ORDERHUVUDPOST FINNS         
065200*    EJ FÖR ORDERN. SKRIV HELA ORDERN PÅ FELLISTAN.                       
065300     PERFORM S09-SPARA-ORDERIDENT                                         
065400     PERFORM UNTIL SORTFIL-EOF = JA OR                                    
065500             SWS-IDTYP = 'RX5' OR SWS-IDTYP = 'RX3' OR                    
065600             SWS-IDTYP = 'RX7' OR                                         
065700             SPAR-IDDISTR  NOT = SWS-IDDISTR  OR                          
065800             SPAR-IDKUNDNR NOT = SWS-IDKUNDNR OR                          
065900             SPAR-IDORDNR5 NOT = SWS-IDORDNR                              
066000        MOVE SWS-SORT-POST    TO FEL-POST                                 
066100        MOVE 'ORDERHUVUD SAKNAS FÖR NAMN/ADRESS POST'                     
066200                              TO FEL-POST-TEXT                            
066300        PERFORM S04-SKRIV-W41203                                          
066400        PERFORM S06-SORT-RETURN                                           
066500        IF SORTFIL-EOF = NEJ                                              
066600           PERFORM S17-KONTROLL-POSTTYP                                   
066700        END-IF                                                            
066800     END-PERFORM                                                          
066900     .                                                                    
067000     EJECT                                                                
067100                                                                          
067200 CC-SKAPA-ORDERHUVUD SECTION.                                             
067300     SKIP2                                                                
067400*    POSTTYP R50,R51,R52 OCH R53 BEHANDLAS                                
067500                                                                          
067600     IF PREV-HUVUD-FEL                                                    
067700*------FELFIL                                                             
067800       MOVE R50-AREA             TO FEL-POST                              
067900       MOVE 'DISTRIKT/KUND/ORDERNR EJ NUMERISKT'                          
068000                                 TO FEL-POST-TEXT                         
068100       PERFORM S04-SKRIV-W41203                                           
068200     END-IF                                                               
068300                                                                          
068400     MOVE SWS-AREA               TO R50-AREA                              
068500                                    R50-AREA-X                            
068600     IF R50-IDKUNDNR-X = SPACE                                            
068700        MOVE ZERO           TO R50-IDKUNDNR                               
068800     END-IF                                                               
068900     PERFORM S09-SPARA-ORDERIDENT                                         
069000                                                                          
069100     MOVE NEJ                     TO SW-DOSKALLE                          
069200     IF SWS-IDTYP = 'R50' AND SWS-KDMASK = 'A'                            
069300        MOVE JA                   TO SW-DOSKALLE                          
069400        MOVE SPACE                TO CHAR-AREA                            
069500        MOVE R50-IDDISTR          TO CHAR-IDDISTR                         
069600        MOVE R50-IDKUNDNR         TO CHAR-IDKUNDNR                        
069700        MOVE R50-IDORDNR          TO CHAR-IDORDNR5                        
069800        PERFORM S10-KONV-ORDERIDENT                                       
069900        PERFORM CCB-SKAPA-RAD-DOSKALLE                                    
070000     END-IF                                                               
070100                                                                          
070200     PERFORM S06-SORT-RETURN                                              
070300                                                                          
070400     IF SORTFIL-EOF   = NEJ          AND                                  
070500        SPAR-IDDISTR  = SWS-IDDISTR  AND                                  
070600        SPAR-IDKUNDNR = SWS-IDKUNDNR AND                                  
070700        SPAR-IDORDNR5 = SWS-IDORDNR  AND                                  
070800            SWS-IDTYP = 'R51'                                             
070900        MOVE SWS-AREA            TO R51-AREA                              
071000        PERFORM S06-SORT-RETURN                                           
071100     ELSE                                                                 
071200        MOVE SPACE               TO R51-AREA                              
071300     END-IF                                                               
071400                                                                          
071500     IF SORTFIL-EOF   = NEJ          AND                                  
071600        SPAR-IDDISTR  = SWS-IDDISTR  AND                                  
071700        SPAR-IDKUNDNR = SWS-IDKUNDNR AND                                  
071800        SPAR-IDORDNR5 = SWS-IDORDNR  AND                                  
071900            SWS-IDTYP = 'R52'                                             
072000        MOVE SWS-AREA            TO R52-AREA                              
072100        PERFORM S06-SORT-RETURN                                           
072200     ELSE                                                                 
072300        MOVE SPACE               TO R52-AREA                              
072400     END-IF                                                               
072500                                                                          
072600     IF SORTFIL-EOF   = NEJ          AND                                  
072700        SPAR-IDDISTR  = SWS-IDDISTR  AND                                  
072800        SPAR-IDKUNDNR = SWS-IDKUNDNR AND                                  
072900        SPAR-IDORDNR5 = SWS-IDORDNR  AND                                  
073000            SWS-IDTYP = 'R53'                                             
073100        MOVE SWS-AREA            TO R53-AREA                              
073200        PERFORM S06-SORT-RETURN                                           
073300     ELSE                                                                 
073400        MOVE SPACE               TO R53-AREA                              
073500     END-IF                                                               
073600                                                                          
073700     IF (SORTFIL-EOF = NEJ  AND  DOSKALLE-NEJ  AND                        
073800        (SWS-IDTYP NOT = 'R55' AND 'R75' ))                               
073900                       OR                                                 
074000        (SORTFIL-EOF = JA AND DOSKALLE-NEJ)                               
074100        MOVE R50-AREA TO FEL-POST                                         
074200        MOVE 'ORDERHUVUD SAKNAR ORDERRADPOSTER'                           
074300                      TO FEL-POST-TEXT                                    
074400        PERFORM S04-SKRIV-W41203                                          
074500     ELSE                                                                 
074600       MOVE SPACE                TO CHAR-AREA                             
074700       MOVE R50-IDDISTR          TO CHAR-IDDISTR                          
074800       MOVE R50-IDKUNDNR         TO CHAR-IDKUNDNR                         
074900       MOVE R50-IDORDNR          TO CHAR-IDORDNR5                         
075000       PERFORM S10-KONV-ORDERIDENT                                        
075100                                                                          
075200                                                                          
075300*    SPARA ORDERHUVUDIDENTITET                                            
075400       MOVE KONV-IDDISTR     TO SPAR-IDDISTR                              
075500       MOVE KONV-IDKUNDNR    TO SPAR-IDKUNDNR                             
075600       MOVE KONV-IDORDNR7    TO SPAR-IDORDNR7                             
075700                                                                          
075800*    SKRIV FÖRGÅENDE ORDERHUVUDPOST                                       
075900       IF UT-OHUV-IDPTYP NOT = SPACE                                      
076000               AND    PREV-HUVUD-OK                                       
076100           PERFORM S02-SKRIV-W41202                                       
076200       END-IF                                                             
076300                                                                          
076400       PERFORM CCC-INIT-NAESTA-OHUV-AREA                                  
076500     END-IF                                                               
076600     .                                                                    
076700     EJECT                                                                
076800 CCB-SKAPA-RAD-DOSKALLE SECTION.                                          
076900                                                                          
077000     IF KONV-IDDISTR NUMERIC  AND  KONV-IDKUNDNR NUMERIC                  
077100                              AND  KONV-IDORDNR7 NUMERIC                  
077200                                                                          
077300       MOVE SPACE              TO UT-ORAD-AREA                            
077400       MOVE 'OVR '             TO UT-ORAD-IDSYSTEM                        
077500       MOVE DATUM-AAMMDD       TO UT-ORAD-TIFILDAT                        
077600       ADD +1                  TO W-TIKLOCK                               
077700       MOVE W-TIKLOCK          TO UT-ORAD-TIKLOCK                         
077800       MOVE 'W411500'          TO UT-ORAD-IDCPYTXT                        
077900       MOVE 'R55'              TO UT-ORAD-IDPTYP                          
078000       MOVE KONV-IDDISTR       TO UT-ORAD-IDDISTR                         
078100       MOVE KONV-IDKUNDNR      TO UT-ORAD-IDKUNDNR                        
078200       MOVE KONV-IDORDNR7      TO UT-ORAD-IDORDNR                         
078300                                                                          
078400       MOVE SPACE              TO UT-ORAD-IDARTNR                         
078500                                  UT-ORAD-REKSIFFR                        
078600                                  UT-ORAD-KVBEART                         
078700                                  UT-ORAD-TITPO                           
078800                                  UT-ORAD-FLRESTN                         
078900                                  UT-ORAD-FLINVEST                        
079000                                  UT-ORAD-IDKONTO                         
079100                                  UT-ORAD-IDKST                           
079200                                  UT-ORAD-BEVOLREF                        
079300                                  UT-ORAD-KDVRINFO                        
079400                                  UT-ORAD-KDDSP                           
079500                                  UT-ORAD-FLSLATT                         
079600                                  UT-ORAD-KDKVBRYT                        
079700                                  UT-ORAD-BERADREF                        
079800                                                                          
079900       MOVE SPACE              TO UT-ORAD-PRARTNTO                        
080000       PERFORM S03-SKRIV-W41202                                           
080100     END-IF                                                               
080200     .                                                                    
080300     EJECT                                                                
080400                                                                          
080500 CCC-INIT-NAESTA-OHUV-AREA SECTION.                                       
080600                                                                          
080700     MOVE JA                  TO PREV-HUVUD-SW                            
080800     INITIALIZE                  UT-OHUV-AREA                             
080900     MOVE ZERO           TO UT-OHUV-TIREPDAT                              
081000     IF R50-IDDISTR NOT NUMERIC  OR R50-IDKUNDNR NOT NUMERIC OR           
081100                                    R50-IDORDNR NOT NUMERIC               
081200        MOVE NEJ              TO PREV-HUVUD-SW                            
081300     END-IF                                                               
081400                                                                          
081500     MOVE 'OVR'               TO UT-OHUV-IDSYSTEM                         
081600     MOVE DATUM-AAMMDD        TO UT-OHUV-TIFILDAT                         
081700     IF DOSKALLE-NEJ                                                      
081800        ADD +1                TO W-TIKLOCK                                
081900     END-IF                                                               
082000     MOVE W-TIKLOCK           TO UT-OHUV-TIKLOCK                          
082100     MOVE 'W411500'           TO UT-OHUV-IDCPYTXT                         
082200     MOVE R50-IDTYP           TO UT-OHUV-IDPTYP                           
082300     MOVE KONV-IDDISTR        TO UT-OHUV-IDDISTR                          
082400     MOVE UT-OHUV-IDDISTR     TO TEST-IDDISTR                             
082500     MOVE KONV-IDKUNDNR       TO UT-OHUV-IDKUNDNR                         
082600     MOVE KONV-IDORDNR7       TO UT-OHUV-IDORDNR                          
082700     MOVE R50-KDORDKL         TO UT-OHUV-KDORDKL                          
082710     MOVE SPACE               TO UT-OHUV-KDFRAKT                          
083500                                                                          
083600*----KONVERTERA TIBEGPD FORMAT VVD TILL FORMAT AAVVD.                     
083700*----OM VVD FÖR TIBEGPD ÄR >= DAGENS VVD SÄTT TIBEGPD-AA TILL             
083800*----DAGENS ÅRTAL - ANNARS SÄTT TIBEGPD TILL NÄSTA ÅR                     
083900     MOVE R50-TIBEGPD         TO W-TIBEGPD-VVD                            
084000     IF W-TIBEGPD-VVD NOT = SPACE AND                                     
084100        R50-TIBEGPD > ZERO                                                
084200        MOVE DKONV-AA         TO W-TIBEGPD-AA                             
084300        IF W-TIBEGPD-VVD < DKONV-VVD                                      
084400           ADD +1             TO W-TIBEGPD-AA                             
084500        END-IF                                                            
084600*-------KONVERTERA TIBEGPD FORMAT AAVVD TILL FORMAT AAMMDD                
084700*-------MHA MODUL WDATKONV. FLYTTA TILL TIRFS.                            
084800        MOVE 'AAVVD '         TO DAT-KDDATFORM                            
084900        MOVE W-TIBEGPD        TO DAT-I-TIDATUM                            
085000        CALL WDATKONV USING DAT-KDDATFORM                                 
085100                            DAT-I-TIDATUM                                 
085200                            DAT-O-TIDATUM                                 
085300                            DAT-KDSVAR                                    
085400        IF DAT-KDSVAR NOT = SPACE                                         
085500           MOVE ' FELAKTIG RETURKOD FRÅN WDATKONV I CC-SKAPA-'            
085600                                     TO FELTEXT                           
085700           DISPLAY ' FEL RETURKOD FRÅN WDATKONV I CC-SKAPA-'              
085800           DISPLAY DAT-KDDATFORM                                          
085900           DISPLAY DAT-I-TIDATUM                                          
086000           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
086100           MOVE W-TIBEGPD     TO UT-OHUV-TIRFS                            
086200        ELSE                                                              
086300           MOVE DAT-TIAAMMDD  TO UT-OHUV-TIRFS                            
086400        END-IF                                                            
086500     ELSE                                                                 
086600        MOVE SPACE            TO UT-OHUV-TIRFS                            
086700     END-IF                                                               
086800                                                                          
086900     MOVE R50-BEKUNDRF-001    TO UT-OHUV-BEKUNDRF                         
087000     MOVE R50-TIREF1          TO UT-OHUV-TIKUNDRF                         
087100     MOVE R50-KDFAKTYP        TO UT-OHUV-KDFAKTYP                         
087200     MOVE R50-FLRESTN         TO UT-OHUV-FLRESTN                          
087300     INSPECT R53-IDDIVORD REPLACING ALL ' ' BY '0'                        
087400     IF R53-IDDIVORD NUMERIC                                              
087500        IF R53-IDDIVORD > ZERO                                            
087600           MOVE '2'           TO UT-OHUV-KDTPOTYP                         
087700        ELSE                                                              
087800           MOVE SPACE         TO UT-OHUV-KDTPOTYP                         
087900        END-IF                                                            
088000     ELSE                                                                 
088100        MOVE SPACE            TO UT-OHUV-KDTPOTYP                         
088200     END-IF                                                               
088300     MOVE SPACE               TO UT-OHUV-TITPO                            
088400     MOVE SPACE               TO UT-OHUV-BELAGINS                         
088500     IF R51-IDTYP NOT = SPACE                                             
088600        MOVE R51-BEGODSM      TO UT-OHUV-BEGODSM                          
088700     ELSE                                                                 
088800        MOVE SPACE            TO UT-OHUV-BEGODSM                          
088900     END-IF                                                               
089000     IF R52-IDTYP NOT = SPACE                                             
089100        MOVE R52-ADGODSMK     TO UT-OHUV-ADGODSM                          
089200     ELSE                                                                 
089300        MOVE SPACE            TO UT-OHUV-ADGODSM                          
089400     END-IF                                                               
089500     MOVE R50-KDROPACK        TO UT-OHUV-KDROPACK                         
089600     IF UT-OHUV-KDFAKTYP = 'G' OR 'N'                                     
089700        MOVE R50-IDKONTO      TO W-IDKONTO                                
089800        MOVE R50-IDKST        TO W-IDKST                                  
089900        MOVE R50-IDANALYS     TO W-IDANALYS                               
090000                                                                          
090100        IF R50-IDFTG = ZERO                                               
090200           IF W-IDKONTO-POS1 NUMERIC                                      
090300             PERFORM S18-OMVANDLA-FORETAGSKOD                             
090400           ELSE                                                           
090500              MOVE SPACE      TO UT-OHUV-IDFTG                            
090600           END-IF                                                         
090700        ELSE                                                              
090800           MOVE R50-IDFTG     TO UT-OHUV-IDFTG                            
090900        END-IF                                                            
091000        MOVE W-IDKONTO        TO UT-OHUV-IDKONTO                          
091100        MOVE W-IDKST          TO UT-OHUV-IDKST                            
091200        MOVE W-IDANALYS       TO UT-OHUV-IDANALYS                         
091300     ELSE                                                                 
091400        MOVE SPACE            TO UT-OHUV-IDKONTO                          
091500                                 UT-OHUV-IDKST                            
091600                                 UT-OHUV-IDANALYS                         
091700                                 UT-OHUV-IDFTG                            
091800     END-IF                                                               
091900     IF UT-OHUV-KDFAKTYP NOT = 'G' AND                                    
092000        UT-OHUV-KDFAKTYP NOT = 'N'                                        
092100        MOVE R50-BEVARREF     TO UT-OHUV-BEVARREF                         
092200     ELSE                                                                 
092300        MOVE SPACE            TO UT-OHUV-BEVARREF                         
092400     END-IF                                                               
092500     MOVE R50-KDTULLVE        TO UT-OHUV-KDTULLVE                         
092600     MOVE R50-KDNOTES         TO UT-OHUV-KDNOTES                          
092700     MOVE SPACE               TO UT-OHUV-FLAUTFAK                         
092800     MOVE ZERO                TO UT-OHUV-IDDEPT                           
092900     MOVE NEJ                 TO UT-OHUV-FLAUTPAC                         
093000     IF DIST20-EMBALLAGE                                                  
093100        MOVE JA               TO UT-OHUV-FLEMBORD                         
093200     ELSE                                                                 
093300        MOVE NEJ              TO UT-OHUV-FLEMBORD                         
093400     END-IF                                                               
093500     MOVE NEJ                 TO UT-OHUV-FLOVRLEV                         
093600     MOVE ZERO                TO UT-OHUV-IDGROSS                          
093700     MOVE ZERO                TO UT-OHUV-TIHHMM                           
093800     MOVE SPACE               TO UT-OHUV-IDDC                             
093900                                 UT-OHUV-FLLSBOK                          
094000                                 UT-OHUV-IDBILREG                         
094100                                 UT-OHUV-IDCISNR                          
094200                                 UT-OHUV-IDVIN                            
094300                                 UT-OHUV-BEMEKAN                          
094400                                 UT-OHUV-BETELNR-TACD                     
094500                                 UT-OHUV-FLFPLOCK                         
094600                                 UT-OHUV-TETACDBO                         
094700     .                                                                    
094800     EJECT                                                                
094900                                                                          
095000 CD-SKAPA-ORDERRAD SECTION.                                               
095100     SKIP2                                                                
095200*    POSTTYP R55 BEHANDLAS                                                
095300                                                                          
095400     MOVE SWS-AREA               TO R55-AREA                              
095500                                    R55-AREA-X                            
095600     IF R55-IDKUNDNR-X = SPACE                                            
095700        MOVE ZERO                TO R55-IDKUNDNR                          
095800     END-IF                                                               
095900     MOVE SPACE                  TO CHAR-AREA                             
096000     MOVE R55-IDDISTR            TO CHAR-IDDISTR                          
096100     MOVE R55-IDKUNDNR           TO CHAR-IDKUNDNR                         
096200     MOVE R55-IDORDNR            TO CHAR-IDORDNR5                         
096300     PERFORM S10-KONV-ORDERIDENT                                          
096400                                                                          
096500*    KONTROLLERA ATT ORDERHUVUD HAR SKAPATS                               
096600     IF SPAR-IDDISTR  NOT = KONV-IDDISTR   OR                             
096700        SPAR-IDKUNDNR NOT = KONV-IDKUNDNR  OR                             
096800        SPAR-IDORDNR7 NOT = KONV-IDORDNR7                                 
096900                                                                          
097000        MOVE SWS-SORT-POST    TO FEL-POST                                 
097100        MOVE 'ORDERHUVUD SAKNAS/FEL FÖR ORDERRADPOST '                    
097200                              TO FEL-POST-TEXT                            
097300        PERFORM S04-SKRIV-W41203                                          
097400     ELSE                                                                 
097500        IF KONV-IDDISTR NUMERIC AND KONV-IDKUNDNR NUMERIC                 
097600                                AND KONV-IDORDNR7 NUMERIC                 
097700           PERFORM CDA-KONV-ORDERRAD                                      
097800           PERFORM S03-SKRIV-W41202                                       
097900        ELSE                                                              
098000           MOVE SWS-SORT-POST    TO FEL-POST                              
098100           MOVE 'ORDERRADENS NYCKELFÄLT EJ NUMERISKT'                     
098200                                 TO FEL-POST-TEXT                         
098300           PERFORM S04-SKRIV-W41203                                       
098400        END-IF                                                            
098500     END-IF                                                               
098600                                                                          
098700     PERFORM S06-SORT-RETURN                                              
098800     .                                                                    
098900     EJECT                                                                
099000                                                                          
099100 CDA-KONV-ORDERRAD SECTION.                                               
099200     SKIP2                                                                
099300                                                                          
099400*    SKAPA ORDERRADPOST                                                   
099500     MOVE SPACE                  TO UT-ORAD-AREA                          
099600     MOVE 'OVR'                  TO UT-ORAD-IDSYSTEM                      
099700     MOVE DATUM-AAMMDD           TO UT-ORAD-TIFILDAT                      
099800     MOVE W-TIKLOCK              TO UT-ORAD-TIKLOCK                       
099900     MOVE SWS-IDTYP              TO UT-ORAD-IDPTYP                        
100000     MOVE KONV-IDDISTR           TO UT-ORAD-IDDISTR                       
100100     MOVE KONV-IDKUNDNR          TO UT-ORAD-IDKUNDNR                      
100200     MOVE KONV-IDORDNR7          TO UT-ORAD-IDORDNR                       
100300                                                                          
100400     MOVE 'W411550'              TO UT-ORAD-IDCPYTXT                      
100500     PERFORM CDAB-KONV-R55                                                
100600     .                                                                    
100700     EJECT                                                                
100800 CDAB-KONV-R55      SECTION.                                              
100900                                                                          
101000     MOVE R55-IDARTNR         TO UT-ORAD-IDARTNR                          
101100     MOVE R55-REKSIFFR        TO UT-ORAD-REKSIFFR                         
101200     PERFORM S12-W009KSIF                                                 
101300     MOVE R55-KVBEART         TO UT-ORAD-KVBEART                          
101400     IF R55-PRARTNTO NUMERIC                                              
101500        MOVE R55-PRARTNTO (1:7) TO W-PRARTNTO-HEL                         
101600        MOVE R55-PRARTNTO (8:2) TO W-PRARTNTO-DEC                         
101700        MOVE W-PRARTNTO         TO UT-ORAD-PRARTNTO                       
101800     ELSE                                                                 
101900        MOVE SPACE            TO UT-ORAD-PRARTNTO                         
102000     END-IF                                                               
102100     MOVE SPACE               TO UT-ORAD-TITPO                            
102200                                                                          
102300     IF R53-IDTYP = 'R53'                                                 
102400*       KONVERTERA TIINLEV FORMAT AVV TILL FORMAT AAVV -                  
102500*       SÄTT SEKEL TILL DAGENS SEKEL. OM TIINLEV ÅRTAL < DAGENS           
102600*       ÅRTAL, ADDERA 1 TILL SEKEL.                                       
102700        MOVE R55-TIINLEV      TO W-TIINLEV-AVV                            
102800        IF W-TIINLEV-VV NOT = SPACE AND                                   
102900           R55-TIINLEV > ZERO                                             
103000           MOVE DKONV-A1      TO W-TIINLEV-A1                             
103100           IF W-TIINLEV-A2 < DKONV-A2                                     
103200              ADD +1          TO W-TIINLEV-A1                             
103300           END-IF                                                         
103400*          KONVERTERA TIINLEV FORMAT AAVV TILL FORMAT AAMMDD              
103500*          MHA MODUL WDATKONV. FLYTTA TILL TITPO.                         
103600           MOVE 'AAVV  '      TO DAT-KDDATFORM                            
103700           MOVE W-TIINLEV     TO DAT-I-TIDATUM                            
103800           CALL WDATKONV USING DAT-KDDATFORM                              
103900                               DAT-I-TIDATUM                              
104000                               DAT-O-TIDATUM                              
104100                               DAT-KDSVAR                                 
104200           IF DAT-KDSVAR NOT = SPACE                                      
104300              MOVE ' FEL RETURKOD FRÅN WDATKONV I CDAB-KONV-'             
104400                                  TO FELTEXT                              
104500              CALL FELLOG                                                 
104600           ELSE                                                           
104700              MOVE DAT-TIAAMMDD  TO UT-ORAD-TITPO                         
104800           END-IF                                                         
104900        ELSE                                                              
105000           MOVE SPACE         TO UT-ORAD-TITPO                            
105100        END-IF                                                            
105200     END-IF                                                               
105300                                                                          
105400     MOVE R55-KDKVBRYT        TO UT-ORAD-KDKVBRYT                         
105500     MOVE SPACE               TO UT-ORAD-FLINVEST                         
105600                                 UT-ORAD-KDVRINFO                         
105700                                 UT-ORAD-FLRESTN                          
105800                                 UT-ORAD-KDDSP                            
105900                                 UT-ORAD-FLSLATT                          
106000     IF UT-OHUV-IDFTG = SPACE                                             
106100        MOVE R55-IDKONTO      TO W-IDKONTO                                
106200        MOVE R55-IDKST        TO W-IDKST                                  
106300        IF W-IDKONTO-POS1 NUMERIC                                         
106400          PERFORM S18-OMVANDLA-FORETAGSKOD                                
106500        ELSE                                                              
106600           MOVE SPACE         TO UT-OHUV-IDFTG                            
106700        END-IF                                                            
106800        MOVE W-IDKONTO        TO UT-ORAD-IDKONTO                          
106900        MOVE W-IDKST          TO UT-ORAD-IDKST                            
107000     ELSE                                                                 
107100        MOVE R55-IDKONTO      TO UT-ORAD-IDKONTO                          
107200        MOVE R55-IDKST        TO UT-ORAD-IDKST                            
107300     END-IF                                                               
107400     IF R55-IDRADNR-VO NUMERIC AND                                        
107500        R55-IDRADNR-VO > ZERO                                             
107600        MOVE R55-IDRADNR-VO   TO UT-ORAD-BERADREF                         
107700     ELSE                                                                 
107800        MOVE SPACE            TO UT-ORAD-BERADREF                         
107900     END-IF                                                               
108000     MOVE UT-OHUV-BEKUNDRF    TO UT-ORAD-BEVOLREF                         
108100     .                                                                    
108200     EJECT                                                                
108300                                                                          
108400 CE-SKAPA-ORDERRADER SECTION.                                             
108500     SKIP2                                                                
108600*    POSTTYP R75 BEHANDLAS                                                
108700                                                                          
108800     MOVE SWS-AREA               TO R75-AREA                              
108900                                    R75-AREA-X                            
109000     IF R75-IDKUNDNR-X = SPACE                                            
109100        MOVE ZERO                TO R75-IDKUNDNR                          
109200     END-IF                                                               
109300     MOVE SPACE                  TO CHAR-AREA                             
109400     MOVE R75-IDDISTR            TO CHAR-IDDISTR                          
109500     MOVE R75-IDKUNDNR           TO CHAR-IDKUNDNR                         
109600     MOVE R75-IDORDNR            TO CHAR-IDORDNR5                         
109700     PERFORM S10-KONV-ORDERIDENT                                          
109800                                                                          
109900*    KONTROLLERA ATT ORDERHUVUD HAR SKAPATS                               
110000     IF (SPAR-IDDISTR  NOT = KONV-IDDISTR  OR                             
110100        SPAR-IDKUNDNR NOT = KONV-IDKUNDNR OR                              
110200        SPAR-IDORDNR7 NOT = KONV-IDORDNR7)                                
110300                                                                          
110400        MOVE SWS-SORT-POST    TO FEL-POST                                 
110500        MOVE 'ORDERHUVUD SAKNAS/FEL FÖR ORDERRADPOST '                    
110600                              TO FEL-POST-TEXT                            
110700        PERFORM S04-SKRIV-W41203                                          
110800     ELSE                                                                 
110900                                                                          
111000*       SKAPA ORDERRADPOSTER - GEMENSAMMA FÄLT                            
111100*       UT-OHUV-IDSYSTEM I SAMMA POS I UT SOM UT-ORAD-IDSYSTEM            
111200        MOVE SPACE               TO UT-ORAD-AREA                          
111300        MOVE 'OVR '              TO UT-ORAD-IDSYSTEM                      
111400        MOVE DATUM-AAMMDD        TO UT-ORAD-TIFILDAT                      
111500        MOVE W-TIKLOCK           TO UT-ORAD-TIKLOCK                       
111600        MOVE 'W411575'           TO UT-ORAD-IDCPYTXT                      
111700        MOVE 'R55'               TO UT-ORAD-IDPTYP                        
111800        MOVE KONV-IDDISTR        TO UT-ORAD-IDDISTR                       
111900        MOVE KONV-IDKUNDNR       TO UT-ORAD-IDKUNDNR                      
112000        MOVE KONV-IDORDNR7       TO UT-ORAD-IDORDNR                       
112100        MOVE SPACE               TO UT-ORAD-TITPO                         
112200                                    UT-ORAD-KDKVBRYT                      
112300                                    UT-ORAD-FLINVEST                      
112400                                    UT-ORAD-KDVRINFO                      
112500                                    UT-ORAD-IDKONTO                       
112600                                    UT-ORAD-IDKST                         
112700                                    UT-ORAD-BERADREF                      
112800                                    UT-ORAD-FLRESTN                       
112900                                    UT-ORAD-KDDSP                         
113000                                    UT-ORAD-FLSLATT                       
113100                                    UT-ORAD-PRARTNTO                      
113200        MOVE UT-OHUV-BEKUNDRF    TO UT-ORAD-BEVOLREF                      
113300                                                                          
113400        IF R75-IDARTNR-1 NUMERIC AND                                      
113500           R75-IDARTNR-1 > ZERO                                           
113600           MOVE R75-IDARTNR-1    TO UT-ORAD-IDARTNR                       
113700           MOVE R75-REKSIFFR-1   TO UT-ORAD-REKSIFFR                      
113800           PERFORM S12-W009KSIF                                           
113900           MOVE R75-KVBEART-1    TO UT-ORAD-KVBEART                       
114000           PERFORM S03-SKRIV-W41202                                       
114100        END-IF                                                            
114200                                                                          
114300        IF R75-IDARTNR-2 NUMERIC AND                                      
114400           R75-IDARTNR-2 > ZERO                                           
114500           MOVE R75-IDARTNR-2    TO UT-ORAD-IDARTNR                       
114600           MOVE R75-REKSIFFR-2   TO UT-ORAD-REKSIFFR                      
114700           PERFORM S12-W009KSIF                                           
114800           MOVE R75-KVBEART-2    TO UT-ORAD-KVBEART                       
114900           PERFORM S03-SKRIV-W41202                                       
115000        END-IF                                                            
115100                                                                          
115200        IF R75-IDARTNR-3 NUMERIC AND                                      
115300           R75-IDARTNR-3 > ZERO                                           
115400           MOVE R75-IDARTNR-3    TO UT-ORAD-IDARTNR                       
115500           MOVE R75-REKSIFFR-3   TO UT-ORAD-REKSIFFR                      
115600           PERFORM S12-W009KSIF                                           
115700           MOVE R75-KVBEART-3    TO UT-ORAD-KVBEART                       
115800           PERFORM S03-SKRIV-W41202                                       
115900        END-IF                                                            
116000                                                                          
116100        IF R75-IDARTNR-4 NUMERIC AND                                      
116200           R75-IDARTNR-4 > ZERO                                           
116300           MOVE R75-IDARTNR-4    TO UT-ORAD-IDARTNR                       
116400           MOVE R75-REKSIFFR-4   TO UT-ORAD-REKSIFFR                      
116500           PERFORM S12-W009KSIF                                           
116600           MOVE R75-KVBEART-4    TO UT-ORAD-KVBEART                       
116700           PERFORM S03-SKRIV-W41202                                       
116800        END-IF                                                            
116900                                                                          
117000     END-IF                                                               
117100                                                                          
117200     PERFORM S06-SORT-RETURN                                              
117300     .                                                                    
117400     EJECT                                                                
117500                                                                          
117600 CF-SKAPA-ORDERRAD-RX5 SECTION.                                           
117700     SKIP2                                                                
117800*    POSTTYP RX5 BEHANDLAS                                                
117900                                                                          
118000     MOVE SWS-AREA               TO RX5-AREA                              
118100     MOVE SPACE                  TO CHAR-AREA                             
118200     MOVE RX5-IDDISTR            TO CHAR-IDDISTR                          
118300     MOVE RX5-IDKUNDNR           TO CHAR-IDKUNDNR                         
118400     MOVE RX5-IDORDNR7           TO CHAR-IDORDNR7                         
118500     PERFORM S10-KONV-ORDERIDENT                                          
118600                                                                          
118700*    KONTROLLERA OM ORDERHUVUD HAR SKAPATS                                
118800     IF (SPAR-IDDISTR  NOT = KONV-IDDISTR  OR                             
118900        SPAR-IDKUNDNR NOT = KONV-IDKUNDNR OR                              
119000        SPAR-IDORDNR7 NOT = KONV-IDORDNR7)                                
119100        PERFORM CFA-SKAPA-ORDERHUVUD-RX5                                  
119200     END-IF                                                               
119300                                                                          
119400     IF PREV-HUVUD-OK                                                     
119500                                                                          
119600*       SKAPA ORDERRADPOST                                                
119700        MOVE SPACE                  TO UT-ORAD-AREA                       
119800        MOVE RX5-IDDISTR            TO TEST-IDDISTR                       
119900        IF DIST23-TPO1                                                    
120000           MOVE 'OVR '              TO UT-ORAD-IDSYSTEM                   
120100        ELSE                                                              
120200           MOVE 'VR  '              TO UT-ORAD-IDSYSTEM                   
120300        END-IF                                                            
120400                                                                          
120500        MOVE DATUM-AAMMDD           TO UT-ORAD-TIFILDAT                   
120600        MOVE W-TIKLOCK              TO UT-ORAD-TIKLOCK                    
120700        MOVE 'W412RX5'              TO UT-ORAD-IDCPYTXT                   
120800        MOVE 'R55'                  TO UT-ORAD-IDPTYP                     
120900        MOVE KONV-IDDISTR           TO UT-ORAD-IDDISTR                    
121000        MOVE KONV-IDKUNDNR          TO UT-ORAD-IDKUNDNR                   
121100        MOVE KONV-IDORDNR7          TO UT-ORAD-IDORDNR                    
121200        MOVE RX5-IDARTNR            TO UT-ORAD-IDARTNR                    
121300        MOVE RX5-REKSIFFR           TO UT-ORAD-REKSIFFR                   
121400        PERFORM S12-W009KSIF                                              
121500        MOVE RX5-KVBEART            TO UT-ORAD-KVBEART                    
121600        MOVE RX5-TITPO              TO UT-ORAD-TITPO                      
121700        MOVE SPACE                  TO UT-ORAD-FLRESTN                    
121800                                       UT-ORAD-FLINVEST                   
121900                                       UT-ORAD-IDKONTO                    
122000                                       UT-ORAD-IDKST                      
122100                                       UT-ORAD-BEVOLREF                   
122200        MOVE SPACE                  TO UT-ORAD-PRARTNTO                   
122300        MOVE RX5-KDKVBRYT           TO UT-ORAD-KDKVBRYT                   
122400        MOVE RX5-KDVRINFO           TO UT-ORAD-KDVRINFO                   
122500        MOVE RX5-BERADREF           TO UT-ORAD-BERADREF                   
122600        MOVE '1'                    TO UT-ORAD-KDDSP                      
122700        MOVE RX5-FLSLATT            TO UT-ORAD-FLSLATT                    
122800                                                                          
122900        PERFORM S03-SKRIV-W41202                                          
123000     END-IF                                                               
123100                                                                          
123200     PERFORM S06-SORT-RETURN                                              
123300     .                                                                    
123400     EJECT                                                                
123500                                                                          
123600 CFA-SKAPA-ORDERHUVUD-RX5 SECTION.                                        
123700                                                                          
123800*    SPARA ORDERHUVUDIDENTITET                                            
123900     MOVE KONV-IDDISTR        TO SPAR-IDDISTR                             
124000     MOVE KONV-IDKUNDNR       TO SPAR-IDKUNDNR                            
124100     MOVE KONV-IDORDNR7       TO SPAR-IDORDNR7                            
124200                                                                          
124300*    SKRIV FÖRGÅENDE ORDERHUVUDPOST                                       
124400     IF UT-OHUV-IDPTYP NOT = SPACE                                        
124500        IF PREV-HUVUD-OK                                                  
124600           PERFORM S02-SKRIV-W41202                                       
124700        ELSE                                                              
124800           MOVE SWS-SORT-POST    TO FEL-POST                              
124900           MOVE 'ORDERRADENS NYCKELFÄLT EJ NUMERISKT'                     
125000                                 TO FEL-POST-TEXT                         
125100           PERFORM S04-SKRIV-W41203                                       
125200        END-IF                                                            
125300     END-IF                                                               
125400                                                                          
125500*    SKAPA ORDERHUVUDPOST                                                 
125600     MOVE JA                     TO PREV-HUVUD-SW                         
125700     INITIALIZE                     UT-OHUV-AREA                          
125800     MOVE ZERO           TO UT-OHUV-TIREPDAT                              
125900     IF KONV-IDDISTR NOT NUMERIC OR KONV-IDORDNR7 NOT NUMERIC             
126000        MOVE NEJ                 TO PREV-HUVUD-SW                         
126100     END-IF                                                               
126200     MOVE RX5-IDDISTR            TO TEST-IDDISTR                          
126300     IF DIST23-TPO1                                                       
126400        MOVE 'OVR '              TO UT-OHUV-IDSYSTEM                      
126500     ELSE                                                                 
126600        MOVE 'VR  '              TO UT-OHUV-IDSYSTEM                      
126700     END-IF                                                               
126800     MOVE DATUM-AAMMDD           TO UT-OHUV-TIFILDAT                      
126900     ADD +1                      TO W-TIKLOCK                             
127000     MOVE W-TIKLOCK              TO UT-OHUV-TIKLOCK                       
127100     MOVE 'W412RX5'              TO UT-OHUV-IDCPYTXT                      
127200     MOVE 'R50'                  TO UT-OHUV-IDPTYP                        
127300     MOVE KONV-IDDISTR           TO UT-OHUV-IDDISTR                       
127400     MOVE UT-OHUV-IDDISTR        TO TEST-IDDISTR                          
127500     MOVE KONV-IDKUNDNR          TO UT-OHUV-IDKUNDNR                      
127600     MOVE KONV-IDORDNR7          TO UT-OHUV-IDORDNR                       
127700     MOVE 'R'                    TO UT-OHUV-KDFAKTYP                      
127710     IF UT-OHUV-IDSYSTEM = 'VR  '                                         
127720        MOVE KONV-IDDISTR        TO  W-WDB2-IDDISTR                       
127721        MOVE KONV-IDKUNDNR       TO  W-WDB2-IDKUNDNR                      
127722                                                                          
127723        PERFORM IMS-GU-WDB201                                             
127724                                                                          
127725        IF SEGMENT-FINNS                                                  
127726           MOVE GMT-KDGENFAK     TO  UT-OHUV-KDFAKTYP                     
127727        END-IF                                                            
127740     END-IF                                                               
127800     MOVE SPACE                  TO UT-OHUV-TIRFS                         
127900                                    UT-OHUV-BEKUNDRF                      
128000                                    UT-OHUV-TIKUNDRF                      
128100                                    UT-OHUV-FLRESTN                       
128200                                    UT-OHUV-BELAGINS                      
128300                                    UT-OHUV-BEGODSM                       
128400                                    UT-OHUV-ADGODSM                       
128500                                    UT-OHUV-KDROPACK                      
128600                                    UT-OHUV-IDKONTO                       
128700                                    UT-OHUV-IDKST                         
128800                                    UT-OHUV-IDANALYS                      
128900                                    UT-OHUV-IDFTG                         
129000                                    UT-OHUV-BEVARREF                      
129100                                    UT-OHUV-KDTULLVE                      
129200                                    UT-OHUV-KDNOTES                       
129300                                    UT-OHUV-KDFRAKT                       
129400                                    UT-OHUV-IDDC                          
129500                                    UT-OHUV-FLLSBOK                       
129600                                    UT-OHUV-IDBILREG                      
129700                                    UT-OHUV-IDCISNR                       
129800                                    UT-OHUV-IDVIN                         
129900                                    UT-OHUV-BEMEKAN                       
130000                                    UT-OHUV-BETELNR-TACD                  
130100                                    UT-OHUV-FLFPLOCK                      
130200                                    UT-OHUV-TETACDBO                      
130300                                                                          
130400     MOVE ZERO                   TO UT-OHUV-IDDEPT                        
130500                                    UT-OHUV-IDGROSS                       
130600                                    UT-OHUV-TIHHMM                        
130700     MOVE SPACE                  TO UT-OHUV-FLAUTFAK                      
130800     MOVE NEJ                    TO UT-OHUV-FLAUTPAC                      
130900                                    UT-OHUV-FLEMBORD                      
131000                                    UT-OHUV-FLOVRLEV                      
131100     MOVE RX5-IDORDNR7           TO W-IDORDNR7                            
131200     MOVE RX5-KDORDKL            TO UT-OHUV-KDORDKL                       
131300     MOVE RX5-KDTPOTYP           TO UT-OHUV-KDTPOTYP                      
131400     IF RX5-KDTPOTYP = 1                                                  
131500        MOVE SPACE               TO UT-OHUV-TITPO                         
131600     ELSE                                                                 
131700        MOVE RX5-TITPO           TO UT-OHUV-TITPO                         
131800     END-IF                                                               
131900                                                                          
132000     PERFORM CFAA-SATT-FRAKTKOD-VR                                        
132100     .                                                                    
132200     EJECT                                                                
132300 CFAA-SATT-FRAKTKOD-VR SECTION.                                           
132400                                                                          
132500*------------------------------- FRAKTER                                  
132600     EVALUATE TRUE                                                        
132700     WHEN DIST25-ISRAEL-FRAKT                                             
132800        IF (W-IDORDNR7 NOT < '0088000' AND                                
132900            W-IDORDNR7 NOT > '0088499')                                   
133000*                                FLYGFRAKT                                
133100           MOVE '17'             TO UT-OHUV-KDFRAKT                       
133200        END-IF                                                            
133300        IF (W-IDORDNR7 NOT < '0088500' AND                                
133400            W-IDORDNR7 NOT > '0088999')                                   
133500*                                BÅTFRAKT                                 
133600           MOVE '43'             TO UT-OHUV-KDFRAKT                       
133700        END-IF                                                            
133800     END-EVALUATE                                                         
133900     .                                                                    
134000     EJECT                                                                
134100 CG-SKAPA-ORDERRAD-RX3 SECTION.                                           
134200     SKIP2                                                                
134300*    POSTTYP RX3 BEHANDLAS                                                
134400                                                                          
134500     MOVE SWS-AREA               TO RX3-AREA                              
134600                                    RX3-AREA-X                            
134700     IF RX3-IDKUNDNR-X = SPACE                                            
134800        MOVE ZERO                TO RX3-IDKUNDNR                          
134900     END-IF                                                               
135000     MOVE SPACE                  TO CHAR-AREA                             
135100     MOVE RX3-IDDISTR            TO CHAR-IDDISTR                          
135200                                    DIST35-IDDISTR                        
135300     MOVE RX3-IDKUNDNR           TO CHAR-IDKUNDNR                         
135400     MOVE RX3-IDORDNR7           TO CHAR-IDORDNR7                         
135500     PERFORM S10-KONV-ORDERIDENT                                          
135600                                                                          
135700*    KONTROLLERA OM ORDERHUVUD HAR SKAPATS                                
135800     IF (SPAR-IDDISTR  NOT = KONV-IDDISTR  OR                             
135900        SPAR-IDKUNDNR NOT = KONV-IDKUNDNR OR                              
136000        SPAR-IDORDNR7 NOT = KONV-IDORDNR7)                                
136100        PERFORM CGA-SKAPA-ORDERHUVUD-RX3                                  
136200     END-IF                                                               
136300                                                                          
136400     IF PREV-HUVUD-OK                                                     
136500                                                                          
136600*       SKAPA ORDERRADPOST                                                
136700        MOVE SPACE                  TO UT-ORAD-AREA                       
136800        MOVE 'OVR '                 TO UT-ORAD-IDSYSTEM                   
136900        MOVE DATUM-AAMMDD           TO UT-ORAD-TIFILDAT                   
137000        MOVE W-TIKLOCK              TO UT-ORAD-TIKLOCK                    
137100        MOVE 'W412RX3'              TO UT-ORAD-IDCPYTXT                   
137200        MOVE 'R55'                  TO UT-ORAD-IDPTYP                     
137300        MOVE KONV-IDDISTR           TO UT-ORAD-IDDISTR                    
137400        MOVE KONV-IDKUNDNR          TO UT-ORAD-IDKUNDNR                   
137500        MOVE KONV-IDORDNR7          TO UT-ORAD-IDORDNR                    
137600                                                                          
137700        MOVE RX3-IDARTNR            TO UT-ORAD-IDARTNR                    
137800        MOVE RX3-REKSIFFR           TO UT-ORAD-REKSIFFR                   
137900        PERFORM S12-W009KSIF                                              
138000        MOVE RX3-KVBEART            TO UT-ORAD-KVBEART                    
138100        IF RX3-TITPO = ZERO                                               
138200           MOVE SPACE               TO UT-ORAD-TITPO                      
138300        ELSE                                                              
138400           MOVE RX3-TITPO           TO UT-ORAD-TITPO                      
138500        END-IF                                                            
138600        MOVE SPACE                  TO UT-ORAD-FLRESTN                    
138700                                       UT-ORAD-IDKONTO                    
138800                                       UT-ORAD-IDKST                      
138900                                       UT-ORAD-BEVOLREF                   
139000                                       UT-ORAD-KDVRINFO                   
139100                                       UT-ORAD-KDDSP                      
139200                                       UT-ORAD-FLSLATT                    
139300        MOVE SPACE                  TO UT-ORAD-PRARTNTO                   
139400        IF RX3-KDKVBRYT = 5                                               
139500           MOVE SPACE               TO UT-ORAD-KDKVBRYT                   
139600           MOVE JA                  TO UT-ORAD-FLINVEST                   
139700        ELSE                                                              
139800           MOVE RX3-KDKVBRYT        TO UT-ORAD-KDKVBRYT                   
139900           MOVE SPACE               TO UT-ORAD-FLINVEST                   
140000        END-IF                                                            
140100        MOVE RX3-BERADREF           TO UT-ORAD-BERADREF                   
140200        MOVE RX3-ADLAGOMR-CD        TO UT-ORAD-ADLAGOMR-CD                
140300        MOVE RX3-ADGANG-CD          TO UT-ORAD-ADGANG-CD                  
140400        MOVE RX3-ADPLATS-CD         TO UT-ORAD-ADPLATS-CD                 
140500                                                                          
140600        PERFORM S03-SKRIV-W41202                                          
140700     END-IF                                                               
140800                                                                          
140900     PERFORM S06-SORT-RETURN                                              
141000     .                                                                    
141100     EJECT                                                                
141200                                                                          
141300 CGA-SKAPA-ORDERHUVUD-RX3 SECTION.                                        
141400                                                                          
141500*    SPARA ORDERHUVUDIDENTITET                                            
141600     MOVE KONV-IDDISTR        TO SPAR-IDDISTR                             
141700     MOVE KONV-IDKUNDNR       TO SPAR-IDKUNDNR                            
141800     MOVE KONV-IDORDNR7       TO SPAR-IDORDNR7                            
141900                                                                          
142000*    SKRIV FÖRGÅENDE ORDERHUVUDPOST                                       
142100     IF UT-OHUV-IDPTYP NOT = SPACE                                        
142200        IF PREV-HUVUD-OK                                                  
142300           PERFORM S02-SKRIV-W41202                                       
142400        ELSE                                                              
142500           MOVE SWS-SORT-POST    TO FEL-POST                              
142600           MOVE 'ORDERRADENS NYCKELFÄLT EJ NUMERISKT'                     
142700                                 TO FEL-POST-TEXT                         
142800           PERFORM S04-SKRIV-W41203                                       
142900        END-IF                                                            
143000     END-IF                                                               
143100                                                                          
143200*    SKAPA ORDERHUVUDPOST                                                 
143300     MOVE JA                     TO PREV-HUVUD-SW                         
143400     INITIALIZE                     UT-OHUV-AREA                          
143500     MOVE ZERO           TO UT-OHUV-TIREPDAT                              
143600     IF KONV-IDDISTR NOT NUMERIC OR KONV-IDKUNDNR NOT NUMERIC             
143700                                 OR KONV-IDORDNR7 NOT NUMERIC             
143800        MOVE NEJ                 TO PREV-HUVUD-SW                         
143900     END-IF                                                               
144000                                                                          
144100     MOVE 'OVR '                 TO UT-OHUV-IDSYSTEM                      
144200     MOVE DATUM-AAMMDD           TO UT-OHUV-TIFILDAT                      
144300     ADD +1                      TO W-TIKLOCK                             
144400     MOVE W-TIKLOCK              TO UT-OHUV-TIKLOCK                       
144500     MOVE 'W412RX3'              TO UT-OHUV-IDCPYTXT                      
144600     MOVE 'R50'                  TO UT-OHUV-IDPTYP                        
144700     MOVE KONV-IDDISTR           TO UT-OHUV-IDDISTR                       
144800     MOVE KONV-IDKUNDNR          TO UT-OHUV-IDKUNDNR                      
144900     MOVE KONV-IDORDNR7          TO UT-OHUV-IDORDNR                       
145000     MOVE RX3-KDFAKTYP           TO UT-OHUV-KDFAKTYP                      
145100     MOVE RX3-BEKUNDRF           TO UT-OHUV-BEKUNDRF                      
146200     IF RX3-KDFRAKT = 0                                                   
146300         MOVE SPACE            TO UT-OHUV-KDFRAKT                         
146400     ELSE                                                                 
146500         MOVE RX3-KDFRAKT      TO UT-OHUV-KDFRAKT                         
146600     END-IF                                                               
147100     IF RX3-TITPO = ZERO OR RX3-KDTPOTYP = 1                              
147200        MOVE SPACE               TO UT-OHUV-TITPO                         
147300     ELSE                                                                 
147400        MOVE RX3-TITPO           TO UT-OHUV-TITPO                         
147500     END-IF                                                               
147600     MOVE RX3-IDORDNR7           TO W-IDORDNR7                            
147700     MOVE RX3-KDORDKL            TO UT-OHUV-KDORDKL                       
147800     MOVE RX3-KDTPOTYP           TO UT-OHUV-KDTPOTYP                      
147900     MOVE SPACE                  TO UT-OHUV-TIRFS                         
148000                                    UT-OHUV-TIKUNDRF                      
148100                                    UT-OHUV-FLRESTN                       
148200                                    UT-OHUV-BELAGINS                      
148300                                    UT-OHUV-BEGODSM                       
148400                                    UT-OHUV-ADGODSM                       
148500                                    UT-OHUV-KDROPACK                      
148600                                    UT-OHUV-IDKONTO                       
148700                                    UT-OHUV-IDKST                         
148800                                    UT-OHUV-IDANALYS                      
148900                                    UT-OHUV-IDFTG                         
149000                                    UT-OHUV-BEVARREF                      
149100                                    UT-OHUV-KDTULLVE                      
149200                                    UT-OHUV-KDNOTES                       
149300                                    UT-OHUV-IDDC                          
149400                                    UT-OHUV-FLLSBOK                       
149500                                    UT-OHUV-IDBILREG                      
149600                                    UT-OHUV-IDCISNR                       
149700                                    UT-OHUV-IDVIN                         
149800                                    UT-OHUV-BEMEKAN                       
149900                                    UT-OHUV-BETELNR-TACD                  
150000                                    UT-OHUV-FLFPLOCK                      
150100                                    UT-OHUV-TETACDBO                      
150200     MOVE SPACE                  TO UT-OHUV-FLAUTFAK                      
150300     MOVE ZERO                   TO UT-OHUV-IDDEPT                        
150400                                    UT-OHUV-IDGROSS                       
150500                                    UT-OHUV-TIHHMM                        
150600     MOVE NEJ                    TO UT-OHUV-FLAUTPAC                      
150700                                    UT-OHUV-FLEMBORD                      
150800                                    UT-OHUV-FLOVRLEV                      
150900     .                                                                    
151000     EJECT                                                                
151100                                                                          
151200 CH-SKAPA-ORDERRAD-VG4 SECTION.                                           
151300     SKIP2                                                                
151400*    POSTTYP VG4 BEHANDLAS                                                
151500                                                                          
151600     MOVE SWS-AREA               TO VG4-AREA                              
151700     MOVE SPACE                  TO CHAR-AREA                             
151800     MOVE VG4-IDDISTR            TO CHAR-IDDISTR                          
151900     MOVE VG4-IDKUNDNR           TO CHAR-IDKUNDNR                         
152000     MOVE VG4-IDORDNR7           TO CHAR-IDORDNR7                         
152100     PERFORM S10-KONV-ORDERIDENT                                          
152200                                                                          
152300*    KONTROLLERA OM ORDERHUVUD HAR SKAPATS                                
152400     IF (SPAR-IDDISTR  NOT = KONV-IDDISTR  OR                             
152500        SPAR-IDKUNDNR NOT = KONV-IDKUNDNR OR                              
152600        SPAR-IDORDNR7 NOT = KONV-IDORDNR7)                                
152700        PERFORM CHA-SKAPA-ORDERHUVUD-VG4                                  
152800     END-IF                                                               
152900                                                                          
153000     IF PREV-HUVUD-OK                                                     
153100                                                                          
153200*       SKAPA ORDERRADPOST                                                
153300        MOVE SPACE                  TO UT-ORAD-AREA                       
153400        MOVE VG4-IDDISTR            TO TEST-IDDISTR                       
153500        MOVE 'SOFT'                 TO UT-ORAD-IDSYSTEM                   
153600                                                                          
153700        MOVE DATUM-AAMMDD           TO UT-ORAD-TIFILDAT                   
153800        MOVE W-TIKLOCK              TO UT-ORAD-TIKLOCK                    
153900        MOVE 'W463VG4'              TO UT-ORAD-IDCPYTXT                   
154000        MOVE 'R55'                  TO UT-ORAD-IDPTYP                     
154100        MOVE KONV-IDDISTR           TO UT-ORAD-IDDISTR                    
154200        MOVE KONV-IDKUNDNR          TO UT-ORAD-IDKUNDNR                   
154300        MOVE KONV-IDORDNR7          TO UT-ORAD-IDORDNR                    
154400                                                                          
154500        MOVE VG4-IDARTPRE   TO CIA-IDARTPRE-IN                            
154600        MOVE VG4-IDARTBET   TO CIA-IDARTBET-IN                            
154700        CALL W009CIA USING CIA-W009CIA                                    
154800        IF CIA-KDSVAR = 'F'                                               
154900           MOVE SWS-SORT-POST    TO FEL-POST                              
155000           MOVE 'ARTIKELNUMMER EJ GODKÄND           '                     
155100                                 TO FEL-POST-TEXT                         
155200           PERFORM S04-SKRIV-W41203                                       
155300        ELSE                                                              
155400           MOVE CIA-IDARTNR         TO UT-ORAD-IDARTNR                    
155500        END-IF                                                            
155600                                                                          
155700        MOVE ZERO                   TO UT-ORAD-REKSIFFR                   
155800        PERFORM S12-W009KSIF                                              
155900        MOVE VG4-KVLEVART           TO UT-ORAD-KVBEART                    
156000        MOVE SPACE                  TO UT-ORAD-FLRESTN                    
156100                                       UT-ORAD-TITPO                      
156200                                       UT-ORAD-FLINVEST                   
156300                                       UT-ORAD-IDKONTO                    
156400                                       UT-ORAD-IDKST                      
156500                                       UT-ORAD-KDKVBRYT                   
156600                                       UT-ORAD-KDVRINFO                   
156700                                       UT-ORAD-KDDSP                      
156800                                       UT-ORAD-FLSLATT                    
156900        MOVE SPACE                  TO UT-ORAD-PRARTNTO                   
157000*       MOVE VG4-BEKUNDRF-001       TO UT-ORAD-BEVOLREF                   
157100        MOVE VG4-IDARBREF           TO UT-ORAD-BEVOLREF                   
157200                                       UT-ORAD-IDARBREF                   
157300*       MOVE VG4-BERADREF           TO UT-ORAD-BERADREF                   
157400        MOVE VG4-IDKLIENT           TO UT-ORAD-BERADREF                   
157500                                       UT-ORAD-IDKLIENT                   
157600        MOVE VG4-IDBIL              TO UT-ORAD-IDBIL                      
157700        MOVE VG4-IDVIN              TO UT-ORAD-IDVIN                      
157800                                                                          
157900        PERFORM S03-SKRIV-W41202                                          
158000     END-IF                                                               
158100                                                                          
158200     PERFORM S06-SORT-RETURN                                              
158300     .                                                                    
158400     EJECT                                                                
158500                                                                          
158600 CHA-SKAPA-ORDERHUVUD-VG4 SECTION.                                        
158700                                                                          
158800*    SPARA ORDERHUVUDIDENTITET                                            
158900     MOVE KONV-IDDISTR        TO SPAR-IDDISTR                             
159000     MOVE KONV-IDKUNDNR       TO SPAR-IDKUNDNR                            
159100     MOVE KONV-IDORDNR7       TO SPAR-IDORDNR7                            
159200                                                                          
159300*    SKRIV FÖRGÅENDE ORDERHUVUDPOST                                       
159400     IF UT-OHUV-IDPTYP NOT = SPACE                                        
159500        IF PREV-HUVUD-OK                                                  
159600           PERFORM S02-SKRIV-W41202                                       
159700        ELSE                                                              
159800           MOVE SWS-SORT-POST    TO FEL-POST                              
159900           MOVE 'ORDERRADENS NYCKELFÄLT EJ NUMERISKT'                     
160000                                 TO FEL-POST-TEXT                         
160100           PERFORM S04-SKRIV-W41203                                       
160200        END-IF                                                            
160300     END-IF                                                               
160400                                                                          
160500*    SKAPA ORDERHUVUDPOST                                                 
160600     MOVE JA                     TO PREV-HUVUD-SW                         
160700     INITIALIZE                     UT-OHUV-AREA                          
160800     MOVE ZERO           TO UT-OHUV-TIREPDAT                              
160900     IF KONV-IDDISTR NOT NUMERIC OR KONV-IDORDNR7 NOT NUMERIC             
161000        MOVE NEJ                 TO PREV-HUVUD-SW                         
161100     END-IF                                                               
161200     MOVE VG4-IDDISTR            TO TEST-IDDISTR                          
161300     MOVE 'SOFT'                 TO UT-OHUV-IDSYSTEM                      
161400     MOVE DATUM-AAMMDD           TO UT-OHUV-TIFILDAT                      
161500     ADD +1                      TO W-TIKLOCK                             
161600     MOVE W-TIKLOCK              TO UT-OHUV-TIKLOCK                       
161700     MOVE 'W463VG4'              TO UT-OHUV-IDCPYTXT                      
161800     MOVE 'R50'                  TO UT-OHUV-IDPTYP                        
161900     MOVE KONV-IDDISTR           TO UT-OHUV-IDDISTR                       
162000     MOVE UT-OHUV-IDDISTR        TO TEST-IDDISTR                          
162100     MOVE KONV-IDKUNDNR          TO UT-OHUV-IDKUNDNR                      
162200     MOVE KONV-IDORDNR7          TO UT-OHUV-IDORDNR                       
162300     MOVE 'R'                    TO UT-OHUV-KDFAKTYP                      
162400     MOVE VG4-IDARBREF           TO UT-OHUV-BEKUNDRF                      
162500* FÖR KONTOL PÅ DUBLET ORDER ID I W41202                                  
162600     MOVE VG4-IDBIL(4:10)        TO UT-OHUV-BEVARREF                      
162700     MOVE SPACE                  TO UT-OHUV-TIRFS                         
162800                                    UT-OHUV-TIKUNDRF                      
162900                                    UT-OHUV-FLRESTN                       
163000                                    UT-OHUV-BELAGINS                      
163100                                    UT-OHUV-BEGODSM                       
163200                                    UT-OHUV-ADGODSM                       
163300                                    UT-OHUV-KDROPACK                      
163400                                    UT-OHUV-IDKONTO                       
163500                                    UT-OHUV-IDKST                         
163600                                    UT-OHUV-IDANALYS                      
163700                                    UT-OHUV-IDFTG                         
163800                                    UT-OHUV-KDTULLVE                      
163900                                    UT-OHUV-KDNOTES                       
164000                                    UT-OHUV-KDFRAKT                       
164100                                    UT-OHUV-IDDC                          
164200                                    UT-OHUV-FLLSBOK                       
164300                                    UT-OHUV-FLAUTFAK                      
164400                                    UT-OHUV-IDBILREG                      
164500                                    UT-OHUV-IDCISNR                       
164600                                    UT-OHUV-IDVIN                         
164700                                    UT-OHUV-BEMEKAN                       
164800                                    UT-OHUV-BETELNR-TACD                  
164900                                    UT-OHUV-FLFPLOCK                      
165000                                    UT-OHUV-TETACDBO                      
165100     MOVE ZERO                   TO UT-OHUV-IDDEPT                        
165200                                    UT-OHUV-IDGROSS                       
165300                                    UT-OHUV-TIHHMM                        
165400     MOVE NEJ                    TO UT-OHUV-FLAUTPAC                      
165500                                    UT-OHUV-FLEMBORD                      
165600                                    UT-OHUV-FLOVRLEV                      
165700                                                                          
165800     MOVE WS-CDC-11              TO UT-OHUV-IDDC                          
165900                                                                          
166000     MOVE +1                     TO UT-OHUV-KDORDKL                       
166100                                                                          
166200     MOVE '88'                   TO UT-OHUV-KDFRAKT                       
166300                                                                          
166400     MOVE SPACE                  TO UT-OHUV-KDTPOTYP                      
166500     .                                                                    
166600     EJECT                                                                
166700                                                                          
166800 CI-SKAPA-ORDERRAD-DR5 SECTION.                                           
166900     SKIP2                                                                
167000*    POSTTYP DR5 BEHANDLAS                                                
167100                                                                          
167200     MOVE SWS-AREA               TO DR5-AREA                              
167300     MOVE SPACE                  TO CHAR-AREA                             
167400     MOVE DR5-IDDISTR            TO CHAR-IDDISTR                          
167500     MOVE DR5-IDKUNDNR           TO CHAR-IDKUNDNR                         
167600     MOVE DR5-IDORDNR5           TO CHAR-IDORDNR5                         
167700     PERFORM S10-KONV-ORDERIDENT                                          
167800                                                                          
167900*    KONTROLLERA OM ORDERHUVUD HAR SKAPATS                                
168000     IF (SPAR-IDDISTR  NOT = KONV-IDDISTR  OR                             
168100        SPAR-IDKUNDNR NOT = KONV-IDKUNDNR OR                              
168200        SPAR-IDORDNR7 NOT = KONV-IDORDNR7)                                
168300        PERFORM CIA-SKAPA-ORDERHUVUD-DR5                                  
168400     END-IF                                                               
168500                                                                          
168600     IF PREV-HUVUD-OK                                                     
168700                                                                          
168800*       SKAPA ORDERRADPOST                                                
168900        MOVE SPACE                  TO UT-ORAD-AREA                       
169000        MOVE DR5-IDDISTR            TO TEST-IDDISTR                       
169100        MOVE 'OVR '                 TO UT-ORAD-IDSYSTEM                   
169200        MOVE DATUM-AAMMDD           TO UT-ORAD-TIFILDAT                   
169300        MOVE W-TIKLOCK              TO UT-ORAD-TIKLOCK                    
169400        MOVE 'W418DR5'              TO UT-ORAD-IDCPYTXT                   
169500        MOVE 'R55'                  TO UT-ORAD-IDPTYP                     
169600        MOVE KONV-IDDISTR           TO UT-ORAD-IDDISTR                    
169700        MOVE KONV-IDKUNDNR          TO UT-ORAD-IDKUNDNR                   
169800        MOVE KONV-IDORDNR7          TO UT-ORAD-IDORDNR                    
169900                                                                          
170000        MOVE DR5-IDARTNR            TO UT-ORAD-IDARTNR                    
170100        MOVE ZERO                   TO UT-ORAD-REKSIFFR                   
170200        PERFORM S12-W009KSIF                                              
170300        MOVE DR5-PRARTNTO           TO UT-ORAD-PRARTNTO                   
170400        MOVE DR5-PRARTNTO-LOC       TO UT-ORAD-PRARTNTO-LOC               
170500        IF  DR5-PRARTNTO-LOC > 0                                          
170600          MOVE DR5-PRARTNTO-LOC     TO UT-ORAD-PRARTBTO-LOC               
170700          MOVE 0                    TO UT-ORAD-RERAB                      
170800        END-IF                                                            
170900        MOVE DR5-KVBEART            TO UT-ORAD-KVBEART                    
171000        MOVE DR5-IDRAPPNR           TO UT-ORAD-BEVOLREF                   
171100        MOVE DR5-IDRADNR            TO UT-ORAD-BERADREF                   
171200        MOVE SPACE                  TO UT-ORAD-FLRESTN                    
171300                                       UT-ORAD-TITPO                      
171400                                       UT-ORAD-FLINVEST                   
171500                                       UT-ORAD-IDKONTO                    
171600                                       UT-ORAD-IDKST                      
171700                                       UT-ORAD-KDKVBRYT                   
171800                                       UT-ORAD-FLSLATT                    
171900                                       UT-ORAD-KDDSP                      
172000                                       UT-ORAD-KDVAT                      
172100        MOVE '2'                    TO UT-ORAD-KDVRINFO                   
172200                                                                          
172300        PERFORM S03-SKRIV-W41202                                          
172400     END-IF                                                               
172500                                                                          
172600     PERFORM S06-SORT-RETURN                                              
172700     .                                                                    
172800     EJECT                                                                
172900 CIA-SKAPA-ORDERHUVUD-DR5 SECTION.                                        
173000                                                                          
173100*    SPARA ORDERHUVUDIDENTITET                                            
173200     MOVE KONV-IDDISTR        TO SPAR-IDDISTR                             
173300     MOVE KONV-IDKUNDNR       TO SPAR-IDKUNDNR                            
173400     MOVE KONV-IDORDNR7       TO SPAR-IDORDNR7                            
173500                                                                          
173600     PERFORM S19-DIST-KUND-LDC                                            
173700     PERFORM S20-GET-LOCALDATE                                            
173800                                                                          
173900*    SKRIV FÖRGÅENDE ORDERHUVUDPOST                                       
174000     IF UT-OHUV-IDPTYP NOT = SPACE                                        
174100        IF PREV-HUVUD-OK                                                  
174200           PERFORM S02-SKRIV-W41202                                       
174300        ELSE                                                              
174400           MOVE SWS-SORT-POST    TO FEL-POST                              
174500           MOVE 'ORDERRADENS NYCKELFÄLT EJ NUMERISKT'                     
174600                                 TO FEL-POST-TEXT                         
174700           PERFORM S04-SKRIV-W41203                                       
174800        END-IF                                                            
174900     END-IF                                                               
175000                                                                          
175100*    SKAPA ORDERHUVUDPOST                                                 
175200     MOVE JA                     TO PREV-HUVUD-SW                         
175300     INITIALIZE                     UT-OHUV-AREA                          
175400     MOVE ZERO           TO UT-OHUV-TIREPDAT                              
175500     IF KONV-IDDISTR NOT NUMERIC OR KONV-IDORDNR7 NOT NUMERIC             
175600        MOVE NEJ                 TO PREV-HUVUD-SW                         
175700     END-IF                                                               
175800     MOVE 'OVR '                 TO UT-OHUV-IDSYSTEM                      
175900     MOVE DATUM-AAMMDD           TO UT-OHUV-TIFILDAT                      
176000     ADD +1                      TO W-TIKLOCK                             
176100     MOVE W-TIKLOCK              TO UT-OHUV-TIKLOCK                       
176200     MOVE 'W412DR5'              TO UT-OHUV-IDCPYTXT                      
176300     MOVE 'R50'                  TO UT-OHUV-IDPTYP                        
176400     MOVE KONV-IDDISTR           TO UT-OHUV-IDDISTR                       
176500     MOVE KONV-IDKUNDNR          TO UT-OHUV-IDKUNDNR                      
176600     MOVE KONV-IDORDNR7          TO UT-OHUV-IDORDNR                       
176700     MOVE 'R'                    TO UT-OHUV-KDFAKTYP                      
176800     MOVE DATUM-AAMMDD           TO UT-OHUV-TIRFS                         
176900     MOVE DR5-IDRAPPNR           TO UT-OHUV-BEVARREF                      
177000                                    UT-OHUV-BEKUNDRF                      
177100     MOVE SPACE                  TO UT-OHUV-TIKUNDRF                      
177200                                    UT-OHUV-FLRESTN                       
177300                                    UT-OHUV-BELAGINS                      
177400                                    UT-OHUV-BEGODSM                       
177500                                    UT-OHUV-ADGODSM                       
177600                                    UT-OHUV-KDROPACK                      
177700                                    UT-OHUV-IDKONTO                       
177800                                    UT-OHUV-IDKST                         
177900                                    UT-OHUV-IDANALYS                      
178000                                    UT-OHUV-IDFTG                         
178100                                    UT-OHUV-KDTULLVE                      
178200                                    UT-OHUV-KDNOTES                       
178300                                    UT-OHUV-KDFRAKT                       
178400                                    UT-OHUV-KDTPOTYP                      
178500                                    UT-OHUV-TITPO                         
178600                                    UT-OHUV-IDBILREG                      
178700                                    UT-OHUV-IDCISNR                       
178800                                    UT-OHUV-IDVIN                         
178900                                    UT-OHUV-BEMEKAN                       
179000                                    UT-OHUV-BETELNR-TACD                  
179100                                    UT-OHUV-FLFPLOCK                      
179200                                    UT-OHUV-TETACDBO                      
179300                                                                          
179400     MOVE ZERO                   TO UT-OHUV-IDDEPT                        
179500                                    UT-OHUV-IDGROSS                       
179600                                    UT-OHUV-TIHHMM                        
179700     MOVE JA                     TO UT-OHUV-FLAUTFAK                      
179800                                    UT-OHUV-FLAUTPAC                      
179900                                    UT-OHUV-FLOVRLEV                      
180000     MOVE NEJ                    TO UT-OHUV-FLEMBORD                      
180100     MOVE KONV-IDORDNR7          TO W-IDORDNR7                            
180200     MOVE DR5-KDORDKL            TO UT-OHUV-KDORDKL                       
180300     MOVE DR5-IDDC               TO UT-OHUV-IDDC                          
180400     MOVE DR5-FLLSBOK            TO UT-OHUV-FLLSBOK                       
180500     .                                                                    
180600     EJECT                                                                
180700                                                                          
180800 CK-SKAPA-ORDERRAD-EX1 SECTION.                                           
180900     SKIP2                                                                
181000*    POSTTYP EXT BEHANDLAS                                                
181100                                                                          
181200     MOVE SWS-AREA               TO EX1-AREA                              
181300     MOVE SPACE                  TO CHAR-AREA                             
181400     MOVE EX1-IDDISTR            TO CHAR-IDDISTR                          
181500     MOVE EX1-IDKUNDNR           TO CHAR-IDKUNDNR                         
181600     MOVE EX1-IDORDNR7           TO CHAR-IDORDNR7                         
181700     PERFORM S10-KONV-ORDERIDENT                                          
181800                                                                          
181900*    KONTROLLERA OM ORDERHUVUD HAR SKAPATS                                
182000     IF (SPAR-IDDISTR  NOT = KONV-IDDISTR  OR                             
182100        SPAR-IDKUNDNR NOT = KONV-IDKUNDNR OR                              
182200        SPAR-IDORDNR7 NOT = KONV-IDORDNR7)                                
182300        PERFORM CKA-SKAPA-ORDERHUVUD-EX1                                  
182400     END-IF                                                               
182500                                                                          
182600     IF PREV-HUVUD-OK                                                     
182700                                                                          
182800*       SKAPA ORDERRADPOST                                                
182900        MOVE SPACE                  TO UT-ORAD-AREA                       
183000        MOVE EX1-IDDISTR            TO TEST-IDDISTR                       
183100        MOVE 'OVR '                 TO UT-ORAD-IDSYSTEM                   
183200                                                                          
183300        MOVE DATUM-AAMMDD           TO UT-ORAD-TIFILDAT                   
183400        MOVE W-TIKLOCK              TO UT-ORAD-TIKLOCK                    
183500        MOVE 'W412EX1'              TO UT-ORAD-IDCPYTXT                   
183600        MOVE 'R55'                  TO UT-ORAD-IDPTYP                     
183700        MOVE KONV-IDDISTR           TO UT-ORAD-IDDISTR                    
183800        MOVE KONV-IDKUNDNR          TO UT-ORAD-IDKUNDNR                   
183900        MOVE KONV-IDORDNR7          TO UT-ORAD-IDORDNR                    
184000                                                                          
184100        MOVE EX1-IDARTPRE   TO CIA-IDARTPRE-IN                            
184200        MOVE EX1-IDARTBET   TO CIA-IDARTBET-IN                            
184300        CALL W009CIA USING CIA-W009CIA                                    
184400        IF CIA-KDSVAR = 'F'                                               
184500           MOVE SWS-SORT-POST    TO FEL-POST                              
184600           MOVE 'ARTIKELNUMMER EJ GODKÄND           '                     
184700                                 TO FEL-POST-TEXT                         
184800           PERFORM S04-SKRIV-W41203                                       
184900        ELSE                                                              
185000           MOVE CIA-IDARTNR         TO UT-ORAD-IDARTNR                    
185100        END-IF                                                            
185200                                                                          
185300        MOVE ZERO                   TO UT-ORAD-REKSIFFR                   
185400        PERFORM S12-W009KSIF                                              
185500        MOVE EX1-KVBEART            TO UT-ORAD-KVBEART                    
185600        MOVE EX1-BERADREF           TO UT-ORAD-BERADREF                   
185700        MOVE SPACE                  TO UT-ORAD-FLRESTN                    
185800                                       UT-ORAD-TITPO                      
185900                                       UT-ORAD-FLINVEST                   
186000                                       UT-ORAD-IDKONTO                    
186100                                       UT-ORAD-IDKST                      
186200                                       UT-ORAD-KDKVBRYT                   
186300                                       UT-ORAD-KDVRINFO                   
186400                                       UT-ORAD-KDDSP                      
186500                                       UT-ORAD-FLSLATT                    
186600                                       UT-ORAD-BEVOLREF                   
186700                                       UT-ORAD-IDKLIENT                   
186800                                       UT-ORAD-IDARBREF                   
186900                                       UT-ORAD-IDBIL                      
187000                                       UT-ORAD-IDVIN                      
187100                                       UT-ORAD-PRARTNTO                   
187200                                                                          
187300        PERFORM S03-SKRIV-W41202                                          
187400     END-IF                                                               
187500                                                                          
187600     PERFORM S06-SORT-RETURN                                              
187700     .                                                                    
187800     EJECT                                                                
187900                                                                          
188000 CKA-SKAPA-ORDERHUVUD-EX1 SECTION.                                        
188100                                                                          
188200*    SPARA ORDERHUVUDIDENTITET                                            
188300     MOVE KONV-IDDISTR        TO SPAR-IDDISTR                             
188400     MOVE KONV-IDKUNDNR       TO SPAR-IDKUNDNR                            
188500     MOVE KONV-IDORDNR7       TO SPAR-IDORDNR7                            
188600                                                                          
188700*    SKRIV FÖRGÅENDE ORDERHUVUDPOST                                       
188800     IF UT-OHUV-IDPTYP NOT = SPACE                                        
188900        IF PREV-HUVUD-OK                                                  
189000           PERFORM S02-SKRIV-W41202                                       
189100        ELSE                                                              
189200           MOVE SWS-SORT-POST    TO FEL-POST                              
189300           MOVE 'ORDERRADENS NYCKELFÄLT EJ NUMERISKT'                     
189400                                 TO FEL-POST-TEXT                         
189500           PERFORM S04-SKRIV-W41203                                       
189600        END-IF                                                            
189700     END-IF                                                               
189800                                                                          
189900*    SKAPA ORDERHUVUDPOST                                                 
190000     MOVE JA                     TO PREV-HUVUD-SW                         
190100     INITIALIZE                     UT-OHUV-AREA                          
190200     MOVE ZERO           TO UT-OHUV-TIREPDAT                              
190300     IF KONV-IDDISTR NOT NUMERIC OR KONV-IDORDNR7 NOT NUMERIC             
190400        MOVE NEJ                 TO PREV-HUVUD-SW                         
190500     END-IF                                                               
190600     MOVE EX1-IDDISTR            TO TEST-IDDISTR                          
190700     MOVE 'OVR '                 TO UT-OHUV-IDSYSTEM                      
190800     MOVE DATUM-AAMMDD           TO UT-OHUV-TIFILDAT                      
190900     ADD +1                      TO W-TIKLOCK                             
191000     MOVE W-TIKLOCK              TO UT-OHUV-TIKLOCK                       
191100     MOVE 'W412EX1'              TO UT-OHUV-IDCPYTXT                      
191200     MOVE 'R50'                  TO UT-OHUV-IDPTYP                        
191300     MOVE KONV-IDDISTR           TO UT-OHUV-IDDISTR                       
191400     MOVE UT-OHUV-IDDISTR        TO TEST-IDDISTR                          
191500     MOVE KONV-IDKUNDNR          TO UT-OHUV-IDKUNDNR                      
191600     MOVE KONV-IDORDNR7          TO UT-OHUV-IDORDNR                       
191700     MOVE SPACE                  TO UT-OHUV-KDFAKTYP                      
191800     MOVE EX1-KDORDKL            TO UT-OHUV-KDORDKL                       
191900     MOVE EX1-TIBEGPAC           TO UT-OHUV-TIRFS                         
192000     MOVE SPACE                  TO UT-OHUV-BEKUNDRF                      
192100                                    UT-OHUV-TIKUNDRF                      
192200                                    UT-OHUV-FLRESTN                       
192300                                    UT-OHUV-BELAGINS                      
192400                                    UT-OHUV-BEGODSM                       
192500                                    UT-OHUV-ADGODSM                       
192600                                    UT-OHUV-KDROPACK                      
192700                                    UT-OHUV-IDKONTO                       
192800                                    UT-OHUV-IDKST                         
192900                                    UT-OHUV-IDANALYS                      
193000                                    UT-OHUV-IDFTG                         
193100                                    UT-OHUV-KDFRAKT                       
193200                                    UT-OHUV-BEVARREF                      
193300                                    UT-OHUV-KDTULLVE                      
193400                                    UT-OHUV-KDNOTES                       
193500                                    UT-OHUV-IDDC                          
193600                                    UT-OHUV-FLLSBOK                       
193700                                    UT-OHUV-FLAUTFAK                      
193800                                    UT-OHUV-KDTPOTYP                      
193900                                    UT-OHUV-IDBILREG                      
194000                                    UT-OHUV-IDCISNR                       
194100                                    UT-OHUV-IDVIN                         
194200                                    UT-OHUV-BEMEKAN                       
194300                                    UT-OHUV-BETELNR-TACD                  
194400                                    UT-OHUV-FLFPLOCK                      
194500                                    UT-OHUV-TETACDBO                      
194600     MOVE ZERO                   TO UT-OHUV-IDDEPT                        
194700                                    UT-OHUV-IDGROSS                       
194800                                    UT-OHUV-TIHHMM                        
194900     MOVE NEJ                    TO UT-OHUV-FLAUTPAC                      
195000     MOVE NEJ                    TO UT-OHUV-FLEMBORD                      
195100                                    UT-OHUV-FLOVRLEV                      
195200                                                                          
195300     .                                                                    
195400     EJECT                                                                
195500 CL-SKAPA-ORDERRAD-RX7 SECTION.                                           
195600     SKIP2                                                                
195700*    POSTTYP RX7 BEHANDLAS                                                
195800                                                                          
195900     MOVE SWS-AREA               TO RX7-AREA                              
196000     MOVE SPACE                  TO CHAR-AREA                             
196100     MOVE RX7-IDDISTR            TO CHAR-IDDISTR                          
196200     MOVE RX7-IDKUNDNR           TO CHAR-IDKUNDNR                         
196300     MOVE RX7-IDORDNR7           TO CHAR-IDORDNR7                         
196400     PERFORM S10-KONV-ORDERIDENT                                          
196500                                                                          
196600*    KONTROLLERA OM ORDERHUVUD HAR SKAPATS                                
196700     IF (SPAR-IDDISTR  NOT = KONV-IDDISTR  OR                             
196800        SPAR-IDKUNDNR NOT = KONV-IDKUNDNR OR                              
196900        SPAR-IDORDNR7 NOT = KONV-IDORDNR7)                                
197000        PERFORM CLA-SKAPA-ORDERHUVUD-RX7                                  
197100     END-IF                                                               
197200                                                                          
197300     IF PREV-HUVUD-OK                                                     
197400                                                                          
197500*       SKAPA ORDERRADPOST                                                
197600        MOVE SPACE                  TO UT-ORAD-AREA                       
197700        MOVE RX7-IDDISTR            TO TEST-IDDISTR                       
197800        IF DIST23-TPO1                                                    
197900           MOVE 'OVR '              TO UT-ORAD-IDSYSTEM                   
198000        ELSE                                                              
198100           MOVE 'VR  '              TO UT-ORAD-IDSYSTEM                   
198200        END-IF                                                            
198300                                                                          
198400        MOVE DATUM-AAMMDD           TO UT-ORAD-TIFILDAT                   
198500        MOVE W-TIKLOCK              TO UT-ORAD-TIKLOCK                    
198600        MOVE 'W412RX7'              TO UT-ORAD-IDCPYTXT                   
198700        MOVE 'R55'                  TO UT-ORAD-IDPTYP                     
198800        MOVE KONV-IDDISTR           TO UT-ORAD-IDDISTR                    
198900        MOVE KONV-IDKUNDNR          TO UT-ORAD-IDKUNDNR                   
199000        MOVE KONV-IDORDNR7          TO UT-ORAD-IDORDNR                    
199100        MOVE RX7-IDARTNR            TO UT-ORAD-IDARTNR                    
199200        MOVE RX7-REKSIFFR           TO UT-ORAD-REKSIFFR                   
199300        PERFORM S12-W009KSIF                                              
199400        MOVE RX7-KVBEART            TO UT-ORAD-KVBEART                    
199500        MOVE RX7-TITPO              TO UT-ORAD-TITPO                      
199600        MOVE SPACE                  TO UT-ORAD-FLRESTN                    
199700                                       UT-ORAD-PRARTNTO                   
199800                                       UT-ORAD-FLINVEST                   
199900                                       UT-ORAD-IDKONTO                    
200000                                       UT-ORAD-IDKST                      
200100                                       UT-ORAD-BEVOLREF                   
200200        MOVE RX7-KDKVBRYT           TO UT-ORAD-KDKVBRYT                   
200300        MOVE RX7-KDVRINFO           TO UT-ORAD-KDVRINFO                   
200400        MOVE RX7-BERADREF           TO UT-ORAD-BERADREF                   
200500        MOVE '1'                    TO UT-ORAD-KDDSP                      
200600        MOVE RX7-FLSLATT            TO UT-ORAD-FLSLATT                    
200700        MOVE SPACE                  TO UT-ORAD-IDPRQUES                   
200800        MOVE RX7-PRARTNTO-LOC       TO W-PRARTXXX-MED-PUNKT               
200900        MOVE W-PRARTXXX-MED-PUNKT   TO UT-ORAD-PRARTNTO-LOC               
201000        MOVE ZERO                   TO UT-ORAD-PRARTNTO-LOCPREL           
201100        MOVE RX7-PRARTBTO-LOC       TO W-PRARTXXX-MED-PUNKT               
201200        MOVE W-PRARTXXX-MED-PUNKT   TO UT-ORAD-PRARTBTO-LOC               
201300        MOVE RX7-KDVALISO           TO UT-ORAD-KDVALISO                   
201400        MOVE RX7-KDVAT              TO UT-ORAD-KDVAT                      
201500        MOVE RX7-RERAB              TO UT-ORAD-RERAB                      
201600        MOVE RX7-KDRAB              TO UT-ORAD-KDRAB                      
201700        MOVE RX7-BEART-VIPS         TO UT-ORAD-BEART-VIPS                 
201800                                                                          
201900        PERFORM S03-SKRIV-W41202                                          
202000     END-IF                                                               
202100                                                                          
202200     PERFORM S06-SORT-RETURN                                              
202300     .                                                                    
202400     EJECT                                                                
202500                                                                          
202600 CLA-SKAPA-ORDERHUVUD-RX7 SECTION.                                        
202700                                                                          
202800*    SPARA ORDERHUVUDIDENTITET                                            
202900     MOVE KONV-IDDISTR        TO SPAR-IDDISTR                             
203000     MOVE KONV-IDKUNDNR       TO SPAR-IDKUNDNR                            
203100     MOVE KONV-IDORDNR7       TO SPAR-IDORDNR7                            
203200                                                                          
203300*    SKRIV FÖRGÅENDE ORDERHUVUDPOST                                       
203400     IF UT-OHUV-IDPTYP NOT = SPACE                                        
203500        IF PREV-HUVUD-OK                                                  
203600           PERFORM S02-SKRIV-W41202                                       
203700        ELSE                                                              
203800           MOVE SWS-SORT-POST    TO FEL-POST                              
203900           MOVE 'ORDERRADENS NYCKELFÄLT EJ NUMERISKT'                     
204000                                 TO FEL-POST-TEXT                         
204100           PERFORM S04-SKRIV-W41203                                       
204200        END-IF                                                            
204300     END-IF                                                               
204400                                                                          
204500*    SKAPA ORDERHUVUDPOST                                                 
204600     MOVE JA                     TO PREV-HUVUD-SW                         
204700     INITIALIZE                     UT-OHUV-AREA                          
204800     MOVE ZERO           TO UT-OHUV-TIREPDAT                              
204900     IF KONV-IDDISTR NOT NUMERIC OR KONV-IDORDNR7 NOT NUMERIC             
205000        MOVE NEJ                 TO PREV-HUVUD-SW                         
205100     END-IF                                                               
205200     MOVE RX7-IDDISTR            TO TEST-IDDISTR                          
205300     IF DIST23-TPO1                                                       
205400        MOVE 'OVR '              TO UT-OHUV-IDSYSTEM                      
205500     ELSE                                                                 
205600        MOVE 'VR  '              TO UT-OHUV-IDSYSTEM                      
205700     END-IF                                                               
205800     MOVE DATUM-AAMMDD           TO UT-OHUV-TIFILDAT                      
205900     ADD +1                      TO W-TIKLOCK                             
206000     MOVE W-TIKLOCK              TO UT-OHUV-TIKLOCK                       
206100     MOVE 'W412RX7'              TO UT-OHUV-IDCPYTXT                      
206200     MOVE 'R50'                  TO UT-OHUV-IDPTYP                        
206300     MOVE KONV-IDDISTR           TO UT-OHUV-IDDISTR                       
206400     MOVE UT-OHUV-IDDISTR        TO TEST-IDDISTR                          
206500     MOVE KONV-IDKUNDNR          TO UT-OHUV-IDKUNDNR                      
206600     MOVE KONV-IDORDNR7          TO UT-OHUV-IDORDNR                       
206700     MOVE 'R'                    TO UT-OHUV-KDFAKTYP                      
206800     MOVE SPACE                  TO UT-OHUV-TIRFS                         
206900                                    UT-OHUV-BEKUNDRF                      
207000                                    UT-OHUV-TIKUNDRF                      
207100                                    UT-OHUV-FLRESTN                       
207200                                    UT-OHUV-BELAGINS                      
207300                                    UT-OHUV-BEGODSM                       
207400                                    UT-OHUV-ADGODSM                       
207500                                    UT-OHUV-KDROPACK                      
207600                                    UT-OHUV-IDKONTO                       
207700                                    UT-OHUV-IDKST                         
207800                                    UT-OHUV-IDANALYS                      
207900                                    UT-OHUV-IDFTG                         
208000                                    UT-OHUV-BEVARREF                      
208100                                    UT-OHUV-KDTULLVE                      
208200                                    UT-OHUV-KDNOTES                       
208300                                    UT-OHUV-KDFRAKT                       
208400                                    UT-OHUV-IDDC                          
208500                                    UT-OHUV-FLLSBOK                       
208600                                    UT-OHUV-IDBILREG                      
208700                                    UT-OHUV-IDCISNR                       
208800                                    UT-OHUV-IDVIN                         
208900                                    UT-OHUV-BEMEKAN                       
209000                                    UT-OHUV-BETELNR-TACD                  
209100                                    UT-OHUV-FLFPLOCK                      
209200                                    UT-OHUV-TETACDBO                      
209300                                                                          
209400     MOVE ZERO                   TO UT-OHUV-IDDEPT                        
209500                                    UT-OHUV-IDGROSS                       
209600                                    UT-OHUV-TIHHMM                        
209700     MOVE SPACE                  TO UT-OHUV-FLAUTFAK                      
209800     MOVE NEJ                    TO UT-OHUV-FLAUTPAC                      
209900                                    UT-OHUV-FLEMBORD                      
210000                                    UT-OHUV-FLOVRLEV                      
210100     MOVE RX7-IDORDNR7           TO W-IDORDNR7                            
210200     MOVE RX7-KDORDKL            TO UT-OHUV-KDORDKL                       
210300     MOVE RX7-KDTPOTYP           TO UT-OHUV-KDTPOTYP                      
210400     IF RX7-KDTPOTYP = 1                                                  
210500        MOVE SPACE               TO UT-OHUV-TITPO                         
210600     ELSE                                                                 
210700        MOVE RX7-TITPO           TO UT-OHUV-TITPO                         
210800     END-IF                                                               
210900                                                                          
211000     PERFORM CLAA-SATT-FRAKTKOD-VR                                        
211100     .                                                                    
211200     EJECT                                                                
211300 CLAA-SATT-FRAKTKOD-VR SECTION.                                           
211400                                                                          
211500*------------------------------- FRAKTER                                  
211600     EVALUATE TRUE                                                        
211700     WHEN DIST25-PERU-FRAKT                                               
211800        IF (W-IDORDNR7 NOT < '0064001' AND                                
211900            W-IDORDNR7 NOT > '0069000') OR                                
212000           (W-IDORDNR7 NOT < '0084001' AND                                
212100            W-IDORDNR7 NOT > '0089499') OR                                
212200           (W-IDORDNR7 NOT < '0088000' AND                                
212300            W-IDORDNR7 NOT > '0088500') OR                                
212400           (W-IDORDNR7 NOT < '0089000' AND                                
212500            W-IDORDNR7 NOT > '0089500')                                   
212600*                                FLYGFRAKT                                
212700           MOVE '19'             TO UT-OHUV-KDFRAKT                       
212800        END-IF                                                            
212900        IF (W-IDORDNR7 NOT < '0069001' AND                                
213000            W-IDORDNR7 NOT > '0084000') OR                                
213100           (W-IDORDNR7 NOT < '0088501' AND                                
213200            W-IDORDNR7 NOT > '0088999') OR                                
213300           (W-IDORDNR7 NOT < '0089501' AND                                
213400            W-IDORDNR7 NOT > '0089999')                                   
213500*                                BÅTFRAKT                                 
213600           MOVE '43'             TO UT-OHUV-KDFRAKT                       
213700        END-IF                                                            
213800     WHEN DIST25-ISRAEL-FRAKT                                             
213900        IF (W-IDORDNR7 NOT < '0088000' AND                                
214000            W-IDORDNR7 NOT > '0088499')                                   
214100*                                FLYGFRAKT                                
214200           MOVE '17'             TO UT-OHUV-KDFRAKT                       
214300        END-IF                                                            
214400        IF (W-IDORDNR7 NOT < '0088500' AND                                
214500            W-IDORDNR7 NOT > '0088999')                                   
214600*                                BÅTFRAKT                                 
214700           MOVE '43'             TO UT-OHUV-KDFRAKT                       
214800        END-IF                                                            
214900     END-EVALUATE                                                         
215000     .                                                                    
215100     EJECT                                                                
215200 CM-SKAPA-ORDERRAD-TS2 SECTION.                                           
215300     SKIP2                                                                
215400*    POSTTYP TS2 TS3 TS4 TS5 TS6 BEHANDLAS                                
215500     MOVE SWS-AREA               TO TS2-AREA                              
215600     MOVE SPACE                  TO CHAR-AREA                             
215700     MOVE TS2-IDDISTR            TO CHAR-IDDISTR                          
215800     MOVE TS2-IDKUNDNR           TO CHAR-IDKUNDNR                         
215900     MOVE TS2-IDORDNR7           TO CHAR-IDORDNR7                         
216000     PERFORM S10-KONV-ORDERIDENT                                          
216100     PERFORM S19-DIST-KUND-LDC                                            
216200                                                                          
216300*    MOVE TS2-TIBEGPAC        TO UT-OHUV-TIRFS                            
216400     IF TS2-IDPTYP = 'TS6'                                                
216500                                                                          
216600       MOVE SPACE             TO UT-ANNU-AREA                             
216700                                                                          
216800       MOVE 'TACD'            TO UT-ANNU-IDSYSTEM                         
216900       MOVE TS2-IDPTYP        TO UT-ANNU-IDPTYP                           
217000       MOVE KONV-IDDISTR      TO UT-ANNU-IDDISTR                          
217100       MOVE KONV-IDKUNDNR     TO UT-ANNU-IDKUNDNR                         
217200       MOVE KONV-IDORDNR7     TO UT-ANNU-IDORDNR                          
217300                                                                          
217400       MOVE TS2-IDARTPRE   TO CIA-IDARTPRE-IN                             
217500       MOVE TS2-IDARTBET   TO CIA-IDARTBET-IN                             
217600       CALL W009CIA USING CIA-W009CIA                                     
217700       IF CIA-KDSVAR = 'F'                                                
217800          MOVE SWS-SORT-POST    TO FEL-POST                               
217900          MOVE 'ARTIKELNUMMER EJ GODKÄND           '                      
218000                                 TO FEL-POST-TEXT                         
218100          PERFORM S04-SKRIV-W41203                                        
218200       ELSE                                                               
218300           MOVE CIA-IDARTNR         TO UT-ANNU-IDARTNR                    
218400       END-IF                                                             
218500       MOVE TS2-KVBEART       TO UT-ANNU-KVBEART                          
218600       MOVE SPACE             TO UT-ANNU-IDDC                             
218700                                                                          
218800       PERFORM S07-SKRIV-ANNULLATIONSFIL                                  
218900     ELSE                                                                 
219000*    KONTROLLERA OM ORDERHUVUD HAR SKAPATS                                
219100       IF (SPAR-IDDISTR  NOT = KONV-IDDISTR  OR                           
219200          SPAR-IDKUNDNR NOT = KONV-IDKUNDNR OR                            
219300          SPAR-IDORDNR7 NOT = KONV-IDORDNR7)                              
219400          PERFORM CMA-SKAPA-ORDERHUVUD-TS2                                
219500       END-IF                                                             
219600                                                                          
219700       IF PREV-HUVUD-OK                                                   
219800                                                                          
219900*       SKAPA ORDERRADPOST                                                
220000          MOVE SPACE                  TO UT-ORAD-AREA                     
220100          MOVE TS2-IDDISTR            TO TEST-IDDISTR                     
220200          IF GMT-FLLDCKND = JA                                            
220300            IF TS2-IDPTYP  = 'TS2'                                        
220400            OR TS2-IDPTYP  = 'TS3'                                        
220500              MOVE 'LDC '             TO UT-ORAD-IDSYSTEM                 
220600              MOVE TS2-BERADREF       TO UT-ORAD-IDKUNDRF-WIP             
220700            ELSE                                                          
220800              IF TS2-KDORDKL = 1                                          
220900                MOVE TS2-BERADREF     TO UT-ORAD-IDKUNDRF-WIP             
221000              END-IF                                                      
221100              MOVE 'TACD'             TO UT-ORAD-IDSYSTEM                 
221200            END-IF                                                        
221300          ELSE                                                            
221400            MOVE 'OVR '               TO UT-ORAD-IDSYSTEM                 
221500          END-IF                                                          
221600                                                                          
221700          MOVE DATUM-AAMMDD           TO UT-ORAD-TIFILDAT                 
221800          MOVE W-TIKLOCK              TO UT-ORAD-TIKLOCK                  
221900          MOVE 'W412TS2'              TO UT-ORAD-IDCPYTXT                 
222000          MOVE 'R55'                  TO UT-ORAD-IDPTYP                   
222100          MOVE KONV-IDDISTR           TO UT-ORAD-IDDISTR                  
222200          MOVE KONV-IDKUNDNR          TO UT-ORAD-IDKUNDNR                 
222300          MOVE KONV-IDORDNR7          TO UT-ORAD-IDORDNR                  
222400                                                                          
222500          MOVE TS2-IDARTPRE   TO CIA-IDARTPRE-IN                          
222600          MOVE TS2-IDARTBET   TO CIA-IDARTBET-IN                          
222700          CALL W009CIA USING CIA-W009CIA                                  
222800          IF CIA-KDSVAR = 'F'                                             
222900             MOVE SWS-SORT-POST    TO FEL-POST                            
223000             MOVE 'ARTIKELNUMMER EJ GODKÄND           '                   
223100                                   TO FEL-POST-TEXT                       
223200             PERFORM S04-SKRIV-W41203                                     
223300          ELSE                                                            
223400             MOVE CIA-IDARTNR         TO UT-ORAD-IDARTNR                  
223500          END-IF                                                          
223600                                                                          
223700          MOVE ZERO                   TO UT-ORAD-REKSIFFR                 
223800          PERFORM S12-W009KSIF                                            
223900          MOVE TS2-KVBEART            TO UT-ORAD-KVBEART                  
224000          MOVE TS2-BERADREF           TO UT-ORAD-BERADREF                 
224100          MOVE SPACE                  TO UT-ORAD-FLRESTN                  
224200                                         UT-ORAD-TITPO                    
224300                                         UT-ORAD-FLINVEST                 
224400                                         UT-ORAD-IDKONTO                  
224500                                         UT-ORAD-IDKST                    
224600                                         UT-ORAD-KDKVBRYT                 
224700                                         UT-ORAD-KDVRINFO                 
224800                                         UT-ORAD-KDDSP                    
224900                                         UT-ORAD-FLSLATT                  
225000                                         UT-ORAD-BEVOLREF                 
225100                                         UT-ORAD-IDKLIENT                 
225200                                         UT-ORAD-IDARBREF                 
225300                                         UT-ORAD-IDBIL                    
225400                                         UT-ORAD-IDVIN                    
225500                                         UT-ORAD-PRARTNTO                 
225600                                                                          
225700          PERFORM S03-SKRIV-W41202                                        
225800       END-IF                                                             
225900     END-IF                                                               
226000     PERFORM S06-SORT-RETURN                                              
226100     .                                                                    
226200     EJECT                                                                
226300                                                                          
226400 CMA-SKAPA-ORDERHUVUD-TS2 SECTION.                                        
226500                                                                          
226600*    SPARA ORDERHUVUDIDENTITET                                            
226700     MOVE KONV-IDDISTR        TO SPAR-IDDISTR                             
226800     MOVE KONV-IDKUNDNR       TO SPAR-IDKUNDNR                            
226900     MOVE KONV-IDORDNR7       TO SPAR-IDORDNR7                            
227000                                                                          
227100*    SKRIV FÖRGÅENDE ORDERHUVUDPOST                                       
227200     IF UT-OHUV-IDPTYP NOT = SPACE                                        
227300        IF PREV-HUVUD-OK                                                  
227400           PERFORM S02-SKRIV-W41202                                       
227500        ELSE                                                              
227600           MOVE SWS-SORT-POST    TO FEL-POST                              
227700           MOVE 'ORDERRADENS NYCKELFÄLT EJ NUMERISKT'                     
227800                                 TO FEL-POST-TEXT                         
227900           PERFORM S04-SKRIV-W41203                                       
228000        END-IF                                                            
228100     END-IF                                                               
228200                                                                          
228300*    SKAPA ORDERHUVUDPOST                                                 
228400     MOVE JA                     TO PREV-HUVUD-SW                         
228500     INITIALIZE                     UT-OHUV-AREA                          
228600     MOVE ZERO           TO UT-OHUV-TIREPDAT                              
228700     IF KONV-IDDISTR NOT NUMERIC OR KONV-IDORDNR7 NOT NUMERIC             
228800        MOVE NEJ                 TO PREV-HUVUD-SW                         
228900     END-IF                                                               
229000     MOVE TS2-IDDISTR            TO TEST-IDDISTR                          
229100                                                                          
229200     PERFORM S20-GET-LOCALDATE                                            
229300                                                                          
229400*    NEW RULE. FOR ORDERS WITHOUT ORDER CLASS AND WITH REP DATE,          
229500*    SET ORDER CLASS USING FOLLOWING CONDTIONS.                           
229600*    ORDER CLASS AS 1, IF REP DATE <= TODAY                               
229700*    ORDER CLASS AS 2, IF # DAYS BEF. REP DATE < KVDAGAR-RFS-DEF          
229800*    ELSE ORDER CLASS 3.                                                  
229900*    FOR ORDERS WITHOUT ORDER CLASS AND WITHOUT REP DATE, SET             
230000*    ORDER CLASS AS 1.                                                    
230100                                                                          
230200     IF TS2-KDORDKL = ZERO AND                                            
230300        TS2-TIBEGPAC > ZERO                                               
230400                                                                          
230500       MOVE 'YYMMDD'             TO DAYS-KDDATFMT1                        
230600       MOVE WS-TILOKDAT          TO DAYS-TIDATE1                          
230700       MOVE 'YYMMDD'             TO DAYS-KDDATFMT2                        
230800       MOVE TS2-TIBEGPAC         TO DAYS-TIDATE2                          
230900       MOVE ZERO                 TO DAYS-KVDAYS                           
231000       MOVE SPACE                TO DAYS-IDCALEND                         
231100                                                                          
231200       CALL WZ20DAYS USING DAYS-WZ20DAYS                                  
231300                                                                          
231400       IF DAYS-KDRC = ZERO                                                
231500         IF DAYS-KVDAYS <= ZERO                                           
231600           MOVE 1                TO TS2-KDORDKL                           
231700         ELSE                                                             
231800           MOVE GMT-KVDAGAR-RFS-DEF TO WS-KVDAGAR-RFS-DEF                 
231900           PERFORM                                                        
232000           VARYING RFS-IX FROM 1 BY 1                                     
232100             UNTIL RFS-IX > MAX-RFS-IX                                    
232200             IF GMT-IDDC-RFS (RFS-IX) = GMT-IDDC-BULK(1)                  
232300               MOVE GMT-KVDAGAR-RFS (RFS-IX)                              
232400                                 TO WS-KVDAGAR-RFS-DEF                    
232500             END-IF                                                       
232600           END-PERFORM                                                    
232700                                                                          
232800           IF DAYS-KVDAYS < WS-KVDAGAR-RFS-DEF                            
232900             MOVE 2              TO TS2-KDORDKL                           
233000           ELSE                                                           
233100             MOVE 3              TO TS2-KDORDKL                           
233200           END-IF                                                         
233300         END-IF                                                           
233400       END-IF                                                             
233500     END-IF                                                               
233600                                                                          
233700     IF TS2-KDORDKL = ZERO AND                                            
233800        TS2-TIBEGPAC = ZERO                                               
233900       MOVE 1                    TO TS2-KDORDKL                           
234000     END-IF                                                               
234100                                                                          
234200     IF GMT-FLLDCKND = JA                                                 
234300       IF TS2-IDPTYP = 'TS2'                                              
234400       OR TS2-IDPTYP = 'TS3'                                              
234500                                                                          
234600         MOVE 'LDC '             TO UT-OHUV-IDSYSTEM                      
234700                                                                          
234800         IF TS2-KDORDKL = 2                                               
234900         OR TS2-IDPTYP = 'TS3'                                            
235000            MOVE 'PC'            TO UT-OHUV-KDORDTYP-LDC                  
235100         END-IF                                                           
235200                                                                          
235300         IF TS2-KDORDKL = 3                                               
235400         AND TS2-IDPTYP = 'TS2'                                           
235500            IF TS2-KDORDTYP-TACDIS = 'V '                                 
235600               MOVE 'PW'         TO UT-OHUV-KDORDTYP-LDC                  
235700            ELSE                                                          
235800               MOVE 'PC'         TO UT-OHUV-KDORDTYP-LDC                  
235900            END-IF                                                        
236000         END-IF                                                           
236100         MOVE TS2-TIBEGPAC       TO UT-OHUV-TIREPDAT                      
236200       ELSE                                                               
236300         IF TS2-KDORDKL = 1                                               
236400            IF TS2-KDORDTYP-TACDIS = 'VA'                                 
236500               MOVE 'FW'         TO UT-OHUV-KDORDTYP-LDC                  
236600            ELSE                                                          
236700               MOVE 'FC'         TO UT-OHUV-KDORDTYP-LDC                  
236800            END-IF                                                        
236900         END-IF                                                           
237000         MOVE 'TACD'             TO UT-OHUV-IDSYSTEM                      
237100       END-IF                                                             
237200     ELSE                                                                 
237300       MOVE 'OVR '               TO UT-OHUV-IDSYSTEM                      
237400     END-IF                                                               
237500     MOVE DATUM-AAMMDD           TO UT-OHUV-TIFILDAT                      
237600     ADD +1                      TO W-TIKLOCK                             
237700     MOVE W-TIKLOCK              TO UT-OHUV-TIKLOCK                       
237800     MOVE 'W412TS2'              TO UT-OHUV-IDCPYTXT                      
237900     MOVE 'R50'                  TO UT-OHUV-IDPTYP                        
238000     MOVE KONV-IDDISTR           TO UT-OHUV-IDDISTR                       
238100     MOVE UT-OHUV-IDDISTR        TO TEST-IDDISTR                          
238200     MOVE KONV-IDKUNDNR          TO UT-OHUV-IDKUNDNR                      
238300     MOVE KONV-IDORDNR7          TO UT-OHUV-IDORDNR                       
238400     MOVE 'R'                    TO UT-OHUV-KDFAKTYP                      
238500     MOVE TS2-KDORDKL            TO UT-OHUV-KDORDKL                       
238600     MOVE TS2-TIBEGPAC           TO UT-OHUV-TIRFS                         
238700     MOVE TS2-IDDEPT             TO UT-OHUV-IDDEPT                        
238800     MOVE TS2-FLRESTN            TO UT-OHUV-FLRESTN                       
238900     MOVE TS2-BEGMT-RAD1         TO UT-OHUV-BEGODSM-RAD1                  
239000     MOVE TS2-BEGMT-RAD2         TO UT-OHUV-BEGODSM-RAD2                  
239100     MOVE TS2-ADGMT-GATA         TO UT-OHUV-ADGODSM-RAD1                  
239200     MOVE TS2-ADGMT-PADR         TO UT-OHUV-ADGODSM-RAD2                  
239300     IF TS2-IDGROSS = SPACE                                               
239400       MOVE ZERO                 TO UT-OHUV-IDGROSS                       
239500     ELSE                                                                 
239600       MOVE TS2-IDGROSS          TO UT-OHUV-IDGROSS                       
239700     END-IF                                                               
239800     MOVE TS2-IDBILREG           TO UT-OHUV-IDBILREG                      
239900     MOVE TS2-IDCISNR            TO UT-OHUV-IDCISNR                       
240000     MOVE TS2-IDVIN              TO UT-OHUV-IDVIN                         
240100     MOVE TS2-BEMEKAN            TO UT-OHUV-BEMEKAN                       
240200     MOVE TS2-BETELNR-TACD       TO UT-OHUV-BETELNR-TACD                  
240300     MOVE TS2-FLFPLOCK           TO UT-OHUV-FLFPLOCK                      
240400     MOVE TS2-TETACDBO           TO UT-OHUV-TETACDBO                      
240500     MOVE TS2-TIHHMM             TO UT-OHUV-TIHHMM                        
240600     MOVE SPACE                  TO UT-OHUV-BEKUNDRF                      
240700                                    UT-OHUV-TIKUNDRF                      
240800                                    UT-OHUV-BELAGINS                      
240900                                    UT-OHUV-KDROPACK                      
241000                                    UT-OHUV-IDKONTO                       
241100                                    UT-OHUV-IDKST                         
241200                                    UT-OHUV-IDANALYS                      
241300                                    UT-OHUV-IDFTG                         
241400                                    UT-OHUV-KDFRAKT                       
241500                                    UT-OHUV-BEVARREF                      
241600                                    UT-OHUV-KDTULLVE                      
241700                                    UT-OHUV-KDNOTES                       
241800                                    UT-OHUV-IDDC                          
241900                                    UT-OHUV-FLLSBOK                       
242000                                    UT-OHUV-FLAUTFAK                      
242100                                    UT-OHUV-KDTPOTYP                      
242200     MOVE NEJ                    TO UT-OHUV-FLAUTPAC                      
242300                                    UT-OHUV-FLEMBORD                      
242400                                    UT-OHUV-FLOVRLEV                      
242500                                                                          
242600     .                                                                    
242700     EJECT                                                                
242800                                                                          
242900 Z-FINIT    SECTION.                                                      
243000                                                                          
243100     CLOSE  W41201                                                        
243200            W41202                                                        
243300            W41203                                                        
243400            W41209                                                        
243500                                                                          
243600     MOVE 'S'                  TO POSTSUM-OPKOD                           
243700     CALL POSTSUM USING POSTSUM-PARM                                      
243800     .                                                                    
243900     EJECT                                                                
244000                                                                          
244100 S01-LAS-W41201 SECTION.                                                  
244200     SKIP2                                                                
244300     READ W41201 INTO IN-AREA                                             
244400       AT END                                                             
244500          MOVE JA TO W41201-EOF                                           
244600     END-READ                                                             
244700                                                                          
244800     IF W41201-EOF = NEJ                                                  
244900        MOVE 'W41201'    TO POSTSUM-FDNAMN                                
245000        MOVE 'W41201D1'  TO POSTSUM-DDNAMN2                               
245100        MOVE  IN-IDTYP   TO POSTSUM-TRANSTYP                              
245200        CALL POSTSUM USING POSTSUM-PARM                                   
245300     END-IF                                                               
245400     .                                                                    
245500     EJECT                                                                
245600                                                                          
245700 S02-SKRIV-W41202 SECTION.                                                
245800     SKIP2                                                                
245900     IF UT-OHUV-KDFRAKT = ZERO                                            
246000        MOVE SPACE            TO UT-OHUV-KDFRAKT                          
246100     END-IF                                                               
246200     IF UT-OHUV-FLRESTN = '0'                                             
246300        MOVE JA               TO UT-OHUV-FLRESTN                          
246400     END-IF                                                               
246500     IF UT-OHUV-FLRESTN = '1'                                             
246600        MOVE NEJ              TO UT-OHUV-FLRESTN                          
246700     END-IF                                                               
246800                                                                          
246900     WRITE W41202-W412500 FROM UT-OHUV-AREA                               
247000                                                                          
247100     MOVE 'W41202'       TO POSTSUM-FDNAMN                                
247200     MOVE 'W41201D2'     TO POSTSUM-DDNAMN2                               
247300     MOVE UT-OHUV-IDPTYP TO POSTSUM-TRANSTYP                              
247400     CALL POSTSUM USING POSTSUM-PARM                                      
247500     .                                                                    
247600     EJECT                                                                
247700                                                                          
247800 S03-SKRIV-W41202 SECTION.                                                
247900     SKIP2                                                                
248000     WRITE W41202-W412550 FROM UT-ORAD-AREA                               
248100                                                                          
248200     MOVE 'W41202'       TO POSTSUM-FDNAMN                                
248300     MOVE 'W41201D2'     TO POSTSUM-DDNAMN2                               
248400     MOVE UT-ORAD-IDPTYP TO POSTSUM-TRANSTYP                              
248500     CALL POSTSUM USING POSTSUM-PARM                                      
248600     .                                                                    
248700     EJECT                                                                
248800                                                                          
248900 S04-SKRIV-W41203 SECTION.                                                
249000     SKIP2                                                                
249100     WRITE FELPOST       FROM FEL-AREA                                    
249200                                                                          
249300     MOVE 'W41203'       TO POSTSUM-FDNAMN                                
249400     MOVE 'W41201D3'     TO POSTSUM-DDNAMN2                               
249500     MOVE SWS-IDTYP      TO POSTSUM-TRANSTYP                              
249600     CALL POSTSUM USING POSTSUM-PARM                                      
249700     .                                                                    
249800     EJECT                                                                
249900                                                                          
250000 S05-SORT-RELEASE     SECTION.                                            
250100     SKIP2                                                                
250200     RELEASE SORTPOST FROM SWS-AREA                                       
250300     .                                                                    
250400     EJECT                                                                
250500                                                                          
250600 S06-SORT-RETURN      SECTION.                                            
250700     SKIP2                                                                
250800     RETURN SORTFIL INTO SWS-AREA                                         
250900       AT END                                                             
251000            MOVE JA TO SORTFIL-EOF                                        
251100     END-RETURN                                                           
251200     .                                                                    
251300     EJECT                                                                
251400 S07-SKRIV-ANNULLATIONSFIL SECTION.                                       
251500     SKIP2                                                                
251600                                                                          
251700                                                                          
251800     WRITE ANNUPOST     FROM UT-ANNU-AREA                                 
251900                                                                          
252000     MOVE 'W41209'       TO POSTSUM-FDNAMN                                
252100     MOVE 'W41201D4'     TO POSTSUM-DDNAMN2                               
252200     MOVE UT-ANNU-IDPTYP TO POSTSUM-TRANSTYP                              
252300     CALL POSTSUM USING POSTSUM-PARM                                      
252400     .                                                                    
252500     EJECT                                                                
252600                                                                          
252700 S09-SPARA-ORDERIDENT SECTION.                                            
252800     SKIP2                                                                
252900*    SPAR ORDERIDENT I FORMAT FÖRE KONVERTERING                           
253000     MOVE ZERO                   TO KONV-IDDISTR                          
253100                                    KONV-IDKUNDNR                         
253200                                    KONV-IDORDNR7                         
253300     MOVE SWS-IDDISTR            TO SPAR-IDDISTR                          
253400     MOVE SWS-IDKUNDNR           TO SPAR-IDKUNDNR                         
253500     MOVE SWS-IDORDNR            TO SPAR-IDORDNR5                         
253600     MOVE ZERO                   TO SPAR-IDORDNRPOS1-2                    
253700     .                                                                    
253800     EJECT                                                                
253900                                                                          
254000 S10-KONV-ORDERIDENT SECTION.                                             
254100     SKIP2                                                                
254200*    KONVERTERA ORDERIDENTITET TILL FORMAT SOM SKRIV                      
254300*    ORDERTRANSAKTIONER OCH ANVÄNDS VID JÄMFÖRELSE MOT                    
254400*    SENASTE ORDERHUVUD                                                   
254500     MOVE ZERO                   TO KONV-IDDISTR                          
254600                                    KONV-IDKUNDNR                         
254700                                    KONV-IDORDNR7                         
254800                                                                          
254900*    KONVERTERA BLANKA TECKEN TILL NOLL FÖR ORDERTRANSAKTION              
255000     INSPECT CHAR-IDDISTR  REPLACING ALL ' ' BY '0'                       
255100     INSPECT CHAR-IDKUNDNR REPLACING ALL ' ' BY '0'                       
255200     INSPECT CHAR-IDORDNR7 REPLACING ALL ' ' BY '0'                       
255300     MOVE CHAR-IDDISTR     TO KONV-IDDISTR                                
255400     MOVE CHAR-IDKUNDNR    TO KONV-IDKUNDNR                               
255500     MOVE CHAR-IDORDNR7    TO KONV-IDORDNR7                               
255600     .                                                                    
255700     EJECT                                                                
255800                                                                          
255900 S12-W009KSIF  SECTION.                                                   
256000     SKIP2                                                                
256100*    BERÄKNA KONTROLLSIFFRA FÖR ARTIKELNR                                 
256200     IF UT-ORAD-REKSIFFR = SPACE OR                                       
256300        UT-ORAD-REKSIFFR = '0' OR                                         
256400        UT-ORAD-REKSIFFR NOT NUMERIC                                      
256500        IF UT-ORAD-IDARTNR NUMERIC                                        
256600           MOVE UT-ORAD-IDARTNR  TO REK-IDARTNR                           
256700           MOVE 0                TO REK-REKSIFFR                          
256800           MOVE 9                TO REK-LNGD                              
256900           CALL W009KSIF USING REK-IDARTNR                                
257000                               REK-LNGD                                   
257100                               REK-REKSIFFR                               
257200           MOVE REK-REKSIFFR     TO UT-ORAD-REKSIFFR                      
257300        END-IF                                                            
257400     END-IF                                                               
257500     .                                                                    
257600     EJECT                                                                
257700                                                                          
257800                                                                          
257900 S17-KONTROLL-POSTTYP SECTION.                                            
258000     SKIP2                                                                
258100     PERFORM UNTIL SORTFIL-EOF = JA OR                                    
258200        SWS-IDTYP = 'R50' OR                                              
258300        SWS-IDTYP = 'R51' OR                                              
258400        SWS-IDTYP = 'R52' OR                                              
258500        SWS-IDTYP = 'R53' OR                                              
258600        SWS-IDTYP = 'R55' OR                                              
258700        SWS-IDTYP = 'R75' OR                                              
258800        SWS-IDTYP = 'VG4' OR                                              
258900        SWS-IDTYP = 'TS2' OR                                              
259000        SWS-IDTYP = 'TS3' OR                                              
259100        SWS-IDTYP = 'TS4' OR                                              
259200        SWS-IDTYP = 'TS5' OR                                              
259300        SWS-IDTYP = 'TS6' OR                                              
259400        SWS-IDTYP = 'EXT' OR                                              
259500        SWS-IDTYP = 'RX3' OR                                              
259600        SWS-IDTYP = 'RX5' OR                                              
259700        SWS-IDTYP = 'RX7' OR                                              
259800        SWS-IDTYP = 'DR5'                                                 
259900        MOVE SWS-SORT-POST    TO FEL-POST                                 
260000        MOVE 'POSTTYP ÄR EJ GODKÄND '                                     
260100                              TO FEL-POST-TEXT                            
260200        PERFORM S04-SKRIV-W41203                                          
260300        MOVE 'FEL'            TO SWS-IDTYP                                
260400        PERFORM S06-SORT-RETURN                                           
260500     END-PERFORM                                                          
260600     .                                                                    
260700     EJECT                                                                
260800 S18-OMVANDLA-FORETAGSKOD SECTION.                                        
260900     SKIP2                                                                
261000     IF W-IDKONTO-POS1 = 9                                                
261100        IF W-IDKST2 = 99                                                  
261200           MOVE 09            TO UT-OHUV-IDFTG                            
261300        ELSE                                                              
261400           MOVE 90            TO UT-OHUV-IDFTG                            
261500        END-IF                                                            
261600     ELSE                                                                 
261700        MOVE ZERO                   TO W-IDFTG1                           
261800        IF W-IDKONTO-POS1 = 5                                             
261900           IF W-IDKST2 = 57                                               
262000              MOVE 57               TO UT-OHUV-IDFTG                      
262100           ELSE                                                           
262200              MOVE 5                TO W-IDFTG2                           
262300              MOVE W-IDFTG          TO UT-OHUV-IDFTG                      
262400           END-IF                                                         
262500        ELSE                                                              
262600           IF W-IDKONTO-POS1 = 7                                          
262700              IF W-IDKST2 = 78                                            
262800                 MOVE 07            TO UT-OHUV-IDFTG                      
262900              ELSE                                                        
263000                 IF W-IDKST2 = 70 OR 71 OR 72                             
263100                    MOVE 75         TO UT-OHUV-IDFTG                      
263200                 ELSE                                                     
263300                    MOVE 76         TO UT-OHUV-IDFTG                      
263400                 END-IF                                                   
263500              END-IF                                                      
263600           ELSE                                                           
263700             MOVE W-IDKONTO-POS1      TO W-IDFTG2                         
263800             MOVE W-IDFTG             TO UT-OHUV-IDFTG                    
263900           END-IF                                                         
264000        END-IF                                                            
264100     END-IF                                                               
264200     .                                                                    
264300     EJECT                                                                
264400 S19-DIST-KUND-LDC SECTION.                                               
264500                                                                          
264600     MOVE KONV-IDDISTR        TO  W-WDB2-IDDISTR                          
264700     MOVE KONV-IDKUNDNR       TO  W-WDB2-IDKUNDNR                         
264800                                                                          
264900     PERFORM IMS-GU-WDB201                                                
265000                                                                          
265100     IF SEGMENT-SAKNAS                                                    
265200        MOVE NEJ              TO  GMT-FLLDCKND                            
265300     END-IF                                                               
265400                                                                          
265500     .                                                                    
265600     EJECT                                                                
265700                                                                          
265800 S20-GET-LOCALDATE SECTION.                                               
265900                                                                          
266000     MOVE ALL '+'                TO MSGI-WMSGINIT                         
266100     MOVE '013'                  TO MSGI-KDCALL                           
266200     MOVE 'WIDDC   '             TO MSGI-IDUSER                           
266300     MOVE GMT-IDDC-BULK(1)       TO MSGI-IDUSER(6:2)                      
266400     MOVE 'W412'                 TO MSGI-IDTRANS                          
266500                                                                          
266600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
266700                                                                          
266800     MOVE MSGI-TILOKDAT          TO WS-TILOKDAT                           
266900     .                                                                    
267000     EJECT                                                                
267100                                                                          
267200* --- IMS SEKTIONER ---                                                   
267300 IMS-GU-WDB201      SECTION.                                              
267400                                                                          
267500     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
267600          DELIMITED BY SIZE INTO SSA1                                     
267700     MOVE '  GE' TO GODK-STATUSKODER                                      
267800     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
267900     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
268000     PERFORM IMS-STATUSKONTROLL                                           
268100     .                                                                    
268200     EJECT                                                                
268300 IMS-STATUSKONTROLL SECTION.                                              
268400                                                                          
268500     SET STATUS-IX TO 1                                                   
268600     SEARCH GODK-STATUS                                                   
268700       AT END                                                             
268800         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT-STR                     
268900         DISPLAY FELTEXT                                                  
269000         CALL FELLOG                                                      
269100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
269200         CONTINUE                                                         
269300     END-SEARCH                                                           
269400     .                                                                    
