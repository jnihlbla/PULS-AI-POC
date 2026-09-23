000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W4252000.                                        
000300 AUTHOR                  STAFFAN EDSTRÖM.                                 
000400 DATE-WRITTEN.           JANUARI, 1976.                                   
000500                                                                          
000600                                                                          
000700     REMARKS.                                                             
000800*                        W42540                                           
000900*                        POSTERNA KOMPLETTERAS                            
001000*                        MED DIVERSE ELEMENT OCH SKRIVS                   
001100*                        UT PÅ W42599 SOM GÅR VIA W42550 TILL VR          
001200*                        SYSTEMET. VILKA INPOSTER SOM                     
001300*                        SKAPAR VILKA UTPOSTER FRAMGÅR                    
001400*                        NEDAN.                                           
001500*            OBS                                                          
001600*                        YTTERLIGARE KOMPLETTERING SKER                   
001700*                        W42520 INNAN TRANS GÅR TILL VR                   
001800*                            PT 685 SKAPAR PT SU6                         
001900*                            PT 686 SKAPAR PT SU7                         
002000*                            PT 687 SKAPAR PT SU8 SU9 SUH                 
002100*                            PT 688 SKAPAR PT SUB                         
002200*                            PT SUA SKAPAR PT SUA                         
002300*                                                                         
002400*            INDATA      W42540  ORDERINFORMATION TILL                    
002500*                                VR-SYSTEMEN.                             
002600*                            PT SU1  ORDERRAD                             
002700*                            PT SU2  ORDERBEKRÄFTELSE, ERSÄTTNING         
002800*                            PT SU3  ORDERBEKRÄFTELSE, AVVIKELSE          
002900*                                    I LEVERERAT ANTAL                    
003000*                            PT SU4  ORDERBEKRÄFTELSE,                    
003100*                                    RESTNOTERING                         
003200*                            PT SU5  RESTORDERRELEASE                     
003300*                            PT SU6  FAKTURAHUVUD                         
003400*                            PT SU7  ORDERHUVUD 1 FAKTURA                 
003500*                            PT SU8  FAKTURARAD DEL 1                     
003600*                            PT SU9  FAKTURARAD DEL 2                     
003700*                            PT SUA  BIPACKAD RESTORDER                   
003800*                            PT SUB  KOLLI-INFORMATION                    
003900*                            PT SUD  ?????                                
004000*                            PT SUH  SOFTWARE INFORMATION                 
004100*                                                                         
004200*                                FAKTURA                                  
004300*                            PT 685  FAKTURAHUVUD                         
004400*                            PT 686  ORDERHUVUD                           
004500*                            PT 687  FAKTURARAD                           
004600*                            PT 688  KOLLI-INFORMATION                    
004700*                                                                         
004800*            UTDATA      W42599  ORDERINFORMATION TILL                    
004900*                                VR-SYSTEMEN.                             
005000*                            PT SU1  ORDERRAD                             
005100*                            PT SU2  ORDERBEKRÄFTELSE, ERSÄTTNING         
005200*                            PT SU3  ORDERBEKRÄFTELSE, AVVIKELSE          
005300*                                    I LEVERERAT ANTAL                    
005400*                            PT SU4  ORDERBEKRÄFTELSE,                    
005500*                                    RESTNOTERING                         
005600*                            PT SU5  RESTORDERRELEASE                     
005700*                            PT SU6  FAKTURAHUVUD                         
005800*                            PT SU7  ORDERHUVUD 1 FAKTURA                 
005900*                            PT SU8  FAKTURARAD DEL 1                     
006000*                            PT SU9  FAKTURARAD DEL 2                     
006100*                            PT SUA  BIPACKAD RESTORDER                   
006200*                            PT SUB  KOLLI-INFORMATION                    
006300*                            PT SUD  ?????                                
006400*                            PT SUH  SOFTWARE INFORMATION                 
006500*                                                                         
006600*                                                                         
006700*            COPYTEXTER                                                   
006800*                        W425685   W425SU1   W425PSU8                     
006900*                        W425686   W425SU2   W425PSU9                     
007000*                        W425687   W425SU3   W425SUA                      
007100*                                  W425SU4   W425SUB                      
007200*                                  W425SU5   W425SUC                      
007300*                                  W425SU6   W425SUD                      
007400*                                  W425SU7   W425PSUH                     
007500*                                                                         
007600*            SUBRUTINER  DATKORT                                          
007700*                        POSTSUM                                          
007800*                        W009KSIF                                         
007900     EJECT                                                                
008000 ENVIRONMENT DIVISION.                                                    
008100                                                                          
008200 INPUT-OUTPUT SECTION.                                                    
008300 FILE-CONTROL.                                                            
008400     SELECT W42520 ASSIGN TO UT-S-W42520D1.                               
008500     SELECT W42599 ASSIGN TO UT-S-W42520D2.                               
008600*- - - - - - - - - - - - SORTFIL:                                         
008700     SELECT SORTFIL ASSIGN TO UT-S-W42520DS.                              
008800     EJECT                                                                
008900 DATA DIVISION.                                                           
009000                                                                          
009100 FILE SECTION.                                                            
009200 FD  W42599                                                               
009300     RECORDING F                                                          
009400     BLOCK 0 RECORDS                                                      
009500     LABEL RECORD STANDARD.                                               
009600*01  UTPOST        -COPY W425PSU8 -L.                                     
009700     SKIP3                                                                
009800 FD  W42520                                                               
009900     RECORDING V                                                          
010000     BLOCK 0 RECORDS                                                      
010100     LABEL RECORD STANDARD.                                               
010200*01  INPOST-425 -COPY W425687 -L.                                         
010300     SKIP3                                                                
010400     EJECT                                                                
010500 SD  SORTFIL                                                              
010600     RECORDING F.                                                         
010700*01  POST   -COPY W425W099   -PRE SORT-                                   
010800*03  -COPY W425687     -L.                                                
010900     EJECT                                                                
011000 WORKING-STORAGE SECTION.                                                 
011100                                                                          
011200                                                                          
011300*    -- CHECKED BY WY2000                                                 
011400 77  JA                  PIC X           VALUE 'J'.                       
011500 77  NEJ                 PIC X           VALUE 'N'.                       
011600 77  EOF-W42520          PIC X           VALUE 'N'.                       
011700 77  EOF-SORTFIL         PIC X(1)        VALUE 'N'.                       
011800 77  BERAKNINGSFALT-ART  PIC 9(9).                                        
011900 77  FALTLANGD-ART       PIC 9(1)        VALUE 9.                         
012000 77  FALTLANGD-DIST      PIC 9(1)        VALUE 4.                         
012100 77  KONTROLLSIFFRA      PIC 9(1).                                        
012200 77  W-IDDISTR           PIC 9(5).                                        
012300*      --- VALID IDDC CODES                                               
012400*                                                                         
012500*01    -COPY WWDC99                                                       
012500*01    -COPY WWDCKONS                                                     
012600       EJECT                                                              
012700                                                                          
012800*- - - - - - - - - - - - - - - - - - -- - - - - - - - - - -               
012900 01  FILLER              PIC X(16)       VALUE ALL 'A'.                   
013000     SKIP3                                                                
013100                                                                          
013200 01  IDARTNR-KSIFF       PIC 9(9).                                        
013300 01  FILLER REDEFINES IDARTNR-KSIFF.                                      
013400     03  IDARTNR-DISP    PIC 9(8).                                        
013500     03  REKSIFFR-DISP   PIC 9(1).                                        
013600                                                                          
013700 01  WS-IDKUNDRF-RO      PIC X(10).                                       
013800 01  FILLER REDEFINES WS-IDKUNDRF-RO.                                     
013900     03 WS-IDKUNDRF-RO-1 PIC 9(5).                                        
014000     03 FILLER           PIC X(5).                                        
014100     SKIP2                                                                
014200 01  WS-IDKUNDRF         PIC X(10).                                       
014300 01  FILLER REDEFINES WS-IDKUNDRF.                                        
014400     03 WS-IDKUNDRF1-5   PIC 9(5).                                        
014500     03 FILLER           PIC X(5).                                        
014600     SKIP2                                                                
014700 01  W-IDORDNR           PIC 9(5).                                        
014800 01  W-IDORDNR-1-5       REDEFINES W-IDORDNR.                             
014900     03  W-IDORDNR-1-2   PIC 9(2).                                        
015000     03  W-IDORDNR-3-5   PIC 9(3).                                        
015100     EJECT                                                                
015200 01  TEST-IDDISTR        PIC 9(5)  COMP-3.                                
015300*01  FILLER -COPY WWDIST03 -RED TEST-IDDISTR.                             
015400     EJECT                                                                
015500 01  TEST-ARTIKEL        PIC 9(9)    COMP-3.                              
015600*01 FILLER  -COPY WWART04  -RED  TEST-ARTIKEL.                            
015700     EJECT                                                                
015800 01  SKRIVPOST.                                                           
015900*    03  -COPY W425PSU8 -PRE SKRIV.                                       
016000                                                                          
016100 01  DATUMFALT.                                                           
016200     03  AAR             PIC 9(2).                                        
016300     03  MAANAD          PIC 9(2).                                        
016400     03  DAG             PIC 9(2).                                        
016500                                                                          
016600 01  GENERELLA-SUBRUTINER.                                                
016700     03  FELLOG          PIC X(8)        VALUE 'FELLOG '.                 
016800     03  ABEND           PIC X(8)        VALUE 'ABEND  '.                 
016900     03  DATKORT         PIC X(8)        VALUE 'DATKORT'.                 
017000     03  POSTSUM         PIC X(8)        VALUE 'POSTSUM'.                 
017100     03  W009KSIF        PIC X(8)        VALUE 'W009KSIF'.                
017200                                                                          
017300 01  RETURKODER.                                                          
017400     03  RKOD                   PIC S9(4)  COMP SYNC VALUE +32.           
017500     03  RKOD-ABEND-UTAN-DUMP   PIC S9(4)  COMP SYNC VALUE +16.           
017600     03  RKOD-ABEND-MED-DUMP    PIC S9(4)  COMP SYNC VALUE +1000.         
017700 01  DATUMKORTAREA.                                                       
017800     03  DATUMPGM            PIC X(6)    VALUE 'W42520'.                  
017900     03  DATUMKTYP           PIC X(6)    VALUE 'WDATUM'.                  
018000*    03  -COPY WDATKORT                                                   
018100 01  FILLER                  PIC X(16)   VALUE 'B'.                       
018200     EJECT                                                                
018300*01  -COPY W0005 -PRE POSTSUM-.                                           
018400     EJECT                                                                
018500* - - - - - - - - - - - - - - - - INPOST-AREA                             
018600                                                                          
018700     SKIP3                                                                
018800 01  FILLER          PIC X(16)   VALUE ALL 'D'.                           
018900     SKIP3                                                                
019000* - I DENNA AREA LÄSER MAN IN ALLA SU- OCH FAKTURA  POSTER FRÅN           
019100* - FRÅN FIL W42520                                                       
019200 01  W-INPOST.                                                            
019300   03 W-IDPTYP       PIC X(3).                                            
019400   03 FILLER         PIC X(204).                                          
019500     SKIP3                                                                
019600*01  -COPY W425685 -PRE W685-.                                            
019700     EJECT                                                                
019800*01  -COPY W425686 -PRE W686-.                                            
019900     EJECT                                                                
020000*01  AREA  -PRE WSORT-  -COPY W425W099                                    
020100*03  -COPY W425687 -PRE W687-.                                            
020200     EJECT                                                                
020300*01  -COPY W425688 -PRE W688-.                                            
020400     EJECT                                                                
020500 01  FILLER               PIC X(16)  VALUE ALL 'F'.                       
020600*01  -COPY W425SU1 -PRE SU1-.                                             
020700     EJECT                                                                
020800*01  -COPY W425SU2 -PRE SU2-.                                             
020900     EJECT                                                                
021000*01  -COPY W425SU3 -PRE SU3-.                                             
021100     EJECT                                                                
021200*01  -COPY W425SU4 -PRE SU4-.                                             
021300     EJECT                                                                
021400*01  -COPY W425SU5 -PRE SU5-.                                             
021500     EJECT                                                                
021600*01  -COPY W425SU6 -PRE SU6-.                                             
021700     EJECT                                                                
021800*01  -COPY W425SU7 -PRE SU7-.                                             
021900     EJECT                                                                
022000*01  -COPY W425PSU8 -PRE SU8-.                                            
022100     EJECT                                                                
022200*01  -COPY W425PSU9 -PRE SU9-.                                            
022300     EJECT                                                                
022400*01  -COPY W425SUA -PRE SUA-.                                             
022500     EJECT                                                                
022600*01  -COPY W425SUB -PRE SUB-.                                             
022700     EJECT                                                                
022800*01  -COPY W425SUC -PRE SUC-.                                             
022900     EJECT                                                                
023000*01  -COPY W425SUD -PRE SUD-.                                             
023100     EJECT                                                                
023200*01  -COPY W425PSUH -PRE SUH-.                                            
023300     EJECT                                                                
023400 PROCEDURE DIVISION.                                                      
023500                                                                          
023600 STYR SECTION.                                                            
023700                                                                          
023800     PERFORM A-HK                                                         
023900                                                                          
024000     SORT SORTFIL ASCENDING                                               
024100                  SORT-IDFAKT                                             
024200                  SORT-IDDISTR                                            
024300                  SORT-IDKUNDNR                                           
024400                  SORT-KDSORT1                                            
024500          INPUT PROCEDURE B-SELEKTERA-BEARBETA                            
024600          OUTPUT PROCEDURE C-BEARBETA-SORTERADE                           
024700     SKIP2                                                                
024800     IF SORT-RETURN > ZERO                                                
024900        DISPLAY '***  W4252000  - FEL VID SORTERING'                      
025000        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
025100     ELSE                                                                 
025200       MOVE ZERO TO RETURN-CODE                                           
025300     END-IF                                                               
025400     PERFORM Z-FINIT                                                      
025500     GOBACK                                                               
025600     .                                                                    
025700     EJECT                                                                
025800 A-HK SECTION.                                                            
025900                                                                          
026000     CALL DATKORT USING DATUMPGM DATUMKTYP DATUMKORT                      
026100     MOVE D-AAR                TO AAR                                     
026200     MOVE D-MAANAD             TO MAANAD                                  
026300     MOVE D-DAG                TO DAG                                     
026400     .                                                                    
026500     EJECT                                                                
026600 B-SELEKTERA-BEARBETA SECTION.                                            
026700                                                                          
026800*                                                                         
026900     SKIP3                                                                
027000     OPEN INPUT  W42520                                                   
027100          OUTPUT W42599                                                   
027200     PERFORM S02-LAES-W42520                                              
027300     PERFORM UNTIL EOF-W42520  = JA                                       
027400         IF W-IDPTYP           = '685' OR '686' OR '687' OR '688'         
027500             PERFORM BA-BEHANDLA-FAKTURA-POSTER                           
027600          ELSE                                                            
027700             PERFORM BB-BEHANDLA-SU-POSTER                                
027800         END-IF                                                           
027900         PERFORM S02-LAES-W42520                                          
028000     END-PERFORM                                                          
028100     .                                                                    
028200     EJECT                                                                
028300*                                                                         
028400 BA-BEHANDLA-FAKTURA-POSTER    SECTION.                                   
028500                                                                          
028600     MOVE W-INPOST             TO W687-W425687                            
028700     IF W687-IDPTYP            = '687'                                    
028800         MOVE W687-IDFAKT      TO WSORT-IDFAKT                            
028900         MOVE W687-IDDISTR     TO WSORT-IDDISTR                           
029000         MOVE W687-IDKUNDNR    TO WSORT-IDKUNDNR                          
029100         MOVE +0               TO WSORT-KDSORT1                           
029200     END-IF                                                               
029300     IF W687-IDPTYP            = '685'                                    
029400         MOVE W687-IDFAKT      TO WSORT-IDFAKT                            
029500         MOVE W687-IDDISTR     TO WSORT-IDDISTR                           
029600         MOVE W687-IDKUNDNR    TO WSORT-IDKUNDNR                          
029700         MOVE +1               TO WSORT-KDSORT1                           
029800     END-IF                                                               
029900     IF W687-IDPTYP            = '686'                                    
030000         MOVE W687-IDFAKT      TO WSORT-IDFAKT                            
030100         MOVE W687-IDDISTR     TO WSORT-IDDISTR                           
030200         MOVE W687-IDKUNDNR    TO WSORT-IDKUNDNR                          
030300         MOVE +1               TO WSORT-KDSORT1                           
030400     END-IF                                                               
030500     IF W687-IDPTYP            = '688'                                    
030600         MOVE W687-W425687     TO W688-W425688                            
030700         MOVE W688-IDFAKT      TO WSORT-IDFAKT                            
030800         MOVE W688-IDDISTR     TO WSORT-IDDISTR                           
030900         MOVE W688-IDKUNDNR    TO WSORT-IDKUNDNR                          
031000         MOVE +1               TO WSORT-KDSORT1                           
031100     END-IF                                                               
031200     RELEASE SORT-POST FROM WSORT-AREA                                    
031300     .                                                                    
031400     EJECT                                                                
031500 BB-BEHANDLA-SU-POSTER     SECTION.                                       
031600                                                                          
031700     PERFORM BBA-KOLLA-IDPTYP                                             
031800     PERFORM S06-SKRIV-UTPOST                                             
031900     .                                                                    
032000     EJECT                                                                
032100 BBA-KOLLA-IDPTYP          SECTION.                                       
032200                                                                          
032300*                                                                         
032400     SKIP3                                                                
032500     EVALUATE TRUE                                                        
032600                                                                          
032700     WHEN W-IDPTYP  =  'SU1'                                              
032800         MOVE W-INPOST            TO SU1-W425SU1-CTX                      
032900         MOVE SU1-W425SU1-CTX     TO SKRIVPOST                            
033000                                                                          
033100     WHEN W-IDPTYP  =  'SU2'                                              
033200         MOVE W-INPOST            TO SU2-W425SU2-CTX                      
033300         MOVE SU2-W425SU2-CTX     TO SKRIVPOST                            
033400                                                                          
033500     WHEN W-IDPTYP  =  'SU3'                                              
033600         MOVE W-INPOST            TO SU3-W425SU3-CTX                      
033700         MOVE SU3-W425SU3-CTX     TO SKRIVPOST                            
033800                                                                          
033900     WHEN W-IDPTYP  =  'SU4'                                              
034000         MOVE W-INPOST            TO SU4-W425SU4-CTX                      
034100         MOVE SU4-W425SU4-CTX     TO SKRIVPOST                            
034200                                                                          
034300     WHEN W-IDPTYP  =  'SU5'                                              
034400         MOVE W-INPOST            TO SU5-W425SU5                          
034500         MOVE SU5-W425SU5         TO SKRIVPOST                            
034600                                                                          
034700                                                                          
034800     WHEN W-IDPTYP  =  'SUA'                                              
034900         MOVE W-INPOST            TO SUA-W425SUA-CTX                      
035000         MOVE SUA-W425SUA-CTX     TO SKRIVPOST                            
035100                                                                          
035200     WHEN W-IDPTYP  =  'SUC'                                              
035300         MOVE W-INPOST            TO SUC-W425SUC-CTX                      
035400         MOVE SUC-W425SUC-CTX     TO SKRIVPOST                            
035500                                                                          
035600     WHEN W-IDPTYP  =  'SUD'                                              
035700         MOVE W-INPOST            TO SUD-W425SUD-CTX                      
035800         MOVE SUD-W425SUD-CTX     TO SKRIVPOST                            
035900                                                                          
036000     END-EVALUATE                                                         
036100     .                                                                    
036200     EJECT                                                                
036300 C-BEARBETA-SORTERADE SECTION.                                            
036400     SKIP2                                                                
036500     PERFORM S18-LAS-SORTFIL                                              
036600     PERFORM UNTIL EOF-SORTFIL = JA                                       
036700         IF W687-IDPTYP = '687'                                           
036800             IF W687-KDFAKTYP = 'R' OR 'G' OR 'N' OR 'K'                  
036900               PERFORM CC-SKAPA-SU8-SU9-SUH-POST                          
037000             END-IF                                                       
037100         END-IF                                                           
037200         IF W687-IDPTYP = '685'                                           
037300             MOVE W687-W425687 TO W685-W425685                            
037400             IF W685-KDFAKTYP = 'R' OR 'G' OR 'N' OR 'K'                  
037500               PERFORM CA-SKAPA-SU6-POST                                  
037600             END-IF                                                       
037700         END-IF                                                           
037800         IF W687-IDPTYP = '686'                                           
037900             MOVE W687-W425687 TO W686-W425686                            
038000             IF W686-KDFAKTYP = 'R' OR 'G' OR 'N' OR 'K'                  
038100               PERFORM CB-SKAPA-SU7-POST                                  
038200             END-IF                                                       
038300         END-IF                                                           
038400         IF W687-IDPTYP = '688'                                           
038500             MOVE W687-W425687 TO W688-W425688                            
038600             PERFORM CD-SKAPA-SUB-POST                                    
038700         END-IF                                                           
038800         PERFORM S18-LAS-SORTFIL                                          
038900     END-PERFORM                                                          
039000     .                                                                    
039100     EJECT                                                                
039200 CA-SKAPA-SU6-POST SECTION.                                               
039300                                                                          
039400     MOVE 'SU6'                TO SU6-IDPTYP                              
039500     MOVE W685-IDDISTR         TO SU6-IDDISTR                             
039600     MOVE W685-IDKUNDNR        TO SU6-IDKUNDNR                            
039700     MOVE W685-IDDC               TO WS-IDDC                              
039800     EVALUATE TRUE                                                        
039900       WHEN SDC-NL                                                        
040000         MOVE 729                 TO SU6-IDSUPPL                          
040300       WHEN LDC-GB-3A                                                     
040400         MOVE 729                 TO SU6-IDSUPPL                          
040500       WHEN SDC-ES                                                        
040600         MOVE 729                 TO SU6-IDSUPPL                          
040700       WHEN SDC-IT                                                        
040800         MOVE 729                 TO SU6-IDSUPPL                          
040900       WHEN SDC-AT                                                        
041000         MOVE 729                 TO SU6-IDSUPPL                          
041100       WHEN OTHER                                                         
041200         MOVE 711                 TO SU6-IDSUPPL                          
041300     END-EVALUATE                                                         
041400     MOVE W685-KDFAKTYP           TO SU6-KDFAKTYP                         
041500     MOVE W685-IDFAKT             TO SU6-IDFAKT                           
041600     MOVE W685-TIFAKT             TO SU6-TIFAKT                           
041700     MOVE W685-SUFKTBEL           TO SU6-SUFKTBEL                         
041800     MOVE W685-PREMBHNT           TO SU6-PREMBHNT                         
041900     MOVE W685-PRFRAKT            TO SU6-PRFRAKT                          
042000     MOVE W685-PRFOERS            TO SU6-PRFOERS                          
042100     MOVE W685-KDFRAKT            TO SU6-KDFRAKT                          
042200     MOVE +01                     TO SU6-KDVALUTA                         
042300     MOVE W685-PRKURS             TO SU6-PRKURS                           
042400     MOVE W685-IDDISTR TO TEST-IDDISTR                                    
042500     IF DIST03-SVERIGE OR W685-PRKURS = +0                                
042600                                                                          
042700         MOVE +0  TO SU6-SUFAKTBEL-UTL                                    
042800     ELSE                                                                 
042900         COMPUTE SU6-SUFAKTBEL-UTL ROUNDED =                              
043000         W685-SUFKTBEL / W685-PRKURS                                      
043100     END-IF                                                               
043200     MOVE W685-TIAAMMDD          TO SU6-TIAAMMDD                          
043300     MOVE W685-TIKLOCK           TO SU6-TIKLOCK                           
043400     MOVE SU6-W425SU6-CTX        TO SKRIVPOST                             
043500     PERFORM S06-SKRIV-UTPOST                                             
043600     .                                                                    
043700     EJECT                                                                
043800 CB-SKAPA-SU7-POST SECTION.                                               
043900                                                                          
044000     MOVE 'SU7'                TO SU7-IDPTYP                              
044100     MOVE W686-IDDISTR         TO SU7-IDDISTR                             
044200     MOVE W686-IDKUNDNR        TO SU7-IDKUNDNR                            
044300     MOVE W686-IDDC             TO WS-IDDC                                
044400     EVALUATE TRUE                                                        
044500       WHEN SDC-NL                                                        
044600         MOVE 729               TO SU7-IDSUPPL                            
044900       WHEN LDC-GB-3A                                                     
045000         MOVE 729               TO SU7-IDSUPPL                            
045100       WHEN SDC-ES                                                        
045200         MOVE 729               TO SU7-IDSUPPL                            
045300       WHEN SDC-IT                                                        
045400         MOVE 729               TO SU7-IDSUPPL                            
045500       WHEN SDC-AT                                                        
045600         MOVE 729               TO SU7-IDSUPPL                            
045700       WHEN OTHER                                                         
045800         MOVE 711               TO SU7-IDSUPPL                            
045900     END-EVALUATE                                                         
046000     MOVE W686-IDFAKT           TO SU7-IDFAKT                             
046100     MOVE W686-IDKUNDRF         TO WS-IDKUNDRF                            
046200     MOVE WS-IDKUNDRF1-5        TO SU7-IDORDNR-002                        
046300     MOVE W686-IDDISTR TO TEST-IDDISTR                                    
046400     IF DIST03-SVERIGE                                                    
046500                                                                          
046600         PERFORM S14-JUSTERA-686-POST                                     
046700     END-IF                                                               
046800                                                                          
046900      MOVE W686-KDORDKL          TO SU7-KDORDER                           
047000      MOVE W686-KVRAD-UPD        TO SU7-KVRAD-UPPD                        
047100      MOVE W686-KVKOLLIO         TO SU7-KVKOLLIO-002                      
047200      MOVE W686-VLORDBTO         TO SU7-VLORD                             
047300      MOVE W686-VKORDBTO         TO SU7-VKORDBTO                          
047400      MOVE W686-KDFAKTYP         TO SU7-KDFAKTYP                          
047500      MOVE W686-IDPRODNR         TO SU7-IDPRODNR                          
047600      MOVE W686-KDREFNOT         TO SU7-KDREFNOT                          
047700      MOVE W686-TIAAMMDD         TO SU7-TIAAMMDD                          
047800      MOVE W686-TIKLOCK          TO SU7-TIKLOCK                           
047900      MOVE SU7-W425SU7-CTX       TO SKRIVPOST                             
048000     PERFORM S06-SKRIV-UTPOST                                             
048100     .                                                                    
048200     EJECT                                                                
048300 CC-SKAPA-SU8-SU9-SUH-POST SECTION.                                       
048400                                                                          
048500     MOVE 'SU8'                TO SU8-IDPTYP                              
048600     MOVE W687-IDDISTR         TO SU8-IDDISTR                             
048700                                  SU9-IDDISTR                             
048800                                  SUH-IDDISTR                             
048900     MOVE W687-IDKUNDNR        TO SU8-IDKUNDNR                            
049000                                  SU9-IDKUNDNR                            
049100                                  SUH-IDKUNDNR                            
049200     MOVE W687-IDDC            TO WS-IDDC                                 
049300     EVALUATE TRUE                                                        
049400       WHEN SDC-NL                                                        
049500         MOVE 729              TO SU8-IDSUPPL                             
049600                                  SU9-IDSUPPL                             
049700                                  SUH-IDSUPPL                             
050200       WHEN LDC-GB-3A                                                     
050300         MOVE 729              TO SU8-IDSUPPL                             
050400                                  SU9-IDSUPPL                             
050500                                  SUH-IDSUPPL                             
050600       WHEN SDC-ES                                                        
050700         MOVE 729              TO SU8-IDSUPPL                             
050800                                  SU9-IDSUPPL                             
050900                                  SUH-IDSUPPL                             
051000       WHEN SDC-IT                                                        
051100         MOVE 729              TO SU8-IDSUPPL                             
051200                                  SU9-IDSUPPL                             
051300                                  SUH-IDSUPPL                             
051400       WHEN SDC-AT                                                        
051500         MOVE 729              TO SU8-IDSUPPL                             
051600                                  SU9-IDSUPPL                             
051700                                  SUH-IDSUPPL                             
051800       WHEN OTHER                                                         
051900         MOVE 711              TO SU8-IDSUPPL                             
052000                                  SU9-IDSUPPL                             
052100                                  SUH-IDSUPPL                             
052200     END-EVALUATE                                                         
052300     MOVE W687-IDFAKT            TO SU8-IDFAKT                            
052400                                    SUH-IDFAKT                            
052500     MOVE W687-IDKUNDRF          TO WS-IDKUNDRF                           
052600     MOVE W687-IDKUNDRF-RO       TO WS-IDKUNDRF-RO                        
052700     MOVE W687-IDDISTR TO TEST-IDDISTR                                    
052800     IF DIST03-SVERIGE                                                    
052900                                                                          
053000         PERFORM S15-JUSTERA-687-POST                                     
053100     END-IF                                                               
053200     MOVE WS-IDKUNDRF1-5         TO SU8-IDORDNR                           
053300                                    SUH-IDORDNR                           
053400     IF WS-IDKUNDRF-RO-1 NUMERIC AND WS-IDKUNDRF-RO-1 > ZERO              
053500         MOVE WS-IDKUNDRF-RO-1   TO SU8-IDRONR                            
053600                                    SUH-IDRONR                            
053700     ELSE                                                                 
053800          MOVE WS-IDKUNDRF1-5    TO SU8-IDRONR                            
053900                                    SUH-IDRONR                            
054000     END-IF                                                               
054100      MOVE W687-IDKOLLI          TO SU8-IDKOLLI                           
054200                                    SUH-IDKOLLI                           
054300      MOVE W687-IDARTNR          TO IDARTNR-DISP                          
054400      MOVE W687-IDARTNR          TO BERAKNINGSFALT-ART                    
054500      PERFORM S08-KONTROLLSIFFERBER-ARTNR                                 
054600      MOVE KONTROLLSIFFRA        TO REKSIFFR-DISP                         
054700      MOVE IDARTNR-KSIFF         TO SU8-IDARTNR                           
054800                                    SUH-IDARTNR                           
054900      MOVE W687-KVLEVART         TO SU8-KVLEVART                          
055000      MOVE W687-PRARTNTO         TO SU8-PRARTNTO                          
055100      MOVE W687-PRARTBTO-EXP     TO SU8-PRARTBTO                          
055200                                    SU9-PRARTBTO                          
055300      MOVE W687-IDFKNGRP         TO SU8-IDFKNGRP                          
055400      MOVE W687-PRARTULL         TO SU8-PRARTULL                          
055500      MOVE W687-KDSRA            TO SU8-KDSRA                             
055600      MOVE W687-VKART            TO SU8-VKART                             
055700      MOVE W687-KDARTURS         TO SU8-KDARTURS                          
055800      IF W687-KDSORT = 'SW'                                               
055900        MOVE +9                  TO SU8-KDVRINFO                          
056000                                    SU9-KDVRINFO                          
056100                                    SUH-KDVRINFO                          
056200      ELSE                                                                
056300        MOVE W687-KDVRINFO       TO SU8-KDVRINFO                          
056400                                    SU9-KDVRINFO                          
056500                                    SUH-KDVRINFO                          
056600      END-IF                                                              
056700      MOVE W687-TIPRIS           TO SU8-TIPRIS                            
056800      MOVE W687-REBPRIS          TO SU8-REBPRIS                           
056900                                    SU9-REBPRIS                           
057000      MOVE W687-KDVIP            TO SU8-KDVIP                             
057100      MOVE W687-BERADREF         TO SU8-BERADREF                          
057200                                    SUH-BERADREF                          
057300      MOVE W687-KDFAKTYP         TO SU8-KDFAKTYP                          
057400                                    SUH-KDFAKTYP                          
057500      MOVE W687-KDORDKL          TO SU8-KDORDKL                           
057600      MOVE W687-KDORDKL          TO SU8-KDORDKL-LEV                       
057700      MOVE W687-FLPRTILL         TO SU8-FLPRTILL                          
057800     MOVE W687-IDKUNDRF-RO       TO WS-IDKUNDRF-RO                        
057900     IF WS-IDKUNDRF-RO-1 NOT NUMERIC OR                                   
058000           WS-IDKUNDRF-RO-1 = ZERO                                        
058100          MOVE 0                 TO SU8-KDRO                              
058200                                    SU9-KDRO                              
058300                                    SUH-KDRO                              
058400     ELSE                                                                 
058500          MOVE 1                 TO SU8-KDRO                              
058600                                    SU9-KDRO                              
058700                                    SUH-KDRO                              
058800     END-IF                                                               
058900     MOVE W687-KDORDKL           TO SU8-KDORDER                           
059000                                    SU9-KDORDER                           
059100                                    SUH-KDORDER                           
059200      MOVE ZERO                  TO SU8-FLQPRIS                           
059300                                    SU9-FLQPRIS                           
059400*                                                                         
059500*                                                                         
059600      MOVE W687-TIAAMMDD         TO SU8-TIAAMMDD                          
059700                                    SU9-TIAAMMDD                          
059800                                    SUH-TIAAMMDD                          
059900      MOVE W687-TIKLOCK          TO SU8-TIKLOCK                           
060000                                    SU9-TIKLOCK                           
060100                                    SUH-TIKLOCK                           
060200      MOVE W687-KDRABATT         TO SU8-KDARTRAB                          
060300*                                                                         
060400      MOVE SU8-W425PSU8           TO SKRIVPOST                            
060500     PERFORM S06-SKRIV-UTPOST                                             
060600      IF W687-KDSORT = 'SW'                                               
060700        MOVE W687-IDARTNR        TO TEST-ARTIKEL                          
060800        IF ART04-SOFTWARE                                                 
060900          CONTINUE                                                        
061000        ELSE                                                              
061100          MOVE 'SUH'             TO SUH-IDPTYP                            
061200          MOVE W687-IDKLIENT     TO SUH-IDKLIENT                          
061300          MOVE W687-IDARBREF     TO SUH-IDARBREF                          
061400          MOVE W687-IDBIL        TO SUH-IDBIL                             
061500          MOVE W687-IDVIN        TO SUH-IDVIN                             
061600          MOVE SUH-W425PSUH      TO SKRIVPOST                             
061700          PERFORM S06-SKRIV-UTPOST                                        
061800        END-IF                                                            
061900      END-IF                                                              
062000      MOVE 'SU9'                 TO SU9-IDPTYP                            
062100      MOVE W687-IDDISTR          TO SU9-IDDISTR                           
062200      MOVE W687-IDKUNDNR         TO SU9-IDKUNDNR                          
062300      MOVE W687-IDARTNR          TO SU9-IDARTNR                           
062400      MOVE KONTROLLSIFFRA        TO SU9-REKSIFFR                          
062500      MOVE W687-BEART            TO SU9-BEART-004                         
062600      MOVE W687-IDFKNGRP         TO SU9-IDFKNGRP                          
062800      IF W687-KDPRODSL = 71                                               
062810        MOVE 93                  TO SU9-KDPRODSL                          
062820      ELSE                                                                
062830        IF W687-KDPRODSL = 72                                             
062840          MOVE 95                TO SU9-KDPRODSL                          
062850        ELSE                                                              
062860          IF W687-KDPRODSL = 73                                           
062870            MOVE 91              TO SU9-KDPRODSL                          
062880          ELSE                                                            
062890            IF W687-KDPRODSL = 74                                         
062891              MOVE 94            TO SU9-KDPRODSL                          
062892            ELSE                                                          
062893              MOVE W687-KDPRODSL TO SU9-KDPRODSL                          
062894            END-IF                                                        
062895          END-IF                                                          
062896        END-IF                                                            
062897      END-IF                                                              
062900      MOVE W687-KDVVKL           TO SU9-KDVVKL                            
063000      MOVE W687-KDRABATT         TO SU9-KDRABATT                          
063100      MOVE W687-KVQPACK          TO SU9-KVQPACK                           
063200      MOVE W687-KDSORT           TO SU9-KDSORT                            
063300      MOVE SU9-W425PSU9-CTX      TO SKRIVPOST                             
063400     PERFORM S06-SKRIV-UTPOST                                             
063500     .                                                                    
063600     EJECT                                                                
063700 CD-SKAPA-SUB-POST SECTION.                                               
063800                                                                          
063900     MOVE 'SUB'                  TO SUB-IDPTYP                            
064000     MOVE W688-IDDISTR           TO SUB-IDDISTR                           
064100     MOVE W688-IDKUNDNR          TO SUB-IDKUNDNR                          
064200     MOVE W688-IDDC              TO WS-IDDC                               
064300     EVALUATE TRUE                                                        
064400       WHEN SDC-NL                                                        
064500         MOVE 729                TO SUB-IDSUPPL                           
064800       WHEN LDC-GB-3A                                                     
064900         MOVE 729                TO SUB-IDSUPPL                           
065000       WHEN SDC-ES                                                        
065100         MOVE 729                TO SUB-IDSUPPL                           
065200       WHEN SDC-IT                                                        
065300         MOVE 729                TO SUB-IDSUPPL                           
065400       WHEN SDC-AT                                                        
065500         MOVE 729                TO SUB-IDSUPPL                           
065600       WHEN OTHER                                                         
065700         MOVE 711                TO SUB-IDSUPPL                           
065800     END-EVALUATE                                                         
065900     MOVE W688-IDLBBET           TO SUB-IDLBBET                           
066000     MOVE W688-IDORDNR           TO SUB-IDORDNR-002                       
066100     MOVE W688-IDFAKT            TO SUB-IDFAKT                            
066200     MOVE W688-TIFAKT            TO SUB-TIFAKT                            
066300     MOVE W688-IDKOLLI           TO SUB-IDKOLLI                           
066400     MOVE W688-VLORDBTO-KOLLI    TO SUB-VLORDBTO-KOLLI                    
066500     MOVE W688-VKORDBTO-KOLLI    TO SUB-VKORDBTO-KOLLI                    
066600     MOVE W688-TIAAMMDD          TO SUB-TIAAMMDD                          
066700     MOVE W688-TIKLOCK           TO SUB-TIKLOCK                           
066800     MOVE SUB-W425SUB-CTX        TO SKRIVPOST                             
066900     PERFORM S06-SKRIV-UTPOST                                             
067000     .                                                                    
067100     EJECT                                                                
067200 Z-FINIT              SECTION.                                            
067300                                                                          
067400     CLOSE W42520                                                         
067500           W42599                                                         
067600     MOVE 'S'                  TO POSTSUM-OPKOD                           
067700     CALL POSTSUM USING POSTSUM-PARM                                      
067800     .                                                                    
067900     EJECT                                                                
068000 S02-LAES-W42520 SECTION.                                                 
068100                                                                          
068200     READ W42520 INTO W-INPOST                                            
068300         AT END MOVE JA TO EOF-W42520                                     
068400     END-READ                                                             
068500                                                                          
068600     IF EOF-W42520 = NEJ                                                  
068700         MOVE 'W42520'         TO POSTSUM-FDNAMN                          
068800         MOVE 'W42520D1'       TO POSTSUM-DDNAMN2                         
068900         MOVE W-IDPTYP         TO POSTSUM-TRANSTYP                        
069000         CALL POSTSUM USING POSTSUM-PARM                                  
069100     END-IF                                                               
069200     SKIP3                                                                
069300     .                                                                    
069400 S06-SKRIV-UTPOST SECTION.                                                
069500                                                                          
069600     WRITE UTPOST              FROM SKRIVPOST                             
069700     MOVE 'W42520'             TO POSTSUM-PROGNAMN                        
069800     MOVE 'W42599'             TO POSTSUM-FDNAMN                          
069900     MOVE 'W42520D2'           TO POSTSUM-DDNAMN2                         
070000     MOVE SKRIVIDPTYP          TO POSTSUM-TRANSTYP                        
070100     CALL POSTSUM USING POSTSUM-PARM                                      
070200     SKIP3                                                                
070300     .                                                                    
070400 S08-KONTROLLSIFFERBER-ARTNR SECTION.                                     
070500                                                                          
070600     CALL W009KSIF USING BERAKNINGSFALT-ART                               
070700     FALTLANGD-ART                                                        
070800     KONTROLLSIFFRA                                                       
070900     SKIP3                                                                
071000     .                                                                    
071100 S14-JUSTERA-686-POST SECTION.                                            
071200                                                                          
071300     IF  WS-IDKUNDRF1-5 > 79999                                           
071400          MOVE +4                TO W686-KDORDKL                          
071500     ELSE                                                                 
071600          MOVE +2                TO W686-KDORDKL                          
071700     END-IF                                                               
071800     SKIP3                                                                
071900     .                                                                    
072000 S15-JUSTERA-687-POST SECTION.                                            
072100                                                                          
072200     IF  WS-IDKUNDRF-RO-1 = ZERO                                          
072300         IF WS-IDKUNDRF1-5 > 79999                                        
072400             MOVE +4 TO W687-KDORDKL                                      
072500          ELSE                                                            
072600             MOVE +2 TO W687-KDORDKL                                      
072700          END-IF                                                          
072800     ELSE                                                                 
072900         IF WS-IDKUNDRF-RO-1 > 79999                                      
073000             MOVE +4 TO W687-KDORDKL                                      
073100         ELSE                                                             
073200             MOVE +2 TO W687-KDORDKL                                      
073300         END-IF                                                           
073400     END-IF                                                               
073500     SKIP3                                                                
073600     .                                                                    
073700 S18-LAS-SORTFIL SECTION.                                                 
073800     SKIP2                                                                
073900     RETURN SORTFIL   INTO WSORT-AREA                                     
074000                      AT END MOVE JA TO EOF-SORTFIL                       
074100     EJECT                                                                
074200     .                                                                    
