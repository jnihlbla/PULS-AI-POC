000400 ID DIVISION.                                                             
000500*                                                                         
000600 PROGRAM-ID.             W4254300.                                        
001000*AUTHOR.                 ERIK KÅREBY                                      
001100*DATE-WRITTEN.           OKTOBER 1982.                                    
001200                                                                          
001300*REMARKS.                                                                 
001400                                                                          
001500*    FUNKTION:                                                            
001600                                                                          
001700*            REDIGERIG AV VR-KOPPLINGSPOSTER FRÅN FAKTURAFILEN            
001800*            W47597                                                       
001900                                                                          
002000*       POSTSUM                                                           
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*                            INFILER:                                     
002900                                                                          
003000     SELECT  FAKTURAFIL               ASSIGN  UT-S-W42543D1.              
003100     SKIP2                                                                
003200*                            UTFILER:                                     
003300                                                                          
003400     SELECT  W42543                   ASSIGN  UT-S-W42543D2.              
003500     SKIP2                                                                
003600*                            SORTFIL:                                     
003700                                                                          
003800     SELECT  SORTFIL                  ASSIGN  UT-S-W42543DS.              
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP1                                                                
004200 FILE SECTION.                                                            
004300     SKIP1                                                                
004400 FD  FAKTURAFIL                                                           
004500     RECORDING      V                                                     
004600     BLOCK CONTAINS 0.                                                    
004700                                                                          
004800 01  VR-POST-5.                                                           
004900*    03  FILLER   -COPY W475F800    -L.                                   
005100*    03  FILLER   -COPY W475F805    -L.                                   
005300     SKIP2                                                                
005400 01  VR-POST-4.                                                           
005500*    03  FILLER   -COPY W475F800    -L.                                   
005700*    03  FILLER   -COPY W475F804    -L.                                   
005900     SKIP2                                                                
006000 01  VR-POST-3.                                                           
006100*    03  FILLER   -COPY W475F800    -L.                                   
006300*    03  FILLER   -COPY W475F803    -L.                                   
006500     SKIP2                                                                
006600 01  VR-POST-2.                                                           
006700*    03  FILLER   -COPY W475F800    -L.                                   
006900*    03  FILLER   -COPY W475F802    -L.                                   
007100     SKIP2                                                                
007200 01  VR-POST-1.                                                           
007300*    03  FILLER   -COPY W475F800    -L.                                   
007500*    03  FILLER   -COPY W475F801    -L.                                   
007700     EJECT                                                                
007800 FD  W42543                                                               
007900     RECORDING      V                                                     
008000     BLOCK  CONTAINS 0.                                                   
008100                                                                          
008200 01  PT687-POST.                                                          
008300*    03  FILLER   -COPY W425687     -L.                                   
008500                                                                          
008600 01  PT685-POST.                                                          
008700*    03  FILLER   -COPY W425685     -L.                                   
008900                                                                          
009000 01  PT686-POST.                                                          
009100*    03  FILLER   -COPY W425686     -L.                                   
009300                                                                          
009400 01  PT688-POST.                                                          
009500*    03  FILLER   -COPY W425688     -L.                                   
009700     EJECT                                                                
009800 SD  SORTFIL.                                                             
010100 01  SORT-POST.                                                           
010200   03  SORT-IDPTYP               PIC X(3).                                
010300   03  SORT-ARG                  PIC X(33).                               
010400   03  FILLER                    PIC X(9).                                
010500*    03  FILLER   -COPY W475F805    -L.                                   
010700     EJECT                                                                
010800 WORKING-STORAGE SECTION.                                                 
010810                                                                          
010900*    -- CHECKED BY WY2000                                                 
011500*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
011600 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4254300'.            
011800     SKIP2                                                                
011900*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
012000                                                                          
012100 77  JA                          PIC X       VALUE 'J'.                   
012200 77  NEJ                         PIC X       VALUE 'N'.                   
012300 77  RETURKOD                    PIC S9(2)   VALUE +16.                   
012400     SKIP2                                                                
012500*- - - - - - - - - - - - - -  ARBETSFÄLT                                  
012600     SKIP2                                                                
012700 01  W-SUM-VAR.                                                           
012800   03  WS-ANT-RADER              PIC S9(7)       COMP-3 VALUE +0.         
012900   03  WS-ANT-KOLLI              PIC S9(5)       COMP-3 VALUE +0.         
013000   03  WS-SUM-VKORDBTO           PIC S9(6)V9(1)  COMP-3 VALUE +0.         
013100   03  WS-SUM-VLORDBTO           PIC S9(4)V9(3)  COMP-3 VALUE +0.         
013200   03  WS-TIFAKT                 PIC S9(7)       COMP-3 VALUE +0.         
013300   03  WS-KDORDKL                PIC S9(1)       COMP-3 VALUE +0.         
013400   03  WS-IDLBBET                PIC X(12)       VALUE SPACE.             
013500   03  WS-KDREFNOT               PIC X(2)        VALUE SPACE.             
013600   03  WS-IDKUNDRF-X.                                                     
013700     05  WS-IDORDNR              PIC 9(5).                                
013800     05  FILLER                  PIC X(5).                                
013900     EJECT                                                                
014000 01  DYNAMISKA-SUBPROGRAM.                                                
014100   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
014200   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
014300     SKIP3                                                                
014400 01  SWITCHAR.                                                            
014500   03  SORT-EOF-SW               PIC X(1)    VALUE 'N'.                   
014600    88 SORT-EOF                              VALUE 'J'.                   
014700     EJECT                                                                
014800*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
014900                                                                          
015000*01  -COPY W0005       -PRE POSTSUM-.                                     
015200     EJECT                                                                
015300 01  FILLER                      PIC X(24)    VALUE                       
015400                                              'FAKT-AREA-START'.          
015500 01  INPOST-AREA.                                                         
015600   03  KEY-AREA.                                                          
015700*    05  -COPY W475F800 -PRE VR-                                          
015900                                                                          
016000                                                                          
016100*    03  DATA-AREA  -COPY W475F805    -PRE VR- -L.                        
016300     EJECT                                                                
016400*  03  FILLER  -COPY W475F801   -RED  VR-DATA-AREA -PRE VR1-              
016600     EJECT                                                                
016700*  03  FILLER  -COPY W475F802   -RED  VR-DATA-AREA -PRE VR2-              
016900     EJECT                                                                
017000*  03  FILLER  -COPY W475F803   -RED  VR-DATA-AREA -PRE VR3-              
017200     EJECT                                                                
017300*  03  FILLER  -COPY W475F804   -RED  VR-DATA-AREA -PRE VR4-              
017500     EJECT                                                                
017600*  03  FILLER  -COPY W475F805   -RED  VR-DATA-AREA -PRE VR5-              
017800     EJECT                                                                
017900 01  FILLER                      PIC X(24)    VALUE                       
018000                                              'UT-AREA-START'.            
018100* 01     -COPY W425685 -PRE  PT685-                                       
018300     EJECT                                                                
018400* 01     -COPY W425686 -PRE  PT686-                                       
018600     EJECT                                                                
018700* 01     -COPY W425687 -PRE  PT687-                                       
018900     EJECT                                                                
019000* 01     -COPY W425688 -PRE  PT688-                                       
019200     EJECT                                                                
019300 PROCEDURE DIVISION.                                                      
019400     SKIP3                                                                
019500     SORT SORTFIL                                                         
019600          ASCENDING SORT-ARG                                              
019700          USING FAKTURAFIL                                                
019800          OUTPUT PROCEDURE A-RETURN-SORT                                  
019900                                                                          
020000     IF SORT-RETURN NOT = 0                                               
020100        MOVE SORT-RETURN      TO RETURN-CODE                              
020200        CALL ABEND USING RETURKOD                                         
020300     ELSE                                                                 
020400        MOVE ZERO TO RETURN-CODE                                          
020500     END-IF                                                               
020600     GOBACK                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 A-RETURN-SORT SECTION.                                                   
021000     SKIP3                                                                
021100     PERFORM AA-INIT                                                      
021200                                                                          
021300     RETURN SORTFIL INTO INPOST-AREA                                      
021400            AT END MOVE JA TO SORT-EOF-SW                                 
021500     END-RETURN                                                           
021600                                                                          
021700     PERFORM UNTIL                                                        
021800      ( SORT-EOF )                                                        
021900       EVALUATE VR-IDPTYP                                                 
022000       WHEN 'VR1'                                                         
022100         PERFORM F-SPARA-KDREFNOT                                         
022200         PERFORM S01-SKRIV-PT685                                          
022300       WHEN 'VR2'                                                         
022400         PERFORM B-TESTA-PT686                                            
022500       WHEN 'VR3'                                                         
022600         PERFORM C-SPARA-IDLBBET                                          
022700       WHEN 'VR4'                                                         
022800         PERFORM D-ADD-KOLLI                                              
022900         PERFORM S04-SKRIV-PT688                                          
023000       WHEN 'VR5'                                                         
023100         PERFORM E-ADD-RAD                                                
023200         PERFORM S03-SKRIV-PT687                                          
023300       END-EVALUATE                                                       
023400       RETURN SORTFIL INTO INPOST-AREA                                    
023500             AT END MOVE JA TO SORT-EOF-SW                                
023600       END-RETURN                                                         
023700                                                                          
023800     END-PERFORM                                                          
023900     PERFORM Z-FINIT                                                      
024000     .                                                                    
024100                                                                          
024200     EJECT                                                                
024300 AA-INIT SECTION.                                                         
024400     SKIP3                                                                
024500     OPEN OUTPUT W42543                                                   
024600                                                                          
024700     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
024800     MOVE 'W42543D1' TO POSTSUM-DDNAMN2                                   
024900     MOVE 'VR'      TO POSTSUM-FDNAMN                                     
025000     .                                                                    
025100     EJECT                                                                
025200 B-TESTA-PT686 SECTION.                                                   
025300     SKIP3                                                                
025400     IF WS-ANT-KOLLI > ZERO                                               
025500     OR WS-ANT-RADER > ZERO                                               
025600       PERFORM S02-SKRIV-PT686                                            
025700     END-IF                                                               
025800     MOVE ZERO TO WS-ANT-KOLLI                                            
025900                  WS-ANT-RADER                                            
026000                  WS-SUM-VKORDBTO                                         
026100                  WS-SUM-VLORDBTO                                         
026200     .                                                                    
026300     EJECT                                                                
026400 C-SPARA-IDLBBET SECTION.                                                 
026500     SKIP3                                                                
026600     MOVE VR3-IDLBBET TO WS-IDLBBET                                       
026700     MOVE VR3-KDORDKL TO WS-KDORDKL                                       
026800     .                                                                    
026900     EJECT                                                                
027000 D-ADD-KOLLI SECTION.                                                     
027100     SKIP3                                                                
027200     ADD +1 TO WS-ANT-KOLLI                                               
027300     ADD VR4-VLORDBTO TO WS-SUM-VLORDBTO                                  
027400     ADD VR4-VKORDBTO TO WS-SUM-VKORDBTO                                  
027500     .                                                                    
027600     EJECT                                                                
027700 E-ADD-RAD SECTION.                                                       
027800     SKIP3                                                                
027900     ADD +1 TO WS-ANT-RADER                                               
028000     .                                                                    
028100     EJECT                                                                
028200 F-SPARA-KDREFNOT     SECTION.                                            
028300                                                                          
028400     MOVE VR1-KDREFNOT           TO WS-KDREFNOT                           
028500     .                                                                    
028600     EJECT                                                                
028700 S01-SKRIV-PT685 SECTION.                                                 
028800     SKIP3                                                                
028900     MOVE '685'                  TO PT685-IDPTYP                          
029000     MOVE VR-IDDISTR             TO PT685-IDDISTR                         
029010     MOVE VR-IDKUNDNR            TO PT685-IDKUNDNR                        
029100     MOVE VR-IDDC                TO PT685-IDDC                            
029200     MOVE VR-IDFAKT              TO PT685-IDFAKT                          
029300     MOVE VR-KDFAKTYP            TO PT685-KDFAKTYP                        
029400     MOVE VR-TIAAMMDD            TO PT685-TIAAMMDD                        
029500     MOVE VR-TIKLOCK             TO PT685-TIKLOCK                         
029600                                                                          
029700     MOVE VR1-KDFRAKT            TO PT685-KDFRAKT                         
029800     MOVE VR1-PREMBHNT           TO PT685-PREMBHNT                        
029900     MOVE VR1-PRFRAKT            TO PT685-PRFRAKT                         
030000     MOVE VR1-PRFOERS            TO PT685-PRFOERS                         
030100     MOVE VR1-SUFKTBEL           TO PT685-SUFKTBEL                        
030200     MOVE VR1-TIFAKT             TO PT685-TIFAKT                          
030300     MOVE VR1-PRKURS             TO PT685-PRKURS                          
030500                                                                          
030600     WRITE PT685-POST FROM PT685-W425685                                  
030700                                                                          
030800     MOVE '685'  TO POSTSUM-TRANSTYP                                      
030900     CALL POSTSUM USING POSTSUM-PARM                                      
031000                                                                          
031100     MOVE VR1-TIFAKT             TO WS-TIFAKT                             
031200     .                                                                    
031300     EJECT                                                                
031400 S02-SKRIV-PT686 SECTION.                                                 
031500     SKIP3                                                                
031600     MOVE '686'                  TO PT686-IDPTYP                          
031700     MOVE VR-IDDISTR             TO PT686-IDDISTR                         
031710     MOVE VR-IDKUNDNR            TO PT686-IDKUNDNR                        
031800     MOVE VR-IDDC                TO PT686-IDDC                            
031900     MOVE VR-IDFAKT              TO PT686-IDFAKT                          
032000     MOVE VR-KDFAKTYP            TO PT686-KDFAKTYP                        
032100     MOVE VR-IDKUNDRF            TO PT686-IDKUNDRF                        
032200     MOVE VR-TIAAMMDD            TO PT686-TIAAMMDD                        
032300     MOVE VR-TIKLOCK             TO PT686-TIKLOCK                         
032400                                                                          
032500     MOVE VR2-KDORDKL            TO PT686-KDORDKL                         
032600     MOVE WS-ANT-RADER           TO PT686-KVRAD-UPD                       
032700     MOVE WS-ANT-KOLLI           TO PT686-KVKOLLIO                        
032800     MOVE WS-SUM-VLORDBTO        TO PT686-VLORDBTO                        
032900     MOVE WS-SUM-VKORDBTO        TO PT686-VKORDBTO                        
033000     MOVE VR-IDPRODNR            TO PT686-IDPRODNR                        
033100     MOVE WS-KDREFNOT            TO PT686-KDREFNOT                        
033200                                                                          
033300     WRITE PT686-POST FROM PT686-W425686                                  
033400                                                                          
033500     MOVE '686'  TO POSTSUM-TRANSTYP                                      
033600     CALL POSTSUM USING POSTSUM-PARM                                      
033700     .                                                                    
033800     EJECT                                                                
033900 S03-SKRIV-PT687 SECTION.                                                 
034000     SKIP3                                                                
034100     MOVE '687'                  TO PT687-IDPTYP                          
034200     MOVE VR-IDDISTR             TO PT687-IDDISTR                         
034210     MOVE VR-IDKUNDNR            TO PT687-IDKUNDNR                        
034300     MOVE VR-IDDC                TO PT687-IDDC                            
034400     MOVE VR-IDFAKT              TO PT687-IDFAKT                          
034500     MOVE VR-KDFAKTYP            TO PT687-KDFAKTYP                        
034600     MOVE VR-IDPRODNR            TO PT687-IDPRODNR                        
034700     MOVE VR-TIAAMMDD            TO PT687-TIAAMMDD                        
034800     MOVE VR-TIKLOCK             TO PT687-TIKLOCK                         
034900                                                                          
035000     MOVE WS-KDORDKL             TO PT687-KDORDKL                         
035100     MOVE VR5-IDKUNDRF-RO        TO PT687-IDKUNDRF-RO                     
035200     MOVE VR-IDARTNR             TO PT687-IDARTNR                         
035300     MOVE VR-IDKOLLI             TO PT687-IDKOLLI                         
035400     MOVE VR5-KVLEVART           TO PT687-KVLEVART                        
035500     MOVE VR5-KDVRINFO           TO PT687-KDVRINFO                        
035600     MOVE VR5-KDSRA              TO PT687-KDSRA                           
035700     MOVE VR5-KDARTURS           TO PT687-KDARTURS                        
035800     MOVE VR5-IDFKNGRP           TO PT687-IDFKNGRP                        
035900     MOVE ZERO                   TO PT687-KDPRODSL                        
036000     MOVE ZERO                   TO PT687-KDVVKL                          
036100     MOVE VR5-KVQPACK            TO PT687-KVQPACK                         
036200     MOVE VR5-VKART              TO PT687-VKART                           
036300     MOVE VR5-PRARTNTO           TO PT687-PRARTNTO                        
036500     MOVE VR5-PRARTULL           TO PT687-PRARTULL                        
036600     MOVE VR5-KDRABATT           TO PT687-KDRABATT                        
036700     MOVE VR5-BEART              TO PT687-BEART                           
036800     MOVE VR5-KDSORT             TO PT687-KDSORT                          
036900     MOVE VR-IDKUNDRF            TO PT687-IDKUNDRF                        
037000     MOVE VR5-TIPRIS             TO PT687-TIPRIS                          
037100     MOVE SPACE                  TO PT687-KDVIP                           
037200     MOVE VR5-BERADREF           TO PT687-BERADREF                        
037300     MOVE VR5-KDTULLVE           TO PT687-KDTULLVE                        
037400     MOVE VR5-REBPRIS            TO PT687-REBPRIS                         
037500     MOVE VR5-FLPRTILL           TO PT687-FLPRTILL                        
037510     MOVE VR5-PRARTBTO-EXP       TO PT687-PRARTBTO-EXP                    
037520     MOVE VR5-IDKLIENT           TO PT687-IDKLIENT                        
037530     MOVE VR5-IDARBREF           TO PT687-IDARBREF                        
037540     MOVE VR5-IDBIL              TO PT687-IDBIL                           
037550     MOVE VR5-IDVIN              TO PT687-IDVIN                           
037600                                                                          
037700     WRITE PT687-POST FROM PT687-W425687                                  
037800                                                                          
037900     MOVE '687'  TO POSTSUM-TRANSTYP                                      
038000     CALL POSTSUM USING POSTSUM-PARM                                      
038100     .                                                                    
038200     EJECT                                                                
038300 S04-SKRIV-PT688 SECTION.                                                 
038400     SKIP3                                                                
038500     MOVE '688'                  TO PT688-IDPTYP                          
038600     MOVE VR-IDDISTR             TO PT688-IDDISTR                         
038610     MOVE VR-IDKUNDNR            TO PT688-IDKUNDNR                        
038700     MOVE VR-IDDC                TO PT688-IDDC                            
038800     IF VR-IDKUNDRF NOT = SPACE                                           
038900       MOVE VR-IDKUNDRF        TO WS-IDKUNDRF-X                           
039000       MOVE WS-IDORDNR         TO PT688-IDORDNR                           
039100     ELSE                                                                 
039200       MOVE ZERO               TO PT688-IDORDNR                           
039300     END-IF                                                               
039400     MOVE VR-IDFAKT              TO PT688-IDFAKT                          
039500     MOVE WS-TIFAKT              TO PT688-TIFAKT                          
039600     MOVE VR-IDKOLLI             TO PT688-IDKOLLI                         
039700     MOVE WS-IDLBBET             TO PT688-IDLBBET                         
039800     MOVE VR-TIAAMMDD            TO PT688-TIAAMMDD                        
039900     MOVE VR-TIKLOCK             TO PT688-TIKLOCK                         
040000                                                                          
040100     MOVE VR4-VLORDBTO           TO PT688-VLORDBTO-KOLLI                  
040200     MOVE VR4-VKORDBTO           TO PT688-VKORDBTO-KOLLI                  
040300                                                                          
040400     WRITE PT688-POST FROM PT688-W425688                                  
040500                                                                          
040600     MOVE '688'  TO POSTSUM-TRANSTYP                                      
040700     CALL POSTSUM USING POSTSUM-PARM                                      
040800     .                                                                    
040900     EJECT                                                                
041000 Z-FINIT   SECTION.                                                       
041100     SKIP3                                                                
041200     MOVE 'S'                  TO POSTSUM-OPKOD                           
041300     CALL POSTSUM     USING    POSTSUM-PARM                               
041400                                                                          
041500     CLOSE  W42543                                                        
041600     .                                                                    
