000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W009TEST.                                                 
000300 AUTHOR.        RICHARD.                                                  
000400 DATE-WRITTEN.  APRIL 1992.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        TEST-PROGRAM FÖR XXXXXXXX (STANDARDSUB-PROGRAM).                 
000900                                                                          
001000                                                                          
001100 ENVIRONMENT DIVISION.                                                    
001200                                                                          
001300 CONFIGURATION SECTION.                                                   
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800                                                                          
001900                                                                          
002000 DATA DIVISION.                                                           
002100                                                                          
002200 FILE SECTION.                                                            
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700                                                                          
002800 77    IDPGM                     PIC X(8)    VALUE 'W009TEST'.            
002900 77    JA                        PIC X(1)    VALUE 'J'.                   
003000 77    NEJ                       PIC X(1)    VALUE 'N'.                   
003100 77    W-IDARTNR                 PIC S9(9)   VALUE +12345 COMP-3.         
003200 77    W-KDCLAGER                PIC S9(1)   VALUE +1     COMP-3.         
003300 77    W-KVARBDAG                PIC S9(3)   VALUE ZERO   COMP-3.         
003400 77    W-KVWORKD                 PIC S9(3)   VALUE ZERO   COMP-3.         
003500 77    RKOD                      PIC S9(4)   VALUE 888    COMP.           
003600 77    EJNUM                     PIC X(6)    VALUE '12345.'.              
003700 77    W-VECKA                   PIC 9(4)    VALUE 8700.                  
003800 77    MAX-TAL                   PIC 9(9)                 COMP.           
003900 77    SVAR-TAL                  PIC 9(9)                 COMP.           
004000 77    IX                        PIC 9(9)                 COMP.           
004100 77    RAND-IDARTNR              PIC S9(9)   VALUE ZERO   COMP-3.         
004200 77    RAND-RANDOM               PIC S9(9)   VALUE ZERO   COMP.           
004300 77    RAND-BASE                 PIC X(4)    VALUE SPACE.                 
004400     EJECT                                                                
004500 01  DAGENS-DATUM.                                                        
004600     03  DAGENS-DATUM-AAR        PIC X(2)    VALUE SPACE.                 
004700     03  DAGENS-DATUM-MAANAD     PIC X(2)    VALUE SPACE.                 
004800     03  DAGENS-DATUM-DAG        PIC X(2)    VALUE SPACE.                 
004900     EJECT                                                                
005000 01    DYNAMISKA-SUBPROGRAM.                                              
005100   03    WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
005200   03    WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
005300   03    WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005400   03    WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
005500   03    WISOLAND                PIC X(8)    VALUE 'WISOLAND'.            
005600   03    WKPSKONV                PIC X(8)    VALUE 'WKPSKONV'.            
005700   03    WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005800   03    WRANDOM                 PIC X(8)    VALUE 'WRANDOM '.            
005900   03    WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
006000   03    W006ASCI                PIC X(8)    VALUE 'W006ASCI'.            
006100   03    W009MOMS                PIC X(8)    VALUE 'W009MOMS'.            
006200   03    W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
006300   03    W015RAND                PIC X(8)    VALUE 'W015RAND'.            
006400   03    ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006500     EJECT                                                                
006600 01    ASCI-PARAMETRAR.                                                   
006700   03    ASCI-001                PIC X(3)    VALUE '001'.                 
006800   03    ASCI-LL                 PIC S9(9)   VALUE +120   COMP.           
006900 01    ASCI-DATA.                                                         
007000   03  FILLER                    PIC X(12)   VALUE '1234567890+é'.        
007100   03  FILLER                    PIC X(12)   VALUE '!"§¤%&/()=?É'.        
007200   03  FILLER                    PIC X(12)   VALUE 'qwertyuiopåü'.        
007300   03  FILLER                    PIC X(12)   VALUE 'QWERTYUIOPÅ^'.        
007400   03  FILLER                    PIC X(12)   VALUE 'asdfghjklöä '.        
007500   03  FILLER                    PIC X(12)   VALUE 'ASDFGHJKLÖÄ*'.        
007600   03  FILLER                    PIC X(12)   VALUE '<zxcvbnm,.- '.        
007700   03  FILLER                    PIC X(12)   VALUE '>ZXCVBNM;:_ '.        
007800   03  FILLER                    PIC X(50)   VALUE space.                 
007900     EJECT                                                                
008000*01    -COPY WORKAREA                                                     
008100     EJECT                                                                
008200*01    -COPY WDAGAREA                                                     
008300     EJECT                                                                
008400*01    -COPY WDATAREA                                                     
008500     EJECT                                                                
008600*01    -COPY WDECAREA                                                     
008700     EJECT                                                                
008800*01    -COPY WISOLAND                                                     
008900     EJECT                                                                
009000*01    -COPY WKPSAREA                                                     
009100     EJECT                                                                
009200*01    -COPY WMEDAREA                                                     
009300     EJECT                                                                
009400*01    -COPY WSECAREA                                                     
009500     EJECT                                                                
009600*01    -COPY W009MOMS                                                     
009700     EJECT                                                                
009800*01    -COPY W009CIA                                                      
009900     EJECT                                                                
010000 LINKAGE SECTION.                                                         
010100                                                                          
010200 01  PARM-INPUT.                                                          
010300   03  PARM-LTH                  PIC S9(4)                COMP.           
010400   03  PARM-IDPGM                PIC X(8).                                
010500     EJECT                                                                
010600 PROCEDURE DIVISION USING PARM-INPUT.                                     
010700                                                                          
010800 MAIN SECTION.                                                            
010900     DISPLAY 'TEST STARTAR'                                               
011000     DISPLAY '            '                                               
011100                                                                          
011200     EVALUATE PARM-IDPGM                                                  
011300       WHEN 'W009MOMS'                                                    
011400         PERFORM A-MOMS                                                   
011500       WHEN 'W009CIA '                                                    
011600         PERFORM B-CIA                                                    
011700       WHEN 'W006ASCI'                                                    
011800         PERFORM C-ASCI                                                   
011900       WHEN 'WDATKONV'                                                    
012000         PERFORM E-DATKONV                                                
012100       WHEN 'WRANDOM '                                                    
012200         PERFORM F-WRANDOM                                                
012300       WHEN 'WDECEDIT'                                                    
012400         PERFORM G-WDECEDIT                                               
012500       WHEN 'WSECURIT'                                                    
012600         PERFORM H-WSECURIT                                               
012700       WHEN 'WKPSKONV'                                                    
012800         PERFORM I-WKPSKONV                                               
012900       WHEN 'WMEDKONV'                                                    
013000         PERFORM J-WMEDKONV                                               
013100       WHEN 'WISOLAND'                                                    
013200         PERFORM K-WISOLAND                                               
013300       WHEN 'W015RAND'                                                    
013400         PERFORM L-W015RAND                                               
013500       WHEN 'WDAGKONV'                                                    
013600         PERFORM M-DAGKONV                                                
013700       WHEN 'WORKDAY '                                                    
013800         PERFORM N-WORKDAY                                                
013900       WHEN OTHER                                                         
014000         DISPLAY 'PROGRAM MISSING IN PARM'                                
014100     END-EVALUATE                                                         
014200                                                                          
014300     DISPLAY '            '                                               
014400     DISPLAY 'TEST SLUTAR '                                               
014500                                                                          
014600     MOVE ZERO TO RETURN-CODE                                             
014700     GOBACK                                                               
014800     .                                                                    
014900     EJECT                                                                
015000 A-MOMS SECTION.                                                          
015100                                                                          
015200     DISPLAY '            '                                               
015300     DISPLAY 'MOMS TEST STARTAR '                                         
015400                                                                          
015500     MOVE 'XX' TO MOMS-IDLANDX2                                           
015600     MOVE '01' TO MOMS-KDVAT                                              
015700     CALL W009MOMS USING MOMS-W009MOMS                                    
015800     PERFORM AA-MOMS                                                      
015900                                                                          
016000     MOVE 'BE' TO MOMS-IDLANDX2                                           
016100     MOVE '66' TO MOMS-KDVAT                                              
016200     CALL W009MOMS USING MOMS-W009MOMS                                    
016300     PERFORM AA-MOMS                                                      
016400                                                                          
016500     MOVE 'BE' TO MOMS-IDLANDX2                                           
016600     MOVE '01' TO MOMS-KDVAT                                              
016700     CALL W009MOMS USING MOMS-W009MOMS                                    
016800     PERFORM AA-MOMS                                                      
016900                                                                          
017000     MOVE 'BE' TO MOMS-IDLANDX2                                           
017100     MOVE '02' TO MOMS-KDVAT                                              
017200     CALL W009MOMS USING MOMS-W009MOMS                                    
017300     PERFORM AA-MOMS                                                      
017400                                                                          
017500     MOVE 'BE' TO MOMS-IDLANDX2                                           
017600     MOVE '03' TO MOMS-KDVAT                                              
017700     CALL W009MOMS USING MOMS-W009MOMS                                    
017800     PERFORM AA-MOMS                                                      
017900                                                                          
018000     MOVE 'FR' TO MOMS-IDLANDX2                                           
018100     MOVE '01' TO MOMS-KDVAT                                              
018200     CALL W009MOMS USING MOMS-W009MOMS                                    
018300     PERFORM AA-MOMS                                                      
018400                                                                          
018500     MOVE 'FR' TO MOMS-IDLANDX2                                           
018600     MOVE '02' TO MOMS-KDVAT                                              
018700     CALL W009MOMS USING MOMS-W009MOMS                                    
018800     PERFORM AA-MOMS                                                      
018900                                                                          
019000     MOVE 'FR' TO MOMS-IDLANDX2                                           
019100     MOVE '03' TO MOMS-KDVAT                                              
019200     CALL W009MOMS USING MOMS-W009MOMS                                    
019300     PERFORM AA-MOMS                                                      
019400                                                                          
019500     MOVE 'GB' TO MOMS-IDLANDX2                                           
019600     MOVE '01' TO MOMS-KDVAT                                              
019700     CALL W009MOMS USING MOMS-W009MOMS                                    
019800     PERFORM AA-MOMS                                                      
019900                                                                          
020000     MOVE 'GB' TO MOMS-IDLANDX2                                           
020100     MOVE '02' TO MOMS-KDVAT                                              
020200     CALL W009MOMS USING MOMS-W009MOMS                                    
020300     PERFORM AA-MOMS                                                      
020400                                                                          
020500     MOVE 'GB' TO MOMS-IDLANDX2                                           
020600     MOVE '03' TO MOMS-KDVAT                                              
020700     CALL W009MOMS USING MOMS-W009MOMS                                    
020800     PERFORM AA-MOMS                                                      
020900                                                                          
021000     MOVE 'SE' TO MOMS-IDLANDX2                                           
021100     MOVE '01' TO MOMS-KDVAT                                              
021200     CALL W009MOMS USING MOMS-W009MOMS                                    
021300     PERFORM AA-MOMS                                                      
021400                                                                          
021500     MOVE 'SE' TO MOMS-IDLANDX2                                           
021600     MOVE '02' TO MOMS-KDVAT                                              
021700     CALL W009MOMS USING MOMS-W009MOMS                                    
021800     PERFORM AA-MOMS                                                      
021900                                                                          
022000     MOVE 'SE' TO MOMS-IDLANDX2                                           
022100     MOVE '03' TO MOMS-KDVAT                                              
022200     CALL W009MOMS USING MOMS-W009MOMS                                    
022300     PERFORM AA-MOMS                                                      
022400     .                                                                    
022500     EJECT                                                                
022600 AA-MOMS SECTION.                                                         
022700                                                                          
022800     DISPLAY '             '                                              
022900                                                                          
023000     DISPLAY MOMS-IDLANDX2 ' ' MOMS-KDVAT                                 
023100         ' ' MOMS-REVAT    ' ' MOMS-BEVAT ' ' MOMS-KDSVAR                 
023200     .                                                                    
023300     EJECT                                                                
023400 B-CIA SECTION.                                                           
023500                                                                          
023600     DISPLAY 'CIA-TEST STARTAR '                                          
023700                                                                          
023800     MOVE 'GB2' TO CIA-IDARTPRE-IN                                        
023900     MOVE '12345678901234567' TO CIA-IDARTBET-IN                          
024000     CALL W009CIA USING CIA-W009CIA                                       
024100     PERFORM BA-CIA                                                       
024200                                                                          
024300     MOVE 'LE ' TO CIA-IDARTPRE-IN                                        
024400     MOVE 'ABCDEFG          ' TO CIA-IDARTBET-IN                          
024500     CALL W009CIA USING CIA-W009CIA                                       
024600     PERFORM BA-CIA                                                       
024700                                                                          
024800     MOVE 'VO ' TO CIA-IDARTPRE-IN                                        
024900     MOVE 'ABCDEFG          ' TO CIA-IDARTBET-IN                          
025000     CALL W009CIA USING CIA-W009CIA                                       
025100     PERFORM BA-CIA                                                       
025200                                                                          
025300     MOVE 'VO ' TO CIA-IDARTPRE-IN                                        
025400     MOVE '1234567          ' TO CIA-IDARTBET-IN                          
025500     CALL W009CIA USING CIA-W009CIA                                       
025600     PERFORM BA-CIA                                                       
025700                                                                          
025800     MOVE 'VO ' TO CIA-IDARTPRE-IN                                        
025900     MOVE '  3456789        ' TO CIA-IDARTBET-IN                          
026000     CALL W009CIA USING CIA-W009CIA                                       
026100     PERFORM BA-CIA                                                       
026200                                                                          
026300     MOVE 'VO ' TO CIA-IDARTPRE-IN                                        
026400     MOVE ' 123456789       ' TO CIA-IDARTBET-IN                          
026500     CALL W009CIA USING CIA-W009CIA                                       
026600     PERFORM BA-CIA                                                       
026700                                                                          
026800     MOVE 'VO ' TO CIA-IDARTPRE-IN                                        
026900     MOVE '000000000        ' TO CIA-IDARTBET-IN                          
027000     CALL W009CIA USING CIA-W009CIA                                       
027100     PERFORM BA-CIA                                                       
027200                                                                          
027300     MOVE 'VO ' TO CIA-IDARTPRE-IN                                        
027400     MOVE '                 ' TO CIA-IDARTBET-IN                          
027500     CALL W009CIA USING CIA-W009CIA                                       
027600     PERFORM BA-CIA                                                       
027700                                                                          
027800     MOVE 'VO ' TO CIA-IDARTPRE-IN                                        
027900     MOVE '  0006789        ' TO CIA-IDARTBET-IN                          
028000     CALL W009CIA USING CIA-W009CIA                                       
028100     PERFORM BA-CIA                                                       
028200                                                                          
028300     MOVE 'VO ' TO CIA-IDARTPRE-IN                                        
028400     MOVE W-IDARTNR           TO CIA-IDARTBET-IN                          
028500     CALL W009CIA USING CIA-W009CIA                                       
028600     PERFORM BA-CIA                                                       
028700                                                                          
028800     MOVE 'VO ' TO CIA-IDARTPRE-IN                                        
028900     MOVE W-KDCLAGER          TO CIA-IDARTBET-IN                          
029000     CALL W009CIA USING CIA-W009CIA                                       
029100     PERFORM BA-CIA                                                       
029200                                                                          
029300     .                                                                    
029400     EJECT                                                                
029500 BA-CIA SECTION.                                                          
029600                                                                          
029700     DISPLAY '             '                                              
029800                                                                          
029900     DISPLAY CIA-IDARTPRE-IN                                              
030000         ' ' CIA-IDARTBET-IN                                              
030100     DISPLAY CIA-IDARTPRE-UT                                              
030200         ' ' CIA-IDARTBET-UT                                              
030300         ' ' CIA-IDARTBET-MOD                                             
030400     DISPLAY CIA-IDARTNR                                                  
030500         ' ' CIA-IDARTN8                                                  
030600         ' ' CIA-IDARTX9                                                  
030700         ' ' CIA-KDSVAR                                                   
030800     .                                                                    
030900     EJECT                                                                
031000 C-ASCI SECTION.                                                          
031100                                                                          
031200     DISPLAY 'ASCI-TEST STARTAR '                                         
031300                                                                          
031400     CALL W006ASCI USING ASCI-001 ASCI-LL ASCI-DATA                       
031500     PERFORM CA-ASCI                                                      
031600                                                                          
031700     .                                                                    
031800     EJECT                                                                
031900 CA-ASCI SECTION.                                                         
032000                                                                          
032100     DISPLAY '             '                                              
032200                                                                          
032300     DISPLAY ASCI-001                                                     
032400         ' ' ASCI-LL                                                      
032500         ' ' ASCI-DATA                                                    
032600     .                                                                    
032700     EJECT                                                                
032800 E-DATKONV SECTION.                                                       
032900                                                                          
033000     DISPLAY                                                              
033100     'FORM   INPUT  AAMMDD AAVVD AADDD AAP AAPP AARP SEK V '              
033200     'SEKDAT S'                                                           
033300                                                                          
033400     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
033500     MOVE 0        TO DAT-I-TIDATUM                                       
033600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
033700                         DAT-O-TIDATUM DAT-KDSVAR                         
033800     PERFORM EA-DATKONV                                                   
033900                                                                          
034000     MOVE 'AAVV  ' TO DAT-KDDATFORM                                       
034100     MOVE EJNUM    TO DAT-I-TIDATUM                                       
034200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
034300                         DAT-O-TIDATUM DAT-KDSVAR                         
034400     PERFORM EA-DATKONV                                                   
034500                                                                          
034600     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
034700     MOVE 970101   TO DAT-I-TIDATUM                                       
034800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
034900                         DAT-O-TIDATUM DAT-KDSVAR                         
035000     PERFORM EA-DATKONV                                                   
035100                                                                          
035200     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
035300     MOVE 970102   TO DAT-I-TIDATUM                                       
035400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
035500                         DAT-O-TIDATUM DAT-KDSVAR                         
035600     PERFORM EA-DATKONV                                                   
035700                                                                          
035800     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
035900     MOVE 970103   TO DAT-I-TIDATUM                                       
036000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
036100                         DAT-O-TIDATUM DAT-KDSVAR                         
036200     PERFORM EA-DATKONV                                                   
036300                                                                          
036400     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
036500     MOVE 970104   TO DAT-I-TIDATUM                                       
036600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
036700                         DAT-O-TIDATUM DAT-KDSVAR                         
036800     PERFORM EA-DATKONV                                                   
036900                                                                          
037000     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
037100     MOVE 970105   TO DAT-I-TIDATUM                                       
037200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
037300                         DAT-O-TIDATUM DAT-KDSVAR                         
037400     PERFORM EA-DATKONV                                                   
037500                                                                          
037600     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
037700     MOVE 970106   TO DAT-I-TIDATUM                                       
037800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
037900                         DAT-O-TIDATUM DAT-KDSVAR                         
038000     PERFORM EA-DATKONV                                                   
038100                                                                          
038200     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
038300     MOVE 970107   TO DAT-I-TIDATUM                                       
038400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
038500                         DAT-O-TIDATUM DAT-KDSVAR                         
038600     PERFORM EA-DATKONV                                                   
038700                                                                          
038800     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
038900     MOVE 970128   TO DAT-I-TIDATUM                                       
039000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
039100                         DAT-O-TIDATUM DAT-KDSVAR                         
039200     PERFORM EA-DATKONV                                                   
039300                                                                          
039400     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
039500     MOVE 970313   TO DAT-I-TIDATUM                                       
039600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
039700                         DAT-O-TIDATUM DAT-KDSVAR                         
039800     PERFORM EA-DATKONV                                                   
039900                                                                          
040000     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
040100     MOVE 970314   TO DAT-I-TIDATUM                                       
040200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
040300                         DAT-O-TIDATUM DAT-KDSVAR                         
040400     PERFORM EA-DATKONV                                                   
040500                                                                          
040600     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
040700     MOVE 970317   TO DAT-I-TIDATUM                                       
040800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
040900                         DAT-O-TIDATUM DAT-KDSVAR                         
041000     PERFORM EA-DATKONV                                                   
041100                                                                          
041200     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
041300     MOVE 970501   TO DAT-I-TIDATUM                                       
041400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
041500                         DAT-O-TIDATUM DAT-KDSVAR                         
041600     PERFORM EA-DATKONV                                                   
041700                                                                          
041800     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
041900     MOVE 971223   TO DAT-I-TIDATUM                                       
042000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
042100                         DAT-O-TIDATUM DAT-KDSVAR                         
042200     PERFORM EA-DATKONV                                                   
042300                                                                          
042400     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
042500     MOVE 971224   TO DAT-I-TIDATUM                                       
042600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
042700                         DAT-O-TIDATUM DAT-KDSVAR                         
042800     PERFORM EA-DATKONV                                                   
042900                                                                          
043000     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
043100     MOVE 971225   TO DAT-I-TIDATUM                                       
043200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
043300                         DAT-O-TIDATUM DAT-KDSVAR                         
043400     PERFORM EA-DATKONV                                                   
043500                                                                          
043600     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
043700     MOVE 971226   TO DAT-I-TIDATUM                                       
043800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
043900                         DAT-O-TIDATUM DAT-KDSVAR                         
044000     PERFORM EA-DATKONV                                                   
044100                                                                          
044200     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
044300     MOVE 971227   TO DAT-I-TIDATUM                                       
044400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
044500                         DAT-O-TIDATUM DAT-KDSVAR                         
044600     PERFORM EA-DATKONV                                                   
044700                                                                          
044800     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
044900     MOVE 971228   TO DAT-I-TIDATUM                                       
045000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
045100                         DAT-O-TIDATUM DAT-KDSVAR                         
045200     PERFORM EA-DATKONV                                                   
045300                                                                          
045400     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
045500     MOVE 971229   TO DAT-I-TIDATUM                                       
045600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
045700                         DAT-O-TIDATUM DAT-KDSVAR                         
045800     PERFORM EA-DATKONV                                                   
045900                                                                          
046000     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
046100     MOVE 971230   TO DAT-I-TIDATUM                                       
046200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
046300                         DAT-O-TIDATUM DAT-KDSVAR                         
046400     PERFORM EA-DATKONV                                                   
046500                                                                          
046600     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
046700     MOVE 971231   TO DAT-I-TIDATUM                                       
046800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
046900                         DAT-O-TIDATUM DAT-KDSVAR                         
047000     PERFORM EA-DATKONV                                                   
047100                                                                          
047200     MOVE 'AAVVD ' TO DAT-KDDATFORM                                       
047300     MOVE 97041    TO DAT-I-TIDATUM                                       
047400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
047500                         DAT-O-TIDATUM DAT-KDSVAR                         
047600     PERFORM EA-DATKONV                                                   
047700                                                                          
047800     MOVE 'AAVVD ' TO DAT-KDDATFORM                                       
047900     MOVE 97051    TO DAT-I-TIDATUM                                       
048000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
048100                         DAT-O-TIDATUM DAT-KDSVAR                         
048200     PERFORM EA-DATKONV                                                   
048300                                                                          
048400     MOVE 'AAVVD ' TO DAT-KDDATFORM                                       
048500     MOVE 97061    TO DAT-I-TIDATUM                                       
048600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
048700                         DAT-O-TIDATUM DAT-KDSVAR                         
048800     PERFORM EA-DATKONV                                                   
048900                                                                          
049000     MOVE 'AAVVD ' TO DAT-KDDATFORM                                       
049100     MOVE 97071    TO DAT-I-TIDATUM                                       
049200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
049300                         DAT-O-TIDATUM DAT-KDSVAR                         
049400     PERFORM EA-DATKONV                                                   
049500                                                                          
049600     MOVE 'AAVVD ' TO DAT-KDDATFORM                                       
049700     MOVE 97081    TO DAT-I-TIDATUM                                       
049800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
049900                         DAT-O-TIDATUM DAT-KDSVAR                         
050000     PERFORM EA-DATKONV                                                   
050100                                                                          
050200     MOVE 'AAVVD ' TO DAT-KDDATFORM                                       
050300     MOVE 97091    TO DAT-I-TIDATUM                                       
050400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
050500                         DAT-O-TIDATUM DAT-KDSVAR                         
050600     PERFORM EA-DATKONV                                                   
050700                                                                          
050800     MOVE 'AAVVD ' TO DAT-KDDATFORM                                       
050900     MOVE 97121    TO DAT-I-TIDATUM                                       
051000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
051100                         DAT-O-TIDATUM DAT-KDSVAR                         
051200     PERFORM EA-DATKONV                                                   
051300                                                                          
051400     MOVE 'AAVVD ' TO DAT-KDDATFORM                                       
051500     MOVE 97526    TO DAT-I-TIDATUM                                       
051600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
051700                         DAT-O-TIDATUM DAT-KDSVAR                         
051800     PERFORM EA-DATKONV                                                   
051900                                                                          
052000     MOVE 'AAVVD ' TO DAT-KDDATFORM                                       
052100     MOVE 97527    TO DAT-I-TIDATUM                                       
052200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
052300                         DAT-O-TIDATUM DAT-KDSVAR                         
052400     PERFORM EA-DATKONV                                                   
052500                                                                          
052600     MOVE 'AAVVD ' TO DAT-KDDATFORM                                       
052700     MOVE 97528    TO DAT-I-TIDATUM                                       
052800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
052900                         DAT-O-TIDATUM DAT-KDSVAR                         
053000     PERFORM EA-DATKONV                                                   
053100                                                                          
053200     MOVE 'AAP   ' TO DAT-KDDATFORM                                       
053300     MOVE 971      TO DAT-I-TIDATUM                                       
053400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
053500                         DAT-O-TIDATUM DAT-KDSVAR                         
053600     PERFORM EA-DATKONV                                                   
053700                                                                          
053800     MOVE 'AAP   ' TO DAT-KDDATFORM                                       
053900     MOVE 978      TO DAT-I-TIDATUM                                       
054000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
054100                         DAT-O-TIDATUM DAT-KDSVAR                         
054200     PERFORM EA-DATKONV                                                   
054300                                                                          
054400     MOVE 'AAPP  ' TO DAT-KDDATFORM                                       
054500     MOVE 9701     TO DAT-I-TIDATUM                                       
054600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
054700                         DAT-O-TIDATUM DAT-KDSVAR                         
054800     PERFORM EA-DATKONV                                                   
054900                                                                          
055000     MOVE 'AAPP  ' TO DAT-KDDATFORM                                       
055100     MOVE 9712     TO DAT-I-TIDATUM                                       
055200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
055300                         DAT-O-TIDATUM DAT-KDSVAR                         
055400     PERFORM EA-DATKONV                                                   
055500                                                                          
055600     MOVE 'AARP  ' TO DAT-KDDATFORM                                       
055700     MOVE 9701     TO DAT-I-TIDATUM                                       
055800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
055900                         DAT-O-TIDATUM DAT-KDSVAR                         
056000     PERFORM EA-DATKONV                                                   
056100                                                                          
056200     MOVE 'AARP  ' TO DAT-KDDATFORM                                       
056300     MOVE 9712     TO DAT-I-TIDATUM                                       
056400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
056500                         DAT-O-TIDATUM DAT-KDSVAR                         
056600     PERFORM EA-DATKONV                                                   
056700                                                                          
056800     MOVE 'AARP  ' TO DAT-KDDATFORM                                       
056900     MOVE 9700     TO DAT-I-TIDATUM                                       
057000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
057100                         DAT-O-TIDATUM DAT-KDSVAR                         
057200     PERFORM EA-DATKONV                                                   
057300                                                                          
057400     MOVE 'AARP  ' TO DAT-KDDATFORM                                       
057500     MOVE 9713     TO DAT-I-TIDATUM                                       
057600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
057700                         DAT-O-TIDATUM DAT-KDSVAR                         
057800     PERFORM EA-DATKONV                                                   
057900                                                                          
058000     MOVE 'AAP   ' TO DAT-KDDATFORM                                       
058100     MOVE 971      TO DAT-I-TIDATUM                                       
058200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
058300                         DAT-O-TIDATUM DAT-KDSVAR                         
058400     PERFORM EA-DATKONV                                                   
058500                                                                          
058600     MOVE 'AAP   ' TO DAT-KDDATFORM                                       
058700     MOVE 978      TO DAT-I-TIDATUM                                       
058800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
058900                         DAT-O-TIDATUM DAT-KDSVAR                         
059000     PERFORM EA-DATKONV                                                   
059100                                                                          
059200     MOVE 'AAPP  ' TO DAT-KDDATFORM                                       
059300     MOVE 9701     TO DAT-I-TIDATUM                                       
059400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
059500                         DAT-O-TIDATUM DAT-KDSVAR                         
059600     PERFORM EA-DATKONV                                                   
059700                                                                          
059800     MOVE 'AAPP  ' TO DAT-KDDATFORM                                       
059900     MOVE 9712     TO DAT-I-TIDATUM                                       
060000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
060100                         DAT-O-TIDATUM DAT-KDSVAR                         
060200     PERFORM EA-DATKONV                                                   
060300                                                                          
060400     MOVE 'AARP  ' TO DAT-KDDATFORM                                       
060500     MOVE 9701     TO DAT-I-TIDATUM                                       
060600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
060700                         DAT-O-TIDATUM DAT-KDSVAR                         
060800     PERFORM EA-DATKONV                                                   
060900                                                                          
061000     MOVE 'AARP  ' TO DAT-KDDATFORM                                       
061100     MOVE 9712     TO DAT-I-TIDATUM                                       
061200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
061300                         DAT-O-TIDATUM DAT-KDSVAR                         
061400     PERFORM EA-DATKONV                                                   
061500                                                                          
061600     MOVE 'AARP  ' TO DAT-KDDATFORM                                       
061700     MOVE 9700     TO DAT-I-TIDATUM                                       
061800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
061900                         DAT-O-TIDATUM DAT-KDSVAR                         
062000     PERFORM EA-DATKONV                                                   
062100                                                                          
062200     MOVE 'AARP  ' TO DAT-KDDATFORM                                       
062300     MOVE 9713     TO DAT-I-TIDATUM                                       
062400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
062500                         DAT-O-TIDATUM DAT-KDSVAR                         
062600     PERFORM EA-DATKONV                                                   
062700                                                                          
062800     MOVE 'AARP  ' TO DAT-KDDATFORM                                       
062900     MOVE 9912     TO DAT-I-TIDATUM                                       
063000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
063100                         DAT-O-TIDATUM DAT-KDSVAR                         
063200     PERFORM EA-DATKONV                                                   
063300                                                                          
063400     MOVE 'AARP  ' TO DAT-KDDATFORM                                       
063500     MOVE 0001     TO DAT-I-TIDATUM                                       
063600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
063700                         DAT-O-TIDATUM DAT-KDSVAR                         
063800     PERFORM EA-DATKONV                                                   
063900                                                                          
064000     MOVE 9600 TO W-VECKA                                                 
064100     MOVE 'AAVV  ' TO DAT-KDDATFORM                                       
064200     PERFORM 53 TIMES                                                     
064300       ADD 1 TO W-VECKA                                                   
064400       MOVE W-VECKA TO DAT-I-TIDATUM                                      
064500       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
064600                           DAT-O-TIDATUM DAT-KDSVAR                       
064700       PERFORM EA-DATKONV                                                 
064800     END-PERFORM                                                          
064900                                                                          
065000     MOVE 9700 TO W-VECKA                                                 
065100     MOVE 'AAVV  ' TO DAT-KDDATFORM                                       
065200     PERFORM 53 TIMES                                                     
065300       ADD 1 TO W-VECKA                                                   
065400       MOVE W-VECKA TO DAT-I-TIDATUM                                      
065500       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
065600                           DAT-O-TIDATUM DAT-KDSVAR                       
065700       PERFORM EA-DATKONV                                                 
065800     END-PERFORM                                                          
065900                                                                          
066000     MOVE 9800 TO W-VECKA                                                 
066100     MOVE 'AAVV  ' TO DAT-KDDATFORM                                       
066200     PERFORM 53 TIMES                                                     
066300       ADD 1 TO W-VECKA                                                   
066400       MOVE W-VECKA TO DAT-I-TIDATUM                                      
066500       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
066600                           DAT-O-TIDATUM DAT-KDSVAR                       
066700       PERFORM EA-DATKONV                                                 
066800     END-PERFORM                                                          
066900                                                                          
067000     MOVE 9900 TO W-VECKA                                                 
067100     MOVE 'AAVV  ' TO DAT-KDDATFORM                                       
067200     PERFORM 53 TIMES                                                     
067300       ADD 1 TO W-VECKA                                                   
067400       MOVE W-VECKA TO DAT-I-TIDATUM                                      
067500       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
067600                           DAT-O-TIDATUM DAT-KDSVAR                       
067700       PERFORM EA-DATKONV                                                 
067800     END-PERFORM                                                          
067900                                                                          
068000     MOVE 0000 TO W-VECKA                                                 
068100     MOVE 'AAVV  ' TO DAT-KDDATFORM                                       
068200     PERFORM 53 TIMES                                                     
068300       ADD 1 TO W-VECKA                                                   
068400       MOVE W-VECKA TO DAT-I-TIDATUM                                      
068500       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
068600                           DAT-O-TIDATUM DAT-KDSVAR                       
068700       PERFORM EA-DATKONV                                                 
068800     END-PERFORM                                                          
068900     .                                                                    
069000     EJECT                                                                
069100 EA-DATKONV SECTION.                                                      
069200                                                                          
069300     DISPLAY '             '                                              
069400                                                                          
069500     DISPLAY DAT-KDDATFORM                                                
069600         ' ' DAT-I-TIDATUM                                                
069700         ' ' DAT-TIAAMMDD                                                 
069800         ' ' DAT-TIAAVVD                                                  
069900         ' ' DAT-TIAADDD                                                  
070000         ' ' DAT-TIAAP                                                    
070100         ' ' DAT-TIAAPP                                                   
070200         ' ' DAT-TIAARP                                                   
070300         ' ' DAT-TISEKEL                                                  
070400        '  ' DAT-KVVIPER                                                  
070500         ' ' DAT-TISEKDAT                                                 
070600         ' ' DAT-KDSVAR                                                   
070700     .                                                                    
070800     EJECT                                                                
070900 F-WRANDOM SECTION.                                                       
071000                                                                          
071100                                                                          
071200     DISPLAY '100 TAL 1 - 1000:'                                          
071300     MOVE 999        TO MAX-TAL                                           
071400     MOVE 500        TO SVAR-TAL                                          
071500     MOVE 1          TO IX                                                
071600     PERFORM UNTIL IX > 100                                               
071700       CALL WRANDOM USING MAX-TAL SVAR-TAL                                
071800       DISPLAY SVAR-TAL                                                   
071900       ADD 1 TO IX                                                        
072000     END-PERFORM                                                          
072100                                                                          
072200     DISPLAY ' '                                                          
072300     DISPLAY '25 TAL 0 - 100:'                                            
072400     MOVE 99         TO MAX-TAL                                           
072500     MOVE 50         TO SVAR-TAL                                          
072600     MOVE 1          TO IX                                                
072700     PERFORM UNTIL IX > 25                                                
072800       CALL WRANDOM USING MAX-TAL SVAR-TAL                                
072900       DISPLAY SVAR-TAL                                                   
073000       ADD 1 TO IX                                                        
073100     END-PERFORM                                                          
073200                                                                          
073300     DISPLAY ' '                                                          
073400     DISPLAY '20 TAL 0 - 9:'                                              
073500     MOVE 9          TO MAX-TAL                                           
073600     MOVE 50         TO SVAR-TAL                                          
073700     MOVE 1          TO IX                                                
073800     PERFORM UNTIL IX > 20                                                
073900       CALL WRANDOM USING MAX-TAL SVAR-TAL                                
074000       DISPLAY SVAR-TAL                                                   
074100       ADD 1 TO IX                                                        
074200     END-PERFORM                                                          
074300     .                                                                    
074400 G-WDECEDIT SECTION.                                                      
074500                                                                          
074600     MOVE '5.5'    TO DEC-IDFRIDATA                                       
074700     MOVE 7        TO DEC-KVHELTAL                                        
074800     MOVE 2        TO DEC-KVDECIMAL                                       
074900                                                                          
075000     CALL WDECEDIT USING DEC-WDECAREA                                     
075100                                                                          
075200     DISPLAY DEC-KDSVAR                                                   
075300     DISPLAY DEC-IDFRIDATA                                                
075400     DISPLAY DEC-IDEDITDATA DEC-KVHELTAL DEC-KVDECIMAL                    
075500     DISPLAY '---'                                                        
075600                                                                          
075700     .                                                                    
075800     EJECT                                                                
075900 H-WSECURIT SECTION.                                                      
076000                                                                          
076100     DISPLAY 'IDUSER     IDTRANS    IDKEY                  KDSVAR'        
076200                                                                          
076300     MOVE 'pc60485 ' TO SEC-IDUSER                                        
076400     MOVE '4211'     TO SEC-IDTRANS                                       
076500     MOVE '0893'     TO SEC-IDKEY                                         
076600     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
076700                         SEC-IDKEY SEC-KDSVAR                             
076800     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
076900             SEC-IDKEY ' , ' SEC-KDSVAR                                   
077000                                                                          
077100     MOVE 'pc60485 ' TO SEC-IDUSER                                        
077200     MOVE '4211'     TO SEC-IDTRANS                                       
077300     MOVE '0000'     TO SEC-IDKEY                                         
077400     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
077500                         SEC-IDKEY SEC-KDSVAR                             
077600     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
077700             SEC-IDKEY ' , ' SEC-KDSVAR                                   
077800                                                                          
077900     MOVE 'pc60485 ' TO SEC-IDUSER                                        
078000     MOVE '4211'     TO SEC-IDTRANS                                       
078100     MOVE '9999'     TO SEC-IDKEY                                         
078200     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
078300                         SEC-IDKEY SEC-KDSVAR                             
078400     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
078500             SEC-IDKEY ' , ' SEC-KDSVAR                                   
078600                                                                          
078700     MOVE 'RNL01   ' TO SEC-IDUSER                                        
078800     MOVE '1101'     TO SEC-IDTRANS                                       
078900     MOVE '9999'     TO SEC-IDKEY                                         
079000     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
079100                         SEC-IDKEY SEC-KDSVAR                             
079200     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
079300             SEC-IDKEY ' , ' SEC-KDSVAR                                   
079400                                                                          
079500     MOVE 'RAUS1   ' TO SEC-IDUSER                                        
079600     MOVE '4211'     TO SEC-IDTRANS                                       
079700     MOVE '7829'     TO SEC-IDKEY                                         
079800     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
079900                         SEC-IDKEY SEC-KDSVAR                             
080000     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
080100             SEC-IDKEY ' , ' SEC-KDSVAR                                   
080200                                                                          
080300     MOVE 'RAUS1   ' TO SEC-IDUSER                                        
080400     MOVE '4211'     TO SEC-IDTRANS                                       
080500     MOVE '7830'     TO SEC-IDKEY                                         
080600     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
080700                         SEC-IDKEY SEC-KDSVAR                             
080800     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
080900             SEC-IDKEY ' , ' SEC-KDSVAR                                   
081000                                                                          
081100     MOVE 'RAUS1   ' TO SEC-IDUSER                                        
081200     MOVE '4211'     TO SEC-IDTRANS                                       
081300     MOVE '7833'     TO SEC-IDKEY                                         
081400     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
081500                         SEC-IDKEY SEC-KDSVAR                             
081600     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
081700             SEC-IDKEY ' , ' SEC-KDSVAR                                   
081800                                                                          
081900     MOVE 'RAUS1   ' TO SEC-IDUSER                                        
082000     MOVE '4211'     TO SEC-IDTRANS                                       
082100     MOVE '7835'     TO SEC-IDKEY                                         
082200     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
082300                         SEC-IDKEY SEC-KDSVAR                             
082400     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
082500             SEC-IDKEY ' , ' SEC-KDSVAR                                   
082600                                                                          
082700     MOVE 'RAUS1   ' TO SEC-IDUSER                                        
082800     MOVE '4211'     TO SEC-IDTRANS                                       
082900     MOVE '7836'     TO SEC-IDKEY                                         
083000     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
083100                         SEC-IDKEY SEC-KDSVAR                             
083200     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
083300             SEC-IDKEY ' , ' SEC-KDSVAR                                   
083400                                                                          
083500     MOVE 'RA001   ' TO SEC-IDUSER                                        
083600     MOVE '4211'     TO SEC-IDTRANS                                       
083700     MOVE '2345'     TO SEC-IDKEY                                         
083800     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
083900                         SEC-IDKEY SEC-KDSVAR                             
084000     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
084100             SEC-IDKEY ' , ' SEC-KDSVAR                                   
084200                                                                          
084300     MOVE 'RA001   ' TO SEC-IDUSER                                        
084400     MOVE '4211'     TO SEC-IDTRANS                                       
084500     MOVE '2346'     TO SEC-IDKEY                                         
084600     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
084700                         SEC-IDKEY SEC-KDSVAR                             
084800     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
084900             SEC-IDKEY ' , ' SEC-KDSVAR                                   
085000                                                                          
085100     MOVE 'RA001   ' TO SEC-IDUSER                                        
085200     MOVE '4211'     TO SEC-IDTRANS                                       
085300     MOVE '2347'     TO SEC-IDKEY                                         
085400     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
085500                         SEC-IDKEY SEC-KDSVAR                             
085600     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
085700             SEC-IDKEY ' , ' SEC-KDSVAR                                   
085800                                                                          
085900     MOVE 'RSF01   ' TO SEC-IDUSER                                        
086000     MOVE '4211'     TO SEC-IDTRANS                                       
086100     MOVE '1000'     TO SEC-IDKEY                                         
086200     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
086300                         SEC-IDKEY SEC-KDSVAR                             
086400     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
086500             SEC-IDKEY ' , ' SEC-KDSVAR                                   
086600                                                                          
086700     MOVE 'RGB01   ' TO SEC-IDUSER                                        
086800     MOVE '4211'     TO SEC-IDTRANS                                       
086900     MOVE '1320'     TO SEC-IDKEY                                         
087000     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
087100                         SEC-IDKEY SEC-KDSVAR                             
087200     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
087300             SEC-IDKEY ' , ' SEC-KDSVAR                                   
087400                                                                          
087500     MOVE 'RGB01   ' TO SEC-IDUSER                                        
087600     MOVE '4211'     TO SEC-IDTRANS                                       
087700     MOVE '1321'     TO SEC-IDKEY                                         
087800     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
087900                         SEC-IDKEY SEC-KDSVAR                             
088000     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
088100             SEC-IDKEY ' , ' SEC-KDSVAR                                   
088200                                                                          
088300     MOVE 'RGB01   ' TO SEC-IDUSER                                        
088400     MOVE '4211'     TO SEC-IDTRANS                                       
088500     MOVE '1398'     TO SEC-IDKEY                                         
088600     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
088700                         SEC-IDKEY SEC-KDSVAR                             
088800     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
088900             SEC-IDKEY ' , ' SEC-KDSVAR                                   
089000                                                                          
089100     MOVE 'WORD    ' TO SEC-IDUSER                                        
089200     MOVE '4211'     TO SEC-IDTRANS                                       
089300     MOVE '1812'     TO SEC-IDKEY                                         
089400     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
089500                         SEC-IDKEY SEC-KDSVAR                             
089600     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
089700             SEC-IDKEY ' , ' SEC-KDSVAR                                   
089800                                                                          
089900     MOVE 'PS      ' TO SEC-IDUSER                                        
090000     MOVE '4211'     TO SEC-IDTRANS                                       
090100     MOVE '0067'     TO SEC-IDKEY                                         
090200     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
090300                         SEC-IDKEY SEC-KDSVAR                             
090400     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
090500             SEC-IDKEY ' , ' SEC-KDSVAR                                   
090600                                                                          
090700     MOVE 'PB      ' TO SEC-IDUSER                                        
090800     MOVE '4211'     TO SEC-IDTRANS                                       
090900     MOVE '0056'     TO SEC-IDKEY                                         
091000     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
091100                         SEC-IDKEY SEC-KDSVAR                             
091200     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
091300             SEC-IDKEY ' , ' SEC-KDSVAR                                   
091400                                                                          
091500     MOVE 'RB      ' TO SEC-IDUSER                                        
091600     MOVE '4211'     TO SEC-IDTRANS                                       
091700     MOVE '1234'     TO SEC-IDKEY                                         
091800     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
091900                         SEC-IDKEY SEC-KDSVAR                             
092000     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
092100             SEC-IDKEY ' , ' SEC-KDSVAR                                   
092200                                                                          
092300     MOVE 'W0      ' TO SEC-IDUSER                                        
092400     MOVE '4211'     TO SEC-IDTRANS                                       
092500     MOVE '5678'     TO SEC-IDKEY                                         
092600     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
092700                         SEC-IDKEY SEC-KDSVAR                             
092800     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
092900             SEC-IDKEY ' , ' SEC-KDSVAR                                   
093000                                                                          
093100     MOVE 'D       ' TO SEC-IDUSER                                        
093200     MOVE '4211'     TO SEC-IDTRANS                                       
093300     MOVE '0014'     TO SEC-IDKEY                                         
093400     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
093500                         SEC-IDKEY SEC-KDSVAR                             
093600     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
093700             SEC-IDKEY ' , ' SEC-KDSVAR                                   
093800                                                                          
093900     MOVE 'D       ' TO SEC-IDUSER                                        
094000     MOVE '4211'     TO SEC-IDTRANS                                       
094100     MOVE '0091'     TO SEC-IDKEY                                         
094200     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
094300                         SEC-IDKEY SEC-KDSVAR                             
094400     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
094500             SEC-IDKEY ' , ' SEC-KDSVAR                                   
094600                                                                          
094700     MOVE 'D       ' TO SEC-IDUSER                                        
094800     MOVE '4211'     TO SEC-IDTRANS                                       
094900     MOVE '0092'     TO SEC-IDKEY                                         
095000     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
095100                         SEC-IDKEY SEC-KDSVAR                             
095200     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
095300             SEC-IDKEY ' , ' SEC-KDSVAR                                   
095400                                                                          
095500     MOVE 'D       ' TO SEC-IDUSER                                        
095600     MOVE '4211'     TO SEC-IDTRANS                                       
095700     MOVE '0100'     TO SEC-IDKEY                                         
095800     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
095900                         SEC-IDKEY SEC-KDSVAR                             
096000     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
096100             SEC-IDKEY ' , ' SEC-KDSVAR                                   
096200                                                                          
096300     MOVE 'D       ' TO SEC-IDUSER                                        
096400     MOVE '4211'     TO SEC-IDTRANS                                       
096500     MOVE '0799'     TO SEC-IDKEY                                         
096600     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
096700                         SEC-IDKEY SEC-KDSVAR                             
096800     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
096900             SEC-IDKEY ' , ' SEC-KDSVAR                                   
097000                                                                          
097100     MOVE 'D       ' TO SEC-IDUSER                                        
097200     MOVE '4211'     TO SEC-IDTRANS                                       
097300     MOVE '0800'     TO SEC-IDKEY                                         
097400     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
097500                         SEC-IDKEY SEC-KDSVAR                             
097600     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
097700             SEC-IDKEY ' , ' SEC-KDSVAR                                   
097800                                                                          
097900     MOVE 'D076890 ' TO SEC-IDUSER                                        
098000     MOVE '4211'     TO SEC-IDTRANS                                       
098100     MOVE '7512'     TO SEC-IDKEY                                         
098200     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
098300                         SEC-IDKEY SEC-KDSVAR                             
098400     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
098500             SEC-IDKEY ' , ' SEC-KDSVAR                                   
098600                                                                          
098700     MOVE 'D076890 ' TO SEC-IDUSER                                        
098800     MOVE '4211'     TO SEC-IDTRANS                                       
098900     MOVE '0628'     TO SEC-IDKEY                                         
099000     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
099100                         SEC-IDKEY SEC-KDSVAR                             
099200     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
099300             SEC-IDKEY ' , ' SEC-KDSVAR                                   
099400                                                                          
099500     MOVE 'D000090 ' TO SEC-IDUSER                                        
099600     MOVE '4211'     TO SEC-IDTRANS                                       
099700     MOVE '0628'     TO SEC-IDKEY                                         
099800     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
099900                         SEC-IDKEY SEC-KDSVAR                             
100000     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
100100             SEC-IDKEY ' , ' SEC-KDSVAR                                   
100200                                                                          
100300     MOVE 'D080920 ' TO SEC-IDUSER                                        
100400     MOVE '4211'     TO SEC-IDTRANS                                       
100500     MOVE '0628'     TO SEC-IDKEY                                         
100600     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
100700                         SEC-IDKEY SEC-KDSVAR                             
100800     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
100900             SEC-IDKEY ' , ' SEC-KDSVAR                                   
101000                                                                          
101100     MOVE 'WABC    ' TO SEC-IDUSER                                        
101200     MOVE '4211'     TO SEC-IDTRANS                                       
101300     MOVE '1812'     TO SEC-IDKEY                                         
101400     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
101500                         SEC-IDKEY SEC-KDSVAR                             
101600     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
101700             SEC-IDKEY ' , ' SEC-KDSVAR                                   
101800                                                                          
101900     DISPLAY '            '                                               
102000     MOVE 'NOLLJAG ' TO SEC-IDUSER                                        
102100     MOVE '5108'     TO SEC-IDTRANS                                       
102200     MOVE 'C25108'   TO SEC-IDKEY                                         
102300     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
102400                         SEC-IDKEY SEC-KDSVAR                             
102500     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
102600             SEC-IDKEY ' , ' SEC-KDSVAR                                   
102700                                                                          
102800     DISPLAY '            '                                               
102900     MOVE 'WUSA211 ' TO SEC-IDUSER                                        
103000     MOVE '4211'     TO SEC-IDTRANS                                       
103100     MOVE '7554'     TO SEC-IDKEY                                         
103200     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
103300                         SEC-IDKEY SEC-KDSVAR                             
103400     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
103500             SEC-IDKEY ' , ' SEC-KDSVAR                                   
103600                                                                          
103700     DISPLAY '            '                                               
103800     MOVE 'WUSA211 ' TO SEC-IDUSER                                        
103900     MOVE '4211'     TO SEC-IDTRANS                                       
104000     MOVE '7557'     TO SEC-IDKEY                                         
104100     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
104200                         SEC-IDKEY SEC-KDSVAR                             
104300     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
104400             SEC-IDKEY ' , ' SEC-KDSVAR                                   
104500                                                                          
104600     DISPLAY '            '                                               
104700     MOVE 'TN90616 ' TO SEC-IDUSER                                        
104800     MOVE '4211'     TO SEC-IDTRANS                                       
104900     MOVE '1275'     TO SEC-IDKEY                                         
105000     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
105100                         SEC-IDKEY SEC-KDSVAR                             
105200     DISPLAY SEC-IDUSER ' , ' SEC-IDTRANS ' , '                           
105300             SEC-IDKEY ' , ' SEC-KDSVAR                                   
105400     .                                                                    
105500     EJECT                                                                
105600 I-WKPSKONV SECTION.                                                      
105700                                                                          
105800     MOVE 001        TO KPS-KDCALL                                        
105900     MOVE 0314101    TO KPS-IDLKTO                                        
106000     CALL WKPSKONV USING KPS-WKPSAREA                                     
106100     DISPLAY KPS-WKPSAREA                                                 
106200                                                                          
106300     MOVE 001        TO KPS-KDCALL                                        
106400     MOVE 0314201    TO KPS-IDLKTO                                        
106500     CALL WKPSKONV USING KPS-WKPSAREA                                     
106600     DISPLAY KPS-WKPSAREA                                                 
106700                                                                          
106800     MOVE 001        TO KPS-KDCALL                                        
106900     MOVE 0314301    TO KPS-IDLKTO                                        
107000     CALL WKPSKONV USING KPS-WKPSAREA                                     
107100     DISPLAY KPS-WKPSAREA                                                 
107200                                                                          
107300     MOVE 001        TO KPS-KDCALL                                        
107400     MOVE 0314771    TO KPS-IDLKTO                                        
107500     CALL WKPSKONV USING KPS-WKPSAREA                                     
107600     DISPLAY KPS-WKPSAREA                                                 
107700                                                                          
107800     MOVE 001        TO KPS-KDCALL                                        
107900     MOVE 0314773    TO KPS-IDLKTO                                        
108000     CALL WKPSKONV USING KPS-WKPSAREA                                     
108100     DISPLAY KPS-WKPSAREA                                                 
108200                                                                          
108300     MOVE 001        TO KPS-KDCALL                                        
108400     MOVE 0315399    TO KPS-IDLKTO                                        
108500     CALL WKPSKONV USING KPS-WKPSAREA                                     
108600     DISPLAY KPS-WKPSAREA                                                 
108700                                                                          
108800     MOVE 001        TO KPS-KDCALL                                        
108900     MOVE 0315647    TO KPS-IDLKTO                                        
109000     CALL WKPSKONV USING KPS-WKPSAREA                                     
109100     DISPLAY KPS-WKPSAREA                                                 
109200                                                                          
109300     MOVE 001        TO KPS-KDCALL                                        
109400     MOVE 0315656    TO KPS-IDLKTO                                        
109500     CALL WKPSKONV USING KPS-WKPSAREA                                     
109600     DISPLAY KPS-WKPSAREA                                                 
109700                                                                          
109800     MOVE 001        TO KPS-KDCALL                                        
109900     MOVE 0318801    TO KPS-IDLKTO                                        
110000     CALL WKPSKONV USING KPS-WKPSAREA                                     
110100     DISPLAY KPS-WKPSAREA                                                 
110200                                                                          
110300     MOVE 001        TO KPS-KDCALL                                        
110400     MOVE 5714101    TO KPS-IDLKTO                                        
110500     CALL WKPSKONV USING KPS-WKPSAREA                                     
110600     DISPLAY KPS-WKPSAREA                                                 
110700                                                                          
110800     MOVE 001        TO KPS-KDCALL                                        
110900     MOVE 5714201    TO KPS-IDLKTO                                        
111000     CALL WKPSKONV USING KPS-WKPSAREA                                     
111100     DISPLAY KPS-WKPSAREA                                                 
111200                                                                          
111300     MOVE 001        TO KPS-KDCALL                                        
111400     MOVE 5714650    TO KPS-IDLKTO                                        
111500     CALL WKPSKONV USING KPS-WKPSAREA                                     
111600     DISPLAY KPS-WKPSAREA                                                 
111700                                                                          
111800     MOVE 001        TO KPS-KDCALL                                        
111900     MOVE 5714651    TO KPS-IDLKTO                                        
112000     CALL WKPSKONV USING KPS-WKPSAREA                                     
112100     DISPLAY KPS-WKPSAREA                                                 
112200                                                                          
112300     MOVE 001        TO KPS-KDCALL                                        
112400     MOVE 5715997    TO KPS-IDLKTO                                        
112500     CALL WKPSKONV USING KPS-WKPSAREA                                     
112600     DISPLAY KPS-WKPSAREA                                                 
112700                                                                          
112800     MOVE 001        TO KPS-KDCALL                                        
112900     MOVE 5718888    TO KPS-IDLKTO                                        
113000     CALL WKPSKONV USING KPS-WKPSAREA                                     
113100     DISPLAY KPS-WKPSAREA                                                 
113200                                                                          
113300     MOVE 002        TO KPS-KDCALL                                        
113400     MOVE 01         TO KPS-KDPRODSL                                      
113500     CALL WKPSKONV USING KPS-WKPSAREA                                     
113600     DISPLAY KPS-WKPSAREA                                                 
113700                                                                          
113800     MOVE 002        TO KPS-KDCALL                                        
113900     MOVE 11         TO KPS-KDPRODSL                                      
114000     CALL WKPSKONV USING KPS-WKPSAREA                                     
114100     DISPLAY KPS-WKPSAREA                                                 
114200                                                                          
114300     MOVE 002        TO KPS-KDCALL                                        
114400     MOVE 12         TO KPS-KDPRODSL                                      
114500     CALL WKPSKONV USING KPS-WKPSAREA                                     
114600     DISPLAY KPS-WKPSAREA                                                 
114700                                                                          
114800     MOVE 002        TO KPS-KDCALL                                        
114900     MOVE 15         TO KPS-KDPRODSL                                      
115000     CALL WKPSKONV USING KPS-WKPSAREA                                     
115100     DISPLAY KPS-WKPSAREA                                                 
115200                                                                          
115300     MOVE 002        TO KPS-KDCALL                                        
115400     MOVE 19         TO KPS-KDPRODSL                                      
115500     CALL WKPSKONV USING KPS-WKPSAREA                                     
115600     DISPLAY KPS-WKPSAREA                                                 
115700                                                                          
115800     MOVE 002        TO KPS-KDCALL                                        
115900     MOVE 21         TO KPS-KDPRODSL                                      
116000     CALL WKPSKONV USING KPS-WKPSAREA                                     
116100     DISPLAY KPS-WKPSAREA                                                 
116200                                                                          
116300     MOVE 002        TO KPS-KDCALL                                        
116400     MOVE 51         TO KPS-KDPRODSL                                      
116500     CALL WKPSKONV USING KPS-WKPSAREA                                     
116600     DISPLAY KPS-WKPSAREA                                                 
116700                                                                          
116800     MOVE 002        TO KPS-KDCALL                                        
116900     MOVE 52         TO KPS-KDPRODSL                                      
117000     CALL WKPSKONV USING KPS-WKPSAREA                                     
117100     DISPLAY KPS-WKPSAREA                                                 
117200                                                                          
117300     MOVE 002        TO KPS-KDCALL                                        
117400     MOVE 55         TO KPS-KDPRODSL                                      
117500     CALL WKPSKONV USING KPS-WKPSAREA                                     
117600     DISPLAY KPS-WKPSAREA                                                 
117700                                                                          
117800     MOVE 002        TO KPS-KDCALL                                        
117900     MOVE 59         TO KPS-KDPRODSL                                      
118000     CALL WKPSKONV USING KPS-WKPSAREA                                     
118100     DISPLAY KPS-WKPSAREA                                                 
118200                                                                          
118300     MOVE 002        TO KPS-KDCALL                                        
118400     MOVE 70         TO KPS-KDPRODSL                                      
118500     CALL WKPSKONV USING KPS-WKPSAREA                                     
118600     DISPLAY KPS-WKPSAREA                                                 
118700                                                                          
118800     MOVE 002        TO KPS-KDCALL                                        
118900     MOVE 98         TO KPS-KDPRODSL                                      
119000     CALL WKPSKONV USING KPS-WKPSAREA                                     
119100     DISPLAY KPS-WKPSAREA                                                 
119200     .                                                                    
119300     EJECT                                                                
119400 J-WMEDKONV SECTION.                                                      
119500                                                                          
119600     MOVE 'AAA' TO MED-IDSKYLT                                            
119700     MOVE '001' TO MED-IDMFSMED                                           
119800     MOVE '001' TO MED-IDMFSFEL                                           
119900     MOVE '001' TO MED-IDMFSINF                                           
120000     CALL WMEDKONV USING MED-WMEDAREA                                     
120100     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
120200     DISPLAY '   '       ' , ' MED-MFSFEL                                 
120300     DISPLAY '   '       ' , ' MED-MFSINF                                 
120400                                                                          
120500     MOVE 'S  ' TO MED-IDSKYLT                                            
120600     MOVE '001' TO MED-IDMFSMED                                           
120700     MOVE '   ' TO MED-IDMFSFEL                                           
120800     MOVE '   ' TO MED-IDMFSINF                                           
120900     CALL WMEDKONV USING MED-WMEDAREA                                     
121000     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
121100     DISPLAY '   '       ' , ' MED-MFSFEL                                 
121200     DISPLAY '   '       ' , ' MED-MFSINF                                 
121300                                                                          
121400     MOVE 'S  ' TO MED-IDSKYLT                                            
121500     MOVE '   ' TO MED-IDMFSMED                                           
121600     MOVE '002' TO MED-IDMFSFEL                                           
121700     MOVE '   ' TO MED-IDMFSINF                                           
121800     CALL WMEDKONV USING MED-WMEDAREA                                     
121900     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
122000     DISPLAY '   '       ' , ' MED-MFSFEL                                 
122100     DISPLAY '   '       ' , ' MED-MFSINF                                 
122200                                                                          
122300     MOVE 'S  ' TO MED-IDSKYLT                                            
122400     MOVE '   ' TO MED-IDMFSMED                                           
122500     MOVE '   ' TO MED-IDMFSFEL                                           
122600     MOVE '003' TO MED-IDMFSINF                                           
122700     CALL WMEDKONV USING MED-WMEDAREA                                     
122800     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
122900     DISPLAY '   '       ' , ' MED-MFSFEL                                 
123000     DISPLAY '   '       ' , ' MED-MFSINF                                 
123100                                                                          
123200     MOVE 'S  ' TO MED-IDSKYLT                                            
123300     MOVE '001' TO MED-IDMFSMED                                           
123400     MOVE '002' TO MED-IDMFSFEL                                           
123500     MOVE '003' TO MED-IDMFSINF                                           
123600     CALL WMEDKONV USING MED-WMEDAREA                                     
123700     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
123800     DISPLAY '   '       ' , ' MED-MFSFEL                                 
123900     DISPLAY '   '       ' , ' MED-MFSINF                                 
124000                                                                          
124100     MOVE 'S  ' TO MED-IDSKYLT                                            
124200     MOVE '101' TO MED-IDMFSMED                                           
124300     MOVE '102' TO MED-IDMFSFEL                                           
124400     MOVE '103' TO MED-IDMFSINF                                           
124500     CALL WMEDKONV USING MED-WMEDAREA                                     
124600     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
124700     DISPLAY '   '       ' , ' MED-MFSFEL                                 
124800     DISPLAY '   '       ' , ' MED-MFSINF                                 
124900                                                                          
125000     MOVE 'S  ' TO MED-IDSKYLT                                            
125100     MOVE '726' TO MED-IDMFSMED                                           
125200     MOVE '727' TO MED-IDMFSFEL                                           
125300     MOVE '728' TO MED-IDMFSINF                                           
125400     CALL WMEDKONV USING MED-WMEDAREA                                     
125500     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
125600     DISPLAY '   '       ' , ' MED-MFSFEL                                 
125700     DISPLAY '   '       ' , ' MED-MFSINF                                 
125800                                                                          
125900     MOVE 'S  ' TO MED-IDSKYLT                                            
126000     MOVE '114' TO MED-IDMFSMED                                           
126100     MOVE '114' TO MED-IDMFSFEL                                           
126200     MOVE '114' TO MED-IDMFSINF                                           
126300     CALL WMEDKONV USING MED-WMEDAREA                                     
126400     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
126500     DISPLAY '   '       ' , ' MED-MFSFEL                                 
126600     DISPLAY '   '       ' , ' MED-MFSINF                                 
126700                                                                          
126800     MOVE 'S  ' TO MED-IDSKYLT                                            
126900     MOVE '401' TO MED-IDMFSMED                                           
127000     MOVE '401' TO MED-IDMFSFEL                                           
127100     MOVE '401' TO MED-IDMFSINF                                           
127200     CALL WMEDKONV USING MED-WMEDAREA                                     
127300     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
127400     DISPLAY '   '       ' , ' MED-MFSFEL                                 
127500     DISPLAY '   '       ' , ' MED-MFSINF                                 
127600                                                                          
127700     MOVE 'GB ' TO MED-IDSKYLT                                            
127800     MOVE '401' TO MED-IDMFSMED                                           
127900     MOVE '401' TO MED-IDMFSFEL                                           
128000     MOVE '401' TO MED-IDMFSINF                                           
128100     CALL WMEDKONV USING MED-WMEDAREA                                     
128200     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
128300     DISPLAY '   '       ' , ' MED-MFSFEL                                 
128400     DISPLAY '   '       ' , ' MED-MFSINF                                 
128500                                                                          
128600     MOVE 'GB ' TO MED-IDSKYLT                                            
128700     MOVE '114' TO MED-IDMFSMED                                           
128800     MOVE '114' TO MED-IDMFSFEL                                           
128900     MOVE '114' TO MED-IDMFSINF                                           
129000     CALL WMEDKONV USING MED-WMEDAREA                                     
129100     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
129200     DISPLAY '   '       ' , ' MED-MFSFEL                                 
129300     DISPLAY '   '       ' , ' MED-MFSINF                                 
129400                                                                          
129500     MOVE 'S  ' TO MED-IDSKYLT                                            
129600     MOVE '999' TO MED-IDMFSMED                                           
129700     MOVE '999' TO MED-IDMFSFEL                                           
129800     MOVE '999' TO MED-IDMFSINF                                           
129900     CALL WMEDKONV USING MED-WMEDAREA                                     
130000     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
130100     DISPLAY '   '       ' , ' MED-MFSFEL                                 
130200     DISPLAY '   '       ' , ' MED-MFSINF                                 
130300                                                                          
130400     MOVE 'S  ' TO MED-IDSKYLT                                            
130500     MOVE 'FFF' TO MED-IDMFSMED                                           
130600     MOVE 'FFF' TO MED-IDMFSFEL                                           
130700     MOVE 'FFF' TO MED-IDMFSINF                                           
130800     CALL WMEDKONV USING MED-WMEDAREA                                     
130900     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
131000     DISPLAY '   '       ' , ' MED-MFSFEL                                 
131100     DISPLAY '   '       ' , ' MED-MFSINF                                 
131200                                                                          
131300     MOVE 'GB ' TO MED-IDSKYLT                                            
131400     MOVE '001' TO MED-IDMFSMED                                           
131500     MOVE '   ' TO MED-IDMFSFEL                                           
131600     MOVE '   ' TO MED-IDMFSINF                                           
131700     CALL WMEDKONV USING MED-WMEDAREA                                     
131800     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
131900     DISPLAY '   '       ' , ' MED-MFSFEL                                 
132000     DISPLAY '   '       ' , ' MED-MFSINF                                 
132100                                                                          
132200     MOVE 'GB ' TO MED-IDSKYLT                                            
132300     MOVE '   ' TO MED-IDMFSMED                                           
132400     MOVE '002' TO MED-IDMFSFEL                                           
132500     MOVE '   ' TO MED-IDMFSINF                                           
132600     CALL WMEDKONV USING MED-WMEDAREA                                     
132700     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
132800     DISPLAY '   '       ' , ' MED-MFSFEL                                 
132900     DISPLAY '   '       ' , ' MED-MFSINF                                 
133000                                                                          
133100     MOVE 'GB ' TO MED-IDSKYLT                                            
133200     MOVE '   ' TO MED-IDMFSMED                                           
133300     MOVE '   ' TO MED-IDMFSFEL                                           
133400     MOVE '003' TO MED-IDMFSINF                                           
133500     CALL WMEDKONV USING MED-WMEDAREA                                     
133600     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
133700     DISPLAY '   '       ' , ' MED-MFSFEL                                 
133800     DISPLAY '   '       ' , ' MED-MFSINF                                 
133900                                                                          
134000     MOVE 'GB ' TO MED-IDSKYLT                                            
134100     MOVE '001' TO MED-IDMFSMED                                           
134200     MOVE '002' TO MED-IDMFSFEL                                           
134300     MOVE '003' TO MED-IDMFSINF                                           
134400     CALL WMEDKONV USING MED-WMEDAREA                                     
134500     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
134600     DISPLAY '   '       ' , ' MED-MFSFEL                                 
134700     DISPLAY '   '       ' , ' MED-MFSINF                                 
134800                                                                          
134900     MOVE 'GB ' TO MED-IDSKYLT                                            
135000     MOVE '726' TO MED-IDMFSMED                                           
135100     MOVE '727' TO MED-IDMFSFEL                                           
135200     MOVE '728' TO MED-IDMFSINF                                           
135300     CALL WMEDKONV USING MED-WMEDAREA                                     
135400     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
135500     DISPLAY '   '       ' , ' MED-MFSFEL                                 
135600     DISPLAY '   '       ' , ' MED-MFSINF                                 
135700                                                                          
135800     MOVE 'GB ' TO MED-IDSKYLT                                            
135900     MOVE '999' TO MED-IDMFSMED                                           
136000     MOVE '999' TO MED-IDMFSFEL                                           
136100     MOVE '999' TO MED-IDMFSINF                                           
136200     CALL WMEDKONV USING MED-WMEDAREA                                     
136300     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
136400     DISPLAY '   '       ' , ' MED-MFSFEL                                 
136500     DISPLAY '   '       ' , ' MED-MFSINF                                 
136600                                                                          
136700     MOVE 'GB ' TO MED-IDSKYLT                                            
136800     MOVE 'FFF' TO MED-IDMFSMED                                           
136900     MOVE 'FFF' TO MED-IDMFSFEL                                           
137000     MOVE 'FFF' TO MED-IDMFSINF                                           
137100     CALL WMEDKONV USING MED-WMEDAREA                                     
137200     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
137300     DISPLAY '   '       ' , ' MED-MFSFEL                                 
137400     DISPLAY '   '       ' , ' MED-MFSINF                                 
137500                                                                          
137600     MOVE 'B  ' TO MED-IDSKYLT                                            
137700     MOVE '001' TO MED-IDMFSMED                                           
137800     MOVE '   ' TO MED-IDMFSFEL                                           
137900     MOVE '   ' TO MED-IDMFSINF                                           
138000     CALL WMEDKONV USING MED-WMEDAREA                                     
138100     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
138200     DISPLAY '   '       ' , ' MED-MFSFEL                                 
138300     DISPLAY '   '       ' , ' MED-MFSINF                                 
138400                                                                          
138500     MOVE 'B  ' TO MED-IDSKYLT                                            
138600     MOVE '   ' TO MED-IDMFSMED                                           
138700     MOVE '002' TO MED-IDMFSFEL                                           
138800     MOVE '   ' TO MED-IDMFSINF                                           
138900     CALL WMEDKONV USING MED-WMEDAREA                                     
139000     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
139100     DISPLAY '   '       ' , ' MED-MFSFEL                                 
139200     DISPLAY '   '       ' , ' MED-MFSINF                                 
139300                                                                          
139400     MOVE 'B  ' TO MED-IDSKYLT                                            
139500     MOVE '   ' TO MED-IDMFSMED                                           
139600     MOVE '   ' TO MED-IDMFSFEL                                           
139700     MOVE '003' TO MED-IDMFSINF                                           
139800     CALL WMEDKONV USING MED-WMEDAREA                                     
139900     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
140000     DISPLAY '   '       ' , ' MED-MFSFEL                                 
140100     DISPLAY '   '       ' , ' MED-MFSINF                                 
140200                                                                          
140300     MOVE 'B  ' TO MED-IDSKYLT                                            
140400     MOVE '001' TO MED-IDMFSMED                                           
140500     MOVE '002' TO MED-IDMFSFEL                                           
140600     MOVE '003' TO MED-IDMFSINF                                           
140700     CALL WMEDKONV USING MED-WMEDAREA                                     
140800     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
140900     DISPLAY '   '       ' , ' MED-MFSFEL                                 
141000     DISPLAY '   '       ' , ' MED-MFSINF                                 
141100                                                                          
141200     MOVE 'B  ' TO MED-IDSKYLT                                            
141300     MOVE '726' TO MED-IDMFSMED                                           
141400     MOVE '727' TO MED-IDMFSFEL                                           
141500     MOVE '728' TO MED-IDMFSINF                                           
141600     CALL WMEDKONV USING MED-WMEDAREA                                     
141700     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
141800     DISPLAY '   '       ' , ' MED-MFSFEL                                 
141900     DISPLAY '   '       ' , ' MED-MFSINF                                 
142000                                                                          
142100     MOVE 'B  ' TO MED-IDSKYLT                                            
142200     MOVE '999' TO MED-IDMFSMED                                           
142300     MOVE '999' TO MED-IDMFSFEL                                           
142400     MOVE '999' TO MED-IDMFSINF                                           
142500     CALL WMEDKONV USING MED-WMEDAREA                                     
142600     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
142700     DISPLAY '   '       ' , ' MED-MFSFEL                                 
142800     DISPLAY '   '       ' , ' MED-MFSINF                                 
142900                                                                          
143000     MOVE 'B  ' TO MED-IDSKYLT                                            
143100     MOVE 'FFF' TO MED-IDMFSMED                                           
143200     MOVE 'FFF' TO MED-IDMFSFEL                                           
143300     MOVE 'FFF' TO MED-IDMFSINF                                           
143400     CALL WMEDKONV USING MED-WMEDAREA                                     
143500     DISPLAY MED-IDSKYLT ' , ' MED-MFSMED                                 
143600     DISPLAY '   '       ' , ' MED-MFSFEL                                 
143700     DISPLAY '   '       ' , ' MED-MFSINF                                 
143800     .                                                                    
143900     EJECT                                                                
144000 K-WISOLAND SECTION.                                                      
144100                                                                          
144200     MOVE 'AAA' TO LAND-IDLANDX3                                          
144300     MOVE '  '  TO LAND-IDLANDX2                                          
144400     CALL WISOLAND USING LAND-WISOLAND                                    
144500     PERFORM KA-WISOLAND                                                  
144600                                                                          
144700     MOVE 'AFG' TO LAND-IDLANDX3                                          
144800     MOVE '  '  TO LAND-IDLANDX2                                          
144900     CALL WISOLAND USING LAND-WISOLAND                                    
145000     PERFORM KA-WISOLAND                                                  
145100                                                                          
145200     MOVE 'HVO' TO LAND-IDLANDX3                                          
145300     MOVE '  '  TO LAND-IDLANDX2                                          
145400     CALL WISOLAND USING LAND-WISOLAND                                    
145500     PERFORM KA-WISOLAND                                                  
145600                                                                          
145700     MOVE 'DDR' TO LAND-IDLANDX3                                          
145800     MOVE '  '  TO LAND-IDLANDX2                                          
145900     CALL WISOLAND USING LAND-WISOLAND                                    
146000     PERFORM KA-WISOLAND                                                  
146100                                                                          
146200     MOVE 'DEU' TO LAND-IDLANDX3                                          
146300     MOVE '  '  TO LAND-IDLANDX2                                          
146400     CALL WISOLAND USING LAND-WISOLAND                                    
146500     PERFORM KA-WISOLAND                                                  
146600                                                                          
146700     MOVE 'USA' TO LAND-IDLANDX3                                          
146800     MOVE '  '  TO LAND-IDLANDX2                                          
146900     CALL WISOLAND USING LAND-WISOLAND                                    
147000     PERFORM KA-WISOLAND                                                  
147100                                                                          
147200     MOVE 'ZZZ' TO LAND-IDLANDX3                                          
147300     MOVE '  '  TO LAND-IDLANDX2                                          
147400     CALL WISOLAND USING LAND-WISOLAND                                    
147500     PERFORM KA-WISOLAND                                                  
147600                                                                          
147700     MOVE '   ' TO LAND-IDLANDX3                                          
147800     MOVE 'AA' TO LAND-IDLANDX2                                           
147900     CALL WISOLAND USING LAND-WISOLAND                                    
148000     PERFORM KA-WISOLAND                                                  
148100                                                                          
148200     MOVE '   ' TO LAND-IDLANDX3                                          
148300     MOVE 'AF' TO LAND-IDLANDX2                                           
148400     CALL WISOLAND USING LAND-WISOLAND                                    
148500     PERFORM KA-WISOLAND                                                  
148600                                                                          
148700     MOVE '   ' TO LAND-IDLANDX3                                          
148800     MOVE 'HV' TO LAND-IDLANDX2                                           
148900     CALL WISOLAND USING LAND-WISOLAND                                    
149000     PERFORM KA-WISOLAND                                                  
149100                                                                          
149200     MOVE '   ' TO LAND-IDLANDX3                                          
149300     MOVE 'ZZ' TO LAND-IDLANDX2                                           
149400     CALL WISOLAND USING LAND-WISOLAND                                    
149500     PERFORM KA-WISOLAND                                                  
149600                                                                          
149700     MOVE SPACE TO LAND-KDSVAR                                            
149800     MOVE '   ' TO LAND-IDLANDX3                                          
149900     PERFORM UNTIL LAND-KDSVAR = 'S'                                      
150000       MOVE '>>' TO LAND-IDLANDX2                                         
150100       CALL WISOLAND USING LAND-WISOLAND                                  
150200       PERFORM KA-WISOLAND                                                
150300     END-PERFORM                                                          
150400     .                                                                    
150500     EJECT                                                                
150600 KA-WISOLAND SECTION.                                                     
150700                                                                          
150800     DISPLAY LAND-IDLANDX3                                                
150900       ' , ' LAND-IDLANDX2                                                
151000       ' , ' LAND-BELAND-SVE                                              
151100       ' , ' LAND-BELAND-ENG                                              
151200     DISPLAY LAND-WISOVAL(1)                                              
151300       ' , ' LAND-WISOVAL(2)                                              
151400       ' , ' LAND-WISOVAL(3)                                              
151500       ' , ' LAND-KDSVAR                                                  
151600     .                                                                    
151700     EJECT                                                                
151800 L-W015RAND SECTION.                                                      
151900                                                                          
152000     MOVE '123'  TO RAND-IDARTNR                                          
152100     MOVE 'WDK6' TO RAND-BASE                                             
152200     CALL W015RAND USING RAND-IDARTNR RAND-RANDOM RAND-BASE               
152300     PERFORM LA-W015RAND                                                  
152400                                                                          
152500     MOVE '456'  TO RAND-IDARTNR                                          
152600     MOVE 'WDK6' TO RAND-BASE                                             
152700     CALL W015RAND USING RAND-IDARTNR RAND-RANDOM RAND-BASE               
152800     PERFORM LA-W015RAND                                                  
152900     .                                                                    
153000     EJECT                                                                
153100 LA-W015RAND SECTION.                                                     
153200                                                                          
153300     DISPLAY RAND-IDARTNR                                                 
153400       ' , ' RAND-RANDOM                                                  
153500       ' , ' RAND-BASE                                                    
153600     .                                                                    
153700     EJECT                                                                
153800 M-DAGKONV SECTION.                                                       
153900                                                                          
154000     DISPLAY 'CAL  DAGFOM   DAGTOM  ANTAL SVAR'                           
154100                                                                          
154200     MOVE 001 TO DAG-KDCALL                                               
154300     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
154400     PERFORM MA-DAGKONV                                                   
154500                                                                          
154600     MOVE 001 TO DAG-KDCALL                                               
154700     MOVE EJNUM  TO DAG-TIAAMMDD-FOM                                      
154800     MOVE EJNUM  TO DAG-TIAAMMDD-TOM                                      
154900     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
155000     PERFORM MA-DAGKONV                                                   
155100                                                                          
155200     MOVE 001 TO DAG-KDCALL                                               
155300     MOVE 971220 TO DAG-TIAAMMDD-FOM                                      
155400     MOVE 971220 TO DAG-TIAAMMDD-TOM                                      
155500     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
155600     PERFORM MA-DAGKONV                                                   
155700                                                                          
155800     MOVE 001 TO DAG-KDCALL                                               
155900     MOVE 971223 TO DAG-TIAAMMDD-FOM                                      
156000     MOVE 971223 TO DAG-TIAAMMDD-TOM                                      
156100     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
156200     PERFORM MA-DAGKONV                                                   
156300                                                                          
156400     MOVE 001 TO DAG-KDCALL                                               
156500     MOVE 971223 TO DAG-TIAAMMDD-FOM                                      
156600     MOVE 971231 TO DAG-TIAAMMDD-TOM                                      
156700     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
156800     PERFORM MA-DAGKONV                                                   
156900                                                                          
157000     MOVE 001 TO DAG-KDCALL                                               
157100     MOVE 961220 TO DAG-TIAAMMDD-FOM                                      
157200     MOVE 970107 TO DAG-TIAAMMDD-TOM                                      
157300     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
157400     PERFORM MA-DAGKONV                                                   
157500                                                                          
157600     MOVE 001 TO DAG-KDCALL                                               
157700     MOVE 970323 TO DAG-TIAAMMDD-FOM                                      
157800     MOVE 970323 TO DAG-TIAAMMDD-TOM                                      
157900     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
158000     PERFORM MA-DAGKONV                                                   
158100                                                                          
158200     MOVE 001 TO DAG-KDCALL                                               
158300     MOVE 970101 TO DAG-TIAAMMDD-FOM                                      
158400     MOVE 970108 TO DAG-TIAAMMDD-TOM                                      
158500     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
158600     PERFORM MA-DAGKONV                                                   
158700                                                                          
158800     MOVE 001 TO DAG-KDCALL                                               
158900     MOVE 970412 TO DAG-TIAAMMDD-FOM                                      
159000     MOVE 970417 TO DAG-TIAAMMDD-TOM                                      
159100     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
159200     PERFORM MA-DAGKONV                                                   
159300                                                                          
159400     MOVE 001 TO DAG-KDCALL                                               
159500     MOVE 970427 TO DAG-TIAAMMDD-FOM                                      
159600     MOVE 970503 TO DAG-TIAAMMDD-TOM                                      
159700     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
159800     PERFORM MA-DAGKONV                                                   
159900                                                                          
160000     MOVE 001 TO DAG-KDCALL                                               
160100     MOVE 970601 TO DAG-TIAAMMDD-FOM                                      
160200     MOVE 970605 TO DAG-TIAAMMDD-TOM                                      
160300     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
160400     PERFORM MA-DAGKONV                                                   
160500                                                                          
160600     MOVE 001 TO DAG-KDCALL                                               
160700     MOVE 970620 TO DAG-TIAAMMDD-FOM                                      
160800     MOVE 970625 TO DAG-TIAAMMDD-TOM                                      
160900     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
161000     PERFORM MA-DAGKONV                                                   
161100                                                                          
161200     DISPLAY '                                '                           
161300     DISPLAY 'CAL  DATFOM   DATTOM   DAG  SVAR'                           
161400                                                                          
161500     MOVE 002 TO DAG-KDCALL                                               
161600     MOVE 970101 TO DAG-TIAAMMDD-FOM                                      
161700     MOVE 970101 TO DAG-TIAAMMDD-TOM                                      
161800     MOVE +1     TO W-KVARBDAG                                            
161900     PERFORM 366 TIMES                                                    
162000       MOVE W-KVARBDAG TO DAG-KVKALDAG                                    
162100       CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR           
162200       PERFORM MA-DAGKONV                                                 
162300       ADD +1 TO W-KVARBDAG                                               
162400     END-PERFORM                                                          
162500                                                                          
162600     DISPLAY '                                '                           
162700     DISPLAY 'CAL  DATFOM   DATTOM   DAG  SVAR'                           
162800                                                                          
162900     MOVE 002 TO DAG-KDCALL                                               
163000     MOVE 970101 TO DAG-TIAAMMDD-FOM                                      
163100     MOVE 970101 TO DAG-TIAAMMDD-TOM                                      
163200     MOVE +1     TO W-KVARBDAG                                            
163300     PERFORM 366 TIMES                                                    
163400       MOVE W-KVARBDAG TO DAG-KVKALDAG                                    
163500       CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR           
163600       PERFORM MA-DAGKONV                                                 
163700       ADD +1 TO W-KVARBDAG                                               
163800     END-PERFORM                                                          
163900                                                                          
164000     DISPLAY '                                '                           
164100     DISPLAY 'CAL  DATFOM   DATTOM   DAG  SVAR'                           
164200                                                                          
164300     MOVE 003 TO DAG-KDCALL                                               
164400     MOVE 970101 TO DAG-TIAAMMDD-FOM                                      
164500     MOVE 970101 TO DAG-TIAAMMDD-TOM                                      
164600     MOVE +1     TO W-KVARBDAG                                            
164700     PERFORM 366 TIMES                                                    
164800       MOVE W-KVARBDAG TO DAG-KVKALDAG                                    
164900       CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR           
165000       PERFORM MA-DAGKONV                                                 
165100       ADD +1 TO W-KVARBDAG                                               
165200     END-PERFORM                                                          
165300                                                                          
165400     DISPLAY '                                '                           
165500     DISPLAY 'CAL  DATFOM   DATTOM   DAG  SVAR'                           
165600                                                                          
165700     MOVE 003 TO DAG-KDCALL                                               
165800     MOVE 980101 TO DAG-TIAAMMDD-FOM                                      
165900     MOVE 980101 TO DAG-TIAAMMDD-TOM                                      
166000     MOVE +1     TO W-KVARBDAG                                            
166100     PERFORM 366 TIMES                                                    
166200       MOVE W-KVARBDAG TO DAG-KVKALDAG                                    
166300       CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR           
166400       PERFORM MA-DAGKONV                                                 
166500       ADD +1 TO W-KVARBDAG                                               
166600     END-PERFORM                                                          
166700                                                                          
166800     .                                                                    
166900     EJECT                                                                
167000 MA-DAGKONV SECTION.                                                      
167100                                                                          
167200     DISPLAY '             '                                              
167300                                                                          
167400     DISPLAY DAG-KDCALL                                                   
167500         ' ' DAG-TISEKEL-FOM                                              
167600             DAG-TIAAMMDD-FOM                                             
167700         ' ' DAG-TISEKEL-FOM                                              
167800             DAG-TIAAMMDD-TOM                                             
167900         ' ' DAG-KVKALDAG                                                 
168000         ' ' DAG-KDSVAR                                                   
168100     .                                                                    
168200     EJECT                                                                
168300 N-WORKDAY SECTION.                                                       
168400                                                                          
168500     DISPLAY 'CALL DC DATFOM DATTOM DAG NEXT-D NEXT-V SVAR'               
168600                                                                          
168700     MOVE 001 TO WORK-KDCALL                                              
168800     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
168900     PERFORM NA-WORKDAY                                                   
169000                                                                          
169100     MOVE 001 TO WORK-KDCALL                                              
169200     MOVE EJNUM  TO WORK-TIAAMMDD-FOM                                     
169300     MOVE EJNUM  TO WORK-TIAAMMDD-TOM                                     
169400     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
169500     PERFORM NA-WORKDAY                                                   
169600                                                                          
169700     MOVE 001 TO WORK-KDCALL                                              
169800     MOVE EJNUM  TO WORK-IDDC                                             
169900     MOVE 971220 TO WORK-TIAAMMDD-FOM                                     
170000     MOVE 971220 TO WORK-TIAAMMDD-TOM                                     
170100     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
170200     PERFORM NA-WORKDAY                                                   
170300                                                                          
170400     MOVE 001 TO WORK-KDCALL                                              
170500     MOVE '11'   TO WORK-IDDC                                             
170600     MOVE 971220 TO WORK-TIAAMMDD-FOM                                     
170700     MOVE 971220 TO WORK-TIAAMMDD-TOM                                     
170800     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
170900     PERFORM NA-WORKDAY                                                   
171000                                                                          
171100     MOVE 001 TO WORK-KDCALL                                              
171200     MOVE '11'   TO WORK-IDDC                                             
171300     MOVE 971223 TO WORK-TIAAMMDD-FOM                                     
171400     MOVE 971223 TO WORK-TIAAMMDD-TOM                                     
171500     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
171600     PERFORM NA-WORKDAY                                                   
171700                                                                          
171800     MOVE 001 TO WORK-KDCALL                                              
171900     MOVE '11'   TO WORK-IDDC                                             
172000     MOVE 971223 TO WORK-TIAAMMDD-FOM                                     
172100     MOVE 971231 TO WORK-TIAAMMDD-TOM                                     
172200     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
172300     PERFORM NA-WORKDAY                                                   
172400                                                                          
172500     MOVE 001 TO WORK-KDCALL                                              
172600     MOVE '11'   TO WORK-IDDC                                             
172700     MOVE 971220 TO WORK-TIAAMMDD-FOM                                     
172800     MOVE 980107 TO WORK-TIAAMMDD-TOM                                     
172900     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
173000     PERFORM NA-WORKDAY                                                   
173100                                                                          
173200     MOVE 001 TO WORK-KDCALL                                              
173300     MOVE '11'   TO WORK-IDDC                                             
173400     MOVE 970101 TO WORK-TIAAMMDD-FOM                                     
173500     MOVE 970108 TO WORK-TIAAMMDD-TOM                                     
173600     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
173700     PERFORM NA-WORKDAY                                                   
173800                                                                          
173900     MOVE 001 TO WORK-KDCALL                                              
174000     MOVE '11'   TO WORK-IDDC                                             
174100     MOVE 970412 TO WORK-TIAAMMDD-FOM                                     
174200     MOVE 970417 TO WORK-TIAAMMDD-TOM                                     
174300     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
174400     PERFORM NA-WORKDAY                                                   
174500                                                                          
174600     MOVE 001 TO WORK-KDCALL                                              
174700     MOVE '11'   TO WORK-IDDC                                             
174800     MOVE 970427 TO WORK-TIAAMMDD-FOM                                     
174900     MOVE 970503 TO WORK-TIAAMMDD-TOM                                     
175000     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
175100     PERFORM NA-WORKDAY                                                   
175200                                                                          
175300     MOVE 001 TO WORK-KDCALL                                              
175400     MOVE '11'   TO WORK-IDDC                                             
175500     MOVE 970601 TO WORK-TIAAMMDD-FOM                                     
175600     MOVE 970605 TO WORK-TIAAMMDD-TOM                                     
175700     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
175800     PERFORM NA-WORKDAY                                                   
175900                                                                          
176000     MOVE 001 TO WORK-KDCALL                                              
176100     MOVE '00'   TO WORK-IDDC                                             
176200     MOVE 980620 TO WORK-TIAAMMDD-FOM                                     
176300     MOVE 980625 TO WORK-TIAAMMDD-TOM                                     
176400     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
176500     PERFORM NA-WORKDAY                                                   
176600                                                                          
176700     MOVE 001 TO WORK-KDCALL                                              
176800     MOVE '11'   TO WORK-IDDC                                             
176900     MOVE 980620 TO WORK-TIAAMMDD-FOM                                     
177000     MOVE 980625 TO WORK-TIAAMMDD-TOM                                     
177100     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
177200     PERFORM NA-WORKDAY                                                   
177300                                                                          
177400     MOVE 001 TO WORK-KDCALL                                              
177500     MOVE '21'   TO WORK-IDDC                                             
177600     MOVE 980620 TO WORK-TIAAMMDD-FOM                                     
177700     MOVE 980625 TO WORK-TIAAMMDD-TOM                                     
177800     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
177900     PERFORM NA-WORKDAY                                                   
178000                                                                          
178100     MOVE 001 TO WORK-KDCALL                                              
178200     MOVE '22'   TO WORK-IDDC                                             
178300     MOVE 980620 TO WORK-TIAAMMDD-FOM                                     
178400     MOVE 980625 TO WORK-TIAAMMDD-TOM                                     
178500     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
178600     PERFORM NA-WORKDAY                                                   
178700                                                                          
178800     MOVE 001 TO WORK-KDCALL                                              
178900     MOVE '23'   TO WORK-IDDC                                             
179000     MOVE 980620 TO WORK-TIAAMMDD-FOM                                     
179100     MOVE 980625 TO WORK-TIAAMMDD-TOM                                     
179200     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
179300     PERFORM NA-WORKDAY                                                   
179400                                                                          
179500     MOVE 001 TO WORK-KDCALL                                              
179600     MOVE '24'   TO WORK-IDDC                                             
179700     MOVE 980620 TO WORK-TIAAMMDD-FOM                                     
179800     MOVE 980625 TO WORK-TIAAMMDD-TOM                                     
179900     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
180000     PERFORM NA-WORKDAY                                                   
180100                                                                          
180200     MOVE 001 TO WORK-KDCALL                                              
180300     MOVE '25'   TO WORK-IDDC                                             
180400     MOVE 980620 TO WORK-TIAAMMDD-FOM                                     
180500     MOVE 980625 TO WORK-TIAAMMDD-TOM                                     
180600     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
180700     PERFORM NA-WORKDAY                                                   
180800                                                                          
180900     MOVE 001 TO WORK-KDCALL                                              
181000     MOVE '26'   TO WORK-IDDC                                             
181100     MOVE 980620 TO WORK-TIAAMMDD-FOM                                     
181200     MOVE 980625 TO WORK-TIAAMMDD-TOM                                     
181300     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
181400     PERFORM NA-WORKDAY                                                   
181500                                                                          
181600     MOVE 001 TO WORK-KDCALL                                              
181700     MOVE '41'   TO WORK-IDDC                                             
181800     MOVE 980620 TO WORK-TIAAMMDD-FOM                                     
181900     MOVE 980625 TO WORK-TIAAMMDD-TOM                                     
182000     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
182100     PERFORM NA-WORKDAY                                                   
182200                                                                          
182300     MOVE 001 TO WORK-KDCALL                                              
182400     MOVE '42'   TO WORK-IDDC                                             
182500     MOVE 980620 TO WORK-TIAAMMDD-FOM                                     
182600     MOVE 980625 TO WORK-TIAAMMDD-TOM                                     
182700     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
182800     PERFORM NA-WORKDAY                                                   
182900                                                                          
183000     MOVE 001 TO WORK-KDCALL                                              
183100     MOVE '43'   TO WORK-IDDC                                             
183200     MOVE 980620 TO WORK-TIAAMMDD-FOM                                     
183300     MOVE 980625 TO WORK-TIAAMMDD-TOM                                     
183400     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
183500     PERFORM NA-WORKDAY                                                   
183600                                                                          
183700     MOVE 001 TO WORK-KDCALL                                              
183800     MOVE '51'   TO WORK-IDDC                                             
183900     MOVE 980620 TO WORK-TIAAMMDD-FOM                                     
184000     MOVE 980625 TO WORK-TIAAMMDD-TOM                                     
184100     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
184200     PERFORM NA-WORKDAY                                                   
184300                                                                          
184400     DISPLAY 'CALL DC DATFOM DATTOM DAG NEXT-D NEXT-V SVAR'               
184500                                                                          
184600     MOVE 002 TO WORK-KDCALL                                              
184700     MOVE '11'   TO WORK-IDDC                                             
184800     MOVE 970101 TO WORK-TIAAMMDD-FOM                                     
184900     MOVE 970101 TO WORK-TIAAMMDD-TOM                                     
185000     MOVE +1     TO W-KVWORKD                                             
185100     PERFORM 265 TIMES                                                    
185200       MOVE W-KVWORKD TO WORK-KVWORKD                                     
185300       CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA   WORK-KDSVAR        
185400       PERFORM NA-WORKDAY                                                 
185500       ADD +1 TO W-KVWORKD                                                
185600     END-PERFORM                                                          
185700                                                                          
185800     DISPLAY 'CALL DC DATFOM DATTOM DAG NEXT-D NEXT-V SVAR'               
185900                                                                          
186000     MOVE 002 TO WORK-KDCALL                                              
186100     MOVE '11'   TO WORK-IDDC                                             
186200     MOVE 980101 TO WORK-TIAAMMDD-FOM                                     
186300     MOVE 980101 TO WORK-TIAAMMDD-TOM                                     
186400     MOVE +1     TO W-KVWORKD                                             
186500     PERFORM 265 TIMES                                                    
186600       MOVE W-KVWORKD TO WORK-KVWORKD                                     
186700       CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA   WORK-KDSVAR        
186800       PERFORM NA-WORKDAY                                                 
186900       ADD +1 TO W-KVWORKD                                                
187000     END-PERFORM                                                          
187100                                                                          
187200     DISPLAY 'CALL DC DATFOM DATTOM DAG NEXT-D NEXT-V SVAR'               
187300                                                                          
187400     MOVE 002 TO WORK-KDCALL                                              
187500     MOVE '11'   TO WORK-IDDC                                             
187600     MOVE 990101 TO WORK-TIAAMMDD-FOM                                     
187700     MOVE 990101 TO WORK-TIAAMMDD-TOM                                     
187800     MOVE +1     TO W-KVWORKD                                             
187900     PERFORM 265 TIMES                                                    
188000       MOVE W-KVWORKD TO WORK-KVWORKD                                     
188100       CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA   WORK-KDSVAR        
188200       PERFORM NA-WORKDAY                                                 
188300       ADD +1 TO W-KVWORKD                                                
188400     END-PERFORM                                                          
188500                                                                          
188600     DISPLAY 'CALL DC DATFOM DATTOM DAG NEXT-D NEXT-V SVAR'               
188700                                                                          
188800     MOVE 002 TO WORK-KDCALL                                              
188900     MOVE '11'   TO WORK-IDDC                                             
189000     MOVE 000101 TO WORK-TIAAMMDD-FOM                                     
189100     MOVE 000101 TO WORK-TIAAMMDD-TOM                                     
189200     MOVE +1     TO W-KVWORKD                                             
189300     PERFORM 265 TIMES                                                    
189400       MOVE W-KVWORKD TO WORK-KVWORKD                                     
189500       CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA   WORK-KDSVAR        
189600       PERFORM NA-WORKDAY                                                 
189700       ADD +1 TO W-KVWORKD                                                
189800     END-PERFORM                                                          
189900                                                                          
190000     DISPLAY 'CALL DC DATFOM DATTOM DAG NEXT-D NEXT-V SVAR'               
190100                                                                          
190200     MOVE 002 TO WORK-KDCALL                                              
190300     MOVE '41'   TO WORK-IDDC                                             
190400     MOVE 980101 TO WORK-TIAAMMDD-FOM                                     
190500     MOVE 990101 TO WORK-TIAAMMDD-TOM                                     
190600     MOVE +1     TO W-KVWORKD                                             
190700     PERFORM 265 TIMES                                                    
190800       MOVE W-KVWORKD TO WORK-KVWORKD                                     
190900       CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA   WORK-KDSVAR        
191000       PERFORM NA-WORKDAY                                                 
191100       ADD +1 TO W-KVWORKD                                                
191200     END-PERFORM                                                          
191300                                                                          
191400     DISPLAY 'CALL DC DATFOM DATTOM DAG NEXT-D NEXT-V SVAR'               
191500                                                                          
191600     MOVE 003 TO WORK-KDCALL                                              
191700     MOVE '51'   TO WORK-IDDC                                             
191800     MOVE 980101 TO WORK-TIAAMMDD-FOM                                     
191900     MOVE 990101 TO WORK-TIAAMMDD-TOM                                     
192000     MOVE +1     TO W-KVWORKD                                             
192100     PERFORM 265 TIMES                                                    
192200       MOVE W-KVWORKD TO WORK-KVWORKD                                     
192300       CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA   WORK-KDSVAR        
192400       PERFORM NA-WORKDAY                                                 
192500       ADD +1 TO W-KVWORKD                                                
192600     END-PERFORM                                                          
192700                                                                          
192800     DISPLAY 'WORKDAY END    '                                            
192900     DISPLAY '                '                                           
193000                                                                          
193100     .                                                                    
193200     EJECT                                                                
193300 NA-WORKDAY SECTION.                                                      
193400                                                                          
193500     DISPLAY WORK-KDCALL                                                  
193600        '  ' WORK-IDDC                                                    
193700         ' ' WORK-TIAAMMDD-FOM                                            
193800         ' ' WORK-TIAAMMDD-TOM                                            
193900         ' ' WORK-KVWORKD                                                 
194000         ' ' WORK-TIAAMMDD-NEXT-WORKDAY                                   
194100         ' ' WORK-TIAAMMDD-NEXT-WEEK                                      
194200         ' ' WORK-KDSVAR                                                  
194300     DISPLAY '             '                                              
194400                                                                          
194500     .                                                                    
